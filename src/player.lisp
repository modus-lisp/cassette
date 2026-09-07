;;;; player.lisp — pull-model playback: a demuxer cursor feeding a VP8
;;;; decoder for video and reed's Opus decoder for audio.  Nothing here
;;;; touches a device; a caller pulls frames and paces them itself.
(in-package #:cassette)

(defstruct (webm-player (:conc-name player-))
  webm
  video-track audio-track
  reader                                        ; block reader over the whole stream
  vp8                                           ; VP8-DECODER or NIL
  opus                                          ; reed opus decoder state or NIL
  (pending-audio '())                           ; frames read past a video frame
  (pending-video '())
  (need-key nil)                                ; after a seek: skip to the next key frame
  (eof nil))

(defun open-webm (source &key (audio t))
  "Open a WebM from SOURCE (a pathname/namestring or an octet vector).
   Returns a WEBM-PLAYER.  Signals WEBM-ERROR when the video codec is not VP8."
  (let* ((bytes (if (or (stringp source) (pathnamep source)) (slurp-file source) source))
         (w (parse-webm bytes))
         (vt (webm-video-track w))
         (at (and audio (webm-audio-track w))))
    (when (and vt (not (string= (track-codec-id vt) "V_VP8")))
      (%err "video codec ~a is not supported (only V_VP8)" (track-codec-id vt)))
    (when (and at (not (string= (track-codec-id at) "A_OPUS")))
      (setf at nil))                            ; other audio codecs: video only
    (make-webm-player :webm w :video-track vt :audio-track at
                      :reader (make-block-reader w)
                      :vp8 (and vt (make-decoder))
                      :opus (and at (reed:make-opus-decoder
                                     :channels (track-channels at))))))

(defun player-eof-p (p) (player-eof p))

(defun seek-webm (p seconds)
  "Reposition the player at the Cue cluster at or before SECONDS.  Pending frames are dropped
   and the next video frame handed out is the first key frame from there, so the decoder never
   sees an inter frame against a stale reference.  Returns the timestamp actually sought to
   (seconds, double), or NIL for an empty file.  Files without Cues are indexed by walking
   their cluster headers once."
  (let* ((w (player-webm p)) (scale (webm-timecode-scale w))
         (ticks (max 0 (floor (* seconds 1d9) scale)))
         (cue (let ((best nil))
                (dolist (c (cluster-index w) best)
                  (when (and (<= (car c) ticks) (or (null best) (> (car c) (car best))))
                    (setf best c))))))
    (when cue
      (setf (player-reader p) (make-block-reader w :start (cdr cue))
            (player-pending-audio p) '() (player-pending-video p) '()
            (player-eof p) nil
            (player-need-key p) t)
      (/ (* (car cue) scale) 1d9))))

(defun player-duration (p)
  "Duration in seconds (double) or NIL."
  (let ((w (player-webm p)))
    (and (webm-duration w) (/ (* (webm-duration w) (webm-timecode-scale w)) 1d9))))

(defun player-frame-rate (p)
  "Nominal video frame rate from DefaultDuration, or NIL."
  (let ((vt (player-video-track p)))
    (and vt (track-default-duration vt) (/ 1d9 (track-default-duration vt)))))

(defun %next-frame-for (p track)
  "Next demuxed frame of TRACK, queueing frames of the other track."
  (let ((vt (player-video-track p)) (at (player-audio-track p)))
    (loop
      (let ((queued (cond ((eq track vt) (pop (player-pending-video p)))
                          ((eq track at) (pop (player-pending-audio p))))))
        (when queued (return queued)))
      (when (player-eof p) (return nil))
      (let ((f (read-next-frame (player-reader p))))
        (cond ((null f) (setf (player-eof p) t) (return nil))
              ((eq (frame-track f) track) (return f))
              ((and at (eq (frame-track f) at))
               (setf (player-pending-audio p) (nconc (player-pending-audio p) (list f))))
              ((and vt (eq (frame-track f) vt))
               (setf (player-pending-video p) (nconc (player-pending-video p) (list f)))))))))

(defun next-video-frame (p)
  "Decode and return the next *displayed* video PICTURE (timestamp set in
   seconds), or NIL at end of stream.  Hidden frames (altref) are decoded
   and skipped."
  (let ((vt (player-video-track p)) (scale (webm-timecode-scale (player-webm p))))
    (unless vt (return-from next-video-frame nil))
    (loop
     (block skip
      (let ((f (%next-frame-for p vt)))
        (when (null f) (return-from next-video-frame nil))
        (when (player-need-key p)
          (if (frame-keyframe-p f) (setf (player-need-key p) nil) (return-from skip)))
        (multiple-value-bind (pic shown)
            (decode-frame (player-vp8 p) (frame-data f) :timestamp (frame-timestamp f scale))
          (when (and shown pic (not (frame-invisible-p f)))
            (return-from next-video-frame pic))))))))

(defun next-audio-frame (p)
  "Decode the next Opus packet.  Returns (values pcm timestamp-seconds) where
   PCM is a reed PCM struct (16-bit interleaved, 48 kHz), or NIL at end."
  (let ((at (player-audio-track p)) (scale (webm-timecode-scale (player-webm p))))
    (unless at (return-from next-audio-frame nil))
    (let ((f (%next-frame-for p at)))
      (when f
        (values (reed:decode-opus-packet (player-opus p) (frame-data f))
                (frame-timestamp f scale))))))

(defun decode-all-audio (p)
  "Decode the whole audio track to one reed PCM struct, or NIL without audio."
  (let ((chunks '()) (rate 48000) (channels 2))
    (loop for pcm = (next-audio-frame p)
          while pcm
          do (setf rate (reed:pcm-sample-rate pcm) channels (reed:pcm-channels pcm))
             (push (reed:pcm-samples pcm) chunks))
    (when chunks
      (let* ((total (reduce #'+ chunks :key #'length))
             (all (make-array total :element-type '(signed-byte 16)))
             (o 0))
        (dolist (c (nreverse chunks))
          (replace all c :start1 o)
          (incf o (length c)))
        (reed:make-pcm :samples all :channels channels :sample-rate rate)))))

;;; ---- convenience output ------------------------------------------------------------

(defun write-ppm (pic path)
  "Write PIC as a binary PPM."
  (let ((rgb (picture->rgb pic)))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence (string-octets (format nil "P6~%~d ~d~%255~%" (picture-width pic) (picture-height pic))) s)
      (write-sequence rgb s))
    path))

(defun play-to-ffplay (source &key (stream *standard-output*) (max-frames nil))
  "Decode SOURCE and write raw I420 frames to STREAM (an octet stream, e.g. a
   pipe into `ffplay -f rawvideo -pixel_format yuv420p -video_size WxH -`).
   Returns the number of frames written."
  (let ((p (open-webm source :audio nil)) (n 0))
    (loop for pic = (next-video-frame p)
          while (and pic (or (null max-frames) (< n max-frames)))
          do (write-sequence (picture->yuv420 pic) stream)
             (incf n))
    (finish-output stream)
    n))
