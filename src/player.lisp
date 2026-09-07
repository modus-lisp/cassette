;;;; player.lisp — pull-model playback: a demuxer cursor feeding reel's VP8
;;;; decoder for video and reed's Opus decoder for audio.  Nothing here
;;;; touches a device; a caller pulls frames and paces them itself.
;;;;
;;;; BOTH CONTAINERS, ONE PLAYER.  The two demuxers hand back the same TRACK and BLOCK-FRAME
;;;; structs, so what differs here is three lines: which reader to advance, how many nanoseconds
;;;; a timecode tick is, and how to seek.  OPEN-MEDIA sniffs and the rest of this file does not
;;;; ask again.
;;;;
;;;; A CODEC THIS IMAGE CANNOT DECODE IS NOT AN ERROR.  An MP4 is usually H.264, which reel does
;;;; not decode; refusing to open the file would also refuse its audio, which is decodable and is
;;;; most of what a person wants from a file they cannot watch.  So an undecodable track is
;;;; reported — PLAYER-UNSUPPORTED says which codec it was — and the other track still plays.
(in-package #:cassette)

(defstruct (webm-player (:conc-name player-))
  (kind :webm)                                  ; :webm or :mp4
  webm                                          ; the WEBM or MP4 container struct
  (tick 1000000)                                ; nanoseconds per BLOCK-FRAME timecode tick
  unsupported                                   ; (codec-id ...) present but not decodable here
  video-note                                    ; why the video track was refused, if it was
  video-track audio-track
  reader                                        ; block reader over the whole stream
  vp8                                           ; reel VP8 decoder, or NIL
  h264                                          ; reel.h264 decoder, or NIL
  (nal-length 4)                                ; bytes of length prefix on each MP4 NAL unit
  opus                                          ; reed opus decoder state or NIL
  (pending-audio '())                           ; frames read past a video frame
  (pending-video '())
  (need-key nil)                                ; after a seek: skip to the next key frame
  ;; H.264 pictures decoded ahead, in order, waiting to be handed out one at a time
  (h264-ready '())
  ;; Presentation times of samples fed but not yet matched to a picture, smallest first.  B
  ;; pictures come out of the decoder in DISPLAY order and go in in coding order, so a picture
  ;; cannot be labelled with the timestamp of the sample that produced it — the two are different
  ;; samples.  Sorted ascending, the pending times ARE display order, so each picture takes the
  ;; earliest one still unclaimed.
  (h264-stamps '())
  (h264-batch 0)                                ; 0 = decide on the first batch, -1 = serial only
  (eof nil))

(defun %decodable-video-p (codec)
  (member codec '("V_VP8" "V_MPEG4/ISO/AVC") :test #'equal))
(defun %decodable-audio-p (codec) (equal codec "A_OPUS"))

(defun open-media (source &key (audio t))
  "Open a WebM or an MP4 from SOURCE (a pathname, a namestring, or an octet vector), whichever
   it turns out to be.  Returns a WEBM-PLAYER over either.

   Tracks whose codec this image cannot decode are left out of VIDEO-TRACK / AUDIO-TRACK and
   named in PLAYER-UNSUPPORTED instead, so a file that is half playable plays half."
  (let* ((bytes (if (or (stringp source) (pathnamep source)) (slurp-file source) source))
         (mp4p (and (not (webm-p bytes)) (mp4-p bytes))))
    (unless (or mp4p (webm-p bytes))
      (%err "not a WebM or MP4 file"))
    (let* ((c (if mp4p (parse-mp4 bytes) (parse-webm bytes)))
           (vt (if mp4p (mp4-video-track c) (webm-video-track c)))
           (at (and audio (if mp4p (mp4-audio-track c) (webm-audio-track c))))
           (unsupported '()) (note nil) (h264 nil))
      (when (and vt (not (%decodable-video-p (track-codec-id vt))))
        (push (track-codec-id vt) unsupported)
        (setf note (format nil "~a is not a codec this decodes" (track-codec-id vt)))
        (setf vt nil))
      ;; An H.264 track whose parameter sets will not parse — ten-bit, 4:2:2, a High profile
      ;; feature — is undecodable too, and it is worth finding that out HERE.  The parameter sets
      ;; are read when the file is opened, so letting that error escape fails the whole file and
      ;; takes the audio with it, which is the one thing this player is supposed not to do.
      (when (and vt (equal (track-codec-id vt) "V_MPEG4/ISO/AVC"))
        (handler-case (setf h264 (%make-h264 vt))
          (error (e)
            (push (track-codec-id vt) unsupported)
            (setf note (princ-to-string e) vt nil h264 nil))))
      (when (and at (not (%decodable-audio-p (track-codec-id at))))
        (push (track-codec-id at) unsupported)
        (setf at nil))
      (make-webm-player
       :kind (if mp4p :mp4 :webm)
       :webm c
       :tick (if mp4p +mp4-tick+ (webm-timecode-scale c))
       :unsupported (nreverse unsupported)
       :video-note note
       :video-track vt :audio-track at
       :reader (if mp4p (make-mp4-reader c) (make-block-reader c))
       :vp8 (and vt (equal (track-codec-id vt) "V_VP8") (make-decoder))
       :h264 h264
       :nal-length (or (and vt (%avcc-nal-length (track-codec-private vt))) 4)
       :opus (and at (reed:make-opus-decoder :channels (track-channels at)))))))

(defun %avcc-nal-length (avcc)
  "The NAL length-prefix width an `avcC' declares, or NIL when there is no avcC."
  (when (and avcc (>= (length avcc) 5)) (1+ (logand (aref avcc 4) 3))))

(defun %make-h264 (vt)
  "An H.264 decoder primed with the SPS and PPS from the track's `avcC'.

   Those parameter sets live in the container, not in the samples — an MP4 or Matroska H.264
   track carries them once in CodecPrivate and the samples reference them — so a decoder that is
   only ever fed samples never learns the picture size and refuses the first slice."
  (let ((d (reel.h264:make-decoder))
        (avcc (track-codec-private vt)))
    (when (and avcc (plusp (length avcc)))
      (multiple-value-bind (sps pps) (reel.h264:avcc-parameter-sets avcc)
        (dolist (n (append sps pps)) (reel.h264:feed-nal d n))))
    d))

(defun open-webm (source &key (audio t))
  "OPEN-MEDIA, kept under the name it had when this repo only read one container."
  (open-media source :audio audio))

(defun %advance-reader (p)
  "The next frame from whichever demuxer this player is holding."
  (if (eq (player-kind p) :mp4)
      (read-next-mp4-frame (player-reader p))
      (read-next-frame (player-reader p))))

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
            (player-h264-ready p) '() (player-h264-stamps p) '()
            (player-eof p) nil
            (player-need-key p) t)
      (/ (* (car cue) scale) 1d9))))

(defun player-duration (p)
  "Duration in seconds (double) or NIL."
  (let ((c (player-webm p)))
    (if (eq (player-kind p) :mp4)
        (mp4-duration c)
        (and (webm-duration c) (/ (* (webm-duration c) (webm-timecode-scale c)) 1d9)))))

(defun player-frame-rate (p)
  "Nominal video frame rate from DefaultDuration, or NIL."
  (let ((vt (player-video-track p)))
    (and vt (track-default-duration vt) (/ 1d9 (track-default-duration vt)))))

(defun seek-media (p seconds)
  "Reposition at or before SECONDS, whichever container this is.  Returns the timestamp actually
   landed on, in seconds."
  (if (eq (player-kind p) :mp4)
      (multiple-value-bind (r landed) (seek-mp4 (player-webm p) seconds)
        (setf (player-reader p) r
              (player-pending-audio p) '() (player-pending-video p) '()
              (player-h264-ready p) '() (player-h264-stamps p) '()
              (player-eof p) nil (player-need-key p) t)
        landed)
      (seek-webm p seconds)))

(defun %next-frame-for (p track)
  "Next demuxed frame of TRACK, queueing frames of the other track."
  (let ((vt (player-video-track p)) (at (player-audio-track p)))
    (loop
      (let ((queued (cond ((eq track vt) (pop (player-pending-video p)))
                          ((eq track at) (pop (player-pending-audio p))))))
        (when queued (return queued)))
      (when (player-eof p) (return nil))
      (let ((f (%advance-reader p)))
        (cond ((null f) (setf (player-eof p) t) (return nil))
              ((eq (frame-track f) track) (return f))
              ((and at (eq (frame-track f) at))
               (setf (player-pending-audio p) (nconc (player-pending-audio p) (list f))))
              ((and vt (eq (frame-track f) vt))
               (setf (player-pending-video p) (nconc (player-pending-video p) (list f)))))))))

(defparameter *h264-read-ahead* 8
  "How many H.264 pictures to decode at once when the stream allows it.

   The pictures in an all-intra stream are independent, so a batch of them decodes on as many cores
   as there are pictures — about five times faster here at eight.  The number is a trade: bigger
   batches scale better and cost more latency on a seek, because the first picture after one is not
   ready until its whole batch is.  Eight is a third of a second of video and about 60 ms of work.")

(defun %h264-fill-batch (p vt scale)
  "Read ahead up to *H264-READ-AHEAD* samples and decode their pictures at once.

   Returns T if anything was queued.  Falls back to serial decoding — and remembers to keep doing
   so — the moment the stream turns out not to be all-intra, which is what happens on the first P
   slice of an ordinary inter-coded file."
  (let ((aus '()) (stamps '()) (n 0))
    ;; collect the compressed samples first; demuxing stays serial and is cheap
    (loop while (< n *h264-read-ahead*)
          do (let ((f (%next-frame-for p vt)))
               (when (null f) (return))
               ;; after a seek, drop samples until the first key frame rather than decoding them
               (cond
                 ((and (player-need-key p) (not (frame-keyframe-p f))))
                 (t
                  (setf (player-need-key p) nil)
                  (push (reel.h264:length-prefixed-nals (frame-data f)
                                                        :length-size (player-nal-length p))
                        aus)
                  (push (frame-timestamp f scale) stamps)
                  (incf n)))))
    (setf aus (nreverse aus) stamps (nreverse stamps))
    (when (null aus)
      ;; end of the samples: whatever the reorder buffer is still holding comes out now, or the
      ;; last pictures of every file would simply never appear
      (let ((tail (ignore-errors (reel.h264:flush-decoder (player-h264 p)))))
        (when tail
          (setf (player-h264-ready p) (%label-pictures p tail))
          (return-from %h264-fill-batch t)))
      (return-from %h264-fill-batch nil))
    (let ((pics (and (> *h264-read-ahead* 1)
                     (not (minusp (player-h264-batch p)))
                     (reel.h264:decode-independent (player-h264 p) aus))))
      (cond
        (pics
         (setf (player-h264-ready p)
               (loop for pic in pics for ts in stamps
                     when pic
                       collect (let ((out (reel.h264:as-picture pic)))
                                 (setf (picture-timestamp out) ts)
                                 out)))
         t)
        (t
         ;; not independently decodable: this stream is serial from here on, and the samples just
         ;; read still have to be decoded, in order, through the streaming decoder
         (setf (player-h264-batch p) -1)
         (let ((got '()))
           (loop for au in aus for ts in stamps
                 do (setf (player-h264-stamps p)
                          (merge 'list (player-h264-stamps p) (list ts) #'<))
                    (dolist (nal au)
                      (let ((q (reel.h264:feed-nal (player-h264 p) nal)))
                        (when q (push q got)))))
           (setf (player-h264-ready p) (%label-pictures p (nreverse got))))
         t)))))

(defun %label-pictures (p pics)
  "Turn decoder pictures into cassette ones, each taking the earliest unclaimed presentation time."
  (loop for pic in pics
        collect (let ((out (reel.h264:as-picture pic)))
                  (setf (picture-timestamp out) (pop (player-h264-stamps p)))
                  out)))

(defun next-video-frame (p)
  "Decode and return the next *displayed* video PICTURE (timestamp set in
   seconds), or NIL at end of stream.  Hidden frames (altref) are decoded
   and skipped."
  (let ((vt (player-video-track p)) (scale (player-tick p)))
    (unless vt (return-from next-video-frame nil))
    ;; H.264 always goes through the read-ahead queue, even once a stream has turned out not to be
    ;; independently decodable.  It is not only about speed: the queue is where pictures are matched
    ;; to presentation times, and a B picture cannot take the time of the sample that produced it —
    ;; it comes out of the decoder several samples later, in display order.
    (when (player-h264 p)
      (loop
        (when (player-h264-ready p)
          (return-from next-video-frame (pop (player-h264-ready p))))
        (unless (%h264-fill-batch p vt scale) (return-from next-video-frame nil))))
    (loop
     (block skip
      (let ((f (%next-frame-for p vt)))
        (when (null f) (return-from next-video-frame nil))
        (when (player-need-key p)
          (if (frame-keyframe-p f) (setf (player-need-key p) nil) (return-from skip)))
        (cond
          ((player-h264 p)
           ;; an MP4/Matroska sample is a run of length-prefixed NAL units, not a byte stream
           (let ((pic nil))
             (dolist (n (reel.h264:length-prefixed-nals
                         (frame-data f) :length-size (player-nal-length p)))
               (let ((r (reel.h264:feed-nal (player-h264 p) n)))
                 (when r (setf pic r))))
             (when pic
               (let ((out (reel.h264:as-picture pic)))
                 (setf (picture-timestamp out) (frame-timestamp f scale))
                 (return-from next-video-frame out)))))
          (t
           (multiple-value-bind (pic shown)
               (decode-frame (player-vp8 p) (frame-data f) :timestamp (frame-timestamp f scale))
             (when (and shown pic (not (frame-invisible-p f)))
               (return-from next-video-frame pic))))))))))

(defun next-audio-frame (p)
  "Decode the next Opus packet.  Returns (values pcm timestamp-seconds) where
   PCM is a reed PCM struct (16-bit interleaved, 48 kHz), or NIL at end."
  (let ((at (player-audio-track p)) (scale (player-tick p)))
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
