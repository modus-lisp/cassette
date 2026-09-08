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
  aac                                           ; reed AAC decoder state or NIL
  mp2                                           ; reed MPEG audio Layer II decoder state or NIL
  mpeg2                                         ; reel.mpeg2 decoder state or NIL
  mpeg4                                         ; reel.mpeg4 decoder state or NIL
  ffv1                                          ; reel.ffv1 decoder state or NIL
  ;; MPEG video pictures decoded ahead, in display order, and the presentation times still unclaimed
  (mpeg2-ready '())
  (mpeg2-stamps '())
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
  (member codec '("V_VP8" "V_MPEG4/ISO/AVC" "V_MPEG1" "V_MPEG2" "V_MPEG4/ISO/ASP" "V_FFV1")
          :test #'equal))

(defun %vfw-codec (private)
  "What a Matroska `V_MS/VFW/FOURCC' track really holds, and where its real CodecPrivate begins.

   VFW is Matroska's escape hatch for codecs that predate a proper mapping: the CodecPrivate is a
   Windows BITMAPINFOHEADER with the codec's own extra data glued on the end.  FFV1 is written this
   way by every muxer in practice, so a reader that does not open the envelope sees a container with
   nothing decodable in it."
  (when (and private (>= (length private) 40))
    (let ((fourcc (string-upcase (map 'string #'code-char (subseq private 16 20)))))
      (values (cdr (assoc fourcc +avi-video-codecs+ :test #'string=))
              (if (> (length private) 40) (subseq private 40) nil)))))
(defun %decodable-audio-p (codec)
  ;; AAC arrives from MP4 as `mp4a' and from Matroska as `A_AAC', and the access units inside are
  ;; identical: one raw_data_block each, configured by an AudioSpecificConfig the container carries
  ;; separately.  The two containers only ever disagreed about the NAME.
  (member codec '("A_OPUS" "A_AAC" "A_MPEG/L2") :test #'equal))

(defun open-media (source &key (audio t))
  "Open a WebM or an MP4 from SOURCE (a pathname, a namestring, or an octet vector), whichever
   it turns out to be.  Returns a WEBM-PLAYER over either.

   Tracks whose codec this image cannot decode are left out of VIDEO-TRACK / AUDIO-TRACK and
   named in PLAYER-UNSUPPORTED instead, so a file that is half playable plays half."
  (let* ((bytes (if (or (stringp source) (pathnamep source)) (slurp-file source) source))
         (mp4p (and (not (webm-p bytes)) (mp4-p bytes)))
         (avip (and (not mp4p) (not (webm-p bytes)) (avi-p bytes)))
         (sysp (and (not mp4p) (not avip) (not (webm-p bytes))
                    (or (mpegts-p bytes) (mpegps-p bytes))))
         (sys-frames nil))
    (unless (or mp4p sysp avip (webm-p bytes))
      (%err "not a WebM, MP4, AVI, program stream or transport stream"))
    (let* ((c (cond (mp4p (parse-mp4 bytes))
                    (avip (parse-avi bytes))
                    (sysp (multiple-value-bind (m fr) (parse-mpegsys bytes)
                            (setf sys-frames fr) m))
                    (t (parse-webm bytes))))
           (vt (cond (mp4p (mp4-video-track c)) (avip (avi-video-track c))
                     (sysp (mpegsys-video-track c))
                     (t (webm-video-track c))))
           (at (and audio (cond (mp4p (mp4-audio-track c)) (avip (avi-audio-track c))
                                (sysp (mpegsys-audio-track c))
                                (t (webm-audio-track c)))))
           (unsupported '()) (note nil) (h264 nil) (ffv1 nil))
      ;; a VFW-wrapped Matroska track names its codec inside the envelope
      (when (and vt (equal (track-codec-id vt) "V_MS/VFW/FOURCC"))
        (multiple-value-bind (codec extra) (%vfw-codec (track-codec-private vt))
          (when codec (setf (track-codec-id vt) codec (track-codec-private vt) extra))))
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
      ;; FFV1's whole configuration is in the container, so a stream this cannot decode is known
      ;; before a single frame is read — which is where it should be found out
      (when (and vt (equal (track-codec-id vt) "V_FFV1"))
        (handler-case
            (let ((cfg (reel.ffv1:parse-configuration
                        (coerce (track-codec-private vt)
                                '(simple-array (unsigned-byte 8) (*))))))
              (setf (reel.ffv1::cfg-width cfg) (track-width vt)
                    (reel.ffv1::cfg-height cfg) (track-height vt))
              ;; THE DECODER HANDLES 4:2:2 AND RGB; THE PICTURE THIS PLAYER HANDS OUT DOES NOT.
              ;; One picture type serves every codec here and it is 4:2:0, so a stream in another
              ;; layout is turned away rather than delivered at the wrong size — which is the same
              ;; rule the codecs follow, applied one level up.
              (unless (and (= 1 (reel.ffv1::cfg-chroma-h-shift cfg))
                           (= 1 (reel.ffv1::cfg-chroma-v-shift cfg))
                           (zerop (reel.ffv1:cfg-colorspace cfg)))
                (error "this FFV1 stream is not 4:2:0, which is the only layout this player's ~
                        picture type can carry"))
              (setf ffv1 (reel.ffv1:make-ffv1-decoder cfg)))
          (error (e)
            (push (track-codec-id vt) unsupported)
            (setf note (princ-to-string e) vt nil ffv1 nil))))
      (when (and at (not (%decodable-audio-p (track-codec-id at))))
        (push (track-codec-id at) unsupported)
        (setf at nil))
      (make-webm-player
       :kind (cond (mp4p :mp4) (avip :avi) (sysp :mpegsys) (t :webm))
       :webm c
       :tick (cond (mp4p +mp4-tick+) (avip +avi-tick+) (sysp (mpegsys-tick c))
                   (t (webm-timecode-scale c)))
       :unsupported (nreverse unsupported)
       :video-note note
       :video-track vt :audio-track at
       :reader (cond (mp4p (make-mp4-reader c))
                     (avip (make-avi-reader c))
                     (sysp (make-mpegsys-reader c sys-frames))
                     (t (make-block-reader c)))
       :vp8 (and vt (equal (track-codec-id vt) "V_VP8") (make-decoder))
       :h264 h264
       :mpeg2 (and vt (member (track-codec-id vt) '("V_MPEG1" "V_MPEG2") :test #'equal)
                   (reel.mpeg2:make-decoder))
       :mpeg4 (and vt (equal (track-codec-id vt) "V_MPEG4/ISO/ASP")
                   (reel.mpeg4:make-decoder))
       :ffv1 (and vt (equal (track-codec-id vt) "V_FFV1") ffv1)
       :nal-length (or (and vt (%avcc-nal-length (track-codec-private vt))) 4)
       :opus (and at (equal (track-codec-id at) "A_OPUS")
                  (reed:make-opus-decoder :channels (track-channels at)))
       :mp2 (and at (equal (track-codec-id at) "A_MPEG/L2") (reed:make-mp2-decoder #()))
       :aac (and at (equal (track-codec-id at) "A_AAC")
                 (reed:make-aac-decoder :asc (track-codec-private at)
                                        :channels (max 1 (or (track-channels at) 2))
                                        :sample-rate (round (or (track-sample-rate at) 44100))))))))

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
  (case (player-kind p)
    (:mp4 (read-next-mp4-frame (player-reader p)))
    (:mpegsys (read-next-mpegsys-frame (player-reader p)))
    (:avi (read-next-avi-frame (player-reader p)))
    (t (read-next-frame (player-reader p)))))

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

(defun %nals-of (p f)
  "The NAL units of one sample, however this container frames them.

   MP4 and Matroska prefix each unit with its length; a transport or program stream carries the
   Annex B byte stream with start codes, exactly as a `.h264' file does.  Reading one as the other
   fails immediately and loudly — a length prefix read as a start code has the forbidden zero bit
   set — which is the one mercy in it."
  (if (member (player-kind p) '(:mpegsys :avi))
      (reel.h264:annex-b-nals (frame-data f))
      (reel.h264:length-prefixed-nals (frame-data f) :length-size (player-nal-length p))))

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
                  (push (%nals-of p f) aus)
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

(defun %mpeg-feed (p bytes)
  "Give one access unit to whichever MPEG video decoder this player holds."
  (if (player-mpeg4 p)
      (reel.mpeg4:feed-bytes (player-mpeg4 p) (coerce bytes '(simple-array (unsigned-byte 8) (*))))
      (reel.mpeg2:feed-bytes (player-mpeg2 p) bytes)))

(defun %mpeg-flush (p)
  (if (player-mpeg4 p)
      (reel.mpeg4:flush-decoder (player-mpeg4 p))
      (reel.mpeg2:flush-decoder (player-mpeg2 p))))

(defun %mpeg-as-picture (p f)
  (if (player-mpeg4 p) (reel.mpeg4:as-picture f) (reel.mpeg2:as-picture f)))

(defun %mpeg2-claim (p pics)
  "Attach presentation times to pictures that have come out in display order.

   The times of the samples fed are held sorted, and each picture takes the earliest still
   unclaimed.  Sorted, the pending times ARE display order — which is the whole reason this works
   without the decoder having to report anything about ordering."
  (loop for pic in pics
        collect (let ((out (%mpeg-as-picture p pic)))
                  (when (player-mpeg2-stamps p)
                    (setf (picture-timestamp out) (pop (player-mpeg2-stamps p))))
                  out)))

(defun %mpeg2-fill (p vt scale)
  "Feed one access unit and queue whatever pictures that released.  NIL at the end of the stream."
  (let ((f (%next-frame-for p vt)))
    (cond
      ((null f)
       (let ((tail (ignore-errors (%mpeg-flush p))))
         (when tail
           (setf (player-mpeg2-ready p) (%mpeg2-claim p tail))
           (return-from %mpeg2-fill t))
         nil))
      (t
       (when (player-need-key p)
         (if (frame-keyframe-p f)
             (setf (player-need-key p) nil)
             (return-from %mpeg2-fill t)))
       (setf (player-mpeg2-stamps p)
             (merge 'list (player-mpeg2-stamps p) (list (frame-timestamp f scale)) #'<))
       (let ((pics (%mpeg-feed p (frame-data f))))
         (setf (player-mpeg2-ready p) (%mpeg2-claim p pics)))
       t))))

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
    ;; FFV1 has no reordering at all — every packet is exactly one picture — so it needs none of
    ;; the queue machinery the others do
    (when (player-ffv1 p)
      (let ((f (%next-frame-for p vt)))
        (when (null f) (return-from next-video-frame nil))
        (let* ((fr (reel.ffv1:decode-frame (player-ffv1 p)
                                           (coerce (frame-data f)
                                                   '(simple-array (unsigned-byte 8) (*)))))
               (out (reel.ffv1:as-picture fr)))
          (setf (picture-timestamp out) (frame-timestamp f scale))
          (return-from next-video-frame out))))
    ;; MPEG video goes through a queue for the same reason H.264 does, and it is worth saying
    ;; plainly: ONE ACCESS UNIT IS NOT ONE PICTURE OUT.  A decoder holds each reference picture back
    ;; until the next one arrives, so feeding a P picture yields the I picture before it, and the
    ;; presentation time on the sample just fed belongs to a picture that has not come out yet.
    (when (or (player-mpeg2 p) (player-mpeg4 p))
      (loop
        (when (player-mpeg2-ready p)
          (return-from next-video-frame (pop (player-mpeg2-ready p))))
        (unless (%mpeg2-fill p vt scale) (return-from next-video-frame nil))))
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
             (dolist (n (%nals-of p f))
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
  "Decode the next audio packet.  Returns (values pcm timestamp-seconds) where PCM is a reed PCM
   struct of 16-bit interleaved samples, or NIL at end.

   Opus is always 48 kHz; AAC comes out at whatever rate its config declares, so a caller that
   mixes tracks has to look at PCM-SAMPLE-RATE rather than assume."
  (let ((at (player-audio-track p)) (scale (player-tick p)))
    (unless at (return-from next-audio-frame nil))
    (let ((f (%next-frame-for p at)))
      (when f
        (values (cond ((player-aac p) (reed:decode-aac-packet (player-aac p) (frame-data f)))
                      ((player-mp2 p) (reed:decode-mp2-packet (player-mp2 p) (frame-data f)))
                      (t (reed:decode-opus-packet (player-opus p) (frame-data f))))
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
