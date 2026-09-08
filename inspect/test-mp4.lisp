;;;; test-mp4.lisp — the MP4 demuxer against ffprobe, packet for packet.
;;;;
;;;; The decoder tests assert pixels; this asserts the thing a demuxer is actually responsible
;;;; for, which is that every sample comes out, at the right size, at the right time, with the
;;;; right idea of whether it is a sync sample.  ffprobe is the oracle, as ffmpeg is everywhere
;;;; else here, and the comparison is exact rather than approximate: byte sizes are integers, and
;;;; presentation times are compared in microseconds with a one-tick tolerance for the fact that
;;;; ffprobe prints seconds as a decimal.
;;;;
;;;; The four fixtures are deliberately different shapes:
;;;;   av.mp4     H.264 baseline + AAC, two tracks, no B-frames, an edit list on the audio
;;;;   audio.m4a  AAC alone, and the priming delay that makes its first sample start before zero
;;;;   fast.mp4   B-frames, so composition offsets and an edit list that puts the first frame at 0
;;;;   frag.mp4   fragmented: an empty moov, and the index distributed across moof boxes
;;;;
;;;;   sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-mp4.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-mp4-test (:use #:cl)) (in-package #:cassette-mp4-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun sh-lines (fmt &rest args)
  (let ((out (uiop:run-program (apply #'format nil fmt args)
                               :output :string :error-output nil :ignore-error-status t)))
    (remove-if (lambda (l) (zerop (length (string-trim " " l))))
               (uiop:split-string out :separator '(#\Newline)))))

(defstruct (pkt (:conc-name pkt-)) pts size key)

(defun ffprobe-packets (path stream)
  "ffprobe's view of one stream: (pts-microseconds size keyframe-p) per packet, in file order."
  (loop for line in (sh-lines "ffprobe -v error -select_streams ~a -show_entries packet=pts_time,size,flags -of csv=p=0 ~a"
                              stream path)
        for fields = (uiop:split-string line :separator ",")
        when (and (>= (length fields) 3)
                  (plusp (length (first fields)))
                  (plusp (length (second fields))))
          collect (make-pkt :pts (round (* 1000000 (read-from-string (first fields))))
                            :size (parse-integer (second fields))
                            :key (and (find #\K (third fields)) t))))

(defun our-packets (m track)
  (let ((r (cassette:make-mp4-reader m)) (out '()))
    (loop for f = (cassette:read-next-mp4-frame r)
          while f
          do (when (eq (cassette:frame-track f) track)
               (push (make-pkt :pts (cassette:frame-timecode f)
                               :size (length (cassette:frame-data f))
                               :key (cassette:frame-keyframe-p f))
                     out)))
    (nreverse out)))

(defun compare (name path m track stream)
  (let* ((theirs (ffprobe-packets path stream))
         (ours (our-packets m track))
         (label (format nil "~a ~a" name stream)))
    (ok (format nil "~a: ~d packets (ffprobe ~d)" label (length ours) (length theirs))
        (= (length ours) (length theirs)))
    (when (= (length ours) (length theirs))
      (let ((bad-size nil) (bad-pts nil) (bad-key nil) (worst 0))
        (loop for a in ours for b in theirs for i from 0
              do (unless (= (pkt-size a) (pkt-size b)) (unless bad-size (setf bad-size (list i (pkt-size a) (pkt-size b)))))
                 (let ((d (abs (- (pkt-pts a) (pkt-pts b)))))
                   (setf worst (max worst d))
                   (when (> d 1) (unless bad-pts (setf bad-pts (list i (pkt-pts a) (pkt-pts b))))))
                 (unless (eq (pkt-key a) (pkt-key b)) (unless bad-key (setf bad-key (list i (pkt-key a) (pkt-key b))))))
        (ok (format nil "~a: every packet is the right size~@[ — first bad ~a~]" label bad-size) (null bad-size))
        (ok (format nil "~a: every presentation time matches (worst ~d us)~@[ — first bad ~a~]" label worst bad-pts)
            (null bad-pts))
        (ok (format nil "~a: every sync flag matches~@[ — first bad ~a~]" label bad-key) (null bad-key))))))

(dolist (spec '(("av.mp4"    :video t :audio t)
                ("audio.m4a" :video nil :audio t)
                ("fast.mp4"  :video t :audio nil)
                ("frag.mp4"  :video t :audio nil)))
  (destructuring-bind (name &key video audio) spec
    (let* ((path (format nil "vectors/~a" name))
           (m (cassette:parse-mp4 (cassette::slurp-file path))))
      (format t "~&== ~a (~:[tabulated~;fragmented~], ~,2f s)~%" name (cassette:mp4-fragmented m)
              (or (cassette:mp4-duration m) 0))
      (when video (compare name path m (cassette:mp4-video-track m) "v:0"))
      (when audio (compare name path m (cassette:mp4-audio-track m) "a:0"))
      ;; seeking: a sync sample at or before the target, and the reader really starts there
      (when (and video (cassette:mp4-duration m) (> (cassette:mp4-duration m) 2.5))
        (multiple-value-bind (r landed) (cassette:seek-mp4 m 2.0d0)
          (let ((f (cassette:read-next-mp4-frame r)))
            (ok (format nil "~a: seek to 2.0 s lands on a sync sample at ~,3f" name landed)
                (and f (cassette:frame-keyframe-p f) (<= landed 2.0)))
            ;; and it is the LAST sync sample at or before the target, not merely one of them
            (let* ((st (gethash (cassette:track-number (cassette:mp4-video-track m))
                                (cassette::mp4-tables m)))
                   (i (cassette:mp4-sync-sample-before st 2.0d0))
                   (next (loop for j from (1+ i) below (cassette:st-count st)
                               when (= 1 (aref (cassette:st-sync st) j)) do (return j))))
              (ok (format nil "~a: and it is the closest one (next sync is after 2.0 s)" name)
                  (or (null next) (> (cassette:st-time-seconds st next) 2.0))))))))))

(format t "~&== AAC, from an MP4 and from a Matroska, through the same decoder~%")
;; The point of this one is that the two answers are IDENTICAL.  An AAC access unit is a
;; raw_data_block either way and the AudioSpecificConfig is the same bytes in `esds' and in
;; CodecPrivate; the containers only ever disagreed about the codec's NAME.  Comparing the two
;; decodes sample for sample is what proves the routing is routing and not a second decoder.
(handler-case
    (let ((a (cassette:decode-all-audio (cassette:open-media "vectors/av.mp4")))
          (b (cassette:decode-all-audio (cassette:open-media "vectors/aac-in-mkv.mkv"))))
      (ok "AAC decodes from an MP4" (and a (plusp (length (reed:pcm-samples a)))))
      (ok "AAC decodes from a Matroska" (and b (plusp (length (reed:pcm-samples b)))))
      (ok "and the two are sample for sample the same"
          (and a b (equalp (reed:pcm-samples a) (reed:pcm-samples b))
               (= (reed:pcm-sample-rate a) (reed:pcm-sample-rate b))
               (= (reed:pcm-channels a) (reed:pcm-channels b)))))
  (error (e) (ok (format nil "AAC in two containers: ~a" e) nil)))

(format t "~&== Ogg, which is a framing layer and nothing else~%")
;; Ogg carries no codec identifier, no track table and no index.  What a stream holds is decided by
;; recognising a magic string in its first packet, which is why the assertions below are about
;; IDENTIFICATION as much as about decoding.
(handler-case
    (let ((o (cassette:parse-ogg (cassette::slurp-file "vectors/theora-av.ogv"))))
      (ok "two streams in one file are separated and both recognised"
          (and (= 2 (length (cassette:ogg-tracks o)))
               (equal (sort (mapcar #'cassette:track-codec-id (cassette:ogg-tracks o))
                            #'string<)
                      (list "A_VORBIS" "V_THEORA"))))
      (let ((vt (cassette:ogg-video-track o)))
        (ok "and the Theora identification packet gives the picture size"
            (and vt (= 176 (cassette:track-width vt)) (= 144 (cassette:track-height vt)))))
      ;; three header packets each for Theora and Vorbis, and they are separated from the data
      (ok "the header packets are separated from the frames"
          (every (lambda (tr) (= 3 (length (cassette:os-headers (cassette:ogg-stream-for o tr)))))
                 (cassette:ogg-tracks o))))
  (error (e) (ok (format nil "Ogg inventory: ~a" e) nil)))

(handler-case
    (let* ((p (cassette:open-media "vectors/opus-ogg.ogg"))
           (pcm (cassette:decode-all-audio p)))
      (ok "Opus in Ogg plays"
          (and pcm (plusp (length (reed:pcm-samples pcm)))
               (= 48000 (reed:pcm-sample-rate pcm))
               (null (cassette:player-unsupported p)))))
  (error (e) (ok (format nil "Opus in Ogg: ~a" e) nil)))

(handler-case
    (let ((p (cassette:open-media "vectors/theora.ogv" :audio nil)))
      (ok "a Theora track is named as undecodable rather than guessed at"
          (and (null (cassette:player-video-track p))
               (member "V_THEORA" (cassette:player-unsupported p) :test #'equal))))
  (error (e) (ok (format nil "Theora refusal: ~a" e) nil)))

(format t "~&~a~%" (if (zerop *fails*) "MP4 DEMUX OK" (format nil "MP4 DEMUX: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
