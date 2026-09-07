;;;; test-h264.lisp — reel's H.264 decoder against ffmpeg, and MP4 video end to end.
;;;;
;;;; Lives here rather than in reel for the same reason the VP8 conformance test does: reel
;;;; depends on nothing and its own test needs nothing, so the comparisons that need ffmpeg live
;;;; where ffmpeg is already a fixture.
;;;;
;;;; The comparison is against ffmpeg's ORDINARY output — deblocking filter and all — because that
;;;; is what a conforming decoder produces.  (While the loop filter was being written it was useful
;;;; to compare against `ffmpeg -skip_loop_filter all`, which is the reconstruction before
;;;; filtering; if a change ever breaks this test, that flag tells you in one run whether the
;;;; damage is in the filter or under it.)
;;;;
;;;; The fixtures are deliberately spread: a macroblock-aligned size and a cropped one, a fine
;;;; quantiser and a coarse one, synthetic bars and a mandelbrot, and 320x240 so that more than a
;;;; handful of macroblocks are in play.
;;;;
;;;;   sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-h264.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-h264-test (:use #:cl)) (in-package #:cassette-h264-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun slurp (p) (cassette::slurp-file p))

(format t "~&== Annex B streams, every frame, against ffmpeg~%")
(dolist (name '("intra" "crop" "big" "mandel" "coarse"))
  (handler-case
      (let* ((pics (reel.h264:decode-annex-b (slurp (format nil "vectors/~a.h264" name))))
             (oracle (slurp (format nil "vectors/~a.yuv" name))))
        (if (null pics)
            (ok (format nil "~a: decoded no pictures" name) nil)
            (let* ((fb (length (reel.h264:picture->yuv420 (first pics))))
                   (n (min (length pics) (floor (length oracle) fb)))
                   (exact 0) (worst 0))
              (dotimes (i n)
                (let ((y (reel.h264:picture->yuv420 (nth i pics))) (off (* i fb)) (bad 0))
                  (dotimes (k fb)
                    (let ((d (abs (- (aref y k) (aref oracle (+ off k))))))
                      (when (plusp d) (incf bad) (setf worst (max worst d)))))
                  (when (zerop bad) (incf exact))))
              (ok (format nil "~a: ~dx~d, ~d frames, ~d bit-exact~@[, worst sample error ~d~]"
                          name (reel.h264:pic-width (first pics)) (reel.h264:pic-height (first pics))
                          n exact (and (plusp worst) worst))
                  (and (plusp n) (= exact n))))))
    (error (e) (ok (format nil "~a: ~a" name e) nil))))

(format t "~&== CABAC: the arithmetic decoder, against ffmpeg~%")
;; CABAC has no partial credit.  A wrong context index feeds the wrong probability to the
;; arithmetic decoder, which returns the wrong bin, and every symbol after it in the slice is
;; garbage — so these either match exactly or fail completely, and a near miss is not a thing.
;; Spread across quantisers because the coefficient contexts are where the detail lives.
(dolist (spec '(("cabac-intra" . "intra, qp 26, synthetic")
                ("cabac-fine" . "intra, qp 12, almost lossless")
                ("cabac-mandel" . "intra, qp 34, coarse, detailed")
                ("cabac-big" . "intra, qp 18, 320x240")
                ("cabac-p" . "P slices, one reference")
                ("c-m3ref" . "P slices, three references")
                ("c-mall" . "P slices, three references and every partition size")
                ("cabac-pbbb" . "real motion, 60 frames, everything at once")
                ("w-cabac" . "weighted prediction and a reordered reference list")))
  (destructuring-bind (name . what) spec
    (handler-case
        (let* ((pics (reel.h264:decode-annex-b (slurp (format nil "vectors/~a.h264" name))
                                               :threads 1))
               (oracle (slurp (format nil "vectors/~a.yuv" name))))
          (if (null pics)
              (ok (format nil "~a: decoded no pictures" name) nil)
              (let* ((fb (length (reel.h264:picture->yuv420 (first pics))))
                     (n (min (length pics) (floor (length oracle) fb)))
                     (exact 0))
                (dotimes (i n)
                  (let ((y (reel.h264:picture->yuv420 (nth i pics))) (off (* i fb)) (bad 0))
                    (dotimes (k fb)
                      (unless (= (aref y k) (aref oracle (+ off k))) (incf bad)))
                    (when (zerop bad) (incf exact))))
                (ok (format nil "~a (~a): ~d frames, ~d bit-exact" name what n exact)
                    (and (plusp n) (= exact n))))))
      (error (e) (ok (format nil "~a: ~a" name e) nil)))))

(format t "~&== P slices: motion compensation, against ffmpeg~%")
;; Inter prediction has failure modes that do not desynchronise the bitstream — a wrong predicted
;; vector, a wrong interpolation position, a wrong boundary strength — so these compare every
;; sample of every frame with the loop filter ON, which is what a conforming decoder outputs.
;; The fixtures are chosen to separate the things that broke independently while this was written:
;; a single reference against three, and 16x16 partitions against sub-8x8 ones.
(dolist (spec '(("pslice" . "synthetic motion, one reference")
                ("pmandel" . "a coarse quantiser")
                ("t-ref3-16x16" . "three reference pictures")
                ("t-p4x4" . "8x4, 4x8 and 4x4 sub-partitions")
                ("t-i4x4" . "intra macroblocks inside P slices")
                ("pbbb" . "real motion, three references, every partition size")
                ("w-cavlc" . "weighted prediction and a reordered reference list")))
  (destructuring-bind (name . what) spec
    (handler-case
        (let* ((pics (reel.h264:decode-annex-b (slurp (format nil "vectors/~a.h264" name))
                                               :threads 1))
               (oracle (slurp (format nil "vectors/~a.yuv" name))))
          (if (null pics)
              (ok (format nil "~a: decoded no pictures" name) nil)
              (let* ((fb (length (reel.h264:picture->yuv420 (first pics))))
                     (n (min (length pics) (floor (length oracle) fb)))
                     (exact 0))
                (dotimes (i n)
                  (let ((y (reel.h264:picture->yuv420 (nth i pics))) (off (* i fb)) (bad 0))
                    (dotimes (k fb)
                      (unless (= (aref y k) (aref oracle (+ off k))) (incf bad)))
                    (when (zerop bad) (incf exact))))
                (ok (format nil "~a (~a): ~d frames, ~d bit-exact" name what n exact)
                    (and (plusp n) (= exact n))))))
      (error (e) (ok (format nil "~a: ~a" name e) nil)))))

(format t "~&== B slices: two reference lists, and prediction from both at once~%")
;; A B picture is decoded AFTER the picture it is displayed before, so these also check the output
;; reordering: the comparison is against ffmpeg's frames in DISPLAY order, and a decoder that hands
;; pictures over as it finishes them fails here even with every sample right.
;;
;; The fixtures separate what broke separately: the two direct modes are different algorithms, the
;; weighted variant derives its weights from picture order rather than being told them, and b-mix
;; has a scene cut in it, which is what makes an encoder put intra macroblocks inside a B slice and
;; give four 8x8 partitions four different prediction modes.
(dolist (spec '(("b-spat" . "spatial direct, CAVLC")
                ("b-temp" . "temporal direct, CAVLC")
                ("b-cabac" . "spatial direct, CABAC")
                ("b-ref3" . "three reference pictures")
                ("b-sub" . "every partition size")
                ("b-wb" . "implicit weighted bi-prediction")
                ("b-mix" . "a scene cut: intra in B slices, mixed partition modes")
                ("bt-r3" . "temporal direct with three reference pictures")
                ("bs-r3" . "spatial direct with three reference pictures")
                ("bt-r1" . "temporal direct with one reference picture")
                ("b-hard" . "everything at once: weighting, three refs, all partitions")
                ("k-nopart" . "a coded direct macroblock beside one coding a reference index")
                ("hp-plain" . "High profile using neither the 8x8 transform nor scaling lists")
                ("hp-cqm" . "High profile with the default scaling matrices")))
  (destructuring-bind (name . what) spec
    (handler-case
        (let* ((pics (reel.h264:decode-annex-b (slurp (format nil "vectors/~a.h264" name))
                                               :threads 1))
               (oracle (slurp (format nil "vectors/~a.yuv" name))))
          (if (null pics)
              (ok (format nil "~a: decoded no pictures" name) nil)
              (let* ((fb (length (reel.h264:picture->yuv420 (first pics))))
                     (n (min (length pics) (floor (length oracle) fb)))
                     (exact 0))
                (dotimes (i n)
                  (let ((y (reel.h264:picture->yuv420 (nth i pics))) (off (* i fb)) (bad 0))
                    (dotimes (k fb)
                      (unless (= (aref y k) (aref oracle (+ off k))) (incf bad)))
                    (when (zerop bad) (incf exact))))
                (ok (format nil "~a (~a): ~d frames in display order, ~d bit-exact" name what n exact)
                    (and (plusp n) (= exact n))))))
      (error (e) (ok (format nil "~a: ~a" name e) nil)))))

(format t "~&== an MP4's video track, through the container~%")
(handler-case
    (let* ((oracle (slurp "vectors/intra20.yuv"))
           (p (cassette:open-media "vectors/intra20.mp4"))
           (vt (cassette:player-video-track p)))
      (ok "the video track is recognised as decodable H.264"
          (and vt (equal (cassette:track-codec-id vt) "V_MPEG4/ISO/AVC")
               (null (cassette:player-unsupported p))))
      (let ((n 0) (exact 0) (fb nil))
        (loop for pic = (cassette:next-video-frame p)
              while pic
              do (let ((y (cassette:picture->yuv420 pic)))
                   (unless fb (setf fb (length y)))
                   (let ((off (* n fb)) (bad 0))
                     (dotimes (k fb) (unless (= (aref y k) (aref oracle (+ off k))) (incf bad)))
                     (when (zerop bad) (incf exact))
                     (incf n))))
        (ok (format nil "~d frames out of the MP4, ~d bit-exact" n exact)
            (and (= n 20) (= exact 20)))))
  (error (e) (ok (format nil "MP4 video: ~a" e) nil)))

(format t "~&== decoding pictures at the same time gives the same pictures~%")
;; The parallel path is only sound because an I picture is decodable on its own, and the whole
;; claim is that it changes nothing.  So: decode every fixture both ways and compare the samples,
;; not just the frame count — a race that dropped or duplicated a picture would still count right.
(dolist (name '("intra" "crop" "big" "mandel" "coarse"))
  (handler-case
      (let* ((bytes (slurp (format nil "vectors/~a.h264" name)))
             (serial (reel.h264:decode-annex-b bytes :threads 1))
             (parallel (reel.h264:decode-annex-b bytes :threads 8)))
        (ok (format nil "~a: ~d pictures either way" name (length serial))
            (and (plusp (length serial)) (= (length serial) (length parallel))))
        (let ((same t))
          (loop for a in serial for b in parallel
                do (let ((ya (reel.h264:picture->yuv420 a)) (yb (reel.h264:picture->yuv420 b)))
                     (unless (equalp ya yb) (setf same nil))))
          (ok (format nil "~a: every picture is sample-for-sample identical" name) same)))
    (error (e) (ok (format nil "~a parallel: ~a" name e) nil))))

(handler-case
    (let ((aus (nth-value 1 (reel.h264:split-access-units
                             (reel.h264:annex-b-nals (slurp "vectors/intra.h264"))))))
      (ok (format nil "an all-intra stream is recognised as independently decodable (~d units)"
                  (length aus))
          (reel.h264:access-units-independent-p aus)))
  (error (e) (ok (format nil "independence check: ~a" e) nil)))

(handler-case
    (let* ((m (cassette:parse-mp4 (slurp "vectors/fast.mp4")))
           (tr (cassette:mp4-video-track m))
           (r (cassette:make-mp4-reader m))
           (aus (loop repeat 6
                      for f = (cassette:read-next-mp4-frame r)
                      while f
                      when (eq (cassette:frame-track f) tr)
                        collect (reel.h264:length-prefixed-nals (cassette:frame-data f)
                                                                :length-size 4))))
      ;; fast.mp4 has P and B frames: it must NOT be claimed as independently decodable
      (ok "an inter-coded stream is refused by the independence check, not decoded in parallel"
          (not (reel.h264:access-units-independent-p aus))))
  (error (e) (ok (format nil "inter-coded independence check: ~a" e) nil)))

(format t "~&== what is refused is refused, not decoded wrong~%")
;; fast.mp4 is High profile with the 8x8 transform.  That flag is the whole reason it is turned
;; away: with it set, every macroblock carrying luma residual also carries a transform_size_8x8_flag,
;; and a decoder that does not read that bit does not lose the transform, it loses the bitstream —
;; one bit per macroblock, confident garbage from the first picture.  It used to be refused for
;; having B slices, which it also has, but those decode now.
;;
;; What is asserted is the SHAPE of the refusal, not its wording: the track is named as
;; undecodable, no picture is produced, and the file still opens so the rest of it can play.
(handler-case
    (let ((p (cassette:open-media "vectors/fast.mp4")))
      (ok "a High profile stream is named as undecodable rather than decoded"
          (and (null (cassette:player-video-track p))
               (member "V_MPEG4/ISO/AVC" (cassette:player-unsupported p) :test #'equal)
               (null (cassette:next-video-frame p))))
      (ok "and it says why" (let ((n (cassette:player-video-note p)))
                              (and n (search "8x8" n)))))
  (error (e) (ok (format nil "High profile refusal: ~a" e) nil)))

(format t "~&~a~%" (if (zerop *fails*) "H264 OK" (format nil "H264: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
