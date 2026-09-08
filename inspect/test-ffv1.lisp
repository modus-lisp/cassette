;;;; test-ffv1.lisp — FFV1 against ffmpeg, which for a LOSSLESS codec means bit-exact or nothing.
;;;;
;;;; There is no tolerance to argue about here and no transform whose rounding could differ: FFV1
;;;; reconstructs the encoder's input exactly or it is broken.  So the oracle is simply ffmpeg's
;;;; decode of the same file, and the only interesting number is how many frames match.
;;;;
;;;; The fixtures cover what actually varies: chroma layout, slice count, which of the two range
;;;; coder state tables the stream chose, and the reversible colour transform that makes lossless
;;;; RGB possible.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/test-ffv1.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-ffv1-test (:use #:cl)) (in-package #:cassette-ffv1-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))
(defun slurp (p) (cassette::slurp-file p))

(defun check (name what)
  "Decode every frame of a file straight through the codec, and compare the planes."
  (handler-case
      (let* ((m (cassette:parse-webm (slurp (format nil "vectors/~a.mkv" name))))
             (vt (cassette:webm-video-track m))
             (cp (cassette:track-codec-private vt))
             ;; FFV1 in Matroska is wrapped in a Windows BITMAPINFOHEADER, which every muxer does
             ;; and which a reader that does not open it sees as an undecodable track
             (extra (coerce (if (equal (cassette:track-codec-id vt) "V_MS/VFW/FOURCC")
                                (subseq cp 40) cp)
                            '(simple-array (unsigned-byte 8) (*))))
             (cfg (reel.ffv1:parse-configuration extra))
             (dec (progn (setf (reel.ffv1::cfg-width cfg) (cassette:track-width vt)
                               (reel.ffv1::cfg-height cfg) (cassette:track-height vt))
                         (reel.ffv1:make-ffv1-decoder cfg)))
             (oracle (slurp (format nil "vectors/~a.yuv" name)))
             (r (cassette:make-block-reader m))
             (i 0) (exact 0))
        (loop for f = (cassette:read-next-frame r) while f
              do (when (eq (cassette:frame-track f) vt)
                   (let* ((fr (reel.ffv1:decode-frame
                               dec (coerce (cassette:frame-data f)
                                           '(simple-array (unsigned-byte 8) (*)))))
                          (y (reel.ffv1:picture->yuv420 fr))
                          (off (* i (length y))) (bad 0))
                     (when (<= (+ off (length y)) (length oracle))
                       (dotimes (k (length y))
                         (unless (= (aref y k) (aref oracle (+ off k))) (incf bad)))
                       (when (zerop bad) (incf exact)))
                     (incf i))))
        (ok (format nil "~a (~a): ~d frames, ~d bit-exact" name what i exact)
            (and (plusp i) (= i exact))))
    (error (e) (ok (format nil "~a: ~a" name e) nil))))

(format t "~&== FFV1, every frame, against ffmpeg~%")
(check "ffv1-a"       "4:2:0, four slices, the stream's own state table")
(check "ffv1-422"     "4:2:2")
(check "ffv1-16sl"    "sixteen slices")
(check "ffv1-dflttab" "the default state table rather than a transmitted one")
(check "ffv1-rgb"     "RGB through the reversible colour transform")

(format t "~&== through the container, where the picture type is 4:2:0 only~%")
(handler-case
    (let* ((p (cassette:open-media "vectors/ffv1-a.mkv" :audio nil))
           (n 0) (exact 0) (oracle (slurp "vectors/ffv1-a.yuv")) (fb nil))
      (ok "the VFW envelope is opened and the codec inside is named"
          (and (cassette:player-video-track p)
               (equal (cassette:track-codec-id (cassette:player-video-track p)) "V_FFV1")))
      (loop for pic = (cassette:next-video-frame p) while pic
            do (let ((y (cassette:picture->yuv420 pic)))
                 (unless fb (setf fb (length y)))
                 (let ((off (* n fb)) (bad 0))
                   (when (<= (+ off fb) (length oracle))
                     (dotimes (k fb) (unless (= (aref y k) (aref oracle (+ off k))) (incf bad)))
                     (when (zerop bad) (incf exact))))
                 (incf n)))
      (ok (format nil "and it plays: ~d frames, ~d bit-exact" n exact) (and (plusp n) (= n exact))))
  (error (e) (ok (format nil "FFV1 through the container: ~a" e) nil)))

;; The decoder handles 4:2:2 and RGB; the picture this player hands out is 4:2:0, so those are
;; turned away rather than delivered at the wrong size.  Refusing one level up is the same rule.
(handler-case
    (let ((p (cassette:open-media "vectors/ffv1-422.mkv" :audio nil)))
      (ok "a 4:2:2 stream is refused by the player rather than resized"
          (and (null (cassette:player-video-track p))
               (member "V_FFV1" (cassette:player-unsupported p) :test #'equal))))
  (error (e) (ok (format nil "4:2:2 refusal: ~a" e) nil)))

(format t "~&~a~%" (if (zerop *fails*) "FFV1 OK" (format nil "FFV1: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
