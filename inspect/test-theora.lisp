;;;; test-theora.lisp — Theora against ffmpeg, every frame, bit for bit.
;;;;
;;;; Theora leaves nothing to a decoder's judgement.  Its inverse transform is fixed point with the
;;;; multipliers written into the format, so two conforming decoders agree exactly and there is no
;;;; tolerance worth arguing about: the only interesting number is how many frames match.
;;;;
;;;; The fixtures cover what varies between real files.  The quantiser matters most, because a
;;;; picture may carry up to THREE quantiser indices and choose between them per block, and whether
;;;; it bothers depends on the quality setting — so a low-quality and a high-quality encode of the
;;;; same source exercise different code.  A picture that is not a whole number of macroblocks is
;;;; here because Theora then codes a larger frame and displays a window inside it, and getting the
;;;; window's origin wrong is invisible until the size is odd.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/test-theora.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-theora-test (:use #:cl)) (in-package #:cassette-theora-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))
(defun slurp (p) (cassette::slurp-file p))

(defun check (name)
  "Decode every picture of one file and compare all three planes against ffmpeg's."
  (handler-case
      (let* ((o (cassette:parse-ogg (slurp (format nil "vectors/~a.ogv" name))))
             (tr (cassette:ogg-video-track o))
             (st (cassette:ogg-stream-for o tr))
             (info (reel.theora:parse-headers (cassette:os-headers st)))
             (d (reel.theora:make-theora-decoder info))
             (oracle (slurp (format nil "vectors/~a.yuv" name)))
             (n 0) (exact 0) (keys 0) (short nil))
        (dolist (pk (cassette:os-packets st))
          (let* ((f (reel.theora:decode-frame d (car pk)))
                 (w (reel.theora:fr-width f)) (h (reel.theora:fr-height f))
                 (cw (reel.theora:fr-cwidth f)) (ch (reel.theora:fr-cheight f))
                 (size (+ (* w h) (* 2 cw ch)))
                 (off (* n size)))
            (when (reel.theora:fr-keyframe f) (incf keys))
            (if (> (+ off size) (length oracle))
                (setf short t)
                (let ((bad 0))
                  (dotimes (k (* w h))
                    (unless (= (aref (reel.theora:fr-y f) k) (aref oracle (+ off k))) (incf bad)))
                  (dotimes (k (* cw ch))
                    (unless (= (aref (reel.theora:fr-u f) k) (aref oracle (+ off (* w h) k)))
                      (incf bad))
                    (unless (= (aref (reel.theora:fr-v f) k)
                               (aref oracle (+ off (* w h) (* cw ch) k)))
                      (incf bad)))
                  (when (zerop bad) (incf exact))))
            (incf n)))
        (ok (format nil "~a: ~d frames (~d key) at ~dx~d, ~d bit-exact"
                    name n keys (reel.theora:inf-picture-width info)
                    (reel.theora:inf-picture-height info) exact)
            (and (plusp n) (= n exact) (not short))))
    (error (e) (ok (format nil "~a: ~a" name e) nil))))

(format t "~&== Theora, every frame against ffmpeg~%")
(dolist (name '("theora" "theora-q2" "theora-q9" "theora-odd" "theora-320" "theora-bars"))
  (check name))

;;; ---- what the headers turn away -----------------------------------------------------------------
;;;
;;; Every refusal is on the FLAG that turns the thing on, so a stream that does not use it decodes.

(defun expect-refusal (what thunk)
  (handler-case (progn (funcall thunk) (ok what nil))
    (reel.theora::theora-error (e)
      (ok (format nil "~a — ~a" what (reel.theora::theora-error-message e)) t))))

(format t "~&== refusals~%")
(let* ((o (cassette:parse-ogg (slurp "vectors/theora.ogv")))
       (st (cassette:ogg-stream-for o (cassette:ogg-video-track o)))
       (id (first (cassette:os-headers st))))
  ;; the chroma format is two bits, three from the top of byte forty-one: six bits of quality and
  ;; five of key-frame shift precede it and neither is byte aligned
  (let ((broken (copy-seq id)))
    (setf (aref broken 41) (logior (aref broken 41) #x18))
    (expect-refusal "a chroma format other than 4:2:0 is named, not resampled"
                    (lambda () (reel.theora:parse-identification broken))))
  ;; before 3.2.0 the picture was stored the other way up, which is a different decoder
  (let ((broken (copy-seq id)))
    (setf (aref broken 7) 3 (aref broken 8) 1 (aref broken 9) 1)
    (expect-refusal "a pre-3.2.0 bitstream is refused"
                    (lambda () (reel.theora:parse-identification broken))))
  ;; and a header packet handed to the frame decoder is a caller's mistake, said out loud
  (let* ((info (reel.theora:parse-headers (cassette:os-headers st)))
         (d (reel.theora:make-theora-decoder info)))
    (expect-refusal "a header packet fed to the frame decoder is refused"
                    (lambda () (reel.theora:decode-frame d id)))))

(format t "~&== through the container, where a .ogv is just a file that plays~%")
(handler-case
    (let* ((p (cassette:open-media "vectors/theora-av.ogv"))
           (oracle (slurp "vectors/theora-av.yuv"))
           (n 0) (exact 0) (fb nil))
      ;; This assertion used to be that the Vorbis track was NAMED and not decoded, and it
      ;; failed the day the Vorbis decoder landed — which is the assertion doing its job.  An
      ;; `.ogv' is now a file where both tracks play.
      (ok "both tracks of the .ogv are decodable, video and audio"
          (and (cassette:player-video-track p)
               (equal (cassette:track-codec-id (cassette:player-video-track p)) "V_THEORA")
               (cassette:player-audio-track p)
               (equal (cassette:track-codec-id (cassette:player-audio-track p)) "A_VORBIS")
               (null (cassette:player-unsupported p))))
      (loop for pic = (cassette:next-video-frame p) while pic
            do (let ((y (cassette:picture->yuv420 pic)))
                 (unless fb (setf fb (length y)))
                 (let ((off (* n fb)) (bad 0))
                   (when (<= (+ off fb) (length oracle))
                     (dotimes (k fb) (unless (= (aref y k) (aref oracle (+ off k))) (incf bad)))
                     (when (zerop bad) (incf exact))))
                 (incf n)))
      (ok (format nil "and it plays: ~d frames, ~d bit-exact" n exact)
          (and (plusp n) (= n exact))))
  (error (e) (ok (format nil "Theora through the container: ~a" e) nil)))

(format t "~&~a~%" (if (zerop *fails*) "THEORA OK" (format nil "THEORA: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
