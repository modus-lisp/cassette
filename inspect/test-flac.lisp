;;;; test-flac.lisp — FLAC through Matroska, where it is byte-for-byte or it is wrong.
;;;;
;;;; The decoder's own correctness is reed's to assert, and reed asserts it twice over: against the
;;;; MD5 the encoder wrote into STREAMINFO, and against ffmpeg byte for byte.  What this file checks
;;;; is that Matroska delivers the stream to it — the native FLAC header lives in CodecPrivate and
;;;; each block holds one frame — and that the result survives the container round trip unchanged.
;;;;
;;;; Being lossless, the assertion here is EQUALP on the samples.  No correlation, no tolerance.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/test-flac.lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-flac-test (:use #:cl)) (in-package #:cassette-flac-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun slurp (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (make-array (file-length s) :element-type '(unsigned-byte 8))))
      (read-sequence b s) b)))

(format t "~&== FLAC out of Matroska, where the header is CodecPrivate and each block is a frame~%")
(handler-case
    (let ((p (cassette:open-media "vectors/vp8-flac.mkv")))
      (ok "vp8-flac.mkv: V_VP8 video and A_FLAC audio, nothing refused"
          (and (cassette:player-video-track p)
               (equal (cassette:track-codec-id (cassette:player-video-track p)) "V_VP8")
               (cassette:player-audio-track p)
               (equal (cassette:track-codec-id (cassette:player-audio-track p)) "A_FLAC")
               (null (cassette:player-unsupported p))))
      (let* ((pcm (cassette:decode-all-audio p))
             (want (slurp "vectors/vp8-flac.s16"))
             (mine (reed:pcm-samples pcm))
             (same (= (length want) (* 2 (length mine)))))
        (when same
          (dotimes (i (length mine))
            (let ((w (let ((u (logior (aref want (* 2 i)) (ash (aref want (1+ (* 2 i))) 8))))
                       (if (>= u #x8000) (- u #x10000) u))))
              (unless (= (aref mine i) w) (setf same nil) (return)))))
        (ok (format nil "vp8-flac.mkv: ~d samples, identical to ffmpeg — not close, identical"
                    (length mine))
            same)))
  (error (e) (ok (format nil "vp8-flac.mkv: ~a" e) nil)))

(format t "~&== and the video still plays alongside it~%")
(handler-case
    (let ((p (cassette:open-media "vectors/vp8-flac.mkv")) (n 0))
      (loop for pic = (cassette:next-video-frame p) while pic do (incf n))
      (ok (format nil "vp8-flac.mkv: ~d pictures decoded with the audio track open" n) (plusp n)))
  (error (e) (ok (format nil "video alongside audio: ~a" e) nil)))

(format t "~&~:[FLAC CONTAINER OK~;FLAC CONTAINER: ~:*~d FAILED~]~%"
        (if (plusp *fails*) *fails* nil))
(when (plusp *fails*) (sb-ext:exit :code 1))
