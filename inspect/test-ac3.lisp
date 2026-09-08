;;;; test-ac3.lisp — AC-3 through the containers that actually carry it.
;;;;
;;;; The decoder's own correctness is reed's to assert.  What this file checks is that the
;;;; containers deliver it, and the program stream is the interesting one: a DVD does not give AC-3
;;;; a stream of its own.  It goes into PRIVATE STREAM 1 (0xBD) together with DTS, linear PCM and
;;;; the subpicture bitmaps, told apart by a substream byte at the head of every PES payload and
;;;; preceded by a four-byte header — substream id, frame count, first-frame offset — that is not
;;;; part of the audio.  Nor do the PES boundaries fall on AC-3 frame boundaries, so the elementary
;;;; stream has to be cut by sync word and frame length the way MPEG audio is.
;;;;
;;;; Both of those are container work, and neither shows up in Matroska, where one block is one
;;;; frame and there is no substream anything.
;;;;
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load inspect/test-ac3.lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-ac3-test (:use #:cl)) (in-package #:cassette-ac3-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun slurp16 (path)
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let* ((n (floor (file-length s) 2))
           (v (make-array n :element-type '(signed-byte 16))))
      (dotimes (i n v)
        (let ((u (logior (read-byte s) (ash (read-byte s) 8))))
          (setf (aref v i) (if (>= u #x8000) (- u #x10000) u)))))))

(defun corr (mine ref)
  (let* ((n (min (length mine) (length ref)))
         (sa 0d0) (sb 0d0) (saa 0d0) (sbb 0d0) (sab 0d0))
    (dotimes (i n)
      (let ((a (float (aref mine i) 1d0)) (b (float (aref ref i) 1d0)))
        (incf sa a) (incf sb b) (incf saa (* a a)) (incf sbb (* b b)) (incf sab (* a b))))
    (let* ((ma (/ sa n)) (mb (/ sb n))
           (ca (- saa (* n ma ma))) (cb (- sbb (* n mb mb))))
      (if (plusp (* ca cb)) (/ (- sab (* n ma mb)) (sqrt (* ca cb))) 0d0))))

(defun check (file ref-path video-codec expect-frames)
  (handler-case
      (let ((p (cassette:open-media file)))
        (ok (format nil "~a: ~a video and A_AC3 audio, nothing refused" file video-codec)
            (and (cassette:player-video-track p)
                 (equal (cassette:track-codec-id (cassette:player-video-track p)) video-codec)
                 (cassette:player-audio-track p)
                 (equal (cassette:track-codec-id (cassette:player-audio-track p)) "A_AC3")
                 (null (cassette:player-unsupported p))))
        (let ((pcm (cassette:decode-all-audio p)))
          (if (null pcm)
              (ok (format nil "~a: the audio track decodes" file) nil)
              (let ((c (corr (reed:pcm-samples pcm) (slurp16 ref-path))))
                (ok (format nil "~a: ~d frames at ~d Hz, corr ~,6f against ffmpeg"
                            file (reed:pcm-frame-count pcm) (reed:pcm-sample-rate pcm) c)
                    (and (> c 0.9998d0)
                         (>= (reed:pcm-frame-count pcm) expect-frames)))))))
    (error (e) (ok (format nil "~a: ~a" file e) nil))))

(format t "~&== AC-3 out of Matroska, where one block is one frame~%")
(check "vectors/vp8-ac3.mkv" "vectors/vp8-ac3.s16" "V_VP8" 130000)

(format t "~&== AC-3 out of an MPEG program stream, which is what a DVD is~%")
(check "vectors/ac3-ps.vob" "vectors/ac3-ps.s16" "V_MPEG2" 130000)

(format t "~&== and the video still plays alongside it~%")
(handler-case
    (let ((p (cassette:open-media "vectors/ac3-ps.vob")) (n 0))
      (loop for pic = (cassette:next-video-frame p) while pic do (incf n))
      (ok (format nil "ac3-ps.vob: ~d MPEG-2 pictures decoded with the audio track open" n)
          (plusp n)))
  (error (e) (ok (format nil "video alongside audio: ~a" e) nil)))

(format t "~&~:[AC3 CONTAINER OK~;AC3 CONTAINER: ~:*~d FAILED~]~%"
        (if (plusp *fails*) *fails* nil))
(when (plusp *fails*) (sb-ext:exit :code 1))
