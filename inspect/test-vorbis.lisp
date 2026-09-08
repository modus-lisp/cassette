;;;; test-vorbis.lisp — Vorbis THROUGH THE CONTAINERS, which is a different question from Vorbis.
;;;;
;;;; The decoder's own correctness is reed's to assert, and reed does: ten fixtures against ffmpeg
;;;; at correlation 1.000000.  What this file checks is that the two containers actually deliver a
;;;; Vorbis stream to it, because they carry its three configuration headers in completely different
;;;; ways and share no code doing it.  Matroska packs them into CodecPrivate with Xiph lacing — a
;;;; count byte, then all but the last length as chains of 255.  Ogg puts them at the head of the
;;;; audio track's own logical stream, interleaved with the video track's pages.
;;;;
;;;; A .webm from before about 2013 is VP8 and Vorbis.  Until this landed, such a file opened, played
;;;; its picture, and named its audio undecodable.
;;;;
;;;;   sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-vorbis.lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-vorbis-test (:use #:cl)) (in-package #:cassette-vorbis-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))

(defun compare (ref tst)
  (let* ((out (with-output-to-string (s)
                (sb-ext:run-program "python3" (list "../reed/test/compare.py" ref tst)
                                    :search t :output s :error nil)))
         (corr (let ((p (search "corr=" out))) (and p (read-from-string out nil nil :start (+ p 5)))))
         (nrms (let ((p (search "nrms=" out))) (and p (read-from-string out nil nil :start (+ p 5))))))
    (values corr nrms)))

(defun check (file ref video-codec)
  (handler-case
      (let ((p (cassette:open-media file)))
        (ok (format nil "~a: ~a video and A_VORBIS audio, nothing refused" file video-codec)
            (and (cassette:player-video-track p)
                 (equal (cassette:track-codec-id (cassette:player-video-track p)) video-codec)
                 (cassette:player-audio-track p)
                 (equal (cassette:track-codec-id (cassette:player-audio-track p)) "A_VORBIS")
                 (null (cassette:player-unsupported p))))
        (let ((pcm (cassette:decode-all-audio p)))
          (if (null pcm)
              (ok (format nil "~a: the audio track decodes" file) nil)
              (let ((out (format nil "/tmp/cassette-vorbis-~a.wav" (pathname-name file))))
                (reed:write-wav-file pcm out)
                (multiple-value-bind (corr nrms) (compare ref out)
                  (ok (format nil "~a: ~d frames at ~d Hz, corr ~,6f against ffmpeg"
                              file (reed:pcm-frame-count pcm) (reed:pcm-sample-rate pcm) corr)
                      (and corr nrms (> corr 0.99999d0) (< nrms 1d-4))))))))
    (error (e) (ok (format nil "~a: ~a" file e) nil))))

(format t "~&== Vorbis out of Matroska, where the headers are Xiph-laced into CodecPrivate~%")
(check "vectors/vp8-vorbis.webm" "vectors/vp8-vorbis.wav" "V_VP8")

(format t "~&== Vorbis out of Ogg, where the headers are the head of its own stream~%")
(check "vectors/theora-av.ogv" "vectors/theora-av.wav" "V_THEORA")

(format t "~&== and the video still plays alongside it~%")
(handler-case
    (let* ((p (cassette:open-media "vectors/vp8-vorbis.webm"))
           (n 0))
      (loop for pic = (cassette:next-video-frame p) while pic do (incf n))
      (ok (format nil "vp8-vorbis.webm: ~d pictures decoded with the audio track open" n)
          (plusp n)))
  (error (e) (ok (format nil "video alongside audio: ~a" e) nil)))

(format t "~&~:[VORBIS CONTAINER OK~;VORBIS CONTAINER: ~:*~d FAILED~]~%"
        (if (plusp *fails*) *fails* nil))
(when (plusp *fails*) (sb-ext:exit :code 1))
