;;;; test-mpeg2.lisp — reel's MPEG-1/MPEG-2 decoder against ffmpeg, and the two system containers.
;;;;
;;;; THE ORACLE NEEDS AN ARGUMENT HERE THAT THE OTHER CODECS DO NOT: `-idct simple'.
;;;;
;;;; H.264 and VP8 specify exact integer inverse transforms, so any conforming decoder produces the
;;;; same bytes and `ffmpeg -i x -f rawvideo' is an unambiguous oracle.  MPEG-2 does not: ISO/IEC
;;;; 13818-2 requires only the accuracy bounds of IEEE 1180, and ffmpeg ships several transforms
;;;; that all meet them and disagree in the last bit.  Comparing against whichever one ffmpeg picked
;;;; for this machine would be comparing against the weather.  Naming one makes the comparison mean
;;;; something, and this decoder implements that one exactly — see src/mpeg2/idct.lisp for the three
;;;; places its arithmetic is not what the mathematics alone would suggest.
;;;;
;;;; `-vsync 0' matters too: without it ffmpeg may duplicate a frame to fill a rate, and then every
;;;; frame after the duplicate compares against its neighbour and the whole file looks wrong.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/test-mpeg2.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-mpeg2-test (:use #:cl)) (in-package #:cassette-mpeg2-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))
(defun slurp (p) (cassette::slurp-file p))

(defun compare (name pics oracle what)
  (if (null pics)
      (ok (format nil "~a: decoded no pictures" name) nil)
      (let* ((fb (length (first pics)))
             (n (min (length pics) (floor (length oracle) fb)))
             (exact 0) (worst 0))
        (dotimes (i n)
          (let ((y (nth i pics)) (off (* i fb)) (bad 0))
            (dotimes (k fb)
              (let ((d (abs (- (aref y k) (aref oracle (+ off k))))))
                (when (plusp d) (incf bad) (setf worst (max worst d)))))
            (when (zerop bad) (incf exact))))
        (ok (format nil "~a (~a): ~d frames, ~d bit-exact~@[, worst sample error ~d~]"
                    name what n exact (and (plusp worst) worst))
            (and (plusp n) (= exact n) (= n (floor (length oracle) fb)))))))

(format t "~&== elementary streams, every frame, against ffmpeg -idct simple~%")
(dolist (spec '(("m2-basic"      . "MPEG-2, I P and B pictures")
                ("m2-mpeg1"      . "MPEG-1, which is the same bitstream without the extensions")
                ("m2-mpeg1-wide" . "MPEG-1 with slices that run across macroblock rows")
                ("m2-ilace"      . "interlaced: field DCT and field motion in a frame picture")
                ("m2-altscan"    . "the alternate scan")
                ("m2-ivlc"       . "the alternative intra coefficient table")
                ("m2-cqm"        . "custom quantiser matrices")
                ("m2-nlq"        . "the non-linear quantiser ladder")
                ("m2-cif"        . "352x288, several slices per row")))
  (destructuring-bind (name . what) spec
    (handler-case
        (let ((pics (mapcar #'reel.mpeg2:picture->yuv420
                            (reel.mpeg2:decode-elementary-stream
                             (slurp (format nil "vectors/~a.m2v" name))))))
          (compare name pics (slurp (format nil "vectors/~a.yuv" name)) what))
      (error (e) (ok (format nil "~a: ~a" name e) nil)))))

(format t "~&== program and transport streams, demuxed and decoded end to end~%")
;; The point of these four is that the same picture arrives through four different framings.  A
;; program stream packs PES packets for a medium that does not lose bytes; a transport stream cuts
;; them into 188-byte cells for one that does; and neither marks where a picture begins, so the
;; payload is reassembled and cut at the codec's own boundaries before any of it is decoded.
(dolist (spec '(("m2-ps.mpg"  . "MPEG-2 in a program stream")
                ("m2-ts.ts"   . "MPEG-2 in a transport stream")
                ("m1-ps.mpg"  . "MPEG-1 in a program stream")
                ("h264-ts.ts" . "H.264 in a transport stream, which is what broadcast is")))
  (destructuring-bind (file . what) spec
    (handler-case
        (let* ((base (subseq file 0 (position #\. file)))
               (p (cassette:open-media (format nil "vectors/~a" file) :audio nil))
               (pics '()))
          (loop for pic = (cassette:next-video-frame p) while pic
                do (push (cassette:picture->yuv420 pic) pics))
          (compare file (nreverse pics) (slurp (format nil "vectors/~a.yuv" base)) what))
      (error (e) (ok (format nil "~a: ~a" file e) nil)))))

(format t "~&== AVI, which names its codec with four characters and nothing else~%")
;; The oracle files are named `h264-avi.yuv' rather than `h264.yuv' because the fixture is
;; `h264.avi' and there is already an `h264' elsewhere in this directory.
(dolist (spec '(("h264.avi" "h264-avi" . "H.264 in an AVI")
                ("m2.avi"   "m2-avi"   . "MPEG-2 in an AVI")
                ("asp.avi"  "asp-avi"  . "MPEG-4 Part 2 in an AVI, which is what DivX and XviD are")))
  (destructuring-bind (file base . what) spec
    (handler-case
        (let* ((p (cassette:open-media (format nil "vectors/~a" file) :audio nil))
               (pics '()))
          (loop for pic = (cassette:next-video-frame p) while pic
                do (push (cassette:picture->yuv420 pic) pics))
          (compare file (nreverse pics) (slurp (format nil "vectors/~a.yuv" base)) what))
      (error (e) (ok (format nil "~a: ~a" file e) nil)))))

(handler-case
    (let ((a (cassette:parse-avi (slurp "vectors/asp.avi"))))
      ;; DIVX, DX50, XVID, FMP4 and MP4V are all ISO/IEC 14496-2 written by different encoders.
      ;; AVI records which ENCODER wrote the file, not what it wrote, so the table has to.
      (ok "XVID is recognised as MPEG-4 Part 2 rather than as a codec of its own"
          (equal (cassette:track-codec-id (cassette:avi-video-track a)) "V_MPEG4/ISO/ASP"))
      (ok "and the MP3 track beside it is named too"
          (equal (cassette:track-codec-id (cassette:avi-audio-track a)) "A_MPEG/L3"))
      ;; AVI carries no timestamps at all: a chunk's time is counted from the ones before it
      (let* ((frames (cassette:avi-frames a))
             (video (remove-if-not (lambda (f) (= 1 (cassette:track-type (cassette:frame-track f))))
                                   frames)))
        (ok "video times are counted, forty milliseconds apart at 25 frames a second"
            (and (> (length video) 3)
                 (= 0 (cassette:frame-timecode (first video)))
                 (= 40000 (cassette:frame-timecode (second video)))))))
  (error (e) (ok (format nil "AVI inventory: ~a" e) nil)))

(format t "~&== MPEG-4 Part 2 elementary streams, every frame, against ffmpeg -idct simple~%")
(dolist (spec '(("mp4v-i"    . "one motion vector per macroblock")
                ("mp4v-4mv"  . "four motion vectors per macroblock")
                ("mp4v-b"    . "B pictures, including direct mode")
                ("mp4v-mq"   . "the MPEG-style quantiser instead of H.263\'s")
                ("mp4v-full" . "all of it at once, at a fine quantiser")
                ("mp4v-big"  . "352x288 at a constant bit rate")
                ("mp4v-asp"  . "Advanced Simple: quarter-sample motion, four vectors and B pictures")))
  (destructuring-bind (name . what) spec
    (handler-case
        (let ((pics (mapcar #'reel.mpeg4:picture->yuv420
                            (reel.mpeg4:decode-elementary-stream
                             (slurp (format nil "vectors/~a.m4v" name))))))
          (compare name pics (slurp (format nil "vectors/~a.yuv" name)) what))
      (error (e) (ok (format nil "~a: ~a" name e) nil)))))

(format t "~&== a program stream and a transport stream, video AND sound~%")
;; The point of having BOTH is that the answer must be identical: the same picture and the same
;; audio arrive through two containers that have nothing in common above the PES packet.  MPEG audio
;; Layer II is what a DVD and a broadcast capture actually carry, and until it decoded, a file like
;; this played perfectly and silently.
(handler-case
    (let (a b)
      (dolist (f '("vectors/m2-av.mpg" "vectors/m2-av.ts"))
        (let* ((p (cassette:open-media f))
               (vt (cassette:player-video-track p))
               (at (cassette:player-audio-track p)))
          (ok (format nil "~a: MPEG-2 video and Layer II audio, both decodable" f)
              (and vt at
                   (equal (cassette:track-codec-id vt) "V_MPEG2")
                   ;; the container says only "MPEG audio"; the LAYER comes from the frame header
                   (equal (cassette:track-codec-id at) "A_MPEG/L2")
                   (null (cassette:player-unsupported p))))
          (let ((pcm (cassette:decode-all-audio p)))
            (if a (setf b pcm) (setf a pcm)))))
      (ok "and the two containers give sample-identical audio"
          (and a b (equalp (reed:pcm-samples a) (reed:pcm-samples b))
               (plusp (length (reed:pcm-samples a))))))
  (error (e) (ok (format nil "program and transport streams with audio: ~a" e) nil)))

(format t "~&== what the containers say is in them~%")
(handler-case
    (multiple-value-bind (m frames) (cassette:parse-mpegsys (slurp "vectors/h264-ts.ts"))
      (let ((vt (cassette:mpegsys-video-track m)))
        (ok "a transport stream's program map names its video track"
            (and vt (equal (cassette:track-codec-id vt) "V_MPEG4/ISO/AVC")))
        ;; the size is not in the container at all: it comes from a parameter set inside the stream
        (ok "and the picture size comes from the sequence parameter set inside it"
            (and vt (= 176 (cassette:track-width vt)) (= 144 (cassette:track-height vt))))
        (ok "and every access unit is one picture, not one packet"
            (= (length frames) 15))))
  (error (e) (ok (format nil "transport stream inventory: ~a" e) nil)))

(handler-case
    (multiple-value-bind (m frames) (cassette:parse-mpegsys (slurp "vectors/m2-ps.mpg"))
      (declare (ignore frames))
      (let ((vt (cassette:mpegsys-video-track m)))
        ;; a program stream has no table of contents, so the codec is decided by probing the payload
        (ok "a program stream's codec is recognised without a table of contents"
            (and vt (equal (cassette:track-codec-id vt) "V_MPEG2")
                 (= 176 (cassette:track-width vt))))))
  (error (e) (ok (format nil "program stream inventory: ~a" e) nil)))

(format t "~&~a~%" (if (zerop *fails*) "MPEG2 OK" (format nil "MPEG2: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
