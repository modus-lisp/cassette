;;;; test-decode.lisp — demux each vector, decode every displayed VP8 frame,
;;;; and compare the I420 output byte-for-byte with ffmpeg's decode.
;;;;   run:  sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-decode.lisp [vector...]
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defvar *vectors* (or (cdr sb-ext:*posix-argv*)
                      '("t1-basic" "t2-altref" "t3-mandel" "t4-testsrc" "t5-av" "bbb360")))
(defvar *max-frames* 90)

(defun compare-vector (name)
  (let* ((path (format nil "vectors/~a.webm" name))
         (oracle-path (format nil "vectors/~a.yuv" name))
         (oracle (cassette::slurp-file oracle-path))
         (p (cassette:open-webm path :audio nil))
         (vt (cassette:player-video-track p))
         (w (cassette:track-width vt)) (h (cassette:track-height vt))
         (frame-bytes (+ (* w h) (* 2 (ceiling w 2) (ceiling h 2))))
         (nframes (min *max-frames* (floor (length oracle) frame-bytes)))
         (exact 0) (worst-frame nil) (worst-max 0) (first-bad nil)
         (t0 (get-internal-real-time)))
    (dotimes (i nframes)
      (let ((pic (cassette:next-video-frame p)))
        (unless pic
          (format t "~&~a: stream ended after ~d frames (oracle has ~d)~%" name i nframes)
          (return))
        (let* ((yuv (cassette:picture->yuv420 pic))
               (off (* i frame-bytes))
               (maxd 0) (ndiff 0))
          (dotimes (k frame-bytes)
            (let ((d (abs (- (aref yuv k) (aref oracle (+ off k))))))
              (when (> d 0) (incf ndiff) (setf maxd (max maxd d)))))
          (cond ((zerop ndiff) (incf exact))
                (t (unless first-bad (setf first-bad (list i ndiff maxd)))
                   (when (> maxd worst-max) (setf worst-max maxd worst-frame i)))))))
    (let ((secs (/ (- (get-internal-real-time) t0) internal-time-units-per-second)))
      (format t "~&~10a ~4dx~4d frames=~3d exact=~3d ~a  (~,2f fps)~%"
              name w h nframes exact
              (if first-bad (format nil "FIRST-BAD frame=~d ndiff=~d maxd=~d worst=~a/~d"
                                    (first first-bad) (second first-bad) (third first-bad) worst-frame worst-max)
                  "OK")
              (/ nframes (max secs 0.001))))
    (= exact nframes)))

(let ((ok t))
  (dolist (v *vectors*)
    (handler-case (unless (compare-vector v) (setf ok nil))
      (error (e) (format t "~&~a: ERROR ~a~%" v e) (setf ok nil))))
  (format t "~&~a~%" (if ok "ALL VECTORS BIT-EXACT" "MISMATCHES"))
  (sb-ext:exit :code (if ok 0 1)))
