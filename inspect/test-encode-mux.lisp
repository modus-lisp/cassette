;;;; test-encode-mux.lisp — the whole pure-CL chain: webrtc-media's VP8 encoder
;;;; -> webm-pure muxer -> .webm file -> ffprobe/ffmpeg accept it, and both
;;;; ffmpeg and webm-pure's decoder reconstruct the same pixels from it.
;;;;   run:  sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-encode-mux.lisp
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(push (truename "../webrtc-media/") asdf:*central-registry*)
(handler-case (progn (asdf:load-system :webm-pure) (asdf:load-system :webrtc-media))
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defun sh (fmt &rest args)
  (multiple-value-bind (out err code)
      (uiop:run-program (apply #'format nil fmt args) :output :string :error-output :string
                                                      :ignore-error-status t)
    (declare (ignore err))
    (values (string-trim '(#\Newline #\Space) out) code)))

(defparameter *w* 176) (defparameter *h* 120) (defparameter *frames* 24) (defparameter *fps* 12)

(defun synth-frame (i)
  "A scene that scrolls by 3 px/frame with a bouncing bright square: (values y u v)."
  (let* ((w *w*) (h *h*) (cw (ceiling w 2)) (ch (ceiling h 2))
         (y (make-array (* w h) :element-type '(unsigned-byte 8)))
         (u (make-array (* cw ch) :element-type '(unsigned-byte 8)))
         (v (make-array (* cw ch) :element-type '(unsigned-byte 8)))
         (dx (* 3 i)) (sq-x (+ 20 (* 5 i))) (sq-y (+ 30 (round (* 30 (sin (/ i 3.0)))))))
    (dotimes (yy h)
      (dotimes (xx w)
        (let ((px (+ xx dx)))
          (setf (aref y (+ (* yy w) xx))
                (if (and (<= sq-x xx (+ sq-x 24)) (<= sq-y yy (+ sq-y 24)))
                    235
                    (logand 255 (+ 40 (* 12 (logand (ash px -4) 7)) (* 6 (logand (ash yy -3) 7)))))))))
    (dotimes (yy ch)
      (dotimes (xx cw)
        (setf (aref u (+ (* yy cw) xx)) (logand 255 (+ 128 (* 20 (logand (ash (+ xx (ash dx -1)) -4) 3))))
              (aref v (+ (* yy cw) xx)) (logand 255 (+ 128 (- (* 16 (logand (ash yy -3) 3))))))))
    (values y u v)))

(defun encode-sequence ()
  "Encode *FRAMES* frames: a key frame then inter frames.  Returns a list of octet vectors."
  (let ((frames '()) (enc nil))
    (dotimes (i *frames*)
      (multiple-value-bind (y u v) (synth-frame i)
        (cond
          ((zerop i)
           (multiple-value-bind (bytes ry ru rv) (vp8:encode-gray-frame y *w* *h* :qi 20 :u u :v v)
             (setf enc (vp8::make-encoder *w* *h*))
             (setf (vp8::ve-ref-y enc) ry (vp8::ve-ref-u enc) ru (vp8::ve-ref-v enc) rv)
             (replace (vp8::ve-prev-y enc) y) (replace (vp8::ve-prev-u enc) u) (replace (vp8::ve-prev-v enc) v)
             (setf (vp8::ve-have-ref enc) t)
             (push bytes frames)))
          (t
           (let ((bytes (vp8::encode-inter-frame enc y u v :qi 16 :motion (cons -3 0))))
             (push bytes frames))))))
    (nreverse frames)))

(let* ((frames (encode-sequence))
       (mx (webm-pure:make-muxer))
       (tn (webm-pure:add-video-track mx :width *w* :height *h* :frame-rate *fps*))
       (out "/tmp/lisp-encoded.webm"))
  (loop for f in frames for i from 0
        do (webm-pure:add-frame mx tn (round (* i 1000000000) *fps*) f
                                :keyframe (webm-pure:vp8-frame-info f)))
  (webm-pure:write-webm-file mx out)
  (format t "~&encoded ~d frames, ~d bytes total, wrote ~a~%"
          (length frames) (reduce #'+ frames :key #'length) out)
  ;; ffmpeg's view of the file
  (format t "~&ffprobe: ~a~%"
          (sh "ffprobe -v error -show_entries stream=codec_name,width,height,r_frame_rate,nb_read_frames -count_frames -of csv=p=0 ~a" out))
  (sh "ffmpeg -v error -y -i ~a -f rawvideo -pix_fmt yuv420p /tmp/lisp-encoded.yuv" out)
  ;; our decoder vs ffmpeg
  (let* ((oracle (webm-pure::slurp-file "/tmp/lisp-encoded.yuv"))
         (p (webm-pure:open-webm out))
         (fb (+ (* *w* *h*) (* 2 (ceiling *w* 2) (ceiling *h* 2))))
         (n (floor (length oracle) fb))
         (exact 0))
    (dotimes (i n)
      (let ((pic (webm-pure:next-video-frame p)))
        (when (and pic (equalp (webm-pure:picture->yuv420 pic) (subseq oracle (* i fb) (* (1+ i) fb))))
          (incf exact))))
    (format t "~&decoded: ffmpeg=~d frames, webm-pure bit-exact on ~d~%" n exact)
    (webm-pure:write-ppm (progn (setf p (webm-pure:open-webm out))
                                (loop repeat 12 do (webm-pure:next-video-frame p))
                                (webm-pure:next-video-frame p))
                         "/tmp/lisp-encoded-0012.ppm")
    (let ((ok (and (= n *frames*) (= exact n))))
      (format t "~&~a~%" (if ok "ENCODE->MUX->DECODE OK" "ENCODE CHAIN PROBLEM"))
      (sb-ext:exit :code (if ok 0 1)))))
