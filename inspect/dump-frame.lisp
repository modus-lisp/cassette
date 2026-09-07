;;;; dump-frame.lisp — decode a WebM and write chosen frames as PPM files.
;;;;   run:  sbcl --dynamic-space-size 2048 --non-interactive --load inspect/dump-frame.lisp FILE.webm OUTPREFIX N [N...]
(require :asdf)
(push (truename "./") asdf:*central-registry*)
(asdf:load-system :webm-pure)

(destructuring-bind (file prefix &rest wanted) (cdr sb-ext:*posix-argv*)
  (let* ((wanted (mapcar #'parse-integer wanted))
         (last (reduce #'max wanted))
         (p (webm-pure:open-webm file :audio nil))
         (vt (webm-pure:player-video-track p)))
    (format t "~&~a: ~dx~d ~a  duration ~,2fs  ~,2f fps nominal~%"
            file (webm-pure:track-width vt) (webm-pure:track-height vt) (webm-pure:track-codec-id vt)
            (or (webm-pure:player-duration p) 0) (or (webm-pure:player-frame-rate p) 0))
    (loop for i from 0 to last
          for pic = (webm-pure:next-video-frame p)
          while pic
          do (when (member i wanted)
               (let ((path (format nil "~a-~4,'0d.ppm" prefix i)))
                 (webm-pure:write-ppm pic path)
                 (format t "~&wrote ~a (t=~,3fs)~%" path (webm-pure:picture-timestamp pic)))))))
