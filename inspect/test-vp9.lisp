;;;; test-vp9.lisp — VP9's headers, which are all that decodes so far.
;;;;
;;;; The uncompressed header is worth having on its own even before a picture can be produced: it
;;;; carries the frame size, the reference indices, the quantiser, the loop filter and the tiling,
;;;; and it is plain bits — no arithmetic coder, no probability state, nothing to carry forward.  A
;;;; container or a router can read all of that without a decoder, which is the point of VP9 putting
;;;; it there.
;;;;
;;;; So this suite checks what can be checked: that every frame of a real file parses, that the
;;;; fields agree with what the encoder was told, that a superframe splits into its parts, and that
;;;; everything not yet built is refused by name.
;;;;
;;;;   sbcl --dynamic-space-size 3072 --non-interactive --load inspect/test-vp9.lisp

(require :asdf)
(push (truename "./") asdf:*central-registry*)
(handler-case (asdf:load-system :cassette)
  (error (e) (format t "~&LOAD-ERR ~a~%" e) (sb-ext:exit :code 1)))

(defpackage #:cassette-vp9-test (:use #:cl)) (in-package #:cassette-vp9-test)

(defvar *fails* 0)
(defun ok (name p) (format t "~&  ~:[FAIL~;ok  ~] ~a~%" p name) (unless p (incf *fails*)))
(defun slurp (p) (cassette::slurp-file p))

(defun headers-of (path)
  "Every frame header of a WebM file's VP9 track, superframes split."
  (let* ((m (cassette:parse-webm (slurp path)))
         (vt (cassette:webm-video-track m))
         (r (cassette:make-block-reader m))
         (refs (make-array 8 :initial-element nil))
         (out '()))
    (loop for f = (cassette:read-next-frame r) while f
          do (when (eq (cassette:frame-track f) vt)
               (let ((data (coerce (cassette:frame-data f) '(simple-array (unsigned-byte 8) (*)))))
                 (dolist (p (reel.vp9:split-superframe data))
                   (let ((h (reel.vp9:parse-header data (car p) (cdr p) :ref-sizes refs)))
                     (push h out)
                     ;; a frame that only shows a reference changes no slot
                     (unless (reel.vp9:h-show-existing h)
                       (dotimes (i 8)
                         (when (logbitp i (reel.vp9:h-refresh-mask h))
                           (setf (aref refs i)
                                 (cons (reel.vp9:h-width h) (reel.vp9:h-height h)))))))))))
    (values (nreverse out) vt)))

(defun check (name w h &key (tiles 1))
  (handler-case
      (multiple-value-bind (hs vt) (headers-of (format nil "vectors/~a.webm" name))
        (ok (format nil "~a: ~d frames parse, none refused" name (length hs)) (plusp (length hs)))
        (ok (format nil "~a: the container and the bitstream agree on ~dx~d" name w h)
            (and (= w (cassette:track-width vt)) (= h (cassette:track-height vt))
                 (every (lambda (x) (or (reel.vp9:h-show-existing x)
                                        (and (= w (reel.vp9:h-width x))
                                             (= h (reel.vp9:h-height x)))))
                        hs)))
        (ok (format nil "~a: the first frame is a key frame and the rest are not" name)
            (and (reel.vp9:h-keyframe (first hs))
                 (notany #'reel.vp9:h-keyframe (rest hs))))
        (ok (format nil "~a: profile 0, and ~d tile column~:p" name tiles)
            (and (every (lambda (x) (zerop (reel.vp9:h-profile x))) hs)
                 (= tiles (ash 1 (reel.vp9:h-log2-tile-cols (first hs))))))
        ;; a compressed header that runs past its frame is the loudest sign of a desynchronised
        ;; parse, and every frame is checked for it inside PARSE-HEADER
        (ok (format nil "~a: every compressed header fits inside its frame" name)
            (every (lambda (x) (or (reel.vp9:h-show-existing x)
                                   (plusp (reel.vp9:h-compressed-size x))))
                   hs)))
    (error (e) (ok (format nil "~a: ~a" name e) nil))))

(format t "~&== VP9 frame headers~%")
(check "vp9-cif" 176 144)
(check "vp9-720" 1280 720 :tiles 4)
(check "vp9-switch" 352 288)

;;; ---- the compressed header ----------------------------------------------------------------------
;;;
;;; There is one very strong check available here without a picture to compare against: the
;;; compressed header is an arithmetic-coded partition of a length the uncompressed header states, and
;;; a correct parse consumes it EXACTLY.  Any field read at the wrong width, in the wrong order, or
;;; under the wrong condition leaves the coder somewhere else — and across thirty headers ranging
;;; from six bytes to nearly three hundred, landing on the last byte every time is not a coincidence.
;;;
;;; (The coder may sit one byte past the end: it reads ahead by one and always has.)

(defun check-compressed (name &key (want-tx nil))
  (handler-case
      (let* ((m (cassette:parse-webm (slurp (format nil "vectors/~a.webm" name))))
             (vt (cassette:webm-video-track m))
             (r (cassette:make-block-reader m))
             (refs (make-array 8 :initial-element nil))
             (ctxs (let ((v (make-array 4)))
                     (dotimes (i 4 v) (setf (aref v i) (reel.vp9::make-default-context)))))
             (n 0) (worst 0) (txs '()))
        (loop for f = (cassette:read-next-frame r) while f
              do (when (eq (cassette:frame-track f) vt)
                   (let ((data (coerce (cassette:frame-data f)
                                       '(simple-array (unsigned-byte 8) (*)))))
                     (dolist (part (reel.vp9:split-superframe data))
                       (let ((h (reel.vp9:parse-header data (car part) (cdr part)
                                                       :ref-sizes refs)))
                         (unless (reel.vp9:h-show-existing h)
                           (when (reel.vp9:h-keyframe h)
                             (dotimes (i 4)
                               (setf (aref ctxs i) (reel.vp9::make-default-context))))
                           (multiple-value-bind (fp c)
                               (reel.vp9::read-compressed-header
                                data (+ (car part) (reel.vp9::h-header-bytes h))
                                (reel.vp9:h-compressed-size h) h
                                (aref ctxs (reel.vp9::h-frame-context h)))
                             (pushnew (reel.vp9::fp-tx-mode fp) txs)
                             (setf worst (max worst (abs (- (reel.vp9::bd-end c)
                                                            (reel.vp9::bd-pos c)))))))
                         (dotimes (i 8)
                           (when (logbitp i (reel.vp9:h-refresh-mask h))
                             (setf (aref refs i) (cons (reel.vp9:h-width h)
                                                       (reel.vp9:h-height h)))))
                         (incf n))))))
        (ok (format nil "~a: ~d compressed headers, each consuming its partition exactly" name n)
            (and (plusp n) (<= worst 1)))
        (when want-tx
          (ok (format nil "~a: the transform mode is ~d, which exercises its probability updates"
                      name want-tx)
              (member want-tx txs))))
    (error (e) (ok (format nil "~a compressed header: ~a" name e) nil))))

(format t "~&== VP9 compressed headers~%")
(check-compressed "vp9-cif" :want-tx 3)
(check-compressed "vp9-720")
;; a slower encode reaches TX_SWITCHABLE, which is the only setting that sends transform-size
;; probabilities at all
(check-compressed "vp9-switch" :want-tx 4)

;;; ---- superframes -------------------------------------------------------------------------------
;;;
;;; A superframe is how VP9 delivers a frame that produces no picture.  Nothing in this repository's
;;; fixtures happens to contain one — libvpx here does not emit alt-refs for these clips — so one is
;;; BUILT, from two real frames and a real index, which exercises the same code the same way.

(format t "~&== VP9 tiles, the partition tree and coefficients (intra frames)~%")

;;; The same invariant, one level down and much sharper.  Every tile is its own arithmetic-coded
;;; partition of a length the frame states, so a correct walk of the partition quadtree — every block
;;; mode, every transform size, every coefficient of every transform block — ends on the last byte of
;;; it.  Getting one symbol wrong anywhere in a tile of thirty-four thousand bytes does not end there.

(defun check-tiles (name)
  (handler-case
      (let* ((m (cassette:parse-webm (slurp (format nil "vectors/~a.webm" name))))
             (vt (cassette:webm-video-track m))
             (r (cassette:make-block-reader m))
             (refs (make-array 8 :initial-element nil))
             (ctxs (let ((v (make-array 4)))
                     (dotimes (i 4 v) (setf (aref v i) (reel.vp9:make-default-context)))))
             (intra 0) (blocks 0) (slack 0))
        (loop for f = (cassette:read-next-frame r) while f
              do (when (eq (cassette:frame-track f) vt)
                   (let ((data (coerce (cassette:frame-data f)
                                       '(simple-array (unsigned-byte 8) (*)))))
                     (dolist (part (reel.vp9:split-superframe data))
                       (let ((h (reel.vp9:parse-header data (car part) (cdr part)
                                                       :ref-sizes refs)))
                         (unless (reel.vp9:h-show-existing h)
                           (when (reel.vp9:h-keyframe h)
                             (dotimes (i 4)
                               (setf (aref ctxs i) (reel.vp9:make-default-context))))
                           (when (or (reel.vp9:h-keyframe h) (reel.vp9:h-intra-only h))
                             (let* ((hb (+ (car part) (reel.vp9:h-header-bytes h)))
                                    (fp (reel.vp9:read-compressed-header
                                         data hb (reel.vp9:h-compressed-size h) h
                                         (aref ctxs (reel.vp9:h-frame-context h))))
                                    (st (reel.vp9:make-state h fp)))
                               (reel.vp9:decode-tiles
                                data (+ hb (reel.vp9:h-compressed-size h)) (cdr part) st)
                               (incf intra)
                               (incf blocks (reel.vp9:st-blocks st))
                               (setf slack (max slack (reel.vp9:st-tile-slack st))))))
                         (dotimes (i 8)
                           (when (logbitp i (reel.vp9:h-refresh-mask h))
                             (setf (aref refs i) (cons (reel.vp9:h-width h)
                                                       (reel.vp9:h-height h))))))))))
        (ok (format nil "~a: ~d intra frame~:p, ~d blocks, every tile consumed exactly"
                    name intra blocks)
            (and (plusp intra) (plusp blocks) (<= slack 1))))
    (error (e) (ok (format nil "~a tiles: ~a" name e) nil))))

(check-tiles "vp9-cif")
(check-tiles "vp9-720")      ; four tile columns, so the tile length fields are exercised too
(check-tiles "vp9-switch")

(format t "~&== VP9 reconstruction: a decoded picture, compared~%")

;;; Every INTRA frame of every fixture, against ffmpeg, sample for sample.  That covers the whole
;;; decoder except inter prediction: the partition walk, every block mode, every coefficient, all
;;; four transform sizes with both the DCT and the ADST, the fifteen intra predictors with their
;;; substitutions for missing neighbours, the loop filter with all three of its widths, and the crop.
;;;
;;; The lossless fixture is here for two reasons of its own: it is the only one that exercises the
;;; Walsh-Hadamard, and — because a lossless frame's filter level is zero — it was the only one that
;;; could be compared at all until the loop filter existed.

(defun check-picture (name)
  (handler-case
      (let* ((m (cassette:parse-webm (slurp (format nil "vectors/~a.webm" name))))
             (vt (cassette:webm-video-track m))
             (r (cassette:make-block-reader m))
             (ctxs (let ((v (make-array 4)))
                     (dotimes (i 4 v) (setf (aref v i) (reel.vp9:make-default-context)))))
             (want (slurp (format nil "vectors/~a.yuv" name)))
             (refs (make-array 8 :initial-element nil))
             (n 0) (exact 0) (frame 0))
        (loop for f = (cassette:read-next-frame r) while f
              do (when (eq (cassette:frame-track f) vt)
                   (let ((data (coerce (cassette:frame-data f)
                                       '(simple-array (unsigned-byte 8) (*)))))
                     (dolist (part (reel.vp9:split-superframe data))
                       (let ((h (reel.vp9:parse-header data (car part) (cdr part)
                                                       :ref-sizes refs)))
                         (dotimes (i 8)
                           (when (and (not (reel.vp9:h-show-existing h))
                                      (logbitp i (reel.vp9:h-refresh-mask h)))
                             (setf (aref refs i) (cons (reel.vp9:h-width h)
                                                       (reel.vp9:h-height h)))))
                         (cond
                           ((reel.vp9:h-show-existing h))
                           ((or (reel.vp9:h-keyframe h) (reel.vp9:h-intra-only h))
                            (when (reel.vp9:h-keyframe h)
                              (dotimes (i 4)
                                (setf (aref ctxs i) (reel.vp9:make-default-context))))
                            (let* ((hb (+ (car part) (reel.vp9:h-header-bytes h)))
                                   (fp (reel.vp9:read-compressed-header
                                        data hb (reel.vp9:h-compressed-size h) h
                                        (aref ctxs (reel.vp9:h-frame-context h))))
                                   (st (reel.vp9:make-state h fp)))
                              (reel.vp9:decode-tiles
                               data (+ hb (reel.vp9:h-compressed-size h)) (cdr part) st)
                              (let* ((y (reel.vp9:picture->yuv420 (reel.vp9:st-frame st)))
                                     (off (* frame (length y)))
                                     (bad 0))
                                (when (<= (+ off (length y)) (length want))
                                  (dotimes (k (length y))
                                    (unless (= (aref y k) (aref want (+ off k))) (incf bad)))
                                  (when (zerop bad) (incf exact)))
                                (incf n)))
                            (incf frame))
                           (t (incf frame))))))))
        (ok (format nil "~a: ~d intra frame~:p, ~d bit-exact against ffmpeg" name n exact)
            (and (plusp n) (= n exact))))
    (error (e) (ok (format nil "~a picture: ~a" name e) nil))))

(check-picture "vp9-cif")
(check-picture "vp9-720")        ; 1280x720 across four tile columns
(check-picture "vp9-switch")     ; the transform mode is switchable here
(check-picture "vp9-lossless")   ; the Walsh-Hadamard, and a filter level of zero

(format t "~&== superframes~%")
(handler-case
    (let* ((m (cassette:parse-webm (slurp "vectors/vp9-cif.webm")))
           (vt (cassette:webm-video-track m))
           (r (cassette:make-block-reader m))
           (frames '()))
      (loop for f = (cassette:read-next-frame r) while f
            do (when (and (eq (cassette:frame-track f) vt) (< (length frames) 2))
                 (push (coerce (cassette:frame-data f) '(simple-array (unsigned-byte 8) (*)))
                       frames)))
      (setf frames (nreverse frames))
      (let* ((a (first frames)) (b (second frames))
             (n 2) (len-size 4)
             (marker (logior #xc0 (ash (1- len-size) 3) (1- n)))
             (idx (concatenate '(simple-array (unsigned-byte 8) (*))
                               (vector marker)
                               (loop for f in frames
                                     append (loop for k below len-size
                                                  collect (logand (ash (length f) (* -8 k)) 255)))
                               (vector marker)))
             (packet (concatenate '(simple-array (unsigned-byte 8) (*)) a b idx))
             (parts (reel.vp9:split-superframe packet)))
        (ok "a two-frame superframe splits into exactly its two frames"
            (equal parts (list (cons 0 (length a))
                               (cons (length a) (+ (length a) (length b))))))
        (ok "and both halves parse as frames"
            (let ((h0 (reel.vp9:parse-header packet 0 (length a)))
                  (h1 (reel.vp9:parse-header packet (length a) (+ (length a) (length b))
                                             :ref-sizes (make-array 8 :initial-element '(176 . 144)))))
              (and (reel.vp9:h-keyframe h0) (not (reel.vp9:h-keyframe h1)))))
        (ok "a packet with no index is one frame"
            (equal (reel.vp9:split-superframe a) (list (cons 0 (length a)))))))
  (error (e) (ok (format nil "superframe: ~a" e) nil)))

;;; ---- what is refused ---------------------------------------------------------------------------
;;;
;;; These are kept as tests so that they stop being true when the work lands.

(format t "~&== refusals, kept as tests until the frame decoder lands~%")
(defun expect-refusal (what thunk)
  (handler-case (progn (funcall thunk) (ok what nil))
    (reel.vp9:vp9-error (e)
      (ok (format nil "~a — ~a" what (reel.vp9:vp9-error-message e)) t))))

(let ((profile2 (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
  ;; frame marker 10, then profile bits low-first: 0 then 1 makes profile 2
  (setf (aref profile2 0) #b10010000)
  (expect-refusal "a profile other than 0 is named, not guessed at"
                  (lambda () (reel.vp9:parse-header profile2 0 16))))

(let ((junk (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
  (expect-refusal "a packet that is not a VP9 frame is refused on its marker"
                  (lambda () (reel.vp9:parse-header junk 0 16))))

(handler-case
    (let ((p (cassette:open-media "vectors/vp9-cif.webm" :audio nil)))
      (ok "the player names V_VP9 as undecodable rather than guessing"
          (and (null (cassette:player-video-track p))
               (member "V_VP9" (cassette:player-unsupported p) :test #'equal))))
  (error (e) (ok (format nil "player refusal: ~a" e) nil)))

(format t "~&~a~%" (if (zerop *fails*) "VP9 HEADERS OK" (format nil "VP9: ~d FAILED" *fails*)))
(sb-ext:exit :code (if (zerop *fails*) 0 1))
