;;;; ebml.lisp — EBML (RFC 8794) primitives: variable-length integers,
;;;; element headers, and a small writer.  Matroska/WebM is EBML with a
;;;; specific element vocabulary (see demux.lisp / mux.lisp).
(in-package #:webm-pure)

;;; ---- reading ------------------------------------------------------------

(defun vint-length (first-byte)
  "Number of octets in a vint whose first octet is FIRST-BYTE (1..8), or NIL
   for the invalid all-zero marker."
  (declare (type (unsigned-byte 8) first-byte))
  (loop for n from 1 to 8
        when (logbitp (- 8 n) first-byte) do (return n)
        finally (return nil)))

(defun read-vint (buf pos &key (end (length buf)))
  "Read an EBML vint at POS with the length marker removed.
   Returns (values value new-pos unknown-p length): UNKNOWN-P is true for the
   all-ones reserved value (unknown size)."
  (declare (type octets buf) (type fixnum pos end))
  (when (>= pos end) (%err "EBML: truncated vint at ~d" pos))
  (let* ((b0 (aref buf pos))
         (len (or (vint-length b0) (%err "EBML: invalid vint marker at ~d" pos))))
    (when (> (+ pos len) end) (%err "EBML: truncated vint at ~d" pos))
    (let ((v (logand b0 (1- (ash 1 (- 8 len)))))
          (all-ones (= (logand b0 (1- (ash 1 (- 8 len)))) (1- (ash 1 (- 8 len))))))
      (loop for i from 1 below len
            for b = (aref buf (+ pos i))
            do (setf v (logior (ash v 8) b))
               (unless (= b 255) (setf all-ones nil)))
      (values v (+ pos len) all-ones len))))

(defun read-element-id (buf pos &key (end (length buf)))
  "Read an element ID (a vint whose marker bits are kept).  Returns (values id new-pos)."
  (declare (type octets buf) (type fixnum pos end))
  (when (>= pos end) (%err "EBML: truncated element id at ~d" pos))
  (let* ((b0 (aref buf pos))
         (len (or (vint-length b0) (%err "EBML: invalid element id at ~d" pos))))
    (when (> len 4) (%err "EBML: element id longer than 4 octets at ~d" pos))
    (when (> (+ pos len) end) (%err "EBML: truncated element id at ~d" pos))
    (let ((v b0))
      (loop for i from 1 below len do (setf v (logior (ash v 8) (aref buf (+ pos i)))))
      (values v (+ pos len)))))

(defun read-element-header (buf pos &key (end (length buf)))
  "Read an element header at POS.  Returns (values id data-start data-size),
   DATA-SIZE being NIL when the size is unknown (RFC 8794 s6.2)."
  (multiple-value-bind (id p) (read-element-id buf pos :end end)
    (multiple-value-bind (size p2 unknown) (read-vint buf p :end end)
      (values id p2 (if unknown nil size)))))

(defun ebml-uint (buf start size)
  (let ((v 0))
    (dotimes (i size) (setf v (logior (ash v 8) (aref buf (+ start i)))))
    v))

(defun ebml-sint (buf start size)
  (let ((v (ebml-uint buf start size)))
    (if (and (> size 0) (logbitp (1- (* 8 size)) v)) (- v (ash 1 (* 8 size))) v)))

(defun ebml-float (buf start size)
  (case size
    (0 0d0)
    (4 (let ((bits (ebml-uint buf start 4)))
         (coerce (decode-ieee-single bits) 'double-float)))
    (8 (decode-ieee-double (ebml-uint buf start 8)))
    (t (%err "EBML: float of size ~d" size))))

(defun decode-ieee-single (bits)
  (let ((sign (if (logbitp 31 bits) -1 1))
        (exp (ldb (byte 8 23) bits))
        (frac (ldb (byte 23 0) bits)))
    (cond ((= exp 255) (if (zerop frac) (* sign most-positive-single-float) 0f0))
          ((zerop exp) (* sign (scale-float (coerce frac 'single-float) -149)))
          (t (* sign (scale-float (+ 1f0 (/ frac 8388608f0)) (- exp 127)))))))

(defun decode-ieee-double (bits)
  (let ((sign (if (logbitp 63 bits) -1 1))
        (exp (ldb (byte 11 52) bits))
        (frac (ldb (byte 52 0) bits)))
    (cond ((= exp 2047) (if (zerop frac) (* sign most-positive-double-float) 0d0))
          ((zerop exp) (* sign (scale-float (coerce frac 'double-float) -1074)))
          (t (* sign (scale-float (+ 1d0 (/ frac 4503599627370496d0)) (- exp 1023)))))))

(defun ebml-string (buf start size)
  "UTF-8/ASCII element payload as a Lisp string, stopping at a NUL terminator."
  (let* ((end (+ start size))
         (nul (position 0 buf :start start :end end))
         (stop (or nul end)))
    (sb-ext:octets-to-string buf :start start :end stop :external-format :utf-8)))

(defun ebml-binary (buf start size)
  (subseq buf start (+ start size)))

;;; ---- writing -----------------------------------------------------------

(defun vint-size-for (value)
  "Smallest vint length able to hold VALUE with the marker."
  (loop for n from 1 to 8
        when (< value (1- (ash 1 (* 7 n)))) do (return n)
        finally (%err "EBML: value ~d too large for a vint" value)))

(defun write-vint (value out &key length)
  "Append VALUE as an EBML vint (size field) to the adjustable octet vector OUT.
   LENGTH forces a width (1..8); the all-ones pattern of that width means unknown."
  (let ((len (or length (vint-size-for value))))
    (vector-push-extend (logior (ash 1 (- 8 len)) (ldb (byte (- 8 len) (* 8 (1- len))) value)) out)
    (loop for i from (- len 2) downto 0 do
      (vector-push-extend (ldb (byte 8 (* 8 i)) value) out))
    out))

(defun write-unknown-size (out &optional (length 8))
  (vector-push-extend (logior (ash 1 (- 8 length)) (1- (ash 1 (- 8 length)))) out)
  (dotimes (i (1- length)) (vector-push-extend 255 out))
  out)

(defun write-id (id out)
  "Append an element ID (marker bits already included) to OUT."
  (let ((len (cond ((< id #x100) 1) ((< id #x10000) 2) ((< id #x1000000) 3) (t 4))))
    (loop for i from (1- len) downto 0 do (vector-push-extend (ldb (byte 8 (* 8 i)) id) out))
    out))

(defun make-out (&optional (capacity 256))
  (make-array capacity :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun out->octets (out)
  (let ((v (octets (length out))))
    (replace v out)
    v))

(defun write-element (id payload out &key size-length)
  "Append element ID with the octet PAYLOAD to OUT.  SIZE-LENGTH forces the width
   of the size vint (handy for fixed-layout tables such as Cues)."
  (write-id id out)
  (write-vint (length payload) out :length size-length)
  (loop for b across payload do (vector-push-extend b out))
  out)

(defun uint-octets (value &key min-length)
  "Big-endian unsigned VALUE in the fewest octets (at least MIN-LENGTH)."
  (let* ((n (max (or min-length 1)
                 (if (zerop value) 1 (ceiling (integer-length value) 8))))
         (v (octets n)))
    (loop for i from 0 below n do (setf (aref v (- n 1 i)) (ldb (byte 8 (* 8 i)) value)))
    v))

(defun sint-octets (value)
  (let* ((n (max 1 (ceiling (1+ (integer-length value)) 8)))
         (u (ldb (byte (* 8 n) 0) value))
         (v (octets n)))
    (loop for i from 0 below n do (setf (aref v (- n 1 i)) (ldb (byte 8 (* 8 i)) u)))
    v))

(defun float-octets (value)
  "IEEE double VALUE as 8 octets."
  (let* ((d (coerce value 'double-float))
         (bits (if (zerop d)
                   0
                   (multiple-value-bind (sig exp sign) (integer-decode-float d)
                     ;; sig has 53 bits; normalise to 52-bit fraction
                     (let* ((shift (- 53 (integer-length sig)))
                            (sig (ash sig shift)) (exp (- exp shift))
                            (e (+ exp 52 1023)))
                       (logior (if (minusp sign) (ash 1 63) 0)
                               (ash (ldb (byte 11 0) e) 52)
                               (ldb (byte 52 0) sig)))))))
    (uint-octets bits :min-length 8)))

(defun string-octets (s) (sb-ext:string-to-octets s :external-format :utf-8))

(defun ebml-element (id payload &key size-length)
  "An element as a fresh octet vector."
  (out->octets (write-element id payload (make-out (+ 12 (length payload))) :size-length size-length)))

(defun ebml-master (id &rest children)
  "A master element containing CHILDREN (octet vectors, NILs skipped)."
  (let ((out (make-out)))
    (dolist (c children) (when c (loop for b across c do (vector-push-extend b out))))
    (ebml-element id (out->octets out))))

(defun concat-octets (&rest parts)
  (let ((out (make-out)))
    (dolist (p parts) (when p (loop for b across p do (vector-push-extend b out))))
    (out->octets out)))
