;;;; mpegsys.lisp — MPEG-2 Systems: program streams and transport streams (ISO/IEC 13818-1).
;;;;
;;;; TWO CONTAINERS, ONE PAYLOAD.  A `.mpg' or `.vob' is a PROGRAM stream: packs of PES packets,
;;;; meant for a medium that does not lose bytes.  A `.ts' is a TRANSPORT stream: fixed 188-byte
;;;; cells with a thirteen-bit stream number in each, meant for a medium that does.  Inside both,
;;;; the unit is the same PES packet with the same header and the same 33-bit clock, which is why
;;;; this is one file and not two.
;;;;
;;;; WHERE A FRAME BEGINS IS NOT WHERE A PACKET BEGINS.  Neither container marks access units: a PES
;;;; packet may hold half a picture, or two, and a picture may span a dozen transport cells.  So the
;;;; payload of a track is reassembled into one byte stream and then CUT AT THE CODEC'S OWN
;;;; boundaries — a picture start code for MPEG video, an access unit delimiter or the first slice
;;;; of a new picture for H.264.  Handing out PES packets as though they were frames appears to work
;;;; and then desynchronises on the first picture that happens to straddle two.

(in-package #:cassette)

;;; ---- stream ids and stream types --------------------------------------------------------------

(defconstant +ps-pack+ #xba)
(defconstant +ps-system-header+ #xbb)
(defconstant +ps-program-end+ #xb9)
(defconstant +ps-padding+ #xbe)
(defconstant +ps-private-2+ #xbf)
(defconstant +ps-map+ #xbc)

(defun %video-stream-id-p (id) (<= #xe0 id #xef))
(defun %audio-stream-id-p (id) (<= #xc0 id #xdf))

(defun %stream-type-codec (type)
  "The codec a transport stream's stream_type names (Table 2-34), in Matroska's vocabulary."
  (case type
    (#x01 "V_MPEG1")
    (#x02 "V_MPEG2")
    (#x03 "A_MPEG/L3")                          ; MPEG-1 audio: layer decided by the frame header
    (#x04 "A_MPEG/L3")
    (#x0f "A_AAC")                              ; ADTS
    (#x11 "A_AAC")                              ; LATM
    (#x1b "V_MPEG4/ISO/AVC")
    (#x24 "V_MPEGH/ISO/HEVC")
    (#x81 "A_AC3")
    (#x86 "A_DTS")
    (#x87 "A_EAC3")
    (t (format nil "stream_type 0x~2,'0x" type))))

;;; ---- the container -----------------------------------------------------------------------------

(defstruct (mpegsys (:conc-name ms-) (:predicate nil))
  bytes
  (kind :ps)                                    ; :ps or :ts
  (packet-size 188)                             ; transport streams only
  (tracks '())
  duration                                      ; seconds, from the first and last presentation time
  (first-pts nil))

(defun mpegsys-video-track (m) (find 1 (ms-tracks m) :key #'track-type))
(defun mpegsys-audio-track (m) (find 2 (ms-tracks m) :key #'track-type))

;;; ---- transport stream packets -------------------------------------------------------------

(defun %ts-packet-size (bytes)
  "188, 192 or 204, whichever spacing the sync bytes actually appear at, or NIL.

   The three sizes are the same packet with nothing, a four-byte arrival timestamp, or sixteen bytes
   of forward error correction around it.  Guessing 188 and being wrong reads a stream of garbage
   PIDs rather than failing, so this checks several packets before believing any of them."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((n (length bytes)))
    (dolist (size '(188 192 204))
      (loop for start of-type fixnum from 0 below (min n (* 2 size))
            do (when (and (= #x47 (aref bytes start))
                          (loop for k of-type fixnum from 1 below 8
                                for p = (+ start (* k size))
                                always (or (>= p n) (= #x47 (aref bytes p)))))
                 (return-from %ts-packet-size (values size start)))))
    nil))

(defun mpegts-p (bytes)
  (and (> (length bytes) 376) (%ts-packet-size bytes) t))

(defun mpegps-p (bytes)
  (and (> (length bytes) 16)
       (= 0 (aref bytes 0)) (= 0 (aref bytes 1)) (= 1 (aref bytes 2))
       (>= (aref bytes 3) #xb9)))

;;; ---- PES headers ---------------------------------------------------------------------------

(defun %read-timestamp (bytes p)
  "A 33-bit presentation or decoding time, spread over five bytes around three marker bits."
  (logior (ash (logand (aref bytes p) #x0e) 29)
          (ash (aref bytes (+ p 1)) 22)
          (ash (logand (aref bytes (+ p 2)) #xfe) 14)
          (ash (aref bytes (+ p 3)) 7)
          (ash (logand (aref bytes (+ p 4)) #xfe) -1)))

(defun %pes-payload (bytes start end)
  "Skip a PES packet's header.  Returns (values payload-start pts dts), or NIL for a packet that
   carries no elementary stream data at all.

   TWO HEADER FORMATS, and a program stream may use either.  MPEG-1's is a run of stuffing bytes
   followed by optional fields identified by their own top bits; MPEG-2's is a fixed pair of flag
   bytes and an explicit header length.  They are told apart by the top two bits of the byte after
   the packet length: '10' is MPEG-2 and anything else is MPEG-1."
  (let ((p start) (pts nil) (dts nil))
    (when (>= (+ p 1) end) (return-from %pes-payload nil))
    (if (= #x80 (logand (aref bytes p) #xc0))
        ;; MPEG-2: flags, flags, header length, then the fields the flags claim
        (let* ((flags (aref bytes (1+ p)))
               (hlen (aref bytes (+ p 2)))
               (q (+ p 3)))
          (when (> (+ q hlen) end) (return-from %pes-payload nil))
          (case (ash flags -6)
            (2 (setf pts (%read-timestamp bytes q)))
            (3 (setf pts (%read-timestamp bytes q)
                     dts (%read-timestamp bytes (+ q 5))))
            (t nil))
          (values (+ q hlen) pts dts))
        ;; MPEG-1: up to sixteen 0xFF stuffing bytes, an optional buffer size, then the times
        (progn
          (loop while (and (< p end) (= #xff (aref bytes p))) do (incf p))
          (when (and (< p end) (= #x40 (logand (aref bytes p) #xc0))) (incf p 2))
          (cond
            ((and (< p end) (= #x20 (logand (aref bytes p) #xf0)))
             (setf pts (%read-timestamp bytes p)) (incf p 5))
            ((and (< p end) (= #x30 (logand (aref bytes p) #xf0)))
             (setf pts (%read-timestamp bytes p)
                   dts (%read-timestamp bytes (+ p 5)))
             (incf p 10))
            ((and (< p end) (= #x0f (aref bytes p))) (incf p))
            (t nil))
          (values p pts dts)))))

;;; ---- walking a program stream ----------------------------------------------------------------

(defun %ps-walk (bytes fn &key (limit nil))
  "Call FN with (stream-id payload-start payload-end pts) for every PES packet, in file order."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((p 0) (n (or limit (length bytes))))
    (loop
      (when (> (+ p 4) n) (return))
      (unless (and (= 0 (aref bytes p)) (= 0 (aref bytes (+ p 1))) (= 1 (aref bytes (+ p 2))))
        ;; resynchronise: a damaged or padded stream is common and the next start code is the fix
        (let ((next (loop for k of-type fixnum from (1+ p) below (- n 3)
                          when (and (zerop (aref bytes k)) (zerop (aref bytes (+ k 1)))
                                    (= 1 (aref bytes (+ k 2))))
                            do (return k))))
          (if next (setf p next) (return))))
      (let ((id (aref bytes (+ p 3))))
        (cond
          ((= id +ps-pack+)
           ;; the pack header is 14 bytes in MPEG-2 and 12 in MPEG-1, and says which by its own
           ;; top bits — plus up to seven bytes of stuffing whose count is in the last byte
           (if (= #x40 (logand (aref bytes (+ p 4)) #xc0))
               (incf p (+ 14 (logand (aref bytes (+ p 13)) 7)))
               (incf p 12)))
          ((= id +ps-program-end+) (return))
          ((>= id #xbb)
           (let* ((len (logior (ash (aref bytes (+ p 4)) 8) (aref bytes (+ p 5))))
                  (body (+ p 6))
                  (end (min n (+ body len))))
             (when (zerop len) (setf end n))     ; a length of zero means "to the next start code"
             (if (or (= id +ps-padding+) (= id +ps-private-2+) (= id +ps-system-header+)
                     (= id +ps-map+))
                 nil
                 (multiple-value-bind (ps pts) (%pes-payload bytes body end)
                   (when ps (funcall fn id ps end pts))))
             (setf p end)))
          (t (incf p 4)))))))

;;; ---- walking a transport stream ---------------------------------------------------------------

(defun %ts-walk (bytes size offset fn)
  "Call FN with (pid payload-start payload-end unit-start-p) for every transport packet payload."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes) (type fixnum size offset))
  (let ((n (length bytes)))
    (loop for base of-type fixnum from offset below n by size
          do (when (> (+ base 188) n) (return))
             ;; a 192-byte packet is a 188-byte one behind a four-byte arrival time
             (let ((p (if (and (= size 192) (/= #x47 (aref bytes base))) (+ base 4) base)))
               (when (= #x47 (aref bytes p))
                 (let* ((pusi (logbitp 6 (aref bytes (+ p 1))))
                        (pid (logior (ash (logand (aref bytes (+ p 1)) #x1f) 8) (aref bytes (+ p 2))))
                        (afc (logand (ash (aref bytes (+ p 3)) -4) 3))
                        (q (+ p 4)))
                   (when (logbitp 1 afc) (incf q (1+ (aref bytes q))))
                   (when (and (logbitp 0 afc) (<= q (+ p 188)))
                     (funcall fn pid q (+ p 188) pusi))))))))

(defun %parse-pat-pmt (bytes size offset)
  "The program map: (values pmt-pid (list (pid . stream-type))).

   A transport stream carries no list of what is in it.  Programme zero's table names a table for
   each programme, and THAT table names the streams — so finding the video means two levels of
   indirection through sections that may themselves be split across packets.  Only the common case
   is handled here: a table that fits in one packet, which is every table in practice because they
   are a few dozen bytes and the standard requires them to repeat often."
  (let ((pmt-pid nil) (streams '()))
    (%ts-walk bytes size offset
              (lambda (pid start end pusi)
                (cond
                  ((and (zerop pid) pusi (null pmt-pid))
                   (let* ((p (+ start 1 (aref bytes start))))   ; pointer_field
                     (when (and (< p end) (zerop (aref bytes p)))
                       (let* ((slen (logior (ash (logand (aref bytes (+ p 1)) #x0f) 8)
                                            (aref bytes (+ p 2))))
                              (q (+ p 8))        ; past the section header
                              (last (min end (+ p 3 slen -4))))
                         (loop while (<= (+ q 4) last)
                               do (let ((prog (logior (ash (aref bytes q) 8) (aref bytes (+ q 1))))
                                        (mp (logior (ash (logand (aref bytes (+ q 2)) #x1f) 8)
                                                    (aref bytes (+ q 3)))))
                                    (when (plusp prog) (setf pmt-pid mp) (return)))
                                  (incf q 4))))))
                  ((and pmt-pid (= pid pmt-pid) pusi (null streams))
                   (let ((p (+ start 1 (aref bytes start))))
                     (when (and (< p end) (= 2 (aref bytes p)))
                       (let* ((slen (logior (ash (logand (aref bytes (+ p 1)) #x0f) 8)
                                            (aref bytes (+ p 2))))
                              (pil (logior (ash (logand (aref bytes (+ p 10)) #x0f) 8)
                                           (aref bytes (+ p 11))))
                              (q (+ p 12 pil))
                              (last (min end (+ p 3 slen -4))))
                         (loop while (<= (+ q 5) last)
                               do (let* ((stype (aref bytes q))
                                         (epid (logior (ash (logand (aref bytes (+ q 1)) #x1f) 8)
                                                       (aref bytes (+ q 2))))
                                         (esl (logior (ash (logand (aref bytes (+ q 3)) #x0f) 8)
                                                      (aref bytes (+ q 4)))))
                                    (push (cons epid stype) streams)
                                    (incf q (+ 5 esl))))))))
                  (t nil))))
    (values pmt-pid (nreverse streams))))

;;; ---- elementary streams ------------------------------------------------------------------------

(defstruct (es (:conc-name es-))
  "One track's payload, lifted out of the container: the bytes in order, and where each
   presentation time attaches to them."
  (id 0)
  (stream-type nil)
  (bytes (make-array 0 :element-type '(unsigned-byte 8)) :type (simple-array (unsigned-byte 8) (*)))
  (fill 0 :type fixnum)
  ;; (elementary-offset pts file-offset), newest first while collecting.  The FILE offset is kept
  ;; because it, and not the presentation time, is the order a decoder must be fed in: a stream is
  ;; transmitted in DECODE order and a B picture's time is earlier than the picture before it.
  (marks '()))

(defun %es-append (e bytes start end pts)
  (let* ((n (- end start))
         (need (+ (es-fill e) n)))
    (when (> need (length (es-bytes e)))
      (let ((new (make-array (max 65536 (* 2 need)) :element-type '(unsigned-byte 8))))
        (replace new (es-bytes e) :end2 (es-fill e))
        (setf (es-bytes e) new)))
    (push (list (es-fill e) pts start) (es-marks e))
    (replace (es-bytes e) bytes :start1 (es-fill e) :start2 start :end2 end)
    (setf (es-fill e) need)))

(defun %collect-streams (m)
  "Every elementary stream in the container, keyed by stream id (program) or PID (transport)."
  (let ((bytes (ms-bytes m)) (table (make-hash-table)))
    (flet ((piece (key type start end pts)
             (let ((e (or (gethash key table)
                          (setf (gethash key table)
                                (make-es :id key :stream-type type)))))
               (%es-append e bytes start end pts))))
      (if (eq (ms-kind m) :ps)
          (%ps-walk bytes (lambda (id start end pts)
                            (when (or (%video-stream-id-p id) (%audio-stream-id-p id)
                                      (= id #xbd))
                              (piece id nil start end pts))))
          ;; a transport stream reassembles PES packets across cells before any of this
          (multiple-value-bind (size offset) (%ts-packet-size bytes)
            (multiple-value-bind (pmt streams) (%parse-pat-pmt bytes size offset)
              (declare (ignore pmt))
              (let ((partial (make-hash-table)))
                (%ts-walk bytes size offset
                          (lambda (pid start end pusi)
                            (let ((stype (cdr (assoc pid streams))))
                              (when stype
                                (if pusi
                                    ;; a new PES packet begins here; its header comes off first
                                    (when (and (< (+ start 6) end)
                                               (zerop (aref bytes start))
                                               (zerop (aref bytes (+ start 1)))
                                               (= 1 (aref bytes (+ start 2))))
                                      (multiple-value-bind (ps pts)
                                          (%pes-payload bytes (+ start 6) end)
                                        (when ps
                                          (setf (gethash pid partial) t)
                                          (piece pid stype ps end pts))))
                                    (when (gethash pid partial)
                                      (piece pid stype start end nil)))))))))))
      table)))

;;; ---- access units --------------------------------------------------------------------------
;;;
;;; Cutting the reassembled stream where the CODEC says a picture starts, not where the container
;;; happened to put a packet boundary.

(defun %mpeg-video-cuts (buf n)
  "The offsets at which MPEG-1/2 access units begin.

   A picture start code begins one, and any sequence header or group header immediately in front of
   it belongs to it rather than to the picture before — so the cut goes before the earliest of the
   run, which is what makes the first frame of a file carry its own sequence header."
  (let ((cuts '()) (pending nil) (seen-picture nil))
    (loop for i of-type fixnum from 0 below (- n 3)
          do (when (and (zerop (aref buf i)) (zerop (aref buf (+ i 1))) (= 1 (aref buf (+ i 2))))
               (let ((code (aref buf (+ i 3))))
                 (cond
                   ;; a sequence or group header: remember where the run started
                   ((or (= code #xb3) (= code #xb8))
                    (when (and seen-picture (null pending)) (setf pending i)))
                   ((zerop code)
                    (if seen-picture
                        (progn (push (or pending i) cuts) (setf pending nil))
                        (setf seen-picture t)))
                   (t (setf pending nil))))))
    (nreverse cuts)))

(defun %h264-cuts (buf n)
  "The offsets at which H.264 access units begin, for a stream carried as Annex B.

   TWO RULES, AND ONLY ONE OF THEM AT A TIME.  An access unit delimiter marks a boundary exactly,
   and broadcast encoders send them; where they are absent the boundary is instead the first slice
   of a new picture, which is a slice NAL whose first_mb_in_slice is zero — an Exp-Golomb code, so
   the test is `the next bit is 1'.  Applying both rules to a stream that has delimiters cuts every
   picture twice, which produces exactly twice as many frames as there are pictures and half of them
   empty of slices."
  (let ((has-aud (loop for i of-type fixnum from 0 below (- n 4)
                       thereis (and (zerop (aref buf i)) (zerop (aref buf (+ i 1)))
                                    (= 1 (aref buf (+ i 2)))
                                    (= 9 (logand (aref buf (+ i 3)) #x1f)))))
        (cuts '()) (seen nil))
    (loop for i of-type fixnum from 0 below (- n 4)
          do (when (and (zerop (aref buf i)) (zerop (aref buf (+ i 1))) (= 1 (aref buf (+ i 2))))
               (let ((type (logand (aref buf (+ i 3)) #x1f)))
                 (cond
                   ((and has-aud (= type 9)) (if seen (push i cuts) (setf seen t)))
                   ((and (not has-aud) (or (= type 1) (= type 5)) (< (+ i 4) n))
                    (when (logbitp 7 (aref buf (+ i 4)))
                      (if seen (push i cuts) (setf seen t))))
                   (t nil)))))
    (nreverse cuts)))

(defun %mpeg-audio-cuts (buf n)
  "Where each MPEG audio frame begins.

   There is no start code: a frame begins with eleven set bits and its own length is computed from
   the four fields after them.  So the walk is `parse a header, jump its length, expect another' —
   and a candidate is only believed once the header AFTER it also parses, because eleven set bits
   occur often enough inside coded audio to find by accident."
  (let ((cuts '()) (i 0))
    (declare (type fixnum i))
    (loop
      (when (>= (+ i 4) n) (return))
      (if (and (= #xff (aref buf i)) (= #xe0 (logand (aref buf (1+ i)) #xe0)))
          (let ((len (%mpeg-audio-frame-length buf i)))
            (if (and len (plusp len) (<= (+ i len 4) n)
                     (= #xff (aref buf (+ i len)))
                     (= #xe0 (logand (aref buf (+ i len 1)) #xe0)))
                (progn (when (plusp i) (push i cuts)) (incf i len))
                (incf i)))
          (incf i)))
    (nreverse cuts)))

(defparameter +mpeg-audio-bitrates+
  #(#(0 32 64 96 128 160 192 224 256 288 320 352 384 416 448 0)     ; MPEG-1 Layer I
    #(0 32 48 56 64 80 96 112 128 160 192 224 256 320 384 0)        ; MPEG-1 Layer II
    #(0 32 40 48 56 64 80 96 112 128 160 192 224 256 320 0)         ; MPEG-1 Layer III
    #(0 32 48 56 64 80 96 112 128 144 160 176 192 224 256 0)        ; MPEG-2 Layer I
    #(0 8 16 24 32 40 48 56 64 80 96 112 128 144 160 0))            ; MPEG-2 Layers II and III
  "Bit rates in kbit/s, by layer and version.  Three tables for MPEG-1 and two for the half-rate
   versions, because the layers do not agree about what an index means.")

(defparameter +mpeg-audio-rates+ #(44100 48000 32000 0))

(defun %mpeg-audio-frame-length (buf i)
  "The length in bytes of the MPEG audio frame whose header starts at I, or NIL if it is not one."
  (let* ((ver (ldb (byte 2 3) (aref buf (1+ i))))
         (layer (ldb (byte 2 1) (aref buf (1+ i))))
         (br-idx (ldb (byte 4 4) (aref buf (+ i 2))))
         (sr-idx (ldb (byte 2 2) (aref buf (+ i 2))))
         (pad (ldb (byte 1 1) (aref buf (+ i 2)))))
    (when (or (= ver 1) (zerop layer) (zerop br-idx) (= br-idx 15) (= sr-idx 3))
      (return-from %mpeg-audio-frame-length nil))
    (let* ((mpeg1 (= ver 3))
           (lnum (case layer (3 1) (2 2) (t 3)))
           (tbl (if mpeg1 (1- lnum) (if (= lnum 1) 3 4)))
           (rate (* 1000 (aref (aref +mpeg-audio-bitrates+ tbl) br-idx)))
           (sr (let ((r (aref +mpeg-audio-rates+ sr-idx)))
                 (cond (mpeg1 r) ((= ver 2) (floor r 2)) (t (floor r 4))))))
      (when (or (zerop rate) (zerop sr)) (return-from %mpeg-audio-frame-length nil))
      (if (= lnum 1)
          (* 4 (+ (floor (* 12 rate) sr) pad))
          (+ (floor (* (if (or mpeg1 (= lnum 2)) 144 72) rate) sr) pad)))))

(defun %mark-at (marks offset)
  "The mark for the packet that supplied byte OFFSET: (elementary-offset pts file-offset)."
  (let ((best nil))
    (dolist (m marks best)
      (when (<= (first m) offset)
        (when (or (null best) (> (first m) (first best))) (setf best m))))))

(defun %split-access-units (e codec track)
  "Cut one elementary stream into frames.  Returns a list of (file-offset . frame), so that the
   caller can put several tracks back into the order they were transmitted in."
  (let* ((buf (es-bytes e)) (n (es-fill e))
         (marks (sort (copy-list (es-marks e)) #'< :key #'first))
         (cuts (cond ((member codec '("V_MPEG1" "V_MPEG2") :test #'equal) (%mpeg-video-cuts buf n))
                     ((equal codec "V_MPEG4/ISO/AVC") (%h264-cuts buf n))
                     ((member codec '("A_MPEG/L2" "A_MPEG/L3") :test #'equal)
                      (%mpeg-audio-cuts buf n))
                     (t '())))
         (bounds (append '(0) cuts (list n)))
         (out '()))
    (when (null cuts)
      ;; nothing known about this codec's framing: hand the whole stream over as one piece rather
      ;; than pretend to have cut it
      (let ((m (%mark-at marks 0)))
        (return-from %split-access-units
          (list (cons (or (third m) 0)
                      (make-block-frame :track track :timecode (or (second m) 0)
                                        :data (subseq buf 0 n) :keyframe-p t))))))
    (loop for (a b) on bounds
          while b
          do (when (> b a)
               (let ((m (%mark-at marks a)))
                 (push (cons (or (third m) 0)
                             (make-block-frame :track track
                                               :timecode (or (second m) 0)
                                               :data (subseq buf a b)
                                               :keyframe-p (%unit-key-p buf a b codec)))
                       out))))
    (nreverse out)))

(defun %unit-key-p (buf a b codec)
  "Does this access unit begin a place a decoder could start from?"
  (cond
    ((member codec '("V_MPEG1" "V_MPEG2") :test #'equal)
     ;; a sequence header, or a picture whose coding type is I
     (loop for i of-type fixnum from a below (- b 5)
           do (when (and (zerop (aref buf i)) (zerop (aref buf (+ i 1))) (= 1 (aref buf (+ i 2))))
                (let ((code (aref buf (+ i 3))))
                  (when (= code #xb3) (return t))
                  (when (zerop code)
                    ;; picture_coding_type is bits 10..12 of the picture header
                    (return (= 1 (logand (ash (logior (ash (aref buf (+ i 4)) 8)
                                                      (aref buf (+ i 5)))
                                              -3)
                                         7))))))))
    ((equal codec "V_MPEG4/ISO/AVC")
     (loop for i of-type fixnum from a below (- b 4)
           do (when (and (zerop (aref buf i)) (zerop (aref buf (+ i 1))) (= 1 (aref buf (+ i 2))))
                (let ((type (logand (aref buf (+ i 3)) #x1f)))
                  (when (or (= type 5) (= type 7)) (return t))))))
    (t t)))

;;; ---- what is in the file ----------------------------------------------------------------------

(defun %probe-video-codec (buf n)
  "Which video codec an elementary stream holds, decided by what its start codes mean.

   A program stream has no table of contents, so this is the only way to know.  MPEG video announces
   itself with a sequence header; H.264 carried the same way announces itself with a sequence
   parameter set, and the two cannot be confused because 0xB3 is not a NAL type H.264 uses at the
   start of a stream."
  (loop for i of-type fixnum from 0 below (min n 262144)
        do (when (and (zerop (aref buf i)) (zerop (aref buf (+ i 1))) (= 1 (aref buf (+ i 2))))
             (let ((c (aref buf (+ i 3))))
               (cond ((= c #xb3) (return "V_MPEG2"))       ; refined below by the extension
                     ((= 7 (logand c #x1f))
                      (when (zerop (logand c #x80)) (return "V_MPEG4/ISO/AVC")))
                     (t nil))))))

(defun %mpeg-video-info (buf n)
  "(values codec width height), by parsing the sequence header the stream begins with."
  (let ((i (reel.mpeg2:find-start-code buf 0)))
    (loop while i
          do (when (= #xb3 (aref buf (+ i 3)))
               (let* ((br (reel.mpeg2:make-br buf :start (+ i 4) :end n))
                      (s (reel.mpeg2:parse-sequence-header br))
                      ;; MPEG-2 only if a sequence extension follows the header
                      (mpeg2 (let ((j (reel.mpeg2:find-start-code buf (+ i 4))))
                               (and j (= #xb5 (aref buf (+ j 3)))
                                    (= 1 (ash (aref buf (+ j 4)) -4))))))
                 (return-from %mpeg-video-info
                   (values (if mpeg2 "V_MPEG2" "V_MPEG1")
                           (reel.mpeg2:seq-width s) (reel.mpeg2:seq-height s)))))
             (setf i (reel.mpeg2:find-start-code buf (+ i 4))))
    (values "V_MPEG2" 0 0)))

(defun %h264-size (buf n)
  "(values width height) from the first sequence parameter set in an Annex B stream, or 0 and 0."
  (handler-case
      (let ((i 0))
        (loop while (< i (- n 4))
              do (if (and (zerop (aref buf i)) (zerop (aref buf (+ i 1))) (= 1 (aref buf (+ i 2))))
                     (progn
                       (when (= 7 (logand (aref buf (+ i 3)) #x1f))
                         (let* ((end (or (loop for k of-type fixnum from (+ i 4) below (- n 3)
                                               when (and (zerop (aref buf k)) (zerop (aref buf (+ k 1)))
                                                         (= 1 (aref buf (+ k 2))))
                                                 do (return k))
                                         n))
                                (nal (reel.h264:parse-nal buf (+ i 3) end))
                                (sps (reel.h264:parse-sps (reel.h264:nal-rbsp nal))))
                           (return-from %h264-size
                             (values (reel.h264:sps-width sps) (reel.h264:sps-height sps)))))
                       (incf i 4))
                     (incf i)))
        (values 0 0))
    (error () (values 0 0))))

(defun %probe-mpeg-audio (buf n)
  "Which MPEG audio layer a stream holds, from the first frame header that checks out.

   A program stream names its audio tracks by stream id alone, and the id says `MPEG audio' without
   saying which layer — which matters, because Layer II and Layer III are different decoders that
   happen to share a frame header."
  (loop for i of-type fixnum from 0 below (min n 65536)
        do (when (and (= #xff (aref buf i)) (< (+ i 4) n)
                      (= #xe0 (logand (aref buf (1+ i)) #xe0))
                      (%mpeg-audio-frame-length buf i))
             (return (case (ldb (byte 2 1) (aref buf (1+ i)))
                       (3 "A_MPEG/L1") (2 "A_MPEG/L2") (t "A_MPEG/L3")))))
      )

(defun parse-mpegsys (bytes)
  "Read a program stream or a transport stream into tracks and frames.

   The whole payload is lifted out of the container up front.  That costs another copy of the file
   in memory, which is the same bargain the rest of this library already makes by reading the file
   whole; what it buys is that a frame is a frame, contiguous, with a time on it, whether it arrived
   in one packet or forty."
  (let* ((bytes (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         (kind (cond ((mpegts-p bytes) :ts) ((mpegps-p bytes) :ps)
                     (t (%err "not a program or transport stream"))))
         (m (make-mpegsys :bytes bytes :kind kind))
         (table (%collect-streams m))
         (tracks '()) (frames '()) (number 0))
    (when (eq kind :ts)
      (setf (ms-packet-size m) (or (%ts-packet-size bytes) 188)))
    (maphash
     (lambda (key e)
       (let* ((buf (es-bytes e)) (n (es-fill e))
              (stype (es-stream-type e))
              (declared (and stype (%stream-type-codec stype)))
              (video-p (or (and stype (member stype '(#x01 #x02 #x1b #x24)))
                           (and (null stype) (%video-stream-id-p key))))
              (codec (cond (declared declared)
                           (video-p (or (%probe-video-codec buf n) "V_UNKNOWN"))
                           ((and (null stype) (%audio-stream-id-p key)) (%probe-mpeg-audio buf n))
                           ((and (null stype) (= key #xbd)) "A_AC3")
                           (t "V_UNKNOWN")))
              (w 0) (h 0))
         (when (and video-p (member codec '("V_MPEG1" "V_MPEG2") :test #'equal))
           (multiple-value-setq (codec w h) (%mpeg-video-info buf n)))
         ;; H.264 in a transport stream carries its parameter sets IN BAND rather than in the
         ;; container, so the picture size is only knowable by parsing one
         (when (and video-p (equal codec "V_MPEG4/ISO/AVC"))
           (multiple-value-setq (w h) (%h264-size buf n)))
         ;; A TRANSPORT STREAM'S TABLE SAYS `MPEG AUDIO' AND NOT WHICH LAYER.  Stream types 3 and 4
         ;; cover Layers I, II and III alike, which are three different decoders that happen to
         ;; share a frame header — so the layer comes from the first frame, as it does in a program
         ;; stream, which has no table at all.
         (when (and (not video-p) (member stype '(#x03 #x04)))
           (setf codec (or (%probe-mpeg-audio buf n) codec)))
         (let ((tr (make-track :number (incf number)
                               :uid key
                               :type (if video-p 1 2)
                               :codec-id codec
                               :width w :height h)))
           (push tr tracks)
           (setf frames (nconc frames (%split-access-units e codec tr))))))
     table)
    (setf (ms-tracks m) (sort (nreverse tracks) #'< :key #'track-number))
    ;; FILE ORDER, NOT TIME ORDER.  The tracks were lifted out separately and have to be put back
    ;; together, and the order to put them back in is the one they arrived in — a stream is
    ;; transmitted in decode order, and sorting by presentation time hands a decoder its B pictures
    ;; before the references they predict from.
    (setf frames (mapcar #'cdr (stable-sort frames #'< :key #'car)))
    (let ((v (mpegsys-video-track m)))
      (declare (ignorable v))
      (let ((times (remove 0 (mapcar #'frame-timecode frames))))
        (when times
          (setf (ms-first-pts m) (reduce #'min times)
                (ms-duration m) (/ (- (reduce #'max times) (reduce #'min times)) 90000d0)))))
    (values m frames)))

;;; ---- the reader ---------------------------------------------------------------------------

(defstruct (mpegsys-reader (:conc-name msr-) (:constructor %make-mpegsys-reader))
  sys (frames '()))

(defun make-mpegsys-reader (m frames)
  (%make-mpegsys-reader :sys m :frames frames))

(defun read-next-mpegsys-frame (r)
  (pop (msr-frames r)))

(defun mpegsys-tick (m)
  "Nanoseconds per unit of the timestamps on these frames.  MPEG-2 Systems counts at 90 kHz."
  (declare (ignore m))
  (/ 1000000000 90000))
