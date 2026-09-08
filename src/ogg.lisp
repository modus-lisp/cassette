;;;; ogg.lisp — Ogg, which is a framing layer and nothing else.
;;;;
;;;; Ogg carries no notion of what is inside it.  There is no codec identifier, no track table, no
;;;; duration and no index: there are PAGES, each belonging to a numbered stream, and each page
;;;; holds some number of SEGMENTS that concatenate into packets.  What a packet means is decided by
;;;; looking at the first one of each stream and recognising a magic string — which is why the
;;;; codec table below is a list of byte prefixes rather than a registry.
;;;;
;;;; THE SEGMENT TABLE IS THE WHOLE TRICK.  A page carries up to 255 segments and each segment is up
;;;; to 255 bytes; a segment of exactly 255 means "and there is more", anything less ends the
;;;; packet.  So a packet of exactly 255 bytes is sent as 255 and then a segment of ZERO, and a
;;;; reader that treats a short segment as "end unless empty" loses one packet in every few hundred.
;;;;
;;;; The granule position is per codec too: for Vorbis and Opus it counts samples, for Theora it is
;;;; a pair of frame counts packed into one number.  Ogg itself only promises it does not decrease.

(in-package #:cassette)

(defun ogg-p (bytes)
  (and (> (length bytes) 27)
       (= (aref bytes 0) (char-code #\O)) (= (aref bytes 1) (char-code #\g))
       (= (aref bytes 2) (char-code #\g)) (= (aref bytes 3) (char-code #\S))))

(defparameter +ogg-codecs+
  '((#(#x80 116 104 101 111 114 97) . "V_THEORA")     ; 0x80 "theora"
    (#(1 118 111 114 98 105 115) . "A_VORBIS")        ; 0x01 "vorbis"
    (#(79 112 117 115 72 101 97 100) . "A_OPUS")      ; "OpusHead"
    (#(127 70 76 65 67) . "A_FLAC")                   ; 0x7f "FLAC"
    (#(83 112 101 101 120 32 32 32) . "A_SPEEX"))     ; "Speex   "
  "How a stream is recognised: by the first bytes of its first packet.

   Ogg has no codec field, so this is not a lookup table that could be replaced by a better one —
   it IS the identification mechanism, and a codec not in it is a stream of anonymous packets.")

(defstruct (ogg-stream (:conc-name os-))
  (serial 0)
  (codec nil)
  (headers '())                                 ; the leading packets that configure the decoder
  (packets '())                                 ; (data . granule), in order
  (last-granule 0))

(defstruct (ogg (:conc-name ogg-) (:predicate nil))
  bytes
  (streams '())
  duration
  (tracks '()))

(defun ogg-video-track (o) (find 1 (ogg-tracks o) :key #'track-type))
(defun ogg-audio-track (o) (find 2 (ogg-tracks o) :key #'track-type))

(defun %ogg-pages (bytes fn)
  "Call FN with (serial header-type granule segments data-start) for every page, in file order."
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((p 0) (n (length bytes)))
    (declare (type fixnum p n))
    (loop
      (when (> (+ p 27) n) (return))
      (unless (and (= (aref bytes p) 79) (= (aref bytes (+ p 1)) 103)
                   (= (aref bytes (+ p 2)) 103) (= (aref bytes (+ p 3)) 83))
        ;; a damaged or padded file: the next capture pattern is the fix, and there is always one
        (let ((next (loop for k of-type fixnum from (1+ p) below (- n 4)
                          when (and (= (aref bytes k) 79) (= (aref bytes (+ k 1)) 103)
                                    (= (aref bytes (+ k 2)) 103) (= (aref bytes (+ k 3)) 83))
                            do (return k))))
          (if next (setf p next) (return))))
      (let* ((htype (aref bytes (+ p 5)))
             (granule (let ((v 0))
                        (dotimes (i 8 v) (setf v (logior v (ash (aref bytes (+ p 6 i)) (* 8 i)))))))
             (serial (let ((v 0))
                       (dotimes (i 4 v) (setf v (logior v (ash (aref bytes (+ p 14 i)) (* 8 i)))))))
             (nsegs (aref bytes (+ p 26)))
             (table (+ p 27))
             (data (+ table nsegs))
             (total 0))
        (declare (type fixnum nsegs table data total))
        (dotimes (i nsegs) (incf total (aref bytes (+ table i))))
        (when (> (+ data total) n) (return))
        (funcall fn serial htype granule nsegs table data)
        (setf p (+ data total))))))

(defun parse-ogg (bytes)
  "Read an Ogg file into streams and their packets."
  (let* ((bytes (coerce bytes '(simple-array (unsigned-byte 8) (*))))
         (o (make-ogg :bytes bytes))
         (table (make-hash-table))
         (partial (make-hash-table)))
    (unless (ogg-p bytes) (%err "not an Ogg file"))
    (%ogg-pages
     bytes
     (lambda (serial htype granule nsegs tbl data)
       (declare (ignore htype))
       (let ((st (or (gethash serial table)
                     (setf (gethash serial table) (make-ogg-stream :serial serial))))
             (p data))
         (declare (type fixnum p))
         (dotimes (i nsegs)
           (let ((len (aref bytes (+ tbl i))))
             (declare (type fixnum len))
             (push (subseq bytes p (+ p len)) (gethash serial partial))
             (incf p len)
             ;; a segment shorter than 255 ends the packet — INCLUDING a segment of zero, which is
             ;; how a packet whose length is a multiple of 255 says that it stopped
             (when (< len 255)
               (let* ((parts (nreverse (gethash serial partial)))
                      (total (reduce #'+ parts :key #'length))
                      (buf (make-array total :element-type '(unsigned-byte 8)))
                      (o2 0))
                 (dolist (c parts) (replace buf c :start1 o2) (incf o2 (length c)))
                 (setf (gethash serial partial) '())
                 (push (cons buf granule) (os-packets st))))))))
     )
    (maphash (lambda (k st)
               (declare (ignore k))
               (setf (os-packets st) (nreverse (os-packets st)))
               (let ((first (car (first (os-packets st)))))
                 (setf (os-codec st) (%ogg-codec first)))
               (push st (ogg-streams o)))
             table)
    (setf (ogg-streams o) (sort (ogg-streams o) #'< :key #'os-serial))
    (%ogg-make-tracks o)
    o))

(defun %ogg-codec (packet)
  (when packet
    (loop for (magic . name) in +ogg-codecs+
          when (and (>= (length packet) (length magic))
                    (every #'= magic (subseq packet 0 (length magic))))
            do (return name))))

(defun %ogg-make-tracks (o)
  "One TRACK per recognised stream, with whatever its identification packet says."
  (let ((n 0))
    (dolist (st (ogg-streams o))
      (let ((codec (os-codec st)))
        (when codec
          (let ((tr (make-track :number (incf n) :uid (os-serial st) :codec-id codec)))
            (cond
              ((equal codec "V_THEORA")
               (setf (track-type tr) 1)
               (let ((id (car (first (os-packets st)))))
                 (when (>= (length id) 22)
                   ;; the identification packet: the picture size is in macroblock units, big-endian
                   (setf (track-width tr) (* 16 (logior (ash (aref id 10) 8) (aref id 11)))
                         (track-height tr) (* 16 (logior (ash (aref id 12) 8) (aref id 13))))
                   ;; and the DISPLAYED size, which may be smaller, follows it
                   (let ((fw (logior (ash (aref id 14) 16) (ash (aref id 15) 8) (aref id 16)))
                         (fh (logior (ash (aref id 17) 16) (ash (aref id 18) 8) (aref id 19))))
                     (when (and (plusp fw) (plusp fh))
                       (setf (track-display-width tr) fw (track-display-height tr) fh))))))
              ((equal codec "A_VORBIS")
               (setf (track-type tr) 2)
               (let ((id (car (first (os-packets st)))))
                 (when (>= (length id) 30)
                   (setf (track-channels tr) (aref id 11)
                         (track-sample-rate tr)
                         (logior (aref id 12) (ash (aref id 13) 8)
                                 (ash (aref id 14) 16) (ash (aref id 15) 24))))))
              ((equal codec "A_OPUS")
               (setf (track-type tr) 2)
               (let ((id (car (first (os-packets st)))))
                 (when (>= (length id) 19)
                   (setf (track-channels tr) (aref id 9)
                         (track-sample-rate tr) 48000
                         (track-codec-private tr) id))))
              (t (setf (track-type tr) 0)))
            ;; THE HEADERS ARE PACKETS, not a separate blob.  Theora sends three and Vorbis three;
            ;; a decoder needs them all before the first frame, so they are separated here rather
            ;; than left for every consumer to count for itself.
            (let ((nheaders (cond ((equal codec "V_THEORA") 3)
                                  ((equal codec "A_VORBIS") 3)
                                  ((equal codec "A_OPUS") 2)
                                  (t 0))))
              (setf (os-headers st) (mapcar #'car (subseq (os-packets st)
                                                          0 (min nheaders (length (os-packets st)))))
                    (os-packets st) (nthcdr nheaders (os-packets st))))
            (push tr (ogg-tracks o))))))
    (setf (ogg-tracks o) (nreverse (ogg-tracks o)))))

(defun ogg-stream-for (o tr)
  (find (track-uid tr) (ogg-streams o) :key #'os-serial))

;;; ---- timestamps ---------------------------------------------------------------------------------
;;;
;;; Ogg's granule position is not a timestamp and is not the same quantity in two codecs: Vorbis and
;;; Opus count samples, Theora packs a key-frame number and an offset into one integer, and a page
;;; only carries the granule of the LAST packet that finished on it.  So a timestamp per packet is
;;; something a demuxer works out from what it knows about the codec, not something it reads.

(defun %theora-frame-duration (id)
  "Seconds per frame, from a Theora identification packet."
  (if (>= (length id) 30)
      (let ((num (logior (ash (aref id 22) 24) (ash (aref id 23) 16)
                         (ash (aref id 24) 8) (aref id 25)))
            (den (logior (ash (aref id 26) 24) (ash (aref id 27) 16)
                         (ash (aref id 28) 8) (aref id 29))))
        (if (and (plusp num) (plusp den)) (/ (float den 1d0) num) (/ 1d0 25)))
      (/ 1d0 25)))

(defun %opus-packet-samples (packet)
  "How many samples at 48 kHz one Opus packet holds, from its table-of-contents byte."
  (if (zerop (length packet))
      0
      (let* ((toc (aref packet 0))
             (config (ash toc -3))
             (frames (case (logand toc 3)
                       (0 1) (1 2) (2 2)
                       (t (if (> (length packet) 1) (logand (aref packet 1) 63) 1))))
             ;; the frame length depends on the mode: SILK at 10/20/40/60 ms, hybrid at 10/20,
             ;; CELT at 2.5/5/10/20
             (ms (cond ((< config 12) (aref #(10 20 40 60) (logand config 3)))
                       ((< config 16) (aref #(10 20) (logand config 1)))
                       (t (aref #(5/2 5 10 20) (logand config 3))))))
        (round (* 48 ms frames)))))

(defstruct (ogg-reader (:conc-name ogr-) (:constructor %make-ogg-reader))
  ogg (frames '()))

(defun make-ogg-reader (o)
  "A cursor over every packet of every recognised track, in file order with timestamps."
  (let ((frames '()))
    (dolist (tr (ogg-tracks o))
      (let* ((st (ogg-stream-for o tr))
             (codec (track-codec-id tr)))
        (cond
          ((equal codec "V_THEORA")
           (let ((dur (%theora-frame-duration (first (os-headers st)))) (i 0))
             (dolist (pk (os-packets st))
               (push (cons (* i dur)
                           (make-block-frame :track tr :timecode (round (* 1000000 i dur))
                                             :data (car pk)
                                             ;; a Theora key frame is the packet whose first bit is
                                             ;; zero, which is the only thing the header layer says
                                             :keyframe-p (and (plusp (length (car pk)))
                                                              (zerop (logand (aref (car pk) 0) #x80)))))
                     frames)
               (incf i))))
          ;; Vorbis packets carry no duration of their own: how many samples a packet yields
          ;; depends on ITS block size and the one before it, because the frame boundary is the
          ;; window centre.  The block size is readable without decoding — it is the mode number,
          ;; two bits into the packet — so timestamps can be laid down before anything decodes.
          ((equal codec "A_VORBIS")
           (handler-case
               (let* ((headers (mapcar (lambda (h)
                                         (coerce h '(simple-array (unsigned-byte 8) (*))))
                                       (os-headers st)))
                      (setup (reed:vorbis-setup-from-headers headers))
                      (rate (float (or (track-sample-rate tr) 44100) 1d0))
                      (prev 0) (pos 0))
                 (dolist (pk (os-packets st))
                   (let ((n (reed:vorbis-packet-block-size
                             setup (coerce (car pk) '(simple-array (unsigned-byte 8) (*))))))
                     (push (cons (/ pos rate)
                                 (make-block-frame :track tr
                                                   :timecode (round (* 1000000 pos) rate)
                                                   :data (car pk) :keyframe-p t))
                           frames)
                     (when (and (plusp prev) (plusp n)) (incf pos (ash (+ prev n) -2)))
                     (setf prev n))))
             (error () nil)))
          ((equal codec "A_OPUS")
           (let ((pos 0))
             (dolist (pk (os-packets st))
               (push (cons (/ pos 48000d0)
                           (make-block-frame :track tr :timecode (round (* 1000000 pos) 48000)
                                             :data (car pk) :keyframe-p t))
                     frames)
               (incf pos (%opus-packet-samples (car pk))))))
          (t nil))))
    (%make-ogg-reader :ogg o :frames (mapcar #'cdr (stable-sort (nreverse frames) #'< :key #'car)))))

(defun read-next-ogg-frame (r) (pop (ogr-frames r)))

(defconstant +ogg-tick+ 1000 "Nanoseconds per Ogg timestamp unit: these are counted in microseconds.")
