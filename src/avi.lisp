;;;; avi.lisp — RIFF/AVI, the container of the DivX era.
;;;;
;;;; AVI is older and simpler than everything else here and it shows in both directions.  There is no
;;;; timestamp anywhere: a video frame's time is its INDEX times the stream's frame duration, and an
;;;; audio packet's time is however many samples came before it.  That works because AVI predates
;;;; the idea that a container might carry pictures out of order — and it is exactly why an AVI
;;;; holding B frames (which the later MPEG-4 codecs use) has to lie about them, packing two coded
;;;; frames into one chunk rather than admitting the order.
;;;;
;;;; WHAT A CHUNK CONTAINS IS NAMED BY A FOURCC AND NOTHING ELSE.  There is no codec registry, no
;;;; profile, no parameter set: the stream header carries four characters that a Windows install in
;;;; 1998 would have looked up in the registry.  So the same codec appears under half a dozen names
;;;; — DIVX, DX50, XVID, FMP4, MP4V are all ISO/IEC 14496-2 — and telling them apart is a table.

(in-package #:cassette)

(defun %fourcc (bytes p)
  (map 'string #'code-char (list (aref bytes p) (aref bytes (+ p 1))
                                 (aref bytes (+ p 2)) (aref bytes (+ p 3)))))

(declaim (inline %le16 %le32))
(defun %le16 (bytes p) (logior (aref bytes p) (ash (aref bytes (+ p 1)) 8)))
(defun %le32 (bytes p)
  (logior (aref bytes p) (ash (aref bytes (+ p 1)) 8)
          (ash (aref bytes (+ p 2)) 16) (ash (aref bytes (+ p 3)) 24)))

(defun avi-p (bytes)
  (and (> (length bytes) 12)
       (string= "RIFF" (%fourcc bytes 0))
       (string= "AVI " (%fourcc bytes 8))))

(defparameter +avi-video-codecs+
  '(("DIVX" . "V_MPEG4/ISO/ASP") ("DX50" . "V_MPEG4/ISO/ASP") ("XVID" . "V_MPEG4/ISO/ASP")
    ("FMP4" . "V_MPEG4/ISO/ASP") ("MP4V" . "V_MPEG4/ISO/ASP") ("MP4S" . "V_MPEG4/ISO/ASP")
    ("DIV3" . "V_MPEG4/MS/V3")   ("DIV4" . "V_MPEG4/MS/V3")   ("DIV5" . "V_MPEG4/MS/V3")
    ("MP43" . "V_MPEG4/MS/V3")   ("MP42" . "V_MPEG4/MS/V2")   ("MPG4" . "V_MPEG4/MS/V1")
    ("H264" . "V_MPEG4/ISO/AVC") ("X264" . "V_MPEG4/ISO/AVC") ("AVC1" . "V_MPEG4/ISO/AVC")
    ("MPG1" . "V_MPEG1")         ("MPG2" . "V_MPEG2")         ("MPEG" . "V_MPEG1")
    ("MJPG" . "V_MJPEG")         ("JPEG" . "V_MJPEG")
    ("VP80" . "V_VP8")           ("VP90" . "V_VP9")
    ("HFYU" . "V_HUFFYUV")       ("FFV1" . "V_FFV1"))
  "fccHandler, upper-cased, to the name the rest of this library uses.

   The same codec under six names is not sloppiness in the table: DIVX, DX50, XVID, FMP4 and MP4V
   are genuinely the same bitstream written by different encoders, and a file says which encoder
   wrote it rather than what it wrote.")

(defparameter +wave-format-codecs+
  '((#x0001 . "A_PCM/INT/LIT") (#x0050 . "A_MPEG/L2") (#x0055 . "A_MPEG/L3")
    (#x2000 . "A_AC3") (#x2001 . "A_DTS") (#x00ff . "A_AAC") (#x1600 . "A_AAC")
    (#x0161 . "A_WMA") (#x0162 . "A_WMA") (#x674f . "A_VORBIS") (#x6771 . "A_VORBIS"))
  "wFormatTag, from the Microsoft registry that WAVEFORMATEX indexes.")

(defstruct (avi (:conc-name avi-) (:predicate nil))
  bytes
  (width 0) (height 0)
  (frame-duration 0d0)                          ; seconds, from the main header
  duration
  (tracks '())
  ;; per track: (number scale rate sample-size).  AVI has ONE timing rule and it is these three
  ;; numbers — see AVI-FRAMES — so they are kept rather than folded into a frame duration.
  (timing '())
  (movi-start 0) (movi-end 0))

(defun avi-video-track (a) (find 1 (avi-tracks a) :key #'track-type))
(defun avi-audio-track (a) (find 2 (avi-tracks a) :key #'track-type))

;;; ---- RIFF walking ------------------------------------------------------------------------------

(defmacro do-riff-chunks ((id start size bytes from to) &body body)
  "Every chunk in BYTES[FROM,TO): its four-character ID, its payload START and SIZE.

   Chunks are padded to an even length and the padding byte is NOT counted in the size, which is
   the single most common way to write a RIFF reader that works on most files."
  (let ((p (gensym)) (e (gensym)))
    `(let ((,p ,from) (,e ,to))
       (loop while (<= (+ ,p 8) ,e)
             do (let* ((,id (%fourcc ,bytes ,p))
                       (,size (%le32 ,bytes (+ ,p 4)))
                       (,start (+ ,p 8)))
                  (when (> (+ ,start ,size) ,e) (setf ,size (- ,e ,start)))
                  ,@body
                  (incf ,p (+ 8 ,size (logand ,size 1))))))))

(defun %parse-strl (a bytes from to number)
  "One stream: its header, its format, and what codec those name."
  (let ((tr (make-track :number number)) (scale 1) (rate 25) (sample-size 0) (kind nil))
    (do-riff-chunks (id start size bytes from to)
      (cond
        ((string= id "strh")
         (setf kind (%fourcc bytes start)
               scale (max 1 (%le32 bytes (+ start 20)))
               rate (max 1 (%le32 bytes (+ start 24)))
               sample-size (%le32 bytes (+ start 44)))
         (let ((handler (string-upcase (%fourcc bytes (+ start 4)))))
           (setf (track-codec-id tr) (cdr (assoc handler +avi-video-codecs+ :test #'string=)))))
        ((string= id "strf")
         (cond
           ((string= kind "vids")
            (setf (track-type tr) 1
                  (track-width tr) (%le32 bytes (+ start 4))
                  (track-height tr) (let ((h (%le32 bytes (+ start 8))))
                                      ;; a negative height means the rows are top-down, which
                                      ;; changes nothing about decoding and everything about a
                                      ;; reader that believes the number
                                      (if (> h #x7fffffff) (- #x100000000 h) h)))
            ;; biCompression is the authority when fccHandler was blank, which some muxers leave it
            (let ((comp (string-upcase (%fourcc bytes (+ start 16)))))
              (unless (track-codec-id tr)
                (setf (track-codec-id tr)
                      (cdr (assoc comp +avi-video-codecs+ :test #'string=)))))
            (unless (track-codec-id tr) (setf (track-codec-id tr) "V_UNKNOWN"))
            (when (> size 40)
              (setf (track-codec-private tr) (subseq bytes (+ start 40) (+ start size)))))
           ((string= kind "auds")
            (setf (track-type tr) 2
                  (track-codec-id tr) (or (cdr (assoc (%le16 bytes start) +wave-format-codecs+))
                                          "A_UNKNOWN")
                  (track-channels tr) (max 1 (%le16 bytes (+ start 2)))
                  (track-sample-rate tr) (%le32 bytes (+ start 4))
                  (track-bit-depth tr) (%le16 bytes (+ start 14)))
            (when (> size 18)
              (let ((extra (%le16 bytes (+ start 16))))
                (when (plusp extra)
                  (setf (track-codec-private tr)
                        (subseq bytes (+ start 18) (min (+ start size) (+ start 18 extra))))))))
           (t (setf (track-type tr) 0))))
        (t nil)))
    (push (list number scale rate sample-size) (avi-timing a))
    (when (and (= 1 (track-type tr)) (plusp rate))
      (setf (track-default-duration tr) (round (* 1000000000 (/ scale rate))))
      (setf (avi-frame-duration a) (/ (float scale 1d0) rate)))
    tr))

(defun parse-avi (bytes)
  "Read an AVI's headers.  The frames themselves are walked on demand by the reader."
  (let ((bytes (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
    (unless (avi-p bytes) (%err "not an AVI file"))
    (let ((a (make-avi :bytes bytes))
          (number 0)
          (total-frames 0))
      (do-riff-chunks (id start size bytes 12 (length bytes))
        (cond
          ((and (string= id "LIST") (string= "hdrl" (%fourcc bytes start)))
           (do-riff-chunks (id2 s2 z2 bytes (+ start 4) (+ start size))
             (cond
               ((string= id2 "avih")
                (setf total-frames (%le32 bytes (+ s2 16))
                      (avi-width a) (%le32 bytes (+ s2 32))
                      (avi-height a) (%le32 bytes (+ s2 36)))
                (let ((us (%le32 bytes s2)))
                  (when (plusp us) (setf (avi-frame-duration a) (/ us 1000000d0)))))
               ((and (string= id2 "LIST") (string= "strl" (%fourcc bytes s2)))
                (push (%parse-strl a bytes (+ s2 4) (+ s2 z2) (incf number)) (avi-tracks a)))
               (t nil))))
          ((and (string= id "LIST") (string= "movi" (%fourcc bytes start)))
           (setf (avi-movi-start a) (+ start 4) (avi-movi-end a) (+ start size)))
          (t nil)))
      (setf (avi-tracks a) (nreverse (avi-tracks a)))
      (when (and (plusp total-frames) (plusp (avi-frame-duration a)))
        (setf (avi-duration a) (* total-frames (avi-frame-duration a))))
      a)))

;;; ---- the frames --------------------------------------------------------------------------------

(defun %avi-stream-number (id)
  "The stream a chunk belongs to, from the two decimal digits its ID begins with."
  (let ((a (digit-char-p (char id 0))) (b (digit-char-p (char id 1))))
    (and a b (+ (* 10 a) b))))

(defun avi-frames (a)
  "Every frame in the file, in the order it was written.

   AVI carries no timestamps, so they are COUNTED rather than read: a video chunk's time is its
   position in that stream times the frame duration.  A chunk of zero length is not a frame at all —
   it is how AVI says a video frame was dropped, and handing it to a decoder as an empty picture is
   how a file plays two frames short of its length."
  (let* ((bytes (avi-bytes a))
         (counts (make-hash-table))
         (out '()))
    (labels ((emit (sn tr start size)
               ;; ONE RULE FOR BOTH MEDIA.  A stream header carries a scale, a rate and a sample
               ;; size, and the time of a chunk is (samples so far) * scale / rate.  A sample size
               ;; of zero — which is what video always uses, and compressed audio usually — means
               ;; one chunk IS one sample; a nonzero one means the samples are that many bytes each,
               ;; which is how uncompressed audio in a container with no timestamps stays in step.
               (destructuring-bind (scale rate ss)
                   (or (cdr (assoc (1+ sn) (avi-timing a))) '(1 25 0))
                 (let* ((before (gethash sn counts 0))
                        (samples (if (plusp ss) (floor before ss) before)))
                   (incf (gethash sn counts 0) (if (plusp ss) size 1))
                   (push (make-block-frame
                          :track tr
                          :timecode (round (* 1000000 samples scale) rate)
                          :data (subseq bytes start (+ start size))
                          :keyframe-p t)
                         out))))
             (walk (from to)
               (do-riff-chunks (id start size bytes from to)
                 (if (and (string= id "LIST")
                          (member (%fourcc bytes start) '("rec " "movi") :test #'string=))
                     (walk (+ start 4) (+ start size))
                     (let* ((sn (%avi-stream-number id))
                            (tr (and sn (nth sn (avi-tracks a)))))
                       (when (and tr (plusp size)) (emit sn tr start size)))))))
      (walk (avi-movi-start a) (avi-movi-end a))
      ;; OpenDML splits a file over 2 GB into several RIFF chunks; the later ones are `AVIX', and a
      ;; reader that stops at the first RIFF plays the first two gigabytes of a long film
      (let ((p (+ 8 (%le32 bytes 4))))
        (loop while (and (< (+ p 12) (length bytes))
                         (string= "RIFF" (%fourcc bytes p))
                         (string= "AVIX" (%fourcc bytes (+ p 8))))
              do (let ((end (min (length bytes) (+ p 8 (%le32 bytes (+ p 4))))))
                   (do-riff-chunks (id start size bytes (+ p 12) end)
                     (when (and (string= id "LIST") (string= "movi" (%fourcc bytes start)))
                       (walk (+ start 4) (+ start size))))
                   (setf p end)))))
    (nreverse out)))

;;; ---- the reader --------------------------------------------------------------------------------

(defstruct (avi-reader (:conc-name avr-) (:constructor %make-avi-reader))
  avi (frames '()))

(defun make-avi-reader (a &optional frames)
  (%make-avi-reader :avi a :frames (or frames (avi-frames a))))

(defun read-next-avi-frame (r) (pop (avr-frames r)))

(defconstant +avi-tick+ 1000 "Nanoseconds per AVI timestamp unit: these are counted in microseconds.")
