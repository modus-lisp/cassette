;;;; mp4.lisp — ISO base media file format (MP4 / M4A / MOV), demuxed.
;;;;
;;;; The second container, and the one that made the repo change its name.  It is a very different
;;;; shape from Matroska and the difference is worth stating, because it is why the two readers
;;;; share their OUTPUT and almost none of their code:
;;;;
;;;;   * Matroska INTERLEAVES.  Frames arrive in the file in the order they are played, each
;;;;     carrying its own timestamp, and a demuxer is a cursor that walks forward.  Seeking needs a
;;;;     Cues index, and a file without one has to be scanned.
;;;;   * MP4 TABULATES.  The samples are a flat run of bytes in `mdat` with nothing to delimit
;;;;     them, and everything about them — where each one starts, how long it is, when it decodes,
;;;;     when it displays, whether it is a sync sample — lives in parallel tables in `moov`.  So
;;;;     the whole index is read up front, and seeking is a binary search rather than a scan.
;;;;
;;;; That second shape is strictly better for seeking and strictly worse for streaming, which is
;;;; why fragmented MP4 exists: `moof` boxes carrying a little index each, ahead of their own
;;;; `mdat`.  Both are read here.
;;;;
;;;; WHAT COMES OUT IS THE SAME AS WEBM'S.  TRACK and BLOCK-FRAME structs, timestamps in the
;;;; container's own tick, so player.lisp does not know or care which reader it is pulling from.
;;;; Codec identifiers are normalised to Matroska's vocabulary ("V_VP8", "A_OPUS", "A_AAC") for the
;;;; same reason: the caller should be answering "can I decode this codec", not "which of two
;;;; spellings of this codec does this container happen to use".
;;;;
;;;; DECODE ORDER, NOT DISPLAY ORDER.  A block's timecode is its COMPOSITION time (pts) because
;;;; that is what a player shows it at, but frames come out in DECODE order (dts), because that is
;;;; the order a decoder can accept them in.  With B-frames those differ, and a reader that sorted
;;;; by pts would hand a decoder frames it cannot yet decode.

(in-package #:cassette)

;;; ---- big-endian readers -------------------------------------------------------------------

(declaim (inline be16 be24 be32 be64))
(defun be16 (b i) (logior (ash (aref b i) 8) (aref b (+ i 1))))
(defun be24 (b i) (logior (ash (aref b i) 16) (ash (aref b (+ i 1)) 8) (aref b (+ i 2))))
(defun be32 (b i) (logior (ash (aref b i) 24) (ash (aref b (+ i 1)) 16)
                          (ash (aref b (+ i 2)) 8) (aref b (+ i 3))))
(defun be64 (b i) (logior (ash (be32 b i) 32) (be32 b (+ i 4))))

(defun s32 (v) (if (logbitp 31 v) (- v (ash 1 32)) v))
(defun s64 (v) (if (logbitp 63 v) (- v (ash 1 64)) v))

(defun box-type (b i)
  (map 'string #'code-char (subseq b i (+ i 4))))

;;; ---- box walking ---------------------------------------------------------------------------

(defun read-box (bytes pos end)
  "Read one box header at POS.  Returns (values type payload-start payload-end), or NIL at END.
   Handles the 64-bit `largesize' form and the `size 0 means to end of file' form."
  (when (> (+ pos 8) end) (return-from read-box nil))
  (let* ((size (be32 bytes pos))
         (type (box-type bytes (+ pos 4)))
         (hdr 8))
    (cond ((= size 1)
           (when (> (+ pos 16) end) (%err "MP4: truncated largesize box"))
           (setf size (be64 bytes (+ pos 8)) hdr 16))
          ((= size 0) (setf size (- end pos))))
    (when (< size hdr) (%err "MP4: box ~a claims size ~d" type size))
    (values type (+ pos hdr) (min end (+ pos size)))))

(defmacro do-boxes ((type start end bytes pos limit) &body body)
  "Iterate the boxes in BYTES[POS..LIMIT), binding TYPE and the payload bounds."
  (let ((p (gensym)) (l (gensym)) (e (gensym)))
    `(let ((,p ,pos) (,l ,limit))
       (loop
         (when (>= ,p ,l) (return))
         (multiple-value-bind (,type ,start ,e) (read-box ,bytes ,p ,l)
           (declare (ignorable ,type ,start))
           (unless ,type (return))
           (let ((,end ,e))
             (declare (ignorable ,end))
             ,@body
             (setf ,p ,end)))))))

(defun find-box (bytes pos limit want)
  "(values payload-start payload-end) of the first box named WANT, or NIL."
  (do-boxes (type start end bytes pos limit)
    (when (string= type want) (return-from find-box (values start end))))
  nil)

(defun find-path (bytes pos limit path)
  "Walk a list of box names down from BYTES[POS..LIMIT)."
  (let ((s pos) (e limit))
    (dolist (name path (values s e))
      (multiple-value-bind (s2 e2) (find-box bytes s e name)
        (unless s2 (return-from find-path nil))
        (setf s s2 e e2)))))

(declaim (inline full-box-version full-box-flags))
(defun full-box-version (b i) (aref b i))
(defun full-box-flags (b i) (be24 b (+ i 1)))

;;; ---- the sample table ------------------------------------------------------------------------
;;;
;;; One of these per track.  Parallel simple-arrays rather than a vector of structs: a two-hour
;;; film is a few hundred thousand samples, and five typed arrays is a few megabytes where a
;;; quarter-million five-slot objects is an order of magnitude more and a GC problem besides.

(defstruct (sample-table (:conc-name st-))
  (count 0 :type fixnum)
  (offset (make-array 0 :element-type '(unsigned-byte 64)) :type (simple-array (unsigned-byte 64) (*)))
  (size (make-array 0 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (*)))
  (dts (make-array 0 :element-type '(signed-byte 64)) :type (simple-array (signed-byte 64) (*)))
  (pts (make-array 0 :element-type '(signed-byte 64)) :type (simple-array (signed-byte 64) (*)))
  (sync (make-array 0 :element-type 'bit) :type simple-bit-vector)
  (timescale 1000 :type fixnum))

(defun st-time-seconds (st i)
  (/ (aref (st-pts st) i) (float (st-timescale st) 1d0)))

;;; ---- the container ---------------------------------------------------------------------------

(defstruct (mp4 (:conc-name mp4-) (:predicate nil))
  bytes
  brand
  (timescale 1000)                      ; the movie header's, for DURATION only
  duration                              ; seconds, or NIL
  (tracks '())                          ; TRACK structs, as the WebM reader produces
  (tables (make-hash-table))            ; track number -> SAMPLE-TABLE
  (fragmented nil))

(defconstant +mp4-tick+ 1000
  "Nanoseconds per BLOCK-FRAME timecode tick for MP4: one microsecond.  WebM's default is a
millisecond, which is coarser than a 60 fps frame interval; MP4's per-track timescales are
arbitrary, so rather than inherit one this normalises to a tick fine enough for any of them.")

(defun mp4-p (bytes)
  "True when BYTES look like an ISO base media file: a box we recognise at offset 0."
  (and (>= (length bytes) 12)
       (let ((type (box-type bytes 4)))
         (and (member type '("ftyp" "styp" "moov" "free" "skip" "mdat" "wide" "pnot")
                      :test #'string=)
              t))))

;;; ---- codec identification --------------------------------------------------------------------

(defun %normalize-codec (fourcc)
  "An MP4 sample-entry name as the Matroska identifier for the same codec, so a caller asks about
   codecs rather than about containers.  NIL when we have no name for it — which is reported as
   the raw fourcc rather than guessed at."
  (cond ((member fourcc '("avc1" "avc3") :test #'string=) "V_MPEG4/ISO/AVC")
        ((member fourcc '("hvc1" "hev1") :test #'string=) "V_MPEGH/ISO/HEVC")
        ((string= fourcc "av01") "V_AV1")
        ((string= fourcc "vp09") "V_VP9")
        ((string= fourcc "vp08") "V_VP8")
        ((string= fourcc "mp4a") "A_AAC")       ; refined from the esds object type below
        ((string= fourcc "Opus") "A_OPUS")
        ((string= fourcc "alac") "A_ALAC")
        ((member fourcc '("ac-3" "ec-3") :test #'string=) "A_AC3")
        (t nil)))

(defun %esds-decoder-config (bytes start end)
  "The DecoderSpecificInfo from an esds box — for AAC this is the AudioSpecificConfig, which is
   what a decoder needs and what Matroska would carry as CodecPrivate.  Returns (values config
   object-type-indication)."
  ;; esds is a full box, then an MPEG-4 descriptor tree with 1-byte tags and varint lengths.
  (let ((p (+ start 4)) (oti nil) (config nil))
    (labels ((desc-len ()
               (let ((v 0))
                 (loop repeat 4
                       for b = (aref bytes p)
                       do (incf p) (setf v (logior (ash v 7) (logand b #x7f)))
                          (unless (logtest b #x80) (return)))
                 v))
             (walk (limit)
               (loop while (< (+ p 2) limit) do
                 (let* ((tag (aref bytes p)))
                   (incf p)
                   (let* ((len (desc-len)) (body-end (min limit (+ p len))))
                     (case tag
                       (#x03 (incf p 3) (walk body-end))          ; ES_Descriptor: id + flags
                       (#x04 (setf oti (aref bytes p))            ; DecoderConfigDescriptor
                             (incf p 13) (walk body-end))
                       (#x05 (setf config (subseq bytes p body-end)))  ; DecoderSpecificInfo
                       (t nil))
                     (setf p body-end))))))
      (ignore-errors (walk end)))
    (values config oti)))

(defun %parse-sample-entry (tr bytes start end)
  "Read the first entry of an `stsd' box into TR: codec id, dimensions or rate, codec private."
  (let ((p (+ start 8)))                ; full box + entry_count
    (multiple-value-bind (type es ee) (read-box bytes p end)
      (unless type (return-from %parse-sample-entry nil))
      (setf (track-codec-id tr) (or (%normalize-codec type) type))
      (cond
        ((= (track-type tr) 1)
         ;; VisualSampleEntry: 6 reserved + 2 data_ref + 16 pre_defined/reserved, then w,h
         (setf (track-width tr) (be16 bytes (+ es 24))
               (track-height tr) (be16 bytes (+ es 26)))
         ;; the codec's own configuration box sits after the 78-byte entry
         (do-boxes (btype bs be bytes (+ es 78) ee)
           (when (member btype '("avcC" "hvcC" "av1C" "vpcC") :test #'string=)
             (setf (track-codec-private tr) (subseq bytes bs be)))))
        ((= (track-type tr) 2)
         ;; AudioSampleEntry: 6 reserved + 2 data_ref + 8 reserved, channels, size, 4, then rate
         (setf (track-channels tr) (be16 bytes (+ es 16))
               (track-bit-depth tr) (be16 bytes (+ es 18))
               (track-sample-rate tr) (float (ash (be32 bytes (+ es 24)) -16) 1d0))
         (do-boxes (btype bs be bytes (+ es 28) ee)
           (cond
             ((string= btype "esds")
              (multiple-value-bind (config oti) (%esds-decoder-config bytes bs be)
                (when config (setf (track-codec-private tr) config))
                ;; 0x40 is MPEG-4 audio (AAC); 0x69/0x6b are MPEG-2/1 audio, i.e. MP3 in MP4
                (when (member oti '(#x69 #x6b)) (setf (track-codec-id tr) "A_MPEG/L3"))))
             ((string= btype "dOps")
              (setf (track-codec-id tr) "A_OPUS" (track-codec-private tr) (subseq bytes bs be)))
             ((string= btype "alac")
              (setf (track-codec-private tr) (subseq bytes bs be)))))))
      t)))

;;; ---- the tables ---------------------------------------------------------------------------------

(defun %u32-table (bytes start end &key (stride 4) (fields 1))
  "The entry array of a full box whose payload is version/flags, a count, then COUNT records of
   FIELDS 32-bit values.  Returns a (count . simple-array) pair."
  (declare (ignore stride))
  (let* ((n (be32 bytes (+ start 4)))
         (v (make-array (* n fields) :element-type '(unsigned-byte 32))))
    (when (> (+ start 8 (* 4 n fields)) end) (%err "MP4: table overruns its box"))
    (dotimes (i (* n fields) (cons n v))
      (setf (aref v i) (be32 bytes (+ start 8 (* 4 i)))))))

(defun %sizes-table (bytes stbl-start stbl-end)
  "Per-sample byte sizes, from `stsz' (uniform or explicit) or `stz2' (packed 4/8/16-bit fields)."
  (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "stsz")
    (when s
      (let ((uniform (be32 bytes (+ s 4))) (n (be32 bytes (+ s 8))))
        (let ((sizes (make-array n :element-type '(unsigned-byte 32))))
          (cond ((plusp uniform) (fill sizes uniform))
                (t (when (> (+ s 12 (* 4 n)) e) (%err "MP4: stsz overruns its box"))
                   (dotimes (i n) (setf (aref sizes i) (be32 bytes (+ s 12 (* 4 i)))))))
          (return-from %sizes-table sizes)))))
  (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "stz2")
    (declare (ignorable e))
    (unless s (%err "MP4: track has neither stsz nor stz2"))
    (let* ((field (aref bytes (+ s 7)))
           (n (be32 bytes (+ s 8)))
           (sizes (make-array n :element-type '(unsigned-byte 32))))
      (ecase field
        (16 (dotimes (i n) (setf (aref sizes i) (be16 bytes (+ s 12 (* 2 i))))))
        (8 (dotimes (i n) (setf (aref sizes i) (aref bytes (+ s 12 i)))))
        (4 (dotimes (i n)
             (let ((b (aref bytes (+ s 12 (floor i 2)))))
               (setf (aref sizes i) (if (evenp i) (ash b -4) (logand b #x0f)))))))
      sizes)))

(defun %chunk-offsets (bytes stbl-start stbl-end)
  "Chunk file offsets, from 32-bit `stco' or 64-bit `co64'."
  (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "stco")
    (declare (ignorable e))
    (when s
      (let* ((n (be32 bytes (+ s 4)))
             (v (make-array n :element-type '(unsigned-byte 64))))
        (dotimes (i n) (setf (aref v i) (be32 bytes (+ s 8 (* 4 i)))))
        (return-from %chunk-offsets v))))
  (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "co64")
    (declare (ignorable e))
    (unless s (%err "MP4: track has neither stco nor co64"))
    (let* ((n (be32 bytes (+ s 4)))
           (v (make-array n :element-type '(unsigned-byte 64))))
      (dotimes (i n) (setf (aref v i) (be64 bytes (+ s 8 (* 8 i)))))
      v)))

(defun %build-sample-table (bytes stbl-start stbl-end timescale)
  "Assemble one track's sample index from the six or seven parallel tables that describe it."
  (let* ((sizes (%sizes-table bytes stbl-start stbl-end))
         (sample-count (length sizes))
         (chunk-offsets (%chunk-offsets bytes stbl-start stbl-end))
         (stsc (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "stsc")
                 (unless s (%err "MP4: track has no stsc"))
                 (%u32-table bytes s e :fields 3)))
         (stts (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "stts")
                 (unless s (%err "MP4: track has no stts"))
                 (%u32-table bytes s e :fields 2)))
         (ctts-signed nil)
         (ctts (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "ctts")
                 (when s
                   (setf ctts-signed (>= (full-box-version bytes s) 1))
                   (%u32-table bytes s e :fields 2))))
         (stss (multiple-value-bind (s e) (find-box bytes stbl-start stbl-end "stss")
                 (when s (%u32-table bytes s e :fields 1))))
         (st (make-sample-table
              :count sample-count :size sizes :timescale timescale
              :offset (make-array sample-count :element-type '(unsigned-byte 64))
              :dts (make-array sample-count :element-type '(signed-byte 64))
              :pts (make-array sample-count :element-type '(signed-byte 64))
              ;; no stss means every sample is a sync sample, which is what an all-intra or an
              ;; audio track is; with an stss, only the ones it lists are
              :sync (make-array sample-count :element-type 'bit
                                             :initial-element (if stss 0 1)))))
    ;; offsets: walk the sample-to-chunk runs, laying samples end to end inside each chunk
    (let ((sample 0) (entries (cdr stsc)) (n (car stsc)) (nchunks (length chunk-offsets)))
      (dotimes (i n)
        (let* ((first-chunk (aref entries (* 3 i)))
               (per-chunk (aref entries (+ (* 3 i) 1)))
               (last-chunk (if (< (1+ i) n) (1- (aref entries (* 3 (1+ i)))) nchunks)))
          (loop for chunk from first-chunk to (min last-chunk nchunks)
                do (let ((off (aref chunk-offsets (1- chunk))))
                     (dotimes (k per-chunk)
                       (declare (ignorable k))
                       (when (< sample sample-count)
                         (setf (aref (st-offset st) sample) off)
                         (incf off (aref sizes sample))
                         (incf sample)))))))
      (when (< sample sample-count)
        (%err "MP4: stsc described ~d samples, stsz has ~d" sample sample-count)))
    ;; decode times: the cumulative sum of the stts deltas
    (let ((sample 0) (now 0) (entries (cdr stts)) (n (car stts)))
      (dotimes (i n)
        (let ((count (aref entries (* 2 i))) (delta (aref entries (+ (* 2 i) 1))))
          (dotimes (k count)
            (declare (ignorable k))
            (when (< sample sample-count)
              (setf (aref (st-dts st) sample) now
                    (aref (st-pts st) sample) now)
              (incf now delta)
              (incf sample))))))
    ;; composition offsets, when the track has B-frames
    (when ctts
      (let ((sample 0) (entries (cdr ctts)) (n (car ctts)))
        (dotimes (i n)
          (let ((count (aref entries (* 2 i)))
                (offset (let ((v (aref entries (+ (* 2 i) 1)))) (if ctts-signed (s32 v) v))))
            (dotimes (k count)
              (declare (ignorable k))
              (when (< sample sample-count)
                (setf (aref (st-pts st) sample) (+ (aref (st-dts st) sample) offset))
                (incf sample)))))))
    ;; sync samples, which are 1-based in the file
    (when stss
      (let ((entries (cdr stss)))
        (dotimes (i (car stss))
          (let ((s (aref entries i)))
            (when (<= 1 s sample-count) (setf (aref (st-sync st) (1- s)) 1))))))
    st))

(defun %apply-edit-list (bytes trak-start trak-end st movie-timescale)
  "Shift a track's times by its edit list, if it has one.

   THIS IS WHAT MAKES A B-FRAME FILE START AT ZERO.  A track with B-frames has composition
   offsets, so its first sample's presentation time is a frame or two AFTER its decode time —
   and the file compensates with an `elst' whose media_time says which media instant the
   presentation begins at.  Ignore it and every timestamp in the file is early by that much,
   which is exactly the 0.133 s an ffmpeg H.264 file came back with before this existed.

   An empty edit (media_time -1) is the other direction: blank presentation time inserted in
   front, which is how an encoder's priming delay is declared."
  (multiple-value-bind (es ee) (find-path bytes trak-start trak-end '("edts" "elst"))
    (declare (ignorable ee))
    (unless es (return-from %apply-edit-list nil))
    (let* ((v (full-box-version bytes es))
           (n (be32 bytes (+ es 4)))
           (p (+ es 8))
           (shift 0) (delay 0))
      (dotimes (i n)
        (let (segdur mtime)
          (if (>= v 1)
              (progn (setf segdur (be64 bytes p) mtime (s64 (be64 bytes (+ p 8)))) (incf p 20))
              (progn (setf segdur (be32 bytes p) mtime (s32 (be32 bytes (+ p 4)))) (incf p 12)))
          (cond ((= mtime -1)
                 ;; segment_duration is in the MOVIE timescale, the media times in the track's
                 (incf delay (round (* segdur (st-timescale st)) (max 1 movie-timescale))))
                ((zerop i) (setf shift mtime)))))
      (when (or (/= shift 0) (/= delay 0))
        (dotimes (i (st-count st))
          (decf (aref (st-dts st) i) (- shift delay))
          (decf (aref (st-pts st) i) (- shift delay))))
      (list shift delay))))

;;; ---- fragments -------------------------------------------------------------------------------

(defun %trex-defaults (bytes moov-start moov-end)
  "track_ID -> (default-duration default-size default-flags) from mvex/trex."
  (let ((h (make-hash-table)))
    (multiple-value-bind (s e) (find-box bytes moov-start moov-end "mvex")
      (when s
        (do-boxes (type bs be bytes s e)
          (when (string= type "trex")
            (setf (gethash (be32 bytes (+ bs 4)) h)
                  (list (be32 bytes (+ bs 12)) (be32 bytes (+ bs 16)) (be32 bytes (+ bs 20))))))))
    h))

(defstruct (frag-acc (:conc-name fa-))
  (offsets '()) (sizes '()) (dts '()) (pts '()) (sync '()) (count 0))

(defun %read-traf (bytes ts te moof-start trex acc)
  "One track fragment: its defaults, its start time, and every run of samples in it."
  (let ((tid nil) (base nil) (dur nil) (size nil) (flags nil) (base-is-moof t) (dts 0))
    (multiple-value-bind (fs fe) (find-box bytes ts te "tfhd")
      (declare (ignorable fe))
      (unless fs (return-from %read-traf nil))
      (let ((f (full-box-flags bytes fs)) (p (+ fs 8)))
        (setf tid (be32 bytes (+ fs 4)))
        (when (logtest f #x01) (setf base (be64 bytes p) base-is-moof nil) (incf p 8))
        (when (logtest f #x02) (incf p 4))                    ; sample description index
        (when (logtest f #x08) (setf dur (be32 bytes p)) (incf p 4))
        (when (logtest f #x10) (setf size (be32 bytes p)) (incf p 4))
        (when (logtest f #x20) (setf flags (be32 bytes p)) (incf p 4))
        (when (logtest f #x020000) (setf base-is-moof t))))
    ;; what the movie extends box said this track's defaults are
    (let ((d (gethash tid trex)))
      (when d
        (setf dur (or dur (first d)) size (or size (second d)) flags (or flags (third d)))))
    ;; tfdt, when present, is this fragment's absolute start on the track's timeline
    (multiple-value-bind (ds de) (find-box bytes ts te "tfdt")
      (declare (ignorable de))
      (when ds
        (setf dts (if (>= (full-box-version bytes ds) 1)
                      (be64 bytes (+ ds 4))
                      (be32 bytes (+ ds 4))))))
    (let ((cell (or (gethash tid acc) (setf (gethash tid acc) (make-frag-acc)))))
      (do-boxes (rtype rs re bytes ts te)
        (declare (ignorable re))
        (when (string= rtype "trun")
          (let ((f (full-box-flags bytes rs))
                (n (be32 bytes (+ rs 4)))
                (p (+ rs 8))
                (data-off 0)
                (first-flags nil))
            (when (logtest f #x001) (setf data-off (s32 (be32 bytes p))) (incf p 4))
            (when (logtest f #x004) (setf first-flags (be32 bytes p)) (incf p 4))
            (let ((off (+ (if base-is-moof moof-start (or base moof-start)) data-off)))
              (dotimes (i n)
                (let ((sdur dur) (ssize size) (sflags flags) (cto 0))
                  (when (logtest f #x100) (setf sdur (be32 bytes p)) (incf p 4))
                  (when (logtest f #x200) (setf ssize (be32 bytes p)) (incf p 4))
                  (when (logtest f #x400) (setf sflags (be32 bytes p)) (incf p 4))
                  (when (logtest f #x800) (setf cto (s32 (be32 bytes p))) (incf p 4))
                  (when (and (zerop i) first-flags) (setf sflags first-flags))
                  (unless (and sdur ssize)
                    (%err "MP4: a trun sample has neither its own size/duration nor a default"))
                  (push off (fa-offsets cell))
                  (push ssize (fa-sizes cell))
                  (push dts (fa-dts cell))
                  (push (+ dts cto) (fa-pts cell))
                  ;; bit 16 of the flags word is sample_is_non_sync_sample
                  (push (if (and sflags (logtest sflags #x00010000)) 0 1) (fa-sync cell))
                  (incf (fa-count cell))
                  (incf off ssize)
                  (incf dts sdur))))))))
    tid))

(defun %read-fragments (m bytes end trex)
  "Walk every `moof' in the file and build each track's index from the runs inside it.

   A fragmented file's `moov' carries empty tables on purpose: the index is distributed, a little
   of it in front of each chunk of media, which is exactly what makes the format streamable and
   what a tabulated file gives up to be seekable."
  (let ((acc (make-hash-table)))
    (do-boxes (type ms me bytes 0 end)
      (when (string= type "moof")
        (let ((moof-start (- ms 8)))
          (do-boxes (ttype ts te bytes ms me)
            (when (string= ttype "traf")
              (%read-traf bytes ts te moof-start trex acc))))))
    ;; the accumulated lists become the same typed arrays the tabulated path produces
    (maphash
     (lambda (tid cell)
       (let* ((tr (find tid (mp4-tracks m) :key #'track-uid))
              (num (if tr (track-number tr) tid))
              (old (gethash num (mp4-tables m)))
              (scale (if old (st-timescale old) 1000))
              (n (fa-count cell)))
         (setf (gethash num (mp4-tables m))
               (make-sample-table
                :count n :timescale scale
                :offset (make-array n :element-type '(unsigned-byte 64)
                                      :initial-contents (nreverse (fa-offsets cell)))
                :size (make-array n :element-type '(unsigned-byte 32)
                                    :initial-contents (nreverse (fa-sizes cell)))
                :dts (make-array n :element-type '(signed-byte 64)
                                   :initial-contents (nreverse (fa-dts cell)))
                :pts (make-array n :element-type '(signed-byte 64)
                                   :initial-contents (nreverse (fa-pts cell)))
                :sync (make-array n :element-type 'bit
                                    :initial-contents (nreverse (fa-sync cell)))))))
     acc)
    (setf (mp4-fragmented m) t)))

;;; ---- the top level -----------------------------------------------------------------------------

(defun parse-mp4 (bytes)
  "Parse an ISO base media file: the movie header, every track's description, and the sample
   index — tabulated from `moov' and, when the file is fragmented, accumulated from every `moof'."
  (declare (type octets bytes))
  (unless (mp4-p bytes) (%err "not an ISO base media (MP4) file"))
  (let ((m (make-mp4 :bytes bytes)) (end (length bytes)) (number 0))
    (multiple-value-bind (fs fe) (find-box bytes 0 end "ftyp")
      (declare (ignore fe))
      (when fs (setf (mp4-brand m) (box-type bytes fs))))
    (multiple-value-bind (moov-start moov-end) (find-box bytes 0 end "moov")
      (unless moov-start (%err "MP4: no moov box"))
      ;; the movie header, for the overall duration
      (multiple-value-bind (s e) (find-box bytes moov-start moov-end "mvhd")
        (declare (ignore e))
        (when s
          (let ((v (full-box-version bytes s)))
            (if (>= v 1)
                (let ((ts (be32 bytes (+ s 20))) (du (be64 bytes (+ s 24))))
                  (setf (mp4-timescale m) ts)
                  ;; a zero duration is what an `empty_moov' fragmented file writes: not a
                  ;; six-hundredths-of-nothing film, but "ask the samples"
                  (when (and (plusp ts) (plusp du)) (setf (mp4-duration m) (/ du (float ts 1d0)))))
                (let ((ts (be32 bytes (+ s 12))) (du (be32 bytes (+ s 16))))
                  (setf (mp4-timescale m) ts)
                  (when (and (plusp ts) (plusp du) (/= du #xffffffff))
                    (setf (mp4-duration m) (/ du (float ts 1d0)))))))))
      ;; the tracks
      (do-boxes (type ts te bytes moov-start moov-end)
        (when (string= type "trak")
          (let ((tr (make-track :number (incf number))))
            (multiple-value-bind (s e) (find-box bytes ts te "tkhd")
              (declare (ignore e))
              (when s
                (setf (track-uid tr)
                      (if (>= (full-box-version bytes s) 1) (be32 bytes (+ s 20)) (be32 bytes (+ s 12))))))
            (multiple-value-bind (mdia-s mdia-e) (find-box bytes ts te "mdia")
              (unless mdia-s (%err "MP4: trak with no mdia"))
              (let ((timescale 1000))
                (multiple-value-bind (s e) (find-box bytes mdia-s mdia-e "mdhd")
                  (declare (ignore e))
                  (when s
                    (let ((v (full-box-version bytes s)))
                      (setf timescale (if (>= v 1) (be32 bytes (+ s 20)) (be32 bytes (+ s 12))))
                      (let ((du (if (>= v 1) (be64 bytes (+ s 24)) (be32 bytes (+ s 16)))))
                        (when (and (plusp timescale) (/= du #xffffffff))
                          (setf (track-default-duration tr) nil))))))
                ;; what kind of track this is
                (multiple-value-bind (s e) (find-box bytes mdia-s mdia-e "hdlr")
                  (declare (ignore e))
                  (when s
                    (let ((handler (box-type bytes (+ s 8))))
                      (setf (track-type tr) (cond ((string= handler "vide") 1)
                                                  ((string= handler "soun") 2)
                                                  ((string= handler "sbtl") 17)
                                                  (t 0))))))
                (multiple-value-bind (stbl-s stbl-e)
                    (find-path bytes mdia-s mdia-e '("minf" "stbl"))
                  (unless stbl-s (%err "MP4: trak with no stbl"))
                  (multiple-value-bind (s e) (find-box bytes stbl-s stbl-e "stsd")
                    (when s (%parse-sample-entry tr bytes s e)))
                  (when (member (track-type tr) '(1 2))
                    (push tr (mp4-tracks m))
                    (setf (gethash (track-number tr) (mp4-tables m))
                          (handler-case (%build-sample-table bytes stbl-s stbl-e timescale)
                            (cassette-error ()
                              ;; a fragmented file's moov carries empty tables on purpose
                              (make-sample-table :count 0 :timescale timescale))))
                    ;; where this track's edit list lives, to be applied once the table is final
                    (setf (track-name tr) (list ts te)))))))))
      (setf (mp4-tracks m) (nreverse (mp4-tracks m)))
      ;; fragmented: the real index is in the moof boxes
      (when (or (find-box bytes 0 end "moof")
                (every (lambda (tr) (zerop (st-count (gethash (track-number tr) (mp4-tables m)))))
                       (mp4-tracks m)))
        (when (find-box bytes 0 end "moof")
          (%read-fragments m bytes end (%trex-defaults bytes moov-start moov-end))))
      ;; The edit list goes on LAST, because for a fragmented file the tables above are the empty
      ;; ones from moov and the real index only exists after the moof walk — shifting the empty
      ;; table would shift nothing and the shift would then be lost.
      (dolist (tr (mp4-tracks m))
        (let ((where (track-name tr)))
          (setf (track-name tr) nil)
          (when (consp where)
            (%apply-edit-list bytes (first where) (second where)
                              (gethash (track-number tr) (mp4-tables m))
                              (mp4-timescale m)))))
      ;; a duration the movie header did not give us, from the samples themselves
      (unless (mp4-duration m)
        (let ((d 0d0))
          (dolist (tr (mp4-tracks m))
            (let ((st (gethash (track-number tr) (mp4-tables m))))
              (when (plusp (st-count st))
                (setf d (max d (st-time-seconds st (1- (st-count st))))))))
          (when (plusp d) (setf (mp4-duration m) d))))
      m)))

(defun mp4-video-track (m) (find 1 (mp4-tracks m) :key #'track-type))
(defun mp4-audio-track (m) (find 2 (mp4-tracks m) :key #'track-type))
(defun mp4-track (m number) (find number (mp4-tracks m) :key #'track-number))

;;; ---- reading samples out ------------------------------------------------------------------------

(defstruct (mp4-reader (:conc-name mr-) (:constructor %make-mp4-reader))
  mp4
  cursors)                              ; list of (track-number . next-index), by track

(defun make-mp4-reader (m &key (start 0))
  "A cursor over M's samples in decode order.  START is a sample index per track, as SEEK-MP4
   computes it."
  (%make-mp4-reader :mp4 m
                    :cursors (mapcar (lambda (tr) (cons (track-number tr) start)) (mp4-tracks m))))

(defun %sample-frame (m tr st i)
  "One sample as a BLOCK-FRAME, with its timecode in +MP4-TICK+ units."
  (let* ((off (aref (st-offset st) i))
         (size (aref (st-size st) i))
         (bytes (mp4-bytes m))
         (scale (st-timescale st)))
    (when (> (+ off size) (length bytes)) (%err "MP4: sample ~d runs past the end of the file" i))
    (make-block-frame
     :track tr
     ;; presentation time, in microseconds, which is what a player shows it at
     :timecode (round (* (aref (st-pts st) i) 1000000) scale)
     :data (subseq bytes off (+ off size))
     :keyframe-p (= 1 (aref (st-sync st) i))
     :invisible-p nil :discardable-p nil
     :duration (when (< (1+ i) (st-count st))
                 (round (* (- (aref (st-dts st) (1+ i)) (aref (st-dts st) i)) 1000000) scale))
     :discard-padding 0)))

(defun read-next-mp4-frame (r)
  "The next sample across all tracks, in DECODE order.  Returns a BLOCK-FRAME or NIL at the end."
  (let* ((m (mr-mp4 r)) (best nil) (best-dts nil))
    (dolist (cell (mr-cursors r))
      (let* ((num (car cell)) (i (cdr cell))
             (st (gethash num (mp4-tables m))))
        (when (and st (< i (st-count st)))
          ;; compare in seconds, because two tracks may be on different timescales
          (let ((d (/ (aref (st-dts st) i) (float (st-timescale st) 1d0))))
            (when (or (null best-dts) (< d best-dts))
              (setf best cell best-dts d))))))
    (when best
      (let* ((num (car best)) (i (cdr best))
             (st (gethash num (mp4-tables m)))
             (tr (mp4-track m num)))
        (incf (cdr best))
        (%sample-frame m tr st i)))))

(defun mp4-sync-sample-before (st seconds)
  "The index of the last sync sample whose presentation time is at or before SECONDS, or 0.
   A binary search, which is what the tabulated form buys over Matroska's cluster scan."
  (let ((target (* seconds (st-timescale st))) (lo 0) (hi (1- (st-count st))) (found 0))
    (when (minusp hi) (return-from mp4-sync-sample-before 0))
    ;; find the last sample at or before the target
    (loop while (<= lo hi) do
      (let ((mid (floor (+ lo hi) 2)))
        (if (<= (aref (st-pts st) mid) target)
            (progn (setf found mid) (setf lo (1+ mid)))
            (setf hi (1- mid)))))
    ;; then walk back to a sync sample
    (loop for i from found downto 0
          when (= 1 (aref (st-sync st) i)) do (return i)
          finally (return 0))))

(defun seek-mp4 (m seconds)
  "A reader positioned at the sync sample at or before SECONDS on the video track (or, with no
   video, on the first track), with the other tracks lined up at the same time.  Returns
   (values reader landed-seconds)."
  (let* ((key-track (or (mp4-video-track m) (first (mp4-tracks m))))
         (kst (and key-track (gethash (track-number key-track) (mp4-tables m))))
         (i (if (and kst (plusp (st-count kst))) (mp4-sync-sample-before kst seconds) 0))
         (landed (if (and kst (plusp (st-count kst))) (st-time-seconds kst i) 0d0))
         (r (%make-mp4-reader :mp4 m :cursors '())))
    (setf (mr-cursors r)
          (mapcar (lambda (tr)
                    (let ((st (gethash (track-number tr) (mp4-tables m))))
                      (cons (track-number tr)
                            (if (eq tr key-track)
                                i
                                ;; every other track starts at its first sample whose decode time
                                ;; is at or after where the video landed, so nothing already heard
                                ;; is played twice
                                (let ((n (st-count st)) (want (* landed (st-timescale st))))
                                  (or (loop for j from 0 below n
                                            when (>= (aref (st-dts st) j) want) do (return j))
                                      n))))))
                  (mp4-tracks m)))
    (values r landed)))
