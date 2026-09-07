;;;; demux.lisp — Matroska/WebM demuxer.  Parses the EBML header, Segment
;;;; Info and Tracks eagerly, then hands out frames lazily from Clusters
;;;; (SimpleBlock and BlockGroup, all four lacing modes, header-stripping
;;;; ContentEncoding).  Unknown-size Segments and Clusters (live streams) work.
(in-package #:webm-pure)

;;; ---- element IDs ---------------------------------------------------------

(defconstant +id-ebml+ #x1A45DFA3)
(defconstant +id-doctype+ #x4282)
(defconstant +id-doctype-version+ #x4287)
(defconstant +id-doctype-read-version+ #x4285)
(defconstant +id-ebml-version+ #x4286)
(defconstant +id-ebml-read-version+ #x42F7)
(defconstant +id-ebml-max-id-length+ #x42F2)
(defconstant +id-ebml-max-size-length+ #x42F3)
(defconstant +id-segment+ #x18538067)
(defconstant +id-seekhead+ #x114D9B74)
(defconstant +id-seek+ #x4DBB)
(defconstant +id-seek-id+ #x53AB)
(defconstant +id-seek-position+ #x53AC)
(defconstant +id-info+ #x1549A966)
(defconstant +id-timecode-scale+ #x2AD7B1)
(defconstant +id-duration+ #x4489)
(defconstant +id-title+ #x7BA9)
(defconstant +id-muxing-app+ #x4D80)
(defconstant +id-writing-app+ #x5741)
(defconstant +id-date-utc+ #x4461)
(defconstant +id-tracks+ #x1654AE6B)
(defconstant +id-track-entry+ #xAE)
(defconstant +id-track-number+ #xD7)
(defconstant +id-track-uid+ #x73C5)
(defconstant +id-track-type+ #x83)
(defconstant +id-flag-enabled+ #xB9)
(defconstant +id-flag-default+ #x88)
(defconstant +id-flag-forced+ #x55AA)
(defconstant +id-flag-lacing+ #x9C)
(defconstant +id-default-duration+ #x23E383)
(defconstant +id-name+ #x536E)
(defconstant +id-language+ #x22B59C)
(defconstant +id-codec-id+ #x86)
(defconstant +id-codec-private+ #x63A2)
(defconstant +id-codec-name+ #x258688)
(defconstant +id-codec-delay+ #x56AA)
(defconstant +id-seek-pre-roll+ #x56BB)
(defconstant +id-video+ #xE0)
(defconstant +id-pixel-width+ #xB0)
(defconstant +id-pixel-height+ #xBA)
(defconstant +id-display-width+ #x54B0)
(defconstant +id-display-height+ #x54BA)
(defconstant +id-flag-interlaced+ #x9A)
(defconstant +id-audio+ #xE1)
(defconstant +id-sampling-frequency+ #xB5)
(defconstant +id-channels+ #x9F)
(defconstant +id-bit-depth+ #x6264)
(defconstant +id-content-encodings+ #x6D80)
(defconstant +id-content-encoding+ #x6240)
(defconstant +id-content-encoding-order+ #x5031)
(defconstant +id-content-encoding-scope+ #x5032)
(defconstant +id-content-encoding-type+ #x5033)
(defconstant +id-content-compression+ #x5034)
(defconstant +id-content-comp-algo+ #x4254)
(defconstant +id-content-comp-settings+ #x4255)
(defconstant +id-cluster+ #x1F43B675)
(defconstant +id-timecode+ #xE7)
(defconstant +id-simple-block+ #xA3)
(defconstant +id-block-group+ #xA0)
(defconstant +id-block+ #xA1)
(defconstant +id-block-duration+ #x9B)
(defconstant +id-reference-block+ #xFB)
(defconstant +id-discard-padding+ #x75A2)
(defconstant +id-block-additions+ #x75A1)
(defconstant +id-cues+ #x1C53BB6B)
(defconstant +id-cue-point+ #xBB)
(defconstant +id-cue-time+ #xB3)
(defconstant +id-cue-track-positions+ #xB7)
(defconstant +id-cue-track+ #xF7)
(defconstant +id-cue-cluster-position+ #xF1)
(defconstant +id-cue-block-number+ #x5378)
(defconstant +id-tags+ #x1254C367)
(defconstant +id-chapters+ #x1043A770)
(defconstant +id-attachments+ #x1941A469)
(defconstant +id-void+ #xEC)
(defconstant +id-crc32+ #xBF)

;;; ---- structures -----------------------------------------------------------

(defstruct (track (:conc-name track-))
  (number 0) (uid 0) (type 0)                     ; type: 1 video, 2 audio, 17 subtitle
  codec-id codec-private name language
  (default-duration nil)                          ; ns per frame, or NIL
  ;; video
  (width 0) (height 0) display-width display-height
  ;; audio
  (sample-rate nil) (channels 1) (bit-depth nil) (codec-delay 0) (seek-pre-roll 0)
  ;; header-stripping compression (ContentCompAlgo 3): octets prepended to each frame
  strip-prefix)

(defstruct (webm (:conc-name webm-) (:predicate nil))
  bytes
  doctype
  (timecode-scale 1000000)                        ; ns per tick
  duration                                        ; ticks (float) or NIL
  title muxing-app writing-app
  (tracks '())
  segment-start segment-end                       ; payload bounds of the Segment
  first-cluster                                   ; offset of the first Cluster element
  (cues '()))                                     ; list of (time-ticks . cluster-offset)

(defun webm-video-track (w)
  (find 1 (webm-tracks w) :key #'track-type))
(defun webm-audio-track (w)
  (find 2 (webm-tracks w) :key #'track-type))
(defun webm-track (w number)
  (find number (webm-tracks w) :key #'track-number))

(defstruct (block-frame (:conc-name frame-))
  track                                           ; TRACK struct
  (timecode 0)                                    ; absolute, in TimecodeScale ticks
  data                                            ; octets (one frame; laces are split)
  keyframe-p invisible-p discardable-p
  duration                                        ; ticks or NIL (BlockGroup only)
  (discard-padding 0))                            ; ns (Opus end trimming)

(defun frame-timestamp (frame scale)
  "Presentation time in seconds as a double, given the segment TimecodeScale."
  (/ (* (frame-timecode frame) scale) 1d9))

;;; ---- child iteration -------------------------------------------------------

(defmacro do-children ((id start size buf pos end) &body body)
  "Iterate over the elements in BUF[POS..END), binding ID, payload START and SIZE.
   Elements of unknown size are treated as extending to END."
  (let ((p (gensym "P")) (e (gensym "E")))
    `(let ((,p ,pos) (,e ,end))
       (loop while (< ,p ,e) do
         (multiple-value-bind (,id ,start ,size) (read-element-header ,buf ,p :end ,e)
           (let ((,size (or ,size (- ,e ,start))))
             (when (> (+ ,start ,size) ,e) (%err "EBML: element ~x overruns its parent" ,id))
             ,@body
             (setf ,p (+ ,start ,size))))))))

;;; ---- top level -------------------------------------------------------------

(defun webm-p (bytes)
  "True when BYTES begin with an EBML header."
  (and (>= (length bytes) 4)
       (= (aref bytes 0) #x1A) (= (aref bytes 1) #x45)
       (= (aref bytes 2) #xDF) (= (aref bytes 3) #xA3)))

(defun parse-webm (bytes)
  "Parse the EBML header, Segment Info, Tracks and (if present) Cues of the
   WebM/Matroska file in BYTES.  Clusters are located but not read; use
   MAKE-BLOCK-READER / MAP-FRAMES for frames."
  (declare (type octets bytes))
  (unless (webm-p bytes) (%err "not an EBML/WebM file"))
  (let ((w (make-webm :bytes bytes)) (pos 0) (end (length bytes)))
    ;; EBML header
    (multiple-value-bind (id start size) (read-element-header bytes pos :end end)
      (unless (= id +id-ebml+) (%err "no EBML header"))
      (let ((size (or size (%err "EBML header of unknown size"))))
        (do-children (cid cstart csize bytes start (+ start size))
          (when (= cid +id-doctype+) (setf (webm-doctype w) (ebml-string bytes cstart csize))))
        (setf pos (+ start size))))
    ;; find the Segment
    (loop
      (when (>= pos end) (%err "no Segment element"))
      (multiple-value-bind (id start size) (read-element-header bytes pos :end end)
        (cond ((= id +id-segment+)
               (setf (webm-segment-start w) start
                     (webm-segment-end w) (if size (min end (+ start size)) end))
               (return))
              (t (setf pos (+ start (or size 0)))))))
    ;; walk the Segment's top-level children until the first Cluster
    (let ((p (webm-segment-start w)) (e (webm-segment-end w)))
      (loop while (< p e) do
        (multiple-value-bind (id start size) (read-element-header bytes p :end e)
          (cond
            ((= id +id-cluster+)
             (setf (webm-first-cluster w) p)
             (return))
            ((= id +id-info+) (parse-info w bytes start (+ start (or size 0))))
            ((= id +id-tracks+) (parse-tracks w bytes start (+ start (or size 0))))
            ((= id +id-cues+) (parse-cues w bytes start (+ start (or size 0)))))
          (unless size (%err "EBML: unknown-size element ~x before the first Cluster" id))
          (setf p (+ start size)))))
    ;; Cues often live after the clusters; if a SeekHead told us where, read them
    (unless (webm-cues w)
      (let ((cues-pos (find-seek-target w +id-cues+)))
        (when (and cues-pos (< cues-pos (webm-segment-end w)))
          (multiple-value-bind (id start size) (read-element-header bytes cues-pos :end end)
            (when (and (= id +id-cues+) size)
              (parse-cues w bytes start (+ start size)))))))
    (setf (webm-tracks w) (sort (webm-tracks w) #'< :key #'track-number))
    w))

(defun find-seek-target (w target-id)
  "Absolute offset of TARGET-ID from the first SeekHead, or NIL."
  (let ((bytes (webm-bytes w)) (p (webm-segment-start w)) (e (webm-segment-end w)))
    (loop while (< p e) do
      (multiple-value-bind (id start size) (read-element-header bytes p :end e)
        (unless size (return nil))
        (when (= id +id-cluster+) (return nil))
        (when (= id +id-seekhead+)
          (do-children (cid cstart csize bytes start (+ start size))
            (when (= cid +id-seek+)
              (let (sid spos)
                (do-children (eid estart esize bytes cstart (+ cstart csize))
                  (cond ((= eid +id-seek-id+) (setf sid (ebml-uint bytes estart esize)))
                        ((= eid +id-seek-position+) (setf spos (ebml-uint bytes estart esize)))))
                (when (and sid spos (= sid target-id))
                  (return-from find-seek-target (+ (webm-segment-start w) spos)))))))
        (setf p (+ start size))))
    nil))

(defun parse-info (w bytes start end)
  (do-children (id s n bytes start end)
    (cond ((= id +id-timecode-scale+) (setf (webm-timecode-scale w) (ebml-uint bytes s n)))
          ((= id +id-duration+) (setf (webm-duration w) (ebml-float bytes s n)))
          ((= id +id-title+) (setf (webm-title w) (ebml-string bytes s n)))
          ((= id +id-muxing-app+) (setf (webm-muxing-app w) (ebml-string bytes s n)))
          ((= id +id-writing-app+) (setf (webm-writing-app w) (ebml-string bytes s n))))))

(defun parse-tracks (w bytes start end)
  (do-children (id s n bytes start end)
    (when (= id +id-track-entry+)
      (push (parse-track-entry bytes s (+ s n)) (webm-tracks w)))))

(defun parse-track-entry (bytes start end)
  (let ((tr (make-track)))
    (do-children (id s n bytes start end)
      (cond
        ((= id +id-track-number+) (setf (track-number tr) (ebml-uint bytes s n)))
        ((= id +id-track-uid+) (setf (track-uid tr) (ebml-uint bytes s n)))
        ((= id +id-track-type+) (setf (track-type tr) (ebml-uint bytes s n)))
        ((= id +id-codec-id+) (setf (track-codec-id tr) (ebml-string bytes s n)))
        ((= id +id-codec-private+) (setf (track-codec-private tr) (ebml-binary bytes s n)))
        ((= id +id-name+) (setf (track-name tr) (ebml-string bytes s n)))
        ((= id +id-language+) (setf (track-language tr) (ebml-string bytes s n)))
        ((= id +id-default-duration+) (setf (track-default-duration tr) (ebml-uint bytes s n)))
        ((= id +id-codec-delay+) (setf (track-codec-delay tr) (ebml-uint bytes s n)))
        ((= id +id-seek-pre-roll+) (setf (track-seek-pre-roll tr) (ebml-uint bytes s n)))
        ((= id +id-video+)
         (do-children (vid vs vn bytes s (+ s n))
           (cond ((= vid +id-pixel-width+) (setf (track-width tr) (ebml-uint bytes vs vn)))
                 ((= vid +id-pixel-height+) (setf (track-height tr) (ebml-uint bytes vs vn)))
                 ((= vid +id-display-width+) (setf (track-display-width tr) (ebml-uint bytes vs vn)))
                 ((= vid +id-display-height+) (setf (track-display-height tr) (ebml-uint bytes vs vn))))))
        ((= id +id-audio+)
         (do-children (aid as an bytes s (+ s n))
           (cond ((= aid +id-sampling-frequency+) (setf (track-sample-rate tr) (ebml-float bytes as an)))
                 ((= aid +id-channels+) (setf (track-channels tr) (ebml-uint bytes as an)))
                 ((= aid +id-bit-depth+) (setf (track-bit-depth tr) (ebml-uint bytes as an))))))
        ((= id +id-content-encodings+)
         (do-children (eid es en bytes s (+ s n))
           (when (= eid +id-content-encoding+)
             (let ((type 0) (algo nil) (settings nil) (scope 1))
               (do-children (cid cs cn bytes es (+ es en))
                 (cond ((= cid +id-content-encoding-type+) (setf type (ebml-uint bytes cs cn)))
                       ((= cid +id-content-encoding-scope+) (setf scope (ebml-uint bytes cs cn)))
                       ((= cid +id-content-compression+)
                        (do-children (kid ks kn bytes cs (+ cs cn))
                          (cond ((= kid +id-content-comp-algo+) (setf algo (ebml-uint bytes ks kn)))
                                ((= kid +id-content-comp-settings+) (setf settings (ebml-binary bytes ks kn))))))))
               (cond ((/= type 0) (%err "encrypted track ~d is not supported" (track-number tr)))
                     ((null algo))
                     ((= algo 3) (when (logtest scope 1) (setf (track-strip-prefix tr) (or settings (octets 0)))))
                     (t (%err "track ~d uses ContentCompAlgo ~d (only header stripping is supported)"
                              (track-number tr) algo)))))))))
    (when (and (track-sample-rate tr) (null (track-default-duration tr)) nil))
    tr))

(defun cluster-index (w)
  "(time-ticks . cluster-offset) for every Cluster, from the file's Cues when it has them and
   otherwise by walking the cluster headers — one element-header read per cluster, no blocks
   touched.  Cached on W, so a file without Cues pays once."
  (or (webm-cues w)
      (let ((bytes (webm-bytes w)) (p (webm-first-cluster w)) (e (webm-segment-end w)) (acc '()))
        (loop while (and p (< p e)) do
          (multiple-value-bind (id start size) (read-element-header bytes p :end e)
            (cond
              ((= id +id-cluster+)
               ;; the Timecode is the first child by convention; look a few children in anyway
               (let ((q start) (tc nil) (stop (if size (+ start size) e)))
                 (loop repeat 4 while (and (null tc) (< q stop)) do
                   (multiple-value-bind (cid cstart csize) (read-element-header bytes q :end stop)
                     (when (= cid +id-timecode+) (setf tc (ebml-uint bytes cstart csize)))
                     (setf q (+ cstart (or csize 0)))))
                 (when tc (push (cons tc p) acc))
                 ;; an unknown-size cluster runs to the next cluster: scan for it
                 (setf p (if size (+ start size) (%next-cluster-offset bytes start e)))))
              (t (setf p (+ start (or size 0)))))))
        (setf (webm-cues w) (nreverse acc)))))

(defun %next-cluster-offset (bytes pos end)
  "The offset of the next Cluster element header at or after POS, walking cluster children."
  (loop while (< pos end) do
    (multiple-value-bind (id start size) (read-element-header bytes pos :end end)
      (when (= id +id-cluster+) (return-from %next-cluster-offset pos))
      (setf pos (+ start (or size 0)))))
  end)

(defun parse-cues (w bytes start end)
  (let ((cues '()))
    (do-children (id s n bytes start end)
      (when (= id +id-cue-point+)
        (let (time cpos)
          (do-children (cid cs cn bytes s (+ s n))
            (cond ((= cid +id-cue-time+) (setf time (ebml-uint bytes cs cn)))
                  ((= cid +id-cue-track-positions+)
                   (do-children (pid ps pn bytes cs (+ cs cn))
                     (when (= pid +id-cue-cluster-position+)
                       (setf cpos (ebml-uint bytes ps pn)))))))
          (when (and time cpos)
            (push (cons time (+ (webm-segment-start w) cpos)) cues)))))
    (setf (webm-cues w) (nreverse cues))))

;;; ---- block reader --------------------------------------------------------

(defstruct (block-reader (:conc-name br-) (:constructor %make-block-reader))
  webm
  pos                                           ; next unread top-level position in the Segment
  cluster-end                                   ; end of the current cluster payload (or NIL)
  (cluster-timecode 0)
  (pending '()))                                ; frames already split out of a laced block

(defun make-block-reader (w &key (start (webm-first-cluster w)))
  "A cursor over the frames of W, beginning at the Cluster at START."
  (%make-block-reader :webm w :pos (or start (webm-segment-end w)) :cluster-end nil))

(defun read-next-frame (br)
  "Return the next BLOCK-FRAME in file order, or NIL at end of stream."
  (loop
    (when (br-pending br) (return (pop (br-pending br))))
    (let* ((w (br-webm br)) (bytes (webm-bytes w)) (end (webm-segment-end w)))
      ;; inside a cluster?
      (cond
        ((and (br-cluster-end br) (< (br-pos br) (br-cluster-end br)))
         (multiple-value-bind (id start size) (read-element-header bytes (br-pos br) :end end)
           (cond
             ;; an unknown-size cluster ends where the next cluster begins
             ((= id +id-cluster+) (setf (br-cluster-end br) nil))
             (t
              (let ((size (or size (- (br-cluster-end br) start))))
                (cond
                  ((= id +id-timecode+) (setf (br-cluster-timecode br) (ebml-uint bytes start size)))
                  ((= id +id-simple-block+)
                   (setf (br-pending br) (parse-block w bytes start (+ start size) (br-cluster-timecode br) t nil 0)))
                  ((= id +id-block-group+)
                   (setf (br-pending br) (parse-block-group w bytes start (+ start size) (br-cluster-timecode br)))))
                (setf (br-pos br) (+ start size)))))))
        ((>= (br-pos br) end) (return nil))
        (t
         (multiple-value-bind (id start size) (read-element-header bytes (br-pos br) :end end)
           (cond
             ((= id +id-cluster+)
              (setf (br-cluster-end br) (if size (min end (+ start size)) end)
                    (br-cluster-timecode br) 0
                    (br-pos br) start))
             (t (setf (br-pos br) (+ start (or size 0)))))))))))

(defun parse-block-group (w bytes start end cluster-tc)
  (let (frames (duration nil) (padding 0) (has-ref nil))
    ;; scan for metadata first so the block can be labelled
    (do-children (id s n bytes start end)
      (cond ((= id +id-block-duration+) (setf duration (ebml-uint bytes s n)))
            ((= id +id-reference-block+) (setf has-ref t))
            ((= id +id-discard-padding+) (setf padding (ebml-sint bytes s n)))))
    (do-children (id s n bytes start end)
      (when (= id +id-block+)
        (setf frames (parse-block w bytes s (+ s n) cluster-tc nil (not has-ref) padding))))
    (when duration (dolist (f frames) (setf (frame-duration f) duration)))
    frames))

(defun parse-block (w bytes start end cluster-tc simple-p group-keyframe-p padding)
  "Split a (Simple)Block payload into BLOCK-FRAMEs, one per lace."
  (multiple-value-bind (tnum p) (read-vint bytes start :end end)
    (when (> (+ p 3) end) (%err "truncated block header"))
    (let* ((tc (ebml-sint bytes p 2))
           (flags (aref bytes (+ p 2)))
           (track (or (webm-track w tnum) (%err "block for unknown track ~d" tnum)))
           (keyframe (if simple-p (logbitp 7 flags) group-keyframe-p))
           (invisible (logbitp 3 flags))
           (discardable (logbitp 0 flags))
           (lacing (ldb (byte 2 1) flags))
           (data-start (+ p 3))
           (laces (split-laces bytes data-start end lacing))
           (prefix (track-strip-prefix track))
           (abs-tc (+ cluster-tc tc))
           (per-lace (or (and (track-default-duration track) (> (length laces) 1)
                              (round (track-default-duration track) (webm-timecode-scale w)))
                         0)))
      (loop for (s . e) in laces
            for i from 0
            collect (make-block-frame
                     :track track
                     :timecode (+ abs-tc (* i per-lace))
                     :data (if prefix
                               (concat-octets prefix (subseq bytes s e))
                               (subseq bytes s e))
                     :keyframe-p keyframe :invisible-p invisible :discardable-p discardable
                     :discard-padding padding)))))

(defun split-laces (bytes start end lacing)
  "Return a list of (start . end) pairs for the frames in a block payload."
  (ecase lacing
    (0 (list (cons start end)))
    (1                                          ; Xiph lacing
     (let* ((count (1+ (aref bytes start))) (p (1+ start)) (sizes '()))
       (dotimes (i (1- count))
         (let ((sz 0))
           (loop (let ((b (aref bytes p))) (incf p) (incf sz b) (when (< b 255) (return))))
           (push sz sizes)))
       (lace-ranges (nreverse sizes) count p end)))
    (2                                          ; fixed-size lacing
     (let* ((count (1+ (aref bytes start))) (p (1+ start))
            (total (- end p)) (each (floor total count)))
       (unless (= (* each count) total) (%err "fixed lacing does not divide evenly"))
       (loop for i below count collect (cons (+ p (* i each)) (+ p (* (1+ i) each))))))
    (3                                          ; EBML lacing
     (let* ((count (1+ (aref bytes start))) (p (1+ start)) (sizes '()))
       (multiple-value-bind (first np) (read-vint bytes p :end end)
         (setf p np) (push first sizes)
         (let ((prev first))
           (dotimes (i (- count 2))
             (multiple-value-bind (raw np len) (read-vint bytes p :end end)
               (declare (ignore len))
               ;; signed: subtract the bias 2^(7*len-1) - 1
               (let* ((nbits (* 7 (- np p))) (delta (- raw (1- (ash 1 (1- nbits))))))
                 (setf p np prev (+ prev delta))
                 (push prev sizes))))))
       (lace-ranges (nreverse sizes) count p end)))))

(defun lace-ranges (sizes count p end)
  "Turn COUNT-1 explicit SIZES plus an implicit last lace into ranges from P."
  (let ((ranges '()))
    (dolist (sz sizes)
      (push (cons p (+ p sz)) ranges)
      (incf p sz))
    (when (> p end) (%err "laces overrun the block"))
    (push (cons p end) ranges)
    (unless (= (length ranges) count) (%err "lace count mismatch"))
    (nreverse ranges)))

(defun map-frames (fn w &key track)
  "Call FN on every frame of W in file order (optionally only those of TRACK,
   a track number or TRACK struct)."
  (let ((br (make-block-reader w))
        (tn (if (track-p track) (track-number track) track)))
    (loop for f = (read-next-frame br)
          while f
          do (when (or (null tn) (= tn (track-number (frame-track f))))
               (funcall fn f)))))

(defun collect-frames (w &key track)
  "All frames of W (or of TRACK) as a list."
  (let ((acc '()))
    (map-frames (lambda (f) (push f acc)) w :track track)
    (nreverse acc)))
