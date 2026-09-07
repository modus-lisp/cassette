;;;; mux.lisp — WebM muxer.  Collects frames per track and writes a seekable
;;;; file: EBML header, Segment { SeekHead, Info, Tracks, Cues, Cluster* }.
;;;; Cues precede the clusters and use fixed-width integers, so cluster
;;;; offsets are known before anything is emitted.
(in-package #:webm-pure)

(defstruct (mux-track (:conc-name mt-))
  number type codec-id codec-private
  width height display-width display-height
  sample-rate channels bit-depth (codec-delay 0) (seek-pre-roll 0)
  default-duration name language)

(defstruct (muxer (:conc-name mx-))
  (tracks '())
  (frames '())                                  ; list of (track-number timecode-ns data keyframe-p)
  (timecode-scale 1000000)                      ; 1 ms ticks
  (cluster-limit-ns 5000000000)                 ; start a new cluster at least this often
  (title nil)
  (writing-app "webm-pure"))

(defun add-video-track (mx &key (codec-id "V_VP8") width height display-width display-height
                                 frame-rate codec-private name (language "und"))
  "Add a video track; returns its track number."
  (let ((n (1+ (length (mx-tracks mx)))))
    (push (make-mux-track :number n :type 1 :codec-id codec-id :codec-private codec-private
                          :width width :height height
                          :display-width display-width :display-height display-height
                          :default-duration (and frame-rate (round 1000000000 frame-rate))
                          :name name :language language)
          (mx-tracks mx))
    n))

(defun add-audio-track (mx &key (codec-id "A_OPUS") sample-rate (channels 2) bit-depth
                                 codec-private (codec-delay 0) (seek-pre-roll 0)
                                 name (language "und"))
  "Add an audio track; returns its track number.  For Opus pass the OpusHead
   as CODEC-PRIVATE, CODEC-DELAY (pre-skip in ns) and SEEK-PRE-ROLL (80 ms)."
  (let ((n (1+ (length (mx-tracks mx)))))
    (push (make-mux-track :number n :type 2 :codec-id codec-id :codec-private codec-private
                          :sample-rate sample-rate :channels channels :bit-depth bit-depth
                          :codec-delay codec-delay :seek-pre-roll seek-pre-roll
                          :name name :language language)
          (mx-tracks mx))
    n))

(defun add-frame (mx track-number timestamp-ns data &key keyframe (discard-padding 0))
  "Queue one frame (octets) for TRACK-NUMBER at TIMESTAMP-NS.  A non-zero
   DISCARD-PADDING (ns of decoded audio to drop at the end, Opus) makes the
   frame a BlockGroup instead of a SimpleBlock."
  (push (list track-number timestamp-ns data keyframe discard-padding) (mx-frames mx))
  mx)

;;; ---- element builders ---------------------------------------------------------

(defun el-uint (id v &key min-length) (ebml-element id (uint-octets v :min-length min-length)))
(defun el-float (id v) (ebml-element id (float-octets v)))
(defun el-string (id s) (ebml-element id (string-octets s)))

(defun ebml-header-element ()
  (ebml-master +id-ebml+
               (el-uint +id-ebml-version+ 1)
               (el-uint +id-ebml-read-version+ 1)
               (el-uint +id-ebml-max-id-length+ 4)
               (el-uint +id-ebml-max-size-length+ 8)
               (el-string +id-doctype+ "webm")
               (el-uint +id-doctype-version+ 4)
               (el-uint +id-doctype-read-version+ 2)))

(defun info-element (mx duration-ticks)
  (ebml-master +id-info+
               (el-uint +id-timecode-scale+ (mx-timecode-scale mx))
               (and duration-ticks (el-float +id-duration+ duration-ticks))
               (and (mx-title mx) (el-string +id-title+ (mx-title mx)))
               (el-string +id-muxing-app+ "webm-pure")
               (el-string +id-writing-app+ (mx-writing-app mx))))

(defun track-entry-element (tr)
  (ebml-master +id-track-entry+
               (el-uint +id-track-number+ (mt-number tr))
               (el-uint +id-track-uid+ (mt-number tr))
               (el-uint +id-track-type+ (mt-type tr))
               (el-uint +id-flag-lacing+ 0)
               (and (mt-language tr) (el-string +id-language+ (mt-language tr)))
               (el-string +id-codec-id+ (mt-codec-id tr))
               (and (mt-codec-private tr) (ebml-element +id-codec-private+ (mt-codec-private tr)))
               (and (mt-name tr) (el-string +id-name+ (mt-name tr)))
               (and (mt-default-duration tr) (el-uint +id-default-duration+ (mt-default-duration tr)))
               (and (plusp (mt-codec-delay tr)) (el-uint +id-codec-delay+ (mt-codec-delay tr)))
               (and (plusp (mt-seek-pre-roll tr)) (el-uint +id-seek-pre-roll+ (mt-seek-pre-roll tr)))
               (ecase (mt-type tr)
                 (1 (ebml-master +id-video+
                                 (el-uint +id-pixel-width+ (mt-width tr))
                                 (el-uint +id-pixel-height+ (mt-height tr))
                                 (and (mt-display-width tr) (el-uint +id-display-width+ (mt-display-width tr)))
                                 (and (mt-display-height tr) (el-uint +id-display-height+ (mt-display-height tr)))))
                 (2 (ebml-master +id-audio+
                                 (el-float +id-sampling-frequency+ (mt-sample-rate tr))
                                 (el-uint +id-channels+ (mt-channels tr))
                                 (and (mt-bit-depth tr) (el-uint +id-bit-depth+ (mt-bit-depth tr))))))))

(defun tracks-element (mx)
  (apply #'ebml-master +id-tracks+
         (mapcar #'track-entry-element (sort (copy-list (mx-tracks mx)) #'< :key #'mt-number))))

(defun simple-block-element (track-number rel-tc data keyframe)
  (let ((out (make-out (+ 16 (length data)))))
    (write-vint track-number out)
    (vector-push-extend (ldb (byte 8 8) rel-tc) out)
    (vector-push-extend (ldb (byte 8 0) rel-tc) out)
    (vector-push-extend (if keyframe #x80 0) out)
    (loop for b across data do (vector-push-extend b out))
    (ebml-element +id-simple-block+ (out->octets out))))

(defun block-group-element (track-number rel-tc data keyframe discard-padding)
  (let ((out (make-out (+ 16 (length data)))))
    (write-vint track-number out)
    (vector-push-extend (ldb (byte 8 8) rel-tc) out)
    (vector-push-extend (ldb (byte 8 0) rel-tc) out)
    (vector-push-extend 0 out)
    (loop for b across data do (vector-push-extend b out))
    (ebml-master +id-block-group+
                 (ebml-element +id-block+ (out->octets out))
                 ;; a non-key frame references the previous one of its track
                 (and (not keyframe) (ebml-element +id-reference-block+ (sint-octets -1)))
                 (and (/= discard-padding 0)
                      (ebml-element +id-discard-padding+ (sint-octets discard-padding))))))

(defun cue-point-element (time-ticks track-number cluster-offset)
  "A CuePoint with fixed-width payload integers so every CuePoint has the same size."
  (ebml-master +id-cue-point+
               (el-uint +id-cue-time+ time-ticks :min-length 8)
               (ebml-master +id-cue-track-positions+
                            (el-uint +id-cue-track+ track-number :min-length 1)
                            (el-uint +id-cue-cluster-position+ cluster-offset :min-length 8))))

(defun seek-entry (id position)
  (ebml-master +id-seek+
               (ebml-element +id-seek-id+ (uint-octets id))
               (el-uint +id-seek-position+ position :min-length 8)))

;;; ---- assembly -------------------------------------------------------------------

(defun group-into-clusters (mx frames scale)
  "Split time-ordered FRAMES (ticks) into clusters: a new one at each video
   keyframe once the cluster is older than the limit, or when the 16-bit
   relative timecode would overflow.  Returns a list of (cluster-tc . frames)."
  (let* ((video (find 1 (mx-tracks mx) :key #'mt-type))
         (vnum (and video (mt-number video)))
         (limit (floor (mx-cluster-limit-ns mx) scale))
         (clusters '()) (cur '()) (cur-tc nil))
    (dolist (f frames)
      (destructuring-bind (tn tc data key pad) f
        (declare (ignore data pad))
        (let ((start-new (or (null cur-tc)
                             (> (- tc cur-tc) 32767)
                             (and (>= (- tc cur-tc) limit)
                                  (or (null vnum) (and (= tn vnum) key))))))
          (when start-new
            (when cur (push (cons cur-tc (nreverse cur)) clusters))
            (setf cur '() cur-tc (max 0 tc)))
          (push f cur))))
    (when cur (push (cons cur-tc (nreverse cur)) clusters))
    (nreverse clusters)))

(defun finish-webm (mx)
  "Assemble the queued frames into a complete WebM file (octets)."
  (let* ((scale (mx-timecode-scale mx))
         (frames (stable-sort (mapcar (lambda (f) (destructuring-bind (tn ns data key pad) f
                                                    (list tn (round ns scale) data key pad)))
                                      (reverse (mx-frames mx)))
                              #'< :key #'second))
         (last-tc (if frames (second (car (last frames))) 0))
         (video (find 1 (mx-tracks mx) :key #'mt-type))
         (duration (let ((extra (if (and video (mt-default-duration video))
                                    (/ (mt-default-duration video) scale) 0)))
                     (float (+ last-tc extra) 1d0)))
         (clusters (group-into-clusters mx frames scale))
         (cluster-octets (mapcar (lambda (c)
                                   (destructuring-bind (ctc . fs) c
                                     (apply #'ebml-master +id-cluster+
                                            (el-uint +id-timecode+ ctc)
                                            (mapcar (lambda (f)
                                                      (destructuring-bind (tn tc data key pad) f
                                                        (if (zerop pad)
                                                            (simple-block-element tn (- tc ctc) data key)
                                                            (block-group-element tn (- tc ctc) data key pad))))
                                                    fs))))
                                 clusters))
         (info (info-element mx duration))
         (tracks (tracks-element mx))
         (cue-track (if video (mt-number video) (mt-number (first (mx-tracks mx)))))
         ;; SeekHead has three fixed-size entries; compute its size with dummy positions
         (seekhead-size (length (ebml-master +id-seekhead+
                                             (seek-entry +id-info+ 0)
                                             (seek-entry +id-tracks+ 0)
                                             (seek-entry +id-cues+ 0))))
         (cues-size (length (apply #'ebml-master +id-cues+
                                   (mapcar (lambda (c) (cue-point-element (car c) cue-track 0)) clusters))))
         ;; positions are relative to the start of the Segment payload
         (info-pos seekhead-size)
         (tracks-pos (+ info-pos (length info)))
         (cues-pos (+ tracks-pos (length tracks)))
         (first-cluster-pos (+ cues-pos cues-size))
         (cue-points (let ((pos first-cluster-pos) (acc '()))
                       (loop for c in clusters for co in cluster-octets do
                         (push (cue-point-element (car c) cue-track pos) acc)
                         (incf pos (length co)))
                       (nreverse acc)))
         (seekhead (ebml-master +id-seekhead+
                                (seek-entry +id-info+ info-pos)
                                (seek-entry +id-tracks+ tracks-pos)
                                (seek-entry +id-cues+ cues-pos)))
         (cues (apply #'ebml-master +id-cues+ cue-points))
         (segment-payload (apply #'concat-octets seekhead info tracks cues cluster-octets)))
    (assert (= (length seekhead) seekhead-size))
    (assert (= (length cues) cues-size))
    (concat-octets (ebml-header-element)
                   (ebml-element +id-segment+ segment-payload :size-length 8))))

(defun write-webm-file (mx path)
  "FINISH-WEBM to PATH.  Returns the number of octets written."
  (let ((bytes (finish-webm mx)))
    (with-open-file (s path :direction :output :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence bytes s))
    (length bytes)))
