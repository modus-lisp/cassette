;;;; packages.lisp
(defpackage #:webm-pure
  (:use #:cl)
  (:import-from #:webp-pure
   #:u16le #:u24le
   #:bool-dec #:bool-init #:bool-bit #:bool-literal #:bool-signed #:bool-flag
   #:plane #:pl-data #:pl-stride #:pl-w #:pl-h #:make-plane* #:pidx #:pget #:pset #:pad-right
   #:treed-read #:get-coeffs #:predict-block #:predict-subblock #:clamp255 #:segment-dequant
   #:dec #:d-mb-cols #:d-mb-rows #:d-width #:d-height #:d-yplane #:d-uplane #:d-vplane
   #:d-coeff-probs #:d-above-y #:d-above-u #:d-above-v #:d-above-y2 #:d-left-y #:d-left-u
   #:d-left-v #:d-left-y2 #:d-seg-enabled #:d-seg-update-map #:d-seg-abs #:d-seg-tree-probs
   #:d-seg-quant #:d-seg-filter #:d-seg-dq #:d-mb-no-skip #:d-prob-skip #:d-filter-simple
   #:d-filter-level #:d-sharpness #:d-lf-delta-enabled #:d-ref-lf-delta #:d-mode-lf-delta
   #:d-mb-i4x4 #:d-mb-nonzero #:d-mb-seg #:d-ycoeffs #:d-ublocks #:d-vblocks #:d-y2coeffs
   #:d-bmodes #:d-above-bmode #:d-left-bmode #:d-pred16 #:d-pred8 #:d-subpred #:d-suba #:d-subl
   #:zero16 #:read-mb-modes #:decode-residue #:add-residual #:reconstruct-luma16
   #:reconstruct-bpred #:reconstruct-chroma #:scatter-y2 #:vp8-idct #:vp8-iwht
   #:kernel-simple #:kernel-sub #:kernel-mb #:filter-plane-edges
   #:+default-coeff-probs+ #:+coeff-update-probs+ #:+kf-ymode-tree+ #:+kf-ymode-prob+
   #:+uv-mode-tree+ #:+kf-uv-mode-prob+ #:+bmode-tree+ #:+mb-segment-tree+
   #:+dc-qlookup+ #:+ac-qlookup+)
  (:export
   ;; conditions
   #:webm-error #:webm-error-message
   ;; EBML primitives (exported for tooling / tests)
   #:read-vint #:read-element-header #:write-vint #:write-element #:ebml-element #:ebml-uint
   #:ebml-sint #:ebml-float #:ebml-string #:ebml-binary #:ebml-master
   ;; demuxer
   #:parse-webm #:webm #:webm-p #:webm-duration #:webm-timecode-scale #:webm-tracks
   #:webm-track #:webm-video-track #:webm-audio-track #:webm-doctype #:webm-title
   #:track #:track-p #:track-number #:track-uid #:track-type #:track-codec-id
   #:track-codec-private #:track-name #:track-language #:track-default-duration
   #:track-width #:track-height #:track-display-width #:track-display-height
   #:track-sample-rate #:track-channels #:track-bit-depth #:track-codec-delay
   #:track-seek-pre-roll
   #:block-frame #:block-frame-p #:frame-track #:frame-timecode #:frame-timestamp
   #:frame-data #:frame-keyframe-p #:frame-invisible-p #:frame-duration #:frame-discard-padding
   #:make-block-reader #:read-next-frame #:map-frames #:collect-frames #:cluster-index #:webm-cues
   ;; muxer
   #:make-muxer #:add-video-track #:add-audio-track #:add-frame #:finish-webm
   #:write-webm-file #:muxer
   ;; VP8 video decoder
   #:make-vp8-decoder #:vp8-decoder #:vp8-decode-frame #:vp8-frame-info
   #:vp8-decoder-width #:vp8-decoder-height #:vp8-decoder-frame-count
   #:picture #:picture-p #:picture-width #:picture-height #:picture-y #:picture-u #:picture-v
   #:picture-y-stride #:picture-uv-stride #:picture-y-offset #:picture-uv-offset
   #:picture-timestamp #:picture->rgb #:picture->rgb-into #:picture->yuv420
   ;; player
   #:open-webm #:webm-player #:next-video-frame #:next-audio-frame #:player-video-track
   #:player-audio-track #:player-webm #:player-duration #:player-frame-rate
   #:decode-all-audio #:write-ppm #:play-to-ffplay #:seek-webm #:player-eof-p))

(in-package #:webm-pure)

(define-condition webm-error (error)
  ((message :initarg :message :reader webm-error-message))
  (:report (lambda (c s) (format s "webm-pure: ~a" (webm-error-message c)))))

(defun %err (fmt &rest args)
  (error 'webm-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun slurp-file (path)
  "Read PATH into a fresh octet vector."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (octets (file-length s))))
      (read-sequence b s)
      b)))
