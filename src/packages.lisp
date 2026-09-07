;;;; packages.lisp
(defpackage #:cassette
  (:use #:cl)
  ;; The VP8 decoder used to live in this repo, importing sixty of webp-pure's internals to get
  ;; the intra half.  Both halves are reel's now — a video codec is not a container's business —
  ;; so what is imported here is a decoder's public face, and re-exported so a caller who has
  ;; opened a file does not need a second package to look at what came out of it.
  (:import-from #:reel
   #:make-decoder #:decode-frame #:frame-info #:decoder-width #:decoder-height
   #:picture #:picture-p #:picture-width #:picture-height #:picture-y #:picture-u #:picture-v
   #:picture-y-stride #:picture-uv-stride #:picture-y-offset #:picture-uv-offset
   #:picture-timestamp #:picture->rgb #:picture->rgb-into #:picture->yuv420)
  (:export
   ;; conditions
   #:cassette-error #:cassette-error-message
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
   ;; MP4 / ISO base media
   #:parse-mp4 #:mp4 #:mp4-p #:mp4-brand #:mp4-duration #:mp4-tracks #:mp4-track
   #:mp4-video-track #:mp4-audio-track #:mp4-fragmented #:mp4-bytes
   #:make-mp4-reader #:read-next-mp4-frame #:seek-mp4 #:mp4-sync-sample-before
   #:sample-table #:st-count #:st-timescale #:st-time-seconds #:st-sync
   ;; muxer
   #:make-muxer #:add-video-track #:add-audio-track #:add-frame #:finish-webm
   #:write-webm-file #:muxer
   ;; the decoder, reel's, re-exported
   #:make-decoder #:decode-frame #:frame-info #:decoder-width #:decoder-height
   #:picture #:picture-p #:picture-width #:picture-height #:picture-y #:picture-u #:picture-v
   #:picture-y-stride #:picture-uv-stride #:picture-y-offset #:picture-uv-offset
   #:picture-timestamp #:picture->rgb #:picture->rgb-into #:picture->yuv420
   ;; player
   #:open-media #:open-webm #:seek-media #:player-kind #:player-tick #:player-unsupported #:player-video-note #:webm-player #:next-video-frame #:next-audio-frame #:player-video-track
   #:player-audio-track #:player-webm #:player-duration #:player-frame-rate
   #:decode-all-audio #:write-ppm #:play-to-ffplay #:seek-webm #:player-eof-p))

(in-package #:cassette)

(define-condition cassette-error (error)
  ((message :initarg :message :reader cassette-error-message))
  (:report (lambda (c s) (format s "cassette: ~a" (cassette-error-message c)))))

(defun %err (fmt &rest args)
  (error 'cassette-error :message (apply #'format nil fmt args)))

(deftype octets () '(simple-array (unsigned-byte 8) (*)))

(defun octets (n) (make-array n :element-type '(unsigned-byte 8) :initial-element 0))

(defun slurp-file (path)
  "Read PATH into a fresh octet vector."
  (with-open-file (s path :element-type '(unsigned-byte 8))
    (let ((b (octets (file-length s))))
      (read-sequence b s)
      b)))
