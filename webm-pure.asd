;;;; webm-pure.asd — a pure Common Lisp WebM (Matroska) demuxer, muxer and
;;;; VP8 video decoder.
(asdf:defsystem :webm-pure
  :description "Pure-CL WebM: EBML/Matroska reader and writer, a full VP8
decoder (key and inter frames, RFC 6386) built on webp-pure's intra pipeline,
and Opus audio through reed.  No FFI."
  :license "MIT"
  :depends-on (:webp-pure :reed)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "ebml")
                             (:file "demux")
                             (:file "mux")
                             (:file "vp8-tables")
                             (:file "vp8-decoder")
                             (:file "player")))))
