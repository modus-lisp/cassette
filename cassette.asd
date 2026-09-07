;;;; cassette.asd — media containers in pure Common Lisp.
;;;;
;;;; A cassette is a shell that holds several tracks wound together and hands them back in step.
;;;; That is what a container format is, and it is all this is: the demuxers, the muxer, and a
;;;; player that paces what comes out.  The codecs are elsewhere and are dependencies — reel
;;;; decodes the video, reed the audio — because which codec a track happens to carry is not
;;;; something the shell around it knows or should.
(asdf:defsystem :cassette
  :description "Pure-CL media containers: an EBML/Matroska (WebM) reader and writer, a
pull-model player that hands out decoded pictures and audio in step, and seeking that lands
on the frame asked for.  Video decoding is reel's, audio is reed's.  No FFI."
  :license "MIT"
  :depends-on (:reel :reed)
  :components ((:module "src"
                :serial t
                :components ((:file "packages")
                             (:file "ebml")
                             (:file "demux")
                             (:file "mp4")
                             (:file "mux")
                             (:file "player")))))
