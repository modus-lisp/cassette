#!/bin/sh
# Regenerate the ffmpeg reference decodes the tests compare against (vectors/*.yuv).
# They are not committed: each is tens of MB and ffmpeg makes them in seconds.
#   sh vectors/regen.sh
set -e
cd "$(dirname "$0")"
for f in t1-basic t2-altref t3-mandel t4-testsrc t5-av wpt-test wpt-circles; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.webm" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done
# only the first 3 s of Big Buck Bunny: 90 frames is what test-decode.lisp compares
ffmpeg -hide_banner -loglevel error -y -i bbb360.webm -t 3 -f rawvideo -pix_fmt yuv420p bbb360.yuv
ls -la ./*.yuv
