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

# The MP4 fixtures are committed (they are small and deterministic), but this is how they were
# made, so a new ffmpeg can reproduce them:
#   ffmpeg -f lavfi -i "testsrc2=size=160x120:rate=15:duration=6" -f lavfi -i "sine=frequency=440:duration=6" \
#          -c:v libx264 -profile:v baseline -g 15 -pix_fmt yuv420p -c:a aac -b:a 64k -shortest av.mp4
#   ffmpeg -f lavfi -i "sine=frequency=440:duration=8" -c:a aac -b:a 96k audio.m4a
#   ffmpeg -f lavfi -i "testsrc2=size=160x120:rate=15:duration=4" -c:v libx264 -g 15 -pix_fmt yuv420p \
#          -movflags +frag_keyframe+empty_moov frag.mp4
#   ffmpeg -f lavfi -i "testsrc2=size=160x120:rate=15:duration=4" -c:v libx264 -g 15 -pix_fmt yuv420p \
#          -movflags +faststart fast.mp4
