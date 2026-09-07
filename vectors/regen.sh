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

# H.264 fixtures (all-intra Constrained Baseline, a range of quantisers and sizes):
#   ffmpeg -f lavfi -i "testsrc2=size=96x64:rate=5:duration=1"   -c:v libx264 -profile:v baseline -g 1 -qp 26 -bsf:v h264_mp4toannexb -f h264 intra.h264
#   ffmpeg -f lavfi -i "testsrc2=size=100x60:rate=5:duration=1"  -c:v libx264 -profile:v baseline -g 1 -qp 26 -bsf:v h264_mp4toannexb -f h264 crop.h264
#   ffmpeg -f lavfi -i "testsrc2=size=320x240:rate=5:duration=1" -c:v libx264 -profile:v baseline -g 1 -qp 18 -bsf:v h264_mp4toannexb -f h264 big.h264
#   ffmpeg -f lavfi -i "mandelbrot=size=176x144:rate=5,trim=duration=1" -c:v libx264 -profile:v baseline -g 1 -qp 34 -bsf:v h264_mp4toannexb -f h264 mandel.h264
#   ffmpeg -f lavfi -i "testsrc2=size=96x64:rate=5:duration=1"   -c:v libx264 -profile:v baseline -g 1 -qp 40 -bsf:v h264_mp4toannexb -f h264 coarse.h264
#   ffmpeg -f lavfi -i "testsrc2=size=128x96:rate=10:duration=2" -c:v libx264 -profile:v baseline -g 1 -qp 24 -pix_fmt yuv420p -frames:v 20 intra20.mp4
for f in intra crop big mandel coarse; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done
# the same, for the fixture that goes through the MP4 container rather than Annex B
ffmpeg -hide_banner -loglevel error -y -i intra20.mp4 -f rawvideo -pix_fmt yuv420p intra20.yuv

# P-slice fixtures (Baseline, CAVLC, no B frames).  Each isolates one thing that broke on its own
# while inter prediction was written, which is why they are separate files and not one clip:
#   pslice       synthetic motion, one reference, default partitions
#   pmandel      a coarse quantiser
#   t-ref3-16x16 three reference pictures, 16x16 partitions only
#   t-p4x4       8x4, 4x8 and 4x4 sub-partitions — the case that needs 6.4.11.7's not-yet-decoded rule
#   t-i4x4       intra macroblocks inside P slices
#   pbbb         real motion, three references, every partition size
#
#   X="bframes=0:cabac=0:weightp=0"
#   ffmpeg -f lavfi -i "testsrc2=size=96x64:rate=10:duration=1" -c:v libx264 -profile:v baseline \
#          -x264-params "$X:ref=1" -qp 26 -bsf:v h264_mp4toannexb -f h264 pslice.h264
#   ffmpeg -f lavfi -i "mandelbrot=size=176x144:rate=10,trim=duration=1" -pix_fmt yuv420p \
#          -c:v libx264 -profile:v baseline -x264-params "$X:ref=1" -qp 30 -bsf:v h264_mp4toannexb -f h264 pmandel.h264
#   ffmpeg -i BBB.webm -t 2 -pix_fmt yuv420p -c:v libx264 -profile:v baseline \
#          -x264-params "$X:ref=3:partitions=none" -qp 24 -bsf:v h264_mp4toannexb -f h264 t-ref3-16x16.h264
#   ...likewise t-p4x4 with ref=1:partitions=p8x8,p4x4 and t-i4x4 with ref=1:partitions=i4x4
#   ffmpeg -i BBB.webm -t 3 -pix_fmt yuv420p -c:v libx264 -profile:v baseline \
#          -x264-params "$X:ref=3:subme=7:partitions=all" -qp 24 -bsf:v h264_mp4toannexb -f h264 pbbb.h264
for f in pslice pmandel pbbb t-ref3-16x16 t-p4x4 t-i4x4; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done

# Main profile, so CABAC: what the decoder must refuse rather than decode wrong.  With audio, so
# it is also the standing test that a picture we cannot decode does not stop the sound.
#   ffmpeg -f lavfi -i "testsrc2=size=160x120:rate=15:duration=6" -f lavfi -i "sine=frequency=440:duration=6" \
#          -pix_fmt yuv420p -c:v libx264 -profile:v main -x264-params "cabac=1:bframes=0" -qp 26 \
#          -c:a aac -b:a 64k -shortest cabac-av.mp4
#   ffmpeg -f lavfi -i "testsrc2=size=96x64:rate=10:duration=1" -c:v libx264 -profile:v main -g 1 \
#          -x264-params "bframes=0:cabac=1" -qp 26 -bsf:v h264_mp4toannexb -f h264 cabac-intra.h264
