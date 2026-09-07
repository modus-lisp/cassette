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

# CABAC fixtures (Main profile).  Intra ones span the quantiser range because the coefficient
# contexts are where CABAC's detail lives; the P ones exist because multiple references and
# sub-8x8 partitions each worked alone and broke together — a partition's reference index takes
# its context from the partition beside it, whose index is read earlier in the same macroblock.
#   M="bframes=0:cabac=1"
#   ffmpeg -f lavfi -i "testsrc2=size=96x64:rate=10:duration=1"   -pix_fmt yuv420p -c:v libx264 -profile:v main -g 1 -x264-params "$M" -qp 26 -bsf:v h264_mp4toannexb -f h264 cabac-intra.h264
#   ...cabac-fine qp 12, cabac-big 320x240 qp 18, cabac-mandel mandelbrot 176x144 qp 34, all with -g 1
#   ffmpeg -f lavfi -i "testsrc2=size=96x64:rate=10:duration=1"   -pix_fmt yuv420p -c:v libx264 -profile:v main -x264-params "$M:ref=1:weightp=0" -qp 26 -bsf:v h264_mp4toannexb -f h264 cabac-p.h264
#   ffmpeg -i BBB.webm -t 1 -s 176x144 -pix_fmt yuv420p -c:v libx264 -profile:v main -x264-params "$M:weightp=0:ref=3:partitions=none" -qp 24 -bsf:v h264_mp4toannexb -f h264 c-m3ref.h264
#   ...c-m1ref with ref=1, c-mall with ref=3:partitions=all:subme=7
#   ffmpeg -i BBB.webm -t 2 -pix_fmt yuv420p -c:v libx264 -profile:v main -x264-params "$M:ref=3:weightp=0:subme=7:partitions=all" -qp 24 -bsf:v h264_mp4toannexb -f h264 cabac-pbbb.h264
for f in cabac-intra cabac-fine cabac-big cabac-mandel cabac-p cabac-pbbb c-m1ref c-m3ref c-mall c-3ref c-p4x4 c-i4x4; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done

# Weighted prediction, which also forces reference list reordering: x264 puts the same picture in
# the list twice at different weights, so the list is longer than the number of distinct pictures.
#   ffmpeg -i BBB.webm -t 1 -s 176x144 -pix_fmt yuv420p -c:v libx264 -profile:v main \
#          -x264-params "bframes=0:cabac=1:weightp=2:ref=3:partitions=all" -qp 24 \
#          -bsf:v h264_mp4toannexb -f h264 w-cabac.h264      (and cabac=0 for w-cavlc)
for f in w-cabac w-cavlc; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done

# B-slice fixtures, for the stage that is not written yet.  b-adapt=0 forces the full B pattern
# rather than letting x264 decide per frame, and the two direct modes are separated because they
# are separate algorithms.
#   B="b-adapt=0:bframes=2:b-pyramid=none:weightp=0:ref=1:partitions=none"
#   ffmpeg -i BBB.webm -t 1 -s 176x144 -pix_fmt yuv420p -c:v libx264 -profile:v main \
#          -x264-params "$B:direct=spatial:cabac=0" -qp 24 -bsf:v h264_mp4toannexb -f h264 b-spat.h264
#   ...b-temp with direct=temporal, b-cabac with direct=spatial:cabac=1
for f in b-spat b-temp b-cabac; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done

# The reproduction for the one known decoder bug: temporal direct prediction combined with more
# than one reference picture.  Neither alone is enough, which is why there are three of them.
#   B="b-adapt=0:bframes=2:b-pyramid=none:cabac=1:partitions=none:weightp=0:weightb=0"
#   ffmpeg -i BBB.webm -t 1 -s 176x144 -pix_fmt yuv420p -c:v libx264 -profile:v main \
#          -x264-params "$B:direct=temporal:ref=3" -qp 24 -bsf:v h264_mp4toannexb -f h264 bt-r3.h264
#   ...bs-r3 with direct=spatial:ref=3, bt-r1 with direct=temporal:ref=1
# b-hard is the same fault with everything else on too, where it desynchronises outright.
for f in bt-r3 bs-r3 bt-r1 b-hard; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done

# Big Buck Bunny as all-intra H.264, if you want a longer real-motion clip to eyeball in a player.
# Nothing in the test suite uses it, which is why it is not in the repository: at 14 MB it would be
# most of the clone.
#   ffmpeg -i BBB.webm -t 8 -s 640x360 -pix_fmt yuv420p -c:v libx264 -profile:v baseline -g 1 \
#          -qp 24 -an bbb-h264.mp4

# High profile.  hp-plain uses neither the 8x8 transform nor scaling lists, so it decodes with no
# High-specific code at all — it is the fixture that proves the refusal tests the FLAG and not the
# profile.  hp-cqm carries the default scaling matrices.  hp-8x8 and hp-8x8c need the 8x8 transform
# and are still refused.
#   H="b-adapt=0:bframes=0:ref=1:weightp=0"
#   ffmpeg -i BBB.webm -t 1 -s 176x144 -pix_fmt yuv420p -c:v libx264 -profile:v high \
#          -x264-params "$H:8x8dct=0:cqm=flat:cabac=1" -qp 24 -bsf:v h264_mp4toannexb -f h264 hp-plain.h264
#   ...hp-cqm with 8x8dct=0:cqm=jvt, hp-8x8 with 8x8dct=1:cqm=flat, hp-8x8c the same with cabac=0
for f in hp-plain hp-cqm hp-8x8 hp-8x8c; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done
