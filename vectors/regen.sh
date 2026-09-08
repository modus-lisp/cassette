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

# fast.mp4 now decodes rather than being refused, so it needs a reference decode like the rest.
# It is worth more than its size suggests: x264's defaults, which means a B PYRAMID, and it was the
# only fixture that exercised adaptive reference marking.
ffmpeg -hide_banner -loglevel error -y -i fast.mp4 -f rawvideo -pix_fmt yuv420p fast.yuv

# High profile.  hp-plain uses neither the 8x8 transform nor scaling lists, so it decodes with no
# High-specific code at all — it is the fixture that proves the refusal tests the FLAG and not the
# profile.  hp-cqm carries the default scaling matrices.  hp-8x8 and hp-8x8c need the 8x8 transform
# and are still refused.
#   H="b-adapt=0:bframes=0:ref=1:weightp=0"
#   ffmpeg -i BBB.webm -t 1 -s 176x144 -pix_fmt yuv420p -c:v libx264 -profile:v high \
#          -x264-params "$H:8x8dct=0:cqm=flat:cabac=1" -qp 24 -bsf:v h264_mp4toannexb -f h264 hp-plain.h264
#   ...hp-cqm with 8x8dct=0:cqm=jvt, hp-8x8 with 8x8dct=1:cqm=flat, hp-8x8c the same with cabac=0
#
# hp-real is the one that matters most and has the fewest options set: x264 with NO parameters at
# all beyond the quantiser, which is what almost every High profile file in the world is.  B
# pyramid, weighted prediction, three references, the 8x8 transform and CABAC, all at once.
#   ffmpeg -i bbb360.webm -t 2 -s 176x144 -pix_fmt yuv420p -c:v libx264 -profile:v high \
#          -preset medium -qp 24 -bsf:v h264_mp4toannexb -f h264 hp-real.h264
# hi422.mp4 is 4:2:2 chroma, which is refused: every sample index in the decoder assumes chroma is
# half the luma in BOTH directions, so this is a shape to change and not a flag to add.  It is the
# fixture that keeps the refusal path honest now that High profile itself decodes.
#   ffmpeg -i bbb360.webm -t 1 -s 176x144 -pix_fmt yuv422p -c:v libx264 -profile:v high422 \
#          -qp 24 -f mp4 hi422.mp4
for f in hp-plain hp-cqm hp-8x8 hp-8x8c hp-real; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.h264" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done

# ---- MPEG-1 and MPEG-2 ---------------------------------------------------------------------------
#
# THE ORACLE TAKES TWO ARGUMENTS THE OTHERS DO NOT, and both matter.
#
#   -idct simple   MPEG-2 does not specify its inverse transform exactly; it requires only the
#                  accuracy of IEEE 1180, and ffmpeg ships several transforms that all meet it and
#                  disagree in the last bit.  Naming one is what makes "bit-exact" mean anything.
#   -vsync 0       without it ffmpeg may duplicate a frame to fill a frame rate, and then every
#                  frame after the duplicate compares against its neighbour.
#
# The fixtures are spread across the features that are genuinely separate code: MPEG-1 as well as
# MPEG-2, both scan orders, both quantiser ladders, the alternative intra coefficient table, custom
# weight matrices, interlaced coding (field DCT and field motion inside a frame picture), and a
# width where ffmpeg's MPEG-1 encoder starts putting several rows in one slice.
#
#   S="testsrc2=size=176x144:rate=25:duration=1"
#   ffmpeg -f lavfi -i "$S" -c:v mpeg2video -g 12 -bf 2 -qscale:v 4 -pix_fmt yuv420p m2-basic.m2v
#   ...m2-mpeg1 with -c:v mpeg1video, m2-ivlc with -intra_vlc 1, m2-altscan with -alternate_scan 1,
#      m2-nlq with -non_linear_quant 1 -qmax 28 -b:v 400k, m2-ilace with -flags +ilme+ildct,
#      m2-cqm with -intra_matrix "8,17,18,...", m2-mpeg1-wide and m2-cif at 352x288
#
# The four system-stream fixtures are the same picture in four framings: a program stream, a
# transport stream, an MPEG-1 program stream, and H.264 in a transport stream — which is what
# broadcast television is, and the reason the transport demuxer earns its keep.
#
#   ffmpeg -f lavfi -i "$S" -c:v mpeg2video -g 12 -bf 2 -b:v 400k -pix_fmt yuv420p -f vob m2-ps.mpg
#   ...m2-ts.ts with -f mpegts, m1-ps.mpg with -c:v mpeg1video -f mpeg,
#      h264-ts.ts with -c:v libx264 -preset fast -crf 26 -f mpegts
for f in m2-basic m2-mpeg1 m2-mpeg1-wide m2-ilace m2-altscan m2-ivlc m2-cqm m2-nlq m2-cif; do
  ffmpeg -hide_banner -loglevel error -y -idct simple -i "$f.m2v" -vsync 0 \
         -f rawvideo -pix_fmt yuv420p "$f.yuv"
done
for f in m2-ps.mpg m2-ts.ts m1-ps.mpg h264-ts.ts; do
  ffmpeg -hide_banner -loglevel error -y -idct simple -i "$f" -vsync 0 \
         -f rawvideo -pix_fmt yuv420p "${f%.*}.yuv"
done

# ---- AVI ------------------------------------------------------------------------------------------
#
# Three files for three purposes: two codecs that decode today, carried in a container that has never
# heard of either, and one that does not — so that the refusal path stays honest.
#
#   S="testsrc2=size=176x144:rate=25:duration=0.6"
#   ffmpeg -f lavfi -i "$S" -c:v libx264 -preset fast -crf 26 -pix_fmt yuv420p h264.avi
#   ffmpeg -f lavfi -i "$S" -c:v mpeg2video -qscale:v 4 -pix_fmt yuv420p m2.avi
#   ffmpeg -f lavfi -i "$S" -f lavfi -i "sine=frequency=440:duration=0.6" \
#          -c:v mpeg4 -vtag XVID -qscale:v 4 -c:a libmp3lame -b:a 64k -shortest -pix_fmt yuv420p asp.avi
for f in h264 m2; do
  ffmpeg -hide_banner -loglevel error -y -idct simple -i "$f.avi" -vsync 0 \
         -f rawvideo -pix_fmt yuv420p "$f-avi.yuv"
done

# ---- MPEG-4 Part 2 --------------------------------------------------------------------------------
#
# The codec behind DivX and XviD.  The fixtures are spread across the pieces that are separate code:
# one motion vector and four, the H.263 quantiser and the MPEG one, B pictures with direct mode, and
# a size where the encoder cuts each picture into more video packets.
#
#   S="testsrc2=size=176x144:rate=25:duration=0.4"
#   ffmpeg -f lavfi -i "$S" -c:v mpeg4 -vtag XVID -qscale:v 4 -g 5 -bf 0 -pix_fmt yuv420p mp4v-i.m4v
#   ...mp4v-4mv adds -flags +mv4, mp4v-b uses -bf 2, mp4v-mq adds -mpeg_quant 1,
#      mp4v-full is duration 1 with -qscale:v 3 -g 12 -bf 2 -flags +mv4 -mpeg_quant 1,
#      mp4v-big is 352x288 duration 0.6 with -b:v 600k -g 12 -bf 2 -flags +mv4
#      mp4v-asp is Advanced Simple: duration 0.6 with -qscale:v 3 -g 8 -bf 2 -flags +qpel+mv4
for f in mp4v-i mp4v-4mv mp4v-b mp4v-mq mp4v-full mp4v-big mp4v-asp; do
  ffmpeg -hide_banner -loglevel error -y -idct simple -i "$f.m4v" -vsync 0 \
         -f rawvideo -pix_fmt yuv420p "$f.yuv"
done
ffmpeg -hide_banner -loglevel error -y -idct simple -i asp.avi -vsync 0 \
       -f rawvideo -pix_fmt yuv420p asp-avi.yuv

# A program stream and a transport stream carrying the SAME video and audio, so that the two
# demuxers can be checked to agree rather than merely each to work.  The audio is MPEG Layer II,
# which is what a DVD and a broadcast capture actually carry — and which the container names only as
# "MPEG audio", leaving the layer to be read out of the first frame header.
#   S="testsrc2=size=176x144:rate=25:duration=0.6"; A="sine=frequency=440:duration=0.6:sample_rate=48000"
#   ffmpeg -f lavfi -i "$S" -f lavfi -i "$A" -c:v mpeg2video -g 12 -bf 2 -b:v 400k \
#          -c:a mp2 -b:a 192k -ac 2 -pix_fmt yuv420p -shortest -f vob m2-av.mpg
#   ...m2-av.ts with -f mpegts

# ---- FFV1 -----------------------------------------------------------------------------------------
#
# The lossless codec archives keep masters in, so the oracle is bit-exact or nothing.  The fixtures
# cover what varies between real files: chroma layout, slice count, which of the two range coder
# state tables the stream chose, and the reversible colour transform that makes lossless RGB work.
#
#   S="testsrc2=size=176x144:rate=25:duration=0.4"
#   ffmpeg -f lavfi -i "$S" -c:v ffv1 -level 3 -coder 1 -slices 4  -pix_fmt yuv420p ffv1-a.mkv
#   ...ffv1-422 with -pix_fmt yuv422p, ffv1-16sl with -slices 16, ffv1-dflttab with -coder 2,
#      ffv1-rgb with -pix_fmt gbrp
for f in ffv1-a ffv1-16sl ffv1-dflttab; do
  ffmpeg -hide_banner -loglevel error -y -i "$f.mkv" -f rawvideo -pix_fmt yuv420p "$f.yuv"
done
ffmpeg -hide_banner -loglevel error -y -i ffv1-422.mkv -f rawvideo -pix_fmt yuv422p ffv1-422.yuv
ffmpeg -hide_banner -loglevel error -y -i ffv1-rgb.mkv -f rawvideo -pix_fmt gbrp    ffv1-rgb.yuv

# ---- Ogg ------------------------------------------------------------------------------------------
#
# Three files for what Ogg actually is: a framing layer with no codec field.  theora-av.ogv has two
# streams so that separating them can be checked; theora.ogv has one so that the refusal path can;
# opus-ogg.ogg is a codec that decodes, so the packet order and timing can be.
#
#   S="testsrc2=size=176x144:rate=25:duration=0.6"; A="sine=frequency=440:duration=0.6"
#   ffmpeg -f lavfi -i "$S" -f lavfi -i "$A" -c:v libtheora -q:v 7 -c:a libvorbis -q:a 4 \
#          -shortest theora-av.ogv
#   ffmpeg -f lavfi -i "$S" -c:v libtheora -q:v 7 -an theora.ogv
#   ffmpeg -f lavfi -i "sine=frequency=440:duration=1" -c:a libopus opus-ogg.ogg
