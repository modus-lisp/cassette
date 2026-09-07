# cassette

**Media containers in pure Common Lisp.** Matroska/EBML (WebM) and ISO base media (MP4, M4A,
MOV) demuxers, a WebM muxer, and a pull-model player that hands out decoded pictures and audio
in step.  No FFI.

*A cassette is a shell that holds several tracks wound together and hands them back in
step.*  That is what a container format is, and it is all this is.  The codecs are
dependencies, not contents: [`reel`](../reel) decodes the video, [`reed`](../reed) the
audio.  Which codec a track happens to carry is not something the shell around it knows.

The VP8 decoder this was built around lives in `reel` now, together with the encoder
that used to be in webrtc-media.  This repo kept the containers — which is the split
that lets the muxer wrap reel's encoder output into a `.webm` without either of them
knowing about the other (see `inspect/test-encode-mux.lisp`).

## Status

| Piece | State |
| ----- | ----- |
| VP8 decoder (in `reel`, asserted here) | **Bit-exact with ffmpeg/libvpx** on the libvpx-encoded vectors (synthetic clips with alt-ref, 2/4 token partitions, error-resilient mode, split MVs) and on Big Buck Bunny 640x360. ~45 fps at 640x360 on one core. **Open:** the two W3C WPT clips (`wpt-test`, `wpt-circles`, an older encoder) differ slightly — ±1 on skipped ZEROMV macroblocks, larger on one B_PRED macroblock inside an inter frame; the partition parse stays in sync, so it is a reconstruction detail, not a desync. |
| WebM demuxer | SimpleBlock and BlockGroup, Xiph/EBML/fixed lacing, header-stripping ContentEncoding, unknown-size Segment/Cluster, Cues via SeekHead, and `cluster-index` (a one-pass walk of cluster headers) for files without Cues. |
| MP4 demuxer | `moov` sample tables (`stsz`/`stz2`, `stco`/`co64`, `stsc`, `stts`, `ctts`, `stss`), fragmented files (`moof`/`traf`/`trun`/`tfdt`, `trex` defaults), edit lists, `avcC`/`esds`/`dOps` codec configuration. **Every packet matches ffprobe** — size, presentation time to the microsecond, and sync flag — across four container shapes including B-frames and fragmentation. Seeking is a binary search of the sync samples. |
| Muxer | Seekable output: SeekHead, Info, Tracks, Cues (before the clusters, fixed-width so offsets are known up front), SimpleBlocks, BlockGroup+DiscardPadding for Opus tails.  Round-trips ffmpeg-made files byte-for-byte at the frame level and ffmpeg decodes the result to identical pixels. |
| Audio | Opus through `reed`.  Vorbis is demuxed but not decoded (video-only playback). |
| Seeking | `seek-webm` repositions at the cluster at or before a time and hands out the next key frame first; a caller wanting the exact frame decodes forward from there (warp's media player does). |
| Codecs | Video is [`reel`](../reel)'s: VP8 only. **An MP4 is usually H.264, which reel does not decode** — such a file opens anyway, names the codec in `player-unsupported`, and plays whatever else it has. Audio is [`reed`](../reed)'s: Opus per packet here; AAC through reed's own MP4 reader. |
| Not done | H.264/VP9/AV1 decoding, Vorbis, an MP4 muxer, A/V pacing (the player is pull-model; the caller paces — see `warp-media` for a paced player on top of this). |

## Playback

```lisp
(asdf:load-system :cassette)

(let ((p (cassette:open-media "movie.webm")))          ; or "movie.mp4" — it sniffs
  (loop for pic = (cassette:next-video-frame p)
        while pic
        do (present (cassette:picture->rgb pic)          ; packed RGB, or :channels 4 for RGBA
                    (cassette:picture-width pic) (cassette:picture-height pic)
                    (cassette:picture-timestamp pic))))   ; seconds
```

`next-video-frame` returns the next *displayed* frame; hidden alt-ref frames
are decoded and skipped.  A `picture` shares its planes with the decoder's
reference buffer and stays valid until the frame after the next one is
decoded — copy it (`picture->rgb`, `picture->yuv420`) if you keep it longer.

Audio: `(cassette:next-audio-frame p)` yields one decoded Opus packet as a
`reed:pcm` (48 kHz, interleaved 16-bit) plus its timestamp;
`decode-all-audio` concatenates the whole track.

Without a display, pipe raw frames to ffplay:

```sh
sbcl --non-interactive --eval '(asdf:load-system :cassette)' \
     --eval '(cassette:play-to-ffplay "movie.webm" :stream (sb-sys:make-fd-stream 1 :output t :element-type (quote (unsigned-byte 8))))' \
  | ffplay -f rawvideo -pixel_format yuv420p -video_size 640x360 -framerate 30 -
```

## Muxing

```lisp
(let* ((mx (cassette:make-muxer))                        ; 1 ms timecode ticks
       (v (cassette:add-video-track mx :width 640 :height 360 :frame-rate 30))
       (a (cassette:add-audio-track mx :sample-rate 48000 :channels 2
                                        :codec-private opus-head :codec-delay 6500000 :seek-pre-roll 80000000)))
  (cassette:add-frame mx v 0 keyframe-octets :keyframe t)
  (cassette:add-frame mx v 33333333 inter-octets)
  (cassette:add-frame mx a 0 opus-packet)
  (cassette:write-webm-file mx "out.webm"))
```

Timestamps are nanoseconds.  Frames may be added in any order; they are sorted
and grouped into clusters (a new one at each video key frame after ~5 s).

## Demuxing only

```lisp
(let ((w (cassette:parse-webm (cassette::slurp-file "movie.webm"))))
  (cassette:webm-tracks w)                    ; TRACK structs: codec-id, width/height, sample-rate, codec-private ...
  (cassette:map-frames (lambda (f) (list (cassette:frame-timecode f) (cassette:frame-keyframe-p f)
                                          (length (cassette:frame-data f))))
                        w :track (cassette:webm-video-track w)))
```

## Tests

All tests compare against ffmpeg (needed on `PATH`):

```sh
sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-decode.lisp      # bit-exact YUV vs ffmpeg, all vectors
sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-mp4.lisp         # every MP4 packet vs ffprobe
sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-mux.lisp         # demux -> remux -> ffprobe / ffmpeg md5
sbcl --dynamic-space-size 2048 --non-interactive --load inspect/test-encode-mux.lisp  # webrtc-media encoder -> mux -> both decoders agree
sbcl --dynamic-space-size 2048 --non-interactive --load inspect/dump-frame.lisp FILE.webm /tmp/out 0 45  # PPM dumps
```

`vectors/*.yuv` are ffmpeg's decodes and are not committed (tens of MB each): run
`sh vectors/regen.sh` once after cloning, and add a line there for any new vector.  `vectors/rfc6386.txt` is the
specification with the reference decoder source, kept next to the code it
was checked against.

## Design notes

- The current frame is reconstructed in webp-pure `plane`s (fixnum rasters
  with the 127/129 intra borders), loop filtered whole, then copied into an
  `rframe` (octet planes with a 32-pixel replicated border).  Intra
  prediction therefore reads unfiltered neighbours and motion compensation
  reads filtered references, as the format requires.
- Motion vectors that reach past the border use clamped fetches, which is
  the unbounded edge replication the specification describes, so no
  vector clamping at prediction time is needed (only the NEAR/NEAREST/best
  clamps of mode decoding, which change the coded meaning).
- Reference buffers are recycled from a pool; the last shown frame is kept
  alive so a caller's `picture` survives one more decode.
