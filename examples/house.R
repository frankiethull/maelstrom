## house.R: deep house, 124 BPM. Four on the floor, offbeat hats,
## chord stabs joining halfway, and a snare fill rolling into each half.
## Format: standalone script and CLI pattern file.
## Standalone: Rscript examples/house.R [out.wav]
## (Also: ./exec/maelstrom render examples/house.R -d 16 --stereo --reverb 0.15)

suppressPackageStartupMessages(library(maelstrom))

cps <- 124 / 240  # 124 BPM
cycles <- 8       # about 15.5 seconds

## the foundation, all the way through
kick <- s("bd*4") |> gain(0.9)
bass <- note("c2 c2 c2 g1") |> synth("sawtooth") |> lpf(700) |> gain(0.75)
chat <- s("hh*16") |> degrade(0.1) |> gain(0.5)
ohat <- s("~ oh ~ oh") |> gain(0.55)

## second-half layers: clap and dubby chord stabs (one entry per bar)
quiet <- silence()
clap <- slowcat(quiet, quiet, quiet, quiet,
                s("~ cp ~ cp") |> gain(0.7),
                s("~ cp ~ cp") |> gain(0.7),
                s("~ cp ~ cp") |> gain(0.7),
                s("~ cp ~ cp") |> gain(0.7))
stabpat <- stack(
  note("~ eb4 ~ ~"),
  note("~ g4 ~ ~"),
  note("~ bb4 ~ ~")
) |> synth("sawtooth") |> lpf(2200) |> attack(0.005) |> gain(0.28)
stab <- slowcat(quiet, quiet, quiet, quiet,
                stabpat, stabpat, stabpat, stabpat) |>
  echo(n = 2, time = 0.125, fb = 0.4) |> pan(0.6)

## snare fill rolling into the end of each half
fill <- every(s("sd*4") |> gain(0.45), 4, function(p) fast(p, 4))

pattern <- stack(kick, bass, chat, ohat, clap, stab, fill)

## -- render ------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else {
    f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    d <- if (length(f) >= 1) dirname(sub("^--file=", "", f[1])) else "."
    file.path(d, "house.wav")
  }
  samples <- render_stereo(pattern, cycles = cycles, cps = cps)
  samples <- add_reverb(samples, 0.15)
  write_wav(out, samples)
  message(sprintf("wrote %s (%.1f seconds, peak %.3f)", out, cycles / cps, max(abs(samples))))
}
