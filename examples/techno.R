## techno.R: driving four-on-the-floor techno, 132 BPM.
## 1 cycle = 1 bar. Two sections: stripped groove, then full.
## The bass filter opens up over the whole track.
## Format: standalone script and CLI pattern file.
## Standalone: Rscript examples/techno.R [out.wav]
## (Also: ./exec/maelstrom render examples/techno.R -d 16 --stereo --reverb 0.15)

suppressPackageStartupMessages(library(maelstrom))

cps <- 0.55  # 132 BPM
cycles <- 8  # about 14.5 seconds

## the groove that never leaves
kick <- s("bd*4") |> gain(0.9)
bass <- note("[c2 c2 c3 c2]*4") |> synth("sawtooth") |> gain(0.7) |>
  lpf(slow(n("300 600 1200 2400 4800 4800 9600 9600"), 8))
chat <- s("hh*16") |> degrade(0.15) |> gain(0.7)

## everything below joins for the second half (cycles 4-7), one entry per bar
quiet <- silence()
clap <- slowcat(quiet, quiet, quiet, quiet,
                s("~ cp ~ cp") |> gain(0.8),
                s("~ cp ~ cp") |> gain(0.8),
                s("~ cp ~ cp") |> gain(0.8),
                s("~ cp ~ cp") |> gain(0.8))
ohat <- slowcat(quiet, quiet, quiet, quiet,
                s("~ oh ~ oh") |> gain(0.6),
                s("~ oh ~ oh") |> gain(0.6),
                s("~ oh ~ oh") |> gain(0.6),
                s("~ oh ~ oh") |> gain(0.6))
stabpat <- note("eb4 ~ g4 ~") |> synth("square") |>
  lpf(1800) |> gain(0.3) |> pan(0.7)
stab <- slowcat(quiet, quiet, quiet, quiet, stabpat, stabpat, stabpat, stabpat)

pattern <- stack(kick, bass, chat, clap, ohat, stab)

## -- render ------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else {
    f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    d <- if (length(f) >= 1) dirname(sub("^--file=", "", f[1])) else "."
    file.path(d, "techno.wav")
  }
  samples <- render_stereo(pattern, cycles = cycles, cps = cps)
  samples <- add_reverb(samples, 0.15)
  write_wav(out, samples)
  message(sprintf("wrote %s (%.1f seconds, peak %.3f)", out, cycles / cps, max(abs(samples))))
}
