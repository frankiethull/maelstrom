## boombap.R: dusty hip-hop groove on the acoustic kit, 92 BPM.
## 1 cycle = 1 bar. Stripped intro for 2 bars, then the full beat drops.
## Format: standalone script and CLI pattern file.
## Standalone: Rscript examples/boombap.R [out.wav]
## (Also: ./exec/maelstrom render examples/boombap.R -d 16 --stereo --reverb 0.2)

suppressPackageStartupMessages(library(maelstrom))

cps <- 92 / 240  # 92 BPM
cycles <- 6      # about 15.6 seconds

## dusty acoustic drums, swung
kick <- s("bd ~ ~ bd ~ ~ bd ~") |> kit("acoustic") |> gain(0.95)
snare <- s("~ ~ sd ~ ~ ~ sd ~") |> kit("acoustic") |> gain(0.9)
hats <- s("hh*8") |> kit("acoustic") |> swing(0.3) |> degrade(0.25) |> gain(1.0)
shaker <- s("~ ~ ~ shaker ~ ~ shaker ~") |> gain(0.8) |> pan(0.65)
rim <- s("rim ~ ~ ~") |> degrade(0.5) |> gain(0.4) |> pan(0.3)

## sub bass and dusty keys
bass <- note("c2 ~ ~ ~ ~ ~ eb2 ~") |> synth("sine") |> gain(0.9)
keys <- note("eb4 g4 bb4") |> slow(2) |> synth("triangle") |>
  attack(0.02) |> gain(0.3) |> pan(0.65)

## intro: kick, shaker and bass for 2 bars, then everything drops in.
## (one slowcat entry per bar keeps every layer at full density)
intro <- stack(kick, shaker, bass)
full <- stack(kick, snare, hats, shaker, rim, bass, keys)
pattern <- slowcat(intro, intro, full, full, full, full)

## -- render ------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else {
    f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    d <- if (length(f) >= 1) dirname(sub("^--file=", "", f[1])) else "."
    file.path(d, "boombap.wav")
  }
  samples <- render_stereo(pattern, cycles = cycles, cps = cps)
  samples <- add_reverb(samples, 0.2)
  write_wav(out, samples)
  message(sprintf("wrote %s (%.1f seconds, peak %.3f)", out, cycles / cps, max(abs(samples))))
}
