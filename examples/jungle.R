## jungle.R: chopped amen-style break with a Reese-ish bass, 170 BPM.
## 1 cycle = 1 bar. Second half adds a ride, a busier bass, and fills.
## Format: standalone script and CLI pattern file.
## Standalone: Rscript examples/jungle.R [out.wav]
## (Also: ./exec/maelstrom render examples/jungle.R -d 16 --stereo --reverb 0.1)

suppressPackageStartupMessages(library(maelstrom))

cps <- 170 / 240  # 170 BPM
cycles <- 8       # about 11.3 seconds

## the break: kick on 1, 7, 11; snare on 5, 13 with ghost 16ths at the end
kick <- s("bd ~ ~ ~ ~ ~ bd ~ ~ ~ bd ~ ~ ~ ~ ~") |> gain(0.9)
snare <- s("~ ~ ~ ~ sd ~ ~ ~ ~ ~ ~ ~ sd ~ sd sd") |> gain(0.85)
hats <- s("hh*16") |> degrade(0.2) |> gain(0.6)

## low Reese-style rumble; the second half doubles it to 16ths.
## (one slowcat entry per bar; slow() would stretch the phrase, which is
## not what a section change wants)
reese8 <- note("c1*8")
reese16 <- note("c1*8") |> fast(2)
reese <- slowcat(reese8, reese8, reese8, reese8,
                 reese16, reese16, reese16, reese16) |>
  synth("sawtooth") |> lpf(300) |> gain(0.9)

## chopped snare answers, plus a ride that only shows up halfway
chop <- s("~ sd ~ cp") |> fast(2) |> gain(0.45) |> pan(0.65)
ridepat <- s("~ ~ oh ~ ~ ~ oh ~") |> gain(0.5)
quiet <- silence()
ride <- slowcat(quiet, quiet, quiet, quiet,
                ridepat, ridepat, ridepat, ridepat)

## snare fill rolling into each half
fill <- every(s("sd*4") |> gain(0.5), 4, function(p) fast(p, 4))

pattern <- stack(kick, snare, hats, reese, chop, ride, fill)

## -- render ------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else {
    f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    d <- if (length(f) >= 1) dirname(sub("^--file=", "", f[1])) else "."
    file.path(d, "jungle.wav")
  }
  samples <- render_stereo(pattern, cycles = cycles, cps = cps)
  samples <- add_reverb(samples, 0.1)
  write_wav(out, samples)
  message(sprintf("wrote %s (%.1f seconds, peak %.3f)", out, cycles / cps, max(abs(samples))))
}
