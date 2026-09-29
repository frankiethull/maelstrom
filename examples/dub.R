## dub.R: one-drop dub with skanks, a two-bar bassline, and a melodica
## that only shows up in the second half. 120 BPM.
## Format: standalone script and CLI pattern file.
## Standalone: Rscript examples/dub.R [out.wav]
## (Also: ./exec/maelstrom render examples/dub.R -d 16 --stereo --reverb 0.3)

suppressPackageStartupMessages(library(maelstrom))

cps <- 0.5  # 120 BPM
cycles <- 8  # 16 seconds

## one drop: kick on 1 and 3, snare on 2 and 4
kick <- s("bd ~ bd ~") |> gain(0.85)
snare <- s("~ sd ~ sd") |> gain(0.8)
hats <- s("hh*8") |> swing(0.25) |> gain(0.55)

## two-bar bassline, alternating every bar
bass <- slowcat(
  note("c2 ~ c2 d2"),
  note("c2 ~ bb1 ~")
) |> synth("sine") |> lpf(400) |> gain(0.9)

## offbeat skank chords
skank <- stack(
  note("~ c4 ~ c4 ~ c4 ~ c4"),
  note("~ eb4 ~ eb4 ~ eb4 ~ eb4"),
  note("~ g4 ~ g4 ~ g4 ~ g4")
) |> synth("sawtooth") |> lpf(1500) |> attack(0.005) |> gain(0.3) |> pan(0.6)

## melodica joins for cycles 4-7, swimming in echo.
## (one slowcat entry per bar; the echo is applied after sectioning so it
## stays tight in bar time)
quiet <- silence()
melpat <- note("g4 ~ a4 bb4 ~ g4 ~ ~") |> synth("square") |>
  lpf(2000) |> gain(0.28)
melodica <- slowcat(quiet, quiet, quiet, quiet,
                    melpat, melpat, melpat, melpat) |>
  echo(n = 3, time = 0.125, fb = 0.5) |> pan(0.4)

pattern <- stack(kick, snare, hats, bass, skank, melodica)

## -- render ------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else {
    f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    d <- if (length(f) >= 1) dirname(sub("^--file=", "", f[1])) else "."
    file.path(d, "dub.wav")
  }
  samples <- render_stereo(pattern, cycles = cycles, cps = cps)
  samples <- add_reverb(samples, 0.3)
  write_wav(out, samples)
  message(sprintf("wrote %s (%.1f seconds, peak %.3f)", out, cycles / cps, max(abs(samples))))
}
