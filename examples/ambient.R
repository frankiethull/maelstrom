## ambient.R: beatless evolving ambient, lots of reverb.
## No drums at all. 1 cycle = 4 seconds; the chord changes halfway through.
## Format: standalone script and CLI pattern file.
## Standalone: Rscript examples/ambient.R [out.wav]
## (Also: ./exec/maelstrom render examples/ambient.R -d 16 --stereo --reverb 0.5)

suppressPackageStartupMessages(library(maelstrom))

cps <- 0.25  # 1 cycle = 4 seconds
cycles <- 4  # 16 seconds

## Cm7 for two bars, then Abmaj7 for two bars; each chord retriggered per bar
## (notes longer than a bar would hit the 4-second voice cap and leave gaps)
pad <- slowcat(
  chord("c3", "eb3", "g3", "bb3"),
  chord("c3", "eb3", "g3", "bb3"),
  chord("ab2", "c3", "eb3", "g3"),
  chord("ab2", "c3", "eb3", "g3")
) |> synth("sine") |> attack(1.5) |> gain(0.45)

## sparse euclidean bells with a long echo tail
bells <- note("eb6(3,16)") |> slow(4) |> synth("sine") |>
  attack(0.01) |> gain(0.2) |> echo(n = 4, time = 0.5, fb = 0.5) |> pan(0.6)

## slow triangle shimmer wandering above the pad, one phrase per bar
shimmer <- note("c6 ~ g5 ~") |> slow(2) |> synth("triangle") |>
  gain(0.15) |> pan(0.35)

## low root drone following the chords, retriggered per bar
sub <- slowcat(note("c2"), note("c2"), note("ab1"), note("ab1")) |>
  synth("sine") |> gain(0.4)

pattern <- stack(pad, bells, shimmer, sub)

## -- render ------------------------------------------------------------------
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  out <- if (length(args) >= 1) args[1] else {
    f <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
    d <- if (length(f) >= 1) dirname(sub("^--file=", "", f[1])) else "."
    file.path(d, "ambient.wav")
  }
  samples <- render_stereo(pattern, cycles = cycles, cps = cps)
  samples <- add_reverb(samples, 0.5)
  write_wav(out, samples)
  message(sprintf("wrote %s (%.1f seconds, peak %.3f)", out, cycles / cps, max(abs(samples))))
}
