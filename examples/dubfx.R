## dubfx.R: dub with swing, echo, stereo panning, and reverb.
## Format: CLI pattern file (defines `pattern` + `cps`).
## Render it with:
##   ./exec/maelstrom render examples/dubfx.R -o dubfx.wav -d 8 --stereo --reverb 0.25
# library(maelstrom)  # uncomment for interactive use; the CLI attaches it

kick <- s("bd ~ bd ~") |> gain(0.8) |> pan(0.3)
snare <- s("~ sd ~ sd") |> pan(0.7)
hats <- s("hh*16") |> swing(0.4) |> gain(0.5)
bass <- note("c2 ~ c2 ~") |> synth("sine") |> lpf(500) |> gain(0.9)
stab <- stack(
  note("~ c4 ~ c4"),
  note("~ eb4 ~ eb4"),
  note("~ g4 ~ g4")
) |>
  synth("sawtooth") |> lpf(1200) |> attack(0.01) |> gain(0.4) |>
  echo(n = 3, time = 0.375, fb = 0.45) |> pan(0.6)

pattern <- stack(kick, snare, hats, bass, stab)

cps <- 0.5
