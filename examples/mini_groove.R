## mini_groove.R: the original groove, rewritten in pipe style.
## Format: CLI pattern file (defines `pattern` + `cps`).
## Render with: ./exec/maelstrom render examples/mini_groove.R -o groove.wav -d 8
# library(maelstrom)  # uncomment for interactive use; the CLI attaches it

kick <- s("bd(4,8) ~")
snare <- s("~ sd ~ sd")
hats <- s("hh*8")
open_hat <- s("~ ~ oh ~ ~ ~ oh ~")
bass <- note("c2 c2 ~ c2 eb2 ~ g1 ~") |> synth("sawtooth") |> gain(0.6)
arp <- note("<c4 eb4 g4 bb4>*2") |> synth("square") |> gain(0.3)

pattern <- stack(kick, snare, hats, open_hat, bass, arp)

cps <- 2.0
