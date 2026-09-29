# maelstrom

The uzu pattern language, built in R. Write music as patterns — short
strings like `"bd(4,8) ~"`, chains like
`s("hh*16") |> swing(0.4) |> gain(0.5)` — layer them with `stack()`, and
render the result to WAV. Everything is synthesized offline in base R, so
there are no dependencies; an optional Rust backend speeds up the reverb.

## Quick start

```r
library(maelstrom)

p <- stack(
  s("bd(4,8) ~"),
  s("~ sd ~ sd"),
  s("hh*8"),
  note("c2 c2 ~ c2 eb2 ~ g1 ~") |> synth("sawtooth") |> gain(0.6)
)
write_wav("groove.wav", render(p, cycles = 16, cps = 2))
```

Or the CLI:

```
./exec/maelstrom validate examples/mini_groove.R
./exec/maelstrom render examples/mini_groove.R -o groove.wav -d 8 --cps 2
```

## Examples

Start here. `examples/` has full tracks — house, techno, dub, ambient,
jungle, boombap — each runnable on its own (`Rscript examples/dub.R`)
or through the CLI. To hear them all at once:

```
Rscript examples/render_all.R
```

(WAVs are gitignored; the vignettes cover the notation and sound design
in full.)

## Shiny playground

Prefer buttons to a terminal? Edit pattern code, press Render, play the
result in the browser:

```r
install.packages("shiny")
maelstrom_playground()
```

Rendering is offline, like everything else here — press Render, then play.

## Install

```r
pak::pak("frankiethull/maelstrom")
# or: devtools::install_github("frankiethull/maelstrom")
# or from a local clone: R CMD INSTALL .
```

`cargo` + `rustc` are optional but recommended: with them, the reverb
runs in Rust (~2000x faster); without them it falls back to pure R and
the CLI tells you so.

Notebooks get inline playback with `maelstrom_listen(p, seconds = 8)`.
`exec/maelstrom-mcp` runs a stdio MCP server exposing the engine
(`validate_pattern`, `render_pattern`, `pattern_events`, `list_voices`)
to agentic clients; it needs the `mcptools` + `ellmer` packages.
