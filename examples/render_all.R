#!/usr/bin/env Rscript
## Render every example to examples/<name>.wav (stereo, per-file cps/cycles).
## Output is gitignored: run `Rscript examples/render_all.R` from the repo root.
## (Standalone examples skip their own render block when sourced here.)

suppressPackageStartupMessages(library(maelstrom))

files <- list.files("examples", pattern = "[.]R$", full.names = TRUE)
files <- files[basename(files) != "render_all.R"]

for (f in files) {
  env <- new.env(parent = globalenv())
  sys.source(f, envir = env)
  if (is.null(env$pattern) || !inherits(env$pattern, "ms_pattern")) {
    message("skip ", f, " (no pattern)")
    next
  }
  cps <- if (!is.null(env$cps)) as.numeric(env$cps) else 0.5
  cycles <- if (!is.null(env$cycles)) as.numeric(env$cycles) else 8
  out <- file.path("examples", sub("[.]R$", ".wav", basename(f)))
  write_wav(out, render_stereo(env$pattern, cycles = cycles, cps = cps))
  message(sprintf("wrote %s (%.1fs)", out, cycles / cps))
}
