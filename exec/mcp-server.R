#!/usr/bin/env Rscript
## maelstrom MCP server (stdio), mirroring the Python uzu-engine prototype.
##
## Exposes the pattern engine to agentic clients (including Rhythmancer, the
## Nanite-Labs Strudel code model): validate pattern code, render it to audio,
## and inspect the event grid, all without a browser.
##
## Plumbing: mcptools (MIT license, CRAN) with tools defined via ellmer::tool().
## Requires the maelstrom package installed (R CMD INSTALL .).
##
## `code` is pattern DSL source defining `pattern` (and optionally `cps`),
## the same format the `maelstrom render` CLI accepts, e.g.:
##   pattern <- s("bd*4") |> gain(0.8) |> lpf(1200); cps <- 0.5
## Code executes locally with the caller's trust; do not expose this server
## to untrusted clients.

if (!requireNamespace("maelstrom", quietly = TRUE)) {
  stop("maelstrom package is not installed: run `R CMD INSTALL .` in the maelstrom repo")
}
if (!requireNamespace("mcptools", quietly = TRUE)) {
  stop("mcptools is not installed: run `install.packages(\"mcptools\")`")
}
if (!requireNamespace("ellmer", quietly = TRUE)) {
  stop("ellmer is not installed: run `install.packages(\"ellmer\")`")
}
library(maelstrom)

DRUM_NAMES <- names(maelstrom:::DRUMS)
OSC_NAMES <- c(names(maelstrom:::SYNTHS), "sine", "sawtooth", "square", "triangle", "noise")

## render to the system temp dir (not R's session tempdir, which R wipes on
## exit) so the returned path survives server restarts, like the Python prototype
sys_tmpdir <- function() {
  ## NB: Rscript sets TMPDIR to its own session dir, so don't trust it here
  if (.Platform$OS.type == "unix" && dir.exists("/tmp")) "/tmp" else tempdir()
}

render_path <- function() {
  stem <- paste(sample(c(letters, 0:9), 12, replace = TRUE), collapse = "")
  file.path(sys_tmpdir(), paste0("maelstrom_", stem, ".wav"))
}

exec_code <- function(code) {
  env <- new.env(parent = globalenv())
  err <- tryCatch({
    eval(parse(text = code), envir = env)
    NULL
  }, error = function(e) e)
  if (!is.null(err)) return(err)
  pat <- env$pattern
  if (is.null(pat) || !inherits(pat, "ms_pattern")) {
    return(simpleError("code must define `pattern` as a maelstrom pattern"))
  }
  cps <- if (!is.null(env$cps)) suppressWarnings(as.numeric(env$cps)) else 0.5
  if (is.na(cps)) cps <- 0.5
  list(pattern = pat, cps = cps)
}

use_cps <- function(arg, code_cps) {
  if (!is.null(arg)) as.numeric(arg) else code_cps
}

validate_pattern <- ellmer::tool(
  function(code) {
    res <- exec_code(code)
    if (inherits(res, "error")) return(paste0("invalid: ", conditionMessage(res)))
    haps <- tryCatch(query_arc(res$pattern, 0, 4), error = function(e) e)
    if (inherits(haps, "error")) return(paste0("invalid: ", conditionMessage(haps)))
    n_rest <- sum(vapply(haps, is_rest, logical(1)))
    unknown <- unique(unlist(lapply(haps, function(h) {
      s <- h$value$sound
      if (!is.null(s) && !(as.character(s) %in% DRUM_NAMES)) as.character(s) else NULL
    })))
    out <- sprintf("valid: %d events over 4 cycles (%d rests), cps=%g",
                   length(haps), n_rest, res$cps)
    if (length(unknown)) {
      out <- paste0(out, "\nwarning: unknown sounds: ", paste(unknown, collapse = ", "))
    }
    out
  },
  name = "validate_pattern",
  description = paste(
    "Validate maelstrom pattern code without rendering audio.",
    "Returns event counts over 4 cycles, rest count, and any unknown sounds.",
    "Code is R source defining `pattern` (and optionally `cps`), e.g.",
    "'pattern <- s(\"bd*4\") |> gain(0.8)'."
  ),
  arguments = list(
    code = ellmer::type_string("Pattern DSL source defining `pattern`.")
  )
)

render_pattern <- ellmer::tool(
  function(code, seconds = 8, cps = NULL) {
    res <- exec_code(code)
    if (inherits(res, "error")) return(paste0("invalid: ", conditionMessage(res)))
    cps <- use_cps(cps, res$cps)
    cycles <- seconds * cps
    samples <- tryCatch(render(res$pattern, cycles = cycles, cps = cps),
                        error = function(e) e)
    if (inherits(samples, "error")) {
      return(paste0("render error: ", conditionMessage(samples)))
    }
    path <- render_path()
    write_wav(path, samples)
    peak <- if (length(samples)) max(abs(samples)) else 0
    n_events <- length(tryCatch(query_arc(res$pattern, 0, cycles),
                                error = function(e) list()))
    sprintf(paste0("rendered: %s\n",
                   "duration: %gs (%g cycles at %g cps), events: %d, peak: %.3f"),
            path, seconds, cycles, cps, n_events, peak)
  },
  name = "render_pattern",
  description = "Render maelstrom pattern code to a WAV file. Returns the file path plus render stats (duration, peak level, event count).",
  arguments = list(
    code = ellmer::type_string("Pattern DSL source defining `pattern`."),
    seconds = ellmer::type_number("Render length in seconds.", required = FALSE),
    cps = ellmer::type_number("Cycles per second; defaults to the code's `cps` or 0.5.", required = FALSE)
  )
)

pattern_events <- ellmer::tool(
  function(code, cycles = 4, cps = NULL) {
    res <- exec_code(code)
    if (inherits(res, "error")) return(paste0("invalid: ", conditionMessage(res)))
    cps <- use_cps(cps, res$cps)
    haps <- tryCatch(query_arc(res$pattern, 0, cycles), error = function(e) e)
    if (inherits(haps, "error")) return(paste0("invalid: ", conditionMessage(haps)))
    events <- lapply(haps, function(h) {
      span <- if (!is.null(h$whole)) h$whole else h$part
      list(
        onset_sec = round(hap_onset(h) / cps, 4),
        dur_sec = round((span$end - span$begin) / cps, 4),
        value = h$value
      )
    })
    ## match the Python prototype: return the grid as a JSON array string
    ## (jsonlite ships with mcptools, so this adds no new dependency)
    if (length(events) == 0) "[]" else
      as.character(jsonlite::toJSON(events, auto_unbox = TRUE))
  },
  name = "pattern_events",
  description = "Return the pattern's event grid as JSON: [{onset_sec, dur_sec, value}].",
  arguments = list(
    code = ellmer::type_string("Pattern DSL source defining `pattern`."),
    cycles = ellmer::type_number("Number of cycles to query.", required = FALSE),
    cps = ellmer::type_number("Cycles per second; defaults to the code's `cps` or 0.5.", required = FALSE)
  )
)

list_voices <- ellmer::tool(
  function() {
    paste0(
      "drums (use {\"sound\": name}): ", paste(DRUM_NAMES, collapse = ", "), "\n",
      "synths (use {\"note\": \"c3\", \"s\": name}): ", paste(OSC_NAMES, collapse = ", "), "\n",
      "value modifiers: gain (multiplier), lpf (Hz, 0=off), attack (seconds)"
    )
  },
  name = "list_voices",
  description = "List the drum voices and synth oscillators available to patterns."
)

mcptools::mcp_server(
  tools = list(validate_pattern, render_pattern, pattern_events, list_voices),
  session_tools = FALSE,
  type = "stdio"
)
