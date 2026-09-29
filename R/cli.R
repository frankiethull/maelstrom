## Command-line interface for maelstrom.
##
##   maelstrom render FILE -o out.wav [-d seconds] [--cps CPS]
##   maelstrom validate FILE

#' Command-line interface
#'
#' Implements the `maelstrom` executable: `render FILE -o OUT [-d SECONDS] [--cps CPS] [--stereo] [--reverb AMT]` and `validate FILE`. The input file must define `pattern` (and may define `cps`). Returns a process exit code.
#'
#' @param args Character vector of command-line arguments.
#' @return An integer exit code (0 on success).
#' @examples
#' maelstrom_cli(character(0))
#' @export
maelstrom_cli <- function(args) {
  if (length(args) == 0) {
    base::cat("usage: maelstrom {render|validate} FILE [-o OUT] [-d SECONDS] [--cps CPS]\n")
    return(1L)
  }
  cmd <- args[1]
  rest <- args[-1]
  get_flag <- function(names, default = NULL) {
    for (i in seq_along(rest)) {
      if (rest[i] %in% names && i < length(rest)) return(rest[i + 1])
    }
    default
  }
  positionals <- rest[!grepl("^-", rest)]
  ## drop flag values from positionals
  vals <- c(get_flag(c("-o", "--output")), get_flag(c("-d", "--seconds")),
            get_flag(c("--cps")))
  positionals <- setdiff(positionals, vals)
  if (length(positionals) == 0) {
    base::cat("error: no input file given\n")
    return(2L)
  }
  file <- positionals[1]

  env <- new.env(parent = globalenv())
  ok <- tryCatch({
    sys.source(file, envir = env)
    TRUE
  }, error = function(e) {
    base::cat(paste0("error loading ", file, ": ", conditionMessage(e), "\n"))
    FALSE
  })
  if (!ok) return(2L)
  pat <- env$pattern
  if (is.null(pat) || !inherits(pat, "ms_pattern")) {
    base::cat(paste0("error: ", file, " must define `pattern` as a maelstrom pattern\n"))
    return(2L)
  }
  cps <- get_flag("--cps", NULL)
  cps <- if (!is.null(cps)) as.numeric(cps) else if (!is.null(env$cps)) as.numeric(env$cps) else 0.5

  if (cmd == "validate") {
    haps <- tryCatch(query_arc(pat, 0, 4), error = function(e) e)
    if (inherits(haps, "error")) {
      base::cat(paste0("error querying pattern in ", file, ": ", conditionMessage(haps), "\n"))
      return(1L)
    }
    n_rest <- sum(vapply(haps, is_rest, logical(1)))
    unknown <- unique(unlist(lapply(haps, function(h) {
      s <- h$value$sound
      if (!is.null(s) && !(s %in% names(DRUMS))) s else NULL
    })))
    base::cat(sprintf("ok: %d events over 4 cycles (%d rests), cps=%g\n",
                length(haps), n_rest, cps))
    if (length(unknown) > 0) {
      base::cat(sprintf("warning: unknown sounds: %s\n", paste(unknown, collapse = ", ")))
      return(1L)
    }
    return(0L)
  }

  if (cmd == "render") {
    output <- get_flag(c("-o", "--output"))
    if (is.null(output)) {
      base::cat("error: render needs -o/--output\n")
      return(2L)
    }
    seconds <- as.numeric(get_flag(c("-d", "--seconds"), "16"))
    cycles <- seconds * cps
    stereo <- any(rest %in% c("--stereo"))
    reverb <- get_flag(c("--reverb"), NULL)
    base::cat(sprintf("rendering %gs (%g cycles at %g cps)%s%s...\n", seconds, cycles, cps,
                      if (stereo) ", stereo" else "",
                      if (!is.null(reverb)) paste0(", reverb ", reverb) else ""))
    samples <- if (stereo) render_stereo(pat, cycles = cycles, cps = cps) else
      render(pat, cycles = cycles, cps = cps)
    if (!is.null(reverb)) {
      if (!rust_available()) {
        base::cat(paste0("note: Rust backend unavailable, using pure-R reverb (slower). ",
                         "Install cargo + rustc and reinstall for the fast path.\n"))
      }
      samples <- add_reverb(samples, amount = as.numeric(reverb))
    }
    write_wav(output, samples)
    peak <- if (length(samples)) max(abs(samples)) else 0
    base::cat(sprintf("wrote %s (%d samples, peak %.3f)\n", output, length(samples), peak))
    return(0L)
  }

  base::cat(paste0("unknown command: ", cmd, "\n"))
  2L
}
