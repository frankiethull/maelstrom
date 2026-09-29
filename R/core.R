## Core pattern model for maelstrom.
##
## A Pattern is a function from a TimeSpan (measured in cycles) to a list of
## Haps (events). This is the most idiomatic R way to express the idea: a
## pattern literally *is* a closure over time. Time is continuous; a hap
## carries the timespan it occupies within the query (`part`) and, when it
## represents a whole discrete event, the timespan of that whole event
## (`whole`). Values are plain named lists describing what to play, e.g.
## list(sound = "bd") or list(note = "c3", s = "sawtooth").

## -- TimeSpan -------------------------------------------------------------

#' Create a TimeSpan
#'
#' A TimeSpan is a half-open interval [begin, end) measured in cycles.
#'
#' @param begin Numeric start of the interval, in cycles.
#' @param end Numeric end of the interval, in cycles.
#' @return An `ms_timespan` object (a list with `begin` and `end`).
#' @examples
#' timespan(0, 1)
#' @export
timespan <- function(begin, end) {
  structure(list(begin = as.numeric(begin), end = as.numeric(end)),
            class = "ms_timespan")
}

#' Length of a TimeSpan
#'
#' Duration of a TimeSpan in cycles.
#'
#' @param ts A TimeSpan.
#' @return A numeric duration.
#' @examples
#' ts_duration(timespan(0.25, 1))
#' @export
ts_duration <- function(ts) ts$end - ts$begin

#' Intersect two TimeSpans
#'
#' The overlap of two TimeSpans, or NULL when they do not overlap.
#'
#' @param a A TimeSpan.
#' @param b A TimeSpan.
#' @return A TimeSpan or NULL.
#' @examples
#' ts_intersection(timespan(0, 1), timespan(0.5, 2))
#' @export
ts_intersection <- function(a, b) {
  begin <- max(a$begin, b$begin)
  end <- min(a$end, b$end)
  if (begin < end) timespan(begin, end) else NULL
}

#' Split a TimeSpan into equal parts
#'
#' Divide a TimeSpan into n contiguous equal TimeSpans.
#'
#' @param ts A TimeSpan.
#' @param n Number of parts.
#' @return A list of n TimeSpans.
#' @examples
#' ts_split(timespan(0, 1), 4)
#' @export
ts_split <- function(ts, n) {
  d <- ts_duration(ts)
  lapply(seq_len(n), function(i) timespan(ts$begin + d * (i - 1) / n,
                                          ts$begin + d * i / n))
}

## -- Hap ------------------------------------------------------------------

#' Create a Hap (event)
#'
#' A Hap is one event: the `whole` span of the discrete event (or NULL for continuous patterns), the `part` actually sounding inside the query, and a named list `value` describing what to play.
#'
#' @param whole TimeSpan of the whole event, or NULL.
#' @param part TimeSpan of the event inside the query.
#' @param value Named list describing the event, e.g. `list(sound = "bd")`.
#' @return An `ms_hap` object.
#' @examples
#' hap(timespan(0, 0.5), timespan(0, 0.5), list(sound = "bd"))
#' @export
hap <- function(whole, part, value = list()) {
  structure(list(whole = whole, part = part, value = value), class = "ms_hap")
}

#' Onset of a hap
#'
#' Start of the hap in cycles: the start of `whole` when present, else the start of `part`.
#'
#' @param h A hap.
#' @return A numeric onset in cycles.
#' @examples
#' hap_onset(hap(timespan(0.5, 1), timespan(0.5, 1), list()))
#' @export
hap_onset <- function(h) {
  if (!is.null(h$whole)) h$whole$begin else h$part$begin
}

#' Test whether a hap begins inside a span
#'
#' True when the hap onset lies in [span$begin, span$end).
#'
#' @param h A hap.
#' @param span A TimeSpan.
#' @return A logical value.
#' @examples
#' hap_begins_in(hap(timespan(0.5, 1), timespan(0.5, 1), list()), timespan(0, 1))
#' @export
hap_begins_in <- function(h, span) {
  t <- hap_onset(h)
  span$begin <= t && t < span$end
}

#' Test whether a hap is a rest
#'
#' A hap is a rest when its value is empty or has `rest = TRUE`.
#'
#' @param h A hap.
#' @return A logical value.
#' @examples
#' is_rest(hap(NULL, timespan(0, 1), list()))
#' @export
is_rest <- function(h) {
  v <- h$value
  length(v) == 0 || isTRUE(v$rest)
}

## -- Pattern --------------------------------------------------------------

## A pattern is a closure: function(span) -> list of haps.
## Named ms_pattern (not `pattern`) so users can freely name their own
## compositions `pattern` without shadowing the constructor.
#' Construct a Pattern from a query function
#'
#' A Pattern literally is a closure over time: a function from a TimeSpan to a list of haps. Named `ms_pattern` (not `pattern`) so users can name their own compositions `pattern` without shadowing it.
#'
#' @param query_fn A function taking a TimeSpan and returning a list of haps.
#' @return An `ms_pattern` object.
#' @examples
#' p <- ms_pattern(function(span) list())
#' query_arc(p, 0, 1)
#' @export
ms_pattern <- function(query_fn) {
  stopifnot(is.function(query_fn))
  structure(query_fn, class = "ms_pattern")
}

#' Layer two patterns
#'
#' Combine two patterns so the result sounds the haps of both.
#'
#' @param a A pattern.
#' @param b A pattern.
#' @return A pattern.
#' @examples
#' query_arc(pat_overlay(s("bd"), s("sd")), 0, 1)
#' @export
pat_overlay <- function(a, b) {
  ms_pattern(function(span) c(a(span), b(span)))
}

#' Transform hap values
#'
#' Apply a function to every hap value in a pattern.
#'
#' @param p A pattern.
#' @param fn A function from a value list to a value list.
#' @return A pattern.
#' @examples
#' query_arc(pat_with_value(s("bd"), function(v) { v$gain <- 0.5; v }), 0, 1)
#' @export
pat_with_value <- function(p, fn) {
  ms_pattern(function(span) {
    lapply(p(span), function(h) hap(h$whole, h$part, fn(h$value)))
  })
}

#' Filter haps by predicate
#'
#' Keep only the haps for which `pred` returns TRUE.
#'
#' @param p A pattern.
#' @param pred A function from a hap to a logical value.
#' @return A pattern.
#' @examples
#' query_arc(pat_filter(s("bd*4"), function(h) TRUE), 0, 1)
#' @export
pat_filter <- function(p, pred) {
  ms_pattern(function(span) Filter(pred, p(span)))
}

#' Query the haps beginning in a time range
#'
#' Run a pattern over [start, start + cycles) and return the haps whose onset falls inside, sorted by onset.
#'
#' @param p A pattern.
#' @param start Start in cycles.
#' @param cycles Number of cycles to query.
#' @return A list of haps, sorted by onset.
#' @examples
#' query_arc(s("bd*4"), 0, 1)
#' @export
query_arc <- function(p, start, cycles) {
  span <- timespan(start, start + cycles)
  haps <- Filter(function(h) hap_begins_in(h, span), p(span))
  if (length(haps) == 0) return(list())
  ord <- order(vapply(haps, hap_onset, numeric(1)))
  haps[ord]
}

## -- Constructors ----------------------------------------------------------

#' A pattern holding one value
#'
#' The value sounds across the whole query span (whole is NULL).
#'
#' @param value A named list describing the event.
#' @return A pattern.
#' @examples
#' query_arc(pure(list(sound = "bd")), 0, 1)
#' @export
pure <- function(value) {
  ms_pattern(function(span) list(hap(NULL, span, value)))
}

#' A pattern with no haps
#'
#' The empty pattern: queries always return an empty list.
#'
#' @return A pattern.
#' @examples
#' query_arc(silence(), 0, 1)
#' @export
silence <- function() {
  ms_pattern(function(span) list())
}

#' Cycle through a list of values
#'
#' Each cycle is split into equal steps, one per list element, in order.
#'
#' @param values A list of hap value lists, one per step.
#' @return A pattern.
#' @examples
#' query_arc(from_list(list(list(sound = "bd"), list(sound = "sd"))), 0, 1)
#' @export
from_list <- function(values) {
  n <- length(values)
  ms_pattern(function(span) {
    out <- list()
    for (cyc in seq(floor(span$begin), ceiling(span$end) - 1)) {
      cycle_span <- timespan(cyc, cyc + 1)
      parts <- ts_split(cycle_span, n)
      for (i in seq_len(n)) {
        part <- ts_intersection(parts[[i]], span)
        if (!is.null(part)) {
          out <- c(out, list(hap(parts[[i]], part, values[[i]])))
        }
      }
    }
    out
  })
}

## -- Euclidean rhythms -----------------------------------------------------

#' Bjorklund Euclidean rhythm
#'
#' Distribute `hits` pulses as evenly as possible over `steps` slots.
#'
#' @param hits Number of pulses.
#' @param steps Number of slots.
#' @return A logical vector of length `steps`.
#' @examples
#' bjorklund(3, 8)
#' @export
bjorklund <- function(hits, steps) {
  if (hits <= 0) return(rep(FALSE, steps))
  if (hits >= steps) return(rep(TRUE, steps))
  pat <- integer(0)
  counts <- integer(0)
  remainders <- hits
  divisor <- steps - hits
  level <- 1L
  repeat {
    counts <- c(counts, divisor %/% remainders[level])
    remainders <- c(remainders, divisor %% remainders[level])
    divisor <- remainders[level]
    level <- level + 1L
    if (remainders[level] <= 1) break
  }
  counts <- c(counts, divisor)
  build <- function(l) {
    if (l == 0) {
      pat <<- c(pat, 0L)
    } else if (l == -1) {
      pat <<- c(pat, 1L)
    } else {
      for (i in seq_len(counts[l])) build(l - 1)
      if (remainders[l] != 0) build(l - 2)
    }
  }
  build(level)
  first <- which(pat == 1L)[1]
  if (first > 1) {
    pat <- c(pat[first:length(pat)], pat[seq_len(first - 1)])
  }
  as.logical(pat)
}

## -- Deterministic randomness ----------------------------------------------
##
## Random musical choices (degrade, choice, sometimes) must be reproducible:
## the same pattern queried twice gives the same events, and no global RNG
## state is disturbed. We seed R's own RNG from a string hash inside a
## save/restore wrapper.

#' Hash a string to an integer seed
#'
#' Simple string hash used to key deterministic randomness.
#'
#' @param s A character string.
#' @return An integer seed.
#' @examples
#' str_seed("uzu:degrade:0")
#' @export
str_seed <- function(s) {
  h <- 0
  for (ch in utf8ToInt(s)) h <- (h * 31 + ch) %% 2147483647
  as.integer(h %% 2147483647)
}

#' Evaluate with R's RNG seeded
#'
#' Set the seed, evaluate `expr`, then restore the previous RNG state, so global randomness is never disturbed.
#'
#' @param seed An integer seed.
#' @param expr An expression to evaluate.
#' @return The value of `expr`.
#' @examples
#' with_seed(1, runif(1))
#' @export
with_seed <- function(seed, expr) {
  has <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old <- if (has) get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(as.integer(seed))
  on.exit({
    if (has) {
      assign(".Random.seed", old, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  force(expr)
}

## n uniform draws in [0, 1), deterministically keyed by `tag`.
#' Deterministic uniform draws
#'
#' `n` uniform draws in [0, 1), keyed by `tag`, without touching the global RNG state.
#'
#' @param n Number of draws.
#' @param tag A string key; same tag gives the same draws.
#' @return A numeric vector of length `n`.
#' @examples
#' uzu_runif(3, "demo")
#' @export
uzu_runif <- function(n, tag) {
  with_seed(str_seed(tag), stats::runif(n))
}
