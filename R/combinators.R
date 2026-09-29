## Pattern combinators for maelstrom.
##
## Standard live-coding transforms. A combinator takes a pattern (a closure
## over time) and returns a new pattern. Pure base R.
##
## The API is pipe-first: the pattern is always the first argument, so
## chains read like Strudel's method calls:
##
##   s("bd*4") |>
##     gain(0.8) |>
##     lpf(1200)
##
## stack(...) layers patterns, just like strudel:
##
##   stack(
##     s("bd*4"),
##     s("~ sd ~ sd"),
##     s("hh*16") |> gain(0.5) |> swing(0.4)
##   )

## -- helpers ---------------------------------------------------------------

#' Remap a pattern through time functions
#'
#' The workhorse behind fast/slow: query the source pattern over a remapped span, then map event times back.
#'
#' @param pat A pattern.
#' @param span_fn Maps a query TimeSpan to the source TimeSpan to query.
#' @param time_fn Maps a source TimeSpan back to query time.
#' @return A pattern.
#' @examples
#' query_arc(remap_pattern(s("bd*4"), function(sp) sp, function(t) t), 0, 1)
#' @export
remap_pattern <- function(pat, span_fn, time_fn) {
  ms_pattern(function(span) {
    out <- list()
    for (h in pat(span_fn(span))) {
      whole <- if (!is.null(h$whole)) time_fn(h$whole) else NULL
      part <- ts_intersection(time_fn(h$part), span)
      if (!is.null(part)) out <- c(out, list(hap(whole, part, h$value)))
    }
    out
  })
}

#' Apply a function per cycle
#'
#' Split the query into whole cycles and call `fn(pat, subspan, cycle)` for each.
#'
#' @param pat A pattern.
#' @param fn A function of (pattern, TimeSpan, cycle number).
#' @return A pattern.
#' @examples
#' query_arc(per_cycle(s("bd*4"), function(p, sub, cyc) p(sub)), 0, 2)
#' @export
per_cycle <- function(pat, fn) {
  ms_pattern(function(span) {
    out <- list()
    for (cyc in seq(floor(span$begin), ceiling(span$end) - 1)) {
      sub <- ts_intersection(timespan(cyc, cyc + 1), span)
      if (!is.null(sub)) out <- c(out, fn(pat, sub, cyc))
    }
    out
  })
}

## -- time transforms -------------------------------------------------------

#' Speed up a pattern
#'
#' Play the pattern `factor` times faster (more events per cycle).
#'
#' @param pat A pattern.
#' @param factor Speed factor.
#' @return A pattern.
#' @examples
#' query_arc(fast(s("bd"), 2), 0, 1)
#' @export
fast <- function(pat, factor) {
  remap_pattern(
    pat,
    function(s) timespan(s$begin * factor, s$end * factor),
    function(t) timespan(t$begin / factor, t$end / factor)
  )
}

#' Slow down a pattern
#'
#' Play the pattern `factor` times slower (fewer events per cycle).
#'
#' @param pat A pattern.
#' @param factor Slow factor.
#' @return A pattern.
#' @examples
#' query_arc(slow(s("bd*4"), 2), 0, 1)
#' @export
slow <- function(pat, factor) {
  remap_pattern(
    pat,
    function(s) timespan(s$begin / factor, s$end / factor),
    function(t) timespan(t$begin * factor, t$end * factor)
  )
}

#' Reverse each cycle
#'
#' Mirror event times within every cycle. Masks `base::rev` when the package is attached.
#'
#' @param pat A pattern.
#' @return A pattern.
#' @examples
#' query_arc(rev(s("bd sd")), 0, 1)
#' @export
rev <- function(pat) {
  ms_pattern(function(span) {
    out <- list()
    for (cyc in seq(floor(span$begin), ceiling(span$end) - 1)) {
      sub <- ts_intersection(timespan(cyc, cyc + 1), span)
      if (is.null(sub)) next
      inner <- timespan(cyc + (cyc + 1 - sub$end), cyc + (cyc + 1 - sub$begin))
      mirror <- function(t) timespan(2 * cyc + 1 - t$end, 2 * cyc + 1 - t$begin)
      for (h in pat(inner)) {
        whole <- if (!is.null(h$whole)) mirror(h$whole) else NULL
        part <- ts_intersection(mirror(h$part), span)
        if (!is.null(part)) out <- c(out, list(hap(whole, part, h$value)))
      }
    }
    out
  })
}

#' Alternate forward and reversed cycles
#'
#' Even cycles play forward, odd cycles play reversed.
#'
#' @param pat A pattern.
#' @return A pattern.
#' @examples
#' query_arc(palindrome(s("bd sd")), 0, 2)
#' @export
palindrome <- function(pat) {
  per_cycle(pat, function(p, sub, cyc) {
    if (cyc %% 2 == 1) rev(p)(sub) else p(sub)
  })
}

## -- combining --------------------------------------------------------------

#' Layer patterns
#'
#' Sound all input patterns at once. Masks `utils::stack` when the package is attached.
#'
#' @param ... One or more patterns.
#' @return A pattern.
#' @examples
#' query_arc(stack(s("bd*4"), s("~ sd")), 0, 1)
#' @export
stack <- function(...) {
  pats <- list(...)
  if (length(pats) == 0) return(silence())
  Reduce(pat_overlay, pats)
}

#' One pattern per cycle
#'
#' Rotate through the inputs: cycle 0 plays the first pattern, cycle 1 the second, and so on.
#'
#' @param ... One or more patterns.
#' @return A pattern.
#' @examples
#' query_arc(slowcat(s("bd*4"), s("sd*4")), 0, 2)
#' @export
slowcat <- function(...) {
  pats <- list(...)
  k <- length(pats)
  if (k == 0) return(silence())
  ms_pattern(function(span) {
    out <- list()
    for (cyc in seq(floor(span$begin), ceiling(span$end) - 1)) {
      sub <- ts_intersection(timespan(cyc, cyc + 1), span)
      if (is.null(sub)) next
      out <- c(out, pats[[cyc %% k + 1]](sub))
    }
    out
  })
}

## `cat` selects one complete pattern per cycle, rotating through the inputs.
cat <- slowcat

#' Arrange sections by bar count
#'
#' Alternating pattern / bar-count pairs, played in order: each pattern gets
#' one cycle per bar. `arrange(intro, 4, verse, 8)` is shorthand for
#' `slowcat(intro, intro, intro, intro, verse, ...)` with 8 verses.
#'
#' @param ... Alternating patterns and bar counts: `arrange(pat1, bars1, pat2, bars2)`.
#' @return A pattern lasting `sum(bars)` cycles.
#' @examples
#' arrange(s("bd*4"), 2, s("bd sd"), 2)
#' @export
arrange <- function(...) {
  args <- list(...)
  n <- length(args)
  if (n %% 2 != 0 || n == 0)
    stop("arrange() takes pattern/count pairs, e.g. arrange(intro, 4, verse, 8)")
  out <- list()
  for (i in seq(1, n, by = 2)) {
    pat <- args[[i]]
    bars <- args[[i + 1]]
    if (!is.function(pat)) stop("arrange() patterns must be patterns")
    if (!is.numeric(bars) || length(bars) != 1 || bars < 0 || bars != round(bars))
      stop("arrange() bar counts must be non-negative integers")
    if (bars > 0) out <- c(out, rep(list(pat), bars))
  }
  do.call(slowcat, out)
}

## `fastcat` squeezes every input into a single cycle, like `cat` did in the
## early Python prototype (where the name was wrong).
#' Squeeze each pattern into one cycle
#'
#' Every input is compressed into a single cycle, in order.
#'
#' @param ... One or more patterns.
#' @return A pattern.
#' @examples
#' query_arc(fastcat(s("bd*4"), s("sd*4")), 0, 1)
#' @export
fastcat <- function(...) {
  pats <- list(...)
  k <- length(pats)
  if (k == 0) return(silence())
  ms_pattern(function(span) {
    out <- list()
    for (slot in seq(floor(span$begin * k), ceiling(span$end * k) - 1)) {
      i <- slot %% k + 1L
      idx0 <- i - 1L
      outer_slot <- timespan(slot / k, (slot + 1) / k)
      sub <- ts_intersection(outer_slot, span)
      if (is.null(sub)) next
      inner <- timespan(sub$begin * k - idx0, sub$end * k - idx0)
      unmap <- function(t) timespan((t$begin + idx0) / k, (t$end + idx0) / k)
      for (h in pats[[i]](inner)) {
        whole <- if (!is.null(h$whole)) unmap(h$whole) else NULL
        part <- ts_intersection(unmap(h$part), span)
        if (!is.null(part)) out <- c(out, list(hap(whole, part, h$value)))
      }
    }
    out
  })
}

## -- conditional / probabilistic --------------------------------------------

#' Deterministic random draw for a cycle
#'
#' One uniform draw in [0, 1) keyed by cycle number (and an optional tag), used by the probabilistic combinators.
#'
#' @param cyc Cycle number.
#' @param extra Extra tag string to separate uses.
#' @return A numeric draw in [0, 1).
#' @examples
#' cycle_choice(3)
#' @export
cycle_choice <- function(cyc, extra = "") {
  uzu_runif(1, paste0("uzu", extra, ":", cyc))
}

#' Apply a transform every n cycles
#'
#' On cycles `n - 1`, `2n - 1`, ..., apply `fn` to the pattern; other cycles are untouched.
#'
#' @param pat A pattern.
#' @param n Period in cycles.
#' @param fn A function from pattern to pattern.
#' @return A pattern.
#' @examples
#' query_arc(every(s("bd*4"), 2, function(p) rev(p)), 0, 2)
#' @export
every <- function(pat, n, fn) {
  per_cycle(pat, function(p, sub, cyc) {
    if (cyc %% n == n - 1) fn(p)(sub) else p(sub)
  })
}

#' Apply a transform with a per-cycle probability
#'
#' Each cycle independently applies `fn` with probability `prob`, deterministically.
#'
#' @param pat A pattern.
#' @param prob Probability in [0, 1].
#' @param fn A function from pattern to pattern.
#' @return A pattern.
#' @examples
#' query_arc(sometimes_by(s("bd*4"), 0.5, function(p) rev(p)), 0, 2)
#' @export
sometimes_by <- function(pat, prob, fn) {
  per_cycle(pat, function(p, sub, cyc) {
    if (cycle_choice(cyc, "sometimes") < prob) fn(p)(sub) else p(sub)
  })
}

#' Apply a transform on about half the cycles
#'
#' `sometimes_by` with probability 0.5.
#'
#' @param pat A pattern.
#' @param fn A function from pattern to pattern.
#' @return A pattern.
#' @examples
#' query_arc(sometimes(s("bd*4"), function(p) rev(p)), 0, 2)
#' @export
sometimes <- function(pat, fn) sometimes_by(pat, 0.5, fn)
#' Apply a transform on most cycles
#'
#' `sometimes_by` with probability 0.75.
#'
#' @param pat A pattern.
#' @param fn A function from pattern to pattern.
#' @return A pattern.
#' @examples
#' query_arc(often(s("bd*4"), function(p) rev(p)), 0, 2)
#' @export
often <- function(pat, fn) sometimes_by(pat, 0.75, fn)
#' Apply a transform on few cycles
#'
#' `sometimes_by` with probability 0.25.
#'
#' @param pat A pattern.
#' @param fn A function from pattern to pattern.
#' @return A pattern.
#' @examples
#' query_arc(rarely(s("bd*4"), function(p) rev(p)), 0, 4)
#' @export
rarely <- function(pat, fn) sometimes_by(pat, 0.25, fn)

#' Randomly drop events
#'
#' Remove each event independently with probability `amount`, deterministically per onset.
#'
#' @param pat A pattern.
#' @param amount Drop probability in [0, 1], default 0.5.
#' @return A pattern.
#' @examples
#' query_arc(degrade(s("bd*8"), 0.5), 0, 1)
#' @export
degrade <- function(pat, amount = 0.5) {
  keep <- function(h) {
    uzu_runif(1, sprintf("uzu:degrade:%.9f", hap_onset(h))) >= amount
  }
  pat_filter(pat, keep)
}

#' Randomly drop events by an amount
#'
#' Alias of `degrade` with the amount required.
#'
#' @param pat A pattern.
#' @param amount Drop probability in [0, 1].
#' @return A pattern.
#' @examples
#' query_arc(degrade_by(s("bd*8"), 0.25), 0, 1)
#' @export
degrade_by <- function(pat, amount) degrade(pat, amount)

## -- rhythmic masks ----------------------------------------------------------

#' Euclidean rhythmic mask
#'
#' Keep only the events whose onset falls on a `bjorklund(hits, steps)` pulse, optionally rotated.
#'
#' @param pat A pattern.
#' @param hits Number of pulses.
#' @param steps Number of slots.
#' @param rotation Rotate the pulse pattern, default 0.
#' @return A pattern.
#' @examples
#' query_arc(euclid(s("bd*8"), 3, 8), 0, 1)
#' @export
euclid <- function(pat, hits, steps, rotation = 0) {
  pulses <- bjorklund(hits, steps)
  if (rotation != 0) {
    rotation <- rotation %% steps
    pulses <- c(pulses[(steps - rotation + 1):steps], pulses[1:(steps - rotation)])
  }
  keep <- function(h) {
    idx <- as.integer((hap_onset(h) %% 1) * steps) %% steps + 1L
    pulses[idx]
  }
  pat_filter(pat, keep)
}

#' Binary step mask
#'
#' Keep events on steps marked x, t, 1 or * in a space-separated mask like `"x . x ."`.
#'
#' @param pat A pattern.
#' @param binary Space-separated step mask.
#' @return A pattern.
#' @examples
#' query_arc(struct(s("bd*8"), "x . x . x . x ."), 0, 1)
#' @export
struct <- function(pat, binary) {
  tokens <- strsplit(binary, " ", fixed = TRUE)[[1]]
  steps <- tolower(tokens) %in% c("x", "t", "1", "*")
  n <- length(steps)
  if (n == 0) return(silence())
  keep <- function(h) {
    idx <- as.integer((hap_onset(h) %% 1) * n) %% n + 1L
    steps[idx]
  }
  pat_filter(pat, keep)
}

## -- value modifiers ----------------------------------------------------------

#' Modify a numeric hap value
#'
#' Apply `fn` to the numeric value stored under `key` (or `default` when unset).
#'
#' @param pat A pattern.
#' @param key Value key, e.g. "gain".
#' @param fn A function from numeric to numeric.
#' @param default Default when the key is unset.
#' @return A pattern.
#' @examples
#' query_arc(modify_num(s("bd"), "gain", function(g) g * 2, 1), 0, 1)
#' @export
modify_num <- function(pat, key, fn, default) {
  pat_with_value(pat, function(v) {
    cur <- v[[key]]
    if (is.null(cur)) cur <- default
    v[[key]] <- fn(as.numeric(cur))
    v
  })
}

#' Default for numeric value keys
#'
#' The default used when a numeric key is unset: 1.0 for gain, 0.5 for pan, 0.0 otherwise.
#'
#' @param key Value key.
#' @return A numeric default.
#' @examples
#' num_default("gain")
#' @export
num_default <- function(key) {
  switch(key, gain = 1.0, pan = 0.5, 0.0)
}

#' Scale event gains
#'
#' Multiply every event gain by `x`.
#'
#' @param pat A pattern.
#' @param x Gain multiplier.
#' @return A pattern.
#' @examples
#' s("bd*4") |> gain(0.8)
#' @export
gain <- function(pat, x) modify_num(pat, "gain", function(g) g * x, num_default("gain"))
## lpf takes a static Hz cutoff, a mini-notation string, or a pattern of
## numbers (e.g. lpf(n("400 800 1600 3200"))). Strings are parsed eagerly so
## typos fail at composition time; the cutoff is resolved per event at render.
#' Lowpass filter cutoff
#'
#' Set a per-event lowpass cutoff. `x` is a static Hz value, a mini-notation string (parsed eagerly, so typos fail at composition time), or a pattern of numbers such as `n("400 800 1600 3200")`; patterns are resolved per event at render.
#'
#' @param pat A pattern.
#' @param x Cutoff in Hz, a mini-notation string, or a pattern of numbers.
#' @return A pattern.
#' @examples
#' s("bd*4") |> lpf(1200)
#' @export
lpf <- function(pat, x) {
  if (is.character(x)) x <- n(x)
  pat_with_value(pat, function(v) { v$lpf <- x; v })
}
## kit("acoustic") selects the acoustic drum kit per event; "synth" (the
## default) keeps the original synthesized kit. Unknown names are an error.
#' Select the drum kit
#'
#' `"synth"` (the default) keeps the original synthesized kit; `"acoustic"` selects the acoustic-style kit per event. Voices missing from the acoustic kit fall back to the synth kit. Unknown names are an error.
#'
#' @param pat A pattern.
#' @param name Kit name: "synth" or "acoustic".
#' @return A pattern.
#' @examples
#' s("bd sd") |> kit("acoustic")
#' @export
kit <- function(pat, name) {
  name <- as.character(name)
  if (!(name %in% c("synth", "acoustic"))) stop(paste("unknown kit:", name))
  pat_with_value(pat, function(v) { v$kit <- name; v })
}
#' Set the attack time
#'
#' Attack time in seconds for synth voices.
#'
#' @param pat A pattern.
#' @param x Attack in seconds.
#' @return A pattern.
#' @examples
#' note("c4") |> attack(0.05)
#' @export
attack <- function(pat, x) {
  pat_with_value(pat, function(v) { v$attack <- x; v })
}
#' Set the oscillator kind
#'
#' Voice for note events: "sine", "sawtooth", "square", "triangle", "noise", or one of the built voices "pluck", "pad", "sub", "lead".
#'
#' @param pat A pattern.
#' @param name Oscillator name.
#' @return A pattern.
#' @examples
#' note("c3") |> synth("sawtooth")
#' @export
synth <- function(pat, name) {
  pat_with_value(pat, function(v) { v$s <- name; v })
}

## Chord: stack note patterns so they sound together.
##   chord("c4", "eb4", "g4") |> synth("sawtooth") |> lpf(1200)
#' Stack note names as a chord
#'
#' Each name becomes a `note()` pattern; all sound together.
#'
#' @param ... Note names like "c4", "eb4", "g4".
#' @return A pattern.
#' @examples
#' query_arc(chord("c4", "eb4", "g4"), 0, 1)
#' @export
chord <- function(...) {
  do.call(stack, lapply(list(...), note))
}

## -- space and groove ----------------------------------------------------------

#' Drop NULLs from a list
#'
#' Remove NULL elements, keeping everything else in order.
#'
#' @param x A list.
#' @return A list with no NULL elements.
#' @examples
#' compact(list(1, NULL, 2))
#' @export
compact <- function(x) Filter(Negate(is.null), x)

#' Stereo pan
#'
#' Pan position per event: 0 is hard left, 1 is hard right, 0.5 is center.
#'
#' @param pat A pattern.
#' @param x Pan position in [0, 1].
#' @return A pattern.
#' @examples
#' s("hh*8") |> pan(0.2)
#' @export
pan <- function(pat, x) {
  pat_with_value(pat, function(v) { v$pan <- x; v })
}

## Shift a pattern in time by dt cycles, scaling event gains.
#' Shift a pattern in time
#'
#' Move every event `dt` cycles later, scaling gains by `gain_mul`.
#'
#' @param pat A pattern.
#' @param dt Shift in cycles.
#' @param gain_mul Gain multiplier, default 1.
#' @return A pattern.
#' @examples
#' query_arc(pat_shift(s("bd"), 0.5), 0, 1)
#' @export
pat_shift <- function(pat, dt, gain_mul = 1) {
  ms_pattern(function(span) {
    qspan <- timespan(span$begin - dt, span$end - dt)
    compact(lapply(pat(qspan), function(h) {
      whole <- if (!is.null(h$whole)) timespan(h$whole$begin + dt, h$whole$end + dt) else NULL
      part <- ts_intersection(timespan(h$part$begin + dt, h$part$end + dt), span)
      if (is.null(part)) return(NULL)
      v <- h$value
      v$gain <- num_or(v$gain, 1.0) * gain_mul
      hap(whole, part, v)
    }))
  })
}

## Echo: n repeats, each `time` cycles later, decaying by fb per repeat.
##   stab |> echo(n = 3, time = 0.375, fb = 0.45)
#' Decaying repeats
#'
#' Overlay `n` repeats, each `time` cycles later and quieter by `fb` per repeat.
#'
#' @param pat A pattern.
#' @param n Number of repeats, default 3.
#' @param time Delay between repeats in cycles, default 0.25.
#' @param fb Gain decay per repeat, default 0.5.
#' @return A pattern.
#' @examples
#' query_arc(echo(s("bd"), 2, 0.5, 0.5), 0, 2)
#' @export
echo <- function(pat, n = 3, time = 0.25, fb = 0.5) {
  reps <- lapply(0:n, function(k) pat_shift(pat, k * time, fb^k))
  Reduce(pat_overlay, reps)
}

## Swing: push odd `division`-ths later by amount * one slot.
##   s("hh*16") |> swing(0.4)
#' Swing
#'
#' Push odd `division`-ths later by `amount` times one slot.
#'
#' @param pat A pattern.
#' @param amount Swing amount, 0 is straight.
#' @param division Slots per cycle, default 16.
#' @return A pattern.
#' @examples
#' s("hh*16") |> swing(0.4)
#' @export
swing <- function(pat, amount, division = 16) {
  ms_pattern(function(span) {
    compact(lapply(pat(span), function(h) {
      pos <- (hap_onset(h) - floor(hap_onset(h))) * division
      idx <- round(pos)
      if (abs(pos - idx) < 1e-6 && idx %% 2 == 1) {
        dt <- amount / division
        whole <- if (!is.null(h$whole)) timespan(h$whole$begin + dt, h$whole$end + dt) else NULL
        part <- ts_intersection(timespan(h$part$begin + dt, h$part$end + dt), span)
        if (is.null(part)) return(NULL)
        hap(whole, part, h$value)
      } else {
        h
      }
    }))
  })
}

## -- insert effects ----------------------------------------------------------
##
## drive, crush, and chorus set a per-event amount; render_hap applies it in
## R/audio.R after the voice renders. Amounts are static (validated here),
## unlike lpf which also accepts patterns.

#' Saturation drive
#'
#' Soft saturation per event: `tanh(drive * x) / tanh(drive)`, normalized so the peak stays near 1.
#'
#' @param pat A pattern.
#' @param amount Drive amount; 0 is clean, 1 is gentle, 5 is heavy. Default 2.
#' @return A pattern.
#' @examples
#' s("bd*4") |> drive(2)
#' @export
drive <- function(pat, amount = 2) {
  if (!is.numeric(amount) || length(amount) != 1 || amount < 0)
    stop("drive amount must be a single non-negative number")
  pat_with_value(pat, function(v) { v$drive <- amount; v })
}

#' Bitcrush
#'
#' Quantize event amplitudes to `bits` bits per event.
#'
#' @param pat A pattern.
#' @param bits Bit depth in [1, 16]. Default 8.
#' @return A pattern.
#' @examples
#' note("c3") |> synth("sawtooth") |> crush(4)
#' @export
crush <- function(pat, bits = 8) {
  if (!is.numeric(bits) || length(bits) != 1 || bits < 1 || bits > 16)
    stop("crush bits must be a single number in [1, 16]")
  pat_with_value(pat, function(v) { v$crush <- bits; v })
}

#' Chorus
#'
#' Add a modulated short delay per event: 12 ms plus an LFO between 0 and `depth` ms.
#'
#' @param pat A pattern.
#' @param depth LFO depth in ms. Default 6.
#' @return A pattern.
#' @examples
#' note("c4") |> synth("pad") |> chorus(6)
#' @export
chorus <- function(pat, depth = 6) {
  if (!is.numeric(depth) || length(depth) != 1 || depth < 0)
    stop("chorus depth must be a single non-negative number (ms)")
  pat_with_value(pat, function(v) { v$chorus <- depth; v })
}

## -- musical helpers ----------------------------------------------------------

#' MIDI number to note name
#'
#' Inverse of `note_to_midi`: 69 becomes "a4". Uses sharps.
#'
#' @param midi A MIDI number.
#' @return A note name like "c4".
#' @examples
#' midi_to_note(69)
#' @export
midi_to_note <- function(midi) {
  midi <- as.integer(round(midi))
  names <- c("c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b")
  paste0(names[midi %% 12 + 1L], midi %/% 12 - 1L)
}

#' Transpose
#'
#' Shift every note or `n` value by `semitones` semitones.
#'
#' @param pat A pattern.
#' @param semitones Semitones to shift, may be negative.
#' @return A pattern.
#' @examples
#' note("c4") |> transpose(7)
#' @export
transpose <- function(pat, semitones) {
  if (!is.numeric(semitones) || length(semitones) != 1)
    stop("semitones must be a single number")
  pat_with_value(pat, function(v) {
    if (!is.null(v[["n"]]))
      v[["n"]] <- if (is.list(v[["n"]])) lapply(v[["n"]], function(x) x + semitones)
             else v[["n"]] + semitones
    if (!is.null(v[["note"]])) {
      tr <- function(x) midi_to_note(note_to_midi(as.character(x)) + semitones)
      v[["note"]] <- if (is.list(v[["note"]])) lapply(v[["note"]], tr) else tr(v[["note"]])
    }
    v
  })
}

## arp: split simultaneous chord tones into equal time slices, low to high.
## Only groups whose haps share one whole span and carry note/n values are
## arpeggiated; everything else passes through untouched.
arp_pitch <- function(v) {
  if (!is.null(v[["note"]])) {
    x <- v[["note"]]
    if (is.list(x)) x <- x[[1]]
    return(note_to_midi(as.character(x)))
  }
  x <- v[["n"]]
  if (is.list(x)) x <- x[[1]]
  as.numeric(x)
}

arp_group_ok <- function(g) {
  wholes <- lapply(g, function(h) h$whole)
  if (any(vapply(wholes, is.null, logical(1)))) return(FALSE)
  b <- wholes[[1]]$begin
  e <- wholes[[1]]$end
  same <- all(vapply(wholes, function(w) w$begin == b && w$end == e, logical(1)))
  if (!same) return(FALSE)
  all(vapply(g, function(h) {
    v <- h$value
    !is_rest(h) && is.null(v[["sound"]]) &&
      (!is.null(v[["note"]]) || !is.null(v[["n"]]))
  }, logical(1)))
}

arp_group <- function(g) {
  w <- g[[1]]$whole
  k <- length(g)
  g <- g[order(vapply(g, function(h) arp_pitch(h$value), numeric(1)))]
  dur <- (w$end - w$begin) / k
  compact(lapply(seq_along(g), function(i) {
    h <- g[[i]]
    whole <- timespan(w$begin + (i - 1) * dur, w$begin + i * dur)
    part <- ts_intersection(whole, h$part)
    if (is.null(part)) return(NULL)
    hap(whole, part, h$value)
  }))
}

#' Arpeggiate chords
#'
#' Split simultaneous chord tones into equal time slices, low to high. Only note events that share one span are split; single notes and drums pass through untouched.
#'
#' @param pat A pattern.
#' @return A pattern.
#' @examples
#' query_arc(arp(chord("c4", "e4", "g4")), 0, 1)
#' @export
arp <- function(pat) {
  ms_pattern(function(span) {
    hs <- pat(span)
    if (length(hs) == 0) return(list())
    keys <- vapply(hs, function(h) sprintf("%.9f", hap_onset(h)), character(1))
    out <- list()
    for (k in unique(keys)) {
      g <- hs[keys == k]
      out <- c(out, if (length(g) > 1 && arp_group_ok(g)) arp_group(g) else g)
    }
    out
  })
}

#' Humanize
#'
#' Deterministic per-event timing and velocity jitter: onsets move by up to `timing` cycles either way, gains scale by up to `velocity` proportionally. Keyed by event position, so the same pattern always humanizes the same way.
#'
#' @param pat A pattern.
#' @param timing Max timing shift in cycles either way, default 0.005.
#' @param velocity Max proportional gain change either way, default 0.1.
#' @return A pattern.
#' @examples
#' s("bd*4") |> humanize(0.005, 0.1)
#' @export
humanize <- function(pat, timing = 0.005, velocity = 0.1) {
  if (!is.numeric(timing) || length(timing) != 1 || timing < 0)
    stop("timing must be a single non-negative number (cycles)")
  if (!is.numeric(velocity) || length(velocity) != 1 || velocity < 0)
    stop("velocity must be a single non-negative number (proportion)")
  ms_pattern(function(span) {
    ## Widen the query so events just outside the window that jitter into it
    ## are not missed (same idea as pat_shift). An event whose jittered onset
    ## lands outside the window is attributed to the neighboring window by
    ## query_arc, consistent with swing at the right edge.
    qspan <- timespan(span$begin - timing, span$end + timing)
    compact(lapply(pat(qspan), function(h) {
      if (is_rest(h)) return(h)
      key <- sprintf("maelstrom:humanize:%.9f:%.9f", h$part$begin, h$part$end)
      u <- uzu_runif(2, key)
      dt <- (u[1] - 0.5) * 2 * timing
      v <- h$value
      v[["gain"]] <- max(num_or(v[["gain"]], 1.0) * (1 + (u[2] - 0.5) * 2 * velocity), 1e-3)
      whole <- if (!is.null(h$whole))
        timespan(h$whole$begin + dt, h$whole$end + dt) else NULL
      part <- ts_intersection(timespan(h$part$begin + dt, h$part$end + dt), span)
      if (is.null(part)) return(NULL)
      hap(whole, part, v)
    }))
  })
}
