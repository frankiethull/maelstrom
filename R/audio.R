## Offline audio renderer for maelstrom.
##
## Renders a Pattern to a mono WAV file using synthesized voices only:
## oscillators and filtered noise, no samples, no audio assets, everything
## MIT-clean. Base R only; vectorized throughout.
##
## Value conventions produced by the mini parser / combinators:
##   list(sound = "bd")              drum voice
##   list(note = "c3", s = "sawtooth") synth voice (note may be a list: chord)
##   list(n = 60)                      synth voice by MIDI number
## Modifiers: gain, attack, lpf.

SR <- 44100

#' Note name to MIDI number
#'
#' Parse names like "a4", "c#3", "eb2" ("s" also means sharp, "b" means flat; default octave 3).
#'
#' @param name A note name.
#' @return An integer MIDI number.
#' @examples
#' note_to_midi("a4")
#' @export
note_to_midi <- function(name) {
  name <- tolower(trimws(name))
  if (!nzchar(name)) stop("empty note name")
  letter <- substr(name, 1, 1)
  offsets <- c(c = 0L, d = 2L, e = 4L, f = 5L, g = 7L, a = 9L, b = 11L)
  if (!(letter %in% names(offsets))) stop(paste("bad note name:", name))
  rest <- substr(name, 2, nchar(name))
  accidental <- 0L
  first <- substr(rest, 1, 1)
  if (first %in% c("#", "s")) {
    accidental <- 1L; rest <- substr(rest, 2, nchar(rest))
  } else if (first == "b") {
    accidental <- -1L; rest <- substr(rest, 2, nchar(rest))
  }
  octave <- if (nzchar(rest)) as.integer(rest) else 3L
  12L * (octave + 1L) + offsets[[letter]] + accidental
}

#' MIDI number to frequency
#'
#' Equal-tempered frequency in Hz (A4 = 440).
#'
#' @param midi A MIDI number.
#' @return A frequency in Hz.
#' @examples
#' midi_to_freq(69)
#' @export
midi_to_freq <- function(midi) 440 * 2^((midi - 69) / 12)

#' Numeric value or default
#'
#' Return `x` as numeric, or `default` when `x` is NULL.
#'
#' @param x A value or NULL.
#' @param default Fallback.
#' @return A numeric value.
#' @examples
#' num_or(NULL, 0.5)
#' @export
num_or <- function(x, default) {
  if (is.null(x)) default else as.numeric(x)
}

## Pull a single Hz value out of a cutoff pattern's hap value: n() patterns
## carry list(n = ...); anything else numeric falls back to its first number.
#' First number from a hap value
#'
#' Pull a single Hz value out of a cutoff pattern's hap value: `n()` patterns carry `list(n = ...)`; anything else numeric falls back to its first number.
#'
#' @param v A hap value list.
#' @return A numeric value.
#' @examples
#' num_from_value(list(n = 3))
#' @export
num_from_value <- function(v) {
  if (!is.null(v$n)) return(as.numeric(v$n)[1])
  nums <- vapply(v, function(e) is.numeric(e) && length(e) >= 1, logical(1))
  if (any(nums)) return(as.numeric(v[[which(nums)[1]]])[1])
  0
}

## Resolve an lpf value to a static Hz cutoff for one event onset. Accepts a
## static number (the historical behavior), a mini-notation string, or a
## pattern of numbers such as n("400 800 1600 3200").
#' Resolve an lpf value to Hz for one event
#'
#' Accepts a static number (the historical behavior), a mini-notation string, or a pattern of numbers such as `n("400 800 1600 3200")`; patterns are queried at the event onset.
#'
#' @param x An lpf value.
#' @param onset Event onset in cycles.
#' @return A cutoff in Hz (0 means no filtering).
#' @examples
#' resolve_lpf(1200, 0)
#' @export
resolve_lpf <- function(x, onset) {
  if (is.null(x)) return(0)
  if (is.numeric(x)) return(as.numeric(x)[1])
  pat <- if (is.character(x)) n(x) else if (is.function(x)) x else NULL
  if (is.null(pat)) return(0)
  span <- timespan(onset, onset + 1e-6)
  hs <- Filter(function(h) hap_begins_in(h, span), pat(span))
  if (length(hs) == 0) return(0)
  num_from_value(hs[[1]]$value)
}

## Deterministic standard normals without touching the global RNG.
#' Deterministic standard normals
#'
#' Box-Muller normals keyed by `tag`, without touching the global RNG state.
#'
#' @param n Number of draws.
#' @param tag A string key.
#' @return A numeric vector of length `n`.
#' @examples
#' uzu_rnorm(4, "demo")
#' @export
uzu_rnorm <- function(n, tag) {
  m <- ceiling(n / 2)
  u1 <- pmax(uzu_runif(m, paste0(tag, ":u1")), 1e-12)
  u2 <- uzu_runif(m, paste0(tag, ":u2"))
  r <- sqrt(-2 * log(u1))
  th <- 2 * pi * u2
  z <- as.vector(rbind(r * cos(th), r * sin(th)))
  z[seq_len(n)]
}

#' Amplitude envelope
#'
#' Exponential decay with a linear attack ramp.
#'
#' @param n Number of samples.
#' @param attack Attack time in seconds.
#' @param decay Decay time constant in seconds.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of length `n`.
#' @examples
#' envelope(100, 0.01, 0.1)
#' @export
envelope <- function(n, attack, decay, sr = SR) {
  tt <- seq(0, n - 1) / sr
  env <- exp(-tt / max(decay, 1e-4))
  a <- as.integer(attack * sr)
  if (a > 0 && a < n) {
    env[seq_len(a)] <- env[seq_len(a)] * seq(0, 1, length.out = a)
  }
  env
}

#' Oscillator
#'
#' One cycle-free waveform: "sine", "sawtooth", "square", "triangle", or "noise".
#'
#' @param kind Oscillator kind.
#' @param freq Frequency in Hz.
#' @param n Number of samples.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of length `n`.
#' @examples
#' osc("sine", 440, 100)
#' @export
osc <- function(kind, freq, n, sr = SR) {
  tt <- seq(0, n - 1) / sr
  phase <- 2 * pi * freq * tt
  switch(kind,
    sine = sin(phase),
    sawtooth = 2 * ((freq * tt) %% 1) - 1,
    square = sign(sin(phase)),
    triangle = 2 * abs(2 * ((freq * tt) %% 1) - 1) - 1,
    noise = uzu_rnorm(n, "maelstrom:osc:noise"),
    stop(paste("unknown osc kind:", kind))
  )
}

#' One-pole lowpass filter
#'
#' y[i] = alpha * x[i] + (1 - alpha) * y[i-1], run in C via `stats::filter`.
#'
#' @param x Input signal.
#' @param cutoff Cutoff in Hz; non-positive returns `x` unchanged.
#' @param sr Sample rate, default 44100.
#' @return The filtered signal.
#' @examples
#' lowpass(c(1, rep(0, 99)), 1000)
#' @export
lowpass <- function(x, cutoff, sr = SR) {
  if (cutoff <= 0) return(x)
  rc <- 1 / (2 * pi * cutoff)
  alpha <- 1 / (1 + rc * sr)
  ## one-pole: y[i] = alpha * x[i] + (1 - alpha) * y[i-1], in C via stats::filter
  as.vector(stats::filter(alpha * x, filter = 1 - alpha, method = "recursive"))
}

#' Highpass filter
#'
#' Highpass via lowpass subtraction: x - lowpass(x).
#'
#' @param x Input signal.
#' @param cutoff Cutoff in Hz.
#' @param sr Sample rate, default 44100.
#' @return The filtered signal.
#' @examples
#' highpass(c(1, rep(0, 99)), 1000)
#' @export
highpass <- function(x, cutoff, sr = SR) x - lowpass(x, cutoff, sr)

#' Add two signals
#'
#' Sum two vectors, padding the shorter with zeros.
#'
#' @param a A numeric vector.
#' @param b A numeric vector.
#' @return The summed vector.
#' @examples
#' mix(c(1, 2), c(1))
#' @export
mix <- function(a, b) {
  n <- max(length(a), length(b))
  out <- numeric(n)
  out[seq_along(a)] <- out[seq_along(a)] + a
  out[seq_along(b)] <- out[seq_along(b)] + b
  out
}

## -- drum voices (all synthesized) ----------------------------------------------

#' Synthesized kick drum
#'
#' Sine with a 150 to 50 Hz pitch drop and a fast decay.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_bd(0.2, 0.8)
#' @export
voice_bd <- function(dur, gain) {
  n <- as.integer(dur * SR)
  tt <- seq(0, n - 1) / SR
  freq <- 50 + 100 * exp(-tt / 0.03)  # pitch drop 150 -> 50 Hz
  phase <- 2 * pi * cumsum(freq) / SR
  sin(phase) * exp(-tt / 0.12) * gain
}

#' Synthesized snare drum
#'
#' 180 Hz tone plus highpassed noise.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_sd(0.2, 0.8)
#' @export
voice_sd <- function(dur, gain) {
  n <- as.integer(dur * SR)
  tt <- seq(0, n - 1) / SR
  tone <- sin(2 * pi * 180 * tt) * exp(-tt / 0.08)
  noise <- highpass(uzu_rnorm(n, "maelstrom:sd") * exp(-tt / 0.12), 1500)
  (tone * 0.6 + noise * 0.5) * gain
}

## Hat-family voices are trimmed internally so they sit under the kick and
## snare in a mix: highpassed noise otherwise peaks several dB hotter than
## the kick at the same gain. Trims land hats ~7-9 dB below the kick.
HH_LEVEL <- 0.25
SHAKER_LEVEL <- 0.3

#' Synthesized hi-hat
#'
#' Highpassed noise with a fast (closed) or long (open) decay, trimmed internally to sit under the kick.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @param open Open hat when TRUE, default FALSE.
#' @return A numeric vector of samples.
#' @examples
#' voice_hh(0.1, 0.8)
#' @export
voice_hh <- function(dur, gain, open = FALSE) {
  n <- as.integer(max(dur, if (open) 0.3 else 0.05) * SR)
  tt <- seq(0, n - 1) / SR
  noise <- highpass(uzu_rnorm(n, "maelstrom:hh"), 7000)
  decay <- if (open) 0.25 else 0.03
  noise * exp(-tt / decay) * gain * HH_LEVEL
}

#' Synthesized clap
#'
#' Bandpassed noise with three bursts plus a tail.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_cp(0.25, 0.8)
#' @export
voice_cp <- function(dur, gain) {
  n <- as.integer(max(dur, 0.25) * SR)
  tt <- seq(0, n - 1) / SR
  noise <- lowpass(highpass(uzu_rnorm(n, "maelstrom:cp"), 1000), 4000)
  bursts <- numeric(n)
  for (at in c(0.0, 0.012, 0.024)) {
    i <- as.integer(at * SR) + 1L
    if (i <= n) bursts[i:n] <- bursts[i:n] + exp(-(tt[i:n] - at) / 0.03)
  }
  tail <- exp(-tt / 0.09)
  noise * pmin(bursts + tail * 0.4, 1.5) * gain * 0.7
}

#' Synthesized tom
#'
#' Sine with a 220 to 90 Hz pitch drop.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_tom(0.25, 0.8)
#' @export
voice_tom <- function(dur, gain) {
  n <- as.integer(max(dur, 0.25) * SR)
  tt <- seq(0, n - 1) / SR
  freq <- 90 + 130 * exp(-tt / 0.05)  # pitch drop 220 -> 90 Hz
  phase <- 2 * pi * cumsum(freq) / SR
  sin(phase) * exp(-tt / 0.18) * gain
}

#' Synthesized rim click
#'
#' 900 Hz square click plus highpassed noise.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_rim(0.1, 0.8)
#' @export
voice_rim <- function(dur, gain) {
  n <- as.integer(max(dur, 0.05) * SR)
  tt <- seq(0, n - 1) / SR
  click <- sign(sin(2 * pi * 900 * tt)) * exp(-tt / 0.015)
  noise <- highpass(uzu_rnorm(n, "maelstrom:rim"), 4000) * exp(-tt / 0.02)
  (click * 0.5 + noise * 0.4) * gain
}

#' Synthesized shaker
#'
#' Bandpassed noise with a fast decay, trimmed internally like the hats.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_shaker(0.15, 0.8)
#' @export
voice_shaker <- function(dur, gain) {
  n <- as.integer(max(dur, 0.12) * SR)
  tt <- seq(0, n - 1) / SR
  noise <- highpass(lowpass(uzu_rnorm(n, "maelstrom:shaker"), 12000), 5000)
  noise * exp(-tt / 0.07) * gain * SHAKER_LEVEL
}

## Cymbal-family trims, same idea as HH_LEVEL: metallic voices peak hotter
## than the kick at the same gain, so they are trimmed internally.
RIDE_LEVEL <- 0.35
CRASH_LEVEL <- 0.4

#' Synthesized ride cymbal
#'
#' Inharmonic metallic partials plus highpassed noise with a medium decay, trimmed internally like the hats.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_ride(0.5, 0.8)
#' @export
voice_ride <- function(dur, gain) {
  n <- as.integer(max(dur, 0.5) * SR)
  tt <- seq(0, n - 1) / SR
  partials <- c(5413, 7091, 9317, 11843)  # inharmonic, metallic
  metal <- rowSums(vapply(partials, function(f) sign(sin(2 * pi * f * tt)),
                           numeric(n))) / length(partials)
  noise <- highpass(uzu_rnorm(n, "maelstrom:ride"), 6000)
  (metal * 0.6 + noise * 0.4) * exp(-tt / 0.35) * gain * RIDE_LEVEL
}

#' Synthesized crash cymbal
#'
#' Brighter metallic partials plus noise with a long decay, trimmed internally.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_crash(1.0, 0.8)
#' @export
voice_crash <- function(dur, gain) {
  n <- as.integer(max(dur, 1.2) * SR)
  tt <- seq(0, n - 1) / SR
  partials <- c(3891, 5217, 7433, 11029)
  metal <- rowSums(vapply(partials, function(f) sign(sin(2 * pi * f * tt)),
                           numeric(n))) / length(partials)
  noise <- highpass(uzu_rnorm(n, "maelstrom:crash"), 4000)
  (metal * 0.5 + noise * 0.6) * exp(-tt / 0.9) * gain * CRASH_LEVEL
}

#' Synthesized conga
#'
#' Pitched membrane with a 350 to 180 Hz drop plus a slap transient. Acoustic
#' by nature, so it also joins the acoustic kit.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_conga(0.25, 0.8)
#' @export
voice_conga <- function(dur, gain) {
  n <- as.integer(max(dur, 0.25) * SR)
  tt <- seq(0, n - 1) / SR
  freq <- 180 + 170 * exp(-tt / 0.03)  # pitch drop 350 -> 180 Hz
  phase <- 2 * pi * cumsum(freq) / SR
  body <- sin(phase) * exp(-tt / 0.14)
  slap <- highpass(uzu_rnorm(n, "maelstrom:conga"), 2500) * exp(-tt / 0.02) * 0.3
  (body + slap) * gain
}

DRUMS <- list(
  bd = voice_bd,
  sd = voice_sd,
  hh = function(d, g) voice_hh(d, g, FALSE),
  oh = function(d, g) voice_hh(d, g, TRUE),
  cp = voice_cp,
  tom = voice_tom,
  rim = voice_rim,
  shaker = voice_shaker,
  ride = voice_ride,
  crash = voice_crash,
  conga = voice_conga
)

## -- acoustic kit ------------------------------------------------------------
##
## A second kit with more acoustic character, selected per event with
## kit("acoustic"). The synth kit above stays the default. Voices missing
## here fall back to the synth kit.

#' Acoustic-style kick drum
#'
#' Pitched 160 to 45 Hz body plus a click transient; used by `kit("acoustic")`.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_bd_ac(0.3, 0.8)
#' @export
voice_bd_ac <- function(dur, gain) {
  n <- as.integer(max(dur, 0.3) * SR)
  tt <- seq(0, n - 1) / SR
  freq <- 45 + 115 * exp(-tt / 0.025)  # pitched drop 160 -> 45 Hz
  phase <- 2 * pi * cumsum(freq) / SR
  body <- sin(phase) * exp(-tt / 0.18)
  click <- highpass(uzu_rnorm(n, "maelstrom:bdac"), 4000) * exp(-tt / 0.004)
  (body + click * 0.8) * gain * 0.9
}

#' Acoustic-style snare drum
#'
#' Tonal body (190 and 320 Hz) plus bandpassed noise; used by `kit("acoustic")`.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @return A numeric vector of samples.
#' @examples
#' voice_sd_ac(0.25, 0.8)
#' @export
voice_sd_ac <- function(dur, gain) {
  n <- as.integer(max(dur, 0.25) * SR)
  tt <- seq(0, n - 1) / SR
  body <- sin(2 * pi * 190 * tt) * exp(-tt / 0.07) +
    sin(2 * pi * 320 * tt) * exp(-tt / 0.05) * 0.4
  noise <- uzu_rnorm(n, "maelstrom:sdac") * exp(-tt / 0.11)
  noise <- lowpass(highpass(noise, 1800), 9000)
  (body * 0.7 + noise * 0.55) * gain * 0.9
}

#' Acoustic-style hi-hat
#'
#' Inharmonic metallic partials plus noise; used by `kit("acoustic")`.
#'
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @param open Open hat when TRUE, default FALSE.
#' @return A numeric vector of samples.
#' @examples
#' voice_hh_ac(0.1, 0.8)
#' @export
voice_hh_ac <- function(dur, gain, open = FALSE) {
  n <- as.integer(max(dur, if (open) 0.35 else 0.06) * SR)
  tt <- seq(0, n - 1) / SR
  partials <- c(5413, 7091, 9317, 11843)  # inharmonic, metallic
  metal <- rowSums(vapply(partials, function(f) sign(sin(2 * pi * f * tt)),
                           numeric(n))) / length(partials)
  noise <- highpass(uzu_rnorm(n, "maelstrom:hhac"), 8000)
  decay <- if (open) 0.28 else 0.035
  (metal * 0.5 + noise * 0.5) * exp(-tt / decay) * gain * HH_LEVEL
}

DRUMS_AC <- list(
  bd = voice_bd_ac,
  sd = voice_sd_ac,
  hh = function(d, g) voice_hh_ac(d, g, FALSE),
  oh = function(d, g) voice_hh_ac(d, g, TRUE),
  conga = voice_conga  # acoustic by nature
)

#' Synth note voice
#'
#' Oscillator through an optional lowpass and an amplitude envelope.
#'
#' @param freq Frequency in Hz.
#' @param dur Duration in seconds.
#' @param kind Oscillator kind.
#' @param gain Gain multiplier.
#' @param lpf Lowpass cutoff in Hz; 0 disables.
#' @param attack Attack in seconds.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of samples.
#' @examples
#' voice_note(440, 0.2, "sine", 0.8, 0, 0.005)
#' @export
voice_note <- function(freq, dur, kind, gain, lpf, attack, sr = SR) {
  n <- max(as.integer(dur * sr), 64L)
  sig <- osc(kind, freq, n, sr)
  if (lpf > 0) sig <- lowpass(sig, lpf, sr)
  sig <- sig * envelope(n, attack, max(dur * 0.6, 0.05), sr)
  sig * gain * 0.5
}

## -- rendering -------------------------------------------------------------------

## Apply per-event insert effects: drive (saturation), crush (bitcrush),
## chorus. Amounts are static per event and validated by the combinators.
apply_fx <- function(sig, v) {
  if (num_or(v$drive, 0) > 0) sig <- saturate(sig, v$drive)
  if (num_or(v$crush, 0) > 0) sig <- bitcrush(sig, v$crush)
  if (num_or(v$chorus, 0) > 0) sig <- chorus_fx(sig, v$chorus)
  sig
}

#' Render one hap to samples
#'
#' Turn a single event into audio: drum voices for `sound` values, `voice_note` for `note`/`n` values, with gain, attack, lpf, and kit applied.
#'
#' @param h A hap.
#' @param cps Cycles per second.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of samples, or NULL for rests and unknown sounds.
#' @examples
#' h <- query_arc(s("bd"), 0, 1)[[1]]
#' render_hap(h, 0.5)
#' @export
render_hap <- function(h, cps, sr = SR) {
  if (is_rest(h)) return(NULL)
  v <- h$value
  gain <- num_or(v$gain, 1.0)
  dur_cycles <- if (!is.null(h$whole)) ts_duration(h$whole) else ts_duration(h$part)
  dur <- min(max(dur_cycles / cps, 0.03), 4.0)
  attack <- num_or(v$attack, 0.005)
  cutoff <- resolve_lpf(v$lpf, hap_onset(h))

  if (!is.null(v$sound)) {
    snd <- as.character(v$sound)
    fn <- DRUMS[[snd]]
    if (identical(v$kit, "acoustic") && !is.null(DRUMS_AC[[snd]])) {
      fn <- DRUMS_AC[[snd]]
    }
    if (is.null(fn)) return(NULL)
    sig <- fn(dur, gain)
    if (cutoff > 0) sig <- lowpass(sig, cutoff, sr)
    return(apply_fx(sig, v))
  }

  freqs <- NULL
  if (!is.null(v$note)) {
    raw <- v$note
    names <- if (is.list(raw)) raw else list(raw)
    freqs <- vapply(names, function(x) midi_to_freq(note_to_midi(as.character(x))), numeric(1))
  } else if (!is.null(v$n)) {
    raw <- v$n
    midis <- if (is.list(raw)) raw else list(raw)
    freqs <- vapply(midis, function(x) midi_to_freq(as.integer(x)), numeric(1))
  } else {
    return(NULL)
  }

  kind <- if (!is.null(v$s)) as.character(v$s) else "sine"
  sfn <- SYNTHS[[kind]]
  out <- NULL
  for (freq in freqs) {
    sig <- if (!is.null(sfn)) sfn(freq, dur, gain, cutoff, attack, sr)
           else voice_note(freq, dur, kind, gain, cutoff, attack, sr)
    out <- if (is.null(out)) sig else mix(out, sig)
  }
  apply_fx(out, v)
}

#' Dedup key for a hap value
#'
#' A string key identifying a hap value for mixdown deduplication; pattern-valued entries are marked, not expanded.
#'
#' @param v A hap value list.
#' @return A character key.
#' @examples
#' val_key(list(sound = "bd", gain = 0.8))
#' @export
val_key <- function(v) {
  ks <- names(v)
  if (is.null(ks)) return("")
  paste(vapply(ks, function(k) {
    x <- v[[k]]
    s <- if (is.function(x)) "<pattern>" else paste(x, collapse = ",")
    paste0(k, "=", s)
  }, character(1)), collapse = ";")
}

## -- mixdown ----------------------------------------------------------------------
##
## mixdown() renders events to samples. Mono by default; stereo = TRUE pans
## each event by its `pan` value (0 = left, 1 = right, default 0.5, constant
## power law) and returns a 2 x N matrix (row 1 = left, row 2 = right).

#' Render a pattern to samples
#'
#' Render every event from `query_arc` into one buffer at `cps`, deduplicating identical events. Mono by default; `stereo = TRUE` pans each event by its `pan` value (constant power law) and returns a 2 x N matrix (row 1 left, row 2 right). Peaks over 1 are normalized, not clipped.
#'
#' @param pat A pattern.
#' @param cycles Number of cycles to render.
#' @param cps Cycles per second, default 0.5.
#' @param sr Sample rate, default 44100.
#' @param stereo Render stereo when TRUE, default FALSE.
#' @return A numeric vector (mono) or 2 x N matrix (stereo).
#' @examples
#' mixdown(s("bd*4"), 1)
#' @export
mixdown <- function(pat, cycles, cps = 0.5, sr = SR, stereo = FALSE) {
  total <- as.integer(cycles / cps * sr)
  if (stereo) {
    L <- numeric(total)
    R <- numeric(total)
  } else {
    M <- numeric(total)
  }
  seen <- new.env(hash = TRUE, parent = emptyenv())
  for (h in query_arc(pat, 0, cycles)) {
    ## Resolve pattern-valued params (e.g. lpf) before keying so the dedup
    ## key distinguishes events that render differently.
    v <- h$value
    v$lpf <- resolve_lpf(v$lpf, hap_onset(h))
    h$value <- v
    key <- paste(h$whole$begin, h$whole$end, val_key(v), sep = "|")
    if (exists(key, envir = seen, inherits = FALSE)) next
    assign(key, TRUE, envir = seen)
    sig <- render_hap(h, cps, sr)
    if (is.null(sig)) next
    start <- as.integer(hap_onset(h) / cps * sr) + 1L
    if (start > total) next
    idx <- seq(start, min(start + length(sig) - 1L, total))
    if (stereo) {
      pan <- min(max(num_or(h$value$pan, 0.5), 0), 1)
      gl <- cos(pan * pi / 2)
      gr <- sin(pan * pi / 2)
      L[idx] <- L[idx] + sig[seq_along(idx)] * gl
      R[idx] <- R[idx] + sig[seq_along(idx)] * gr
    } else {
      M[idx] <- M[idx] + sig[seq_along(idx)]
    }
  }
  if (stereo) {
    peak <- max(max(abs(L)), max(abs(R)))
    if (peak > 1) { L <- L / peak; R <- R / peak }
    rbind(L, R)
  } else {
    peak <- max(abs(M))
    if (peak > 1) M <- M / peak
    M
  }
}

#' Render a pattern to mono samples
#'
#' `mixdown` with `stereo = FALSE`.
#'
#' @param pat A pattern.
#' @param cycles Number of cycles to render.
#' @param cps Cycles per second, default 0.5.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of samples.
#' @examples
#' render(s("bd*4"), 1)
#' @export
render <- function(pat, cycles, cps = 0.5, sr = SR) {
  mixdown(pat, cycles, cps, sr, stereo = FALSE)
}

#' Render a pattern to stereo samples
#'
#' `mixdown` with `stereo = TRUE`.
#'
#' @param pat A pattern.
#' @param cycles Number of cycles to render.
#' @param cps Cycles per second, default 0.5.
#' @param sr Sample rate, default 44100.
#' @return A 2 x N matrix (row 1 left, row 2 right).
#' @examples
#' render_stereo(s("bd*4"), 1)
#' @export
render_stereo <- function(pat, cycles, cps = 0.5, sr = SR) {
  mixdown(pat, cycles, cps, sr, stereo = TRUE)
}

## -- WAV output (pure base R, no dependencies) -------------------------------------
##
## Accepts a mono numeric vector or a 2 x N stereo matrix.

#' Write a 16-bit PCM WAV file
#'
#' Pure base R writer. Accepts a mono numeric vector or a 2 x N stereo matrix (interleaved LRLR). Samples are hard-clipped to [-1, 1] as a backstop.
#'
#' @param path Output file path.
#' @param samples Mono vector or 2 x N stereo matrix.
#' @param sr Sample rate, default 44100.
#' @return The path, invisibly.
#' @examples
#' write_wav(tempfile(), c(0.1, 0.2, 0.3))
#' @export
write_wav <- function(path, samples, sr = SR) {
  if (is.matrix(samples)) {
    channels <- nrow(samples)
    n <- ncol(samples)
    pcm <- matrix(as.integer(pmax(pmin(samples, 1), -1) * 32767), nrow = channels)
    interleaved <- as.vector(pcm)  # column-major: L1 R1 L2 R2 ...
  } else {
    channels <- 1L
    n <- length(samples)
    interleaved <- as.integer(pmax(pmin(samples, 1), -1) * 32767)
  }
  data_bytes <- 2L * length(interleaved)
  con <- file(path, "wb")
  on.exit(close(con))
  w32 <- function(x) writeBin(as.integer(x), con, size = 4, endian = "little")
  w16 <- function(x) writeBin(as.integer(x), con, size = 2, endian = "little")
  writeBin(charToRaw("RIFF"), con); w32(36 + data_bytes)
  writeBin(charToRaw("WAVE"), con)
  writeBin(charToRaw("fmt "), con); w32(16)
  w16(1); w16(channels)                # PCM, channels
  w32(sr); w32(sr * 2 * channels)      # sample rate, byte rate
  w16(2 * channels); w16(16)           # block align, bits per sample
  writeBin(charToRaw("data"), con); w32(data_bytes)
  writeBin(interleaved, con, size = 2, endian = "little")
  invisible(path)
}

#' Render a pattern straight to a WAV file
#'
#' One call for the common case: render, optionally add reverb, write the file.
#'
#' @param pat A pattern.
#' @param path Output file path.
#' @param cycles Number of cycles to render.
#' @param cps Cycles per second, default 0.5.
#' @param sr Sample rate, default 44100.
#' @param stereo Render in stereo, default FALSE.
#' @param reverb Reverb wet mix via `add_reverb()`, default 0 (off).
#' @return The path, invisibly.
#' @examples
#' render_wav(s("bd*4"), tempfile(), cycles = 2)
#' @export
render_wav <- function(pat, path, cycles, cps = 0.5, sr = SR, stereo = FALSE, reverb = 0) {
  samples <- if (isTRUE(stereo)) render_stereo(pat, cycles, cps, sr) else render(pat, cycles, cps, sr)
  if (reverb > 0) samples <- add_reverb(samples, reverb, sr)
  write_wav(path, samples, sr)
}

## -- reverb (Schroeder: 4 parallel combs, 2 series allpasses) ----------------------
##
## The recursive filters run in C via stats::filter, so this stays fast.

#' Schroeder comb filter
#'
#' Recursive comb used by the Schroeder reverb, run in C via `stats::filter`.
#'
#' @param x Input signal.
#' @param delay Delay in samples.
#' @param g Feedback gain.
#' @return The filtered signal.
#' @examples
#' comb_filter(c(1, rep(0, 99)), 10, 0.5)
#' @export
comb_filter <- function(x, delay, g) {
  as.vector(stats::filter(x, c(rep(0, delay - 1), g), method = "recursive"))
}

#' Schroeder allpass filter
#'
#' Allpass used by the Schroeder reverb: a convolution feedforward stage into a recursive stage.
#'
#' @param x Input signal.
#' @param delay Delay in samples.
#' @param g Gain.
#' @return The filtered signal.
#' @examples
#' allpass_filter(c(1, rep(0, 99)), 10, 0.5)
#' @export
allpass_filter <- function(x, delay, g) {
  ma <- stats::filter(x, c(1, rep(0, delay - 1), -g), method = "convolution", sides = 1)
  ma[is.na(ma)] <- 0  # convolution cannot see before the first sample
  as.vector(stats::filter(ma, c(rep(0, delay - 1), g), method = "recursive"))
}

## Schroeder wet signal via the Rust backend (extendr) when available, else
## the pure-R fallback. Bit-exact either way (see tests/test_rust.R).
## The result is memoized per session so the probe runs once.
.ms_state <- new.env(parent = emptyenv())

## TRUE when the compiled Rust backend answers; FALSE (pure-R fallback) when
## the package was installed without cargo/rustc.
#' Check for the Rust backend
#'
#' Whether the compiled Rust reverb backend is available in this session (memoized after the first call). FALSE means `schroeder_wet()` uses the pure-R fallback.
#'
#' @return A logical value.
#' @examples
#' rust_available()
#' @export
rust_available <- function() {
  if (is.null(.ms_state$rust_ok)) {
    .ms_state$rust_ok <- tryCatch({
      schroeder_wet_rust(c(0, 0), 44100L); TRUE
    }, error = function(e) FALSE)
  }
  isTRUE(.ms_state$rust_ok)
}

#' Schroeder reverb wet signal
#'
#' Four parallel combs into two series allpasses. Uses the Rust backend (`schroeder_wet_rust`) when available, otherwise the bit-exact pure-R `schroeder_wet_r()` (slower; install cargo/rustc and reinstall for speed).
#'
#' @param x Input signal.
#' @param sr Sample rate, default 44100.
#' @return The wet (reverberated) signal.
#' @examples
#' schroeder_wet(c(1, rep(0, 999)))
#' @export
schroeder_wet <- function(x, sr = SR) {
  if (rust_available()) {
    schroeder_wet_rust(x, as.integer(sr))
  } else {
    if (is.null(.ms_state$rust_warned)) {
      .ms_state$rust_warned <- TRUE
      message(paste0("maelstrom: Rust backend unavailable, using pure-R reverb (slower). ",
                     "Install cargo + rustc and reinstall for the fast path."))
    }
    schroeder_wet_r(x, sr)
  }
}

## Pure-R Schroeder wet signal (fallback when the Rust library is absent).
#' Pure-R Schroeder wet signal
#'
#' Pure-R reference for `schroeder_wet()`; kept for parity tests and as a fallback.
#'
#' @param x Input signal.
#' @param sr Sample rate, default 44100.
#' @return The wet (reverberated) signal.
#' @examples
#' schroeder_wet_r(c(1, rep(0, 999)))
#' @export
schroeder_wet_r <- function(x, sr = SR) {
  scale <- sr / 44100
  cd <- as.integer(c(1557, 1617, 1491, 1422) * scale)
  wet <- Reduce(`+`, lapply(cd, function(d) comb_filter(x, d, 0.84))) / 4
  wet <- allpass_filter(wet, as.integer(556 * scale), 0.5)
  allpass_filter(wet, as.integer(441 * scale), 0.5)
}

## Add Schroeder reverb to a mono vector or 2 x N stereo matrix.
#' Add Schroeder reverb
#'
#' Mix the wet signal into the input at `amount`, normalizing peaks over 1. Works on mono vectors and 2 x N stereo matrices.
#'
#' @param x Mono vector or 2 x N stereo matrix.
#' @param amount Wet mix, default 0.25.
#' @param sr Sample rate, default 44100.
#' @return The signal with reverb added.
#' @examples
#' add_reverb(c(1, rep(0, 999)), 0.25)
#' @export
add_reverb <- function(x, amount = 0.25, sr = SR) {
  if (is.function(x))
    stop("add_reverb() takes rendered audio, not a pattern. Render first: add_reverb(render(p, cycles), 0.3)")
  wet <- if (is.matrix(x)) {
    t(apply(x, 1, function(ch) schroeder_wet(ch, sr)))
  } else {
    schroeder_wet(x, sr)
  }
  out <- x + amount * wet
  peak <- max(abs(out))
  if (peak > 1) out <- out / peak
  out
}

## -- notebook audio (Jupyter via Ark/IRkernel) --------------------------------------
##
## Pure-R base64 encoder so inline audio needs no dependencies.

#' Base64-encode raw bytes
#'
#' Pure-R base64 encoder so inline notebook audio needs no dependencies.
#'
#' @param bytes A raw vector.
#' @return A base64 character string.
#' @examples
#' b64_encode(charToRaw("hi"))
#' @export
b64_encode <- function(bytes) {
  alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/", "")[[1]]
  n <- length(bytes)
  if (n == 0) return("")
  ## pad to a multiple of 3 bytes so every group yields 4 chars
  pad <- (3 - n %% 3) %% 3
  x <- c(as.integer(bytes), integer(pad))
  m <- matrix(x, nrow = 3)
  bits <- m[1, ] * 65536 + m[2, ] * 256 + m[3, ]
  s <- rbind(bits %/% 262144, (bits %/% 4096) %% 64,
             (bits %/% 64) %% 64, bits %% 64)
  chars <- alphabet[as.vector(s) + 1]
  if (pad > 0) chars[(length(chars) - pad + 1):length(chars)] <- "="
  paste(chars, collapse = "")
}

#' Inline audio HTML tag
#'
#' Render samples to a WAV in memory and wrap it in an `<audio>` tag with a data URI.
#'
#' @param samples Mono vector or 2 x N stereo matrix.
#' @param sr Sample rate, default 44100.
#' @return An HTML `<audio>` string.
#' @examples
#' audio_tag(c(0.1, -0.2, 0.3))
#' @export
audio_tag <- function(samples, sr = SR) {
  tmp <- tempfile(fileext = ".wav")
  on.exit(unlink(tmp))
  write_wav(tmp, samples, sr)
  bytes <- readBin(tmp, "raw", file.info(tmp)$size)
  sprintf('<audio controls src="data:audio/wav;base64,%s"></audio>', b64_encode(bytes))
}

## Render a pattern and display inline audio in a notebook. Returns the
## rendered samples invisibly.
#' Render and play audio in a notebook
#'
#' Render a pattern and display inline audio via IRdisplay when available (Jupyter with Ark/IRkernel); otherwise message and return the `<audio>` tag invisibly.
#'
#' @param pat A pattern.
#' @param seconds Seconds to render, default 8.
#' @param cps Cycles per second, default 0.5.
#' @param sr Sample rate, default 44100.
#' @return The `<audio>` tag, invisibly.
#' @examples
#' maelstrom_listen(s("bd*4"), seconds = 1)
#' @export
maelstrom_listen <- function(pat, seconds = 8, cps = 0.5, sr = SR) {
  samples <- render(pat, cycles = seconds * cps, cps = cps, sr = sr)
  tag <- audio_tag(samples, sr)
  if (in_jupyter_kernel()) {
    IRdisplay::display_html(tag)
  } else {
    message("not in a notebook kernel; returning the <audio> HTML tag invisibly.")
  }
  invisible(tag)
}

## TRUE when IRdisplay can actually publish: running inside a Jupyter kernel.
## IRdisplay's fallback display function (used outside a kernel) lives in its
## own namespace and only warns, so we check for that.
in_jupyter_kernel <- function() {
  if (!requireNamespace("IRdisplay", quietly = TRUE)) return(FALSE)
  f <- getOption("jupyter.base_display_func")
  is.function(f) && !identical(environment(f), getNamespace("IRdisplay"))
}
