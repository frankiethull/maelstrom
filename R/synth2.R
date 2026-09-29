## New synth voices and DSP helpers for maelstrom.
##
## Synth voices take (freq, dur, gain, lpf, attack, sr), mirroring voice_note,
## and are wired into render_hap through the SYNTHS lookup so synth("pluck")
## and friends work. (New drum voices live in R/audio.R next to the other
## drum voices, so the DRUMS list sees them at source time.) All noise is
## deterministic via uzu_rnorm; internal trims keep new voices balanced
## against bd/sd.

## Karplus-Strong plucked string, vectorized by period.
##
## The recurrence y[i] = damp * 0.5 * (y[i-N] + y[i-N-1]) is computed one
## period at a time, so the whole voice is a few dozen vector ops, not a
## per-sample R loop.
karplus_strong <- function(freq, n, sr, tag) {
  N <- max(2L, as.integer(round(sr / max(freq, 20))))
  exc <- uzu_rnorm(N, tag)
  exc <- exc / max(max(abs(exc)), 1e-9)  # normalize excitation peak to 1
  y <- numeric(n)
  m0 <- min(N, n)
  y[seq_len(m0)] <- exc[seq_len(m0)]
  if (n > N) {
    damp <- 0.965  # per-period damping; higher harmonics die first
    pos <- N + 1L
    while (pos <= n) {
      m <- min(N, n - pos + 1L)
      prev <- y[seq.int(pos - N, pos - 1L)]
      prev_last <- if (pos - N - 1L >= 1L) y[pos - N - 1L] else prev[N]
      y[seq.int(pos, pos + m - 1L)] <- damp * 0.5 *
        (prev[seq_len(m)] + c(prev_last, prev[seq_len(m - 1L)]))
      pos <- pos + m
    }
  }
  y
}

#' Plucked string voice
#'
#' Karplus-Strong string with its own natural decay; the envelope only applies the attack ramp.
#'
#' @param freq Frequency in Hz.
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @param lpf Lowpass cutoff in Hz; 0 disables, default 0.
#' @param attack Attack in seconds, default 0.005.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of samples.
#' @examples
#' voice_pluck(220, 0.5, 0.8)
#' @export
voice_pluck <- function(freq, dur, gain, lpf = 0, attack = 0.005, sr = SR) {
  n <- max(as.integer(dur * sr), 64L)
  tag <- paste0("maelstrom:pluck:", as.integer(round(freq)))
  sig <- karplus_strong(freq, n, sr, tag)
  if (lpf > 0) sig <- lowpass(sig, lpf, sr)
  sig <- sig * envelope(n, attack, 1e9, sr)  # attack ramp only
  sig * gain * 0.5
}

#' Pad voice
#'
#' Three detuned saws with a slow attack and a long, gentle decay.
#'
#' @param freq Frequency in Hz.
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @param lpf Lowpass cutoff in Hz; 0 disables, default 0.
#' @param attack Attack in seconds, default 0.005 (raised to 0.2 internally for the slow swell).
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of samples.
#' @examples
#' voice_pad(110, 1.0, 0.6)
#' @export
voice_pad <- function(freq, dur, gain, lpf = 0, attack = 0.005, sr = SR) {
  n <- max(as.integer(dur * sr), 64L)
  det <- c(0.997, 1.0, 1.003)
  sig <- rowMeans(vapply(det, function(d) osc("sawtooth", freq * d, n, sr),
                         numeric(n)))
  if (lpf > 0) sig <- lowpass(sig, lpf, sr)
  sig <- sig * envelope(n, max(attack, 0.2), max(dur * 3, 0.5), sr)
  sig * gain * 0.4
}

#' Sub bass voice
#'
#' Sine an octave below the note plus a touch of the fundamental.
#'
#' @param freq Frequency in Hz.
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @param lpf Lowpass cutoff in Hz; 0 disables, default 0.
#' @param attack Attack in seconds, default 0.005.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of samples.
#' @examples
#' voice_sub(55, 0.5, 0.8)
#' @export
voice_sub <- function(freq, dur, gain, lpf = 0, attack = 0.005, sr = SR) {
  n <- max(as.integer(dur * sr), 64L)
  tt <- seq(0, n - 1) / sr
  sig <- sin(2 * pi * (freq / 2) * tt) * 0.7 + sin(2 * pi * freq * tt) * 0.3
  if (lpf > 0) sig <- lowpass(sig, lpf, sr)
  sig <- sig * envelope(n, attack, max(dur * 0.6, 0.05), sr)
  sig * gain * 0.6
}

#' Lead voice
#'
#' Square and saw blend; bright and cutting, trimmed to sit in a mix.
#'
#' @param freq Frequency in Hz.
#' @param dur Duration in seconds.
#' @param gain Gain multiplier.
#' @param lpf Lowpass cutoff in Hz; 0 disables, default 0.
#' @param attack Attack in seconds, default 0.005.
#' @param sr Sample rate, default 44100.
#' @return A numeric vector of samples.
#' @examples
#' voice_lead(440, 0.4, 0.7)
#' @export
voice_lead <- function(freq, dur, gain, lpf = 0, attack = 0.005, sr = SR) {
  n <- max(as.integer(dur * sr), 64L)
  sig <- osc("square", freq, n, sr) * 0.4 + osc("sawtooth", freq, n, sr) * 0.6
  if (lpf > 0) sig <- lowpass(sig, lpf, sr)
  sig <- sig * envelope(n, attack, max(dur * 0.6, 0.05), sr)
  sig * gain * 0.35
}

## Synth voice lookup for render_hap: kind -> voice function.
## Unknown kinds fall through to voice_note, which errors on unknown osc kinds.
SYNTHS <- list(
  pluck = voice_pluck,
  pad = voice_pad,
  sub = voice_sub,
  lead = voice_lead
)

## Drum voices (ride, crash, conga) live in R/audio.R next to the other
## drum voices; what follows is DSP helpers only.

#' Soft saturation
#'
#' Normalized tanh saturation: `tanh(drive * x) / tanh(drive)`, so the peak stays near 1.
#'
#' @param x Input signal.
#' @param drive Saturation amount; 0 returns `x` unchanged, 1 is gentle, 5 is heavy.
#' @return The saturated signal.
#' @examples
#' saturate(c(-1, -0.5, 0.5, 1), 2)
#' @export
saturate <- function(x, drive) {
  if (!is.numeric(drive) || length(drive) != 1 || drive < 0)
    stop("drive must be a single non-negative number")
  if (drive == 0) return(x)
  tanh(drive * x) / tanh(drive)
}

#' Bitcrush
#'
#' Quantize amplitudes to `bits` bits.
#'
#' @param x Input signal.
#' @param bits Bit depth in [1, 16].
#' @return The quantized signal.
#' @examples
#' bitcrush(c(-1, -0.3, 0.3, 1), 4)
#' @export
bitcrush <- function(x, bits) {
  if (!is.numeric(bits) || length(bits) != 1 || bits < 1 || bits > 16)
    stop("bits must be a single number in [1, 16]")
  q <- 2^(round(bits) - 1)
  round(x * q) / q
}

#' Chorus
#'
#' Mix the signal with a copy delayed by 12 ms plus an LFO between 0 and `depth` ms, via interpolated delay (vectorized).
#'
#' @param x Input signal.
#' @param depth LFO depth in ms; 0 returns `x` unchanged.
#' @param rate LFO rate in Hz, default 0.8.
#' @param sr Sample rate, default 44100.
#' @return The chorused signal.
#' @examples
#' chorus_fx(sin(2 * pi * 440 * seq(0, 999) / 44100), 6)
#' @export
chorus_fx <- function(x, depth, rate = 0.8, sr = SR) {
  if (!is.numeric(depth) || length(depth) != 1 || depth < 0)
    stop("depth must be a single non-negative number (ms)")
  if (depth == 0) return(x)
  n <- length(x)
  if (n < 2) return(x)
  idx <- seq_len(n)
  tt <- (idx - 1) / sr
  dsamp <- (12 + depth * (0.5 + 0.5 * sin(2 * pi * rate * tt))) * sr / 1000
  wet <- stats::approx(idx, x, xout = idx - dsamp)$y
  wet[is.na(wet)] <- 0
  0.65 * x + 0.35 * wet
}
