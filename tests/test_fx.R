## Tests for the space/groove layer: pan, stereo, echo, swing, reverb.

library(maelstrom)

check <- function(name, cond) {
  if (!isTRUE(cond)) stop(paste("FAIL:", name))
  base::cat("ok:", name, "\n")
}

onsets <- function(p, cycles = 1) {
  vapply(query_arc(p, 0, cycles), hap_onset, numeric(1))
}

## swing pushes odd 16ths later, leaves even ones alone
o <- onsets(s("hh*16") |> swing(0.5))
check("swing shifts odd 16ths", {
  abs(o[1] - 0) < 1e-9 && abs(o[2] - (1 / 16 + 0.5 / 16)) < 1e-9 &&
    abs(o[3] - 2 / 16) < 1e-9 && abs(o[4] - (3 / 16 + 0.5 / 16)) < 1e-9
})
check("swing zero is identity", {
  identical(onsets(s("hh*16") |> swing(0)), onsets(s("hh*16")))
})

## echo adds decaying repeats. The k=1 echo of the cycle -1 kick and the k=2
## echo of the cycle 0 kick bleed into the window, which is correct: their
## audible tails overlap [0, 2).
o2 <- sort(onsets(s("bd") |> echo(2, 0.5, 0.5), 2))
check("echo onsets", all(abs(o2 - c(0, 0, 0.5, 1.0, 1.0, 1.5)) < 1e-9))
gains <- vapply(query_arc(s("bd") |> echo(2, 0.5, 0.5), 0, 2),
                function(h) h$value$gain, numeric(1))
check("echo decays", all(abs(sort(gains) - c(0.25, 0.25, 0.5, 0.5, 1, 1)) < 1e-9))
check("echo named args", {
  all(abs(onsets(s("bd") |> echo(n = 1, time = 0.25, fb = 0.5), 1) - c(0, 0.25)) < 1e-9)
})

## pan steers energy left/right in a stereo render
m <- render_stereo(s("bd") |> pan(0), 1, cps = 1)
check("pan hard left", sum(abs(m[1, ])) > 10 * sum(abs(m[2, ])))
m <- render_stereo(s("bd") |> pan(1), 1, cps = 1)
check("pan hard right", sum(abs(m[2, ])) > 10 * sum(abs(m[1, ])))
check("stereo shape", is.matrix(m) && nrow(m) == 2 && ncol(m) == 44100)

## chord stacks notes into a real chord (values stay symbolic until render)
vals <- vapply(query_arc(chord("c4", "eb4", "g4"), 0, 1),
               function(h) h$value$note, character(1))
check("chord", setequal(vals, c("c4", "eb4", "g4")))

## reverb adds a tail where the dry signal is silent
x <- c(1, rep(0, 44100))
y <- add_reverb(x, amount = 1)
check("reverb tail", any(y[5000:44100] != 0) && length(y) == length(x))

## new drum voices exist and render
check("new voices", {
  all(vapply(c("tom", "rim", "shaker"), function(v) {
    sig <- render(s(v), 1, cps = 1)
    max(abs(sig)) > 0.01
  }, logical(1)))
})

## stereo wav writes a valid header
tmp <- tempfile(fileext = ".wav")
write_wav(tmp, render_stereo(stack(s("bd") |> pan(0), s("hh") |> pan(1)), 1, cps = 1))
info <- file.info(tmp)
con <- file(tmp, "rb")
hdr <- readBin(con, "raw", 44)
close(con)
channels <- readBin(hdr[23:24], "integer", size = 2, endian = "little")
check("stereo wav", rawToChar(hdr[1:4]) == "RIFF" && channels == 2 &&
  info$size == 44 + 44100 * 2 * 2)
unlink(tmp)

## stereo samples are interleaved LRLR, not all-left-then-all-right
tmp <- tempfile(fileext = ".wav")
mx <- rbind(c(0.1, 0.2, 0.3), c(0.4, 0.5, 0.6))  # L row, R row
write_wav(tmp, mx)
con <- file(tmp, "rb")
seek(con, 44)
raw <- readBin(con, integer(), n = 6, size = 2, endian = "little")
close(con)
unlink(tmp)
expect <- as.integer(c(0.1, 0.4, 0.2, 0.5, 0.3, 0.6) * 32767)
check("stereo interleave", all(raw == expect))

## pattern-valued lpf: the dedup key must not choke on the pattern closure,
## and must distinguish events with different resolved cutoffs
w <- render(s("bd*4") |> lpf(n("400 800 1600 3200")), cycles = 1)
check("pattern lpf renders", length(w) == as.integer(1 / 0.5 * 44100) && max(abs(w)) <= 1)
w_a <- render(s("bd bd") |> lpf(n("400 400")), cycles = 1)
w_b <- render(s("bd bd") |> lpf(n("400 8000")), cycles = 1)
check("pattern lpf changes the sound", !isTRUE(all.equal(w_a, w_b)))

## kit(): acoustic kit renders, synth stays default, unknown names error
w_ac <- render(s("bd sd hh oh") |> kit("acoustic"), cycles = 1)
check("acoustic kit renders", length(w_ac) > 0 && max(abs(w_ac)) <= 1)
w_sy <- render(s("bd sd hh oh") |> kit("synth"), cycles = 1)
w_df <- render(s("bd sd hh oh"), cycles = 1)
check("synth kit is default", isTRUE(all.equal(w_sy, w_df)))
check("acoustic kit differs from synth", !isTRUE(all.equal(w_ac, w_df)))
err <- tryCatch({ s("bd") |> kit("nope"); NULL }, error = function(e) conditionMessage(e))
check("unknown kit errors", grepl("unknown kit", err))

## add_reverb(): loud early error on a pattern, not a confusing failure
err <- tryCatch({ add_reverb(s("bd*4"), 0.25); NULL }, error = function(e) conditionMessage(e))
check("add_reverb rejects a pattern", grepl("not a pattern", err))

## render_wav(): one-step render to file
f <- tempfile(fileext = ".wav")
p <- render_wav(s("bd*4") |> gain(0.6), f, cycles = 2)
check("render_wav returns path invisibly", identical(p, f) && file.exists(f) && file.info(f)$size > 1000)

base::cat("18 fx tests passed\n")
