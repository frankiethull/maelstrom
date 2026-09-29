## Tests for new sounds: synth voices, drum voices, insert effects, helpers.

library(maelstrom)

passed <- 0
check <- function(name, cond) {
  if (!isTRUE(cond)) stop(paste("FAIL:", name))
  passed <<- passed + 1
  base::cat("ok:", name, "\n")
}

err <- function(expr) {
  tryCatch({ expr; NULL }, error = function(e) conditionMessage(e))
}

exp_len <- as.integer(1 / 0.5 * 44100)  # render(x, cycles = 1)

## -- synth voices ---------------------------------------------------------
for (kind in c("pluck", "pad", "sub", "lead")) {
  p <- note("c3") |> synth(kind)
  w <- render(p, 1)
  check(paste("synth", kind, "length"), length(w) == exp_len)
  check(paste("synth", kind, "peak"), max(abs(w)) <= 1)
  check(paste("synth", kind, "deterministic"), isTRUE(all.equal(w, render(p, 1))))
}

v <- voice_pluck(220, 0.5, 0.8)
check("voice_pluck length", length(v) == as.integer(0.5 * 44100))
check("pluck decays", mean(abs(v[11026:22050])) < mean(abs(v[1:11025])))
check("pluck deterministic", isTRUE(all.equal(v, voice_pluck(220, 0.5, 0.8))))

## -- drum voices ----------------------------------------------------------
for (snd in c("ride", "crash", "conga")) {
  p <- s(snd)
  w <- render(p, 1)
  check(paste("drum", snd, "renders"), length(w) == exp_len && max(abs(w)) <= 1)
  check(paste("drum", snd, "deterministic"), isTRUE(all.equal(w, render(p, 1))))
}
w <- render(s("conga") |> kit("acoustic"), 1)
check("acoustic conga renders", length(w) == exp_len && max(abs(w)) <= 1)

## -- insert effects -------------------------------------------------------
w0 <- render(s("bd*4"), 1)
w1 <- render(s("bd*4") |> drive(3), 1)
check("drive changes signal", !isTRUE(all.equal(w0, w1)))
check("drive bounded", max(abs(w1)) <= 1)
check("drive 0 is identity", isTRUE(all.equal(w0, render(s("bd*4") |> drive(0), 1))))

c0 <- render(note("c3") |> synth("sawtooth"), 1)
c1 <- render(note("c3") |> synth("sawtooth") |> crush(4), 1)
check("crush changes signal", !isTRUE(all.equal(c0, c1)))
check("crush quantizes",
      length(unique(round(c1, 6))) < length(unique(round(c0, 6))))

h0 <- render(note("c4") |> synth("pad"), 1)
h1 <- render(note("c4") |> synth("pad") |> chorus(6), 1)
check("chorus changes signal", !isTRUE(all.equal(h0, h1)))
check("chorus bounded", max(abs(h1)) <= 1)

check("saturate identity at 0",
      isTRUE(all.equal(saturate(c(0.5, -0.5), 0), c(0.5, -0.5))))
check("saturate bounded", max(abs(saturate(c(-2, 2), 3))) <= 1.01)  # near 1 by design
check("bitcrush 1 bit", all(bitcrush(c(-0.9, -0.1, 0.1, 0.9), 1) %in% c(-1, 0, 1)))

check("drive rejects negative", grepl("non-negative", err(drive(s("bd"), -1))))
check("crush rejects 0 bits", grepl("1, 16", err(crush(s("bd"), 0))))
check("crush rejects 17 bits", grepl("1, 16", err(crush(s("bd"), 17))))
check("chorus rejects negative", grepl("non-negative", err(chorus(s("bd"), -2))))
check("unknown synth errors at render",
      grepl("unknown osc kind", err(render(note("c3") |> synth("nosuch"), 1))))

## -- helpers --------------------------------------------------------------
hs <- query_arc(arp(chord("c4", "e4", "g4")), 0, 1)
o <- vapply(hs, hap_onset, numeric(1))
check("arp onsets", length(hs) == 3 && all(abs(sort(o) - c(0, 1 / 3, 2 / 3)) < 1e-9))
mids <- vapply(hs, function(h) note_to_midi(as.character(h$value$note)), numeric(1))
check("arp ascending", all(diff(mids[order(o)]) > 0))
hs2 <- query_arc(arp(note("c4")), 0, 1)
check("arp single note untouched",
      length(hs2) == 1 && abs(hap_onset(hs2[[1]])) < 1e-9)
hs3 <- query_arc(arp(s("bd sd")), 0, 1)
check("arp drums untouched", length(hs3) == 2)

## query two cycles: an event whose jittered onset lands just outside the
## window is attributed to the neighboring window (same as swing)
q1 <- query_arc(humanize(s("bd*4"), 0.01, 0.2), 0, 2)
q2 <- query_arc(humanize(s("bd*4"), 0.01, 0.2), 0, 2)
o1 <- vapply(q1, hap_onset, numeric(1))
o2 <- vapply(q2, hap_onset, numeric(1))
check("humanize deterministic", isTRUE(all.equal(o1, o2)))
g1 <- vapply(q1, function(h) h$value$gain, numeric(1))
check("humanize jitters velocity", any(abs(g1 - 1) > 1e-9))
straight <- seq(0, 2, by = 0.25)
d <- vapply(o1, function(t) min(abs(straight - t)), numeric(1))
check("humanize jitters timing", all(d <= 0.010001) && any(d > 1e-9))
check("humanize rejects negative timing", grepl("non-negative", err(humanize(s("bd"), -1))))

t1 <- query_arc(transpose(note("c4"), 2), 0, 1)
check("transpose note", t1[[1]]$value$note == "d4")
t2 <- query_arc(transpose(n("60"), -2), 0, 1)
check("transpose n", t2[[1]]$value$n == 58)

check("midi_to_note", midi_to_note(69) == "a4" && midi_to_note(60) == "c4")
check("midi roundtrip", midi_to_note(note_to_midi("eb3")) == "d#3")

base::cat(passed, "sound tests passed\n")

## arrange(): pattern/count pairs sequence sections
a <- arrange(s("bd*4"), 2, s("sd*4"), 2)
ev <- query_arc(a, 0, 4)
check("arrange spans 4 cycles", length(ev) == 16)
first_two <- vapply(ev[1:8], function(h) h$value$sound, character(1))
last_two <- vapply(ev[9:16], function(h) h$value$sound, character(1))
check("arrange orders sections", all(first_two == "bd") && all(last_two == "sd"))
err <- tryCatch({ arrange(s("bd"), 2, s("sd")); NULL }, error = function(e) conditionMessage(e))
check("arrange rejects odd args", grepl("pattern/count pairs", err))
