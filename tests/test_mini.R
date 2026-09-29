## Timing tests for the maelstrom mini-notation parser.
## Faithful port of the specification-derived tests from the Python prototype.
## Each test encodes a behavior stated in the mini-notation spec.

library(maelstrom)

events <- function(pat, cycles = 1, start = 0) {
  lapply(query_arc(pat, start, cycles), function(h) {
    list(begin = h$whole$begin - start, end = h$whole$end - start, value = h$value)
  })
}

vals <- function(pat, key, cycles = 1, start = 0) {
  lapply(events(pat, cycles, start), function(e) list(e$begin, e$end, e$value[[key]]))
}

approx <- function(a, b, tol = 1e-4) {
  if (!(abs(a - b) < tol)) stop(sprintf("approx failed: %s != %s", a, b))
}

check <- function(name, expr) {
  force(expr)
  base::cat("ok:", name, "\n")
}

## spec 2.1/2.2: subgroups subdivide their parent step
check("test_subgroup", {
  ev <- vals(note("e5 [b4 c5] d5 c5 b4"), "note")
  stopifnot(identical(vapply(ev, function(e) e[[3]], character(1)),
                      c("e5", "b4", "c5", "d5", "c5", "b4")))
  approx(ev[[2]][[1]], 0.2); approx(ev[[2]][[2]], 0.3)
  approx(ev[[3]][[1]], 0.3); approx(ev[[3]][[2]], 0.4)
})

## spec 2.3: <a b c> rotates one item per cycle
check("test_alternation", {
  p <- note("<e5 b4 d5 c5>")
  got <- vapply(0:4, function(c) vals(p, "note", 1, c)[[1]][[3]], character(1))
  stopifnot(identical(got, c("e5", "b4", "d5", "c5", "e5")))
})

## spec 2.4: *n repeats inside the element's span
check("test_rep_star", {
  ev <- vals(s("bd*3 sn"), "sound")
  stopifnot(identical(vapply(ev, function(e) e[[3]], character(1)),
                      c("bd", "bd", "bd", "sn")))
  approx(ev[[1]][[1]], 0.0)
  approx(ev[[3]][[2]], 0.5)
  approx(ev[[4]][[1]], 0.5)
})

## spec 2.5: !n replicates as equal siblings
check("test_rep_bang", {
  ev <- vals(s("bd!3 sn"), "sound")
  stopifnot(identical(vapply(ev, function(e) e[[3]], character(1)),
                      c("bd", "bd", "bd", "sn")))
  approx(ev[[1]][[1]], 0.0); approx(ev[[1]][[2]], 0.25)
  approx(ev[[4]][[1]], 0.75); approx(ev[[4]][[2]], 1.0)
})

## spec: /n stretches over n cycles
check("test_slow", {
  ev <- vals(s("bd/3"), "sound", 4, 0)
  spans <- lapply(ev, function(e) c(e[[1]], e[[2]]))
  has <- function(b, e) any(vapply(spans, function(sp) {
    abs(sp[1] - b) < 1e-3 && abs(sp[2] - e) < 1e-3
  }, logical(1)))
  stopifnot(has(0.0, 3.0), has(3.0, 6.0))
})

## spec 2.6: @n temporal weights
check("test_weights", {
  ev <- vals(s("x@3 y@2"), "sound")
  approx(ev[[1]][[2]], 0.6)
  approx(ev[[2]][[1]], 0.6)
  approx(ev[[2]][[2]], 1.0)
})

## spec 2.8: _ extends the previous event
check("test_ties", {
  ev <- vals(s("bd _ _ ~ sn _"), "sound")
  stopifnot(identical(vapply(ev, function(e) e[[3]], character(1)), c("bd", "sn")))
  approx(ev[[1]][[1]], 0.0); approx(ev[[1]][[2]], 0.5)
  approx(ev[[2]][[1]], 4 / 6); approx(ev[[2]][[2]], 1.0)
})

## spec 2.9: comma layers are parallel
check("test_layers", {
  ev <- events(s("bd*4, hh*8"))
  stopifnot(length(ev) == 12)
  bds <- vapply(ev, function(e) e$begin, numeric(1))[
    vapply(ev, function(e) e$value$sound, character(1)) == "bd"]
  hhs <- vapply(ev, function(e) e$begin, numeric(1))[
    vapply(ev, function(e) e$value$sound, character(1)) == "hh"]
  stopifnot(length(bds) == 4, length(hhs) == 8)
  approx(bds[2], 0.25)
  approx(hhs[2], 0.125)
})

## spec 2.12: :n sample index
check("test_sample_index", {
  ev <- events(s("hh:2"))
  stopifnot(ev[[1]]$value$sample == 2)
})

## spec 2.13: euclid matches bjorklund distribution
check("test_euclid", {
  ev <- vals(s("bd(3,8)"), "sound")
  pulses <- bjorklund(3, 8)
  expected <- which(pulses) / 8 - 1 / 8
  got <- vapply(ev, function(e) e[[1]], numeric(1))
  stopifnot(length(got) == 3)
  for (i in seq_along(got)) approx(got[i], expected[i])
  approx(got[1], 0.0); approx(got[2], 0.375); approx(got[3], 0.75)
})

## spec 2.13: euclidean rotation offset
check("test_euclid_offset", {
  ev <- vals(s("bd(3,8,2)"), "sound")
  got <- vapply(ev, function(e) e[[1]], numeric(1))
  stopifnot(length(got) == 3)
  approx(got[1], 0.0); approx(got[2], 0.25); approx(got[3], 0.625)
})

## spec 2.15: ranges expand ascending
check("test_range", {
  ev <- vals(n("0 .. 3"), "n")
  stopifnot(identical(vapply(ev, function(e) e[[3]], numeric(1)), as.numeric(0:3)))
  approx(ev[[1]][[1]], 0.0); approx(ev[[1]][[2]], 0.25)
  approx(ev[[4]][[1]], 0.75); approx(ev[[4]][[2]], 1.0)
})

## choice is deterministic per cycle, varies across cycles
check("test_choice", {
  a <- vals(s("[bd|sd]"), "sound", 1, 5)
  b <- vals(s("[bd|sd]"), "sound", 1, 5)
  stopifnot(identical(
    vapply(a, function(e) e[[3]], character(1)),
    vapply(b, function(e) e[[3]], character(1))))
  seen <- unique(vapply(0:7, function(c) vals(s("[bd|sd]"), "sound", 1, c)[[1]][[3]],
                        character(1)))
  stopifnot(length(seen) > 1)
})

## rests: ~ and - are identical
check("test_rests", {
  va <- vapply(vals(s("bd ~ sd"), "sound"), function(e) e[[3]], character(1))
  vb <- vapply(vals(s("bd - sd"), "sound"), function(e) e[[3]], character(1))
  stopifnot(identical(va, vb))
})

## ratio shorthand: 6%5 -> 1.2, fractional values survive n() as doubles
check("test_ratio", {
  ev <- vals(n("6%5"), "n")
  stopifnot(length(ev) == 1)
  approx(ev[[1]][[3]], 1.2)
  stopifnot(is.double(ev[[1]][[3]]))
})

## ratios keep full precision, no integer truncation
check("test_ratio_precision", {
  ev <- vals(n("4%2 3%2 1%3"), "n")
  stopifnot(length(ev) == 3)
  approx(ev[[1]][[3]], 2.0)
  approx(ev[[2]][[3]], 1.5)
  approx(ev[[3]][[3]], 1 / 3)
})

## ratio composes with suffixes: * binds to the ratio atom
check("test_ratio_suffix", {
  ev <- vals(n("6%5*2"), "n")
  stopifnot(length(ev) == 2)
  approx(ev[[1]][[3]], 1.2); approx(ev[[2]][[3]], 1.2)
  approx(ev[[1]][[1]], 0.0); approx(ev[[2]][[1]], 0.5)
})

## denominator must be a nonzero number; parse errors surface on query
check("test_ratio_bad_denom", {
  qerr <- function(text) {
    tryCatch({ query_arc(n(text), 0, 1); NULL },
             error = function(e) conditionMessage(e))
  }
  e1 <- qerr("6%0")
  stopifnot(!is.null(e1), grepl("nonzero", e1))
  e2 <- qerr("6%abc")
  stopifnot(!is.null(e2), grepl("expected number after %", e2))
})

base::cat("18 tests passed\n")
