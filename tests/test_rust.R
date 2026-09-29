## Rust backend parity: schroeder_wet() (extendr) vs schroeder_wet_r() (pure R).
library(maelstrom)

set.seed(42)
x <- rnorm(5000)
ref <- schroeder_wet_r(x)
fast <- schroeder_wet(x)
d <- max(abs(ref - fast))
if (d >= 1e-9) stop(paste("FAIL: rust/R parity, max abs diff =", d))
base::cat("ok: rust schroeder_wet matches pure R (max abs diff", d, ")\n")
