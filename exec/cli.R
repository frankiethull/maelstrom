#!/usr/bin/env Rscript
## Entry point for the `maelstrom` executable. Requires the package installed:
##   R CMD INSTALL .
## (installing compiles the Rust backend via extendr)
library(maelstrom)
quit(status = maelstrom_cli(commandArgs(trailingOnly = TRUE)))
