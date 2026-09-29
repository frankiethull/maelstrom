# Note: Any variables prefixed with `.` are used for text
# replacement in the Makevars.in and Makevars.win.in

# Rust is optional: without cargo/rustc the package still installs and the
# Schroeder reverb falls back to pure R (slower). Detect the toolchain here
# (including ~/.cargo/bin, which msrv.R also appends to PATH) and only run
# the strict MSRV checks when it is present.
Sys.setenv(PATH = paste(Sys.getenv("PATH"),
                        file.path(Sys.getenv("HOME"), ".cargo", "bin"),
                        sep = ":"))
have_rust <- nzchar(Sys.which("cargo")) && nzchar(Sys.which("rustc"))

if (have_rust) {
  # check the packages MSRV first
  source("tools/msrv.R")
} else {
  message(paste0("Cargo/rustc not found: building WITHOUT the Rust accelerator.\n",
                 "Reverb falls back to pure R (slower). Install Rust from ",
                 "https://www.rust-lang.org/tools/install and reinstall ",
                 "for the fast path."))
}

# check DEBUG and NOT_CRAN environment variables
env_debug <- Sys.getenv("DEBUG")
env_not_cran <- Sys.getenv("NOT_CRAN")

# check if the vendored zip file exists
vendor_exists <- file.exists("src/rust/vendor.tar.xz")

is_not_cran <- env_not_cran != ""
is_debug <- env_debug != ""

if (is_debug) {
  # if we have DEBUG then we set not cran to true
  # CRAN is always release build
  is_not_cran <- TRUE
  message("Creating DEBUG build.")
}

if (!is_not_cran) {
  message("Building for CRAN.")
}

# we set cran flags only if NOT_CRAN is empty and if
# the vendored crates are present.
.cran_flags <- ifelse(
  !is_not_cran && vendor_exists,
  "-j 2 --offline",
  ""
)

# when DEBUG env var is present we use `--debug` build
.profile <- ifelse(is_debug, "", "--release")
.clean_targets <- ifelse(is_debug, "", "$(TARGET_DIR)")

# We specify this target when building for webR
webr_target <- "wasm32-unknown-emscripten"

# here we check if the platform we are building for is webr
is_wasm <- identical(R.version$platform, webr_target)

# print to terminal to inform we are building for webr
if (is_wasm) {
  message("Building for WebR")
}

# we check if we are making a debug build or not
# if so, the LIBDIR environment variable becomes:
# LIBDIR = $(TARGET_DIR)/{wasm32-unknown-emscripten}/debug
# this will be used to fill out the LIBDIR env var for Makevars.in
target_libpath <- if (is_wasm) "wasm32-unknown-emscripten" else NULL
cfg <- if (is_debug) "debug" else "release"

# used to replace @LIBDIR@
.libdir <- paste(c(target_libpath, cfg), collapse = "/")

# use this to replace @TARGET@
# we specify the target _only_ on webR
# there may be use cases later where this can be adapted or expanded
.target <- ifelse(is_wasm, paste0("--target=", webr_target), "")

# add panic exports only for WASM builds
.panic_exports <- ifelse(
  is_wasm,
  "CARGO_PROFILE_DEV_PANIC=\"abort\" CARGO_PROFILE_RELEASE_PANIC=\"abort\" ",
  ""
)

# -DMAELSTROM_HAS_RUST guards src/entrypoint.c so the DLL builds (empty
# init) when the Rust static library was skipped.
.rust_def <- ifelse(have_rust, "-DMAELSTROM_HAS_RUST", "")

# read in the Makevars.in file checking
is_windows <- .Platform[["OS.type"]] == "windows"

# if windows we replace in the Makevars.win.in
mv_fp <- ifelse(
  is_windows,
  "src/Makevars.win.in",
  "src/Makevars.in"
)

# set the output file
mv_ofp <- ifelse(
  is_windows,
  "src/Makevars.win",
  "src/Makevars"
)

# delete the existing Makevars{.win/.wasm}
if (file.exists(mv_ofp)) {
  message("Cleaning previous `", mv_ofp, "`.")
  invisible(file.remove(mv_ofp))
}

message("Writing `", mv_ofp, "`.")
if (have_rust) {
  # read as a single string
  mv_txt <- readLines(mv_fp)

  # replace placeholder values
  new_txt <- gsub("@CRAN_FLAGS@", .cran_flags, mv_txt) |>
    gsub("@PROFILE@", .profile, x = _) |>
    gsub("@CLEAN_TARGET@", .clean_targets, x = _) |>
    gsub("@LIBDIR@", .libdir, x = _) |>
    gsub("@TARGET@", .target, x = _) |>
    gsub("@RUST_DEF@", .rust_def, x = _) |>
    gsub("@PANIC_EXPORTS@", .panic_exports, x = _)

  con <- file(mv_ofp, open = "wb")
  writeLines(new_txt, con, sep = "\n")
  close(con)
} else {
  # No Rust: stub Makevars that just compiles entrypoint.c (empty init).
  # The reverb falls back to pure R at runtime (see schroeder_wet()).
  stub <- c("PKG_LIBS =",
            "PKG_CPPFLAGS =",
            "",
            "all: $(SHLIB)",
            "",
            "clean:",
            "\trm -Rf $(SHLIB) $(OBJECTS)")
  con <- file(mv_ofp, open = "wb")
  writeLines(stub, con, sep = "\n")
  close(con)
}

message("`tools/config.R` has finished.")
