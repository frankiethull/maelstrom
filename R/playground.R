## Shiny playground launcher for maelstrom.
##
## The app itself lives in inst/shiny/app.R (installed with the package);
## this wrapper just checks for shiny and runs it. Rendering inside the app
## is offline (render/render_stereo + audio_tag playback), the same as the
## CLI: a playground for trying patterns, not a live-coding scheduler.

#' Launch the maelstrom playground (Shiny)
#'
#' Interactive playground: edit pattern code, render offline to WAV in the browser, inspect the event grid, and download the result. Needs the `shiny` package (in Suggests, so the core stays dependency-free).
#'
#' @param ... Passed to `shiny::runApp()`.
#' @return Called for its side effect (runs the app).
#' @examples
#' if (interactive() && requireNamespace("shiny", quietly = TRUE)) {
#'   maelstrom_playground()
#' }
#' @export
maelstrom_playground <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("maelstrom_playground() needs the shiny package: install.packages(\"shiny\")")
  }
  shiny::runApp(system.file("shiny", package = "maelstrom"), ...)
}
