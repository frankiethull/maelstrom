## maelstrom playground: edit pattern code, render offline to WAV, play it.
##
## This is a playground, not a live-coding instrument: Shiny is
## request/response, so there is no sample-accurate scheduler. Press
## Render, get a WAV back, play it in the browser. The same contract as
## the CLI holds: the code must define `pattern` (and may define `cps`).
##
## Run with maelstrom::maelstrom_playground() (needs the shiny package).

library(shiny)
library(maelstrom)

PRESETS <- list(
  "Mini groove" = paste(
    'kick <- s("bd(4,8) ~")',
    'snare <- s("~ sd ~ sd")',
    'hats <- s("hh*8")',
    'bass <- note("c2 c2 ~ c2 eb2 ~ g1 ~") |> synth("sawtooth") |> gain(0.6)',
    '',
    'pattern <- stack(kick, snare, hats, bass)',
    '',
    'cps <- 2.0',
    sep = "\n"
  ),
  "Dub FX" = paste(
    'kick <- s("bd ~ bd ~") |> gain(0.8) |> pan(0.3)',
    'snare <- s("~ sd ~ sd") |> pan(0.7)',
    'hats <- s("hh*16") |> swing(0.4) |> gain(0.5)',
    'bass <- note("c2 ~ c2 ~") |> synth("sine") |> lpf(500) |> gain(0.9)',
    'stab <- stack(',
    '  note("~ c4 ~ c4"),',
    '  note("~ eb4 ~ eb4"),',
    '  note("~ g4 ~ g4")',
    ') |> synth("sawtooth") |> lpf(1200) |> attack(0.01) |> gain(0.4) |>',
    '  echo(n = 3, time = 0.375, fb = 0.45) |> pan(0.6)',
    '',
    'pattern <- stack(kick, snare, hats, bass, stab)',
    '',
    'cps <- 0.5',
    sep = "\n"
  ),
  "Euclidean sketch" = paste(
    'drums <- stack(',
    '  s("bd(3,8)") |> humanize(0.008, 0.1),',
    '  s("~ sd(2,8) ~"),',
    '  s("hh(5,8)") |> gain(0.5)',
    ')',
    'harmony <- chord("c3", "eb3", "g3", "bb3") |> arp() |> synth("pluck") |>',
    '  gain(0.6)',
    '',
    'pattern <- stack(drums, harmony)',
    '',
    'cps <- 1.0',
    sep = "\n"
  )
)

MAX_SECONDS <- 30
MAX_ROWS <- 200

## Evaluate pattern code under the CLI contract.
eval_pattern <- function(code) {
  env <- new.env(parent = globalenv())
  err <- tryCatch({
    eval(parse(text = code), envir = env)
    NULL
  }, error = function(e) e)
  if (!is.null(err)) return(list(error = paste0("code error: ", conditionMessage(err))))
  pat <- env$pattern
  if (is.null(pat) || !inherits(pat, "ms_pattern")) {
    return(list(error = "code must define `pattern` as a maelstrom pattern"))
  }
  cps <- if (!is.null(env$cps)) suppressWarnings(as.numeric(env$cps)) else 0.5
  if (is.na(cps) || cps <= 0) cps <- 0.5
  list(pattern = pat, cps = cps)
}

event_table <- function(pat, cycles, cps) {
  haps <- query_arc(pat, 0, cycles)
  if (length(haps) == 0) {
    return(data.frame(onset_sec = numeric(0), dur_sec = numeric(0),
                      detail = character(0)))
  }
  rows <- lapply(haps, function(h) {
    v <- h$value
    span <- if (!is.null(h$whole)) h$whole else h$part
    what <- if (!is.null(v$sound)) paste0("sound=", v$sound)
    else if (!is.null(v$note)) paste0("note=", paste(v$note, collapse = "+"))
    else if (!is.null(v$n)) paste0("n=", paste(v$n, collapse = "+"))
    else "(rest/fx)"
    data.frame(
      onset_sec = round(hap_onset(h) / cps, 3),
      dur_sec = round((span$end - span$begin) / cps, 3),
      detail = what,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

ui <- fluidPage(
  titlePanel("maelstrom playground"),
  p(em("Offline playground, not live-coding: press Render, then play the WAV. ",
       "Code must define ", code("pattern"), " (and may define ", code("cps"), ").")),
  sidebarLayout(
    sidebarPanel(
      selectInput("preset", "Preset", choices = names(PRESETS)),
      actionButton("load", "Load preset"),
      textAreaInput("code", "Pattern code", value = PRESETS[[1]],
                    rows = 14, width = "100%"),
      fluidRow(
        column(6, numericInput("cps", "Tempo (cps)", value = 2, min = 0.05,
                               max = 4, step = 0.05)),
        column(6, numericInput("seconds", "Seconds", value = 8, min = 1,
                               max = MAX_SECONDS, step = 1))
      ),
      fluidRow(
        column(6, checkboxInput("stereo", "Stereo", value = TRUE)),
        column(6, sliderInput("reverb", "Reverb", min = 0, max = 0.6,
                              value = 0.15, step = 0.05))
      ),
      actionButton("validate", "Validate"),
      actionButton("render", "Render + play", class = "btn-primary"),
      downloadButton("download", "Download WAV"),
      verbatimTextOutput("status")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Player", br(), uiOutput("player"), plotOutput("wave")),
        tabPanel("Events", br(), tableOutput("events"))
      )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$load, {
    updateTextAreaInput(session, "code", value = PRESETS[[input$preset]])
  })

  state <- reactiveValues(samples = NULL, sr = 44100, status = "edit code, then Render.")

  validated <- eventReactive(input$validate, {
    res <- eval_pattern(input$code)
    if (!is.null(res$error)) return(res$error)
    haps <- tryCatch(query_arc(res$pattern, 0, 4), error = function(e) e)
    if (inherits(haps, "error")) {
      return(paste0("invalid: ", conditionMessage(haps)))
    }
    n_rest <- sum(vapply(haps, is_rest, logical(1)))
    sprintf("valid: %d events over 4 cycles (%d rests), cps=%g",
            length(haps), n_rest, res$cps)
  })

  observeEvent(input$validate, {
    state$status <- validated()
  })

  observeEvent(input$render, {
    res <- eval_pattern(input$code)
    if (!is.null(res$error)) {
      state$status <- res$error
      return()
    }
    cps <- input$cps
    seconds <- min(max(input$seconds, 1), MAX_SECONDS)
    cycles <- seconds * cps
    samples <- withProgress(message = "Rendering...", value = 0.5, {
      out <- if (isTRUE(input$stereo)) {
        render_stereo(res$pattern, cycles = cycles, cps = cps)
      } else {
        render(res$pattern, cycles = cycles, cps = cps)
      }
      if (input$reverb > 0) out <- add_reverb(out, input$reverb)
      out
    })
    state$samples <- samples
    peak <- if (length(samples)) max(abs(samples)) else 0
    n_events <- length(query_arc(res$pattern, 0, cycles))
    state$status <- sprintf("rendered %.1fs (%g cycles at %g cps), %d events, peak %.3f%s",
                            seconds, cycles, cps, n_events, peak,
                            if (!rust_available()) " [pure-R reverb]" else "")
  })

  output$status <- renderText(state$status)

  output$player <- renderUI({
    if (is.null(state$samples)) return(p("Nothing rendered yet."))
    HTML(audio_tag(state$samples, state$sr))
  })

  output$wave <- renderPlot({
    if (is.null(state$samples)) return()
    x <- state$samples
    if (is.matrix(x)) x <- colMeans(x)
    n <- length(x)
    idx <- unique(round(seq(1, n, length.out = min(n, 4000))))
    plot(idx / state$sr, x[idx], type = "l", xlab = "seconds", ylab = "",
         main = "Waveform")
  })

  output$events <- renderTable({
    res <- eval_pattern(input$code)
    if (!is.null(res$error)) return(data.frame(note = res$error))
    cycles <- min(max(input$seconds, 1), MAX_SECONDS) * input$cps
    head(event_table(res$pattern, cycles, input$cps), MAX_ROWS)
  })

  output$download <- downloadHandler(
    filename = function() "maelstrom.wav",
    content = function(file) {
      if (is.null(state$samples)) stop("nothing rendered yet")
      write_wav(file, state$samples, state$sr)
    }
  )
}

shinyApp(ui, server)
