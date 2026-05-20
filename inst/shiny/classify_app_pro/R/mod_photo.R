# =============================================================================
# soilKey Pro -- Photo / VLM extraction module (v0.9.97).
#
# Demonstrates the multimodal extraction pipeline: a profile photo yields
# Munsell colour per horizon, a field-sheet image yields site metadata. The
# default "Demo" provider is MockVLMProvider, which returns a canned, schema-
# valid response so the pipeline runs offline with no API key. A live ellmer
# chat object can be supplied through options(soilKey.vlm_chat = <chat>).
#
# The taxonomic key is never delegated to a model -- extraction only fills the
# PedonRecord; classification stays deterministic.
# =============================================================================

# Canned, schema-valid Munsell response for the demo provider.
.photo_mock_munsell <- function() {
  paste0(
    '{"horizons":[',
    '{"top_cm":0,"bottom_cm":15,"designation":"A",',
    '"munsell_moist":{"hue":"2.5YR","value":3,"chroma":4,',
    '"confidence":0.55,"source_quote":"uppermost ~15 cm next to Munsell card"}},',
    '{"top_cm":15,"bottom_cm":65,"designation":"Bw1",',
    '"munsell_moist":{"hue":"2.5YR","value":3,"chroma":6,',
    '"confidence":0.6,"source_quote":"mid profile, diffuse light"}},',
    '{"top_cm":65,"bottom_cm":150,"designation":"Bw2",',
    '"munsell_moist":{"hue":"10R","value":3,"chroma":6,',
    '"confidence":0.5,"source_quote":"lower profile near card"}}',
    ']}'
  )
}

# Canned, schema-valid site response for the demo provider.
.photo_mock_site <- function() {
  paste0(
    '{"lat":{"value":-22.74,"confidence":0.7,"source_quote":"GPS field sheet"},',
    '"lon":{"value":-43.68,"confidence":0.7,"source_quote":"GPS field sheet"},',
    '"elevation_m":{"value":420,"confidence":0.6,"source_quote":"altimeter"},',
    '"drainage_class":{"value":"well drained","confidence":0.55,',
    '"source_quote":"drainage box ticked"}}'
  )
}

# Resolve the provider: a live ellmer chat from options, else a mock.
.photo_provider <- function(mode, mock_responses) {
  if (identical(mode, "live")) {
    live <- getOption("soilKey.vlm_chat", default = NULL)
    if (is.null(live)) {
      stop("Live mode needs a configured ellmer chat. In R, run e.g. ",
           "options(soilKey.vlm_chat = ellmer::chat_anthropic()) before ",
           "launching the app.", call. = FALSE)
    }
    return(live)
  }
  soilKey::MockVLMProvider$new(responses = mock_responses)
}

photo_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::h5("1. Provider"),
      shinyWidgets::radioGroupButtons(
        ns("provider"), NULL,
        choices = c("Demo (mock)" = "mock", "Live (ellmer)" = "live"),
        selected = "mock", justified = TRUE, size = "sm"
      ),
      shiny::helpText(
        "Demo returns a canned, schema-valid response so the pipeline runs ",
        "offline. Live needs options(soilKey.vlm_chat = <ellmer chat>)."
      ),
      shiny::tags$hr(),
      shiny::h5("2. Profile photo -> Munsell"),
      shiny::fileInput(ns("profile_img"), "Profile photograph",
                       accept = c(".jpg", ".jpeg", ".png")),
      shiny::actionButton(ns("run_munsell"), "Extract Munsell colour",
                          icon = shiny::icon("eye-dropper"),
                          class = "btn-primary w-100"),
      shiny::tags$hr(),
      shiny::h5("3. Field sheet -> site"),
      shiny::fileInput(ns("sheet_img"), "Field-sheet image",
                       accept = c(".jpg", ".jpeg", ".png")),
      shiny::actionButton(ns("run_site"), "Extract site metadata",
                          icon = shiny::icon("map-pin"),
                          class = "btn-secondary w-100")
    ),
    shiny::uiOutput(ns("body"))
  )
}

photo_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    log_msg <- shiny::reactiveVal(character(0))
    add_log <- function(...) log_msg(c(log_msg(), paste0(...)))

    # ---- Munsell extraction ----------------------------------------------
    shiny::observeEvent(input$run_munsell, {
      if (is.null(rv$pedon)) {
        shiny::showNotification("Build a pedon first.", type = "warning")
        return(invisible())
      }
      f <- input$profile_img
      if (is.null(f)) {
        shiny::showNotification("Choose a profile photo first.",
                                type = "warning")
        return(invisible())
      }
      provider <- tryCatch(
        .photo_provider(input$provider,
                        rep(list(.photo_mock_munsell()), 3L)),
        error = function(e) e)
      if (inherits(provider, "error")) {
        shiny::showNotification(conditionMessage(provider),
                                type = "error", duration = 10)
        return(invisible())
      }
      shiny::withProgress(message = "Extracting Munsell colour...", value = 0.5, {
        res <- tryCatch(
          soilKey::extract_munsell_from_photo(rv$pedon, f$datapath, provider),
          error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(
          paste("Extraction failed:", conditionMessage(res)),
          type = "error", duration = 10)
        return(invisible())
      }
      rv$pedon <- rv$pedon                 # bump reactive (R6 mutated in place)
      ex <- attr(res, "vlm_extraction")
      add_log(sprintf("[%s] Munsell extraction: %d field(s) added across %d ",
                      format(Sys.time(), "%H:%M:%S"),
                      ex$fields_added %||% 0L, ex$attempts %||% 1L),
              "attempt(s).")
      shiny::showNotification("Munsell colour merged into the pedon.",
                              type = "message")
    })

    # ---- site extraction --------------------------------------------------
    shiny::observeEvent(input$run_site, {
      if (is.null(rv$pedon)) {
        shiny::showNotification("Build a pedon first.", type = "warning")
        return(invisible())
      }
      f <- input$sheet_img
      if (is.null(f)) {
        shiny::showNotification("Choose a field-sheet image first.",
                                type = "warning")
        return(invisible())
      }
      provider <- tryCatch(
        .photo_provider(input$provider,
                        rep(list(.photo_mock_site()), 3L)),
        error = function(e) e)
      if (inherits(provider, "error")) {
        shiny::showNotification(conditionMessage(provider),
                                type = "error", duration = 10)
        return(invisible())
      }
      shiny::withProgress(message = "Extracting site metadata...", value = 0.5, {
        res <- tryCatch(
          soilKey::extract_site_from_fieldsheet(rv$pedon, f$datapath, provider),
          error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(
          paste("Extraction failed:", conditionMessage(res)),
          type = "error", duration = 10)
        return(invisible())
      }
      rv$pedon <- rv$pedon
      add_log(sprintf("[%s] Site metadata extraction complete.",
                      format(Sys.time(), "%H:%M:%S")))
      shiny::showNotification("Site metadata merged into the pedon.",
                              type = "message")
    })

    # ---- body -------------------------------------------------------------
    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          bslib::card_header("Munsell colour in the pedon"),
          bslib::card_body(DT::DTOutput(ns("munsell_table")))
        ),
        bslib::card(
          bslib::card_header("Extraction log"),
          bslib::card_body(shiny::verbatimTextOutput(ns("log")))
        )
      )
    })

    output$munsell_table <- DT::renderDT({
      shiny::req(rv$pedon)
      h <- as.data.frame(rv$pedon$horizons)
      cols <- intersect(c("designation", "top_cm", "bottom_cm",
                          "munsell_hue_moist", "munsell_value_moist",
                          "munsell_chroma_moist"), names(h))
      if (length(cols) == 0L) {
        return(DT::datatable(data.frame(Note = "No horizons"),
                             rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(h[, cols, drop = FALSE], rownames = FALSE,
                    options = list(dom = "tp", pageLength = 10))
    })

    output$log <- shiny::renderText({
      lg <- log_msg()
      if (length(lg) == 0L) "(no extraction run yet)"
      else paste(lg, collapse = "\n")
    })
  })
}
