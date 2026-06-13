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

# Mean self-reported confidence of the Munsell colours the VLM extracted, read
# from the provenance ledger (cols attribute / source / confidence). Only the
# munsell_* rows tagged extracted_vlm count. Returns NA before any extraction.
.photo_mean_confidence <- function(pedon) {
  if (is.null(pedon) || is.null(pedon$provenance)) return(NA_real_)
  pr <- as.data.frame(pedon$provenance)
  if (!all(c("attribute", "source", "confidence") %in% names(pr)))
    return(NA_real_)
  keep <- grepl("^munsell_", pr$attribute) & pr$source == "extracted_vlm"
  vals <- suppressWarnings(as.numeric(pr$confidence[keep]))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(NA_real_)
  mean(vals)
}

# Map a [0,1] confidence to the same A-E evidence ladder the badges use, so a
# VLM extraction reads on the same scale as the rest of the app.
.photo_confidence_grade <- function(conf) {
  if (is.null(conf) || is.na(conf)) return(NA_character_)
  if (conf >= 0.85) "A" else if (conf >= 0.70) "B" else
    if (conf >= 0.55) "C" else if (conf >= 0.40) "D" else "E"
}

# Resolve the provider: a live ellmer chat from options, else a mock.
.photo_provider <- function(mode, mock_responses) {
  if (identical(mode, "live")) {
    live <- getOption("soilKey.vlm_chat", default = NULL)
    if (is.null(live)) {
      stop(i18n("photo.live_needs_chat"), call. = FALSE)
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
      shiny::h5(i18n("photo.step1_provider")),
      shinyWidgets::radioGroupButtons(
        ns("provider"), NULL,
        choices = stats::setNames(
          c("mock", "live"),
          c(i18n("photo.provider_demo"), i18n("photo.provider_live"))
        ),
        selected = "mock", justified = TRUE, size = "sm"
      ),
      shiny::helpText(
        i18n("photo.provider_help")
      ),
      shiny::tags$hr(),
      shiny::h5(i18n("photo.step2_munsell")),
      shiny::fileInput(ns("profile_img"), i18n("photo.profile_photograph"),
                       accept = c(".jpg", ".jpeg", ".png")),
      shiny::actionButton(ns("run_munsell"), i18n("photo.extract_munsell"),
                          icon = shiny::icon("eye-dropper"),
                          class = "btn-primary w-100"),
      shiny::tags$hr(),
      shiny::h5(i18n("photo.step3_site")),
      shiny::fileInput(ns("sheet_img"), i18n("photo.field_sheet_image"),
                       accept = c(".jpg", ".jpeg", ".png")),
      shiny::actionButton(ns("run_site"), i18n("photo.extract_site"),
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
        shiny::showNotification(i18n("photo.build_pedon_first"), type = "warning")
        return(invisible())
      }
      f <- input$profile_img
      if (is.null(f)) {
        shiny::showNotification(i18n("photo.choose_profile_photo_first"),
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
      shiny::withProgress(message = i18n("photo.extracting_munsell"), value = 0.5, {
        res <- tryCatch(
          soilKey::extract_munsell_from_photo(rv$pedon, f$datapath, provider),
          error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(
          i18n("photo.extraction_failed", conditionMessage(res)),
          type = "error", duration = 10)
        return(invisible())
      }
      rv$pedon <- rv$pedon                 # bump reactive (R6 mutated in place)
      ex <- attr(res, "vlm_extraction")
      add_log(i18n("photo.log_munsell_extraction",
                   format(Sys.time(), "%H:%M:%S"),
                   ex$fields_added %||% 0L, ex$attempts %||% 1L))
      shiny::showNotification(i18n("photo.munsell_merged"),
                              type = "message")
    })

    # ---- site extraction --------------------------------------------------
    shiny::observeEvent(input$run_site, {
      if (is.null(rv$pedon)) {
        shiny::showNotification(i18n("photo.build_pedon_first"), type = "warning")
        return(invisible())
      }
      f <- input$sheet_img
      if (is.null(f)) {
        shiny::showNotification(i18n("photo.choose_field_sheet_first"),
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
      shiny::withProgress(message = i18n("photo.extracting_site"), value = 0.5, {
        res <- tryCatch(
          soilKey::extract_site_from_fieldsheet(rv$pedon, f$datapath, provider),
          error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(
          i18n("photo.extraction_failed", conditionMessage(res)),
          type = "error", duration = 10)
        return(invisible())
      }
      rv$pedon <- rv$pedon
      add_log(i18n("photo.log_site_extraction",
                   format(Sys.time(), "%H:%M:%S")))
      shiny::showNotification(i18n("photo.site_merged"),
                              type = "message")
    })

    # ---- body -------------------------------------------------------------
    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          bslib::card_header(i18n("photo.card_profile_photo")),
          bslib::card_body(
            shiny::uiOutput(ns("img_caption")),
            shiny::imageOutput(ns("profile_preview"), height = "260px")
          )
        ),
        bslib::card(
          bslib::card_header(i18n("photo.card_munsell_in_pedon")),
          bslib::card_body(DT::DTOutput(ns("munsell_table")))
        ),
        bslib::card(
          bslib::card_header(i18n("photo.card_extraction_log")),
          bslib::card_body(shiny::verbatimTextOutput(ns("log")))
        )
      )
    })

    # A small transparent PNG, written once via base graphics, shown before any
    # upload (avoids a broken-image icon while keeping a valid <img> in place).
    blank_png <- local({
      path <- NULL
      function() {
        if (is.null(path)) {
          path <<- tempfile(fileext = ".png")
          grDevices::png(path, width = 1, height = 1, bg = "transparent")
          graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
          grDevices::dev.off()
        }
        path
      }
    })

    # ---- uploaded profile-photo thumbnail ---------------------------------
    # A small preview so the user can confirm the right image is queued before
    # spending a (potentially paid) VLM call on it. deleteFile = FALSE: the
    # path is Shiny's own upload temp file (owned by the fileInput) or our
    # cached transparent placeholder -- neither should be deleted after serving.
    output$profile_preview <- shiny::renderImage({
      f <- input$profile_img
      if (is.null(f)) {
        return(list(src = blank_png(), contentType = "image/png",
                    width = 1, height = 1, alt = i18n("photo.alt_no_photo")))
      }
      list(src = f$datapath,
           contentType = f$type %||% "image/jpeg",
           width = "100%", alt = i18n("photo.alt_uploaded_photo"))
    }, deleteFile = FALSE)

    # Caption: filename + the mean extraction confidence as a coloured badge,
    # once a Munsell extraction has populated the horizons.
    output$img_caption <- shiny::renderUI({
      f <- input$profile_img
      if (is.null(f))
        return(shiny::div(class = "small text-muted mb-2",
                          i18n("photo.upload_in_sidebar")))
      conf <- .photo_mean_confidence(rv$pedon)
      grade <- .photo_confidence_grade(conf)
      shiny::div(
        class = "small mb-2 d-flex justify-content-between align-items-center",
        shiny::span(shiny::icon("image"), " ", f$name),
        if (!is.na(conf)) shiny::span(
          pro_grade_badge(grade),
          shiny::tags$span(class = "text-muted ms-1",
                           i18n("photo.pct_conf", 100 * conf)))
      )
    })

    output$munsell_table <- DT::renderDT({
      shiny::req(rv$pedon)
      h <- as.data.frame(rv$pedon$horizons)
      cols <- intersect(c("designation", "top_cm", "bottom_cm",
                          "munsell_hue_moist", "munsell_value_moist",
                          "munsell_chroma_moist"), names(h))
      if (length(cols) == 0L) {
        return(DT::datatable(
          stats::setNames(data.frame(i18n("photo.no_horizons")),
                          i18n("photo.note_col")),
          rownames = FALSE, options = list(dom = "t")))
      }
      DT::datatable(h[, cols, drop = FALSE], rownames = FALSE,
                    options = list(dom = "tp", pageLength = 10))
    })

    output$log <- shiny::renderText({
      lg <- log_msg()
      if (length(lg) == 0L) i18n("photo.no_extraction_yet")
      else paste(lg, collapse = "\n")
    })
  })
}
