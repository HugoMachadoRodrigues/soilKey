# =============================================================================
# soilKey Pro -- Spectra / OSSL gap-fill module (v0.9.97).
#
# Attach a Vis-NIR spectrum (rows = horizons, columns = wavelengths) to the
# pedon, then gap-fill missing horizon attributes against the Open Soil
# Spectral Library. Filled values enter the provenance ledger tagged
# "predicted_spectra".
# =============================================================================

spectra_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 320,
      shiny::h5("1. Attach a spectrum"),
      shiny::fileInput(ns("vnir_csv"), "Vis-NIR CSV (row = horizon)",
                       accept = c(".csv")),
      shiny::helpText(
        "One row per horizon, one column per wavelength. Row count must ",
        "match the number of horizons in the pedon."
      ),
      shiny::actionButton(ns("attach"), "Attach to pedon",
                          icon = shiny::icon("paperclip"),
                          class = "btn-secondary w-100"),
      shiny::tags$hr(),
      shiny::h5("2. Gap-fill options"),
      shiny::selectInput(ns("method"), "Prediction method",
                         choices = c("Memory-based learning" = "mbl",
                                     "Local PLSR" = "plsr_local",
                                     "Pre-trained PLSR" = "pretrained"),
                         selected = "mbl"),
      shiny::selectInput(ns("region"), "OSSL region",
                         choices = c("global", "south_america",
                                     "north_america", "europe", "africa"),
                         selected = "global"),
      shiny::checkboxInput(ns("overwrite"), "Overwrite existing values",
                           value = FALSE),
      shiny::actionButton(ns("fill"), "Gap-fill from spectra",
                          icon = shiny::icon("wand-magic-sparkles"),
                          class = "btn-primary w-100"),
      shiny::helpText(
        "First use downloads an OSSL cache; this can take a minute and ",
        "needs network access."
      )
    ),
    shiny::uiOutput(ns("body"))
  )
}

spectra_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- attach an uploaded spectrum -------------------------------------
    shiny::observeEvent(input$attach, {
      if (is.null(rv$pedon)) {
        shiny::showNotification("Build a pedon first.", type = "warning")
        return(invisible())
      }
      f <- input$vnir_csv
      if (is.null(f)) {
        shiny::showNotification("Choose a Vis-NIR CSV first.", type = "warning")
        return(invisible())
      }
      mat <- tryCatch({
        m <- as.matrix(utils::read.csv(f$datapath, check.names = FALSE))
        storage.mode(m) <- "double"
        m
      }, error = function(e) e)
      if (inherits(mat, "error")) {
        shiny::showNotification(
          paste("Could not read spectrum:", conditionMessage(mat)),
          type = "error")
        return(invisible())
      }
      nh <- nrow(rv$pedon$horizons)
      if (nrow(mat) != nh) {
        shiny::showNotification(
          sprintf("Spectrum has %d rows but the pedon has %d horizons.",
                  nrow(mat), nh),
          type = "error", duration = 8)
        return(invisible())
      }
      rv$pedon$spectra <- list(vnir = mat)
      rv$pedon <- rv$pedon
      shiny::showNotification(
        sprintf("Attached a %d x %d Vis-NIR matrix.", nrow(mat), ncol(mat)),
        type = "message")
    })

    # ---- gap-fill ---------------------------------------------------------
    shiny::observeEvent(input$fill, {
      if (is.null(rv$pedon)) {
        shiny::showNotification("Build a pedon first.", type = "warning")
        return(invisible())
      }
      if (is.null(rv$pedon$spectra) || is.null(rv$pedon$spectra$vnir)) {
        shiny::showNotification("Attach a Vis-NIR spectrum first.",
                                type = "warning")
        return(invisible())
      }
      shiny::withProgress(message = "Predicting from spectra...", value = 0.4, {
        res <- tryCatch(
          soilKey::fill_from_spectra(
            rv$pedon,
            method  = input$method,
            region  = input$region,
            overwrite = isTRUE(input$overwrite),
            verbose = FALSE
          ),
          error = function(e) e)
      })
      if (inherits(res, "error")) {
        shiny::showNotification(
          paste("Gap-fill failed:", conditionMessage(res)),
          type = "error", duration = 12)
        return(invisible())
      }
      rv$pedon <- rv$pedon
      shiny::showNotification("Horizon attributes gap-filled from spectra.",
                              type = "message")
    })

    # ---- body -------------------------------------------------------------
    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          bslib::card_header("Spectrum status"),
          bslib::card_body(shiny::verbatimTextOutput(ns("status")))
        ),
        bslib::card(
          bslib::card_header("Attached Vis-NIR spectrum"),
          bslib::card_body(plotly::plotlyOutput(ns("spectrum"), height = "300px"))
        ),
        bslib::card(
          bslib::card_header("Horizon attributes (post gap-fill)"),
          bslib::card_body(DT::DTOutput(ns("attr_table")))
        )
      )
    })

    # ---- the attached spectrum, one trace per horizon ---------------------
    output$spectrum <- plotly::renderPlotly({
      shiny::req(rv$pedon)
      sp <- rv$pedon$spectra
      mat <- if (!is.null(sp)) sp$vnir else NULL
      desig <- if (!is.null(rv$pedon$horizons))
        as.data.frame(rv$pedon$horizons)$designation else NULL
      pro_spectrum_plot(mat, designations = desig)
    })

    output$status <- shiny::renderText({
      shiny::req(rv$pedon)
      sp <- rv$pedon$spectra
      if (is.null(sp) || is.null(sp$vnir)) {
        "No spectrum attached. Upload a Vis-NIR CSV and press 'Attach'."
      } else {
        m <- sp$vnir
        sprintf("Vis-NIR matrix attached: %d horizon row(s) x %d wavelength(s).",
                nrow(m), ncol(m))
      }
    })

    output$attr_table <- DT::renderDT({
      shiny::req(rv$pedon)
      h <- as.data.frame(rv$pedon$horizons)
      cols <- intersect(c("designation", "clay_pct", "sand_pct", "silt_pct",
                          "cec_cmol", "bs_pct", "ph_h2o", "oc_pct"),
                        names(h))
      DT::datatable(h[, cols, drop = FALSE], rownames = FALSE,
                    options = list(dom = "tp", pageLength = 12, scrollX = TRUE))
    })
  })
}
