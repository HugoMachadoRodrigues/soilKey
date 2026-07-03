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
      sk_section(
        i18n("spectra.step1_attach"),
        icon = "wave-square",
        desc = "Upload a Vis-NIR reflectance CSV and attach it to the current pedon.",
        shiny::fileInput(
          ns("vnir_csv"),
          sk_label(i18n("spectra.vnir_csv_label"),
                   "CSV of reflectance: one row per horizon, columns are wavelengths (nm). Row order must match the pedon's horizons."),
          accept = c(".csv")),
        shiny::helpText(
          i18n("spectra.help_one_row")
        ),
        bslib::tooltip(
          shiny::actionButton(ns("attach"), i18n("spectra.attach_to_pedon"),
                              icon = shiny::icon("paperclip"),
                              class = "btn-secondary w-100"),
          "Attach the uploaded spectra matrix to the pedon so it can be used for prediction."),
        shiny::div(
          class = "mt-2 small",
          bslib::tooltip(
            shiny::actionLink(ns("demo_spectrum"), i18n("spectra.use_demo"),
                              icon = shiny::icon("wand-magic-sparkles")),
            "Attaches a bundled Vis-NIR demo spectrum (5 horizons -- matches the example Ferralsol)."))
      ),
      shiny::tags$hr(),
      sk_section(
        i18n("spectra.step2_gapfill"),
        icon = "wand-magic-sparkles",
        desc = "Predict missing horizon attributes from the attached spectra via OSSL.",
        shiny::selectInput(
          ns("method"),
          sk_label(i18n("spectra.prediction_method"),
                   "How predictions are made: memory-based learning, a local PLSR fit, or a pretrained OSSL model."),
          choices = stats::setNames(
            c("mbl", "plsr_local", "pretrained"),
            c(i18n("spectra.method_mbl"),
              i18n("spectra.method_plsr_local"),
              i18n("spectra.method_pretrained"))),
          selected = "mbl"),
        shiny::selectInput(
          ns("region"),
          sk_label(i18n("spectra.ossl_region"),
                   "OSSL subset used as reference. A region closer to your samples usually predicts better than global."),
          choices = c("global", "south_america",
                      "north_america", "europe", "africa"),
          selected = "global"),
        shiny::checkboxInput(
          ns("overwrite"),
          sk_label(i18n("spectra.overwrite_existing"),
                   "If ticked, spectral predictions replace measured values; otherwise only empty attributes are filled."),
          value = FALSE),
        bslib::tooltip(
          shiny::actionButton(ns("fill"), i18n("spectra.gapfill_from_spectra"),
                              icon = shiny::icon("wand-magic-sparkles"),
                              class = "btn-primary w-100"),
          "Run the OSSL spectral engine and fill missing attributes; filled values are tagged predicted_spectra in the provenance ledger."),
        shiny::helpText(
          i18n("spectra.help_first_use")
        )
      )
    ),
    shiny::uiOutput(ns("body"))
  )
}

spectra_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # ---- attach a spectrum (shared by the upload and the demo) -----------
    do_attach <- function(path) {
      if (is.null(rv$pedon)) {
        shiny::showNotification(i18n("spectra.build_pedon_first"), type = "warning")
        return(invisible())
      }
      if (is.null(path) || !file.exists(path)) {
        shiny::showNotification(i18n("spectra.choose_vnir_csv_first"), type = "warning")
        return(invisible())
      }
      mat <- tryCatch({
        m <- as.matrix(utils::read.csv(path, check.names = FALSE))
        storage.mode(m) <- "double"
        m
      }, error = function(e) e)
      if (inherits(mat, "error")) {
        shiny::showNotification(
          i18n("spectra.could_not_read", conditionMessage(mat)),
          type = "error")
        return(invisible())
      }
      nh <- nrow(rv$pedon$horizons)
      if (nrow(mat) != nh) {
        shiny::showNotification(
          i18n("spectra.row_count_mismatch", nrow(mat), nh),
          type = "error", duration = 8)
        return(invisible())
      }
      rv$pedon$spectra <- list(vnir = mat)
      rv$pedon <- rv$pedon
      shiny::showNotification(
        i18n("spectra.attached_matrix", nrow(mat), ncol(mat)),
        type = "message")
    }

    shiny::observeEvent(input$attach, {
      f <- input$vnir_csv
      do_attach(if (is.null(f)) NULL else f$datapath)
    })

    # Bundled demo spectrum -- a one-click way to see the gap-fill pipeline run
    # without any data (matches the example Ferralsol's 5 horizons).
    shiny::observeEvent(input$demo_spectrum, {
      p <- .pro_demo_asset("demo_spectrum.csv")
      if (is.null(p)) {
        shiny::showNotification(i18n("spectra.demo_missing"), type = "error")
        return(invisible())
      }
      do_attach(p)
    })

    # ---- gap-fill ---------------------------------------------------------
    shiny::observeEvent(input$fill, {
      if (is.null(rv$pedon)) {
        shiny::showNotification(i18n("spectra.build_pedon_first"), type = "warning")
        return(invisible())
      }
      if (is.null(rv$pedon$spectra) || is.null(rv$pedon$spectra$vnir)) {
        shiny::showNotification(i18n("spectra.attach_spectrum_first"),
                                type = "warning")
        return(invisible())
      }
      shiny::withProgress(message = i18n("spectra.predicting_progress"), value = 0.4, {
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
          i18n("spectra.gapfill_failed", conditionMessage(res)),
          type = "error", duration = 12)
        return(invisible())
      }
      rv$pedon <- rv$pedon
      shiny::showNotification(i18n("spectra.gapfill_done"),
                              type = "message")
    })

    # ---- body -------------------------------------------------------------
    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      bslib::layout_column_wrap(
        width = 1,
        bslib::card(
          bslib::card_header(i18n("spectra.card_status")),
          bslib::card_body(shiny::verbatimTextOutput(ns("status")))
        ),
        bslib::card(
          bslib::card_header(i18n("spectra.card_attached_spectrum")),
          bslib::card_body(plotly::plotlyOutput(ns("spectrum"), height = "300px"))
        ),
        bslib::card(
          bslib::card_header(i18n("spectra.card_attributes")),
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
        i18n("spectra.status_none")
      } else {
        m <- sp$vnir
        i18n("spectra.status_attached", nrow(m), ncol(m))
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
