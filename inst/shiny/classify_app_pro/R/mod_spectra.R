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
      # ---- spectral preprocessing (live preview; saved for the report) -----
      sk_section(
        i18n("spectra.step_preproc"),
        icon = "sliders",
        desc = "Treat the spectrum: absorbance, then Savitzky-Golay smoothing / derivative. It re-plots as you tick, and the sequence is saved for the report.",
        shiny::checkboxInput(
          ns("pp_absorbance"),
          sk_label(i18n("spectra.pp_absorbance"),
                   "Convert reflectance R to absorbance A = log10(1/R) (auto-scales % to a 0-1 fraction)."),
          value = FALSE),
        shiny::checkboxInput(
          ns("pp_smooth"),
          sk_label(i18n("spectra.pp_smooth"),
                   "Savitzky-Golay smoothing, applied before any derivative."),
          value = FALSE),
        shiny::radioButtons(
          ns("pp_deriv"), i18n("spectra.pp_derivative"),
          choices = stats::setNames(
            c("none", "1", "2"),
            c(i18n("spectra.pp_deriv_none"), i18n("spectra.pp_deriv_1"),
              i18n("spectra.pp_deriv_2"))),
          selected = "none", inline = TRUE),
        bslib::accordion(
          open = FALSE, class = "mt-1",
          bslib::accordion_panel(
            i18n("spectra.pp_advanced"), icon = shiny::icon("gear"),
            shiny::numericInput(
              ns("pp_window"),
              sk_label(i18n("spectra.pp_window"),
                       "Savitzky-Golay window (odd, >= poly + 2). Wider = smoother."),
              value = 11, min = 5, max = 51, step = 2),
            shiny::numericInput(
              ns("pp_poly"),
              sk_label(i18n("spectra.pp_poly"), "Savitzky-Golay polynomial order."),
              value = 2, min = 1, max = 5, step = 1)))
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
    # v0.9.173: the result cards are STATIC (not inside a renderUI). Nesting the
    # plotly spectrum plot inside output$body (a renderUI depending on rv$pedon)
    # tore its DOM node down every time the demo/attach reassigned rv$pedon, so
    # the plot rendered blank. Kept static, the plotly node is created once and
    # simply redraws; a conditionalPanel on the has_pedon flag swaps the
    # no-pedon placeholder in/out without touching the plot node.
    shiny::conditionalPanel(
      "!output.has_pedon", ns = ns,
      pro_no_pedon_msg()),
    shiny::conditionalPanel(
      "output.has_pedon", ns = ns,
      bslib::layout_column_wrap(
        width = 1,
        # spectrum card: the status is a slim chip in the header (no big card),
        # and the applied treatment sequence sits just under the header.
        bslib::card(
          bslib::card_header(shiny::div(
            class = "d-flex justify-content-between align-items-center",
            shiny::span(i18n("spectra.card_attached_spectrum")),
            shiny::uiOutput(ns("status_chip"), inline = TRUE))),
          bslib::card_body(
            shiny::uiOutput(ns("preproc_sequence")),
            plotly::plotlyOutput(ns("spectrum"), height = "320px"))),
        bslib::card(
          bslib::card_header(i18n("spectra.card_attributes")),
          bslib::card_body(DT::DTOutput(ns("attr_table"))))
      )
    )
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
      # PedonRecord is R6 (a reference). Mutating in place then self-assigning
      # `rv$pedon <- rv$pedon` reassigns the SAME environment, which
      # reactiveValues treats as identical and SUPPRESSES -> no invalidation ->
      # the spectrum plot / status never re-render. Clone to a fresh reference.
      p <- rv$pedon$clone(deep = TRUE)
      p$spectra <- list(vnir = mat)
      rv$pedon <- p
      shiny::showNotification(
        i18n("spectra.attached_matrix", nrow(mat), ncol(mat)),
        type = "message")
    }

    shiny::observeEvent(input$attach, {
      f <- input$vnir_csv
      do_attach(if (is.null(f)) NULL else f$datapath)
    })

    # Bundled demo spectrum -- a one-click way to see the gap-fill pipeline run
    # without any data. The demo matrix is sized to the CURRENT pedon's horizon
    # count (recycling the bundled rows), so it works whatever the pedon is --
    # e.g. after a photo extraction has changed the horizon count.
    shiny::observeEvent(input$demo_spectrum, {
      if (is.null(rv$pedon)) {
        shiny::showNotification(i18n("spectra.build_pedon_first"), type = "warning")
        return(invisible())
      }
      m <- .pro_demo_spectrum(nrow(rv$pedon$horizons))
      if (is.null(m)) {
        shiny::showNotification(i18n("spectra.demo_missing"), type = "error")
        return(invisible())
      }
      p <- rv$pedon$clone(deep = TRUE)   # fresh ref so the plot re-renders (R6)
      p$spectra <- list(vnir = m)
      rv$pedon <- p
      shiny::showNotification(
        i18n("spectra.attached_matrix", nrow(m), ncol(m)), type = "message")
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
      # fill_from_spectra() mutates the pedon in place (invisible(pedon)); clone
      # to a fresh ref so the reactive fires and the filled attributes show.
      rv$pedon <- rv$pedon$clone(deep = TRUE)
      shiny::showNotification(i18n("spectra.gapfill_done"),
                              type = "message")
    })

    # ---- has-pedon flag drives the static conditionalPanels ---------------
    output$has_pedon <- shiny::reactive(!is.null(rv$pedon))
    shiny::outputOptions(output, "has_pedon", suspendWhenHidden = FALSE)

    # ---- spectral preprocessing pipeline (live preview) -------------------
    pp_opts <- shiny::reactive(list(
      absorbance    = isTRUE(input$pp_absorbance),
      sg_smooth     = isTRUE(input$pp_smooth),
      sg_derivative = switch(input$pp_deriv %||% "none", "1" = 1L, "2" = 2L, 0L),
      window        = as.integer(input$pp_window %||% 11L),
      poly          = as.integer(input$pp_poly   %||% 2L)))

    # Resolve the preprocessing engine from the LOADED soilKey namespace -- via
    # asNamespace so it works whether the function is exported or internal, and
    # returns NULL (rather than erroring) if an OLD soilKey without the engine is
    # installed. Prevents the ugly "'apply_spectral_preprocessing' is not an
    # exported object" breadcrumb; the plot then just shows raw reflectance.
    .preproc_fn <- function() {
      ns <- tryCatch(asNamespace("soilKey"), error = function(e) NULL)
      if (!is.null(ns) &&
          exists("apply_spectral_preprocessing", envir = ns, inherits = FALSE))
        get("apply_spectral_preprocessing", envir = ns)
      else NULL
    }

    # the treated spectrum (matrix + wavelengths + ordered step labels)
    treated <- shiny::reactive({
      shiny::req(rv$pedon)
      mat <- rv$pedon$spectra$vnir
      if (is.null(mat)) return(NULL)
      fn <- .preproc_fn()
      raw <- list(X = mat, wavelengths = NULL, steps = "Reflectance")
      if (is.null(fn)) return(raw)         # old soilKey -> show raw, no error
      o <- pp_opts()
      tryCatch(
        fn(mat, absorbance = o$absorbance, sg_smooth = o$sg_smooth,
           sg_derivative = o$sg_derivative, window = o$window, poly = o$poly),
        error = function(e) raw)           # any failure -> clean raw fallback
    })

    # y-axis label follows the deepest transform applied
    pp_ylab <- function(o) {
      if (o$sg_derivative == 1L) return(i18n("spectra.ylab_deriv1"))
      if (o$sg_derivative == 2L) return(i18n("spectra.ylab_deriv2"))
      if (isTRUE(o$absorbance))  return(i18n("spectra.ylab_absorbance"))
      NULL  # pro_spectrum_plot defaults to Reflectance
    }

    output$spectrum <- plotly::renderPlotly({
      shiny::req(rv$pedon)
      tr <- treated()
      mat <- if (!is.null(tr)) tr$X else NULL
      desig <- if (!is.null(rv$pedon$horizons))
        as.data.frame(rv$pedon$horizons)$designation else NULL
      pro_spectrum_plot(mat, designations = desig, y_label = pp_ylab(pp_opts()))
    })

    # the applied treatment sequence, as arrow-joined chips under the header
    output$preproc_sequence <- shiny::renderUI({
      shiny::req(rv$pedon)
      tr <- treated(); if (is.null(tr)) return(NULL)
      steps <- tr$steps %||% "Reflectance"
      chips <- lapply(seq_along(steps), function(i) {
        shiny::tagList(
          if (i > 1L) shiny::span(class = "sk-seq-arrow", "→"),
          shiny::span(class = "sk-seq-chip", steps[i]))
      })
      shiny::div(class = "sk-seq mb-2", chips)
    })

    # slim status chip in the card header (replaces the big status card)
    output$status_chip <- shiny::renderUI({
      shiny::req(rv$pedon)
      m <- rv$pedon$spectra$vnir
      if (is.null(m))
        shiny::span(class = "badge rounded-pill bg-secondary-subtle text-secondary-emphasis",
                    i18n("spectra.status_none_short"))
      else
        shiny::span(class = "badge rounded-pill bg-success-subtle text-success-emphasis",
                    shiny::icon("wave-square"),
                    sprintf(" %d × %d", nrow(m), ncol(m)))
    })

    # record the pipeline (debounced) in a LIGHTWEIGHT shared field the report
    # reads -- NOT on rv$pedon, so ticking a box here does not churn the map /
    # classify observers on other tabs. The report module injects it into the
    # pedon copy at render time.
    pp_debounced <- shiny::debounce(pp_opts, 600)
    shiny::observeEvent(pp_debounced(), {
      if (is.null(rv$pedon) || is.null(rv$pedon$spectra$vnir)) {
        rv$spectra_pp <- NULL; return()
      }
      tr <- treated()
      rv$spectra_pp <- list(opts  = pp_debounced(),
                            steps = if (!is.null(tr)) tr$steps else NULL)
    }, ignoreInit = FALSE)

    output$attr_table <- DT::renderDT({
      shiny::req(rv$pedon)
      h <- as.data.frame(rv$pedon$horizons)
      cols <- intersect(c("designation", "clay_pct", "sand_pct", "silt_pct",
                          "cec_cmol", "bs_pct", "ph_h2o", "oc_pct"),
                        names(h))
      DT::datatable(.sk_round2(h[, cols, drop = FALSE]), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 12, scrollX = TRUE))
    })
  })
}
