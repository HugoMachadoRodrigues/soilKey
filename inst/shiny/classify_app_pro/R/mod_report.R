# =============================================================================
# soilKey Pro -- Report module (v0.9.97).
#
# Renders a self-contained cross-system report (WRB / SiBCS / USDA, the
# national systems of the last Classify run, the horizon table and the
# provenance log) and offers it as an HTML download, and as a
# PDF where a LaTeX engine is installed. The hosted app has none, so there the
# PDF button is not shown: the HTML report carries a Print / Save as PDF button
# instead (v0.9.210). Until then a "Download PDF" button handed over an HTML
# file. The download buttons sit in the body of the tab: in the sidebar they
# were out of sight whenever it was collapsed.
# =============================================================================

# Can soilKey::report(format = "pdf") work here? It renders with rmarkdown and
# xelatex.
.report_pdf_available <- function() {
  requireNamespace("rmarkdown", quietly = TRUE) &&
    isTRUE(tryCatch(rmarkdown::pandoc_available(), error = function(e) FALSE)) &&
    (nzchar(Sys.which("xelatex")) ||
       (requireNamespace("tinytex", quietly = TRUE) &&
          isTRUE(tryCatch(tinytex::is_tinytex(), error = function(e) FALSE))))
}

# soilKey::report() runs the three keys in this process: under this session's
# Settings (engine, strict mode), not whatever another visitor last chose.
.sk_report <- function(...) .sk_with_session_opts(soilKey::report(...))

report_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      sk_section(
        i18n("report.title"),
        desc = i18n("report.desc_section"),
        icon = "file-arrow-down",
        shiny::textInput(
          ns("title"),
          sk_label(i18n("report.report_title_label"), i18n("report.help_title")),
          i18n("report.default_title")),
        shiny::helpText(i18n("report.help_runs_all_keys"))
      )
    ),
    shiny::uiOutput(ns("body"))
  )
}

report_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    safe_id <- function() {
      id <- rv$pedon$site$id %||% "pedon"
      gsub("[^A-Za-z0-9_-]", "_", id)
    }

    # The Settings tab owns the two depth-level toggles; the report must honour
    # them so the downloaded file matches what the Classify tab shows. Default
    # to FALSE when settings() has not yet initialised (e.g. first render).
    cfg <- function() {
      s <- tryCatch(settings(), error = function(e) NULL)
      list(
        include_family = isTRUE(s$include_family),
        specifiers     = isTRUE(s$specifiers)
      )
    }

    # The systems the report runs: the three it always has, and the national
    # systems of the last Classify run (rv$classified_systems), so the file
    # has what the Classify tab shows.
    report_systems <- function() {
      nat <- intersect(.sk_national_systems()$id,
                       tryCatch(rv$classified_systems, error = function(e) NULL) %||%
                         character(0))
      c(.SK_DEFAULT_SYSTEMS, nat)
    }

    # The Spectra tab records its preprocessing pipeline in rv$spectra_pp (kept
    # off rv$pedon to avoid cross-tab churn); inject it into the pedon copy so
    # the report shows the treatment sequence that was applied.
    report_pedon <- function() {
      p <- rv$pedon
      pp <- tryCatch(rv$spectra_pp, error = function(e) NULL)
      if (!is.null(p) && !is.null(pp) && !is.null(p$spectra$vnir)) {
        p <- p$clone(deep = TRUE)
        p$spectra$preprocessing <- pp
      }
      p
    }

    sk_download("html",
      filename = function() sprintf("soilKey_report_%s.html", safe_id()),
      content  = function(file) {
        shiny::req(rv$pedon)
        cf <- cfg()
        shiny::withProgress(message = i18n("report.rendering_html"), value = 0.5, {
          .sk_report(report_pedon(), file = file, format = "html",
                          pedon = report_pedon(), title = input$title,
                          include_family = cf$include_family,
                          specifiers = cf$specifiers, lang = .sk_app_lang(),
                          systems = report_systems())
        })
      }
    )

    sk_download("pdf",
      filename = function() {
        cf <- cfg()
        out <- tryCatch({
          tmp <- tempfile(fileext = ".pdf")
          .sk_report(report_pedon(), file = tmp, format = "pdf",
                          pedon = report_pedon(), title = input$title,
                          include_family = cf$include_family,
                          specifiers = cf$specifiers, lang = .sk_app_lang(),
                          systems = report_systems())
          "pdf"
        }, error = function(e) "html")
        ext <- if (identical(out, "pdf")) "pdf" else "html"
        sprintf("soilKey_report_%s.%s", safe_id(), ext)
      },
      content = function(file) {
        shiny::req(rv$pedon)
        cf <- cfg()
        shiny::withProgress(message = i18n("report.rendering_pdf"), value = 0.5, {
          ok <- tryCatch({
            .sk_report(report_pedon(), file = file, format = "pdf",
                            pedon = report_pedon(), title = input$title,
                            include_family = cf$include_family,
                            specifiers = cf$specifiers, lang = .sk_app_lang(),
                            systems = report_systems())
            TRUE
          }, error = function(e) FALSE)
          if (!ok) {
            shiny::showNotification(
              i18n("report.pdf_failed_fallback"),
              type = "warning", duration = 8)
            .sk_report(report_pedon(), file = file, format = "html",
                            pedon = report_pedon(), title = input$title,
                            include_family = cf$include_family,
                            specifiers = cf$specifiers, lang = .sk_app_lang(),
                            systems = report_systems())
          }
        })
      }
    )

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      pdf_ok <- .report_pdf_available()
      bslib::card(
        bslib::card_header(i18n("report.preview")),
        bslib::card_body(
          # the downloads, where they cannot be hidden by a collapsed sidebar
          shiny::div(
            class = "d-flex flex-wrap gap-2 mb-2",
            bslib::tooltip(
              sk_download_button(ns("html"), i18n("report.download_html"),
                                 icon = shiny::icon("file-code"),
                                 class = "btn-primary"),
              i18n("report.tip_html")),
            if (pdf_ok) bslib::tooltip(
              sk_download_button(ns("pdf"), i18n("report.download_pdf"),
                                 icon = shiny::icon("file-pdf"),
                                 class = "btn-secondary"),
              i18n("report.tip_pdf"))),
          if (!pdf_ok) shiny::helpText(shiny::icon("print"), " ",
                                       i18n("report.pdf_via_print")),
          shiny::p(i18n("report.bundles_intro")),
          shiny::tags$ul(
            shiny::tags$li(i18n("report.bundle_results")),
            shiny::tags$li(i18n("report.bundle_trace")),
            shiny::tags$li(i18n("report.bundle_table_log"))
          ),
          # which national systems the file will hold (those of the last run)
          shiny::uiOutput(ns("national")),
          # A live checklist of the depth-level options the report will honour,
          # mirroring the Settings tab -- so the user knows what they will get
          # before clicking download.
          shiny::p(class = "mt-2 mb-1",
                   shiny::strong(i18n("report.active_depth_options"))),
          shiny::uiOutput(ns("opts")),
          shiny::verbatimTextOutput(ns("summary"))
        )
      )
    })

    # v0.9.303: the buttons live in renderUI'd UI. They are action buttons
    # now (sk_download), whose observers run whether the tab is shown or not,
    # so the outputOptions() that kept the old download links bound are gone.

    output$national <- shiny::renderUI({
      nat <- setdiff(report_systems(), .SK_DEFAULT_SYSTEMS)
      if (!length(nat))
        return(shiny::div(class = "small text-muted mb-2",
                          shiny::icon("circle-info"), " ",
                          i18n("report.national_none")))
      shiny::div(class = "small mb-2",
                 shiny::icon("circle-check", class = "text-success"), " ",
                 i18n("report.national_included",
                      paste(.sk_slot_labels(nat), collapse = ", ")))
    })

    # Render one row per optional setting with a check/cross icon.
    output$opts <- shiny::renderUI({
      cf <- cfg()
      opt_row <- function(on, label) {
        icon <- if (isTRUE(on))
          shiny::icon("circle-check", class = "text-success")
        else
          shiny::icon("circle", class = "text-muted")
        state <- if (isTRUE(on)) i18n("report.state_on") else i18n("report.state_off")
        shiny::div(class = "small mb-1", icon, " ", label,
                   shiny::tags$span(class = "text-muted", sprintf(" (%s)", state)))
      }
      shiny::tagList(
        opt_row(cf$include_family,
                i18n("report.opt_family")),
        opt_row(cf$specifiers,
                i18n("report.opt_specifiers"))
      )
    })

    output$summary <- shiny::renderPrint({
      shiny::req(rv$pedon)
      cat(i18n("report.summary_pedon"), rv$pedon$site$id %||% i18n("report.unnamed"), "\n")
      cat(i18n("report.summary_horizons"), nrow(rv$pedon$horizons), "\n")
      cat(i18n("report.summary_provenance_rows"),
          if (is.null(rv$pedon$provenance)) 0L else nrow(rv$pedon$provenance),
          "\n")
    })
  })
}
