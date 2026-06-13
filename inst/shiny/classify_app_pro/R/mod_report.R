# =============================================================================
# soilKey Pro -- Report module (v0.9.97).
#
# Renders a self-contained cross-system report (WRB / SiBCS / USDA plus the
# horizon table and provenance log) and offers it as an HTML or PDF download.
# PDF needs a working LaTeX install; if it is missing the module falls back
# to HTML and tells the user.
# =============================================================================

report_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      shiny::h5(i18n("report.title")),
      shiny::textInput(ns("title"), i18n("report.report_title_label"), "soilKey classification"),
      shiny::helpText(
        i18n("report.help_runs_all_keys")
      ),
      shiny::tags$hr(),
      shiny::downloadButton(ns("html"), i18n("report.download_html"),
                            icon = shiny::icon("file-code"),
                            class = "btn-primary w-100"),
      shiny::tags$br(), shiny::tags$br(),
      shiny::downloadButton(ns("pdf"), i18n("report.download_pdf"),
                            icon = shiny::icon("file-pdf"),
                            class = "btn-secondary w-100"),
      shiny::helpText(i18n("report.pdf_needs_latex"))
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

    output$html <- shiny::downloadHandler(
      filename = function() sprintf("soilKey_report_%s.html", safe_id()),
      content  = function(file) {
        shiny::req(rv$pedon)
        cf <- cfg()
        shiny::withProgress(message = i18n("report.rendering_html"), value = 0.5, {
          soilKey::report(rv$pedon, file = file, format = "html",
                          pedon = rv$pedon, title = input$title,
                          include_family = cf$include_family,
                          specifiers = cf$specifiers, lang = .sk_app_lang())
        })
      }
    )

    output$pdf <- shiny::downloadHandler(
      filename = function() {
        cf <- cfg()
        out <- tryCatch({
          tmp <- tempfile(fileext = ".pdf")
          soilKey::report(rv$pedon, file = tmp, format = "pdf",
                          pedon = rv$pedon, title = input$title,
                          include_family = cf$include_family,
                          specifiers = cf$specifiers, lang = .sk_app_lang())
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
            soilKey::report(rv$pedon, file = file, format = "pdf",
                            pedon = rv$pedon, title = input$title,
                            include_family = cf$include_family,
                            specifiers = cf$specifiers, lang = .sk_app_lang())
            TRUE
          }, error = function(e) FALSE)
          if (!ok) {
            shiny::showNotification(
              i18n("report.pdf_failed_fallback"),
              type = "warning", duration = 8)
            soilKey::report(rv$pedon, file = file, format = "html",
                            pedon = rv$pedon, title = input$title,
                            include_family = cf$include_family,
                            specifiers = cf$specifiers, lang = .sk_app_lang())
          }
        })
      }
    )

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      bslib::card(
        bslib::card_header(i18n("report.preview")),
        bslib::card_body(
          shiny::p(i18n("report.bundles_intro")),
          shiny::tags$ul(
            shiny::tags$li(i18n("report.bundle_results")),
            shiny::tags$li(i18n("report.bundle_trace")),
            shiny::tags$li(i18n("report.bundle_table_log"))
          ),
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
