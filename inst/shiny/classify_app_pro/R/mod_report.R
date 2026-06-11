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
      shiny::h5("Cross-system report"),
      shiny::textInput(ns("title"), "Report title", "soilKey classification"),
      shiny::helpText(
        "The report runs all three keys on the current pedon and embeds the ",
        "horizon table and provenance log."
      ),
      shiny::tags$hr(),
      shiny::downloadButton(ns("html"), "Download HTML",
                            icon = shiny::icon("file-code"),
                            class = "btn-primary w-100"),
      shiny::tags$br(), shiny::tags$br(),
      shiny::downloadButton(ns("pdf"), "Download PDF",
                            icon = shiny::icon("file-pdf"),
                            class = "btn-secondary w-100"),
      shiny::helpText("PDF needs LaTeX; falls back to HTML if unavailable.")
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
        shiny::withProgress(message = "Rendering HTML report...", value = 0.5, {
          soilKey::report(rv$pedon, file = file, format = "html",
                          pedon = rv$pedon, title = input$title,
                          include_family = cf$include_family,
                          specifiers = cf$specifiers)
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
                          specifiers = cf$specifiers)
          "pdf"
        }, error = function(e) "html")
        ext <- if (identical(out, "pdf")) "pdf" else "html"
        sprintf("soilKey_report_%s.%s", safe_id(), ext)
      },
      content = function(file) {
        shiny::req(rv$pedon)
        cf <- cfg()
        shiny::withProgress(message = "Rendering PDF report...", value = 0.5, {
          ok <- tryCatch({
            soilKey::report(rv$pedon, file = file, format = "pdf",
                            pedon = rv$pedon, title = input$title,
                            include_family = cf$include_family,
                            specifiers = cf$specifiers)
            TRUE
          }, error = function(e) FALSE)
          if (!ok) {
            shiny::showNotification(
              "PDF rendering failed (LaTeX missing?) -- wrote HTML instead.",
              type = "warning", duration = 8)
            soilKey::report(rv$pedon, file = file, format = "html",
                            pedon = rv$pedon, title = input$title,
                            include_family = cf$include_family,
                            specifiers = cf$specifiers)
          }
        })
      }
    )

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      bslib::card(
        bslib::card_header("Report preview"),
        bslib::card_body(
          shiny::p("The downloadable report bundles:"),
          shiny::tags$ul(
            shiny::tags$li("WRB 2022, SiBCS 5 and USDA ST 13 results"),
            shiny::tags$li("Per-system key trace and evidence grade"),
            shiny::tags$li("The horizon table and provenance log")
          ),
          # A live checklist of the depth-level options the report will honour,
          # mirroring the Settings tab -- so the user knows what they will get
          # before clicking download.
          shiny::p(class = "mt-2 mb-1",
                   shiny::strong("Active depth-level options:")),
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
        state <- if (isTRUE(on)) "on" else "off"
        shiny::div(class = "small mb-1", icon, " ", label,
                   shiny::tags$span(class = "text-muted", sprintf(" (%s)", state)))
      }
      shiny::tagList(
        opt_row(cf$include_family,
                "USDA family (5th level) prepended to the subgroup"),
        opt_row(cf$specifiers,
                "WRB depth specifiers on depth-anchored qualifiers")
      )
    })

    output$summary <- shiny::renderPrint({
      shiny::req(rv$pedon)
      cat("Pedon:", rv$pedon$site$id %||% "(unnamed)", "\n")
      cat("Horizons:", nrow(rv$pedon$horizons), "\n")
      cat("Provenance rows:",
          if (is.null(rv$pedon$provenance)) 0L else nrow(rv$pedon$provenance),
          "\n")
    })
  })
}
