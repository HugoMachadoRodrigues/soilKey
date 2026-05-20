# =============================================================================
# soilKey Pro -- Spatial prior module (v0.9.97).
#
# Queries a SoilGrids "MostProbable WRB" raster around the profile coordinates
# and reports the spatial distribution of reference soil groups -- a prior to
# sanity-check the deterministic classification against.
# =============================================================================

spatial_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::h5("SoilGrids prior"),
      shiny::uiOutput(ns("coords")),
      shiny::textInput(ns("source_url"), "Raster path or URL",
                       placeholder = "GeoTIFF / COG, or leave blank for test raster"),
      shiny::helpText(
        "Point at a SoilGrids WRB raster (files.isric.org/soilgrids). ",
        "If blank, the option soilKey.test_raster is used (handy for demos)."
      ),
      shiny::numericInput(ns("buffer"), "Buffer radius (m)", 250,
                          min = 100, max = 5000, step = 50),
      shiny::numericInput(ns("topn"), "Keep top N classes", 10,
                          min = 1, max = 30, step = 1),
      shiny::actionButton(ns("run"), "Query spatial prior",
                          icon = shiny::icon("satellite"),
                          class = "btn-primary w-100"),
      shiny::helpText("Requires the 'terra' package.")
    ),
    shiny::uiOutput(ns("body"))
  )
}

spatial_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    prior <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      if (!requireNamespace("terra", quietly = TRUE)) {
        return(simpleError("Package 'terra' is not installed."))
      }
      src <- input$source_url
      src <- if (is.null(src) || !nzchar(trimws(src))) NULL else trimws(src)
      shiny::withProgress(message = "Querying SoilGrids...", value = 0.5, {
        tryCatch(
          soilKey::spatial_prior_soilgrids(
            rv$pedon,
            buffer_m      = input$buffer,
            source_url    = src,
            n_classes_top = input$topn
          ),
          error = function(e) e)
      })
    })

    output$coords <- shiny::renderUI({
      if (is.null(rv$pedon)) {
        return(shiny::div(class = "small text-muted", "No pedon yet."))
      }
      lat <- rv$pedon$site$lat %||% NA
      lon <- rv$pedon$site$lon %||% NA
      shiny::div(class = "small mb-2",
                 shiny::strong("Coordinates: "),
                 sprintf("%s, %s", format(lat), format(lon)))
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      p <- prior()
      if (is.null(p)) {
        return(shiny::div(class = "text-muted p-4 text-center",
                          shiny::icon("satellite"),
                          " Press 'Query spatial prior' to run."))
      }
      if (inherits(p, "error")) {
        return(bslib::card(
          bslib::card_header("Spatial prior unavailable"),
          bslib::card_body(
            shiny::tags$p(class = "text-danger", conditionMessage(p)),
            shiny::helpText(
              "Provide a raster path/URL, or set ",
              shiny::tags$code("options(soilKey.test_raster = '...')"),
              " before launching the app.")
          )
        ))
      }
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          bslib::card_header("RSG probabilities"),
          bslib::card_body(DT::DTOutput(ns("table")))
        ),
        bslib::card(
          bslib::card_header("Distribution"),
          bslib::card_body(plotly::plotlyOutput(ns("plot"), height = "320px"))
        )
      )
    })

    output$table <- DT::renderDT({
      p <- prior()
      shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"), "n/a"))
      DT::datatable(as.data.frame(p), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 12)) |>
        DT::formatPercentage("probability", 1)
    })

    output$plot <- plotly::renderPlotly({
      p <- prior()
      shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"), "n/a"))
      df <- as.data.frame(p)
      df <- df[order(-df$probability), , drop = FALSE]
      plotly::plot_ly(
        df, x = ~probability, y = ~stats::reorder(rsg_code, probability),
        type = "bar", orientation = "h",
        marker = list(color = "#41ab5d")
      ) |>
        plotly::layout(
          xaxis = list(title = "Probability", tickformat = ".0%"),
          yaxis = list(title = ""),
          margin = list(l = 80, t = 20, b = 40)
        )
    })
  })
}
