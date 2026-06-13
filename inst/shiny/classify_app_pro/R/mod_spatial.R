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
      shiny::h5(i18n("spatial.title")),
      shiny::uiOutput(ns("coords")),
      shiny::textInput(ns("source_url"), i18n("spatial.raster_path_label"),
                       placeholder = i18n("spatial.raster_path_placeholder")),
      shiny::helpText(
        i18n("spatial.raster_help")
      ),
      shiny::numericInput(ns("buffer"), i18n("spatial.buffer_label"), 250,
                          min = 100, max = 5000, step = 50),
      shiny::numericInput(ns("topn"), i18n("spatial.topn_label"), 10,
                          min = 1, max = 30, step = 1),
      shiny::actionButton(ns("run"), i18n("spatial.run"),
                          icon = shiny::icon("satellite"),
                          class = "btn-primary w-100"),
      shiny::helpText(i18n("spatial.requires_terra"))
    ),
    shiny::uiOutput(ns("body"))
  )
}

spatial_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    prior <- shiny::eventReactive(input$run, {
      shiny::req(rv$pedon)
      if (!requireNamespace("terra", quietly = TRUE)) {
        return(simpleError(i18n("spatial.terra_not_installed")))
      }
      src <- input$source_url
      src <- if (is.null(src) || !nzchar(trimws(src))) NULL else trimws(src)
      shiny::withProgress(message = i18n("spatial.querying"), value = 0.5, {
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
        return(shiny::div(class = "small text-muted", i18n("spatial.no_pedon_yet")))
      }
      lat <- rv$pedon$site$lat %||% NA
      lon <- rv$pedon$site$lon %||% NA
      shiny::div(class = "small mb-2",
                 shiny::strong(i18n("spatial.coordinates")),
                 sprintf("%s, %s", format(lat), format(lon)))
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) return(pro_no_pedon_msg())
      p <- prior()
      if (is.null(p)) {
        return(shiny::div(class = "text-muted p-4 text-center",
                          shiny::icon("satellite"),
                          i18n("spatial.press_to_run")))
      }
      if (inherits(p, "error")) {
        return(bslib::card(
          bslib::card_header(i18n("spatial.unavailable_header")),
          bslib::card_body(
            shiny::tags$p(class = "text-danger", conditionMessage(p)),
            shiny::helpText(
              i18n("spatial.provide_raster_pre"),
              shiny::tags$code("options(soilKey.test_raster = '...')"),
              i18n("spatial.provide_raster_post"))
          )
        ))
      }
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          bslib::card_header(i18n("spatial.rsg_probabilities")),
          bslib::card_body(DT::DTOutput(ns("table")))
        ),
        bslib::card(
          bslib::card_header(i18n("spatial.distribution")),
          bslib::card_body(plotly::plotlyOutput(ns("plot"), height = "320px"))
        )
      )
    })

    output$table <- DT::renderDT({
      p <- prior()
      shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"), i18n("spatial.na")))
      DT::datatable(as.data.frame(p), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 12)) |>
        DT::formatPercentage("probability", 1)
    })

    output$plot <- plotly::renderPlotly({
      p <- prior()
      shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"), i18n("spatial.na")))
      df <- as.data.frame(p)
      df <- df[order(-df$probability), , drop = FALSE]
      plotly::plot_ly(
        df, x = ~probability, y = ~stats::reorder(rsg_code, probability),
        type = "bar", orientation = "h",
        marker = list(color = "#41ab5d")
      ) |>
        plotly::layout(
          xaxis = list(title = i18n("spatial.probability"), tickformat = ".0%"),
          yaxis = list(title = ""),
          margin = list(l = 80, t = 20, b = 40)
        )
    })
  })
}
