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
      sk_section(
        i18n("spatial.title"),
        desc = "Sample a SoilGrids WRB raster around the point for a spatial prior to cross-check the key.",
        icon = "map-location-dot",
        shiny::uiOutput(ns("coords"))
      ),
      sk_section(
        i18n("spatial.raster_path_label"),
        desc = "Point to a local SoilGrids MostProbable raster; leave blank to use the packaged default.",
        icon = "layer-group",
        shiny::textInput(
          ns("source_url"),
          sk_label(
            i18n("spatial.raster_path_label"),
            "File path or URL of a SoilGrids MostProbable WRB raster. Leave empty to use the built-in source."
          ),
          placeholder = i18n("spatial.raster_path_placeholder")
        ),
        shiny::helpText(
          i18n("spatial.raster_help")
        )
      ),
      sk_section(
        i18n("spatial.buffer_label"),
        desc = "Set how the raster is sampled around the point and how many classes to report.",
        icon = "sliders",
        shiny::numericInput(
          ns("buffer"),
          sk_label(
            i18n("spatial.buffer_label"),
            "Radius in metres of the neighbourhood sampled around the point; larger values average over more terrain."
          ),
          250, min = 100, max = 5000, step = 50
        ),
        shiny::numericInput(
          ns("topn"),
          sk_label(
            i18n("spatial.topn_label"),
            "How many of the most probable reference soil groups to keep and display in the results."
          ),
          10, min = 1, max = 30, step = 1
        )
      ),
      bslib::tooltip(
        shiny::actionButton(ns("run"), i18n("spatial.run"),
                            icon = shiny::icon("satellite"),
                            class = "btn-primary w-100"),
        "Query the raster at the coordinates and show the spatial distribution of reference soil groups."
      ),
      shiny::helpText(i18n("spatial.requires_terra"))
    ),
    # The location map is ALWAYS shown -- base tiles + the profile point and its
    # sampling buffer. The SoilGrids class prior (optional) is overlaid below it
    # when a raster is configured; otherwise a small caption explains it.
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(shiny::icon("map-location-dot"), " Location"),
      bslib::card_body(
        leaflet::leafletOutput(ns("map"), height = "360px"),
        class = "p-0"
      )
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

    # The location map is ALWAYS rendered: base tiles + the profile point and its
    # sampling buffer. It does not depend on a SoilGrids raster, so the Spatial
    # tab never shows a blank / error-only screen.
    output$map <- leaflet::renderLeaflet({
      lat <- if (!is.null(rv$pedon)) suppressWarnings(as.numeric(rv$pedon$site$lat)) else NA_real_
      lon <- if (!is.null(rv$pedon)) suppressWarnings(as.numeric(rv$pedon$site$lon)) else NA_real_
      m <- leaflet::leaflet() |> leaflet::addProviderTiles("CartoDB.Positron")
      if (is.finite(lat) && is.finite(lon)) {
        buf <- suppressWarnings(as.numeric(input$buffer)); if (!is.finite(buf)) buf <- 250
        id  <- rv$pedon$site$id %||% "pedon"
        m |>
          leaflet::setView(lng = lon, lat = lat, zoom = 9) |>
          leaflet::addCircles(lng = lon, lat = lat, radius = buf,
                              color = "#B5652E", weight = 1, fillOpacity = 0.12) |>
          leaflet::addCircleMarkers(lng = lon, lat = lat, radius = 6,
                              color = "#4A3226", weight = 1.5,
                              fillColor = "#7A5230", fillOpacity = 0.95,
                              label = sprintf("%s  (%.3f, %.3f)", id, lat, lon))
      } else {
        m |> leaflet::setView(lng = -51, lat = -14, zoom = 3)   # Brazil default
      }
    })

    output$body <- shiny::renderUI({
      ns <- session$ns
      if (is.null(rv$pedon)) {
        return(shiny::helpText(class = "mt-3", i18n("spatial.no_pedon_yet")))
      }
      p <- prior()
      if (is.null(p)) {
        return(shiny::helpText(class = "mt-3",
                               shiny::icon("satellite"), " ",
                               i18n("spatial.press_to_run")))
      }
      if (inherits(p, "error")) {
        # A small caption BELOW the always-present map -- never a full-screen
        # error. The prior is optional; the map above still shows the point.
        return(shiny::div(
          class = "alert alert-light border small mt-3",
          shiny::icon("circle-info", class = "text-secondary"), " ",
          shiny::tags$strong(i18n("spatial.unavailable_header")), " ",
          i18n("spatial.unavailable_body"), " ",
          i18n("spatial.provide_raster_pre"),
          shiny::tags$code("options(soilKey.test_raster = '...')"),
          i18n("spatial.provide_raster_post"),
          shiny::tags$details(
            class = "mt-1",
            shiny::tags$summary(i18n("spatial.unavailable_details")),
            shiny::tags$code(conditionMessage(p)))
        ))
      }
      bslib::layout_column_wrap(
        width = 1 / 2, class = "mt-3",
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
