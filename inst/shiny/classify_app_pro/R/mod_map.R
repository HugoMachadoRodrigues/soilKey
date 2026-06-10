# =============================================================================
# soilKey Pro -- Interactive map module (v0.9.101).
#
# The app's first cartographic surface. A leaflet map where the user clicks to
# place a point and queries the SoilGrids class prior at that location via
# soil_classes_at_location() -- the same engine the Spatial tab uses, but here
# driven by a coordinate the user can pick on the map rather than typed into
# the pedon.
#
# Phase 1 of the mapping roadmap (point prior). Phase 2 (batch multi-profile
# classification) and Phase 3 (gridded prediction) are tracked in NEWS /
# ARCHITECTURE and are NOT implemented here.
#
# Coordinate state:
#   * If a pedon exists, clicking the map rewrites rv$pedon$site$lat/lon so the
#     Spatial tab stays in sync (PedonRecord is an R6 reference, but reassigning
#     the rv$pedon slot is what invalidates downstream reactives).
#   * If no pedon exists yet, the click is held in a local reactiveVal, so the
#     tab is useful on its own -- soil_classes_at_location() needs only lat/lon.
# =============================================================================

# Default provider tiles offered in the basemap selector.
.map_basemaps <- function() {
  c("Streets (OSM)"        = "OpenStreetMap",
    "Satellite (Esri)"     = "Esri.WorldImagery",
    "Light (CartoDB)"      = "CartoDB.Positron",
    "Topographic"          = "OpenTopoMap")
}

# A coordinate is usable iff finite and inside the WGS-84 envelope.
.map_valid_ll <- function(lat, lon) {
  is.numeric(lat) && is.numeric(lon) &&
    length(lat) == 1L && length(lon) == 1L &&
    !is.na(lat) && !is.na(lon) &&
    lat >= -90 && lat <= 90 && lon >= -180 && lon <= 180
}

map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,
      shiny::h5("Map & spatial prior"),
      shiny::uiOutput(ns("coords")),
      shiny::selectInput(ns("basemap"), "Base map",
                         choices = .map_basemaps(), selected = "OpenStreetMap"),
      shiny::selectInput(ns("system"), "Classification system",
                         choices = c("WRB 2022"     = "wrb2022",
                                     "USDA ST 13"    = "usda",
                                     "SiBCS 5"       = "sibcs"),
                         selected = "wrb2022"),
      shiny::textInput(ns("source_url"), "SoilGrids raster (path or URL)",
                       placeholder = "GeoTIFF / COG, or blank for test raster"),
      shiny::helpText(
        "Point at a SoilGrids WRB raster (files.isric.org/soilgrids). ",
        "If blank, the option soilKey.test_raster is used (handy for demos)."
      ),
      shiny::numericInput(ns("buffer"), "Buffer radius (m)", 1000,
                          min = 100, max = 20000, step = 100),
      shiny::numericInput(ns("topn"), "Keep top N classes", 5,
                          min = 1, max = 30, step = 1),
      shiny::actionButton(ns("run"), "Query prior here",
                          icon = shiny::icon("satellite"),
                          class = "btn-primary w-100"),
      shiny::helpText(
        shiny::icon("hand-pointer"),
        " Click the map to place the point. Requires 'leaflet' and 'terra'."
      )
    ),
    bslib::layout_column_wrap(
      width = 1, heights_equal = "row",
      bslib::card(
        bslib::card_header("Location"),
        bslib::card_body(
          padding = 0,
          leaflet::leafletOutput(ns("map"), height = "460px")
        )
      ),
      bslib::layout_column_wrap(
        width = 1 / 2,
        bslib::card(
          bslib::card_header("Class distribution at point"),
          bslib::card_body(DT::DTOutput(ns("dist_table")))
        ),
        bslib::card(
          bslib::card_header("Typical attributes"),
          bslib::card_body(DT::DTOutput(ns("attrs_table")))
        )
      )
    )
  )
}

map_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Coordinate clicked on the map when no pedon is loaded.
    clicked <- shiny::reactiveVal(NULL)

    # The active coordinate: pedon site if valid, else the last map click.
    coords_r <- shiny::reactive({
      lat <- rv$pedon$site$lat %||% NA
      lon <- rv$pedon$site$lon %||% NA
      if (!is.null(rv$pedon) && .map_valid_ll(lat, lon))
        return(list(lat = lat, lon = lon, src = "pedon"))
      cl <- clicked()
      if (!is.null(cl) && .map_valid_ll(cl$lat, cl$lon))
        return(list(lat = cl$lat, lon = cl$lon, src = "click"))
      NULL
    })

    # ---- base map (rendered once; everything else via leafletProxy) --------
    output$map <- leaflet::renderLeaflet({
      cc   <- shiny::isolate(coords_r())
      prov <- shiny::isolate(input$basemap) %||% "OpenStreetMap"
      m <- leaflet::leaflet() |>
        leaflet::addProviderTiles(prov)
      if (!is.null(cc)) {
        m <- m |>
          leaflet::setView(lng = cc$lon, lat = cc$lat, zoom = 9) |>
          leaflet::addMarkers(lng = cc$lon, lat = cc$lat, layerId = "site")
      } else {
        # Centre on Brazil as a sensible default for this package's audience.
        m <- m |> leaflet::setView(lng = -51, lat = -14, zoom = 4)
      }
      m
    })

    # ---- swap the basemap tiles on demand ----------------------------------
    shiny::observeEvent(input$basemap, {
      leaflet::leafletProxy("map", session) |>
        leaflet::clearTiles() |>
        leaflet::addProviderTiles(input$basemap)
    }, ignoreInit = TRUE)

    # ---- map click -> move the point ---------------------------------------
    shiny::observeEvent(input$map_click, {
      pt <- input$map_click
      if (is.null(pt) || !.map_valid_ll(pt$lat, pt$lng)) return()
      if (!is.null(rv$pedon)) {
        # Rewrite the pedon site so the Spatial tab sees the same coordinate.
        p <- rv$pedon
        p$site$lat <- pt$lat
        p$site$lon <- pt$lng
        rv$pedon <- p
      } else {
        clicked(list(lat = pt$lat, lon = pt$lng))
      }
    })

    # ---- keep the marker on the active coordinate --------------------------
    shiny::observeEvent(coords_r(), {
      cc <- coords_r()
      proxy <- leaflet::leafletProxy("map", session)
      if (is.null(cc)) {
        proxy |> leaflet::removeMarker("site")
      } else {
        proxy |>
          leaflet::clearGroup("site") |>
          leaflet::addMarkers(lng = cc$lon, lat = cc$lat,
                              layerId = "site", group = "site")
      }
    }, ignoreNULL = FALSE)

    # ---- coordinate readout in the sidebar ---------------------------------
    output$coords <- shiny::renderUI({
      cc <- coords_r()
      if (is.null(cc)) {
        return(shiny::div(class = "small text-muted mb-2",
                          "No point yet -- click the map to place one."))
      }
      shiny::div(
        class = "small mb-2",
        shiny::strong("Point: "),
        sprintf("%.5f, %.5f", cc$lat, cc$lon),
        shiny::tags$span(class = "text-muted",
                         sprintf(" (%s)", if (cc$src == "pedon")
                           "from pedon" else "map click"))
      )
    })

    # ---- run the prior at the active coordinate ----------------------------
    prior <- shiny::eventReactive(input$run, {
      cc <- coords_r()
      if (is.null(cc)) return(simpleError("Place a point on the map first."))
      if (!requireNamespace("terra", quietly = TRUE))
        return(simpleError("Package 'terra' is not installed."))
      src <- input$source_url
      src <- if (is.null(src) || !nzchar(trimws(src))) NULL else trimws(src)
      shiny::withProgress(message = "Querying SoilGrids prior...", value = 0.5, {
        tryCatch(
          soilKey::soil_classes_at_location(
            lat        = cc$lat,
            lon        = cc$lon,
            system     = input$system,
            buffer_m   = input$buffer,
            source_url = src,
            top_n      = input$topn,
            verbose    = FALSE
          ),
          error = function(e) e)
      })
    })

    # ---- draw the buffer + a popup once a prior comes back ------------------
    shiny::observeEvent(prior(), {
      p  <- prior()
      cc <- coords_r()
      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("buffer")
      if (inherits(p, "error") || is.null(cc)) return()
      dist <- as.data.frame(p$distribution)
      popup <- if (nrow(dist) == 0L) {
        "No SoilGrids pixels here. Set a raster source."
      } else {
        top <- dist[1, ]
        sprintf("<b>%s</b><br/>%s (%.0f%%)<br/><span style='color:#666'>buffer %g m</span>",
                input$system, top$rsg_name %||% top$rsg_code,
                100 * top$probability, input$buffer)
      }
      proxy |>
        leaflet::addCircles(
          lng = cc$lon, lat = cc$lat, radius = input$buffer,
          group = "buffer", weight = 1, color = "#41ab5d",
          fillColor = "#41ab5d", fillOpacity = 0.15, popup = popup)
    })

    # ---- distribution table ------------------------------------------------
    output$dist_table <- DT::renderDT({
      p <- prior()
      shiny::req(p)
      shiny::validate(
        shiny::need(!inherits(p, "error"),
                    if (inherits(p, "error")) conditionMessage(p) else "n/a"))
      df <- as.data.frame(p$distribution)
      shiny::validate(shiny::need(
        nrow(df) > 0L,
        "No SoilGrids pixels in the buffer. Provide a raster path/URL, or set options(soilKey.test_raster = '...')."))
      # Reorder by name (source order is rsg_code, probability, rsg_name) and
      # give friendly headers, then format the percentage by its final name.
      cols <- intersect(c("rsg_code", "rsg_name", "probability"), names(df))
      df <- df[, cols, drop = FALSE]
      names(df) <- c(rsg_code = "Code", rsg_name = "Class",
                     probability = "Probability")[cols]
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8)) |>
        DT::formatPercentage("Probability", 1)
    })

    # ---- typical-attribute table -------------------------------------------
    output$attrs_table <- DT::renderDT({
      p <- prior()
      shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"), "n/a"))
      df <- as.data.frame(p$typical_attributes)
      shiny::validate(shiny::need(
        nrow(df) > 0L, "Query a point with a configured raster to see typical attributes."))
      DT::datatable(df, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })
  })
}
