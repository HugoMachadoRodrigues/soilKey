# =============================================================================
# soilKey Pro -- Unified Map module (v0.9.174).
#
# Replaces the three disjoint, unsynchronised sub-tabs (Point prior / Batch /
# Grid) with ONE square leaflet map driven by a mode selector, all centred on
# the same coordinate:
#
#   * Explore point -- the pedon (or a clicked) point, the surrounding demo
#     profiles, and a SoilGrids WRB class prior sampled at the point.
#   * Batch profiles -- many described profiles classified and mapped by class.
#   * Grid prediction -- a soil-class raster predicted over an area.
#
# A single "SoilGrids overlay" toggle draws the WRB class raster for the visible
# area on the SAME map in every mode, so the user sees the point, its neighbours
# and what SoilGrids provides together. The overlay defaults to a small bundled
# demo raster (www/soilgrids_wrb_demo.tif) so it works with zero configuration;
# a pasted URL or options(soilKey.test_raster=) overrides it.
#
# Batch and grid reuse the pure helpers in mod_map_batch.R / mod_map_grid.R.
# =============================================================================

# Provider tiles offered in the basemap selector (satellite leads).
.map_basemaps <- function() {
  c("Satellite (Esri)" = "Esri.WorldImagery",
    "Streets (OSM)"     = "OpenStreetMap",
    "Light (CartoDB)"   = "CartoDB.Positron",
    "Topographic"       = "OpenTopoMap")
}

.map_valid_ll <- function(lat, lon) {
  is.numeric(lat) && is.numeric(lon) && length(lat) == 1L && length(lon) == 1L &&
    !is.na(lat) && !is.na(lon) && lat >= -90 && lat <= 90 &&
    lon >= -180 && lon <= 180
}

# The real ISRIC SoilGrids WRB "MostProbable" class raster (global, 250 m). Read
# through /vsicurl so terra streams only the needed pixels. Categorical: its
# embedded RAT carries the RSG names, so it is read by LABEL, not the numeric LUT.
.SOILGRIDS_LIVE_VRT <-
  "/vsicurl/https://files.isric.org/soilgrids/latest/data/wrb/MostProbable.vrt"

# Resolve the SoilGrids raster source. Priority: a pasted URL, else the
# test-raster option, else the chosen `kind` -- "live" = real ISRIC raster,
# anything else = the bundled offline demo COG (so the overlay always shows).
.map_soilgrids_source <- function(user_url = NULL, kind = "demo") {
  u <- if (!is.null(user_url) && nzchar(trimws(user_url))) trimws(user_url) else NULL
  if (!is.null(u)) return(u)
  opt <- getOption("soilKey.test_raster", default = NULL)
  if (!is.null(opt) && nzchar(opt)) return(opt)
  if (identical(kind, "live")) return(.SOILGRIDS_LIVE_VRT)
  .pro_demo_asset("soilgrids_wrb_demo.tif")
}

map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 330,

      sk_section(
        i18n("map.mode_title"), icon = "layer-group",
        desc = "One map, three ways to use it -- all centred on the same point.",
        shinyWidgets::radioGroupButtons(
          ns("mode"),
          choices = stats::setNames(
            c("point", "batch", "grid"),
            c(i18n("map.mode_point"), i18n("map.mode_batch"),
              i18n("map.mode_grid"))),
          selected = "point", direction = "vertical", size = "sm"),
        shiny::selectInput(
          ns("basemap"),
          sk_label(i18n("mpoint.base_map"),
                   "Background tiles. Satellite (default) shows the field and landform."),
          choices = .map_basemaps(), selected = "Esri.WorldImagery")
      ),

      # ---- SoilGrids overlay: shared across every mode --------------------
      sk_section(
        i18n("map.soilgrids_overlay"), icon = "map-location-dot",
        desc = "Draw the SoilGrids WRB class prior for the visible area on the map.",
        shiny::checkboxInput(ns("show_soilgrids"),
                             i18n("map.show_soilgrids"), value = TRUE),
        shinyWidgets::radioGroupButtons(
          ns("sg_source"),
          label = sk_label(i18n("map.sg_source"), i18n("map.sg_source_help")),
          choices = stats::setNames(c("demo", "live"),
                                    c(i18n("map.sg_demo"), i18n("map.sg_live"))),
          selected = "demo", justified = TRUE, size = "sm"),
        shiny::textInput(
          ns("source_url"),
          sk_label(i18n("mpoint.soilgrids_raster"),
                   "Optional path/URL of a WRB class raster. Overrides the choice above."),
          placeholder = i18n("mpoint.raster_placeholder")),
        shiny::uiOutput(ns("overlay_note"))
      ),

      # ---- Point mode ----------------------------------------------------
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'point'", ns("mode")),
        sk_section(
          i18n("map.tab_point"), icon = "location-dot",
          desc = "Click the map to drop a point, then read the SoilGrids class prior there.",
          shiny::uiOutput(ns("coords")),
          shiny::selectInput(
            ns("system"), i18n("mpoint.classification_system"),
            choices = c("WRB 2022" = "wrb2022", "USDA ST 13" = "usda",
                        "SiBCS 5" = "sibcs"), selected = "wrb2022"),
          shiny::numericInput(ns("buffer"), i18n("mpoint.buffer_radius"),
                              1000, min = 100, max = 20000, step = 100),
          shiny::numericInput(ns("topn"), i18n("mpoint.keep_top_n"),
                              5, min = 1, max = 30, step = 1),
          bslib::tooltip(
            shiny::actionButton(ns("run_point"), i18n("mpoint.query_prior"),
                                icon = shiny::icon("satellite"),
                                class = "btn-primary w-100"),
            "Read the class prior at the current point and rank the classes.")
        )),

      # ---- Batch mode ----------------------------------------------------
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'batch'", ns("mode")),
        sk_section(
          i18n("map.tab_batch"), icon = "layer-group",
          desc = "Classify many profiles and colour them by class.",
          shinyWidgets::radioGroupButtons(
            ns("batch_source"),
            choices = stats::setNames(c("demo", "upload"),
                                      c(i18n("mbatch.source_demo"),
                                        i18n("mbatch.source_upload"))),
            selected = "demo", justified = TRUE, size = "sm"),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'demo'", ns("mode")),
            shiny::numericInput(ns("n_demo"), i18n("mbatch.n_demo"),
                                12, min = 1, max = 60, step = 1)),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'upload'", ns("batch_source")),
            shiny::fileInput(ns("batch_csv"), i18n("mbatch.long_csv"),
                             accept = ".csv"),
            shiny::downloadLink(ns("batch_template"),
                                i18n("mbatch.download_template"))),
          shiny::selectInput(
            ns("batch_system"), i18n("mbatch.colour_by_system"),
            choices = c("WRB 2022" = "wrb", "SiBCS 5" = "sibcs",
                        "USDA ST 13" = "usda"), selected = "wrb"),
          bslib::tooltip(
            shiny::actionButton(ns("run_batch"), i18n("mbatch.run"),
                                icon = shiny::icon("layer-group"),
                                class = "btn-primary w-100"),
            "Classify each point under all three systems and map them by class."),
          bslib::tooltip(
            shiny::downloadButton(ns("batch_export"), i18n("mbatch.export"),
                                  class = "btn-outline-secondary w-100 mt-2"),
            "Save the classified points as a GeoPackage for GIS."),
          bslib::tooltip(
            shiny::downloadButton(ns("batch_report"), i18n("mbatch.report"),
                                  icon = shiny::icon("file-lines"),
                                  class = "btn-outline-secondary w-100 mt-2"),
            "Download a multi-profile HTML report (map + one page per profile).")
        )),

      # ---- Grid mode -----------------------------------------------------
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'grid'", ns("mode")),
        sk_section(
          i18n("map.tab_grid"), icon = "table-cells",
          desc = "Predict a soil-class raster over an area of interest.",
          shiny::selectInput(
            ns("grid_method"), i18n("mgrid.method"),
            choices = stats::setNames(
              c("covariates", "interpolate", "overlay"),
              c(i18n("mgrid.method_covariates"), i18n("mgrid.method_interpolate"),
                i18n("mgrid.method_overlay"))), selected = "overlay"),
          shiny::selectInput(
            ns("grid_system"), i18n("mgrid.classification_system"),
            choices = c("WRB 2022" = "wrb2022", "SiBCS 5" = "sibcs",
                        "USDA ST 13" = "usda"), selected = "wrb2022"),
          bslib::tooltip(
            shiny::actionButton(ns("grid_use_view"), i18n("mgrid.use_current_view"),
                                icon = shiny::icon("crop"),
                                class = "btn-outline-secondary btn-sm w-100 mb-2"),
            "Use the current map view as the area of interest."),
          shiny::sliderInput(ns("grid_res"), i18n("mgrid.cells_per_side"),
                             min = 8, max = 40, value = 20, step = 1),
          bslib::tooltip(
            shiny::actionButton(ns("run_grid"), i18n("mgrid.predict_grid"),
                                icon = shiny::icon("table-cells"),
                                class = "btn-primary w-100"),
            "Predict the soil class for every grid cell and draw it as a raster."),
          bslib::tooltip(
            shiny::downloadButton(ns("grid_export"), i18n("mgrid.export_geotiff"),
                                  class = "btn-outline-secondary w-100 mt-2"),
            "Download the predicted class grid as a categorical GeoTIFF."),
          shiny::uiOutput(ns("grid_help"))
        ))
    ),
    # ---- one square map + a results panel that changes by mode ------------
    # fill = FALSE (card + body): take the map card OUT of the layout_sidebar
    # flex-fill column, so it sizes to the fixed-height .sk-map-square instead
    # of stretching/shrinking against the results panel below it. This is what
    # makes the map render at a stable size in every mode and both languages.
    bslib::card(
      full_screen = TRUE, fill = FALSE,
      bslib::card_header(shiny::icon("map-location-dot"), " ", i18n("mpoint.location")),
      bslib::card_body(
        padding = 0, fill = FALSE, fillable = FALSE,
        shiny::div(class = "sk-map-square",
                   leaflet::leafletOutput(ns("map"), height = "100%")))
    ),
    shiny::uiOutput(ns("results"))
  )
}

map_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    clicked <- shiny::reactiveVal(NULL)
    coords_r <- shiny::reactive({
      lat <- rv$pedon$site$lat %||% NA; lon <- rv$pedon$site$lon %||% NA
      if (!is.null(rv$pedon) && .map_valid_ll(lat, lon))
        return(list(lat = lat, lon = lon, src = "pedon"))
      cl <- clicked()
      if (!is.null(cl) && .map_valid_ll(cl$lat, cl$lon))
        return(list(lat = cl$lat, lon = cl$lon, src = "click"))
      NULL
    })

    # Neighbouring demo profiles, spread across Brazil, so the point is shown
    # in context (the user's "surrounding points"). Lightweight ON PURPOSE:
    # only id + coordinates, NOT a full classify_all of each (that ran three
    # keys per profile and blocked the flush for ~10 s, leaving the map stuck
    # "recalculating"). Full classification lives in Batch mode.
    nbr <- shiny::reactive({
      peds <- tryCatch(.batch_demo_pedons(9L), error = function(e) NULL)
      if (is.null(peds) || !length(peds)) return(NULL)
      rows <- lapply(peds, function(p) {
        lat <- suppressWarnings(as.numeric(p$site$lat %||% NA))
        lon <- suppressWarnings(as.numeric(p$site$lon %||% NA))
        if (is.na(lat) || is.na(lon)) return(NULL)
        data.frame(id = as.character(p$site$id %||% "profile"),
                   lat = lat, lon = lon, stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      if (!length(rows)) return(NULL)
      do.call(rbind, rows)
    })

    # SoilGrids WRB class raster for a fixed window around a point (or NULL).
    # Shared by the initial render and the update observer so the overlay is
    # identical whichever path draws it.
    overlay_raster <- function(cc, src) {
      if (is.null(cc) || is.null(src)) return(NULL)
      if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
      bb <- list(lon_min = cc$lon - 3, lon_max = cc$lon + 3,
                 lat_min = cc$lat - 3, lat_max = cc$lat + 3)
      tryCatch({
        g <- .grid_make(bb, 60L)
        .grid_to_raster(g$raster, .grid_overlay(g$coords, source_url = src))
      }, error = function(e) NULL)
    }
    # Resolve the source from the live/demo toggle (+ pasted URL) and read the
    # overlay, falling back to the offline demo raster if the live ISRIC fetch
    # fails or returns nothing (network blocked/slow) so the map is never empty.
    sg_fellback <- shiny::reactiveVal(FALSE)
    overlay_for <- function(cc, kind, url) {
      kind <- kind %||% "demo"
      rr <- overlay_raster(cc, .map_soilgrids_source(url, kind))
      fell <- FALSE
      if (is.null(rr) && identical(kind, "live") && !nzchar(url %||% "")) {
        rr <- overlay_raster(cc, .map_soilgrids_source(NULL, "demo"))
        fell <- !is.null(rr)
      }
      list(rr = rr, fell_back = fell)
    }
    add_overlay <- function(map, rr) {
      if (is.null(rr)) return(map)
      lut <- rr$lut
      pal <- leaflet::colorFactor("Set3", domain = lut$id, na.color = "transparent")
      # suppressWarnings: addRasterImage resamples the class grid and warns
      # "values outside the color scale" for the interpolated edges -- benign.
      map <- suppressWarnings(
        leaflet::addRasterImage(map, rr$raster, colors = pal, opacity = 0.55,
                                group = "soilgrids"))
      leaflet::addLegend(map, "bottomleft", colors = pal(lut$id),
                         labels = lut$class, title = i18n("map.soilgrids_overlay"),
                         opacity = 0.85, layerId = "sg_legend")
    }
    add_points <- function(map, cc, nb, mode) {
      if (identical(mode, "point") && !is.null(nb) && nrow(nb))
        map <- map |> leaflet::addCircleMarkers(
          lng = nb$lon, lat = nb$lat, radius = 5, weight = 1,
          color = "#7A5230", fillColor = "#C89B6B", fillOpacity = 0.75,
          group = "neighbours", label = nb$id,
          popup = sprintf("<b>%s</b><br/>%s", nb$id, i18n("map.neighbour_profile")))
      if (!is.null(cc))
        map <- map |> leaflet::addCircleMarkers(
          lng = cc$lon, lat = cc$lat, radius = 8, weight = 2,
          color = "#4A3226", fillColor = "#B5652E", fillOpacity = 0.95,
          group = "site", label = rv$pedon$site$id %||% "point")
      map
    }

    # ---- the single base map ---------------------------------------------
    # Rendered ONCE, SELF-CONTAINED: the initial view, neighbour points, pedon
    # point and SoilGrids overlay are all baked in here (everything isolated).
    # This is deliberate -- proxy calls made before the widget exists on the
    # client are dropped ("Couldn't find map"), so relying on observers for the
    # first paint leaves a blank map. Observers below only handle later updates,
    # by which time the map is live. onRender invalidates the size the moment
    # the (initially hidden) tab becomes visible -- the leaflet-in-a-tab fix.
    output$map <- leaflet::renderLeaflet({
      prov <- shiny::isolate(input$basemap) %||% "Esri.WorldImagery"
      cc   <- shiny::isolate(coords_r())
      nb   <- shiny::isolate(nbr())
      mode <- shiny::isolate(input$mode) %||% "point"
      m <- leaflet::leaflet() |> leaflet::addProviderTiles(prov)
      m <- if (!is.null(cc)) m |> leaflet::setView(cc$lon, cc$lat, zoom = 7)
           else m |> leaflet::setView(-51, -14, zoom = 4)
      m <- add_points(m, cc, nb, mode)
      if (isTRUE(shiny::isolate(input$show_soilgrids)))
        m <- add_overlay(m, overlay_for(cc, shiny::isolate(input$sg_source),
                                        shiny::isolate(input$source_url))$rr)
      if (requireNamespace("htmlwidgets", quietly = TRUE))
        m <- htmlwidgets::onRender(m, "function(el, x) {
          var map = this;
          var fix = function() { map.invalidateSize(); };
          setTimeout(fix, 250); setTimeout(fix, 800);
          if (window.ResizeObserver) { new ResizeObserver(fix).observe(el); }
        }")
      m
    })
    # NB: leave suspendWhenHidden at its default (TRUE). leaflet must render
    # while the tab is VISIBLE (full size) or it initialises at 0x0 and never
    # applies its view/tiles; htmlwidgets also defers renderValue for hidden
    # widgets. Because the initial view/points/overlay are all baked into the
    # render above, render-on-show paints everything without needing a proxy.

    shiny::observeEvent(input$basemap, {
      leaflet::leafletProxy("map", session) |>
        leaflet::clearTiles() |>
        leaflet::addProviderTiles(input$basemap)
    }, ignoreInit = TRUE)

    # ---- steer the view to the active coordinate (updates only) ----------
    shiny::observeEvent(coords_r(), {
      cc <- coords_r(); if (is.null(cc)) return()
      leaflet::leafletProxy("map", session) |>
        leaflet::setView(cc$lon, cc$lat, zoom = 7)
    }, ignoreInit = TRUE)

    # ---- map click moves the point ---------------------------------------
    shiny::observeEvent(input$map_click, {
      pt <- input$map_click
      if (is.null(pt) || !.map_valid_ll(pt$lat, pt$lng)) return()
      if (!is.null(rv$pedon)) {
        # clone: PedonRecord is R6, so `p <- rv$pedon` shares the ref and the
        # self-assign would be a no-op (reactiveValues suppresses identical).
        p <- rv$pedon$clone(deep = TRUE)
        p$site$lat <- pt$lat; p$site$lon <- pt$lng; rv$pedon <- p
      } else clicked(list(lat = pt$lat, lon = pt$lng))
    })

    # ---- pedon point + neighbours (updates only; initial paint is baked in)
    shiny::observeEvent(list(coords_r(), input$mode), {
      cc <- coords_r(); mode <- input$mode %||% "point"
      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("site") |> leaflet::clearGroup("neighbours")
      add_points(proxy, cc, nbr(), mode)
    }, ignoreInit = TRUE)

    # ---- SoilGrids overlay for the visible area (all modes) --------------
    output$overlay_note <- shiny::renderUI({
      if (nzchar(input$source_url %||% "")) return(NULL)   # custom URL: no note
      if (identical(input$sg_source %||% "demo", "live"))
        shiny::helpText(shiny::icon("globe"), " ",
          if (isTRUE(sg_fellback())) i18n("map.live_fell_back")
          else i18n("map.using_live"))
      else
        shiny::helpText(shiny::icon("circle-info"), " ", i18n("map.using_demo_raster"))
    })
    # Redraw the overlay on later toggles/point changes. The FIRST paint is
    # baked into renderLeaflet above; this only handles updates (map is live).
    # The bbox is a fixed window around the point, NOT input$map_bounds --
    # observing bounds while drawing would re-fire in a loop and saturate R.
    shiny::observeEvent(
      list(input$show_soilgrids, input$sg_source, input$source_url, coords_r()), {
        proxy <- leaflet::leafletProxy("map", session) |>
          leaflet::clearGroup("soilgrids") |>
          leaflet::removeControl("sg_legend")
        if (!isTRUE(input$show_soilgrids)) return(invisible())
        res <- shiny::withProgress(
          message = i18n("map.loading_soilgrids"), value = 0.5,
          overlay_for(coords_r(), input$sg_source, input$source_url))
        sg_fellback(res$fell_back)
        add_overlay(proxy, res$rr)
      }, ignoreInit = TRUE, ignoreNULL = FALSE)

    output$coords <- shiny::renderUI({
      cc <- coords_r()
      if (is.null(cc)) return(shiny::div(class = "small text-muted",
                                         i18n("mpoint.no_point_yet")))
      shiny::div(class = "small mb-2", shiny::strong(i18n("mpoint.point_label")),
                 sprintf("%.5f, %.5f", cc$lat, cc$lon))
    })

    # ======================================================================
    #  POINT MODE -- SoilGrids class prior at the point
    # ======================================================================
    prior <- shiny::eventReactive(input$run_point, {
      cc <- coords_r()
      if (is.null(cc)) return(simpleError(i18n("mpoint.place_point_first")))
      if (!requireNamespace("terra", quietly = TRUE))
        return(simpleError(i18n("mpoint.terra_not_installed")))
      src <- .map_soilgrids_source(input$source_url, input$sg_source)
      shiny::withProgress(message = i18n("mpoint.querying_prior"), value = 0.5, {
        tryCatch(soilKey::soil_classes_at_location(
          lat = cc$lat, lon = cc$lon, system = input$system,
          buffer_m = input$buffer, source_url = src,
          top_n = input$topn, verbose = FALSE), error = function(e) e)
      })
    })

    # ======================================================================
    #  BATCH MODE -- classify many profiles, colour by class
    # ======================================================================
    batch_pedons <- shiny::reactiveVal(NULL)
    batch <- shiny::eventReactive(input$run_batch, {
      on_missing <- tryCatch(settings()$on_missing, error = function(e) NULL) %||% "silent"
      shiny::withProgress(message = i18n("mbatch.classifying"), value = 0, {
        peds <- tryCatch({
          if (identical(input$batch_source, "upload")) {
            f <- input$batch_csv
            if (is.null(f)) return(simpleError(i18n("mbatch.upload_first")))
            .batch_parse_csv(utils::read.csv(f$datapath, stringsAsFactors = FALSE))
          } else .batch_demo_pedons(input$n_demo %||% 12L)
        }, error = function(e) e)
        if (inherits(peds, "error")) return(peds)
        batch_pedons(peds)
        .batch_classify(peds, on_missing = on_missing,
                        bump = function(i, n) shiny::incProgress(1 / n))
      })
    })
    shiny::observeEvent(batch(), {
      res <- batch(); sysc <- paste0(input$batch_system %||% "wrb", "_class")
      sysn <- paste0(input$batch_system %||% "wrb", "_name")
      proxy <- leaflet::leafletProxy("map", session) |> leaflet::clearGroup("batch")
      if (inherits(res, "error") || is.null(res) || !nrow(res)) return()
      pal <- leaflet::colorFactor("Set3", domain = sort(unique(res[[sysc]])),
                                  na.color = "#bdbdbd")
      proxy |> leaflet::addCircleMarkers(
        lng = res$lon, lat = res$lat, radius = 7, weight = 1, color = "#333",
        fillOpacity = 0.85, fillColor = pal(res[[sysc]]), group = "batch",
        label = res$id, popup = sprintf(
          "<b>%s</b><br/>WRB: %s<br/>SiBCS: %s<br/>USDA: %s",
          res$id, res$wrb_name, res$sibcs_name, res$usda_name)) |>
        leaflet::fitBounds(min(res$lon), min(res$lat), max(res$lon), max(res$lat))
    })
    output$batch_template <- shiny::downloadHandler(
      filename = function() "soilKey_batch_template.csv",
      content = function(file) writeLines(.batch_starter_csv, file))
    output$batch_export <- shiny::downloadHandler(
      filename = function() "soilkey_soil_map.gpkg",
      content = function(file) {
        res <- batch()
        if (inherits(res, "error") || is.null(res) || !nrow(res))
          stop(i18n("mbatch.nothing_to_export"))
        if (!requireNamespace("sf", quietly = TRUE)) stop(i18n("mbatch.sf_required"))
        sf::st_write(sf::st_as_sf(res, coords = c("lon", "lat"), crs = 4326,
                                  remove = FALSE), file, layer = "soil_points",
                     delete_dsn = TRUE, quiet = TRUE)
      })
    output$batch_report <- shiny::downloadHandler(
      filename = function() "soilkey_profiles_report.html",
      content = function(file) {
        peds <- batch_pedons()
        if (is.null(peds) || !length(peds)) stop(i18n("mbatch.no_profiles_report"))
        soilKey::report_html(peds, file = file)
      })

    # ======================================================================
    #  GRID MODE -- predicted soil-class raster over an area
    # ======================================================================
    grid_bbox <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$grid_use_view, {
      b <- input$map_bounds; shiny::req(b)
      grid_bbox(list(lon_min = b$west, lon_max = b$east,
                     lat_min = b$south, lat_max = b$north))
    })
    grid_result <- shiny::eventReactive(input$run_grid, {
      if (!requireNamespace("terra", quietly = TRUE))
        return(simpleError(i18n("mgrid.err_no_terra")))
      cc <- coords_r()
      bb <- grid_bbox() %||% (if (!is.null(input$map_bounds)) {
        b <- input$map_bounds
        list(lon_min = b$west, lon_max = b$east,
             lat_min = b$south, lat_max = b$north)
      } else if (!is.null(cc))
        list(lon_min = cc$lon - 2, lon_max = cc$lon + 2,
             lat_min = cc$lat - 2, lat_max = cc$lat + 2) else NULL)
      if (is.null(bb)) return(simpleError(i18n("mgrid.err_invalid_bbox")))
      src <- .map_soilgrids_source(input$source_url, input$sg_source)
      shiny::withProgress(message = i18n("mgrid.predicting_grid"), value = 0, {
        tryCatch({
          g <- .grid_make(bb, min(40L, input$grid_res %||% 20L))
          codes <- switch(input$grid_method %||% "overlay",
            covariates = .grid_classify_covariates(
              g$coords, system = input$grid_system,
              bump = function(f, l) shiny::setProgress(f, detail = l)),
            interpolate = .grid_interpolate(
              g$coords, batch() %||% .batch_classify(.batch_demo_pedons(16L),
                                                     on_missing = "silent"),
              paste0(sub("2022", "", input$grid_system), "_class")),
            overlay = .grid_overlay(g$coords, source_url = src))
          rr <- .grid_to_raster(g$raster, codes)
          if (is.null(rr)) return(simpleError(i18n("mgrid.err_no_classes")))
          rr
        }, error = function(e) e)
      })
    })
    shiny::observeEvent(grid_result(), {
      rr <- grid_result()
      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("gridpred") |> leaflet::removeControl("grid_legend")
      if (inherits(rr, "error") || is.null(rr)) return()
      lut <- rr$lut  # data.frame(id, class); raster values ARE the ids
      pal <- leaflet::colorFactor("Set3", domain = lut$id, na.color = "transparent")
      proxy |>
        leaflet::addRasterImage(rr$raster, colors = pal, opacity = 0.7,
                                group = "gridpred") |>
        leaflet::addLegend("bottomright", colors = pal(lut$id), labels = lut$class,
                           title = i18n("mgrid.predicted_class_raster"),
                           opacity = 0.9, layerId = "grid_legend")
    })
    output$grid_export <- shiny::downloadHandler(
      filename = function() "soilkey_grid_prediction.tif",
      content = function(file) {
        rr <- grid_result()
        if (inherits(rr, "error") || is.null(rr)) stop(i18n("mgrid.err_no_classes"))
        terra::writeRaster(rr$raster, file, overwrite = TRUE, datatype = "INT2U")
      })
    output$grid_help <- shiny::renderUI(shiny::helpText(
      switch(input$grid_method %||% "overlay",
             covariates = i18n("mgrid.help_covariates"),
             interpolate = i18n("mgrid.help_interpolate"),
             overlay = i18n("mgrid.help_overlay"))))

    # ======================================================================
    #  Results panel (changes by mode)
    # ======================================================================
    output$results <- shiny::renderUI({
      mode <- input$mode %||% "point"
      if (mode == "point")
        bslib::layout_column_wrap(
          width = 1 / 2, class = "mt-2",
          bslib::card(bslib::card_header(i18n("mpoint.class_distribution")),
                      bslib::card_body(DT::DTOutput(ns("dist_table")))),
          bslib::card(bslib::card_header(i18n("mpoint.typical_attributes")),
                      bslib::card_body(DT::DTOutput(ns("attrs_table")))))
      else if (mode == "batch")
        bslib::card(class = "mt-2",
          bslib::card_header(i18n("mbatch.classified_points")),
          bslib::card_body(DT::DTOutput(ns("batch_table"))))
      else
        bslib::card(class = "mt-2",
          bslib::card_header(i18n("mgrid.class_summary")),
          bslib::card_body(DT::DTOutput(ns("grid_summary"))))
    })

    output$dist_table <- DT::renderDT({
      p <- prior(); shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"),
        if (inherits(p, "error")) conditionMessage(p) else i18n("mpoint.na")))
      df <- as.data.frame(p$distribution)
      shiny::validate(shiny::need(nrow(df) > 0L, i18n("mpoint.no_pixels_buffer")))
      cols <- intersect(c("rsg_code", "rsg_name", "probability"), names(df))
      df <- df[, cols, drop = FALSE]
      DT::datatable(df, rownames = FALSE, options = list(dom = "tp", pageLength = 8)) |>
        DT::formatPercentage("probability", 1)
    })
    output$attrs_table <- DT::renderDT({
      p <- prior(); shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"), i18n("mpoint.na")))
      DT::datatable(as.data.frame(p$typical_attributes), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })
    output$batch_table <- DT::renderDT({
      res <- batch(); shiny::req(res)
      shiny::validate(shiny::need(!inherits(res, "error"),
        if (inherits(res, "error")) conditionMessage(res) else "n/a"))
      shiny::validate(shiny::need(nrow(res) > 0L, i18n("mbatch.no_classifiable")))
      show <- res[, c("id", "lat", "lon", "wrb_name", "sibcs_name", "usda_name"),
                  drop = FALSE]
      names(show) <- c(i18n("mbatch.col_id"), i18n("mbatch.col_lat"),
                       i18n("mbatch.col_lon"), "WRB 2022", "SiBCS 5", "USDA ST 13")
      DT::datatable(show, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })
    output$grid_summary <- DT::renderDT({
      rr <- grid_result(); shiny::req(rr)
      shiny::validate(shiny::need(!inherits(rr, "error"),
        if (inherits(rr, "error")) conditionMessage(rr) else "n/a"))
      v   <- terra::values(rr$raster)[, 1]
      tab <- as.data.frame(table(v), stringsAsFactors = FALSE)
      names(tab) <- c("id", "cells"); tab$id <- as.integer(tab$id)
      tab <- merge(rr$lut, tab, by = "id", all.x = TRUE)
      tab$cells[is.na(tab$cells)] <- 0L
      tab$share <- tab$cells / sum(tab$cells)
      show <- tab[order(-tab$cells), c("class", "cells", "share")]
      names(show) <- c(i18n("mgrid.col_class"), i18n("mgrid.col_cells"),
                       i18n("mgrid.col_share"))
      DT::datatable(show, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 12)) |>
        DT::formatPercentage(i18n("mgrid.col_share"), 1)
    })
  })
}
