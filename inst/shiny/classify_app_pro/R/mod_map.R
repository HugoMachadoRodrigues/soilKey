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
        desc = i18n("map.desc_mode"),
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
                   i18n("map.help_basemap")),
          choices = .map_basemaps(), selected = "Esri.WorldImagery")
      ),

      # ---- SoilGrids overlay: shared across every mode --------------------
      sk_section(
        i18n("map.soilgrids_overlay"), icon = "map-location-dot",
        desc = i18n("map.desc_overlay"),
        shiny::checkboxInput(ns("show_soilgrids"),
                             i18n("map.show_soilgrids"), value = TRUE),
        shinyWidgets::radioGroupButtons(
          ns("sg_source"),
          label = sk_label(i18n("map.sg_source"), i18n("map.sg_source_help")),
          choices = stats::setNames(c("demo", "live"),
                                    c(i18n("map.sg_demo"), i18n("map.sg_live"))),
          selected = "live", justified = TRUE, size = "sm"),
        shiny::textInput(
          ns("source_url"),
          sk_label(i18n("mpoint.soilgrids_raster"),
                   i18n("map.help_source_url")),
          placeholder = i18n("mpoint.raster_placeholder")),
        shiny::uiOutput(ns("overlay_note"))
      ),

      # ---- Point mode ----------------------------------------------------
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'point'", ns("mode")),
        sk_section(
          i18n("map.tab_point"), icon = "location-dot",
          desc = i18n("mpoint.desc"),
          shiny::uiOutput(ns("coords")),
          # The prior IS the SoilGrids WRB MostProbable raster. WRB is native;
          # SiBCS is a published WRB->SiBCS (Schad) translation of the SAME
          # pixels. USDA is intentionally omitted here: there is no USDA
          # SoilGrids layer, so it would mislabel the WRB pixels. (USDA is still
          # available from real profile data on the Classify tab.)
          shiny::selectInput(
            ns("system"),
            sk_label(i18n("mpoint.prior_system"), i18n("mpoint.prior_system_help")),
            choices = stats::setNames(
              c("wrb2022", "sibcs"),
              c(i18n("mpoint.prior_wrb"), i18n("mpoint.prior_sibcs"))),
            selected = "wrb2022"),
          shiny::numericInput(ns("buffer"), i18n("mpoint.buffer_radius"),
                              5000, min = 100, max = 20000, step = 100),
          shiny::numericInput(ns("topn"), i18n("mpoint.keep_top_n"),
                              5, min = 1, max = 30, step = 1),
          bslib::tooltip(
            bslib::input_task_button(ns("run_point"), i18n("mpoint.query_prior"),
                                     icon = shiny::icon("satellite"),
                                     label_busy = i18n("mpoint.querying_prior"),
                                     type = "primary", class = "w-100"),
            i18n("mpoint.tip_query")),
          shiny::conditionalPanel(
            sprintf("input['%s'] == 'live'", ns("sg_source")),
            shiny::helpText(class = "small", shiny::icon("clock"), " ",
                            i18n("mpoint.live_slow")))
        )),

      # ---- Batch mode ----------------------------------------------------
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'batch'", ns("mode")),
        sk_section(
          i18n("map.tab_batch"), icon = "layer-group",
          desc = i18n("mbatch.desc"),
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
            bslib::input_task_button(ns("run_batch"), i18n("mbatch.run"),
                                     icon = shiny::icon("layer-group"),
                                     label_busy = i18n("mbatch.classifying"),
                                     type = "primary", class = "w-100"),
            i18n("mbatch.tip_run")),
          bslib::tooltip(
            shiny::downloadButton(ns("batch_export"), i18n("mbatch.export"),
                                  class = "btn-outline-secondary w-100 mt-2"),
            i18n("mbatch.tip_export")),
          bslib::tooltip(
            shiny::downloadButton(ns("batch_report"), i18n("mbatch.report"),
                                  icon = shiny::icon("file-lines"),
                                  class = "btn-outline-secondary w-100 mt-2"),
            i18n("mbatch.tip_report"))
        )),

      # ---- Grid mode -----------------------------------------------------
      shiny::conditionalPanel(
        sprintf("input['%s'] == 'grid'", ns("mode")),
        sk_section(
          i18n("map.tab_grid"), icon = "table-cells",
          desc = i18n("mgrid.desc"),
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
            i18n("mgrid.tip_use_view")),
          shiny::sliderInput(ns("grid_res"), i18n("mgrid.cells_per_side"),
                             min = 8, max = 40, value = 20, step = 1),
          bslib::tooltip(
            bslib::input_task_button(ns("run_grid"), i18n("mgrid.predict_grid"),
                                     icon = shiny::icon("table-cells"),
                                     label_busy = i18n("mgrid.predicting_grid"),
                                     type = "primary", class = "w-100"),
            i18n("mgrid.tip_predict")),
          bslib::tooltip(
            shiny::downloadButton(ns("grid_export"), i18n("mgrid.export_geotiff"),
                                  class = "btn-outline-secondary w-100 mt-2"),
            i18n("mgrid.tip_export")),
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
    shiny::uiOutput(ns("map_legend_help")),
    shiny::uiOutput(ns("results"))
  )
}

# The helpers the map jobs take to a worker (utils_async.R): the grid and
# overlay code of mod_map_grid.R, which uses only terra, soilKey and itself.
.map_job_helpers <- function() {
  env <- environment(.grid_make)
  mget(ls(env, all.names = TRUE,
          pattern = "^[.](grid_|GRID_|wrb_name_to_code$|overlay_recode$|batch_classify$)"),
       envir = env)
}

# A raster that came back from a worker is packed (terra objects hold external
# pointers and cannot travel as they are).
.map_unwrap <- function(rr) {
  if (!is.null(rr) && inherits(rr$raster, "PackedSpatRaster"))
    rr$raster <- terra::unwrap(rr$raster)
  rr
}

# The SoilGrids WRB class raster for a window around a point, in a background
# worker: the live ISRIC raster takes seconds to read, and in the Shiny process
# that froze every session on the instance. Renders CONTINUOUS patches: crop the
# SOURCE raster (demo 0.04 deg / live ~250 m) to a 6-degree window at NATIVE
# resolution, recode to contiguous integer class ids + a LUT, and let
# addRasterImage draw it -- NOT a coarse point-grid resample. Falls back to
# `fallback_src` (the offline demo) when the first source gives nothing.
.map_overlay_job <- function(lat, lon, src, fallback_src = NULL) {
  read <- function(src) {
    if (is.null(src) || !requireNamespace("terra", quietly = TRUE)) return(NULL)
    tryCatch({
      win <- terra::ext(max(-180, lon - 3), min(180, lon + 3),
                        max(-90,  lat - 3), min(90,  lat + 3))
      if (grepl("vsicurl|^http", src)) .grid_set_gdal_fast()   # fast remote open
      r  <- terra::rast(src)
      # crop in the RASTER's own CRS (live SoilGrids = Homolosine, demo = 4326)
      wv <- terra::project(terra::as.polygons(win, crs = "EPSG:4326"),
                           terra::crs(r))
      rc <- terra::crop(r, wv, snap = "out")
      if (is.null(rc) || terra::ncell(rc) == 0) return(NULL)
      rr <- .overlay_recode(rc)                                # int ids + LUT
      if (is.null(rr)) return(NULL)
      # cap the payload (leaflet addRasterImage maxBytes); modal = class-safe
      fact <- ceiling(max(dim(rr$raster)[1:2]) / 512)
      if (fact > 1)
        rr$raster <- terra::aggregate(rr$raster, fact = fact, fun = "modal")
      if (!terra::same.crs(rr$raster, "EPSG:4326"))
        rr$raster <- terra::project(rr$raster, "EPSG:4326", method = "near")
      if (all(is.na(terra::values(rr$raster, mat = FALSE)))) return(NULL)
      rr$raster <- terra::wrap(rr$raster)
      rr
    }, error = function(e) NULL)
  }
  rr <- read(src)
  fell <- FALSE
  if (is.null(rr) && !is.null(fallback_src)) {
    rr <- read(fallback_src)
    fell <- !is.null(rr)
  }
  list(rr = rr, fell_back = fell)
}

# A predicted soil-class grid over a bounding box, in a background worker.
.map_grid_job <- function(bb, n, method, system, src,
                          points_df = NULL, class_col = NULL, pedons = NULL) {
  # interpolation without a classified batch: classify the demo points here
  if (identical(method, "interpolate") && is.null(points_df) && length(pedons))
    points_df <- .batch_classify(pedons, on_missing = "silent")
  g <- .grid_make(bb, n)
  codes <- switch(method,
    covariates  = .grid_classify_covariates(g$coords, system = system),
    interpolate = .grid_interpolate(g$coords, points_df, class_col),
    overlay     = .grid_overlay(g$coords, source_url = src))
  rr <- .grid_to_raster(g$raster, codes)
  if (is.null(rr)) return(NULL)
  rr$raster <- terra::wrap(rr$raster)
  rr
}

# The class prior at a point, read from SoilGrids in a background worker
# (.sk_async()): only its arguments and package functions.
.map_prior_job <- function(lat, lon, system, buffer_m, source_url, top_n) {
  soilKey::soil_classes_at_location(
    lat = lat, lon = lon, system = system, buffer_m = buffer_m,
    source_url = source_url, top_n = top_n, verbose = FALSE)
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

    # `attribution` credits the source in the map's corner: SoilGrids (CC BY
    # 4.0, which requires it) for the live raster, a plain note for the
    # synthetic demo.
    add_overlay <- function(map, rr, attribution = NULL) {
      if (is.null(rr)) return(map)
      lut <- rr$lut
      pal <- sk_class_pal(lut$id)
      # suppressWarnings: addRasterImage resamples the class grid and warns
      # "values outside the color scale" for the interpolated edges -- benign.
      # method = "ngb": nearest-neighbour keeps hard class edges (bilinear would
      # blend integer class ids and invent non-existent classes at boundaries).
      map <- suppressWarnings(
        leaflet::addRasterImage(map, rr$raster, colors = pal, opacity = 0.6,
                                method = "ngb", project = TRUE,
                                group = "soilgrids", attribution = attribution))
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
    # Draw the query buffer as a circle of the chosen radius (point mode only),
    # so the user SEES the ground the prior samples as they change the radius.
    add_buffer <- function(map, cc, mode, buffer_m) {
      buffer_m <- suppressWarnings(as.numeric(buffer_m))
      if (!identical(mode, "point") || is.null(cc) ||
            length(buffer_m) != 1L || !is.finite(buffer_m) || buffer_m <= 0)
        return(map)
      map |> leaflet::addCircles(
        lng = cc$lon, lat = cc$lat, radius = buffer_m, group = "buffer",
        weight = 1.5, color = "#4A3226", fillColor = "#B5652E",
        fillOpacity = 0.08, dashArray = "4",
        label = sprintf(i18n("mpoint.buffer_circle"), round(buffer_m)))
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
      m <- sk_leaflet() |> leaflet::addProviderTiles(prov)
      m <- if (!is.null(cc)) m |> leaflet::setView(cc$lon, cc$lat, zoom = 7)
           else m |> leaflet::setView(-51, -14, zoom = 4)
      m <- add_points(m, cc, nb, mode)
      m <- add_buffer(m, cc, mode, shiny::isolate(input$buffer))
      # the SoilGrids overlay is drawn once its background read is done (below)
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

    # ---- query buffer ring: redraw live as the radius / point / mode change.
    # Depends only on the point + buffer (NOT map bounds -> no feedback loop).
    shiny::observeEvent(list(input$buffer, coords_r(), input$mode), {
      proxy <- leaflet::leafletProxy("map", session) |> leaflet::clearGroup("buffer")
      add_buffer(proxy, coords_r(), input$mode %||% "point", input$buffer)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ---- SoilGrids overlay for the visible area (all modes) --------------
    output$overlay_note <- shiny::renderUI({
      if (isTRUE(input$show_soilgrids) &&
          identical(overlay_task$status(), "running"))
        return(shiny::helpText(shiny::icon("spinner", class = "fa-spin"), " ",
                               i18n("map.loading_soilgrids")))
      if (nzchar(input$source_url %||% "")) return(NULL)   # custom URL: no note
      if (identical(input$sg_source %||% "live", "live"))
        shiny::helpText(shiny::icon("globe"), " ",
          if (isTRUE(sg_fellback())) i18n("map.live_fell_back")
          else i18n("map.using_live"))
      else
        shiny::helpText(shiny::icon("circle-info"), " ", i18n("map.using_demo_raster"))
    })

    # ---- "What am I looking at?" -- explains the legends beside the map -----
    output$map_legend_help <- shiny::renderUI({
      mode <- input$mode %||% "point"
      show <- isTRUE(input$show_soilgrids)
      paras <- list()
      if (show) {
        src_line <- if (identical(input$sg_source %||% "live", "live"))
          i18n("map.legend_overlay_live") else i18n("map.legend_overlay_demo")
        paras <- c(paras, list(shiny::p(
          shiny::strong(i18n("map.soilgrids_overlay")), " ",
          i18n("map.legend_overlay_body"), " ",
          shiny::tags$em(src_line))))
      }
      if (mode == "grid") {
        sysname <- c(wrb2022 = "WRB 2022", sibcs = "SiBCS 5",
                     usda = "USDA ST 13")[input$grid_system %||% "wrb2022"]
        paras <- c(paras, list(shiny::p(
          shiny::strong(i18n("mgrid.predicted_class_raster")), " ",
          sprintf(i18n("map.legend_grid_body"), sysname %||% "WRB 2022"))))
      } else if (mode == "point") {
        paras <- c(paras, list(shiny::p(i18n("map.legend_point_body"))))
      }
      bslib::card(
        class = "mt-2", bslib::card_header(shiny::icon("circle-question"), " ",
                                           i18n("map.legend_help_title")),
        bslib::card_body(class = "small", paras))
    })

    # ---- the SoilGrids overlay, read in a background worker ---------------
    # The map exists on the client once leaflet reports its bounds; proxy calls
    # made before that are dropped. Drawn from a cache when nothing it depends
    # on changed (editing a horizon reassigns the pedon, and used to re-read the
    # live raster); otherwise read in the background, with a note meanwhile.
    sg_fellback <- shiny::reactiveVal(FALSE)
    overlay_task <- shiny::ExtendedTask$new(function(args)
      promises::then(
        .sk_async(.map_overlay_job, args, helpers = .map_job_helpers()),
        function(res) c(res, list(args = args))))
    map_ready <- shiny::reactiveVal(FALSE)
    shiny::observeEvent(input$map_bounds, if (!isTRUE(map_ready())) map_ready(TRUE))
    overlay_cache   <- shiny::reactiveVal(NULL)
    overlay_pending <- shiny::reactiveVal(NULL)
    overlay_args <- shiny::reactive({
      cc <- coords_r()
      if (!isTRUE(input$show_soilgrids) || is.null(cc)) return(NULL)
      kind <- input$sg_source %||% "live"
      url  <- trimws(input$source_url %||% "")
      live <- identical(kind, "live") && !nzchar(url)
      list(lat = cc$lat, lon = cc$lon,
           src = .map_soilgrids_source(url, kind),
           fallback_src = if (live) .map_soilgrids_source(NULL, "demo"))
    })
    overlay_attribution <- function(args, fell_back) {
      if (is.null(args)) return(NULL)
      if (isTRUE(fell_back) ||
          identical(args$src, .map_soilgrids_source(NULL, "demo")))
        return(i18n("map.attr_demo"))
      if (identical(args$src, .SOILGRIDS_LIVE_VRT)) i18n("map.attr_soilgrids")
    }
    draw_overlay <- function(hit) {
      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("soilgrids") |>
        leaflet::removeControl("sg_legend")
      if (is.null(hit)) return(invisible())
      sg_fellback(isTRUE(hit$fell_back))
      add_overlay(proxy, .map_unwrap(hit$rr),
                  overlay_attribution(hit$args, hit$fell_back))
    }
    shiny::observeEvent(list(map_ready(), overlay_args()), {
      if (!isTRUE(map_ready())) return()
      a <- overlay_args()
      if (is.null(a)) return(draw_overlay(NULL))
      hit <- overlay_cache()
      if (!is.null(hit) && identical(hit$args, a)) return(draw_overlay(hit))
      if (identical(overlay_pending(), a)) return()
      overlay_pending(a)
      overlay_task$invoke(a)
    }, ignoreNULL = FALSE)
    shiny::observeEvent(overlay_task$status(), {
      if (!identical(overlay_task$status(), "success")) return()
      res <- overlay_task$result()
      overlay_pending(NULL)
      overlay_cache(res)
      # still what the map should show? (the point or a toggle may have moved on)
      if (identical(res$args, shiny::isolate(overlay_args()))) draw_overlay(res)
    })

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
    # The button gates the FIRST query (so placing a point / toggling the system
    # never fires a network read before the user asks). After the first run,
    # input$system becomes a live dependency, so toggling WRB <-> SiBCS re-queries
    # immediately -- the fix for "nothing happens when I switch the system".
    # The SoilGrids read takes up to a minute, so it runs in a background
    # worker (utils_async.R) and the other sessions on the instance carry on.
    ran_point_once <- shiny::reactiveVal(FALSE)
    prior_task <- shiny::ExtendedTask$new(function(args) .sk_async(.map_prior_job, args))
    bslib::bind_task_button(prior_task, "run_point")
    prior_msg  <- shiny::reactiveVal(NULL)   # a problem found before any query
    invoke_prior <- function() {
      cc <- coords_r()
      if (is.null(cc)) return(prior_msg(simpleError(i18n("mpoint.place_point_first"))))
      if (!requireNamespace("terra", quietly = TRUE))
        return(prior_msg(simpleError(i18n("mpoint.terra_not_installed"))))
      prior_msg(NULL)
      prior_task$invoke(list(
        lat = cc$lat, lon = cc$lon, system = input$system,
        buffer_m = input$buffer,
        source_url = .map_soilgrids_source(input$source_url, input$sg_source),
        top_n = input$topn))
    }
    shiny::observeEvent(input$run_point, {
      ran_point_once(TRUE)
      invoke_prior()
    }, ignoreInit = TRUE)
    shiny::observeEvent(input$system, {
      if (isTRUE(ran_point_once())) invoke_prior()
    }, ignoreInit = TRUE)
    prior <- shiny::reactive({
      m <- prior_msg()
      if (!is.null(m)) return(m)
      .sk_task_value(prior_task)
    })

    # ======================================================================
    #  BATCH MODE -- classify many profiles, colour by class
    # ======================================================================
    batch_pedons <- shiny::reactiveVal(NULL)
    # Classifying the points runs in a background worker (utils_async.R).
    batch_task <- shiny::ExtendedTask$new(function(args) .sk_async(.batch_classify, args))
    bslib::bind_task_button(batch_task, "run_batch")
    batch_msg  <- shiny::reactiveVal(NULL)   # a problem found before classifying
    shiny::observeEvent(input$run_batch, {
      on_missing <- tryCatch(settings()$on_missing, error = function(e) NULL) %||% "silent"
      peds <- if (identical(input$batch_source, "upload")) {
        f <- input$batch_csv
        if (is.null(f)) simpleError(i18n("mbatch.upload_first"))
        else tryCatch(.batch_parse_csv(utils::read.csv(f$datapath, stringsAsFactors = FALSE)),
                      error = function(e) e)
      } else tryCatch(.batch_demo_pedons(input$n_demo %||% 12L), error = function(e) e)
      if (inherits(peds, "error")) return(batch_msg(peds))
      batch_msg(NULL)
      batch_pedons(peds)
      # share the group of profiles app-wide so the Uncertainty tab can run
      # a per-point analysis over the same points the user entered here.
      rv$batch_pedons <- peds
      batch_task$invoke(list(pedons = peds, on_missing = on_missing))
    })
    batch <- shiny::reactive({
      m <- batch_msg()
      if (!is.null(m)) return(m)
      .sk_task_value(batch_task)
    })
    shiny::observeEvent(batch(), {
      res <- batch(); sysc <- paste0(input$batch_system %||% "wrb", "_class")
      sysn <- paste0(input$batch_system %||% "wrb", "_name")
      proxy <- leaflet::leafletProxy("map", session) |> leaflet::clearGroup("batch")
      if (inherits(res, "error") || is.null(res) || !nrow(res)) return()
      pal <- sk_class_pal(sort(unique(res[[sysc]])), na.color = "#bdbdbd")
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
        .sk_with_session_opts(soilKey::report_html(peds, file = file))
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
    # Predicting the grid reads SoilGrids or classifies many points: in a
    # background worker (utils_async.R), so the other sessions carry on.
    grid_task <- shiny::ExtendedTask$new(function(args)
      .sk_async(.map_grid_job, args, helpers = .map_job_helpers()))
    bslib::bind_task_button(grid_task, "run_grid")
    grid_msg <- shiny::reactiveVal(NULL)   # a problem found before predicting
    shiny::observeEvent(input$run_grid, {
      if (!requireNamespace("terra", quietly = TRUE))
        return(grid_msg(simpleError(i18n("mgrid.err_no_terra"))))
      cc <- coords_r()
      bb <- grid_bbox() %||% (if (!is.null(input$map_bounds)) {
        b <- input$map_bounds
        list(lon_min = b$west, lon_max = b$east,
             lat_min = b$south, lat_max = b$north)
      } else if (!is.null(cc))
        list(lon_min = cc$lon - 2, lon_max = cc$lon + 2,
             lat_min = cc$lat - 2, lat_max = cc$lat + 2) else NULL)
      if (is.null(bb)) return(grid_msg(simpleError(i18n("mgrid.err_invalid_bbox"))))
      method <- input$grid_method %||% "overlay"
      pts <- if (identical(method, "interpolate"))
        tryCatch(batch(), error = function(e) NULL)
      if (inherits(pts, "error")) pts <- NULL
      peds <- if (identical(method, "interpolate") && is.null(pts))
        .batch_demo_pedons(16L)
      grid_msg(NULL)
      grid_task$invoke(list(
        bb = bb, n = min(40L, input$grid_res %||% 20L), method = method,
        system = input$grid_system,
        src = .map_soilgrids_source(input$source_url, input$sg_source),
        points_df = pts, pedons = peds,
        class_col = paste0(sub("2022", "", input$grid_system), "_class")))
    })
    grid_result <- shiny::reactive({
      m <- grid_msg()
      if (!is.null(m)) return(m)
      rr <- .sk_task_value(grid_task)
      if (inherits(rr, "error")) return(rr)
      if (is.null(rr)) return(simpleError(i18n("mgrid.err_no_classes")))
      .map_unwrap(rr)
    })
    shiny::observeEvent(grid_result(), {
      rr <- grid_result()
      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("gridpred") |> leaflet::removeControl("grid_legend")
      if (inherits(rr, "error") || is.null(rr)) return()
      lut <- rr$lut  # data.frame(id, class); raster values ARE the ids
      pal <- sk_class_pal(lut$id)
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

    output$dist_table <- sk_renderDT({
      p <- prior(); shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"),
        if (inherits(p, "error")) conditionMessage(p) else i18n("mpoint.na")))
      df <- as.data.frame(p$distribution)
      shiny::validate(shiny::need(nrow(df) > 0L, i18n("mpoint.no_pixels_buffer")))
      n <- suppressWarnings(as.integer(input$topn %||% 5L))
      if (!is.finite(n) || n < 1L) n <- nrow(df)
      df <- df[order(-df$probability), , drop = FALSE]   # rank by probability
      df <- utils::head(df, n)
      show <- data.frame(rank = seq_len(nrow(df)),
                         rsg_name = df$rsg_name %||% df$rsg_code,
                         rsg_code = df$rsg_code,
                         probability = df$probability,
                         stringsAsFactors = FALSE)
      names(show) <- c(i18n("mpoint.col_rank"), i18n("mpoint.col_class"),
                       i18n("mpoint.col_code"), i18n("mpoint.col_probability"))
      sk_datatable(show, rownames = FALSE,
                    caption = sprintf(i18n("mpoint.topn_caption"), nrow(df),
                                      round(as.numeric(input$buffer %||% 0))),
                    options = list(dom = "t", pageLength = n)) |>
        DT::formatPercentage(i18n("mpoint.col_probability"), 2)
    })

    # Annotate the point with a persistent popup naming the #1 class + its share,
    # on its own group so the site/neighbour redraw does not wipe it.
    shiny::observeEvent(prior(), {
      p <- prior()
      if (inherits(p, "error") || is.null(p)) return()
      df <- as.data.frame(p$distribution); if (!nrow(df)) return()
      cc <- coords_r(); if (is.null(cc)) return()
      top1 <- df[order(-df$probability), , drop = FALSE][1, ]
      popup <- sprintf(i18n("mpoint.popup_top_class"),
                       rv$pedon$site$id %||% i18n("mpoint.point_label"),
                       top1$rsg_name %||% top1$rsg_code,
                       100 * as.numeric(top1$probability),
                       round(as.numeric(input$buffer %||% 0)))
      leaflet::leafletProxy("map", session) |>
        leaflet::clearGroup("site_prior") |>
        leaflet::addCircleMarkers(
          lng = cc$lon, lat = cc$lat, radius = 8, weight = 2, color = "#4A3226",
          fillColor = "#B5652E", fillOpacity = 0.95, group = "site_prior",
          popup = popup,
          popupOptions = leaflet::popupOptions(closeOnClick = FALSE))
    }, ignoreInit = TRUE)
    output$attrs_table <- sk_renderDT({
      p <- prior(); shiny::req(p)
      shiny::validate(shiny::need(!inherits(p, "error"), i18n("mpoint.na")))
      sk_datatable(.sk_round2(p$typical_attributes), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })
    output$batch_table <- sk_renderDT({
      res <- batch(); shiny::req(res)
      shiny::validate(shiny::need(!inherits(res, "error"),
        if (inherits(res, "error")) conditionMessage(res) else "n/a"))
      shiny::validate(shiny::need(nrow(res) > 0L, i18n("mbatch.no_classifiable")))
      show <- res[, c("id", "lat", "lon", "wrb_name", "sibcs_name", "usda_name"),
                  drop = FALSE]
      names(show) <- c(i18n("mbatch.col_id"), i18n("mbatch.col_lat"),
                       i18n("mbatch.col_lon"), "WRB 2022", "SiBCS 5", "USDA ST 13")
      sk_datatable(.sk_round2(show), rownames = FALSE,
                    options = list(dom = "tp", pageLength = 8, scrollX = TRUE))
    })
    output$grid_summary <- sk_renderDT({
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
      sk_datatable(show, rownames = FALSE,
                    options = list(dom = "tp", pageLength = 12)) |>
        DT::formatPercentage(i18n("mgrid.col_share"), 2)
    })
  })
}
