# =============================================================================
# soilKey Pro -- Gridded prediction map module (v0.9.103).
#
# Phase 3 of the mapping roadmap: produce a raster soil-class map over an area
# of interest. Three selectable methods, all reduced to one common shape --
# a vector of class codes over the cell-centre coordinates of a regular grid,
# turned into a categorical SpatRaster and rendered with addRasterImage:
#
#   1. "SoilGrids covariates + key" -- the differentiator. Samples SoilGrids
#      covariates (clay/sand/silt/pH/SOC/CEC) at two depths per cell, builds a
#      two-horizon pseudo-pedon, and runs the DETERMINISTIC key. Unlike
#      SoilGrids MostProbable (which predicts the class by ML), this applies
#      the key to covariates. Needs network. Honest limitation: no
#      morphological traits -> many Cambisol/Regosol, evidence grade C.
#   2. "Interpolate points" -- nearest-neighbour (Voronoi) of the Phase-2
#      classified points across the grid. Pedon-scale, offline.
#   3. "SoilGrids overlay" -- samples the MostProbable WRB raster on the grid
#      and maps integers -> RSG via soilgrids_wrb_lut(). Lightweight context.
#
# The pure helpers (.grid_*) take an injectable sampler / point set so the
# network method is unit-testable offline.
# =============================================================================

# SoilGrids property -> pedon column, with the scale needed on top of
# lookup_soilgrids() (which already returns conventional units, except SOC
# comes back in g/kg and the key wants OC in percent -> divide by 10).
.grid_covariate_map <- function() {
  list(
    clay  = list(col = "clay_pct", scale = 1),
    sand  = list(col = "sand_pct", scale = 1),
    silt  = list(col = "silt_pct", scale = 1),
    phh2o = list(col = "ph_h2o",   scale = 1),
    soc   = list(col = "oc_pct",   scale = 0.1),   # g/kg -> %
    cec   = list(col = "cec_cmol", scale = 1)
  )
}

.GRID_MAX_CELLS <- 1600L   # hard cap (40 x 40): bounds network + classify time

# A regular WGS84 grid over bbox = list(lon_min, lon_max, lat_min, lat_max).
.grid_make <- function(bbox, n) {
  n <- max(2L, min(40L, as.integer(n)))
  r <- terra::rast(nrows = n, ncols = n,
                   xmin = bbox$lon_min, xmax = bbox$lon_max,
                   ymin = bbox$lat_min, ymax = bbox$lat_max,
                   crs = "EPSG:4326")
  list(raster = r,
       coords = terra::xyFromCell(r, seq_len(terra::ncell(r))))  # cols x=lon,y=lat
}

# Default covariate sampler: a thin wrapper over lookup_soilgrids().
.grid_soilgrids_sampler <- function(coords, property, depth) {
  soilKey::lookup_soilgrids(coords, property = property, depth = depth,
                            quantile = "mean")
}

# Method 1: classify SoilGrids covariates with the deterministic key.
# `sampler(coords, property, depth)` is injectable so this is testable offline.
.grid_classify_covariates <- function(coords, system = "wrb2022",
                                      sampler = .grid_soilgrids_sampler,
                                      bump = NULL) {
  cmap   <- .grid_covariate_map()
  depths <- c(top = "5-15cm", sub = "60-100cm")
  # Sample every property at both depths (each call covers the whole grid).
  samp <- list(); i <- 0L; total <- length(cmap) * length(depths)
  for (pn in names(cmap)) {
    samp[[pn]] <- list()
    for (dn in names(depths)) {
      v <- suppressWarnings(as.numeric(sampler(coords, pn, depths[[dn]])))
      samp[[pn]][[dn]] <- v * cmap[[pn]]$scale
      i <- i + 1L
      if (is.function(bump)) bump(i / total * 0.5, i18n("mgrid.progress_sampling"))
    }
  }
  classify_fun <- switch(system,
    wrb2022 = soilKey::classify_wrb2022,
    sibcs   = soilKey::classify_sibcs,
    usda    = soilKey::classify_usda,
    soilKey::classify_wrb2022)

  ncell <- nrow(coords)
  out   <- rep(NA_character_, ncell)
  cols  <- vapply(cmap, function(x) x$col, character(1))
  for (k in seq_len(ncell)) {
    topv <- vapply(names(cmap), function(pn) samp[[pn]]$top[k], numeric(1))
    subv <- vapply(names(cmap), function(pn) samp[[pn]]$sub[k], numeric(1))
    if (all(is.na(topv)) && all(is.na(subv))) next
    hz <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 100))
    for (pn in names(cmap)) hz[[cmap[[pn]]$col]] <- c(topv[[pn]], subv[[pn]])
    ped <- tryCatch(
      soilKey::PedonRecord$new(
        site = list(id = sprintf("grid-%d", k),
                    lat = coords[k, 2], lon = coords[k, 1], crs = 4326),
        horizons = hz),
      error = function(e) NULL)
    if (!is.null(ped)) {
      res <- tryCatch(classify_fun(ped, on_missing = "silent"),
                      error = function(e) NULL)
      if (!is.null(res)) out[k] <- as.character(res$rsg_or_order %||% NA)
    }
    if (is.function(bump)) bump(0.5 + (k / ncell) * 0.5, i18n("mgrid.progress_classifying"))
  }
  out
}

# Method 2: nearest-neighbour interpolation of classified points to the grid.
.grid_interpolate <- function(coords, points_df, class_col) {
  if (is.null(points_df) || !nrow(points_df) ||
      !all(c("lon", "lat", class_col) %in% names(points_df)))
    return(rep(NA_character_, nrow(coords)))
  pts <- points_df[is.finite(points_df$lon) & is.finite(points_df$lat) &
                     !is.na(points_df[[class_col]]), , drop = FALSE]
  if (!nrow(pts)) return(rep(NA_character_, nrow(coords)))
  pts_sf  <- sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)
  grid_sf <- sf::st_as_sf(data.frame(lon = coords[, 1], lat = coords[, 2]),
                          coords = c("lon", "lat"), crs = 4326)
  idx <- sf::st_nearest_feature(grid_sf, pts_sf)
  as.character(pts[[class_col]][idx])
}

# Method 3: sample the SoilGrids MostProbable WRB raster on the grid.
.grid_overlay <- function(coords, source_url = NULL) {
  src <- source_url
  if (is.null(src) || !nzchar(src))
    src <- getOption("soilKey.test_raster", default = NULL)
  if (is.null(src) || !nzchar(src))
    stop(i18n("mgrid.err_no_raster_source"))
  r   <- terra::rast(src)
  pts <- terra::vect(coords, type = "points", crs = "EPSG:4326")
  pp  <- terra::project(pts, terra::crs(r))
  vals <- suppressWarnings(as.numeric(terra::extract(r, pp)[[2]]))
  lut <- soilKey::soilgrids_wrb_lut()
  unname(lut[as.character(round(vals))])
}

# Reduce a vector of class codes over a grid to a categorical SpatRaster + LUT.
.grid_to_raster <- function(grid, codes) {
  uniq <- sort(unique(codes[!is.na(codes)]))
  if (!length(uniq)) return(NULL)
  ids <- match(codes, uniq)
  terra::values(grid) <- ids
  list(raster = grid,
       lut    = data.frame(id = seq_along(uniq), class = uniq,
                           stringsAsFactors = FALSE))
}


map_grid_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,
      shiny::h5(i18n("mgrid.grid_prediction")),
      shinyWidgets::radioGroupButtons(
        ns("method"), i18n("mgrid.method"),
        choices = stats::setNames(
          c("covariates", "interpolate", "overlay"),
          c(i18n("mgrid.method_covariates"),
            i18n("mgrid.method_interpolate"),
            i18n("mgrid.method_overlay"))),
        selected = "overlay", direction = "vertical", size = "sm"),
      shiny::selectInput(ns("system"), i18n("mgrid.classification_system"),
                         choices = c("WRB 2022"  = "wrb2022",
                                     "SiBCS 5"    = "sibcs",
                                     "USDA ST 13" = "usda"),
                         selected = "wrb2022"),
      shiny::div(class = "small text-muted mb-1", i18n("mgrid.area_of_interest")),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("lat_max"), i18n("mgrid.lat_max"), -5, step = 1)),
        shiny::column(6, shiny::numericInput(ns("lat_min"), i18n("mgrid.lat_min"), -30, step = 1))),
      shiny::fluidRow(
        shiny::column(6, shiny::numericInput(ns("lon_min"), i18n("mgrid.lon_min"), -60, step = 1)),
        shiny::column(6, shiny::numericInput(ns("lon_max"), i18n("mgrid.lon_max"), -40, step = 1))),
      shiny::actionButton(ns("use_view"), i18n("mgrid.use_current_view"),
                          icon = shiny::icon("crop"),
                          class = "btn-outline-secondary btn-sm w-100 mb-2"),
      shiny::sliderInput(ns("res"), i18n("mgrid.cells_per_side"), min = 8, max = 40,
                         value = 24, step = 1),
      shiny::uiOutput(ns("ncell_note")),
      shiny::conditionalPanel(
        sprintf("input['%s'] != 'interpolate'", ns("method")),
        shiny::textInput(ns("source_url"), i18n("mgrid.soilgrids_raster"),
                         placeholder = i18n("mgrid.raster_placeholder"))),
      shiny::actionButton(ns("run"), i18n("mgrid.predict_grid"),
                          icon = shiny::icon("table-cells"),
                          class = "btn-primary w-100"),
      shiny::downloadButton(ns("export"), i18n("mgrid.export_geotiff"),
                            class = "btn-outline-secondary w-100 mt-2"),
      shiny::uiOutput(ns("method_help"))
    ),
    bslib::layout_column_wrap(
      width = 1, heights_equal = "row",
      bslib::card(
        bslib::card_header(i18n("mgrid.predicted_class_raster")),
        bslib::card_body(padding = 0,
                         leaflet::leafletOutput(ns("map"), height = "460px"))),
      bslib::card(
        bslib::card_header(i18n("mgrid.class_summary")),
        bslib::card_body(DT::DTOutput(ns("summary"))))
    )
  )
}

map_grid_server <- function(id, rv, settings) {
  shiny::moduleServer(id, function(input, output, session) {

    bbox <- shiny::reactive(list(
      lon_min = input$lon_min, lon_max = input$lon_max,
      lat_min = input$lat_min, lat_max = input$lat_max))

    n_cells <- shiny::reactive((input$res %||% 24L)^2)

    # ---- bbox from the current leaflet viewport -----------------------------
    shiny::observeEvent(input$use_view, {
      b <- input$map_bounds
      shiny::req(b)
      shiny::updateNumericInput(session, "lat_max", value = round(b$north, 4))
      shiny::updateNumericInput(session, "lat_min", value = round(b$south, 4))
      shiny::updateNumericInput(session, "lon_min", value = round(b$west, 4))
      shiny::updateNumericInput(session, "lon_max", value = round(b$east, 4))
    })

    output$ncell_note <- shiny::renderUI({
      nc <- n_cells()
      cls <- if (nc > .GRID_MAX_CELLS) "text-danger" else "text-muted"
      msg <- if (nc > .GRID_MAX_CELLS)
        i18n("mgrid.cells_capped", nc, .GRID_MAX_CELLS)
      else i18n("mgrid.cells_count", nc)
      shiny::div(class = paste("small mb-2", cls), msg)
    })

    output$method_help <- shiny::renderUI({
      txt <- switch(input$method %||% "overlay",
        covariates = i18n("mgrid.help_covariates"),
        interpolate = i18n("mgrid.help_interpolate"),
        overlay = i18n("mgrid.help_overlay"))
      shiny::helpText(txt)
    })

    # ---- run the chosen method ---------------------------------------------
    grid_result <- shiny::eventReactive(input$run, {
      bb <- bbox()
      if (!all(vapply(bb, function(x) is.numeric(x) && is.finite(x), logical(1))))
        return(simpleError(i18n("mgrid.err_invalid_bbox")))
      if (bb$lon_min >= bb$lon_max || bb$lat_min >= bb$lat_max)
        return(simpleError(i18n("mgrid.err_empty_bbox")))
      if (!requireNamespace("terra", quietly = TRUE))
        return(simpleError(i18n("mgrid.err_no_terra")))
      res_n <- min(40L, as.integer(input$res %||% 24L))
      src <- input$source_url
      src <- if (is.null(src) || !nzchar(trimws(src))) NULL else trimws(src)

      shiny::withProgress(message = i18n("mgrid.predicting_grid"), value = 0, {
        tryCatch({
          g <- .grid_make(bb, res_n)
          codes <- switch(input$method %||% "overlay",
            covariates = .grid_classify_covariates(
              g$coords, system = input$system,
              bump = function(frac, lab) shiny::setProgress(frac, detail = lab)),
            interpolate = {
              pts <- rv$batch_points
              if (is.null(pts) || !nrow(pts)) {
                # Fallback: demo points classified on the fly (helpers from
                # mod_map_batch.R, sourced into the same app environment).
                pts <- .batch_classify(.batch_demo_pedons(16L),
                                       on_missing = "silent")
              }
              col <- switch(input$system, wrb2022 = "wrb_class",
                            sibcs = "sibcs_class", usda = "usda_class")
              .grid_interpolate(g$coords, pts, col)
            },
            overlay = .grid_overlay(g$coords, source_url = src))
          rr <- .grid_to_raster(g$raster, codes)
          if (is.null(rr)) return(simpleError(
            i18n("mgrid.err_no_classes")))
          rr
        }, error = function(e) e)
      })
    })

    # ---- base map -----------------------------------------------------------
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() |>
        leaflet::addProviderTiles("CartoDB.Positron") |>
        leaflet::setView(lng = -51, lat = -14, zoom = 4)
    })

    # ---- draw the raster + legend ------------------------------------------
    shiny::observeEvent(grid_result(), {
      rr    <- grid_result()
      proxy <- leaflet::leafletProxy("map", session) |>
        leaflet::clearImages() |>
        leaflet::clearControls()
      if (inherits(rr, "error") || is.null(rr)) return()
      pal <- leaflet::colorFactor("Set3", domain = rr$lut$id,
                                  na.color = "transparent")
      bb  <- bbox()
      proxy |>
        leaflet::addRasterImage(rr$raster, colors = pal, opacity = 0.75,
                                method = "ngb", project = TRUE) |>
        leaflet::addLegend(position = "bottomright",
                           colors = pal(rr$lut$id), labels = rr$lut$class,
                           title = i18n("mgrid.legend_class"), opacity = 0.9) |>
        leaflet::fitBounds(lng1 = bb$lon_min, lat1 = bb$lat_min,
                           lng2 = bb$lon_max, lat2 = bb$lat_max)
    })

    # ---- class summary table ------------------------------------------------
    output$summary <- DT::renderDT({
      rr <- grid_result()
      shiny::req(rr)
      shiny::validate(shiny::need(!inherits(rr, "error"),
                                  if (inherits(rr, "error"))
                                    conditionMessage(rr) else i18n("mgrid.na")))
      v   <- terra::values(rr$raster)[, 1]
      tab <- as.data.frame(table(v), stringsAsFactors = FALSE)
      names(tab) <- c("id", "cells")
      tab$id <- as.integer(tab$id)
      tab <- merge(rr$lut, tab, by = "id", all.x = TRUE)
      tab$cells[is.na(tab$cells)] <- 0L
      tab$share <- tab$cells / sum(tab$cells)
      show <- tab[order(-tab$cells), c("class", "cells", "share")]
      names(show) <- c("Class", "Cells", "Share")
      DT::datatable(show, rownames = FALSE,
                    colnames = c(i18n("mgrid.col_class"),
                                 i18n("mgrid.col_cells"),
                                 i18n("mgrid.col_share")),
                    options = list(dom = "tp", pageLength = 10)) |>
        DT::formatPercentage("Share", 1)
    })

    # ---- GeoTIFF export -----------------------------------------------------
    output$export <- shiny::downloadHandler(
      filename = function() "soilkey_class_grid.tif",
      content = function(file) {
        rr <- grid_result()
        if (inherits(rr, "error") || is.null(rr))
          stop(i18n("mgrid.err_nothing_to_export"))
        r <- rr$raster
        levels(r) <- rr$lut          # write the class labels into the GeoTIFF
        terra::writeRaster(r, file, overwrite = TRUE)
      }
    )
  })
}
