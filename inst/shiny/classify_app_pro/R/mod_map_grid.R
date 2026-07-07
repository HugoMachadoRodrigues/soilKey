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

# SoilGrids /vsicurl COGs are slow to OPEN (~30-60s each) because GDAL lists the
# remote directory by default. These settings skip that and enable HTTP/2 +
# caching, roughly halving each open.
.grid_set_gdal_fast <- function() {
  if (!requireNamespace("terra", quietly = TRUE)) return(invisible())
  for (cfg in c("GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR",
                "GDAL_HTTP_VERSION=2", "GDAL_HTTP_MULTIPLEX=YES",
                "VSI_CACHE=TRUE", "GDAL_HTTP_TIMEOUT=45",
                "CPL_VSIL_CURL_ALLOWED_EXTENSIONS=.vrt,.tif"))
    try(terra::setGDALconfig(cfg), silent = TRUE)
  invisible()
}

# SoilGrids integer -> conventional-unit scale (matches lookup_soilgrids, so the
# parallel path returns the same units). Covariate properties are all 0.1; the
# others are kept for correctness if the covariate map changes.
.GRID_SG_SCALE <- c(clay = 0.1, sand = 0.1, silt = 0.1, phh2o = 0.1,
                    soc = 0.1, cec = 0.1, bdod = 0.01, nitrogen = 0.01,
                    cfvo = 0.1, ocd = 0.1, ocs = 0.1)

# Sample the (property x depth) SoilGrids layers over the whole grid IN PARALLEL
# (PSOCK): the reads are independent and each ~30s, so serial takes minutes.
# The worker uses ONLY terra (NOT soilKey) so PSOCK workers start fast and do
# not thrash the machine loading the full package; it re-implements the thin
# /vsicurl read + conventional-unit scaling of lookup_soilgrids(). Returns a
# list of numeric vectors in `tasks` row order (conventional units), or NULL if
# a cluster cannot be created / the run fails. PSOCK (not fork) because
# terra/GDAL is not fork-safe.
.grid_sample_parallel <- function(coords, tasks, depths) {
  if (!requireNamespace("parallel", quietly = TRUE) ||
      !requireNamespace("terra", quietly = TRUE)) return(NULL)
  base  <- "https://files.isric.org/soilgrids/latest/data"
  urls  <- sprintf("/vsicurl/%s/%s/%s_%s_mean.vrt", base, tasks$pn, tasks$pn,
                   vapply(tasks$dn, function(d) depths[[d]], character(1)))
  scl   <- unname(.GRID_SG_SCALE[tasks$pn]); scl[is.na(scl)] <- 0.1
  cdf   <- as.data.frame(coords)                       # cols x (lon), y (lat)
  names(cdf)[1:2] <- c("x", "y")
  jobs  <- lapply(seq_len(nrow(tasks)),
                  function(i) list(url = urls[i], scale = scl[i]))
  n_work <- min(nrow(tasks), max(2L, min(6L, parallel::detectCores() - 1L)))
  cl <- tryCatch(parallel::makeCluster(n_work, type = "PSOCK"),
                 error = function(e) NULL)
  if (is.null(cl)) return(NULL)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  worker <- function(job, cdf) {
    if (!requireNamespace("terra", quietly = TRUE))
      return(rep(NA_real_, nrow(cdf)))
    for (cfg in c("GDAL_DISABLE_READDIR_ON_OPEN=EMPTY_DIR",
                  "GDAL_HTTP_VERSION=2", "VSI_CACHE=TRUE",
                  "GDAL_HTTP_TIMEOUT=45",
                  "CPL_VSIL_CURL_ALLOWED_EXTENSIONS=.vrt,.tif"))
      try(terra::setGDALconfig(cfg), silent = TRUE)
    r <- tryCatch(terra::rast(job$url), error = function(e) NULL)
    if (is.null(r)) return(rep(NA_real_, nrow(cdf)))
    pts <- terra::project(
      terra::vect(cdf, geom = c("x", "y"), crs = "EPSG:4326"), terra::crs(r))
    suppressWarnings(as.numeric(terra::extract(r, pts)[[2]])) * job$scale
  }
  tryCatch(parallel::parLapply(cl, jobs, worker, cdf = cdf),
           error = function(e) NULL)
}

# Sample all properties x depths into samp[[pn]][[dn]] (conventional units x the
# covariate-map scale). The live SoilGrids sampler is read in PARALLEL; a custom
# injected sampler (tests) runs serially and offline.
.grid_sample_all <- function(coords, cmap, depths,
                             sampler = .grid_soilgrids_sampler, bump = NULL) {
  tasks <- expand.grid(pn = names(cmap), dn = names(depths),
                       stringsAsFactors = FALSE)
  vals <- NULL
  if (identical(sampler, .grid_soilgrids_sampler)) {
    .grid_set_gdal_fast()
    if (is.function(bump)) bump(0.05, i18n("mgrid.progress_sampling"))
    vals <- .grid_sample_parallel(coords, tasks, depths)     # NULL on failure
  }
  if (is.null(vals)) {                                       # serial (tests / fallback)
    vals <- lapply(seq_len(nrow(tasks)), function(i) {
      v <- suppressWarnings(as.numeric(
        sampler(coords, tasks$pn[i], depths[[tasks$dn[i]]])))
      if (is.function(bump))
        bump(i / nrow(tasks) * 0.5, i18n("mgrid.progress_sampling"))
      v
    })
  }
  samp <- stats::setNames(vector("list", length(cmap)), names(cmap))
  for (i in seq_len(nrow(tasks))) {
    pn <- tasks$pn[i]; dn <- tasks$dn[i]
    samp[[pn]][[dn]] <- as.numeric(vals[[i]]) * cmap[[pn]]$scale
  }
  samp
}

# Method 1: classify SoilGrids covariates with the deterministic key.
# `sampler(coords, property, depth)` is injectable so this is testable offline.
.grid_classify_covariates <- function(coords, system = "wrb2022",
                                      sampler = .grid_soilgrids_sampler,
                                      bump = NULL) {
  cmap   <- .grid_covariate_map()
  depths <- c(top = "5-15cm", sub = "60-100cm")
  # Sample every property x depth over the whole grid. The live SoilGrids reads
  # run in parallel (each ~30s; serial would take minutes); an injected sampler
  # (tests) runs serially and offline.
  samp <- .grid_sample_all(coords, cmap, depths, sampler = sampler, bump = bump)
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

# Full WRB Reference Soil Group name -> 2-letter code. The live ISRIC raster is
# categorical and its extract returns the RSG name (incl. the legacy
# "Albeluvisols" == Retisols), so we map names, not integers. Unknown labels
# pass through unchanged so nothing is silently dropped.
.wrb_name_to_code <- function(x) {
  m <- c(Acrisols = "AC", Albeluvisols = "RT", Retisols = "RT", Alisols = "AL",
         Andosols = "AN", Arenosols = "AR", Calcisols = "CL", Cambisols = "CM",
         Chernozems = "CH", Cryosols = "CR", Durisols = "DU", Ferralsols = "FR",
         Fluvisols = "FL", Gleysols = "GL", Gypsisols = "GY", Histosols = "HS",
         Kastanozems = "KS", Leptosols = "LP", Lixisols = "LX", Luvisols = "LV",
         Nitisols = "NT", Phaeozems = "PH", Planosols = "PL", Plinthosols = "PT",
         Podzols = "PZ", Regosols = "RG", Solonchaks = "SC", Solonetz = "SN",
         Stagnosols = "ST", Technosols = "TC", Umbrisols = "UM", Vertisols = "VR")
  x <- as.character(x)
  out <- unname(m[x])
  keep <- is.na(out) & !is.na(x) & nzchar(x)
  out[keep] <- x[keep]
  out
}

# Method 3: sample the SoilGrids MostProbable WRB raster on the grid.
# Handles BOTH the offline demo (plain integer raster -> numeric LUT) and the
# live ISRIC raster (categorical -> extract returns the RSG label -> name map).
.grid_overlay <- function(coords, source_url = NULL) {
  src <- source_url
  if (is.null(src) || !nzchar(src))
    src <- getOption("soilKey.test_raster", default = NULL)
  if (is.null(src) || !nzchar(src))
    stop(i18n("mgrid.err_no_raster_source"))
  if (grepl("vsicurl|^http", src)) .grid_set_gdal_fast()   # fast remote open
  r   <- terra::rast(src)
  pts <- terra::vect(coords, type = "points", crs = "EPSG:4326")
  pp  <- terra::project(pts, terra::crs(r))
  raw <- terra::extract(r, pp)[[2]]
  if (terra::is.factor(r) || is.factor(raw) || is.character(raw)) {
    # live categorical raster: labels are the RSG names
    .wrb_name_to_code(raw)
  } else {
    lut  <- soilKey::soilgrids_wrb_lut()
    vals <- suppressWarnings(as.numeric(raw))
    unname(lut[as.character(round(vals))])
  }
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


# Recode a CROPPED SoilGrids WRB raster to contiguous integer class ids + a LUT
# (id -> RSG code), for direct addRasterImage rendering (continuous patches).
# Handles BOTH the live categorical raster (factor RAT; labels = RSG names ->
# .wrb_name_to_code, incl. Albeluvisols == RT) AND the offline demo (plain
# integer -> soilgrids_wrb_lut). Only classes present in the crop are kept, so
# the legend stays tight. Returns the same list(raster, lut) shape as
# .grid_to_raster so add_overlay's palette/legend code is unchanged.
.overlay_recode <- function(rc) {
  if (terra::is.factor(rc)) {
    rat      <- terra::levels(rc)[[1]]           # col 1 = value, last = label
    map_from <- suppressWarnings(as.integer(rat[[1]]))
    map_code <- .wrb_name_to_code(as.character(rat[[ncol(rat)]]))
    ri       <- rc; terra::levels(ri) <- NULL    # drop RAT -> plain integer grid
  } else {
    lut0     <- soilKey::soilgrids_wrb_lut()
    map_from <- suppressWarnings(as.integer(names(lut0)))
    map_code <- unname(lut0)
    ri       <- rc
  }
  present <- sort(unique(suppressWarnings(
    as.integer(terra::values(ri, mat = FALSE)))))
  present <- present[is.finite(present)]
  if (!length(present)) return(NULL)
  sel  <- match(present, map_from)
  ok   <- !is.na(sel) & !is.na(map_code[sel]) & nzchar(map_code[sel] %||% "")
  from <- present[ok]; code <- map_code[sel][ok]
  if (!length(from)) return(NULL)
  uniq   <- sort(unique(code))                   # RSG codes present in the crop
  new_id <- match(code, uniq)
  ri <- terra::subst(ri, from = from, to = new_id, others = NA)  # exact remap
  list(raster = ri,
       lut    = data.frame(id = seq_along(uniq), class = uniq,
                           stringsAsFactors = FALSE))
}


map_grid_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 340,

      sk_section(
        i18n("mgrid.grid_prediction"),
        desc = "Predict a soil-class map over an area, then review it on the map and in the summary table.",
        icon = "map-location-dot",
        shinyWidgets::radioGroupButtons(
          ns("method"),
          sk_label(i18n("mgrid.method"),
                   "How each grid cell gets its class: from SoilGrids covariates run through the key, interpolated from your classified points, or read off the SoilGrids overlay."),
          choices = stats::setNames(
            c("covariates", "interpolate", "overlay"),
            c(i18n("mgrid.method_covariates"),
              i18n("mgrid.method_interpolate"),
              i18n("mgrid.method_overlay"))),
          selected = "overlay", direction = "vertical", size = "sm"),
        shiny::selectInput(
          ns("system"),
          sk_label(i18n("mgrid.classification_system"),
                   "Which soil taxonomy the predicted classes are named in: WRB 2022, SiBCS, or USDA Soil Taxonomy."),
          choices = c("WRB 2022"  = "wrb2022",
                      "SiBCS 5"    = "sibcs",
                      "USDA ST 13" = "usda"),
          selected = "wrb2022")
      ),

      sk_section(
        i18n("mgrid.area_of_interest"),
        desc = "Set the latitude / longitude bounding box to map, or grab it from the current map view.",
        icon = "location-dot",
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(
            ns("lat_max"),
            sk_label(i18n("mgrid.lat_max"), "Northern edge of the area, in decimal degrees (must be above the minimum latitude)."),
            -5, step = 1)),
          shiny::column(6, shiny::numericInput(
            ns("lat_min"),
            sk_label(i18n("mgrid.lat_min"), "Southern edge of the area, in decimal degrees (must be below the maximum latitude)."),
            -30, step = 1))),
        shiny::fluidRow(
          shiny::column(6, shiny::numericInput(
            ns("lon_min"),
            sk_label(i18n("mgrid.lon_min"), "Western edge of the area, in decimal degrees (must be left of the maximum longitude)."),
            -60, step = 1)),
          shiny::column(6, shiny::numericInput(
            ns("lon_max"),
            sk_label(i18n("mgrid.lon_max"), "Eastern edge of the area, in decimal degrees (must be right of the minimum longitude)."),
            -40, step = 1))),
        bslib::tooltip(
          shiny::actionButton(ns("use_view"), i18n("mgrid.use_current_view"),
                              icon = shiny::icon("crop"),
                              class = "btn-outline-secondary btn-sm w-100 mb-2"),
          "Fill the bounding box from what is currently shown in the map viewport.")
      ),

      sk_section(
        i18n("mgrid.cells_per_side"),
        desc = "Grid resolution. Finer grids sample more cells, so prediction takes longer.",
        icon = "table-cells",
        shiny::sliderInput(
          ns("res"),
          sk_label(i18n("mgrid.cells_per_side"),
                   "Number of cells along each side of the grid; the total cell count is this squared and is capped for speed."),
          min = 8, max = 40, value = 24, step = 1),
        shiny::uiOutput(ns("ncell_note")),
        shiny::conditionalPanel(
          sprintf("input['%s'] != 'interpolate'", ns("method")),
          shiny::textInput(
            ns("source_url"),
            sk_label(i18n("mgrid.soilgrids_raster"),
                     "Optional URL of a SoilGrids raster to sample; leave blank to use the default source for the chosen method."),
            placeholder = i18n("mgrid.raster_placeholder")))
      ),

      sk_section(
        i18n("mgrid.predict_grid"),
        desc = "Run the prediction, then export the resulting class raster.",
        icon = "play",
        bslib::tooltip(
          shiny::actionButton(ns("run"), i18n("mgrid.predict_grid"),
                              icon = shiny::icon("table-cells"),
                              class = "btn-primary w-100"),
          "Predict the soil class for every grid cell and draw it as a raster on the map."),
        bslib::tooltip(
          shiny::downloadButton(ns("export"), i18n("mgrid.export_geotiff"),
                                class = "btn-outline-secondary w-100 mt-2"),
          "Download the predicted class grid as a categorical GeoTIFF for use in GIS."),
        shiny::uiOutput(ns("method_help"))
      )
    ),
    bslib::layout_column_wrap(
      width = 1, heights_equal = "row",
      bslib::card(
        bslib::card_header(i18n("mgrid.predicted_class_raster")),
        bslib::card_body(padding = 0,
                         leaflet::leafletOutput(ns("map"), height = "460px"))),
      bslib::card(
        bslib::card_header(i18n("mgrid.class_summary")),
        bslib::card_body(
          shiny::helpText("Share of grid cells assigned to each predicted soil class."),
          DT::DTOutput(ns("summary"))))
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
        DT::formatPercentage("Share", 2)
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
