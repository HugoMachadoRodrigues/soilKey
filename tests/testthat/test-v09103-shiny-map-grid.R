# Tests for the v0.9.103 gridded-prediction module
# (classify_app_pro / mod_map_grid.R). Phase 3 of the mapping roadmap.
#
# The pure .grid_* helpers are exercised offline: the SoilGrids covariate
# method takes an INJECTABLE sampler, the interpolation method runs on
# synthetic labelled points, and the overlay method reads a synthetic raster.

.mg_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d))
    d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.mg_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.mg_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE)) {
    sys.source(f, envir = env)
  }
  env
}

.mg_skip_if_no_proj <- function() {
  testthat::skip_if_not_installed("terra")
  ok <- tryCatch({
    suppressWarnings(suppressMessages(
      terra::rast(nrows = 1, ncols = 1,
                  ext = terra::ext(0, 1, 0, 1), crs = "EPSG:4326")))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("PROJ database (proj.db) not available")
}

# Synthetic single-class WRB raster (integer 10 -> FR) around Rio de Janeiro.
.mg_build_raster <- function(lon = -43.7, lat = -22.5) {
  ncell_side <- 40L; cell_size <- 0.0025
  ext <- terra::ext(
    lon - ncell_side * cell_size / 2, lon + ncell_side * cell_size / 2,
    lat - ncell_side * cell_size / 2, lat + ncell_side * cell_size / 2)
  rst <- terra::rast(nrows = ncell_side, ncols = ncell_side,
                     ext = ext, crs = "EPSG:4326")
  terra::values(rst) <- rep(10L, terra::ncell(rst))   # all FR
  path <- tempfile(fileext = ".tif")
  terra::writeRaster(rst, path, overwrite = TRUE, datatype = "INT2U")
  path
}

.mg_bbox <- function() list(lon_min = -43.74, lon_max = -43.66,
                            lat_min = -22.54, lat_max = -22.46)


test_that("mod_map_grid.R ships and parses", {
  skip_on_cran()
  app_dir <- .mg_app_dir()
  expect_true(file.exists(file.path(app_dir, "R", "mod_map_grid.R")))
  expect_silent(parse(file.path(app_dir, "R", "mod_map_grid.R")))
})


test_that("map_grid_ui() builds a valid Shiny tag", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("shinyWidgets"); skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")
  env <- .mg_source_modules()
  ui <- get("map_grid_ui", envir = env)("t")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})


test_that(".grid_make builds an n-by-n grid over the bbox", {
  skip_on_cran()
  .mg_skip_if_no_proj()
  env <- .mg_source_modules()
  g <- get(".grid_make", envir = env)(.mg_bbox(), 6L)
  expect_equal(terra::ncell(g$raster), 36L)
  expect_equal(nrow(g$coords), 36L)
  ex <- as.vector(terra::ext(g$raster))
  expect_equal(unname(ex[1]), -43.74, tolerance = 1e-9)  # xmin
  expect_equal(unname(ex[4]), -22.46, tolerance = 1e-9)  # ymax
})


test_that(".grid_classify_covariates classifies via an injectable sampler", {
  skip_on_cran()
  .mg_skip_if_no_proj()
  env <- .mg_source_modules()
  grid_make <- get(".grid_make", envir = env)
  classify  <- get(".grid_classify_covariates", envir = env)

  # Offline sampler returning plausible conventional-unit values per property.
  fake_sampler <- function(coords, property, depth) {
    base <- switch(property, clay = 55, sand = 25, silt = 20,
                   phh2o = 5.2, soc = 18, cec = 6, 0)
    rep(base, nrow(coords))
  }
  g <- grid_make(.mg_bbox(), 4L)
  codes <- classify(g$coords, system = "wrb2022", sampler = fake_sampler)
  expect_length(codes, terra::ncell(g$raster))
  expect_true(any(!is.na(codes)))            # at least one cell got a class
})


test_that(".grid_interpolate assigns each cell its nearest point's class", {
  skip_on_cran()
  skip_if_not_installed("sf")
  .mg_skip_if_no_proj()
  env <- .mg_source_modules()
  grid_make   <- get(".grid_make", envir = env)
  interpolate <- get(".grid_interpolate", envir = env)

  pts <- data.frame(
    lon = c(-43.73, -43.67), lat = c(-22.53, -22.47),
    wrb_class = c("Ferralsols", "Vertisols"), stringsAsFactors = FALSE)
  g <- grid_make(.mg_bbox(), 4L)
  codes <- interpolate(g$coords, pts, "wrb_class")
  expect_length(codes, terra::ncell(g$raster))
  expect_setequal(unique(codes), c("Ferralsols", "Vertisols"))
  # the SW-most cell is nearest the Ferralsol point
  sw <- which.min(g$coords[, 1] + g$coords[, 2])
  expect_equal(codes[sw], "Ferralsols")
})


test_that(".grid_overlay maps SoilGrids integers to RSG codes", {
  skip_on_cran()
  .mg_skip_if_no_proj()
  env <- .mg_source_modules()
  grid_make <- get(".grid_make", envir = env)
  overlay   <- get(".grid_overlay", envir = env)

  rst <- .mg_build_raster()
  on.exit(unlink(rst), add = TRUE)
  g <- grid_make(.mg_bbox(), 5L)
  codes <- overlay(g$coords, source_url = rst)
  expect_length(codes, terra::ncell(g$raster))
  expect_true(all(stats::na.omit(codes) == "FR"))   # raster is all FR
})


test_that(".grid_to_raster reduces codes to a categorical raster + LUT", {
  skip_on_cran()
  .mg_skip_if_no_proj()
  env <- .mg_source_modules()
  grid_make    <- get(".grid_make", envir = env)
  to_raster    <- get(".grid_to_raster", envir = env)
  g <- grid_make(.mg_bbox(), 3L)
  codes <- rep(c("FR", "VR", NA), length.out = terra::ncell(g$raster))
  rr <- to_raster(g$raster, codes)
  expect_s4_class(rr$raster, "SpatRaster")
  expect_setequal(rr$lut$class, c("FR", "VR"))
})


test_that("map_grid_server renders an overlay raster over the test bbox", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("DT"); skip_if_not_installed("leaflet")
  .mg_skip_if_no_proj()

  rst <- .mg_build_raster()
  withr::defer({ options(soilKey.test_raster = NULL); unlink(rst) })
  options(soilKey.test_raster = rst)

  env <- .mg_source_modules()
  map_grid_server <- get("map_grid_server", envir = env)

  rv       <- shiny::reactiveValues(pedon = NULL, batch_points = NULL)
  settings <- shiny::reactive(NULL)
  bb <- .mg_bbox()

  shiny::testServer(
    map_grid_server,
    args = list(rv = rv, settings = settings),
    {
      session$setInputs(method = "overlay", system = "wrb2022",
                        lon_min = bb$lon_min, lon_max = bb$lon_max,
                        lat_min = bb$lat_min, lat_max = bb$lat_max,
                        res = 5, source_url = "")
      session$setInputs(run = 1)
      rr <- grid_result()
      expect_false(inherits(rr, "error"))
      expect_s4_class(rr$raster, "SpatRaster")
      expect_true("FR" %in% rr$lut$class)
      expect_error(output$summary, NA)
    }
  )
})
