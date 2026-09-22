# Tests for the v0.9.101 interactive map tab (classify_app_pro / mod_map.R).
#
# The module is sourced from the app's R/ directory (not the package
# namespace), so we mirror the approach in test-v0997-shiny-pro-app.R: source
# every app module into a throwaway environment and exercise map_ui() /
# map_server() from there. The prior is queried against a synthetic categorical
# raster so the test never touches the network.

.map_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d))
    d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

# Source all app modules + helpers into one env and return it.
.map_source_modules <- function() {
  app_dir <- .map_app_dir()
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$",
                       full.names = TRUE)) {
    sys.source(f, envir = env)
  }
  env
}

# Skip when the PROJ database is missing (terra::rast() errors before any
# soilKey code runs); same guard the SoilGrids tests use.
.map_skip_if_no_proj <- function() {
  testthat::skip_if_not_installed("terra")
  ok <- tryCatch({
    suppressWarnings(suppressMessages(
      terra::rast(nrows = 1, ncols = 1,
                  ext = terra::ext(0, 1, 0, 1), crs = "EPSG:4326")))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("PROJ database (proj.db) not available")
}

# A small categorical WRB raster centred on Rio de Janeiro state, mostly
# Ferralsols (FR -> integer 10). Mirrors build_synthetic_raster() in
# test-spatial-soilgrids.R but kept local to avoid cross-file coupling.
.map_build_raster <- function(lon = -43.7, lat = -22.5) {
  ncell_side <- 10L
  cell_size  <- 0.0025
  ext <- terra::ext(
    lon - ncell_side * cell_size / 2, lon + ncell_side * cell_size / 2,
    lat - ncell_side * cell_size / 2, lat + ncell_side * cell_size / 2)
  rst <- terra::rast(nrows = ncell_side, ncols = ncell_side,
                     ext = ext, crs = "EPSG:4326")
  terra::values(rst) <- rep(10L, terra::ncell(rst))  # all FR
  path <- tempfile(fileext = ".tif")
  terra::writeRaster(rst, path, overwrite = TRUE, datatype = "INT2U")
  path
}


test_that("mod_map.R ships and parses", {
  skip_on_cran()
  app_dir <- .map_app_dir()
  expect_true(file.exists(file.path(app_dir, "R", "mod_map.R")))
  expect_silent(parse(file.path(app_dir, "R", "mod_map.R")))
})


test_that("map_ui() builds a valid Shiny tag", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")

  env <- .map_source_modules()
  ui <- get("map_ui", envir = env)("t")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})


test_that("map_server queries the prior at the pedon coordinate (offline raster)", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")
  .map_skip_if_no_proj()

  rst_path <- .map_build_raster()
  on.exit(unlink(rst_path), add = TRUE)

  env        <- .map_source_modules()
  map_server <- get("map_server", envir = env)

  # A pedon whose site sits inside the synthetic raster footprint.
  pr <- soilKey::PedonRecord$new(
    site = list(id = "map-test", lat = -22.5, lon = -43.7, crs = 4326))
  rv       <- shiny::reactiveValues(pedon = pr)
  settings <- shiny::reactive(NULL)

  shiny::testServer(
    map_server,
    args = list(rv = rv, settings = settings),
    {
      session$setInputs(basemap = "OpenStreetMap", system = "wrb2022",
                        source_url = rst_path, buffer = 600, topn = 5)
      # coords come straight from the pedon site (no map click needed).
      expect_equal(coords_r()$lat, -22.5)
      expect_equal(coords_r()$src, "pedon")

      session$setInputs(run_point = 1)
      res <- prior()
      expect_false(inherits(res, "error"))
      df <- as.data.frame(res$distribution)
      expect_true(nrow(df) >= 1L)
      expect_true("FR" %in% df$rsg_code)

      # Force the DT outputs to render -- guards the regression where the
      # distribution table's formatPercentage() referenced a renamed column.
      expect_error(output$dist_table, NA)
      expect_error(output$attrs_table, NA)
    }
  )
})


test_that("map_server holds a clicked coordinate when no pedon is loaded", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")

  env        <- .map_source_modules()
  map_server <- get("map_server", envir = env)

  rv       <- shiny::reactiveValues(pedon = NULL)
  settings <- shiny::reactive(NULL)

  shiny::testServer(
    map_server,
    args = list(rv = rv, settings = settings),
    {
      expect_null(coords_r())
      session$setInputs(map_click = list(lat = -15.6, lng = -47.7))
      cc <- coords_r()
      expect_equal(cc$src, "click")
      expect_equal(cc$lat, -15.6)
      expect_equal(cc$lon, -47.7)
    }
  )
})
