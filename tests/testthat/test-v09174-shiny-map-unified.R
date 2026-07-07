# Tests for the v0.9.174 unified Map module (classify_app_pro / mod_map.R).
#
# The three former sub-tabs (Point prior / Batch / Grid) are now ONE map driven
# by a mode selector, with a shared SoilGrids overlay that defaults to a bundled
# demo raster. These tests exercise the module headlessly (testServer), so they
# never touch the network; the overlay uses the bundled www/soilgrids_wrb_demo.tif.

.mapu_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.mapu_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.mapu_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE))
    sys.source(f, envir = env)
  env
}

.mapu_skip_if_no_proj <- function() {
  testthat::skip_if_not_installed("terra")
  ok <- tryCatch({
    suppressWarnings(suppressMessages(
      terra::rast(nrows = 1, ncols = 1, ext = terra::ext(0, 1, 0, 1),
                  crs = "EPSG:4326")))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("PROJ database (proj.db) not available")
}


test_that("a bundled SoilGrids demo raster ships with the app", {
  skip_on_cran()
  f <- file.path(.mapu_app_dir(), "www", "soilgrids_wrb_demo.tif")
  expect_true(file.exists(f))
})


test_that(".map_soilgrids_source() defaults to the bundled demo raster", {
  skip_on_cran()
  env <- .mapu_source_modules()
  src_fn <- get(".map_soilgrids_source", envir = env)

  # blank / NULL user URL -> bundled demo raster
  expect_match(src_fn(NULL), "soilgrids_wrb_demo\\.tif$")
  expect_match(src_fn(""), "soilgrids_wrb_demo\\.tif$")
  # an explicit URL wins
  expect_identical(src_fn("/some/raster.tif"), "/some/raster.tif")
  # the options() test-raster override wins over the demo but not the user URL
  withr::with_options(list(soilKey.test_raster = "/opt/r.tif"), {
    expect_identical(src_fn(NULL), "/opt/r.tif")
    expect_identical(src_fn("/user.tif"), "/user.tif")
  })
  # kind = "live" resolves to the real ISRIC MostProbable VRT (via /vsicurl)
  expect_match(src_fn(NULL, "live"), "files\\.isric\\.org/.*MostProbable\\.vrt$")
  expect_match(src_fn(NULL, "live"), "^/vsicurl/")
  expect_match(src_fn(NULL, "demo"), "soilgrids_wrb_demo\\.tif$")
})


test_that(".wrb_name_to_code() maps live-raster RSG labels to 2-letter codes", {
  skip_on_cran()
  env <- .mapu_source_modules()
  n2c <- get(".wrb_name_to_code", envir = env)
  expect_equal(n2c(c("Ferralsols", "Cambisols", "Plinthosols")),
               c("FR", "CM", "PT"))
  expect_equal(n2c("Albeluvisols"), "RT")     # legacy name -> Retisols
  expect_equal(n2c("Umbrisols"), "UM")        # was mis-shifted by the old LUT
  expect_equal(n2c("Vertisols"), "VR")
  expect_true(is.na(n2c(NA)))
  expect_identical(n2c("Nonesuch"), "Nonesuch")  # unknown label passes through
})


test_that("the bundled demo raster is multi-class around the demo point", {
  skip_on_cran()
  .mapu_skip_if_no_proj()
  f <- file.path(.mapu_app_dir(), "www", "soilgrids_wrb_demo.tif")
  r <- terra::rast(f)
  # a ~1-degree window around Rio (-43.7, -22.5) must contain several classes
  win <- terra::crop(r, terra::ext(-44.7, -42.7, -23.5, -21.5))
  n_classes <- length(unique(stats::na.omit(terra::values(win)[, 1])))
  expect_gt(n_classes, 3L)   # was 1 (FR only) with the old coarse raster
})


test_that("map_ui() builds one valid tag with a mode selector", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet"); skip_if_not_installed("DT")
  env <- .mapu_source_modules()
  ui <- get("map_ui", envir = env)("m")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
  html <- as.character(ui)
  expect_true(grepl("m-mode", html))          # the point/batch/grid selector
  expect_true(grepl("m-show_soilgrids", html)) # the shared overlay toggle
  expect_true(grepl("sk-map-square", html))    # the square map wrapper
})


test_that("map_server point mode renders the map and draws the overlay", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet"); skip_if_not_installed("DT")
  .mapu_skip_if_no_proj()

  env        <- .mapu_source_modules()
  map_server <- get("map_server", envir = env)

  pr <- soilKey::PedonRecord$new(
    site = list(id = "map-unified", lat = -22.5, lon = -43.7, crs = 4326))
  rv       <- shiny::reactiveValues(pedon = pr)
  settings <- shiny::reactive(list(on_missing = "silent"))

  shiny::testServer(map_server, args = list(rv = rv, settings = settings), {
    session$setInputs(mode = "point", basemap = "OpenStreetMap",
                      show_soilgrids = TRUE, source_url = "",
                      system = "wrb2022", buffer = 600, topn = 5)
    # coordinate comes straight from the pedon
    expect_equal(coords_r()$lat, -22.5)
    expect_equal(coords_r()$src, "pedon")

    # the overlay raster helper resolves the bundled demo raster for this point
    rr <- overlay_raster(coords_r(), .map_soilgrids_source(""))
    expect_false(is.null(rr))
    expect_true(all(c("id", "class") %in% names(rr$lut)))

    # the base map renders without error (the initial view/points/overlay are
    # baked into renderLeaflet, so this must not raise)
    expect_error(output$map, NA)
  })
})


test_that("map_server batch mode classifies demo points with coordinates", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("leaflet"); skip_if_not_installed("DT")

  env        <- .mapu_source_modules()
  map_server <- get("map_server", envir = env)
  rv       <- shiny::reactiveValues(pedon = NULL)
  settings <- shiny::reactive(list(on_missing = "silent"))

  shiny::testServer(map_server, args = list(rv = rv, settings = settings), {
    session$setInputs(mode = "batch", batch_source = "demo", n_demo = 3,
                      batch_system = "wrb", show_soilgrids = FALSE)
    session$setInputs(run_batch = 1)
    res <- batch()
    expect_false(inherits(res, "error"))
    expect_true(all(c("id", "lat", "lon", "wrb_name") %in% names(res)))
    expect_true(nrow(res) >= 1L)
    expect_error(output$batch_table, NA)
  })
})
