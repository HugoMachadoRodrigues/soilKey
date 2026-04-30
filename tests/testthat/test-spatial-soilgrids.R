# ================================================================
# Tests for the SoilGrids backend.
#
# Build a synthetic categorical raster centred on a tropical site
# (lat = -22.5, lon = -43.7 -- Rio de Janeiro state) with mostly
# Ferralsols (FR -> integer 10) plus a few Acrisol (AC -> 1) and
# Regosol (RG -> 24) cells. The raster is written to a temp file
# under EPSG:4326 because the buffering helper handles reprojection.
# ================================================================


skip_if_no_terra <- function() {
  testthat::skip_if_not_installed("terra")
}

# v0.9.10: skip these tests when the PROJ database is unavailable
# (e.g. a minimal R install without proj.db on PATH). The raster
# constructor in `terra` then errors with "empty srs" before any of
# our code runs, which is unrelated to soilKey functionality. This
# helper centralises the skip so individual tests can call it.
skip_if_no_proj <- function() {
  testthat::skip_if_not_installed("terra")
  ok <- tryCatch({
    suppressWarnings(suppressMessages(
      terra::rast(nrows = 1, ncols = 1,
                   ext = terra::ext(0, 1, 0, 1),
                   crs = "EPSG:4326")
    ))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("PROJ database (proj.db) not available")
}


# Helper: build a synthetic 10x10 categorical raster centred at the
# given lon/lat. Cell size is 0.0025 degrees (~250 m at the equator)
# so the whole raster covers ~2.5 km on a side, easily containing the
# default 250-m buffer.
build_synthetic_raster <- function(lon = -43.7,
                                     lat = -22.5,
                                     dominant = 10L,   # FR
                                     mix      = list(`1` = 5L, `24` = 1L),
                                     path = NULL) {
  testthat::skip_if_not_installed("terra")
  skip_if_no_proj()

  ncell_side <- 10L
  cell_size  <- 0.0025
  ext <- terra::ext(
    lon - ncell_side * cell_size / 2,
    lon + ncell_side * cell_size / 2,
    lat - ncell_side * cell_size / 2,
    lat + ncell_side * cell_size / 2
  )

  rst <- terra::rast(nrows = ncell_side, ncols = ncell_side,
                      ext   = ext,
                      crs   = "EPSG:4326")
  vals <- rep(dominant, terra::ncell(rst))

  # seed mix cells deterministically
  set.seed(42)
  pos <- sample(seq_len(terra::ncell(rst)), sum(unlist(mix)))
  start <- 1L
  for (k in names(mix)) {
    n <- mix[[k]]
    if (n > 0L) {
      idx <- pos[start:(start + n - 1L)]
      vals[idx] <- as.integer(k)
      start <- start + n
    }
  }
  terra::values(rst) <- vals

  path <- path %||% tempfile(fileext = ".tif")
  terra::writeRaster(rst, path, overwrite = TRUE,
                      datatype = "INT2U")
  path
}


# A minimal PedonRecord with just lat/lon (we don't classify in these
# tests, so horizons can stay empty).
make_site_pedon <- function(lon = -43.7, lat = -22.5) {
  PedonRecord$new(
    site = list(id = "test", lat = lat, lon = lon, crs = 4326)
  )
}


test_that("spatial_prior_soilgrids returns a normalized distribution", {
  skip_if_no_terra()
  rst_path <- build_synthetic_raster()
  on.exit(unlink(rst_path), add = TRUE)

  pr <- make_site_pedon()
  prior <- spatial_prior_soilgrids(
    pr,
    buffer_m   = 600,    # ~2 cells radius -> guarantees several pixels
    source_url = rst_path
  )

  expect_s3_class(prior, "data.table")
  expect_named(prior, c("rsg_code", "probability"))
  expect_true(nrow(prior) >= 1L)
  expect_equal(sum(prior$probability), 1, tolerance = 1e-9)
  expect_true(all(prior$probability > 0))
  expect_true("FR" %in% prior$rsg_code)
})


test_that("spatial_prior_soilgrids respects the test_raster option", {
  skip_if_no_terra()
  rst_path <- build_synthetic_raster()
  on.exit(unlink(rst_path), add = TRUE)
  withr::defer(options(soilKey.test_raster = NULL))

  options(soilKey.test_raster = rst_path)
  pr <- make_site_pedon()
  prior <- spatial_prior_soilgrids(pr, buffer_m = 600)
  expect_equal(sum(prior$probability), 1, tolerance = 1e-9)
})


test_that("spatial_prior dispatcher routes to soilgrids backend", {
  skip_if_no_terra()
  rst_path <- build_synthetic_raster()
  on.exit(unlink(rst_path), add = TRUE)

  pr <- make_site_pedon()
  prior <- spatial_prior(
    pr,
    source     = "soilgrids",
    system     = "wrb2022",
    buffer_m   = 600,
    source_url = rst_path
  )
  expect_equal(sum(prior$probability), 1, tolerance = 1e-9)
})


test_that("spatial_prior aborts with a clear message on missing site", {
  pr <- PedonRecord$new(site = list(id = "no-coords"))
  expect_error(spatial_prior(pr, source_url = "anything"),
                "lat and lon")
})


test_that("spatial_prior_soilgrids errors when no source is supplied", {
  skip_if_no_terra()
  withr::defer(options(soilKey.test_raster = NULL))
  options(soilKey.test_raster = NULL)
  pr <- make_site_pedon()
  expect_error(spatial_prior_soilgrids(pr), "No raster source")
})


test_that("utm_crs_for_point picks the right southern UTM zone", {
  # -22.5 lat / -43.7 lon -> UTM zone 23 South -> 32723
  expect_equal(soilKey:::utm_crs_for_point(-43.7, -22.5), 32723L)
  # 47.0 lat / 8.5 lon  (Switzerland) -> zone 32 North -> 32632
  expect_equal(soilKey:::utm_crs_for_point(8.5, 47.0), 32632L)
})


test_that("soilgrids_wrb_lut covers all 32 RSGs", {
  lut <- soilgrids_wrb_lut()
  expect_length(lut, 32L)
  expect_true("FR" %in% lut)
  expect_true("RG" %in% lut)
  expect_true("AC" %in% lut)
})
