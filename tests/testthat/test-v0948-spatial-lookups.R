# =============================================================================
# Tests for v0.9.48 -- lookup_mapbiomas_solos() + lookup_soilgrids().
#
# Network-dependent tests for SoilGrids skip cleanly when
# SOILKEY_NETWORK_TESTS is not set. Local-raster tests for MapBiomas
# build a tiny synthetic GeoTIFF on the fly via terra so they run
# unconditionally (when terra is available).
# =============================================================================


# ---- Skip helpers --------------------------------------------------------

.skip_if_no_terra <- function() {
  testthat::skip_if_not_installed("terra")
}

.skip_if_no_network <- function() {
  if (!nzchar(Sys.getenv("SOILKEY_NETWORK_TESTS"))) {
    testthat::skip(paste0("Network tests disabled. Set ",
                           "SOILKEY_NETWORK_TESTS=1 to enable."))
  }
}


# ---- MapBiomas: build a tiny synthetic raster + legend ------------------

.make_synthetic_mapbiomas <- function() {
  if (!requireNamespace("terra", quietly = TRUE)) return(NULL)
  # 4x4 raster covering Brazil with class codes 1..3
  r <- terra::rast(nrows = 4, ncols = 4,
                    xmin = -50, xmax = -40,
                    ymin = -30, ymax = -20,
                    crs  = "EPSG:4326")
  terra::values(r) <- rep(c(1L, 2L, 3L), length.out = 16L)
  tf <- tempfile(fileext = ".tif")
  terra::writeRaster(r, tf, overwrite = TRUE)
  list(
    path = tf,
    legend = data.frame(
      value = c(1L, 2L, 3L),
      class_name = c("Latossolo Vermelho-Amarelo",
                       "Argissolo Vermelho-Amarelo",
                       "Cambissolo Haplico"),
      stringsAsFactors = FALSE
    )
  )
}


# ---- lookup_mapbiomas_solos: basic shape --------------------------------

test_that("lookup_mapbiomas_solos returns the raw integer when legend is NULL", {
  .skip_if_no_terra()
  s <- .make_synthetic_mapbiomas()
  if (is.null(s)) skip("terra unavailable")
  on.exit(unlink(s$path), add = TRUE)
  out <- lookup_mapbiomas_solos(c(-45.0, -25.0), s$path, legend = NULL)
  expect_length(out, 1L)
  expect_true(is.numeric(out))
  expect_true(out %in% c(1L, 2L, 3L))
})


test_that("lookup_mapbiomas_solos decodes when legend is supplied", {
  .skip_if_no_terra()
  s <- .make_synthetic_mapbiomas()
  if (is.null(s)) skip("terra unavailable")
  on.exit(unlink(s$path), add = TRUE)
  out <- lookup_mapbiomas_solos(c(-45.0, -25.0), s$path, legend = s$legend)
  expect_length(out, 1L)
  expect_true(is.character(out))
  expect_true(out %in% s$legend$class_name)
})


test_that("lookup_mapbiomas_solos vectorises over rows", {
  .skip_if_no_terra()
  s <- .make_synthetic_mapbiomas()
  if (is.null(s)) skip("terra unavailable")
  on.exit(unlink(s$path), add = TRUE)
  coords <- rbind(c(-49, -29), c(-45, -25), c(-41, -21))
  out <- lookup_mapbiomas_solos(coords, s$path, legend = s$legend)
  expect_length(out, 3L)
  expect_true(is.character(out))
})


test_that("lookup_mapbiomas_solos errors when raster is missing", {
  .skip_if_no_terra()
  expect_error(
    lookup_mapbiomas_solos(c(-45, -25), "/no/such/file.tif"),
    "not found"
  )
})


test_that("lookup_mapbiomas_solos errors on a malformed legend", {
  .skip_if_no_terra()
  s <- .make_synthetic_mapbiomas()
  if (is.null(s)) skip("terra unavailable")
  on.exit(unlink(s$path), add = TRUE)
  expect_error(
    lookup_mapbiomas_solos(c(-45, -25), s$path,
                             legend = data.frame(x = 1)),
    "two-column"
  )
})


# ---- .soilgrids_scale: unit conversion factors --------------------------

test_that(".soilgrids_scale returns canonical conversions", {
  expect_equal(soilKey:::.soilgrids_scale("clay"),     0.1)
  expect_equal(soilKey:::.soilgrids_scale("phh2o"),    0.1)
  expect_equal(soilKey:::.soilgrids_scale("bdod"),     0.01)
  expect_equal(soilKey:::.soilgrids_scale("nitrogen"), 0.01)
  expect_equal(soilKey:::.soilgrids_scale("unknown"),  1.0)
})


# ---- lookup_soilgrids: argument validation (no network) -----------------

test_that("lookup_soilgrids rejects unknown property / depth / quantile", {
  .skip_if_no_terra()
  expect_error(lookup_soilgrids(c(0, 0), property = "magnesium"))
  expect_error(lookup_soilgrids(c(0, 0), depth = "0-1cm"))
  expect_error(lookup_soilgrids(c(0, 0), quantile = "Q0.42"))
})


test_that("lookup_soilgrids returns NA on unreachable URL", {
  .skip_if_no_terra()
  out <- suppressWarnings(
    lookup_soilgrids(
      c(-43, -22),
      property = "phh2o",
      depth = "0-5cm",
      quantile = "mean",
      baseurl = "https://localhost:1/no_such_endpoint"
    )
  )
  expect_length(out, 1L)
  expect_true(is.na(out))
})


# ---- lookup_soilgrids: live network smoke test (opt-in) -----------------

test_that("lookup_soilgrids returns finite pH for a real Brazilian point", {
  .skip_if_no_terra()
  .skip_if_no_network()
  out <- lookup_soilgrids(c(-43.0, -22.0),
                            property = "phh2o",
                            depth    = "0-5cm",
                            quantile = "mean")
  expect_length(out, 1L)
  if (!is.na(out)) {
    expect_true(out >= 3.5 && out <= 9.0)
  }
})


# ---- .coerce_lonlat helper ----------------------------------------------

test_that(".coerce_lonlat accepts vector / matrix / data.frame", {
  m1 <- soilKey:::.coerce_lonlat(c(-43, -22))
  expect_equal(dim(m1), c(1L, 2L))
  m2 <- soilKey:::.coerce_lonlat(rbind(c(-43, -22), c(0, 0)))
  expect_equal(dim(m2), c(2L, 2L))
  m3 <- soilKey:::.coerce_lonlat(data.frame(lon = -43, lat = -22))
  expect_equal(dim(m3), c(1L, 2L))
})


test_that(".coerce_lonlat rejects malformed input", {
  expect_error(soilKey:::.coerce_lonlat(c(1, 2, 3)),
                "length-2|2-col")
  expect_error(soilKey:::.coerce_lonlat(matrix(1:9, 3, 3)),
                "exactly 2 columns")
})
