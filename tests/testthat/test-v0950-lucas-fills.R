# =============================================================================
# Tests for v0.9.50 -- comprehensive SoilGrids fill (subsoil + cfvo + bdod
# + nitrogen) + Vis-NIR LUCAS spectra wire-up in benchmark_lucas_2018.
#
# Network is avoided via the soilgrids_lookup_fn injection point on the
# benchmark and the lookup_fn parameter on .fill_horizon_from_soilgrids.
# =============================================================================


# ---- Stubs / fixtures ---------------------------------------------------

.stub_soilgrids <- function(values_by_property = NULL,
                              values_by_depth   = NULL) {
  # Returns a function with the lookup_soilgrids() signature, returning a
  # deterministic value per (property, depth) for testing.
  function(coords, property = "phh2o", depth = "0-5cm",
             quantile = "mean", baseurl = NULL, raw = FALSE) {
    if (!is.null(values_by_depth) && depth %in% names(values_by_depth) &&
          property %in% names(values_by_depth[[depth]])) {
      return(values_by_depth[[depth]][[property]])
    }
    if (!is.null(values_by_property) &&
          property %in% names(values_by_property)) {
      return(values_by_property[[property]])
    }
    NA_real_
  }
}


.make_one_lucas_pedon <- function(id = "1001", lat = 40, lon = -4) {
  hz <- data.table::data.table(
    top_cm    = 0,
    bottom_cm = 20,
    designation = "Ap",
    ph_h2o    = 6.3,
    oc_pct    = 2.18
  )
  PedonRecord$new(
    site = list(id = id, lat = lat, lon = lon, country = "ES",
                  reference_source = "synth-lucas"),
    horizons = ensure_horizon_schema(hz)
  )
}


# ---- .SOILGRIDS_TO_HORIZON_MAP coverage ---------------------------------

test_that(".SOILGRIDS_TO_HORIZON_MAP covers the 9 SoilGrids properties", {
  m <- soilKey:::.SOILGRIDS_TO_HORIZON_MAP
  expect_setequal(names(m),
                    c("clay", "sand", "silt", "phh2o", "soc", "cec",
                      "bdod", "nitrogen", "cfvo"))
  expect_equal(m$clay$col,  "clay_pct")
  expect_equal(m$cfvo$col,  "coarse_fragments_pct")
  expect_equal(m$soc$col,   "oc_pct")
  expect_equal(m$soc$scale_secondary, 0.1)
  expect_equal(m$nitrogen$scale_secondary, 0.1)
  expect_equal(m$bdod$col, "bulk_density_g_cm3")
})


# ---- .fill_horizon_from_soilgrids: writes the horizon and applies scale ----

test_that(".fill_horizon_from_soilgrids writes 9 properties to horizon 1", {
  p <- .make_one_lucas_pedon()
  stub <- .stub_soilgrids(values_by_property = list(
    clay = 30, sand = 40, silt = 30, phh2o = 6.5, soc = 25,
    cec = 18, bdod = 1.30, nitrogen = 2.0, cfvo = 5
  ))
  written <- soilKey:::.fill_horizon_from_soilgrids(
    p, horizon_idx = 1L,
    properties = names(soilKey:::.SOILGRIDS_TO_HORIZON_MAP),
    soilgrids_depth = "0-5cm",
    lookup_fn = stub
  )
  # ph_h2o (6.3) and oc_pct (2.18) were already finite -> not overwritten
  expect_equal(written, 7L)
  h <- p$horizons
  expect_equal(h$clay_pct[1L],              30)
  expect_equal(h$cec_cmol[1L],              18)
  expect_equal(h$bulk_density_g_cm3[1L],    1.30)
  expect_equal(h$coarse_fragments_pct[1L],  5)
  expect_equal(h$oc_pct[1L],                2.18)   # already finite, kept
  # New writes: secondary scale applied
  expect_equal(h$n_total_pct[1L],           0.2)    # 2.0 g/kg * 0.1
  expect_equal(h$ph_h2o[1L],                6.3)    # original kept
})


test_that(".fill_horizon_from_soilgrids synthesises a subsoil horizon", {
  p <- .make_one_lucas_pedon()
  expect_equal(nrow(p$horizons), 1L)
  stub <- .stub_soilgrids(values_by_property = list(
    clay = 40, sand = 30, silt = 30, soc = 5
  ))
  soilKey:::.fill_horizon_from_soilgrids(
    p, horizon_idx = 2L,
    properties = c("clay", "sand", "silt", "soc"),
    soilgrids_depth     = "30-60cm",
    horizon_top_cm      = 30,
    horizon_bottom_cm   = 60,
    horizon_designation = "B",
    lookup_fn = stub
  )
  expect_equal(nrow(p$horizons), 2L)
  expect_equal(p$horizons$top_cm[2L], 30)
  expect_equal(p$horizons$bottom_cm[2L], 60)
  expect_equal(p$horizons$designation[2L], "B")
  expect_equal(p$horizons$clay_pct[2L], 40)
  expect_equal(p$horizons$oc_pct[2L], 0.5)  # 5 g/kg * 0.1 -> 0.5%
})


test_that(".fill_horizon_from_soilgrids respects existing finite values", {
  p <- .make_one_lucas_pedon()
  stub <- .stub_soilgrids(values_by_property = list(
    phh2o = 7.5,  # different from existing 6.3
    clay  = 30
  ))
  soilKey:::.fill_horizon_from_soilgrids(
    p, horizon_idx = 1L,
    properties = c("phh2o", "clay"),
    lookup_fn  = stub
  )
  expect_equal(p$horizons$ph_h2o[1L], 6.3)   # preserved
  expect_equal(p$horizons$clay_pct[1L], 30)  # filled
})


test_that(".fill_horizon_from_soilgrids skips when coords are NA", {
  p <- .make_one_lucas_pedon(lat = NA, lon = NA)
  stub <- .stub_soilgrids(values_by_property = list(clay = 30))
  written <- soilKey:::.fill_horizon_from_soilgrids(
    p, horizon_idx = 1L, properties = "clay", lookup_fn = stub
  )
  expect_equal(written, 0L)
  expect_true(is.na(p$horizons$clay_pct[1L]))
})


# ---- benchmark_lucas_2018: backward compat with fill_texture_from -------

.skip_if_no_terra <- function() testthat::skip_if_not_installed("terra")
.skip_if_no_foreign <- function() testthat::skip_if_not_installed("foreign")

.make_synth_esdb_root_v0950 <- function() {
  .skip_if_no_terra()
  root <- tempfile("esdb_v0950_"); dir.create(root)
  attr_dir <- file.path(root, "WRBLV1"); dir.create(attr_dir)
  r <- terra::rast(nrows = 4, ncols = 4,
                    xmin = -10, xmax = 30,
                    ymin = 35,  ymax = 65,
                    crs  = "EPSG:4326")
  terra::values(r) <- rep(c(21L, 11L, 30L, 7L), length.out = 16L)
  terra::writeRaster(r, file.path(attr_dir, "WRBLV1.tif"), overwrite = TRUE)
  if (requireNamespace("foreign", quietly = TRUE)) {
    foreign::write.dbf(
      data.frame(Value = c(7L, 11L, 21L, 30L),
                 Count = c(4L, 4L, 4L, 4L),
                 LV1   = c("CM", "FR", "LV", "TC"),
                 stringsAsFactors = FALSE),
      file.path(attr_dir, "WRBLV1.vat.dbf")
    )
  }
  root
}


test_that("benchmark_lucas_2018 still accepts fill_texture_from = 'none'", {
  .skip_if_no_terra(); .skip_if_no_foreign()
  esdb <- .make_synth_esdb_root_v0950()
  on.exit(unlink(esdb, recursive = TRUE), add = TRUE)
  pedons <- list(.make_one_lucas_pedon())
  bench <- benchmark_lucas_2018(pedons, esdb_root = esdb,
                                  fill_texture_from = "none",
                                  verbose = FALSE)
  expect_equal(bench$config$fill_topsoil_from, "none")
  expect_equal(bench$config$fill_subsoil_from, "none")
})


test_that("fill_texture_from = 'soilgrids' (legacy) maps to topsoil-only clay/sand/silt", {
  .skip_if_no_terra(); .skip_if_no_foreign()
  esdb <- .make_synth_esdb_root_v0950()
  on.exit(unlink(esdb, recursive = TRUE), add = TRUE)
  pedons <- list(.make_one_lucas_pedon())
  stub <- .stub_soilgrids(values_by_property = list(
    clay = 25, sand = 50, silt = 25
  ))
  bench <- benchmark_lucas_2018(pedons, esdb_root = esdb,
                                  fill_texture_from   = "soilgrids",
                                  soilgrids_lookup_fn = stub,
                                  verbose = FALSE)
  expect_equal(bench$config$fill_topsoil_from, "soilgrids")
  expect_equal(bench$config$fill_subsoil_from, "none")
  expect_setequal(bench$config$fill_properties, c("clay", "sand", "silt"))
  expect_equal(pedons[[1L]]$horizons$clay_pct[1L], 25)
})


# ---- New: subsoil fill synthesises a 30-60 cm horizon -----------------

test_that("benchmark_lucas_2018 synthesises a subsoil horizon when fill_subsoil_from='soilgrids'", {
  .skip_if_no_terra(); .skip_if_no_foreign()
  esdb <- .make_synth_esdb_root_v0950()
  on.exit(unlink(esdb, recursive = TRUE), add = TRUE)
  pedons <- list(.make_one_lucas_pedon())
  expect_equal(nrow(pedons[[1L]]$horizons), 1L)
  stub <- .stub_soilgrids(values_by_depth = list(
    "0-5cm"   = list(clay = 25, sand = 50, silt = 25),
    "30-60cm" = list(clay = 45, sand = 30, silt = 25, cec = 22, bdod = 1.4,
                       phh2o = 7.0, soc = 5, nitrogen = 0.5, cfvo = 10)
  ))
  bench <- benchmark_lucas_2018(pedons, esdb_root = esdb,
                                  fill_topsoil_from   = "soilgrids",
                                  fill_subsoil_from   = "soilgrids",
                                  soilgrids_lookup_fn = stub,
                                  verbose = FALSE)
  expect_equal(bench$config$fill_subsoil_from, "soilgrids")
  hz <- pedons[[1L]]$horizons
  expect_equal(nrow(hz), 2L)
  expect_equal(hz$top_cm[2L], 30)
  expect_equal(hz$bottom_cm[2L], 60)
  expect_equal(hz$clay_pct[2L], 45)
  expect_equal(hz$cec_cmol[2L], 22)
})


# ---- cfvo proxy lifts Leptosols ----------------------------------------

test_that("cfvo >= 90 from SoilGrids unlocks Leptosols (leptic_features predicate)", {
  .skip_if_no_terra(); .skip_if_no_foreign()
  esdb <- .make_synth_esdb_root_v0950()
  on.exit(unlink(esdb, recursive = TRUE), add = TRUE)
  pedons <- list(.make_one_lucas_pedon())
  stub <- .stub_soilgrids(values_by_property = list(cfvo = 95))
  bench <- benchmark_lucas_2018(pedons, esdb_root = esdb,
                                  fill_topsoil_from   = "soilgrids",
                                  fill_properties     = c("cfvo"),
                                  soilgrids_lookup_fn = stub,
                                  verbose = FALSE)
  hz <- pedons[[1L]]$horizons
  expect_equal(hz$coarse_fragments_pct[1L], 95)
  # The classifier may now return Leptosols (unless other RSGs win first)
  # We only verify that leptic_features fires on this horizon directly:
  lf <- soilKey::leptic_features(pedons[[1L]])
  expect_true(isTRUE(lf$passed))
})


# ---- fill_topsoil_from = 'spectra' requires ossl_models -----------------

test_that("benchmark_lucas_2018 errors when 'spectra' fill is requested without models", {
  .skip_if_no_terra()
  pedons <- list(.make_one_lucas_pedon())
  expect_error(
    benchmark_lucas_2018(pedons, esdb_root = tempdir(),
                          fill_topsoil_from = "spectra",
                          ossl_models = NULL, verbose = FALSE),
    "ossl_models"
  )
})


# ---- attach_lucas_spectra: wide format ----------------------------------

test_that("attach_lucas_spectra joins a wide spectra table by POINT_ID", {
  pedons <- list(
    .make_one_lucas_pedon(id = "A1"),
    .make_one_lucas_pedon(id = "A2"),
    .make_one_lucas_pedon(id = "A3")
  )
  wl <- seq(400, 2400, by = 100)
  wide <- data.frame(POINT_ID = c("A1", "A2", "A4"))
  for (w in wl) wide[[as.character(w)]] <- runif(3, 0, 1)
  attach_lucas_spectra(pedons, wide, point_id_col = "POINT_ID",
                        verbose = FALSE)
  expect_false(is.null(pedons[[1L]]$spectra$vnir))
  expect_equal(ncol(pedons[[1L]]$spectra$vnir), length(wl))
  expect_equal(as.numeric(colnames(pedons[[1L]]$spectra$vnir)), wl)
  expect_false(is.null(pedons[[2L]]$spectra$vnir))
  # A3 has no row in spectra table -> stays NULL
  expect_true(is.null(pedons[[3L]]$spectra) ||
                is.null(pedons[[3L]]$spectra$vnir))
})


# ---- attach_lucas_spectra: long format ---------------------------------

test_that("attach_lucas_spectra joins a long spectra table by POINT_ID", {
  pedons <- list(.make_one_lucas_pedon(id = "B1"))
  long <- data.frame(
    POINT_ID      = rep("B1", 5),
    wavelength_nm = c(400, 500, 600, 700, 800),
    reflectance   = c(0.1, 0.15, 0.2, 0.25, 0.3)
  )
  attach_lucas_spectra(pedons, long, verbose = FALSE)
  m <- pedons[[1L]]$spectra$vnir
  expect_false(is.null(m))
  expect_equal(ncol(m), 5L)
  expect_equal(as.numeric(m[1L, ]), c(0.1, 0.15, 0.2, 0.25, 0.3))
})


test_that("attach_lucas_spectra errors on missing point_id column", {
  pedons <- list(.make_one_lucas_pedon(id = "C1"))
  bad <- data.frame(`400` = 0.1, `500` = 0.2, check.names = FALSE)
  expect_error(attach_lucas_spectra(pedons, bad, point_id_col = "POINT_ID"),
                "POINT_ID")
})


# ---- fill_properties validation ----------------------------------------

test_that("benchmark_lucas_2018 rejects unknown fill_properties", {
  .skip_if_no_terra(); .skip_if_no_foreign()
  esdb <- .make_synth_esdb_root_v0950()
  on.exit(unlink(esdb, recursive = TRUE), add = TRUE)
  pedons <- list(.make_one_lucas_pedon())
  expect_error(
    benchmark_lucas_2018(pedons, esdb_root = esdb,
                          fill_topsoil_from = "soilgrids",
                          fill_properties   = c("clay", "magnesium"),
                          verbose = FALSE),
    "magnesium"
  )
})
