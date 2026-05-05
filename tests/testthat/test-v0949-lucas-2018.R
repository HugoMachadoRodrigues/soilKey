# =============================================================================
# Tests for v0.9.49 -- LUCAS Soil 2018 loader + ESDB-backed WRB benchmark.
#
# All tests build a tiny synthetic LUCAS CSV in tempdir() so they run
# unconditionally (without requiring the real 18,984-point release).
# Tests that touch the ESDB raster require terra; benchmarks build a
# synthetic 4x4 raster on the fly when terra is available.
# =============================================================================


# ---- Synthetic LUCAS folder builder -------------------------------------

.make_synth_lucas_dir <- function() {
  dir <- tempfile("lucas_v0949_")
  dir.create(dir)
  csv <- file.path(dir, "LUCAS-SOIL-2018.csv")
  # Header from the real 2018 release (27 cols).
  hdr <- paste(
    "Depth", "POINTID", "pH_CaCl2", "pH_H2O", "EC", "OC", "CaCO3", "P", "N",
    "K", "OC (20-30 cm)", "CaCO3 (20-30 cm)", "Ox_Al", "Ox_Fe",
    "NUTS_0", "NUTS_1", "NUTS_2", "NUTS_3", "TH_LAT", "TH_LONG",
    "SURVEY_DATE", "Elev", "LC", "LU", "LC0_Desc", "LC1_Desc", "LU1_Desc",
    sep = ","
  )
  rows <- c(
    # Spain, lat 40 lon -4 -- typical mediterranean topsoil
    paste("0-20 cm", "1001", "5.6", "6.3", "9.5", "21.8", "5", "26.5", "2.0",
            "153", "13.4", "5",     "0.9", "1.9",
            "ES", "ES1", "ES11", "ES111", "40.0", "-4.0",
            "10-06-18", "650", "C23", "U111", "Cropland", "Cereal cropland",
            "Common wheat", sep = ","),
    # France, lat 48 lon 2 -- with subsoil chemistry
    paste("0-20 cm", "1002", "6.8", "7.2", "12.0", "15.2", "10", "30.0", "1.4",
            "200", "8.0",  "5",     "0.5", "1.2",
            "FR", "FR1", "FR10", "FR101", "48.0", "2.0",
            "12-06-18", "120", "C10", "U120", "Grassland", "Permanent grassland",
            "Grass", sep = ","),
    # Sweden, lat 59 lon 18 -- with "< LOD" handling on P + missing Ox_Fe
    paste("0-20 cm", "1003", "4.2", "4.8", "8.7", "92.4", "< LOD", "< LOD",
            "5.5", "120", "", "",   "", "",
            "SE", "SE1", "SE11", "SE110", "59.0", "18.0",
            "20-06-18", "30", "C30", "U130", "Forest", "Coniferous forest",
            "Pine", sep = ","),
    # Italy, lat 42 lon 12
    paste("0-20 cm", "1004", "7.4", "7.9", "20.0", "12.0", "200", "15.5",
            "1.1", "180", "", "",   "1.0", "2.0",
            "IT", "IT1", "IT11", "IT111", "42.0", "12.0",
            "15-06-18", "200", "C20", "U150", "Cropland", "Olive groves",
            "Olive", sep = ",")
  )
  writeLines(c(hdr, rows), csv)

  # Add a tiny BulkDensity sister file for IDs 1001 and 1002
  bd_csv <- file.path(dir, "BulkDensity_2018_final-2.csv")
  writeLines(c(
    "POINT_ID,BD 0-10,BD 10-20,BD 20-30,BD 0-20",
    "1001,1.31,1.42,1.51,1.36",
    "1002,1.20,1.25,1.30,1.22"
  ), bd_csv)

  dir
}


# ---- Loader: basic ------------------------------------------------------

test_that("load_lucas_soil_2018 reads chemistry + BD and builds PedonRecord list", {
  dir <- .make_synth_lucas_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  pedons <- load_lucas_soil_2018(dir, verbose = FALSE)

  expect_type(pedons, "list")
  expect_length(pedons, 4L)
  expect_true(all(vapply(pedons, inherits, logical(1L), "PedonRecord")))

  # ID 1001 row: pH_H2O = 6.3, OC = 21.8 g/kg -> 2.18 %
  p1 <- pedons[[1L]]
  expect_equal(p1$site$id, "1001")
  expect_equal(p1$site$lat, 40.0)
  expect_equal(p1$site$lon, -4.0)
  expect_equal(p1$site$country, "ES")
  expect_equal(p1$horizons$ph_h2o[1L],  6.3)
  expect_equal(p1$horizons$oc_pct[1L],  2.18, tolerance = 1e-6)
  expect_equal(p1$horizons$caco3_pct[1L], 0.5)
  expect_equal(p1$horizons$ec_dS_m[1L], 9.5 * 0.01)
  # BD attached for 1001 (BD 0-20 = 1.36)
  expect_equal(p1$horizons$bulk_density_g_cm3[1L], 1.36)
})


test_that("load_lucas_soil_2018 builds a subsoil horizon when 20-30 cm chemistry is finite", {
  dir <- .make_synth_lucas_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  pedons <- load_lucas_soil_2018(dir, verbose = FALSE)

  # Rows 1 and 2 have OC (20-30 cm) finite -> 2 horizons
  expect_equal(nrow(pedons[[1L]]$horizons), 2L)
  expect_equal(nrow(pedons[[2L]]$horizons), 2L)
  # Rows 3 and 4 have no subsoil chemistry -> 1 horizon
  expect_equal(nrow(pedons[[3L]]$horizons), 1L)
  expect_equal(nrow(pedons[[4L]]$horizons), 1L)
  # Subsoil OC for row 1: 13.4 g/kg -> 1.34 %
  expect_equal(pedons[[1L]]$horizons$oc_pct[2L], 1.34, tolerance = 1e-6)
})


test_that("load_lucas_soil_2018 handles '< LOD' and empty cells as NA", {
  dir <- .make_synth_lucas_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  pedons <- load_lucas_soil_2018(dir, verbose = FALSE)

  # Row 3 (Sweden): P "< LOD", CaCO3 "< LOD"
  p3 <- pedons[[3L]]
  expect_true(is.na(p3$horizons$p_mehlich3_mg_kg[1L]))
  expect_true(is.na(p3$horizons$caco3_pct[1L]))
  expect_true(is.na(p3$horizons$fe_ox_pct[1L]))
})


test_that("load_lucas_soil_2018 honours the 'countries' and 'max_n' filters", {
  dir <- .make_synth_lucas_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  ped_es <- load_lucas_soil_2018(dir, countries = c("ES"), verbose = FALSE)
  expect_length(ped_es, 1L)
  expect_equal(ped_es[[1L]]$site$country, "ES")

  ped_2 <- load_lucas_soil_2018(dir, max_n = 2L, verbose = FALSE)
  expect_length(ped_2, 2L)
})


test_that("load_lucas_soil_2018 errors when path does not contain LUCAS-SOIL-2018.csv", {
  dir <- tempfile("empty_v0949_")
  dir.create(dir); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  expect_error(load_lucas_soil_2018(dir), "LUCAS-SOIL-2018.csv")
  expect_error(load_lucas_soil_2018("/no/such/path"), "does not exist")
})


# ---- WRB code mapping --------------------------------------------------

test_that(".WRB_LV1_NAME_BY_CODE covers the 30 active WRB 2022 RSGs", {
  m <- soilKey:::.WRB_LV1_NAME_BY_CODE
  # 31 codes including the legacy 'AB' which is mapped to NA
  expect_equal(length(m), 31L)
  expect_true("FL" %in% names(m))
  expect_true("FR" %in% names(m))
  expect_equal(unname(m["FL"]), "Fluvisols")
  expect_equal(unname(m["FR"]), "Ferralsols")
  # Legacy AB (Albeluvisols) -> NA
  expect_true(is.na(unname(m["AB"])))
})


# ---- Internal helpers ---------------------------------------------------

test_that(".lucas_numeric handles '< LOD' / '<LOD' / '' / 'n.d.' / 'ND' as NA", {
  expect_true(is.na(soilKey:::.lucas_numeric("< LOD")))
  expect_true(is.na(soilKey:::.lucas_numeric("<LOD")))
  expect_true(is.na(soilKey:::.lucas_numeric("")))
  expect_true(is.na(soilKey:::.lucas_numeric("n.d.")))
  expect_true(is.na(soilKey:::.lucas_numeric("ND")))
  expect_true(is.na(soilKey:::.lucas_numeric(NA)))
  expect_equal(soilKey:::.lucas_numeric("21.8"), 21.8)
  expect_equal(soilKey:::.lucas_numeric(c("5", "< LOD", "10")),
                c(5, NA_real_, 10))
})


# ---- Benchmark: synthetic ESDB raster + comparison loop -----------------

.make_synth_esdb_root <- function(coords_to_codes) {
  testthat::skip_if_not_installed("terra")
  root <- tempfile("esdb_v0949_"); dir.create(root)
  attr_dir <- file.path(root, "WRBLV1"); dir.create(attr_dir)
  # 4x4 raster covering Europe with 4 RSG codes by quadrant.
  # Codes 21, 11, 30, 7 correspond to LV (Luvisols), FR (Ferralsols
  # surrogate), TC (Technosols), CM (Cambisols) -- but for the
  # benchmark we just need consistent integer codes and a VAT.
  r <- terra::rast(nrows = 4, ncols = 4,
                    xmin = -10, xmax = 30,
                    ymin = 35,  ymax = 65,
                    crs  = "EPSG:4326")
  vals <- rep(c(21L, 11L, 30L, 7L), length.out = 16L)
  terra::values(r) <- vals
  tif <- file.path(attr_dir, "WRBLV1.tif")
  terra::writeRaster(r, tif, overwrite = TRUE)

  # VAT mapping integer -> 2-letter WRB code
  vat_path <- file.path(attr_dir, "WRBLV1.vat.dbf")
  vat <- data.frame(
    Value = c(7L, 11L, 21L, 30L),
    Count = c(4L, 4L, 4L, 4L),
    LV1   = c("CM", "FR", "LV", "TC"),
    stringsAsFactors = FALSE
  )
  if (requireNamespace("foreign", quietly = TRUE)) {
    foreign::write.dbf(vat, vat_path)
  }
  list(root = root, raster = tif, vat = vat_path)
}


test_that("benchmark_lucas_2018 runs end-to-end on a tiny synthetic stack", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("foreign")
  dir <- .make_synth_lucas_dir()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  esdb <- .make_synth_esdb_root()
  on.exit(unlink(esdb$root, recursive = TRUE), add = TRUE)

  pedons <- load_lucas_soil_2018(dir, verbose = FALSE)
  bench  <- benchmark_lucas_2018(pedons,
                                   esdb_root = esdb$root,
                                   fill_texture_from = "none",
                                   verbose = FALSE)

  expect_type(bench, "list")
  expect_named(bench, c("predictions", "confusion", "accuracy",
                          "per_rsg", "n_in_scope", "n_total",
                          "n_errors", "errors", "config"))
  expect_s3_class(bench$predictions, "data.frame")
  expect_equal(nrow(bench$predictions), length(pedons))
  expect_true(all(c("predicted", "reference_code", "reference_name",
                     "agree") %in% names(bench$predictions)))
  expect_equal(bench$config$classify_with, "wrb2022")
  expect_equal(bench$config$fill_texture_from, "none")
})


test_that("benchmark_lucas_2018 attaches reference_name when reference_code maps to a known RSG", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("foreign")
  dir <- .make_synth_lucas_dir(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  esdb <- .make_synth_esdb_root(); on.exit(unlink(esdb$root, recursive = TRUE), add = TRUE)
  pedons <- load_lucas_soil_2018(dir, verbose = FALSE)
  bench  <- benchmark_lucas_2018(pedons, esdb_root = esdb$root,
                                   verbose = FALSE)
  # All reference_codes should be in {CM, FR, LV, TC}
  refs <- bench$predictions$reference_code
  expect_true(all(refs %in% c("CM", "FR", "LV", "TC", NA_character_)))
  # And reference_name should resolve those to plural English names
  expected_names <- c(CM = "Cambisols", FR = "Ferralsols",
                       LV = "Luvisols",  TC = "Technosols")
  for (i in seq_along(refs)) {
    if (!is.na(refs[i])) {
      expect_equal(bench$predictions$reference_name[i],
                    unname(expected_names[refs[i]]))
    }
  }
})


test_that("benchmark_lucas_2018 errors on empty / non-PedonRecord input", {
  expect_error(benchmark_lucas_2018(list(), esdb_root = "/tmp"),
               "non-empty list")
  expect_error(benchmark_lucas_2018(list(1, 2), esdb_root = "/tmp"),
               "PedonRecord")
})


test_that("benchmark_lucas_2018 supports classify_with = 'sibcs'", {
  testthat::skip_if_not_installed("terra")
  testthat::skip_if_not_installed("foreign")
  dir <- .make_synth_lucas_dir(); on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  esdb <- .make_synth_esdb_root(); on.exit(unlink(esdb$root, recursive = TRUE), add = TRUE)
  pedons <- load_lucas_soil_2018(dir, verbose = FALSE)
  bench  <- benchmark_lucas_2018(pedons, esdb_root = esdb$root,
                                   classify_with = "sibcs",
                                   verbose = FALSE)
  expect_equal(bench$config$classify_with, "sibcs")
  # At least one prediction set
  expect_true(any(!is.na(bench$predictions$predicted)))
})
