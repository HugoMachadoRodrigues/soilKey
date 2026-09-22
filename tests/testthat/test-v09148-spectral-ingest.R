# v0.9.148 — spectral-dataset ingestion scaffolding
#
# read_spectral_library() / pedons_from_spectral_table() turn an arbitrary
# Vis-NIR/MIR reflectance + lab-label table (e.g. a BR library) into the
# canonical list(Xr, Yr) + PedonRecords-with-spectra the engine consumes;
# benchmark_spectral_fill() measures the ON/OFF accuracy lift; and the gapfill
# dispatcher gains a "spectra" method. All opt-in (default path byte-identical).

mk_spectral_table <- function(n = 18L, by = 25L) {
  set.seed(42)
  wl   <- seq(350, 2500, by = by)
  Xr   <- matrix(runif(n * length(wl), 5, 60), nrow = n)   # percent scale
  colnames(Xr) <- as.character(wl)
  refl <- data.frame(id = sprintf("P%02d", seq_len(n)), Xr, check.names = FALSE)
  # Portuguese headers on purpose, to exercise the alias map.
  meta <- data.frame(
    id              = sprintf("P%02d", seq_len(n)),
    argila          = round(runif(n, 10, 60)),
    silte           = round(runif(n, 5, 30)),
    areia           = round(runif(n, 20, 70)),
    ph              = round(runif(n, 4, 7), 1),
    carbono         = round(runif(n, 0.3, 4), 2),
    reference_sibcs = rep(c("LATOSSOLO VERMELHO", "ARGISSOLO AMARELO",
                            "NEOSSOLO LITOLICO"), length.out = n),
    lat = runif(n, -23, -5), lon = runif(n, -55, -38),
    stringsAsFactors = FALSE)
  list(refl = refl, meta = meta, wl = wl)
}

test_that("read_spectral_library maps Portuguese headers + normalises percent", {
  skip_on_cran()
  tb  <- mk_spectral_table()
  lib <- read_spectral_library(tb$refl, tb$meta, id_col = "id", verbose = FALSE)
  expect_true(all(c("Xr", "Yr", "metadata") %in% names(lib)))
  expect_equal(nrow(lib$Xr), 18L)
  expect_equal(ncol(lib$Xr), length(tb$wl))
  # argila->clay_pct, silte->silt_pct, areia->sand_pct, ph->ph_h2o, carbono->oc_pct
  expect_true(all(c("clay_pct", "silt_pct", "sand_pct", "ph_h2o", "oc_pct") %in%
                    names(lib$Yr)))
  expect_true("sibcs_ordem" %in% names(lib$Yr))   # reference_sibcs -> label
  expect_true(all(c("lat", "lon") %in% names(lib$Yr)))
  expect_lte(max(lib$Xr), 1)                       # percent -> fraction
  expect_gte(min(lib$Xr), 0)
})

test_that("read_spectral_library accepts long format + explicit property_map", {
  skip_on_cran()
  tb   <- mk_spectral_table(n = 6L)
  long <- do.call(rbind, lapply(seq_len(nrow(tb$refl)), function(i) {
    data.frame(id = tb$refl$id[i], wavelength_nm = tb$wl,
               reflectance = as.numeric(tb$refl[i, -1]) / 100)
  }))
  lib <- read_spectral_library(long, tb$meta, id_col = "id",
                               normalize = "none",
                               property_map = list(clay_pct = "argila"),
                               verbose = FALSE)
  expect_equal(nrow(lib$Xr), 6L)
  expect_true("clay_pct" %in% names(lib$Yr))
})

test_that("read_spectral_library can resample onto a target grid", {
  skip_on_cran()
  tb  <- mk_spectral_table(by = 50L)
  lib <- read_spectral_library(tb$refl, tb$meta, id_col = "id",
                               resample_to = seq(400, 2400, by = 10),
                               verbose = FALSE)
  expect_equal(ncol(lib$Xr), length(seq(400, 2400, by = 10)))
  expect_false(anyNA(lib$Xr[1, ]))                 # rule(2) clamp, no NA gaps
})

test_that("pedons_from_spectral_table attaches vnir + reference label", {
  skip_on_cran()
  tb   <- mk_spectral_table()
  peds <- pedons_from_spectral_table(tb$refl, tb$meta, id_col = "id",
                                     verbose = FALSE)
  expect_length(peds, 18L)
  expect_s3_class(peds[[1]], "PedonRecord")
  expect_true(is.matrix(peds[[1]]$spectra$vnir))
  expect_equal(ncol(peds[[1]]$spectra$vnir), length(tb$wl))
  expect_true(nzchar(peds[[1]]$site$reference_sibcs))
  # default keep_properties = FALSE -> horizons carry no clay (the field-scan case)
  expect_true(is.null(peds[[1]]$horizons$clay_pct) ||
                all(is.na(peds[[1]]$horizons$clay_pct)))
})

test_that("gapfill dispatcher routes method='spectra' and never mutates caller", {
  skip_on_cran()        # drives the (version-fragile) OSSL model backend
  skip_if_not(exists("ossl_demo_sa"))
  data("ossl_demo_sa", package = "soilKey")
  ps  <- make_synthetic_pedon_with_spectra(n_horizons = 3L)
  out <- suppressWarnings(soilKey:::.classify_apply_gapfill(
    ps, list(method = "spectra", ossl_library = ossl_demo_sa,
             fill_method = "plsr_local", verbose = FALSE)))
  expect_s3_class(out, "PedonRecord")
  expect_false(identical(out, ps))
  # original untouched: no predicted_spectra provenance on the caller
  expect_true(is.null(ps$provenance) ||
                !any(ps$provenance$source == "predicted_spectra", na.rm = TRUE))
})

test_that("unknown gapfill method error lists spectra", {
  skip_on_cran()
  tgt <- PedonRecord$new(
    horizons = ensure_horizon_schema(data.table::data.table(
      top_cm = 0, bottom_cm = 20, designation = "A", clay_pct = 10)))
  expect_error(
    soilKey:::.classify_apply_gapfill(tgt, list(method = "bogus")),
    "spectra")
})

test_that("benchmark_spectral_fill returns the ON/OFF structure", {
  skip_on_cran()        # drives the (version-fragile) OSSL model backend
  tb <- mk_spectral_table(n = 18L)
  b  <- suppressWarnings(benchmark_spectral_fill(
    tb$refl, tb$meta, id_col = "id", system = "sibcs", folds = 3L,
    method = "plsr_local", verbose = FALSE))
  expect_true(all(c("accuracy_off", "accuracy_on", "delta", "n",
                    "predictions") %in% names(b)))
  expect_equal(b$system, "sibcs")
  expect_gt(b$n, 0L)
  expect_s3_class(b$predictions, "data.frame")
  # accuracy on synthetic random spectra is meaningless; only the harness is tested
})
