# =============================================================================
# Tests for v0.9.46 -- train_pls_from_ossl() + predict_from_spectra() +
# save/load_ossl_models() + predict.soilKey_pls_model() S3 method.
#
# Heavy lifting requires the optional `pls` package; tests are skipped
# cleanly when it is not available.
# =============================================================================

# ---- Synthetic OSSL fixture ---------------------------------------------

.make_synth_ossl_lib <- function(n = 200L, seed = 42L,
                                    wavelengths = seq(400, 2400, by = 10)) {
  set.seed(seed)
  X <- matrix(stats::runif(n * length(wavelengths), 0, 1),
                nrow = n, ncol = length(wavelengths))
  colnames(X) <- as.character(wavelengths)
  # Two synthetic targets that are linearly related to spectral PCs so PLS
  # can actually fit them. Use the first PC as a coarse covariate.
  pc1 <- prcomp(X)$x[, 1L]
  Yr <- data.frame(
    clay_pct = 30 + 10 * scale(pc1)[, 1L] + stats::rnorm(n, 0, 2),
    ph_h2o   = 5.5 + 0.5 * scale(pc1)[, 1L] + stats::rnorm(n, 0, 0.2)
  )
  list(Xr = X, Yr = Yr)
}


# ---- train_pls_from_ossl() requires pls ---------------------------------

test_that("train_pls_from_ossl errors clearly when pls is missing", {
  if (requireNamespace("pls", quietly = TRUE)) {
    skip("pls is installed -- cannot exercise the missing-package path")
  }
  lib <- list(Xr = matrix(1:30, nrow = 5),
                Yr = data.frame(clay_pct = 1:5))
  expect_error(train_pls_from_ossl(lib, properties = "clay_pct"), "pls")
})


# ---- train_pls_from_ossl() shape validation -----------------------------

test_that("train_pls_from_ossl rejects malformed ossl_library", {
  skip_if_not_installed("pls")
  expect_error(train_pls_from_ossl(list(Xr = matrix(1:6, 2)),
                                      properties = "clay_pct"),
               "Xr.*Yr|Yr.*Xr")
  expect_error(train_pls_from_ossl(list(Xr = matrix(1:6, 2),
                                           Yr = data.frame(a = 1:3)),
                                      properties = "a"),
               "nrow")
})


# ---- train_pls_from_ossl() trains a model and reports diagnostics -------

test_that("train_pls_from_ossl trains a model from synthetic data", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib,
                                  properties = c("clay_pct", "ph_h2o"),
                                  ncomp_max  = 5L,
                                  validation = "CV",
                                  segments   = 5L,
                                  preprocess = "snv+sg1",
                                  verbose    = FALSE)
  expect_type(models, "list")
  expect_equal(sort(names(models)), c("clay_pct", "ph_h2o"))
  for (prop in names(models)) {
    m <- models[[prop]]
    expect_s3_class(m, "soilKey_pls_model")
    expect_true(m$ncomp_opt >= 1L && m$ncomp_opt <= 5L)
    expect_true(is.finite(m$rmse_train))
    expect_equal(m$preprocess, "snv+sg1")
    expect_equal(m$property, prop)
  }
  expect_true(!is.null(attr(models, "trained_at")))
  expect_true(!is.null(attr(models, "soilKey_version")))
})


# ---- predict.soilKey_pls_model() returns the right schema ---------------

test_that("predict.soilKey_pls_model returns value/pi95_low/pi95_high", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib,
                                  properties = "clay_pct",
                                  ncomp_max  = 4L,
                                  validation = "CV",
                                  segments   = 5L,
                                  verbose    = FALSE)
  Xp <- preprocess_spectra(lib$Xr[1:3, ], method = "snv+sg1")
  out <- predict(models$clay_pct, Xp)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("value", "pi95_low", "pi95_high"))
  expect_equal(nrow(out), 3L)
  expect_true(all(out$pi95_low <= out$value, na.rm = TRUE))
  expect_true(all(out$value <= out$pi95_high, na.rm = TRUE))
})


# ---- predict_from_spectra() with raw matrix ----------------------------

test_that("predict_from_spectra accepts a raw matrix and returns long-form", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib, properties = "clay_pct",
                                  ncomp_max = 4L, validation = "CV",
                                  segments = 5L, verbose = FALSE)
  preds <- predict_from_spectra(lib$Xr[1:5, ], models = models)
  expect_s3_class(preds, "data.frame")
  expect_true(all(c("horizon_idx", "property", "value",
                     "pi95_low", "pi95_high") %in% names(preds)))
  expect_equal(nrow(preds), 5L)
  # backend should be the real path (not synthetic) when models are real
  expect_equal(attr(preds, "backend"), "pretrained")
})


# ---- predict_from_spectra() with PedonRecord delegates -----------------

test_that("predict_from_spectra(PedonRecord) writes to provenance", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib, properties = "clay_pct",
                                  ncomp_max = 4L, validation = "CV",
                                  segments = 5L, verbose = FALSE)
  pedon <- make_synthetic_pedon_with_spectra(
    n_horizons = 2L,
    wavelengths = as.integer(colnames(lib$Xr))
  )
  pedon$horizons$clay_pct <- NA_real_  # ensure overwrite-skip is exercised
  before_n <- nrow(pedon$provenance)
  out <- predict_from_spectra(pedon, models = models, overwrite = TRUE,
                                verbose = FALSE)
  expect_s3_class(out, "PedonRecord")
  expect_true(nrow(out$provenance) > before_n)
  expect_true("predicted_spectra" %in% out$provenance$source)
})


# ---- predict_from_spectra() requires models -----------------------------

test_that("predict_from_spectra errors when models is NULL or empty", {
  expect_error(predict_from_spectra(matrix(0, 1, 5)), "models")
  expect_error(predict_from_spectra(matrix(0, 1, 5), models = list()), "models")
})


# ---- predict_from_spectra() rejects unknown properties ------------------

test_that("predict_from_spectra rejects properties not in models", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib, properties = "clay_pct",
                                  ncomp_max = 4L, validation = "CV",
                                  segments = 5L, verbose = FALSE)
  expect_error(
    predict_from_spectra(lib$Xr[1, , drop = FALSE], models = models,
                          properties = c("clay_pct", "magnesium")),
    "no models for"
  )
})


# ---- save_ossl_models() / load_ossl_models() round-trip ----------------

test_that("save/load_ossl_models round-trips a model list", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib, properties = "clay_pct",
                                  ncomp_max = 3L, validation = "CV",
                                  segments = 5L, verbose = FALSE)
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  save_ossl_models(models, tf)
  expect_true(file.exists(tf))
  loaded <- load_ossl_models(tf)
  expect_equal(names(loaded), names(models))
  expect_s3_class(loaded$clay_pct, "soilKey_pls_model")
  # Predictions should match deterministically
  Xp <- preprocess_spectra(lib$Xr[1, , drop = FALSE], method = "snv+sg1")
  expect_equal(predict(models$clay_pct, Xp),
                predict(loaded$clay_pct, Xp))
})


# ---- save_ossl_models() rejects malformed input -------------------------

test_that("save_ossl_models errors on non-soilKey_pls_model lists", {
  expect_error(save_ossl_models(list(), tempfile()), "non-empty")
  expect_error(save_ossl_models(list(a = 1), tempfile()),
               "soilKey_pls_model")
})


# ---- load_ossl_models() errors when file is missing ---------------------

test_that("load_ossl_models errors when the file does not exist", {
  expect_error(load_ossl_models(file.path(tempdir(),
                                            "no_such_file_v0946.rds")),
                "not found")
})


# ---- predict.soilKey_pls_model() ncol mismatch error -------------------

test_that("predict.soilKey_pls_model errors on ncol mismatch", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib, properties = "clay_pct",
                                  ncomp_max = 3L, validation = "CV",
                                  segments = 5L, verbose = FALSE)
  Xp <- preprocess_spectra(lib$Xr[1, , drop = FALSE], method = "snv+sg1")
  truncated <- Xp[, 1:5, drop = FALSE]
  expect_error(predict(models$clay_pct, truncated),
                "expects")
})


# ---- print method shows useful summary ---------------------------------

test_that("print.soilKey_pls_model prints a summary line", {
  skip_if_not_installed("pls")
  lib <- .make_synth_ossl_lib()
  models <- train_pls_from_ossl(lib, properties = "clay_pct",
                                  ncomp_max = 3L, validation = "CV",
                                  segments = 5L, verbose = FALSE)
  out <- capture.output(print(models$clay_pct))
  expect_true(any(grepl("soilKey_pls_model", out)))
  expect_true(any(grepl("clay_pct", out)))
})
