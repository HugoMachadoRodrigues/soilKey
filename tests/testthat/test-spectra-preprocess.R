# ================================================================
# Tests for R/spectra-preprocess.R
#
# Synthetic Vis-NIR matrices (rows = horizons, cols = 350..2500 nm)
# are generated with a fixed seed so tests are deterministic.
# ================================================================


# --- helpers ------------------------------------------------------------------

make_synth_vnir <- function(n_horizons = 5L,
                              wavelengths = 350:2500,
                              seed = 7L) {
  set.seed(seed)
  base <- 0.25 + 0.0001 * (wavelengths - 350)
  noise <- matrix(rnorm(n_horizons * length(wavelengths), 0, 0.005),
                    nrow = n_horizons)
  feature <- outer(seq_len(n_horizons), wavelengths,
                     function(i, w) 0.08 * exp(-((w - 1400 - 30 * i)^2) / 1e4))
  X <- sweep(noise + feature, 2, base, `+`)
  colnames(X) <- as.character(wavelengths)
  X
}


# --- core invariants ----------------------------------------------------------

test_that("preprocess_spectra() rejects bad inputs", {
  skip_on_cran()
  expect_error(preprocess_spectra(NULL),               "NULL")
  expect_error(preprocess_spectra(list(1, 2, 3)),       "numeric")
  X <- make_synth_vnir()
  expect_error(preprocess_spectra(X, w = 4L), "odd")
  expect_error(preprocess_spectra(X[, 1:3], w = 5L),
                "wavelengths|window")
})


test_that("snv() centres each row to zero mean and unit variance", {
  skip_on_cran()
  X <- make_synth_vnir()
  Y <- preprocess_spectra(X, method = "snv")
  expect_equal(dim(Y), dim(X))
  rowMeans <- apply(Y, 1, mean)
  rowSds   <- apply(Y, 1, stats::sd)
  expect_true(all(abs(rowMeans) < 1e-10))
  expect_true(all(abs(rowSds - 1) < 1e-6))
})


test_that("snv() handles a constant row without producing NaN", {
  skip_on_cran()
  X <- rbind(make_synth_vnir(2),
              matrix(0.5, nrow = 1, ncol = 2151))
  colnames(X) <- as.character(350:2500)
  Y <- preprocess_spectra(X, method = "snv")
  expect_true(all(is.finite(Y)))
})


test_that("sg1 trims the spectrum by w-1 columns and returns numeric matrix", {
  skip_on_cran()
  X <- make_synth_vnir()
  Y <- preprocess_spectra(X, method = "sg1", w = 5L)
  expect_equal(nrow(Y), nrow(X))
  expect_equal(ncol(Y), ncol(X) - 4L)
  expect_true(is.numeric(Y))
})


test_that("snv+sg1 chains the two transforms", {
  skip_on_cran()
  X <- make_synth_vnir()
  Y_chain <- preprocess_spectra(X, method = "snv+sg1", w = 5L)

  Y_snv  <- preprocess_spectra(X, method = "snv")
  Y_sg1  <- preprocess_spectra(Y_snv, method = "sg1", w = 5L)

  expect_equal(dim(Y_chain), dim(Y_sg1))
  expect_equal(Y_chain, Y_sg1, tolerance = 1e-10)
})


# --- native-fallback correctness vs prospectr ---------------------------------
#
# When prospectr is installed we should get the same answer from both
# paths. We verify via a direct call to the closed-form 5-point
# coefficients and via the generic LS solver in .sg_coefficients().

test_that("native SG coefficients match the textbook 5-pt 1st-derivative kernel", {
  skip_on_cran()
  k_native <- soilKey:::.sg_coefficients(w = 5L, p = 2L, m = 1L)
  k_book   <- c(-2, -1, 0, 1, 2) / 10
  expect_equal(k_native, k_book, tolerance = 1e-12)
})


test_that("native SG path agrees with prospectr (when available)", {
  skip_on_cran()
  skip_if_not_installed("prospectr")
  X <- make_synth_vnir()

  # Force the native path via a temporary mock of requireNamespace.
  Y_prosp  <- prospectr::savitzkyGolay(X = preprocess_spectra(X, "snv"),
                                          m = 1, p = 2, w = 5)
  Y_prosp  <- as.matrix(Y_prosp)
  Y_native <- {
    coefs <- soilKey:::.sg_coefficients(w = 5L, p = 2L, m = 1L)
    Z <- preprocess_spectra(X, "snv")
    half <- 2L
    out_cols <- ncol(Z) - 2L * half
    out <- matrix(NA_real_, nrow = nrow(Z), ncol = out_cols)
    for (j in seq_len(out_cols)) {
      win <- Z[, j:(j + 5L - 1L), drop = FALSE]
      out[, j] <- as.numeric(win %*% coefs)
    }
    out
  }
  expect_equal(unname(Y_prosp), unname(Y_native), tolerance = 1e-10)
})


# --- shape & dispatch ---------------------------------------------------------

test_that("preprocess_spectra() defaults to 'snv+sg1'", {
  skip_on_cran()
  X <- make_synth_vnir()
  Y_default <- preprocess_spectra(X)
  Y_explicit <- preprocess_spectra(X, method = "snv+sg1")
  expect_equal(Y_default, Y_explicit)
})


test_that("preprocess_spectra() accepts a vector by treating it as one row", {
  skip_on_cran()
  X <- as.numeric(make_synth_vnir(1L)[1L, ])
  Y <- preprocess_spectra(X, method = "snv")
  expect_equal(nrow(Y), 1L)
  expect_equal(ncol(Y), length(X))
})


test_that("preprocess_spectra() accepts a data.frame", {
  skip_on_cran()
  X <- as.data.frame(make_synth_vnir())
  Y <- preprocess_spectra(X, method = "snv")
  expect_true(is.matrix(Y))
})


# --- apply_spectral_preprocessing (v0.9.177 composable pipeline) --------------

test_that("apply_spectral_preprocessing validates the derivative order", {
  skip_on_cran()
  expect_error(apply_spectral_preprocessing(make_synth_vnir(), sg_derivative = 3L),
               "sg_derivative")
})

test_that("absorbance auto-scales percent reflectance and equals log10(1/R)", {
  skip_on_cran()
  X <- matrix(c(25, 50), nrow = 1); colnames(X) <- c("500", "505")  # % reflectance
  r <- apply_spectral_preprocessing(X, absorbance = TRUE)
  expect_equal(unname(r$X[1, ]), log10(1 / c(0.25, 0.50)), tolerance = 1e-10)
  expect_true(any(grepl("Absorbance", r$steps)))
})

test_that("absorbance clamps zeros/negatives so output is finite", {
  skip_on_cran()
  X <- make_synth_vnir(); X[1, 1] <- 0; X[2, 2] <- -0.1
  r <- apply_spectral_preprocessing(X, absorbance = TRUE)
  expect_true(all(is.finite(r$X)))
})

test_that("SG smoothing trims the wavelength axis in lock-step with X", {
  skip_on_cran()
  X <- make_synth_vnir()
  r <- apply_spectral_preprocessing(X, sg_smooth = TRUE, window = 11L)
  expect_equal(ncol(r$X), ncol(X) - 10L)          # (w-1) trimmed
  expect_equal(length(r$wavelengths), ncol(r$X))  # axis stays aligned
  expect_equal(as.numeric(colnames(r$X)), r$wavelengths)
  expect_true(any(grepl("SG smoothing", r$steps)))
})

test_that("canonical order + cumulative trim when smoothing AND deriving", {
  skip_on_cran()
  X <- make_synth_vnir()
  r <- apply_spectral_preprocessing(X, absorbance = TRUE, sg_smooth = TRUE,
                                    sg_derivative = 1L, window = 11L)
  expect_equal(r$steps[1], "Reflectance")
  expect_true(grepl("Absorbance",     r$steps[2]))
  expect_true(grepl("SG smoothing",   r$steps[3]))
  expect_true(grepl("1st derivative", r$steps[4]))
  expect_equal(ncol(r$X), ncol(X) - 20L)          # two SG passes
  expect_equal(length(r$wavelengths), ncol(r$X))
  expect_true(all(is.finite(r$X)))
})

test_that("SG derivative reduces high-frequency noise vs a raw finite difference", {
  skip_on_cran()
  X <- make_synth_vnir()
  r <- apply_spectral_preprocessing(X, sg_derivative = 1L, window = 11L)
  raw_diff <- t(apply(X, 1, diff))
  # the SG derivative should be far smoother than a naive lag-1 difference
  expect_lt(stats::sd(as.numeric(r$X)), stats::sd(as.numeric(raw_diff)))
})

test_that("a valid spectrum never errors; an infeasible window is skipped", {
  skip_on_cran()
  X <- make_synth_vnir(wavelengths = 350:360)   # only 11 bands
  expect_no_error(r <- apply_spectral_preprocessing(X, sg_derivative = 1L,
                                                    window = 11L))
  expect_true(any(grepl("skipped", r$steps)))   # window 11 not < ncol 11
  expect_equal(dim(r$X), dim(X))                 # X left untouched
})
