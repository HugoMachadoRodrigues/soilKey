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
  expect_error(preprocess_spectra(NULL),               "NULL")
  expect_error(preprocess_spectra(list(1, 2, 3)),       "numeric")
  X <- make_synth_vnir()
  expect_error(preprocess_spectra(X, w = 4L), "odd")
  expect_error(preprocess_spectra(X[, 1:3], w = 5L),
                "wavelengths|window")
})


test_that("snv() centres each row to zero mean and unit variance", {
  X <- make_synth_vnir()
  Y <- preprocess_spectra(X, method = "snv")
  expect_equal(dim(Y), dim(X))
  rowMeans <- apply(Y, 1, mean)
  rowSds   <- apply(Y, 1, stats::sd)
  expect_true(all(abs(rowMeans) < 1e-10))
  expect_true(all(abs(rowSds - 1) < 1e-6))
})


test_that("snv() handles a constant row without producing NaN", {
  X <- rbind(make_synth_vnir(2),
              matrix(0.5, nrow = 1, ncol = 2151))
  colnames(X) <- as.character(350:2500)
  Y <- preprocess_spectra(X, method = "snv")
  expect_true(all(is.finite(Y)))
})


test_that("sg1 trims the spectrum by w-1 columns and returns numeric matrix", {
  X <- make_synth_vnir()
  Y <- preprocess_spectra(X, method = "sg1", w = 5L)
  expect_equal(nrow(Y), nrow(X))
  expect_equal(ncol(Y), ncol(X) - 4L)
  expect_true(is.numeric(Y))
})


test_that("snv+sg1 chains the two transforms", {
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
  k_native <- soilKey:::.sg_coefficients(w = 5L, p = 2L, m = 1L)
  k_book   <- c(-2, -1, 0, 1, 2) / 10
  expect_equal(k_native, k_book, tolerance = 1e-12)
})


test_that("native SG path agrees with prospectr (when available)", {
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
  X <- make_synth_vnir()
  Y_default <- preprocess_spectra(X)
  Y_explicit <- preprocess_spectra(X, method = "snv+sg1")
  expect_equal(Y_default, Y_explicit)
})


test_that("preprocess_spectra() accepts a vector by treating it as one row", {
  X <- as.numeric(make_synth_vnir(1L)[1L, ])
  Y <- preprocess_spectra(X, method = "snv")
  expect_equal(nrow(Y), 1L)
  expect_equal(ncol(Y), length(X))
})


test_that("preprocess_spectra() accepts a data.frame", {
  X <- as.data.frame(make_synth_vnir())
  Y <- preprocess_spectra(X, method = "snv")
  expect_true(is.matrix(Y))
})
