# =============================================================================
# Tests for v0.9.47 -- predict_xyz / lab / munsell from Vis-NIR reflectance.
#
# CIE 1931 / D65 integration is dependency-free (CIE table embedded in
# R/sysdata.rda), so XYZ and Lab tests run unconditionally. Munsell HVC
# tests skip cleanly when 'munsellinterpol' is not installed.
# =============================================================================


# ---- Synthetic spectra helpers ------------------------------------------

.flat_white_R <- function(level = 0.9, wl = seq(380, 780, by = 5)) {
  list(R = matrix(level, nrow = 1L, ncol = length(wl)), wl = wl)
}

.spectral_red <- function(wl = seq(380, 780, by = 5)) {
  # High reflectance above ~600 nm, low below -- maps to red region.
  R <- ifelse(wl >= 600, 0.9, 0.1)
  list(R = matrix(R, nrow = 1L), wl = wl)
}

.spectral_blue <- function(wl = seq(380, 780, by = 5)) {
  R <- ifelse(wl <= 500, 0.9, 0.1)
  list(R = matrix(R, nrow = 1L), wl = wl)
}


# ---- predict_xyz_from_spectra basic shape -------------------------------

test_that("predict_xyz_from_spectra returns X/Y/Z with right shape", {
  s <- .flat_white_R()
  out <- predict_xyz_from_spectra(s$R, s$wl)
  expect_s3_class(out, "data.frame")
  expect_named(out, c("X", "Y", "Z"))
  expect_equal(nrow(out), 1L)
  expect_true(all(is.finite(unlist(out))))
})


# ---- White reflector should yield Y close to scaled white ---------------

test_that("Flat white reflector yields Y proportional to the reflectance", {
  s <- .flat_white_R(level = 1.0)
  out <- predict_xyz_from_spectra(s$R, s$wl)
  # Perfect diffuse white should have Y == 100 by definition; allow a
  # small tolerance because the integration uses a 5-nm grid.
  expect_equal(out$Y, 100, tolerance = 0.5)
  # X and Z should be in the D65 white-point ratios.
  # CIE D65 white: X = 95.047, Z = 108.883 when Y = 100.
  expect_equal(out$X, 95.047, tolerance = 1.0)
  expect_equal(out$Z, 108.883, tolerance = 1.5)
})


test_that("Half-reflectance gives half-Y", {
  hi <- predict_xyz_from_spectra(.flat_white_R(level = 1.0)$R,
                                   .flat_white_R()$wl)
  lo <- predict_xyz_from_spectra(.flat_white_R(level = 0.5)$R,
                                   .flat_white_R()$wl)
  expect_equal(lo$Y, hi$Y / 2, tolerance = 0.5)
})


# ---- Vector input is accepted -------------------------------------------

test_that("predict_xyz_from_spectra accepts a numeric vector", {
  wl <- seq(380, 780, by = 5)
  R <- rep(0.5, length(wl))
  out <- predict_xyz_from_spectra(R, wl)
  expect_equal(nrow(out), 1L)
  expect_true(out$Y > 0)
})


# ---- Reflectance scale auto-detect (% vs decimal) -----------------------

test_that("Reflectance in % (0..100) is auto-detected", {
  wl <- seq(380, 780, by = 5)
  R_pct <- rep(50, length(wl))
  R_frac <- rep(0.5, length(wl))
  out_pct  <- predict_xyz_from_spectra(R_pct,  wl)
  out_frac <- predict_xyz_from_spectra(R_frac, wl)
  expect_equal(out_pct$Y, out_frac$Y, tolerance = 0.1)
})


# ---- Wavelength count must match -----------------------------------------

test_that("Wavelength length must match ncol(spectra)", {
  expect_error(
    predict_xyz_from_spectra(matrix(0.5, 1, 10), wavelengths = seq(380, 780, 5)),
    "length\\(wavelengths\\)"
  )
})


# ---- predict_lab_from_spectra basic shape -------------------------------

test_that("predict_lab_from_spectra returns L/a/b with right shape", {
  s <- .flat_white_R()
  lab <- predict_lab_from_spectra(s$R, s$wl)
  expect_s3_class(lab, "data.frame")
  expect_named(lab, c("L", "a", "b"))
  expect_true(all(is.finite(unlist(lab))))
})


test_that("White reflector has L close to 100, a~0, b~0", {
  s <- .flat_white_R(level = 1.0)
  lab <- predict_lab_from_spectra(s$R, s$wl)
  expect_equal(lab$L, 100, tolerance = 0.5)
  expect_equal(abs(lab$a) < 1.5, TRUE)
  expect_equal(abs(lab$b) < 1.5, TRUE)
})


# ---- Spectral red has positive a* ---------------------------------------

test_that("Red spectrum yields positive a* (red-green axis)", {
  s <- .spectral_red()
  lab <- predict_lab_from_spectra(s$R, s$wl)
  expect_true(lab$a > 0)
})


test_that("Blue spectrum yields negative b* (yellow-blue axis)", {
  s <- .spectral_blue()
  lab <- predict_lab_from_spectra(s$R, s$wl)
  expect_true(lab$b < 0)
})


# ---- Munsell prediction: skip if munsellinterpol unavailable ------------

test_that("predict_munsell_from_spectra needs munsellinterpol", {
  if (requireNamespace("munsellinterpol", quietly = TRUE)) {
    skip("munsellinterpol installed -- can't exercise the missing-pkg path")
  }
  s <- .flat_white_R()
  expect_error(predict_munsell_from_spectra(s$R, s$wl), "munsellinterpol")
})


test_that("predict_munsell_from_spectra returns the soilKey-named columns", {
  skip_if_not_installed("munsellinterpol")
  s <- .flat_white_R(level = 0.5)
  out <- predict_munsell_from_spectra(s$R, s$wl)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("munsell_hue_moist", "munsell_value_moist",
                     "munsell_chroma_moist", "munsell_string",
                     "X", "Y", "Z") %in% names(out)))
  expect_equal(nrow(out), 1L)
  expect_true(is.finite(out$munsell_value_moist))
})


# ---- White reflector maps to a high Munsell value -----------------------

test_that("Bright neutral spectrum maps to high Munsell value", {
  skip_if_not_installed("munsellinterpol")
  s <- .flat_white_R(level = 0.85)
  out <- predict_munsell_from_spectra(s$R, s$wl)
  # Value scale is 0..10; a bright near-white should be >= 8
  expect_true(out$munsell_value_moist >= 8)
  # Chroma should be small (near-neutral)
  expect_true(out$munsell_chroma_moist <= 4)
})


# ---- Red spectrum maps to a hue in the R / YR family --------------------

test_that("Red-dominant spectrum predicts an R or YR hue", {
  skip_if_not_installed("munsellinterpol")
  s <- .spectral_red()
  out <- predict_munsell_from_spectra(s$R, s$wl)
  expect_match(out$munsell_hue_moist, "(R|YR)$", ignore.case = TRUE)
})


# ---- fill_munsell_from_spectra writes provenance ------------------------

test_that("fill_munsell_from_spectra writes Munsell cells with provenance", {
  skip_if_not_installed("munsellinterpol")
  wl <- seq(380, 2400, by = 10)
  pedon <- make_synthetic_pedon_with_spectra(n_horizons = 2L,
                                                wavelengths = wl)
  pedon$horizons$munsell_hue_moist    <- NA_character_
  pedon$horizons$munsell_value_moist  <- NA_real_
  pedon$horizons$munsell_chroma_moist <- NA_real_

  before <- nrow(pedon$provenance)
  out <- fill_munsell_from_spectra(pedon, overwrite = TRUE,
                                      verbose = FALSE)
  expect_s3_class(out, "PedonRecord")
  expect_true(nrow(out$provenance) > before)
  expect_true("predicted_spectra" %in% out$provenance$source)
  munsell_rows <- out$provenance[grepl("^munsell_",
                                         out$provenance$attribute), ]
  expect_true(nrow(munsell_rows) >= 1L)
})


# ---- Sanity: the embedded CIE table has the right shape -----------------

test_that("Embedded CIE 1931 / D65 table covers 380-780 nm at 5 nm steps", {
  cie <- soilKey:::.cie_d65_5nm
  expect_equal(nrow(cie), 81L)
  expect_equal(min(cie$wavelength), 380)
  expect_equal(max(cie$wavelength), 780)
  expect_named(cie, c("wavelength", "xbar", "ybar", "zbar", "D65"))
  expect_true(all(is.finite(unlist(cie))))
})
