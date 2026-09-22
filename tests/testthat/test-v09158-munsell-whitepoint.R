# =============================================================================
# v0.9.158 -- follow-up to the v0.9.156 Munsell illuminant fix, on G. Davis'
# suggestion: derive the D65 white point from the *same* bundled CIE table that
# predict_xyz_from_spectra() integrates against (CMFs weighted by the table's
# D65 column), so a constant-reflectance spectrum maps to an exact neutral
# (Chroma 0) instead of leaving a ~0.0007 residual against the textbook white.
# Also exercises the now-vectorised (matrix) conversion path.
# =============================================================================

wl_vis <- seq(380, 780, by = 5)

test_that("a perfectly constant 18% reflectance gives an exact neutral", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  out <- predict_munsell_from_spectra(rep(0.18, length(wl_vis)), wl_vis,
                                      round_chip = FALSE)
  # Self-consistent white -> Chroma collapses to (essentially) machine zero,
  # not the ~0.0007 residual the textbook white left.
  expect_lt(out$munsell_chroma_moist, 1e-4)
  # And it rounds to a neutral chip.
  rounded <- predict_munsell_from_spectra(rep(0.18, length(wl_vis)), wl_vis)
  expect_equal(rounded$munsell_hue_moist, "N")
  expect_equal(rounded$munsell_chroma_moist, 0)
})

test_that("constant reflectance is neutral at other levels too", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  for (level in c(0.05, 0.35, 0.70)) {
    out <- predict_munsell_from_spectra(rep(level, length(wl_vis)), wl_vis,
                                        round_chip = FALSE)
    expect_lt(out$munsell_chroma_moist, 1e-4)
  }
})

test_that("matrix (batch) input matches row-by-row conversion", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  set.seed(99)
  S <- matrix(pmin(pmax(stats::runif(6 * length(wl_vis), 0.04, 0.6), 0), 1),
              nrow = 6)
  batch <- predict_munsell_from_spectra(S, wl_vis)
  rows  <- do.call(rbind, lapply(seq_len(nrow(S)), function(i)
    predict_munsell_from_spectra(S[i, ], wl_vis)))
  expect_equal(batch$munsell_string,       rows$munsell_string)
  expect_equal(batch$munsell_value_moist,  rows$munsell_value_moist)
  expect_equal(batch$munsell_chroma_moist, rows$munsell_chroma_moist)
})

test_that("a missing/black row yields NA without breaking the batch", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  S <- rbind(rep(0.3, length(wl_vis)),   # ok
             rep(0,   length(wl_vis)),   # black -> Y = 0 -> NA
             rep(0.5, length(wl_vis)))   # ok
  out <- predict_munsell_from_spectra(S, wl_vis)
  expect_equal(nrow(out), 3L)
  expect_true(is.na(out$munsell_chroma_moist[2]))
  expect_false(is.na(out$munsell_chroma_moist[1]))
  expect_false(is.na(out$munsell_chroma_moist[3]))
})
