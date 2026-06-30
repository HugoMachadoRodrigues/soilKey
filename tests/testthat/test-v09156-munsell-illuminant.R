# =============================================================================
# v0.9.156 -- Munsell-from-spectra correctness fixes reported by Glenn Davis
# (author of munsellinterpol / spacesXYZ):
#
#  (1) Illuminant mismatch. xyYtoMunsell() expects xyY under Illuminant C,
#      but our colorimetry is D65. Without a D65 -> C chromatic adaptation,
#      a perfectly neutral (constant-reflectance) spectrum returns
#      Chroma ~ 0.65 with a spurious green-yellow tint instead of ~ 0.
#      Fixed by going XYZ -> Lab (D65) -> munsellinterpol::LabToMunsell(),
#      which adapts D65 -> C internally.
#
#  (2) roundHVC() was called without its mandatory `books=` argument, so it
#      always errored and silently fell back to the UNrounded HVC -- i.e.
#      round_chip = TRUE never rounded. Fixed with books = "soil" and proper
#      extraction of the rounded chip.
# =============================================================================

wl_vis <- seq(380, 780, by = 5)

test_that("a perfectly neutral spectrum maps to ~zero Chroma (illuminant C)", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  out <- predict_munsell_from_spectra(rep(0.18, length(wl_vis)), wl_vis)
  # The D65 -> C adaptation must collapse a true neutral to a near-zero
  # chroma. Pre-fix this was ~0.65 with a GY hue.
  expect_lt(out$munsell_chroma_moist, 0.5)
  expect_equal(out$munsell_hue_moist, "N")
})

test_that("the bug is genuinely the missing adaptation (D65 xyY != our path)", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  R <- rep(0.18, length(wl_vis))
  xyz <- predict_xyz_from_spectra(R, wl_vis)
  s <- xyz$X + xyz$Y + xyz$Z
  # The OLD (buggy) path: feed D65 chromaticity straight to xyYtoMunsell.
  buggy <- munsellinterpol::xyYtoMunsell(c(xyz$X / s, xyz$Y / s, xyz$Y))
  buggy_chroma <- as.numeric(buggy$HVC[3L])
  fixed_chroma <- predict_munsell_from_spectra(R, wl_vis)$munsell_chroma_moist
  expect_gt(buggy_chroma, 0.3)     # the documented ~0.65 tint
  expect_lt(fixed_chroma, buggy_chroma / 5)
})

test_that("round_chip = TRUE actually snaps to soil-book chips", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  R <- ifelse(wl_vis >= 600, 0.45, 0.08)   # reddish, chromatic
  rounded    <- predict_munsell_from_spectra(R, wl_vis, round_chip = TRUE)
  continuous <- predict_munsell_from_spectra(R, wl_vis, round_chip = FALSE)
  # Rounded value/chroma are on the soil grid (integers / half-steps here).
  expect_equal(rounded$munsell_value_moist,
               round(rounded$munsell_value_moist * 2) / 2)
  expect_equal(rounded$munsell_chroma_moist,
               round(rounded$munsell_chroma_moist))
  # Continuous output is genuinely finer (would equal rounded if the old
  # silent-fallback bug were still present).
  expect_false(isTRUE(all.equal(
    c(rounded$munsell_value_moist, rounded$munsell_chroma_moist),
    c(continuous$munsell_value_moist, continuous$munsell_chroma_moist))))
  expect_true(is.finite(continuous$munsell_chroma_moist))
})

test_that("a chromatic red spectrum keeps a sensible R/YR hue after adaptation", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  R <- ifelse(wl_vis >= 600, 0.45, 0.08)
  out <- predict_munsell_from_spectra(R, wl_vis)
  expect_match(out$munsell_hue_moist, "(R|YR)$")
  expect_gt(out$munsell_chroma_moist, 1)
})

test_that("predict_lab_from_spectra is unchanged by the .cielab_from_xyz refactor", {
  skip_on_cran()
  # Standard CIELAB for a flat white: L ~ 100, a ~ 0, b ~ 0.
  lab <- predict_lab_from_spectra(rep(1.0, length(wl_vis)), wl_vis)
  expect_equal(lab$L, 100, tolerance = 0.5)
  expect_lt(abs(lab$a), 1.5)
  expect_lt(abs(lab$b), 1.5)
})
