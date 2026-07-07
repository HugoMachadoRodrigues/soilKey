# =============================================================================
# v0.9.183 -- G. Davis (munsellinterpol author) asked for the canonical
# colorimetry sanity check: "100% constant reflectance should produce Munsell
# Value = 10" (a perfect reflecting diffuser is, by definition, the top of the
# Munsell value scale and a pure neutral). This is the natural companion to the
# v0.9.156/158 illuminant + white-point fixes, and it pins down the boundary the
# lower-reflectance neutral tests only approach.
#
# It also documents the v0.9.183 switch to the canonical conversion path
# munsellinterpol::XYZtoMunsell(XYZ, white=) (>= 3.4-0), which adapts D65 -> C
# internally -- the exact call Glenn documents -- with the older
# XYZ -> Lab -> LabToMunsell() route kept only as a fallback.
# =============================================================================

wl_vis <- seq(380, 780, by = 5)

test_that("a perfect reflecting diffuser (100% reflectance) is Munsell value 10", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  out <- predict_munsell_from_spectra(rep(1, length(wl_vis)), wl_vis,
                                      round_chip = FALSE)
  # Top of the value scale, exactly.
  expect_equal(out$munsell_value_moist, 10, tolerance = 1e-3)
  # ...and a pure neutral (no hue/chroma for a flat spectrum).
  expect_lt(out$munsell_chroma_moist, 1e-4)
})

test_that("the perfect diffuser rounds to the neutral chip N", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  rounded <- predict_munsell_from_spectra(rep(1, length(wl_vis)), wl_vis)
  expect_equal(rounded$munsell_hue_moist, "N")
  expect_equal(rounded$munsell_chroma_moist, 0)
  # A neutral of value ~10 is, in the soil book, "N 9.5/" (the highest chip);
  # the important invariant is the neutral hue + zero chroma above.
  expect_gt(rounded$munsell_value_moist, 9)
})

test_that("value climbs monotonically with reflectance up to 10", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  levels <- c(0.05, 0.2, 0.5, 0.9, 1.0)
  vals <- vapply(levels, function(L)
    predict_munsell_from_spectra(rep(L, length(wl_vis)), wl_vis,
                                 round_chip = FALSE)$munsell_value_moist,
    numeric(1))
  expect_true(all(diff(vals) > 0))          # strictly increasing
  expect_equal(vals[length(vals)], 10, tolerance = 1e-3)  # tops out at 10
})
