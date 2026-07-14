# =============================================================================
# v0.9.185 -- the neutral axis reports an EXACT zero chroma.
#
# Follow-up to the G. Davis exchange. Two invariants were already in place:
#   * the self-consistent D65 white (v0.9.158) puts a perfect reflecting
#     diffuser at Munsell Value 10 / Chroma 0;
#   * the C < 1e-4 guard (v0.9.184) collapses the undefined hue to "N".
#
# But the *reported* chroma was still the raw residual: a dark flat grey keys
# out at C ~ 7e-15 on this BLAS, so a row whose hue already said "N" carried a
# non-zero chroma beside it. That is internally inconsistent, and it is the
# wedge a platform with a larger residual would use to leak a chromatic hue
# back in. v0.9.185 clamps it: on the neutral axis, chroma is exactly 0.
#
# The invariant must not depend on how much floating-point noise the local BLAS
# leaves behind -- hence expect_identical(., 0) rather than a tolerance.
# =============================================================================

wl_vis <- seq(380, 780, by = 5)

test_that("a flat spectrum reports chroma EXACTLY zero, not a residual", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  # Span the reflectance range, including the dark end where the residual bit.
  for (level in c(0.010, 0.05, 0.18, 0.5, 0.9, 1.0)) {
    out <- predict_munsell_from_spectra(rep(level, length(wl_vis)), wl_vis,
                                        round_chip = FALSE)
    expect_identical(out$munsell_chroma_moist, 0,
                     info = paste("reflectance =", level))
    expect_equal(out$munsell_hue_moist, "N")
  }
})

test_that("the clamp does not perturb the neutral string or the value", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  out <- predict_munsell_from_spectra(rep(1, length(wl_vis)), wl_vis,
                                      round_chip = FALSE)
  # Still the perfect diffuser: value 10, neutral notation "N <value>/".
  expect_equal(out$munsell_value_moist, 10, tolerance = 1e-3)
  expect_match(out$munsell_string, "^N ")
  expect_false(grepl("RP|GY|R |Y |G |B ", out$munsell_string))
})

test_that("a genuinely chromatic spectrum is untouched by the clamp", {
  skip_on_cran()
  skip_if_not_installed("munsellinterpol")
  # A sloped (reddish) spectrum must keep a real hue and a non-zero chroma --
  # the clamp fires only on the neutral axis.
  refl <- seq(0.10, 0.60, length.out = length(wl_vis))   # rises to the red end
  out  <- predict_munsell_from_spectra(refl, wl_vis, round_chip = FALSE)
  expect_gt(out$munsell_chroma_moist, 1e-4)
  expect_false(identical(out$munsell_hue_moist, "N"))
})
