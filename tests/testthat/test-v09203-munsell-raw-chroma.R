# v0.9.203: a negative Chroma reaches munsellinterpol, so it can say so.
#
# munsellinterpol 3.6-0 (G. Davis, 2026-09) warns when a Chroma is negative,
# which is distinct from the tiny positive residual that `ctol` exists for. A
# negative Chroma means the spectral integration or the chromatic adaptation
# went wrong upstream, and the warning was requested precisely so it would be
# visible. soilKey's continuous-notation path zeroed the neutral axis BEFORE
# naming, so the warning could never fire from there, while the round_chip path
# passed the raw value -- the same input, two different diagnostics depending on
# a formatting flag.

.mi36 <- function() {
  requireNamespace("munsellinterpol", quietly = TRUE) &&
    utils::packageVersion("munsellinterpol") >= "3.6.0"
}

.flat <- function(r) {
  wl <- seq(350, 2500, by = 1)
  list(sp = matrix(r, nrow = 1, ncol = length(wl), dimnames = list(NULL, wl)),
       wl = wl)
}


test_that("a negative Chroma is reported, not silently neutralised", {
  skip_on_cran()
  skip_if_not(.mi36(), "needs munsellinterpol >= 3.6.0")
  # XYZtoMunsell never returns a negative Chroma on real data (see below), so
  # force one the way an upstream fault would produce it.
  real <- munsellinterpol::XYZtoMunsell
  testthat::local_mocked_bindings(
    XYZtoMunsell = function(XYZ, ...) {
      out <- real(XYZ, ...); out[, "C"] <- -0.3; out
    },
    .package = "munsellinterpol")
  f <- .flat(0.3)
  expect_warning(
    r <- predict_munsell_from_spectra(f$sp, f$wl, round_chip = FALSE),
    "Chroma.*< 0")
  # the notation itself is unchanged: every negative is < 1e-4, so neutral
  expect_identical(r$munsell_hue_moist, "N")
  expect_identical(r$munsell_chroma_moist, 0)
  expect_match(r$munsell_string, "^N [0-9.]+/$")
})

test_that("both notation paths now raise the same diagnostic", {
  skip_on_cran()
  skip_if_not(.mi36(), "needs munsellinterpol >= 3.6.0")
  real <- munsellinterpol::XYZtoMunsell
  testthat::local_mocked_bindings(
    XYZtoMunsell = function(XYZ, ...) {
      out <- real(XYZ, ...); out[, "C"] <- -0.3; out
    },
    .package = "munsellinterpol")
  f <- .flat(0.3)
  expect_warning(predict_munsell_from_spectra(f$sp, f$wl, round_chip = TRUE),
                 "Chroma.*< 0")
  expect_warning(predict_munsell_from_spectra(f$sp, f$wl, round_chip = FALSE),
                 "Chroma.*< 0")
})

test_that("ordinary greys raise nothing, so the signal is not noise", {
  skip_on_cran()
  skip_if_not(.mi36(), "needs munsellinterpol >= 3.6.0")
  # Measured before this change: across reflectance 0.01..1.0 the raw residual
  # is exactly 0, or +7e-15 on the darkest grey -- never negative. Passing the
  # raw value through must therefore stay silent on every one of them.
  for (r in c(0.01, 0.02, 0.1, 0.3, 0.6, 0.95, 1)) {
    f <- .flat(r)
    for (rc in c(TRUE, FALSE))
      expect_no_warning(predict_munsell_from_spectra(f$sp, f$wl, round_chip = rc))
  }
})

test_that("the neutral axis still reports an exact 0 chroma", {
  # v0.9.185's guarantee must survive the reorder: the hue says "N", so a
  # non-zero chroma beside it would be internally inconsistent.
  skip_if_not_installed("munsellinterpol")
  f <- .flat(0.01)
  r <- predict_munsell_from_spectra(f$sp, f$wl, round_chip = FALSE)
  expect_identical(r$munsell_hue_moist, "N")
  expect_identical(r$munsell_chroma_moist, 0)
})
