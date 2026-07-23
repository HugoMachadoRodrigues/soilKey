# =============================================================================
# v0.9.190 -- artefacts are a WEIGHTED AVERAGE over the upper 100 cm.
#
# WRB 2022 keys Technosols on ">= 20 % (by volume, by weighted average)
# artefacts in the upper 100 cm". Up to v0.9.189 soilKey tested "any single
# layer >= 20 %", a different and more permissive criterion. These tests pin
# the corrected semantics, including the interval reasoning used when part of
# the window is unmeasured.
# =============================================================================

.art_h <- function(top, bottom, artefacts) {
  data.frame(top_cm = top, bottom_cm = bottom, artefacts_pct = artefacts)
}
.art <- function(...) {
  f <- utils::getFromNamespace("test_artefacts_concentration", "soilKey")
  f(.art_h(...))
}

test_that("v0.9.190: a thin rubble lens no longer keys a clean soil to Technosol", {
  skip_on_cran()
  # 5 cm of 100 % artefacts over 95 cm of clean soil: weighted average 5 %.
  # The pre-v0.9.190 'any layer >= 20 %' rule wrongly passed this.
  r <- .art(c(0, 5), c(5, 100), c(100, 0))
  expect_false(isTRUE(r$passed))
  expect_equal(r$details$summary$weighted_mean_pct, 5)
})

test_that("v0.9.190: the weighted average decides, not any single layer", {
  skip_on_cran()
  expect_true(isTRUE(.art(c(0, 50), c(50, 100), c(25, 25))$passed))   # 25 %
  expect_false(isTRUE(.art(c(0, 50), c(50, 100), c(15, 15))$passed))  # 15 %
  # 30 % over 0-80 with a clean 80-100 averages 24 % -> still a Technosol.
  r <- .art(c(0, 80), c(80, 100), c(30, 0))
  expect_true(isTRUE(r$passed))
  expect_equal(r$details$summary$weighted_mean_pct, 24)
})

test_that("v0.9.190: the window stops at 100 cm", {
  skip_on_cran()
  # 40 % between 100 and 200 cm lies outside the window and must not count.
  r <- .art(c(0, 100), c(100, 200), c(0, 40))
  expect_false(isTRUE(r$passed))
  expect_equal(r$details$summary$weighted_mean_pct, 0)
})

test_that("v0.9.190: unmeasured depth is bounded, not guessed", {
  skip_on_cran()
  # Known 50 % over 0-60 already forces the average to >= 30 % whatever the
  # unmeasured 60-100 holds -> decide TRUE without asking for more data.
  expect_true(isTRUE(.art(c(0, 60), c(60, 100), c(50, NA))$passed))
  # Known 5 % over 0-60: the average could be 3 % or 43 % -> genuinely undecided.
  r <- .art(c(0, 60), c(60, 100), c(5, NA))
  expect_true(is.na(r$passed))
  expect_true("artefacts_pct" %in% r$missing)
  # Nothing measured at all -> undecided.
  expect_true(is.na(.art(c(0, 50), c(50, 100), c(NA, NA))$passed))
})

test_that("v0.9.190: a decided result reports no missing data", {
  skip_on_cran()
  # Once the bounds settle the question, the criterion must stop asking --
  # this is what kept ordinary profiles reporting 'data needed' forever.
  expect_length(.art(c(0, 50), c(50, 100), c(25, 25))$missing, 0L)
  expect_length(.art(c(0, 50), c(50, 100), c(0, 0))$missing, 0L)
})
