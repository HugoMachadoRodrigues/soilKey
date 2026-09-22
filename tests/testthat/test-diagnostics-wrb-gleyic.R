test_that("gleyic_properties passes on canonical Gleysol fixture", {
  pr <- make_gleysol_canonical()
  res <- gleyic_properties(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(2L %in% res$layers)   # Bg1 (15-45 cm) within top 50
})

test_that("gleyic_properties fails on Ferralsol, Luvisol, Cambisol, Vertisol", {
  expect_false(isTRUE(gleyic_properties(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(gleyic_properties(make_luvisol_canonical())$passed))
  expect_false(isTRUE(gleyic_properties(make_cambisol_canonical())$passed))
  expect_false(isTRUE(gleyic_properties(make_vertisol_canonical())$passed))
})

test_that("gleyic_properties does NOT trigger on Podzol's albic-like E", {
  # The Podzol fixture sets E with chroma 3 specifically to keep gleyic
  # negative under the conservative v0.2 redox-features-only test.
  pr <- make_podzol_canonical()
  expect_false(isTRUE(gleyic_properties(pr)$passed))
})

test_that("gleyic_properties NA when redox features missing in top 50", {
  pr <- make_gleysol_canonical()
  pr$horizons$redoximorphic_features_pct <- NA_real_
  res <- gleyic_properties(pr)
  expect_true(is.na(res$passed))
})

test_that("gleyic_properties respects max_top_cm", {
  pr <- make_gleysol_canonical()
  res <- gleyic_properties(pr, max_top_cm = 5)   # only A is in range; redox=2
  expect_false(isTRUE(res$passed))
})
