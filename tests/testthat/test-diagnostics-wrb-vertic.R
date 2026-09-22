test_that("vertic_properties passes on canonical Vertisol fixture", {
  pr <- make_vertisol_canonical()
  res <- vertic_properties(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(2L %in% res$layers)   # Bss (25-80 cm)
})

test_that("vertic_properties fails on non-vertic fixtures", {
  expect_false(isTRUE(vertic_properties(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(vertic_properties(make_luvisol_canonical())$passed))
  expect_false(isTRUE(vertic_properties(make_chernozem_canonical())$passed))
  expect_false(isTRUE(vertic_properties(make_cambisol_canonical())$passed))
})

test_that("vertic_properties NA when slickensides missing", {
  pr <- make_vertisol_canonical()
  pr$horizons$slickensides <- NA_character_
  res <- vertic_properties(pr)
  expect_true(is.na(res$passed))
})

test_that("vertic_properties respects clay threshold", {
  pr <- make_vertisol_canonical()
  expect_false(isTRUE(vertic_properties(pr, min_clay = 80)$passed))
})

test_that("test_slickensides_present excludes 'absent' / 'few'", {
  h <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    slickensides = c("absent", "few", "common")
  )
  res <- test_slickensides_present(h)
  expect_equal(res$layers, 3L)
})

test_that("test_clay_above thresholds correctly", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    clay_pct = c(25, 35)
  )
  res <- test_clay_above(h, min_pct = 30)
  expect_equal(res$layers, 2L)
})
