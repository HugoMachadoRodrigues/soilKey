test_that("plinthic passes on canonical Plinthosol fixture", {
  pr <- make_plinthosol_canonical()
  res <- plinthic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(2L %in% res$layers)   # Btv (20-80 cm)
})

test_that("plinthic fails on Ferralsol, Luvisol, Chernozem", {
  expect_false(isTRUE(plinthic(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(plinthic(make_luvisol_canonical())$passed))
  expect_false(isTRUE(plinthic(make_chernozem_canonical())$passed))
})

test_that("plinthic NA when plinthite_pct missing everywhere", {
  pr <- make_plinthosol_canonical()
  pr$horizons$plinthite_pct <- NA_real_
  res <- plinthic(pr)
  expect_true(is.na(res$passed))
  expect_true("plinthite_pct" %in% res$missing)
})

test_that("plinthic respects custom thresholds", {
  pr <- make_plinthosol_canonical()
  expect_false(isTRUE(plinthic(pr, min_plinthite_pct = 50)$passed))
  expect_false(isTRUE(plinthic(pr, min_thickness = 200)$passed))
})

test_that("test_plinthite_concentration counts only layers above threshold", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    plinthite_pct = c(5, 25)
  )
  res <- test_plinthite_concentration(h, min_pct = 15)
  expect_equal(res$layers, 2L)
})
