test_that("gypsic passes on canonical Gypsisol fixture", {
  pr <- make_gypsisol_canonical()
  res <- gypsic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(3L %in% res$layers)   # By1 (50-100 cm)
})

test_that("gypsic fails on Ferralsol, Calcisol, Solonchak", {
  expect_false(isTRUE(gypsic(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(gypsic(make_calcisol_canonical())$passed))
  expect_false(isTRUE(gypsic(make_solonchak_canonical())$passed))
})

test_that("gypsic NA when caso4_pct missing everywhere", {
  pr <- make_gypsisol_canonical()
  pr$horizons$caso4_pct <- NA_real_
  res <- gypsic(pr)
  expect_true(is.na(res$passed))
  expect_true("caso4_pct" %in% res$missing)
})

test_that("gypsic respects custom thresholds", {
  pr <- make_gypsisol_canonical()
  expect_false(isTRUE(gypsic(pr, min_gypsum_pct = 50)$passed))
  expect_false(isTRUE(gypsic(pr, min_thickness = 200)$passed))
})

test_that("gypsic evidence carries the named sub-tests", {
  pr <- make_gypsisol_canonical()
  res <- gypsic(pr)
  expect_named(res$evidence, c("gypsum", "thickness"))
})

test_that("test_caso4_concentration counts only layers above threshold", {
  h <- data.table::data.table(
    top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
    caso4_pct = c(0.5, 8, 35)
  )
  res <- test_caso4_concentration(h, min_pct = 5)
  expect_equal(res$layers, c(2L, 3L))
})
