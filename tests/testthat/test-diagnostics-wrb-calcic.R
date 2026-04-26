test_that("calcic passes on canonical Calcisol fixture", {
  pr <- make_calcisol_canonical()
  res <- calcic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(2L %in% res$layers)   # Bk1 (20-60 cm)
})

test_that("calcic fails on canonical Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- calcic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("calcic fails on canonical Solonchak (no CaCO3 reported)", {
  pr <- make_solonchak_canonical()
  res <- calcic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("calcic fails on canonical Gypsisol (CaCO3 below threshold)", {
  pr <- make_gypsisol_canonical()
  res <- calcic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("calcic NA when CaCO3 missing in all layers", {
  pr <- make_calcisol_canonical()
  pr$horizons$caco3_pct <- NA_real_
  res <- calcic(pr)
  expect_true(is.na(res$passed))
  expect_true("caco3_pct" %in% res$missing)
})

test_that("calcic respects custom CaCO3 threshold", {
  pr <- make_calcisol_canonical()
  res_strict <- calcic(pr, min_caco3_pct = 50)   # only Bk1 (35) and Bk2 (40) and C (30) -- all < 50
  expect_false(isTRUE(res_strict$passed))
})

test_that("calcic respects custom thickness threshold", {
  pr <- make_calcisol_canonical()
  res <- calcic(pr, min_thickness = 200)         # impossibly thick
  expect_false(isTRUE(res$passed))
})

test_that("test_caco3_concentration handles per-layer thresholds", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    caco3_pct = c(5, 25)
  )
  res <- test_caco3_concentration(h, min_pct = 15)
  expect_equal(res$layers, 2L)
  expect_true(isTRUE(res$passed))
})

test_that("calcic evidence carries the named sub-tests", {
  pr <- make_calcisol_canonical()
  res <- calcic(pr)
  expect_named(res$evidence, c("caco3", "thickness"))
})
