test_that("ferralic passes on canonical Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- ferralic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(4L %in% res$layers)   # Bw1 (65-130 cm)
})

test_that("ferralic fails on canonical Luvisol fixture", {
  pr <- make_luvisol_canonical()
  res <- ferralic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("ferralic fails on canonical Chernozem fixture", {
  pr <- make_chernozem_canonical()
  res <- ferralic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("ferralic returns NA when CEC missing in all horizons", {
  pr <- make_ferralsol_canonical()
  pr$horizons$cec_cmol <- NA_real_
  res <- ferralic(pr)
  expect_true(is.na(res$passed))
})

test_that("CEC-per-clay computation is correct", {
  expect_equal(soilKey:::cec_per_clay(8.0, 50), 16)
  expect_equal(soilKey:::cec_per_clay(5.0, 60), 5 * 100 / 60,
                tolerance = 1e-6)
  expect_true(is.na(soilKey:::cec_per_clay(NA_real_, 50)))
  expect_true(is.na(soilKey:::cec_per_clay(8, NA_real_)))
  expect_true(is.na(soilKey:::cec_per_clay(8, 0)))
})

test_that("ferralic still passes with NA ECEC (v0.3.1 no longer requires it)", {
  pr <- make_ferralsol_canonical()
  pr$horizons$ecec_cmol <- NA_real_   # ECEC dropped from ferralic in v0.3.1
  res <- ferralic(pr)
  expect_true(isTRUE(res$passed))
})

test_that("ferralic respects custom CEC threshold", {
  pr <- make_ferralsol_canonical()
  res <- ferralic(pr, max_cec = 5)   # tighter than default 16
  expect_false(isTRUE(res$passed))
})

test_that("ferralic respects custom thickness threshold", {
  pr <- make_ferralsol_canonical()
  res <- ferralic(pr, min_thickness = 1000)   # impossibly thick
  expect_false(isTRUE(res$passed))
})

test_that("texture predicate at the loamy sand / sandy loam boundary", {
  expect_true(soilKey:::is_sandy_loam_or_finer(70, 20, 10))   # silt+2*clay = 40
  expect_false(soilKey:::is_sandy_loam_or_finer(85, 10, 5))   # silt+2*clay = 20
  expect_true(is.na(soilKey:::is_sandy_loam_or_finer(NA, 10, 5)))
})

test_that("ferralic evidence includes the three v0.3.1 sub-tests", {
  # v0.3.1: ECEC/clay test removed (not in WRB 2022 Ch 3.1.10)
  pr <- make_ferralsol_canonical()
  res <- ferralic(pr)
  expect_named(res$evidence, c("texture", "cec_per_clay", "thickness"))
})
