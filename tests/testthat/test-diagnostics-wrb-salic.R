test_that("salic passes on canonical Solonchak fixture", {
  pr <- make_solonchak_canonical()
  res <- salic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(1L %in% res$layers)   # Az (0-25 cm)
})

test_that("salic fails on Ferralsol, Calcisol, Gypsisol", {
  expect_false(isTRUE(salic(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(salic(make_calcisol_canonical())$passed))
  expect_false(isTRUE(salic(make_gypsisol_canonical())$passed))
})

test_that("salic NA when ec_dS_m missing everywhere", {
  pr <- make_solonchak_canonical()
  pr$horizons$ec_dS_m <- NA_real_
  res <- salic(pr)
  expect_true(is.na(res$passed))
  expect_true("ec_dS_m" %in% res$missing)
})

test_that("salic respects custom thresholds", {
  pr <- make_solonchak_canonical()
  expect_false(isTRUE(salic(pr, min_ec_dS_m = 50)$passed))
  expect_false(isTRUE(salic(pr, min_thickness = 100)$passed))
})

test_that("salic evidence carries the named sub-tests", {
  pr <- make_solonchak_canonical()
  res <- salic(pr)
  expect_named(res$evidence, c("ec", "thickness"))
})

test_that("test_ec_concentration handles thresholds correctly", {
  h <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 60),
    ec_dS_m = c(25, 12)
  )
  res <- test_ec_concentration(h, min_dS_m = 15)
  expect_equal(res$layers, 1L)
})

test_that("Solonchak fixture has expected ECEC structure (Na-dominated subsoil)", {
  pr <- make_solonchak_canonical()
  expect_true(pr$horizons$na_cmol[2] > pr$horizons$ca_cmol[2] / 2)
})
