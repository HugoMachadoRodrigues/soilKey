test_that("mollic passes on canonical Chernozem fixture", {
  pr <- make_chernozem_canonical()
  res <- mollic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(1L %in% res$layers)   # Ah1
})

test_that("mollic fails on canonical Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- mollic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("mollic fails on canonical Luvisol fixture", {
  pr <- make_luvisol_canonical()
  res <- mollic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("mollic returns NA when surface attributes missing", {
  pr <- make_chernozem_canonical()
  pr$horizons$munsell_value_moist[1]  <- NA_real_
  pr$horizons$munsell_chroma_moist[1] <- NA_real_
  pr$horizons$bs_pct[1]               <- NA_real_
  pr$horizons$oc_pct[1]               <- NA_real_
  res <- mollic(pr)
  expect_true(is.na(res$passed))
})

test_that("mollic color test caps chroma at 3 (moist)", {
  h <- data.table::data.table(
    top_cm = 0, bottom_cm = 25,
    munsell_value_moist  = 2,
    munsell_chroma_moist = 4,                  # > 3
    munsell_value_dry    = 3
  )
  res <- test_mollic_color(h)
  expect_false(isTRUE(res$passed))
})

test_that("mollic color test substitutes value+1 for missing dry value", {
  h <- data.table::data.table(
    top_cm = 0, bottom_cm = 25,
    munsell_value_moist  = 2,
    munsell_chroma_moist = 1,
    munsell_value_dry    = NA_real_            # use vm + 1 = 3 <= 5
  )
  res <- test_mollic_color(h)
  expect_true(isTRUE(res$passed))
})

test_that("mollic OC test passes at exact threshold", {
  h <- data.table::data.table(top_cm = 0, bottom_cm = 25, oc_pct = 0.6)
  res <- test_mollic_organic_carbon(h, min_pct = 0.6)
  expect_true(isTRUE(res$passed))
})

test_that("mollic BS test fails just below threshold", {
  h <- data.table::data.table(top_cm = 0, bottom_cm = 25, bs_pct = 49)
  res <- test_mollic_base_saturation(h, min_pct = 50)
  expect_false(isTRUE(res$passed))
})

test_that("mollic surface_top_cm parameter narrows candidate layers", {
  pr <- make_chernozem_canonical()
  res_default <- mollic(pr)
  expect_true(isTRUE(res_default$passed))

  # No layer has top_cm <= -1, so candidates is empty -> mollic FAILS.
  res_negative <- mollic(pr, surface_top_cm = -1)
  expect_false(isTRUE(res_negative$passed))
})

test_that("mollic evidence carries all five sub-tests", {
  pr <- make_chernozem_canonical()
  res <- mollic(pr)
  expect_named(res$evidence, c("color", "organic_carbon",
                                 "base_saturation", "thickness", "structure"))
})

test_that("mollic respects custom OC threshold", {
  pr <- make_chernozem_canonical()
  res <- mollic(pr, min_oc = 100)   # impossibly high
  expect_false(isTRUE(res$passed))
})

test_that("mollic respects custom BS threshold", {
  pr <- make_chernozem_canonical()
  res <- mollic(pr, min_bs = 100)   # impossibly high
  expect_false(isTRUE(res$passed))
})
