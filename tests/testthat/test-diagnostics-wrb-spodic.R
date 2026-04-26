test_that("spodic passes on canonical Podzol fixture", {
  pr <- make_podzol_canonical()
  res <- spodic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(3L %in% res$layers)   # Bs (30-70 cm)
})

test_that("spodic fails on Ferralsol, Luvisol, Chernozem", {
  expect_false(isTRUE(spodic(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(spodic(make_luvisol_canonical())$passed))
  expect_false(isTRUE(spodic(make_chernozem_canonical())$passed))
})

test_that("spodic NA when oxalate-extractable Al/Fe missing", {
  pr <- make_podzol_canonical()
  pr$horizons$al_ox_pct <- NA_real_
  pr$horizons$fe_ox_pct <- NA_real_
  res <- spodic(pr)
  expect_true(is.na(res$passed))
})

test_that("spodic respects pH ceiling", {
  pr <- make_podzol_canonical()
  expect_false(isTRUE(spodic(pr, max_ph = 4.0)$passed))
})

test_that("spodic respects custom Al/Fe threshold", {
  pr <- make_podzol_canonical()
  expect_false(isTRUE(spodic(pr, min_alfe = 5)$passed))
})

test_that("spodic Al + 0.5*Fe formula computes correctly", {
  h <- data.table::data.table(
    top_cm = 0, bottom_cm = 30,
    al_ox_pct = 0.4, fe_ox_pct = 0.4
  )
  res <- test_spodic_aluminum_iron(h)
  expect_true(isTRUE(res$passed))
  expect_equal(res$details[[1]]$al_plus_half_fe, 0.6)
})
