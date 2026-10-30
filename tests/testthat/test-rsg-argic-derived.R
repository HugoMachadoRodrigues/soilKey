test_that("acrisol passes on canonical Acrisol fixture", {
  pr <- make_acrisol_canonical()
  res <- acrisol(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
})

test_that("lixisol passes on canonical Lixisol fixture", {
  pr <- make_lixisol_canonical()
  res <- lixisol(pr)
  expect_true(isTRUE(res$passed))
})

test_that("alisol passes on canonical Alisol fixture", {
  pr <- make_alisol_canonical()
  res <- alisol(pr)
  expect_true(isTRUE(res$passed))
})

test_that("luvisol passes on canonical Luvisol fixture", {
  pr <- make_luvisol_canonical()
  res <- luvisol(pr)
  expect_true(isTRUE(res$passed))
})

test_that("argic-derived RSGs are told apart by the order of the key", {
  skip_on_cran()
  # v0.9.310: the WRB 2022 key tests each of them on the "other soils" (Ch 4,
  # p. 119-122), so a gate tests only its own criteria: every argic profile
  # within 100 cm passes the Luvisol gate, and the key's order separates them.
  ac <- make_acrisol_canonical()
  lx <- make_lixisol_canonical()
  al <- make_alisol_canonical()
  lv <- make_luvisol_canonical()

  expect_true(isTRUE(lixisol(ac)$passed))      # low CEC
  expect_true(isTRUE(alisol(ac)$passed))       # Al > bases
  expect_false(isTRUE(acrisol(lx)$passed))     # Al <= bases
  expect_false(isTRUE(alisol(lx)$passed))
  expect_false(isTRUE(acrisol(al)$passed))     # CEC >= 24
  expect_false(isTRUE(lixisol(al)$passed))
  for (p in list(ac, lx, al, lv)) expect_true(isTRUE(luvisol(p)$passed))
  expect_false(isTRUE(acrisol(lv)$passed))
  expect_false(isTRUE(lixisol(lv)$passed))
  expect_false(isTRUE(alisol(lv)$passed))

  rsg <- function(p) classify_wrb2022(p, on_missing = "silent")$rsg_or_order
  expect_equal(rsg(ac), "Acrisols")
  expect_equal(rsg(lx), "Lixisols")
  expect_equal(rsg(al), "Alisols")
  expect_equal(rsg(lv), "Luvisols")
})

test_that("argic-derived RSGs all FAIL on non-argic fixtures", {
  ferralsol <- make_ferralsol_canonical()
  for (fn in list(acrisol, lixisol, alisol, luvisol)) {
    expect_false(isTRUE(fn(ferralsol)$passed))
  }
})

test_that("argic-derived RSGs short-circuit when argic fails", {
  pr <- make_ferralsol_canonical()
  res <- acrisol(pr)
  expect_match(res$notes, "lacks an argic horizon")
})

test_that("compute_al_saturation handles missing inputs gracefully", {
  expect_equal(soilKey:::compute_al_saturation(2, 1, 0.2, 0.05, 4),
                4 / (2 + 1 + 0.2 + 0.05 + 4) * 100)
  expect_true(is.na(soilKey:::compute_al_saturation(NA, 1, 0.2, 0.05, 4)))
  expect_true(is.na(soilKey:::compute_al_saturation(0, 0, 0, 0, 0)))
})

test_that("test_cec_per_clay_above thresholds correctly", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    cec_cmol = c(8, 12), clay_pct = c(40, 30)
  )
  res <- test_cec_per_clay_above(h, min_cmol_per_kg_clay = 24)
  # Layer 1: 8*100/40 = 20, fails. Layer 2: 12*100/30 = 40, passes.
  expect_equal(res$layers, 2L)
})

test_that("test_bs_above and test_bs_below are complementary at threshold", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    bs_pct = c(45, 55)
  )
  expect_equal(test_bs_above(h, min_pct = 50)$layers, 2L)
  expect_equal(test_bs_below(h, max_pct = 50)$layers, 1L)
})
