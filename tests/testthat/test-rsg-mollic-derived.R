test_that("chernozem passes on canonical Chernozem fixture", {
  pr <- make_chernozem_canonical()
  res <- chernozem(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
})

test_that("kastanozem passes on canonical Kastanozem fixture", {
  pr <- make_kastanozem_canonical()
  res <- kastanozem(pr)
  expect_true(isTRUE(res$passed))
})

test_that("phaeozem passes on canonical Phaeozem fixture", {
  pr <- make_phaeozem_canonical()
  res <- phaeozem(pr)
  expect_true(isTRUE(res$passed))
})

test_that("mollic-derived RSGs are mutually exclusive on each fixture", {
  ch <- make_chernozem_canonical()
  ks <- make_kastanozem_canonical()
  ph <- make_phaeozem_canonical()

  expect_false(isTRUE(kastanozem(ch)$passed))
  expect_false(isTRUE(phaeozem(ch)$passed))

  expect_false(isTRUE(chernozem(ks)$passed))
  expect_false(isTRUE(phaeozem(ks)$passed))

  expect_false(isTRUE(chernozem(ph)$passed))
  expect_false(isTRUE(kastanozem(ph)$passed))
})

test_that("mollic-derived RSGs all FAIL on non-mollic fixtures", {
  ferralsol <- make_ferralsol_canonical()
  for (fn in list(chernozem, kastanozem, phaeozem)) {
    expect_false(isTRUE(fn(ferralsol)$passed))
  }
})

test_that("mollic-derived RSGs short-circuit when mollic fails", {
  pr <- make_ferralsol_canonical()
  res <- chernozem(pr)
  expect_match(res$notes, "lacks a mollic horizon")
})

test_that("test_carbonates_present treats 0% as 'no carbonates'", {
  h <- data.table::data.table(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
    caco3_pct = c(0, 0, 0)
  )
  res <- test_carbonates_present(h)
  expect_false(isTRUE(res$passed))
  expect_equal(length(res$layers), 0L)
})

test_that("test_carbonates_present passes when any layer has caco3 > 0", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    caco3_pct = c(0, 8)
  )
  res <- test_carbonates_present(h)
  expect_true(isTRUE(res$passed))
  expect_equal(res$layers, 2L)
})

test_that("test_chernic_color tests upper-20-cm chroma", {
  h <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 60),
    munsell_chroma_moist = c(2, 3)
  )
  # Layer 1 (top=0 < 20): chroma 2 <= 2, passes
  # Layer 2 (top=25 >= 20): not in range
  res <- test_chernic_color(h)
  expect_equal(res$layers, 1L)
})

test_that("Phaeozem fails the chernozem path because no carbonates", {
  pr <- make_phaeozem_canonical()
  res <- chernozem(pr)
  expect_false(isTRUE(res$passed))
  # Confirm the failure is via the carbonates test
  expect_false(isTRUE(res$evidence$carbonates$passed))
})
