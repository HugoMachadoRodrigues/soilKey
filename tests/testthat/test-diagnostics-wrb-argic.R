test_that("argic passes on canonical Luvisol fixture", {
  pr <- make_luvisol_canonical()
  res <- argic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(3L %in% res$layers)   # Bt1 (25-50 cm)
})

test_that("argic fails on canonical Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- argic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("argic fails on canonical Chernozem fixture", {
  pr <- make_chernozem_canonical()
  res <- argic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("argic returns NA when clay data missing", {
  pr <- make_luvisol_canonical()
  pr$horizons$clay_pct <- NA_real_
  res <- argic(pr)
  expect_true(is.na(res$passed))
  expect_true(length(res$missing) > 0L)
  expect_true("clay_pct" %in% res$missing)
})

test_that("clay-increase rule tiers correctly by overlying clay (<15%)", {
  # WRB 2022 Ch 3.1.3 criterion 2.a.iv: +6 percentage points absolute
  h <- data.table::data.table(clay_pct = c(10, 17))   # +7 absolute, threshold +6
  res <- test_clay_increase_argic(h)
  expect_true(2L %in% res$layers)
  expect_true(isTRUE(res$passed))

  h2 <- data.table::data.table(clay_pct = c(10, 14))  # +4 < 6, fails
  res2 <- test_clay_increase_argic(h2)
  expect_false(2L %in% res2$layers)
})

test_that("clay-increase rule tiers correctly (15 to <50% band)", {
  # WRB 2022 Ch 3.1.3 criterion 2.a.v: ratio >= 1.4
  h_fail <- data.table::data.table(clay_pct = c(20, 26))   # ratio 1.30 < 1.4
  expect_false(2L %in% test_clay_increase_argic(h_fail)$layers)

  h_pass <- data.table::data.table(clay_pct = c(20, 30))   # ratio 1.50 >= 1.4
  expect_true(2L %in% test_clay_increase_argic(h_pass)$layers)
})

test_that("clay-increase rule tiers correctly (>=50% band)", {
  # WRB 2022 Ch 3.1.3 criterion 2.a.vi: +20 percentage points absolute
  h_fail <- data.table::data.table(clay_pct = c(55, 65))   # +10 < 20
  expect_false(2L %in% test_clay_increase_argic(h_fail)$layers)

  h_pass <- data.table::data.table(clay_pct = c(55, 75))   # +20 >= 20
  expect_true(2L %in% test_clay_increase_argic(h_pass)$layers)
})

test_that("argic excluded when glossic features detected", {
  pr <- make_luvisol_canonical()
  pr$horizons$designation[3] <- "Btg glossic"
  res <- argic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("test_minimum_thickness honours candidate_layers", {
  h <- data.table::data.table(
    top_cm    = c(0, 10, 30),
    bottom_cm = c(10, 30, 60)
  )
  res <- test_minimum_thickness(h, min_cm = 15,
                                 candidate_layers = c(2L, 3L))
  expect_equal(res$layers, c(2L, 3L))   # both >= 15 cm

  res_strict <- test_minimum_thickness(h, min_cm = 25,
                                         candidate_layers = c(2L, 3L))
  expect_equal(res_strict$layers, 3L)   # only the 30-cm layer
})

test_that("argic returns DiagnosticResult with named sub-tests", {
  pr <- make_luvisol_canonical()
  res <- argic(pr)
  expect_named(res$evidence, c("clay_increase", "thickness", "texture",
                                 "not_albeluvic"))
})

test_that("argic reference includes WRB 2022 chapter citation", {
  pr <- make_luvisol_canonical()
  res <- argic(pr)
  expect_match(res$reference, "WRB \\(2022\\).*Argic")
})
