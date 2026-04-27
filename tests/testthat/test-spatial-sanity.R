# ================================================================
# Tests for prior_consistency_check() and the integration with
# classify_wrb2022(prior = ...).
# ================================================================


test_that("prior_consistency_check returns 'consistent' above threshold", {
  prior <- data.table::data.table(
    rsg_code    = c("FR", "AC", "RG"),
    probability = c(0.85, 0.10, 0.05)
  )
  res <- prior_consistency_check("FR", prior, threshold = 0.01)
  expect_true(res$consistent)
  expect_equal(res$status, "consistent")
  expect_equal(res$p, 0.85, tolerance = 1e-9)
  expect_equal(res$threshold, 0.01)
})


test_that("prior_consistency_check fires warning when p < threshold", {
  prior <- data.table::data.table(
    rsg_code    = c("FR", "AC", "CR"),
    probability = c(0.95, 0.04, 0.005)   # CR (Cryosol) ~0.5% in tropics
  )
  res <- prior_consistency_check("CR", prior, threshold = 0.01)
  expect_false(res$consistent)
  expect_equal(res$status, "inconsistent")
  expect_lt(res$p, 0.01)
  expect_match(res$note, "biogeographically unusual")
})


test_that("prior_consistency_check returns 'inconsistent' when RSG is absent", {
  prior <- data.table::data.table(
    rsg_code    = c("FR", "AC"),
    probability = c(0.7, 0.3)
  )
  res <- prior_consistency_check("CR", prior, threshold = 0.01)
  expect_false(res$consistent)
  expect_equal(res$status, "inconsistent")
  expect_equal(res$p, 0)
})


test_that("prior_consistency_check returns 'no_data' on empty prior", {
  empty <- data.table::data.table(
    rsg_code    = character(),
    probability = numeric()
  )
  res <- prior_consistency_check("FR", empty)
  expect_true(is.na(res$consistent))
  expect_equal(res$status, "no_data")
})


test_that("classify_wrb2022 wires the prior_check on the result", {
  pr <- make_ferralsol_canonical()
  prior <- data.table::data.table(
    rsg_code    = c("FR", "AC", "RG"),
    probability = c(0.8, 0.15, 0.05)
  )
  res <- classify_wrb2022(pr, prior = prior)
  expect_equal(res$rsg_or_order, "Ferralsols")
  expect_false(is.null(res$prior_check))
  expect_equal(res$prior_check$status, "consistent")
  expect_true(res$prior_check$consistent)
  # No prior-related warning when consistent.
  expect_false(any(grepl("biogeographically unusual", res$warnings)),
                info = "no inconsistency warning expected")
})


test_that("classify_wrb2022 emits warning when prior is inconsistent", {
  # Force a case where the deterministic key returns Ferralsols but
  # the prior says this is overwhelmingly Cryosol territory.
  pr <- make_ferralsol_canonical()
  weird_prior <- data.table::data.table(
    rsg_code    = c("CR", "RG", "FR"),
    probability = c(0.95, 0.045, 0.005)
  )
  res <- classify_wrb2022(pr, prior = weird_prior, prior_threshold = 0.01)
  # Deterministic verdict UNCHANGED.
  expect_equal(res$rsg_or_order, "Ferralsols")
  # But the prior_check flags inconsistency and a warning is recorded.
  expect_equal(res$prior_check$status, "inconsistent")
  expect_true(any(grepl("biogeographically unusual|< threshold",
                         res$warnings)))
})


test_that("classify_wrb2022 leaves prior_check NULL when no prior given", {
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr)
  expect_null(res$prior_check)
})
