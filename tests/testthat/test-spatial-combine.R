# ================================================================
# Tests for combine_priors() and the underlying normalize_prior().
# ================================================================


test_that("combine_priors with one prior returns it unchanged", {
  p1 <- data.table::data.table(
    rsg_code    = c("FR", "AC", "RG"),
    probability = c(0.7,  0.2,  0.1)
  )
  out <- combine_priors(list(p1))
  expect_equal(sum(out$probability), 1, tolerance = 1e-9)
  expect_equal(setdiff(p1$rsg_code, out$rsg_code), character())
  # FR should still dominate
  expect_equal(out$rsg_code[1], "FR")
})


test_that("combine_priors with equal weights pools two priors", {
  p1 <- data.table::data.table(
    rsg_code    = c("FR", "AC"),
    probability = c(0.8,  0.2)
  )
  p2 <- data.table::data.table(
    rsg_code    = c("FR", "AC"),
    probability = c(0.4,  0.6)
  )
  out <- combine_priors(list(p1, p2))
  expect_equal(sum(out$probability), 1, tolerance = 1e-9)
  expect_setequal(out$rsg_code, c("FR", "AC"))
  # Geometric mean of (0.8, 0.4) = sqrt(0.32) ~= 0.566
  # Geometric mean of (0.2, 0.6) = sqrt(0.12) ~= 0.346
  # Normalised:                FR = 0.566 / 0.912 ~= 0.620
  fr_p <- out$probability[out$rsg_code == "FR"]
  ac_p <- out$probability[out$rsg_code == "AC"]
  expect_gt(fr_p, ac_p)
  expect_equal(fr_p + ac_p, 1, tolerance = 1e-9)
})


test_that("combine_priors honours weights", {
  p1 <- data.table::data.table(
    rsg_code    = c("FR", "AC"),
    probability = c(0.9, 0.1)
  )
  p2 <- data.table::data.table(
    rsg_code    = c("FR", "AC"),
    probability = c(0.1, 0.9)
  )
  # Equal weights -> roughly even.
  out_eq <- combine_priors(list(p1, p2))
  fr_eq <- out_eq$probability[out_eq$rsg_code == "FR"]
  expect_equal(fr_eq, 0.5, tolerance = 1e-9)

  # Weight p1 heavily -> FR dominates.
  out_w  <- combine_priors(list(p1, p2), weights = c(10, 1))
  fr_w   <- out_w$probability[out_w$rsg_code == "FR"]
  expect_gt(fr_w, 0.7)
})


test_that("combine_priors handles disjoint supports via epsilon", {
  p1 <- data.table::data.table(
    rsg_code    = c("FR", "AC"),
    probability = c(0.7,  0.3)
  )
  p2 <- data.table::data.table(
    rsg_code    = c("LV", "RG"),
    probability = c(0.6,  0.4)
  )
  out <- combine_priors(list(p1, p2), epsilon = 1e-3)
  expect_setequal(out$rsg_code, c("FR", "AC", "LV", "RG"))
  expect_equal(sum(out$probability), 1, tolerance = 1e-9)
  # Each output probability is strictly positive.
  expect_true(all(out$probability > 0))
})


test_that("combine_priors drops empty priors and returns an empty table when all are empty", {
  empty <- data.table::data.table(
    rsg_code    = character(),
    probability = numeric()
  )
  out <- combine_priors(list(empty, empty))
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("rsg_code", "probability"))
})


test_that("normalize_prior renormalises and drops NAs", {
  p <- data.table::data.table(
    rsg_code    = c("FR", "AC", NA),
    probability = c(2,    1,    5)
  )
  out <- soilKey:::normalize_prior(p)
  expect_equal(sum(out$probability), 1, tolerance = 1e-9)
  expect_false(any(is.na(out$rsg_code)))
})


test_that("posterior_classify keeps the deterministic verdict but reports prior", {
  # Build a synthetic ClassificationResult by faking a trace where FR
  # passed and several others failed.
  trace <- list(
    HS = list(code = "HS", name = "Histosols",  passed = FALSE, missing = character()),
    AT = list(code = "AT", name = "Anthrosols", passed = FALSE, missing = character()),
    FR = list(code = "FR", name = "Ferralsols", passed = TRUE,  missing = character()),
    RG = list(code = "RG", name = "Regosols",   passed = FALSE, missing = character())
  )
  result <- ClassificationResult$new(
    system         = "WRB 2022",
    name           = "Ferralsols",
    rsg_or_order   = "Ferralsols",
    trace          = trace
  )
  prior <- data.table::data.table(
    rsg_code    = c("FR", "AC", "LV"),
    probability = c(0.6, 0.3, 0.1)
  )
  post <- posterior_classify(result, prior)
  expect_s3_class(post, "data.table")
  expect_named(post, c("rsg_code", "prior", "likelihood", "posterior"))
  expect_equal(sum(post$posterior), 1, tolerance = 1e-9)
  # FR has both highest prior AND likelihood = 1, so it should top.
  expect_equal(post$rsg_code[1], "FR")
})
