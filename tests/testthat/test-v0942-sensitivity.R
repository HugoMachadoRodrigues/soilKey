# Tests for v0.9.42 sensitivity / fragility analysis.

test_that("classification_robustness returns the expected list shape", {
  p <- make_ferralsol_canonical()
  res <- classification_robustness(p, system = "wrb2022", n = 10L,
                                       seed = 42)
  expect_named(res, c("baseline", "n", "robustness", "flipped_to", "results"))
  expect_equal(res$n, 10L)
  expect_length(res$results, 10L)
  expect_true(res$robustness >= 0 && res$robustness <= 1)
})

test_that("a stable canonical fixture has near-100% robustness on small perturbation", {
  # Ferralsol canonical fixture is by design a strong example -- 5%
  # perturbation should rarely flip its classification.
  p <- make_ferralsol_canonical()
  res <- classification_robustness(p, system = "wrb2022", n = 30L,
                                       seed = 42)
  expect_gte(res$robustness, 0.7)
  expect_match(res$baseline, "Ferralsol")
})

test_that("classification_robustness respects the seed (deterministic)", {
  p <- make_luvisol_canonical()
  res1 <- classification_robustness(p, system = "wrb2022", n = 20L, seed = 7L)
  res2 <- classification_robustness(p, system = "wrb2022", n = 20L, seed = 7L)
  expect_equal(res1$results, res2$results)
  expect_equal(res1$robustness, res2$robustness)
})

test_that("classification_robustness works with system='sibcs' and 'usda'", {
  p <- make_ferralsol_canonical()
  for (sys in c("sibcs", "usda")) {
    res <- classification_robustness(p, system = sys, n = 5L, seed = 1L)
    expect_true(is.character(res$baseline))
    expect_equal(res$n, 5L)
  }
})

test_that("classification_robustness level='name' is more sensitive than 'order'", {
  # Full-name level catches qualifier flips that order-level doesn't see.
  p <- make_luvisol_canonical()
  res_order <- classification_robustness(p, system = "wrb2022",
                                             level = "order", n = 30L, seed = 1L)
  res_name  <- classification_robustness(p, system = "wrb2022",
                                             level = "name", n = 30L, seed = 1L)
  # name-level robustness <= order-level robustness in expectation,
  # because more granular comparison catches more flips.
  expect_lte(res_name$robustness, res_order$robustness + 0.1)
})

test_that("custom perturbations override the default panel", {
  p <- make_ferralsol_canonical()
  # No-op perturbation -- result must always equal baseline.
  noop_perts <- list(
    clay_pct = function(x) x,
    sand_pct = function(x) x,
    silt_pct = function(x) x
  )
  res <- classification_robustness(p, system = "wrb2022",
                                       n = 10L, seed = 1L,
                                       perturbations = noop_perts)
  expect_equal(res$robustness, 1.0)
})


# ---- batch_robustness ------------------------------------------------------

test_that("batch_robustness returns one row per pedon", {
  pedons <- list(make_ferralsol_canonical(),
                   make_luvisol_canonical(),
                   make_chernozem_canonical())
  res <- batch_robustness(pedons, system = "wrb2022", n = 5L, seed = 1L)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3L)
  expect_named(res, c("id", "baseline", "robustness", "n_flipped"))
  expect_true(all(res$robustness >= 0 & res$robustness <= 1))
})
