# =============================================================================
# Tests for v0.9.111 -- AfSP offline benchmark harness fix.
#
# Two distinct bugs kept the AfSP/wrb2022 row out of the consolidated suite:
#   1. load_afsp_sample() returns the WRAPPER list(pedons, pulled_on, source,
#      filter) -- length 4 -- and the harness fed that straight into
#      benchmark_afsp() instead of its $pedons slot.
#   2. benchmark_afsp() then iterated the 4 wrapper elements, hitting $site on
#      the atomic ones (pulled_on, source) -> "$ operator is invalid for atomic
#      vectors", caught by .suite_run_afsp()'s tryCatch -> silent NULL -> AfSP
#      never appeared in run_all_benchmarks().
# The fix: .afsp_as_pedons() unwraps the wrapper, benchmark_afsp() accepts
# either form and skips non-PedonRecord elements, and .suite_run_afsp() unwraps
# before its length-guard / max_n slice.
# =============================================================================


.afsp_sample_present <- function() {
  file.exists(file.path("inst", "extdata", "afsp_sample.rds")) ||
    nzchar(system.file("extdata", "afsp_sample.rds", package = "soilKey"))
}


# ---- unwrap helper ---------------------------------------------------------

test_that("v0.9.111: .afsp_as_pedons() unwraps the wrapper but passes a bare list", {
  bare <- list("a", "b", "c")
  expect_identical(soilKey:::.afsp_as_pedons(bare), bare)

  wrapped <- list(pedons = bare, pulled_on = "2026-05-09", source = "x",
                  filter = list())
  expect_identical(soilKey:::.afsp_as_pedons(wrapped), bare)

  # NULL (failed load) flows straight through for the downstream length-guard.
  expect_null(soilKey:::.afsp_as_pedons(NULL))
})


# ---- bug #2 regression: benchmark_afsp() on the raw wrapper -----------------

test_that("v0.9.111: benchmark_afsp() accepts the load_afsp_sample() wrapper directly", {
  testthat::skip_if_not(.afsp_sample_present(), "Bundled AfSP sample not present")
  peds <- load_afsp_sample()
  expect_length(peds, 4L)              # it IS the 4-element wrapper

  # Previously: "$ operator is invalid for atomic vectors". Now it unwraps.
  res <- suppressWarnings(benchmark_afsp(peds, verbose = FALSE))
  expect_false(is.null(res))
  expect_true(is.table(res$confusion))
  expect_gt(sum(res$confusion), 0L)    # non-empty confusion matrix
  expect_equal(res$n_total, 120L)

  # Wrapper and its $pedons slot yield the identical benchmark.
  res2 <- suppressWarnings(benchmark_afsp(peds$pedons, verbose = FALSE))
  expect_identical(res$confusion, res2$confusion)
  expect_equal(res$accuracy, res2$accuracy)
})


test_that("v0.9.111: benchmark_afsp() skips non-PedonRecord members without erroring", {
  testthat::skip_if_not(.afsp_sample_present(), "Bundled AfSP sample not present")
  peds <- load_afsp_sample()$pedons
  # Inject atomic junk among the pedons; the refs/preds loops must not crash.
  poisoned <- c(peds[1:5], list("not-a-pedon", 42L, NA))
  res <- suppressWarnings(benchmark_afsp(poisoned, verbose = FALSE))
  expect_false(is.null(res))
  expect_equal(res$n_total, 8L)        # 5 real + 3 junk
  expect_lte(res$n_compared, 5L)       # only real pedons are in scope
})


# ---- bug #1 regression: the suite surfaces a real AfSP row ------------------

test_that("v0.9.111: .suite_run_afsp() returns a full-metric AfSP/wrb2022 row", {
  testthat::skip_if_not(.afsp_sample_present(), "Bundled AfSP sample not present")
  row <- suppressWarnings(soilKey:::.suite_run_afsp(300L, verbose = FALSE))
  expect_false(is.null(row))
  expect_equal(nrow(row), 1L)
  expect_identical(row$dataset, "afsp_sample")
  expect_identical(row$system, "wrb2022")
  expect_equal(row$n_compared, 120L)
  # v0.9.110 metric columns are all present (confusion -> full metric set).
  expect_true(all(c("accuracy", "acc_lo", "acc_hi", "balanced_acc",
                    "macro_f1", "kappa", "nir") %in% names(row)))
  expect_false(is.na(row$balanced_acc))
  expect_false(is.na(row$kappa))
})


test_that("v0.9.111: run_all_benchmarks() surfaces AfSP as an offline WRB row", {
  testthat::skip_if_not(.afsp_sample_present(), "Bundled AfSP sample not present")
  # "canonical" runs no network datasets but still attaches the offline AfSP row.
  out <- suppressWarnings(run_all_benchmarks(datasets = "canonical",
                                             verbose = FALSE))
  hit <- out$summary$dataset == "afsp_sample" & out$summary$system == "wrb2022"
  expect_true(any(hit))
  expect_equal(out$summary$n_compared[hit], 120L)
  expect_gt(out$summary$accuracy[hit], 0.15)
})
