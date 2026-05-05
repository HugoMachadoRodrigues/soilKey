# =============================================================================
# Tests for v0.9.53 -- benchmark_performance() reproducible perf measurements.
# =============================================================================

test_that("benchmark_performance returns the documented schema", {
  bench <- benchmark_performance(n = 5L, verbose = FALSE)
  expect_type(bench, "list")
  expect_named(bench, c("summary", "per_pedon", "config"))
  expect_s3_class(bench$summary, "data.frame")
  expect_s3_class(bench$per_pedon, "data.frame")
  expect_setequal(names(bench$summary),
                    c("system", "n_pedons", "total_seconds",
                      "mean_seconds", "median_seconds",
                      "pedons_per_minute"))
  expect_setequal(names(bench$per_pedon),
                    c("i", "system", "seconds", "status"))
  expect_setequal(unique(bench$summary$system),
                    c("wrb2022", "sibcs", "usda"))
})


test_that("benchmark_performance honours the systems filter", {
  bench <- benchmark_performance(n = 3L, systems = "wrb2022",
                                    verbose = FALSE)
  expect_equal(unique(bench$per_pedon$system), "wrb2022")
  expect_equal(unique(bench$summary$system),  "wrb2022")
})


test_that("benchmark_performance is deterministic at fixed seed", {
  b1 <- benchmark_performance(n = 3L, seed = 1L, verbose = FALSE)
  b2 <- benchmark_performance(n = 3L, seed = 1L, verbose = FALSE)
  expect_equal(b1$per_pedon$system, b2$per_pedon$system)
  expect_equal(b1$per_pedon$status, b2$per_pedon$status)
})


test_that("benchmark_performance config carries platform metadata", {
  bench <- benchmark_performance(n = 2L, verbose = FALSE)
  expect_true(nzchar(bench$config$soilKey_version))
  expect_true(nzchar(bench$config$R_version))
  expect_true(nzchar(bench$config$platform))
})


test_that("benchmark_performance errors on n < 1", {
  expect_error(benchmark_performance(n = 0L, verbose = FALSE),
                "n must be")
})


test_that("median classification time is sane (<= 5s/pedon on dev hardware)", {
  # This is a regression sentinel: if classify_* slows down 50x for
  # any reason on the synthetic fixture, this fails. Generous threshold
  # to keep CI green on slow runners.
  bench <- benchmark_performance(n = 3L, verbose = FALSE)
  for (sys in c("wrb2022", "sibcs", "usda")) {
    row <- bench$summary[bench$summary$system == sys, ]
    if (nrow(row) == 0L) next
    expect_lt(row$median_seconds, 5)
  }
})
