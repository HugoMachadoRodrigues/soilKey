# Tests for the v0.9.106 reproducible benchmark suite: run_all_benchmarks()
# and the Redape wiring into benchmark_unified(). The canonical path is fully
# offline and deterministic; external-dataset rows are skipped when absent.

test_that("run_all_benchmarks canonical mode returns a 100% sanity row + report", {
  tf <- tempfile(fileext = ".md")
  res <- run_all_benchmarks(datasets = "canonical", report_path = tf,
                            verbose = FALSE)
  expect_true(is.data.frame(res$summary))
  expect_true(all(c("dataset", "system", "n_compared", "accuracy") %in%
                    names(res$summary)))
  can <- res$summary[res$summary$dataset == "canonical", ]
  expect_equal(nrow(can), 1L)
  expect_equal(can$accuracy, 1)            # every fixture classifies (coverage)
  expect_gt(can$n_compared, 60L)           # ~44 fixtures x 3 systems
  # report file written with the accuracy table
  expect_true(file.exists(tf))
  md <- readLines(tf)
  expect_true(any(grepl("benchmark suite", md)))
  expect_true(any(grepl("Accuracy by dataset", md)))
})


test_that("auto-detection skips absent datasets without error", {
  empty <- file.path(tempdir(), "no-such-benchmark-data")
  paths <- list(bdsolos = empty, febr = file.path(empty, "x.txt"),
                kssl_gpkg = empty, kssl_nasis = empty,
                lucas_csv = empty, esdb_root = empty, redape = empty)
  res <- expect_no_error(suppressWarnings(
    run_all_benchmarks(datasets = "auto", paths = paths, verbose = FALSE)))
  # Every external (network/path-gated) dataset is skipped, leaving only the
  # offline rows: the canonical sanity row always, plus the bundled AfSP sample
  # when its .rds is present (v0.9.111 -- previously dropped by the harness bug).
  external <- c("bdsolos", "febr", "kssl", "lucas_esdb", "redape")
  expect_false(any(res$summary$dataset %in% external))
  expect_true("canonical" %in% res$summary$dataset)
  if (nzchar(system.file("extdata", "afsp_sample.rds", package = "soilKey")) ||
      file.exists(file.path("inst", "extdata", "afsp_sample.rds")))
    expect_true("afsp_sample" %in% res$summary$dataset)
})


test_that(".benchmark_reproducible_sample is deterministic and seed-safe", {
  smp <- soilKey:::.benchmark_reproducible_sample
  a <- smp(1000L, 50L); b <- smp(1000L, 50L)
  expect_identical(a, b)            # reproducible across calls
  expect_length(a, 50L)
  expect_true(all(a >= 1L & a <= 1000L))
  expect_false(is.unsorted(a))      # returned sorted
  # does not clobber the caller's RNG stream
  set.seed(7); x1 <- runif(1)
  set.seed(7); invisible(smp(1000L, 50L)); x2 <- runif(1)
  expect_equal(x1, x2)
})


test_that(".benchmark_available_datasets reports presence correctly", {
  avail <- soilKey:::.benchmark_available_datasets
  none <- avail(list(bdsolos = "/nope", febr = "/nope/x", kssl_gpkg = "/nope",
                     kssl_nasis = "/nope", lucas_csv = "/nope",
                     esdb_root = "/nope", redape = "/nope"))
  expect_length(none, 0L)
})


test_that("Redape is wired into benchmark_unified (SiBCS)", {
  redape_dir <- file.path(
    getOption("soilKey.benchmark_root",
              "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data"),
    "redape_geotab")
  skip_if_not(dir.exists(redape_dir), "Redape data not present")
  res <- benchmark_unified(systems = "sibcs", datasets = "redape",
                           max_n_per_dataset = 15L, verbose = FALSE)
  s <- res$per_system$sibcs
  expect_true(is.finite(s$accuracy))
  expect_gt(s$n_compared, 0L)
  expect_lte(s$accuracy, 1)
})
