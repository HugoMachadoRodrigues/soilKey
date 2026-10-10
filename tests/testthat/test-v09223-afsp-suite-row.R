# v0.9.223: the AfSP row of run_all_benchmarks().
#
# load_afsp_sample() returns a list that holds the profiles in `$pedons`,
# beside the date, source and filter of the snapshot. The suite cut that whole
# list to `max_n` and handed it to benchmark_afsp(), which stopped with
# "$ operator is invalid for atomic vectors". The error was dropped, so
# run_all_benchmarks() had no AfSP row and said nothing (seen in 0.9.187 and
# 0.9.222). The existing AfSP tests only run with the sample in inst/extdata,
# and the suite test asked for "only the canonical row", which the missing row
# satisfied.

# A sample in miniature: three canonical profiles with their WRB reference, in
# the list load_afsp_sample() returns.
.afsp_like_sample <- function() {
  ref <- c(make_ferralsol_canonical = "Ferralsol",
           make_luvisol_canonical   = "Luvisol",
           make_vertisol_canonical  = "Vertisol")
  pedons <- lapply(names(ref), function(f) {
    p <- get(f, envir = asNamespace("soilKey"))()
    p$site$reference_wrb <- ref[[f]]
    p
  })
  list(pedons = pedons, pulled_on = as.Date("2026-05-09"),
       source = "test", filter = "none")
}

test_that("benchmark_afsp() takes the sample list or its profiles", {
  skip_on_cran()
  s <- .afsp_like_sample()
  from_list   <- benchmark_afsp(s, verbose = FALSE)
  from_pedons <- benchmark_afsp(s$pedons, verbose = FALSE)
  expect_identical(from_list, from_pedons)
  expect_equal(from_list$n_compared, 3L)
  expect_equal(from_list$accuracy, 1)
  # anything else is refused by name, not by an error from inside the loop
  expect_error(benchmark_afsp(list(1, 2), verbose = FALSE), "PedonRecord")
  expect_error(benchmark_afsp(list(pedons = list()), verbose = FALSE))
})

test_that("the suite scores the sample's profiles and applies max_n to them", {
  skip_on_cran()
  local_mocked_bindings(load_afsp_sample = function() .afsp_like_sample())
  row <- .suite_run_afsp(max_n = 1000L, verbose = FALSE)
  expect_s3_class(row, "data.frame")
  expect_equal(row$dataset, "afsp_sample")
  expect_equal(row$system, "wrb2022")
  expect_equal(row$n_compared, 3L)
  expect_equal(row$accuracy, 1)
  # the list has four elements (pedons, pulled_on, source, filter): a cap of
  # 2 is 2 profiles, not 2 elements of the list
  expect_equal(.suite_run_afsp(max_n = 2L, verbose = FALSE)$n_compared, 2L)
  # and the row reaches the summary
  s <- run_all_benchmarks(datasets = "canonical", verbose = FALSE)$summary
  expect_equal(s$n_compared[s$dataset == "afsp_sample"], 3L)
})

test_that("a sample that cannot be scored is reported, an absent one is skipped", {
  skip_on_cran()
  # present but unusable: no row, and a message that says why
  local_mocked_bindings(load_afsp_sample = function() list(pedons = list(1, 2)))
  expect_message(row <- .suite_run_afsp(max_n = 10L, verbose = FALSE),
                 "could not be scored")
  expect_null(row)
  # absent: no row; a note only when verbose
  local_mocked_bindings(load_afsp_sample = function() stop("not present"))
  expect_no_message(row <- .suite_run_afsp(max_n = 10L, verbose = FALSE))
  expect_null(row)
  expect_message(.suite_run_afsp(max_n = 10L, verbose = TRUE), "Skipping AfSP")
})

test_that("with the real sample, the suite has its 120-profile AfSP row", {
  skip_on_cran()
  skip_if(is.null(.lazy_fetch_local_path("afsp_sample")),
          "AfSP sample not cached on this machine")
  row <- .suite_run_afsp(max_n = 1000L, verbose = FALSE)
  expect_equal(row$n_compared, 120L)
  expect_gt(row$accuracy, 0.15)
  expect_false(is.na(row$kappa))
})
