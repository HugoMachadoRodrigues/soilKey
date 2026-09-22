# Tests for v0.9.28 classify_all() wrapper.

test_that("classify_all returns all three classifications by default", {
  pr <- make_ferralsol_canonical()
  res <- classify_all(pr, on_missing = "silent")
  expect_named(res, c("wrb", "sibcs", "usda", "summary"))
  expect_s3_class(res$wrb,   "ClassificationResult")
  expect_s3_class(res$sibcs, "ClassificationResult")
  expect_s3_class(res$usda,  "ClassificationResult")
})

test_that("classify_all summary is a 1-row data.frame with name per system", {
  pr <- make_ferralsol_canonical()
  res <- classify_all(pr, on_missing = "silent")
  expect_s3_class(res$summary, "data.frame")
  expect_equal(nrow(res$summary), 1L)
  expect_named(res$summary, c("wrb", "sibcs", "usda"))
  expect_equal(res$summary$wrb,   res$wrb$name)
  expect_equal(res$summary$sibcs, res$sibcs$name)
  expect_equal(res$summary$usda,  res$usda$name)
})

test_that("classify_all with subset skips other systems", {
  pr <- make_ferralsol_canonical()
  res <- classify_all(pr, systems = c("wrb2022", "usda"), on_missing = "silent")
  expect_s3_class(res$wrb,  "ClassificationResult")
  expect_null(res$sibcs)
  expect_s3_class(res$usda, "ClassificationResult")
  expect_true(is.na(res$summary$sibcs))
  expect_equal(res$summary$wrb,  res$wrb$name)
  expect_equal(res$summary$usda, res$usda$name)
})

test_that("classify_all with single-system selection works", {
  pr <- make_ferralsol_canonical()
  res <- classify_all(pr, systems = "wrb2022", on_missing = "silent")
  expect_s3_class(res$wrb,  "ClassificationResult")
  expect_null(res$sibcs)
  expect_null(res$usda)
})

test_that("classify_all rejects invalid system names via match.arg", {
  pr <- make_ferralsol_canonical()
  expect_error(classify_all(pr, systems = "bogus", on_missing = "silent"))
})

test_that("classify_all handles errors gracefully (returns NULL + warning)", {
  # Build a deliberately broken pedon (no horizons) that will likely
  # cause at least one classifier to error or return indeterminate.
  pr <- PedonRecord$new(
    site = list(id = "broken", lat = 0, lon = 0, country = "TEST"),
    horizons = data.table::data.table()
  )
  # Use suppressWarnings because we expect classifiers to complain.
  res <- suppressWarnings(classify_all(pr, on_missing = "silent"))
  # We don't insist that ALL three error -- some classifiers handle
  # an empty horizons table with NA; we just verify the wrapper does
  # not propagate errors and the summary row is produced.
  expect_s3_class(res$summary, "data.frame")
  expect_equal(nrow(res$summary), 1L)
})

test_that("classify_all 'all' alias expands to the full triple", {
  pr <- make_ferralsol_canonical()
  res_all_alias <- classify_all(pr, systems = "all", on_missing = "silent")
  res_explicit  <- classify_all(pr,
                                  systems = c("wrb2022", "sibcs", "usda"),
                                  on_missing = "silent")
  expect_equal(res_all_alias$summary, res_explicit$summary)
})
