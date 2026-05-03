# Tests for v0.9.30 bundled WoSIS South-America sample (offline cache).

test_that("load_wosis_sample returns a 40-profile snapshot", {
  s <- load_wosis_sample()
  expect_named(s, c("profiles_raw", "pedons", "pulled_on",
                      "endpoint", "filter", "n_pulled"))
  expect_equal(s$n_pulled, 40L)
  expect_length(s$pedons, 40L)
  expect_length(s$profiles_raw, 40L)
})

test_that("WoSIS sample profiles are valid PedonRecord objects", {
  s <- load_wosis_sample()
  for (i in 1:5) {
    expect_s3_class(s$pedons[[i]], "PedonRecord")
    expect_true(!is.null(s$pedons[[i]]$site$id))
    expect_true(!is.null(s$pedons[[i]]$horizons))
  }
})

test_that("WoSIS sample classifies offline (regression-safe path)", {
  s <- load_wosis_sample()
  # Try classify_wrb2022 on the first pedon -- doesn't have to succeed
  # (some profiles may be too sparse) but should NOT raise.
  res <- tryCatch(
    classify_wrb2022(s$pedons[[1]], on_missing = "silent"),
    error = function(e) e
  )
  expect_false(inherits(res, "error"),
                 info = "classify_wrb2022 should not raise on bundled WoSIS sample")
  expect_s3_class(res, "ClassificationResult")
})

test_that("WoSIS sample metadata identifies the snapshot", {
  s <- load_wosis_sample()
  expect_s3_class(s$pulled_on, "Date")
  expect_equal(as.character(s$pulled_on), "2026-05-03")
  expect_match(s$endpoint, "graphql\\.isric\\.org")
  expect_equal(s$filter$continent, "South America")
})
