# =============================================================================
# Tests for v0.9.73 -- WoSIS stratified RSG-balanced sample loader.
# =============================================================================


test_that("v0.9.73: load_wosis_stratified_sample() returns 130 pedons across 26 RSGs", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "wosis_stratified_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "wosis_stratified_sample.rds",
                                                package = "soilKey")),
                          "Bundled stratified WoSIS sample not present")
  s <- load_wosis_stratified_sample()
  expect_named(s, c("pedons", "meta", "pulled_on", "endpoint", "filter",
                      "n_pulled"), ignore.order = TRUE)
  expect_equal(length(s$pedons), 130L)
  rsgs <- vapply(s$pedons,
                  function(p) p$site$wosis_rsg %||% NA_character_,
                  character(1))
  expect_equal(length(unique(rsgs)), 26L)
  expect_true(all(table(rsgs) == 5))   # exactly 5 per RSG
})


test_that("v0.9.73: stratified pedons carry full PedonRecord structure", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "wosis_stratified_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "wosis_stratified_sample.rds",
                                                package = "soilKey")),
                          "Bundled stratified WoSIS sample not present")
  s <- load_wosis_stratified_sample()
  pr <- s$pedons[[1]]
  expect_s3_class(pr, "PedonRecord")
  expect_true(!is.null(pr$site$wosis_rsg))
  expect_true(nrow(pr$horizons) >= 1L)
  # Multi-horizon (full profile depth, the v0.9.71 user pain point)
  multi_horizon_count <- sum(vapply(s$pedons,
                                       function(p) nrow(p$horizons) >= 2L,
                                       logical(1)))
  expect_gt(multi_horizon_count, 50L)   # most should have 2+ horizons
})


test_that("v0.9.73: stratified sample exposes richer analytical fields than SA snapshot", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "wosis_stratified_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "wosis_stratified_sample.rds",
                                                package = "soilKey")),
                          "Bundled stratified WoSIS sample not present")
  s <- load_wosis_stratified_sample()
  has_field <- function(field) {
    any(vapply(s$pedons, function(p) {
      if (!field %in% colnames(p$horizons)) return(FALSE)
      any(!is.na(p$horizons[[field]]))
    }, logical(1)))
  }
  # The cached SA snapshot has these all at 0%; the stratified sample
  # should have them on at least some profiles.
  expect_true(has_field("cec_cmol"))
  expect_true(has_field("ecec_cmol"))
  expect_true(has_field("caco3_pct"))
})


test_that("v0.9.73: classify_wrb2022() runs on every stratified pedon without error", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "wosis_stratified_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "wosis_stratified_sample.rds",
                                                package = "soilKey")),
                          "Bundled stratified WoSIS sample not present")
  s <- load_wosis_stratified_sample()
  errors <- sum(vapply(s$pedons, function(pr) {
    res <- tryCatch(classify_wrb2022(pr, on_missing = "silent"),
                     error = function(e) NULL)
    is.null(res)
  }, logical(1)))
  expect_equal(errors, 0L)
})
