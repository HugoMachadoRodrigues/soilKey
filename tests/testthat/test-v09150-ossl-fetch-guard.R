# v0.9.150: download_ossl_subset() must fail gracefully (actionable message)
# when the OSSL endpoint is unreachable / has moved — not crash with a cryptic
# readRDS error. Fully offline (a bogus file:// endpoint that cannot resolve).

test_that("download_ossl_subset gives an actionable error on an unreachable endpoint", {
  skip_on_cran()
  skip_if_not_installed("utils")
  cache <- withr::local_tempdir()
  withr::local_options(
    soilKey.ossl_endpoint = "file:///soilKey_nonexistent_dir/ossl_%s.rds")
  err <- tryCatch(
    download_ossl_subset(region = "south_america", cache_dir = cache,
                         force = TRUE, verbose = FALSE),
    error = function(e) conditionMessage(e))
  expect_true(is.character(err))
  # the message points the user at the recovery paths, not a raw readRDS failure
  expect_match(err, "read_spectral_library|ossl_demo_sa|ossl_endpoint")
})

test_that("download_ossl_subset rejects a non-.rds payload with the same guidance", {
  skip_on_cran()
  skip_if_not_installed("utils")
  cache <- withr::local_tempdir()
  # an endpoint that resolves (region-templated) but serves a non-RDS body
  dir  <- withr::local_tempdir()
  writeLines("<html><body>404 Not Found</body></html>",
             file.path(dir, "ossl_south_america.rds"))
  withr::local_options(
    soilKey.ossl_endpoint = paste0("file://", dir, "/ossl_%s.rds"))
  err <- tryCatch(
    download_ossl_subset(region = "south_america", cache_dir = cache,
                         force = TRUE, verbose = FALSE),
    error = function(e) conditionMessage(e))
  expect_true(is.character(err))
  expect_match(err, "read_spectral_library|ossl_demo_sa|valid .rds")
})
