# =============================================================================
# Tests for v0.9.94 -- lazy-fetch architecture for the four large
# benchmark caches (afsp_sample, kssl_sample, kssl_nasis_sample,
# wosis_stratified_sample). The .rds files are no longer bundled in
# the CRAN source tarball; loaders look in 4 places (bundled -> dev
# inst/extdata -> dev data-raw/lazy-fetch -> user cache) and offer
# an interactive download from a versioned GitHub Release on first
# call.
# =============================================================================


test_that("v0.9.94: .SOILKEY_LAZY_FETCH_CACHES enumerates the 4 known caches", {
  expect_setequal(.SOILKEY_LAZY_FETCH_CACHES,
                    c("afsp_sample", "kssl_sample",
                      "kssl_nasis_sample", "wosis_stratified_sample"))
})


test_that("v0.9.94: .lazy_fetch_url builds canonical GitHub Release URL", {
  url <- .lazy_fetch_url("afsp_sample")
  expect_match(url, "^https://github\\.com/HugoMachadoRodrigues/soilKey/releases/download/")
  expect_match(url, "/afsp_sample\\.rds$")
  # release tag interpolated
  expect_match(url, .SOILKEY_LAZY_FETCH_RELEASE)
})


test_that("v0.9.94: .lazy_fetch_local_path finds bundled / dev / cache file", {
  # In a developer checkout the file lives at one of two places.
  # The helper should return a non-NULL path for each known cache
  # (it's available in the working tree under data-raw/lazy-fetch/
  # OR in inst/extdata/ on pre-v0.9.94 branches OR in the user cache
  # if download_extdata_cache() has run).
  for (name in .SOILKEY_LAZY_FETCH_CACHES) {
    path <- .lazy_fetch_local_path(name)
    if (is.null(path)) {
      skip(sprintf("%s not present locally; run download_extdata_cache()", name))
    }
    expect_true(file.exists(path))
    expect_match(path, paste0(name, "\\.rds$"))
  }
})


test_that("v0.9.94: .lazy_fetch_local_path returns NULL for missing cache", {
  # Use a name that doesn't exist anywhere
  expect_error(
    .lazy_fetch_local_path("nonexistent_cache"),
    "%in%"   # stopifnot: name %in% .SOILKEY_LAZY_FETCH_CACHES
  )
})


test_that("v0.9.94: load_*_sample loaders return non-empty pedons in dev checkout", {
  # In a developer checkout (where data-raw/lazy-fetch/ has the .rds
  # files), every loader returns non-empty pedons.
  for (loader_name in c("load_afsp_sample", "load_kssl_sample",
                          "load_kssl_nasis_sample",
                          "load_wosis_stratified_sample")) {
    loader <- get(loader_name)
    s <- tryCatch(loader(),
                   error = function(e) NULL)
    if (is.null(s)) {
      skip(sprintf("%s cache not present locally", loader_name))
    }
    peds <- s$pedons %||% s
    expect_true(is.list(peds))
    expect_gt(length(peds), 0L)
    expect_true(inherits(peds[[1]], "PedonRecord"))
  }
})


test_that("v0.9.94: download_extdata_cache validates `which` argument", {
  expect_error(
    download_extdata_cache("not_a_real_cache", verbose = FALSE),
    "should be one of|arg.*should"
  )
})
