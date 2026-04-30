# Tests for download_ossl_subset() / clear_ossl_cache().
# Avoids real network calls; we install a fake .rds at the cache path
# and exercise the read / validation / interpolation paths.


make_fake_ossl_artefact <- function(n = 30L, wavelengths = 350:2500) {
  set.seed(1L)
  Xr <- matrix(0.25 + 0.001 * (seq_along(wavelengths)) +
                 rnorm(n * length(wavelengths), sd = 0.01),
               nrow = n, ncol = length(wavelengths))
  colnames(Xr) <- as.character(wavelengths)
  Yr <- data.frame(
    clay_pct  = runif(n,  5, 60),
    sand_pct  = runif(n, 10, 80),
    silt_pct  = runif(n,  5, 40),
    cec_cmol  = runif(n,  2, 30),
    bs_pct    = runif(n, 10, 90),
    ph_h2o    = runif(n,  4, 8),
    oc_pct    = runif(n,  0.2, 4),
    fe_dcb_pct = runif(n,  0.5, 8),
    caco3_pct = runif(n,  0,  5)
  )
  list(Xr = Xr, Yr = Yr)
}


test_that("download_ossl_subset() reads from cache when present", {
  cache_dir <- file.path(tempdir(), "ossl-cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  cache_file <- file.path(cache_dir, "ossl_global.rds")
  saveRDS(make_fake_ossl_artefact(), cache_file)

  res <- download_ossl_subset(region = "global",
                                cache_dir = cache_dir,
                                verbose = FALSE)
  expect_type(res, "list")
  expect_named(res, c("Xr", "Yr", "metadata"), ignore.order = TRUE)
  expect_true(is.matrix(res$Xr))
  expect_equal(nrow(res$Xr), 30L)
  expect_equal(ncol(res$Xr), 2151L)
  expect_s3_class(res$Yr, "data.frame")
  expect_equal(res$metadata$region,     "global")
  expect_equal(res$metadata$n_profiles, 30L)
})


test_that("download_ossl_subset() restricts Yr to requested properties", {
  cache_dir <- file.path(tempdir(), "ossl-cache-props")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  saveRDS(make_fake_ossl_artefact(),
          file.path(cache_dir, "ossl_south_america.rds"))

  res <- download_ossl_subset(region     = "south_america",
                                properties = c("clay_pct", "ph_h2o"),
                                cache_dir  = cache_dir,
                                verbose    = FALSE)
  expect_equal(names(res$Yr), c("clay_pct", "ph_h2o"))
})


test_that("download_ossl_subset() errors when none of the properties match", {
  cache_dir <- file.path(tempdir(), "ossl-cache-noprops")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  saveRDS(make_fake_ossl_artefact(),
          file.path(cache_dir, "ossl_global.rds"))

  expect_error(
    download_ossl_subset(region     = "global",
                           properties = c("not_a_property"),
                           cache_dir  = cache_dir,
                           verbose    = FALSE),
    regexp = "None of the requested properties"
  )
})


test_that("download_ossl_subset() interpolates Xr to requested wavelengths", {
  cache_dir <- file.path(tempdir(), "ossl-cache-wl")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  saveRDS(make_fake_ossl_artefact(wavelengths = seq(400, 2400, by = 10)),
          file.path(cache_dir, "ossl_global.rds"))

  res <- download_ossl_subset(region      = "global",
                                wavelengths = seq(500, 2200, by = 50),
                                cache_dir   = cache_dir,
                                verbose     = FALSE)
  expect_equal(ncol(res$Xr), length(seq(500, 2200, by = 50)))
  expect_equal(colnames(res$Xr),
                 as.character(seq(500, 2200, by = 50)))
})


test_that("download_ossl_subset() errors helpfully when network fetch fails", {
  cache_dir <- file.path(tempdir(), "ossl-cache-nonet")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  expect_error(
    download_ossl_subset(
      region    = "global",
      endpoint  = "http://localhost:1/does/not/exist/%s.rds",
      cache_dir = cache_dir,
      verbose   = FALSE
    ),
    regexp = "Failed to download OSSL subset"
  )
})


test_that("clear_ossl_cache() removes the right files", {
  cache_dir <- file.path(tempdir(), "ossl-cache-clear")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)

  for (r in c("global", "south_america", "europe")) {
    saveRDS(list(Xr = matrix(1, 1, 1), Yr = data.frame(a = 1)),
            file.path(cache_dir, sprintf("ossl_%s.rds", r)))
  }
  expect_length(list.files(cache_dir, pattern = "^ossl_.*\\.rds$"), 3L)

  removed <- clear_ossl_cache(region = "europe", cache_dir = cache_dir,
                                verbose = FALSE)
  expect_length(removed, 1L)
  expect_length(list.files(cache_dir, pattern = "^ossl_.*\\.rds$"), 2L)

  removed_all <- clear_ossl_cache(cache_dir = cache_dir, verbose = FALSE)
  expect_length(removed_all, 2L)
  expect_length(list.files(cache_dir, pattern = "^ossl_.*\\.rds$"), 0L)
})


test_that("clear_ossl_cache() handles a non-existent dir gracefully", {
  res <- clear_ossl_cache(cache_dir = file.path(tempdir(), "no-such-dir"),
                            verbose = FALSE)
  expect_equal(res, character(0))
})
