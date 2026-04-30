# Tests for download_ossl_subset_with_labels() -- the OSSL + WoSIS
# label-join function. Uses a fake cache file so no network is
# required (the OSSL fetch path) and an injected `query_fn` to
# exercise the spatial-join logic without a real GraphQL call.


make_fake_ossl_with_coords <- function(n = 12L,
                                          wavelengths = 350:2500) {
  set.seed(7L)
  Xr <- matrix(0.30 + rnorm(n * length(wavelengths), sd = 0.005),
               nrow = n, ncol = length(wavelengths))
  colnames(Xr) <- as.character(wavelengths)
  Yr <- data.frame(
    clay_pct = runif(n, 5, 60),
    sand_pct = runif(n, 10, 80),
    silt_pct = runif(n, 5, 40),
    cec_cmol = runif(n, 2, 30),
    bs_pct   = runif(n, 10, 90),
    ph_h2o   = runif(n, 4, 8),
    oc_pct   = runif(n, 0.2, 4),
    fe_dcb_pct = runif(n, 0.5, 8),
    caco3_pct = runif(n, 0, 5),
    lat        = runif(n, -23, -10),
    lon        = runif(n, -55, -40)
  )
  list(Xr = Xr, Yr = Yr)
}


test_that("download_ossl_subset_with_labels() initialises label provenance columns", {
  cache_dir <- file.path(tempdir(), "ossl-with-labels-init")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  saveRDS(make_fake_ossl_with_coords(),
          file.path(cache_dir, "ossl_global.rds"))

  # Inject a query stub that always returns NULL -> all rows stay
  # 'missing' but the column scaffolding is still exercised.
  null_query <- function(...) NULL

  res <- download_ossl_subset_with_labels(
    region          = "global",
    max_distance_km = 5,
    cache_dir       = cache_dir,
    verbose         = FALSE,
    query_fn        = null_query
  )

  expect_type(res, "list")
  expect_named(res, c("Xr", "Yr", "metadata"),
                 ignore.order = TRUE)
  Yr <- res$Yr
  expect_true(all(c("wrb_rsg", "wrb_label_source",
                      "wrb_label_distance_km") %in% names(Yr)))
  expect_true(all(Yr$wrb_label_source == "missing"))
  expect_true(all(is.na(Yr$wrb_rsg)))
  expect_equal(res$metadata$labels$n_unlabeled, nrow(Yr))
})


test_that("download_ossl_subset_with_labels() inherits WoSIS labels via the injected query_fn", {
  cache_dir <- file.path(tempdir(), "ossl-with-labels-join")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE), add = TRUE)
  saveRDS(make_fake_ossl_with_coords(),
          file.path(cache_dir, "ossl_south_america.rds"))

  # Stub returns Ferralsol for every query (always within tolerance).
  ferr_query <- function(lat, lon, max_distance_km, ...)
    list(wrb_rsg = "Ferralsol", distance_km = 0.5)

  res <- download_ossl_subset_with_labels(
    region            = "south_america",
    max_distance_km   = 5,
    cache_dir         = cache_dir,
    translate_systems = TRUE,
    verbose           = FALSE,
    query_fn          = ferr_query
  )

  Yr <- res$Yr
  expect_true(all(Yr$wrb_label_source == "wosis_spatial_join"))
  expect_true(all(Yr$wrb_rsg == "Ferralsol"))
  expect_true(all(Yr$wrb_label_distance_km == 0.5))
  expect_equal(unique(Yr$sibcs_ordem), "L")
  expect_equal(unique(Yr$usda_order),  "Oxisols")
  expect_equal(res$metadata$labels$n_wosis_join_labels, nrow(Yr))
})


test_that("WRB -> SiBCS / USDA modal translation handles NA + unknown gracefully", {
  expect_true(is.na(soilKey:::.wrb_to_sibcs_modal_ordem(NA)))
  expect_true(is.na(soilKey:::.wrb_to_sibcs_modal_ordem("NotARSG")))
  expect_equal(soilKey:::.wrb_to_sibcs_modal_ordem("Ferralsols"), "L")
  expect_equal(soilKey:::.wrb_to_usda_modal_order("Vertisols"),
                 "Vertisols")
  expect_equal(soilKey:::.wrb_to_usda_modal_order("Cryosols"),
                 "Gelisols")
})
