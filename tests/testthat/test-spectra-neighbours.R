# Tests for classify_by_spectral_neighbours() -- the spectral-analogy
# classifier. Uses the bundled `ossl_demo_sa` artefact (synthetic but
# property-correlated) so no network is needed.


make_demo_with_labels <- function() {
  data(ossl_demo_sa, envir = environment(), package = "soilKey")
  lib <- ossl_demo_sa
  set.seed(42L)
  n <- nrow(lib$Yr)
  # Inject WRB labels correlated with clay (more clay -> more likely
  # Ferralsol; less -> Arenosol).
  lib$Yr$wrb_rsg <- ifelse(lib$Yr$clay_pct > 50, "FR",
                      ifelse(lib$Yr$clay_pct < 20, "AR",
                              sample(c("AC", "LX", "AL", "LV"),
                                       n, replace = TRUE)))
  lib$Yr$lat <- runif(n, -23, -10)
  lib$Yr$lon <- runif(n, -55, -40)
  lib
}


test_that("classify_by_spectral_neighbours() returns the canonical shape", {
  lib <- make_demo_with_labels()
  query <- lib$Xr[1, ]
  res <- classify_by_spectral_neighbours(query, lib, k = 5L,
                                            verbose = FALSE)
  expect_type(res, "list")
  expect_named(res, c("distribution", "neighbours", "query"),
                 ignore.order = TRUE)
  expect_s3_class(res$distribution, "data.table")
  expect_named(res$distribution,
                 c("class", "n_neighbours", "probability"),
                 ignore.order = TRUE)
  expect_equal(sum(res$distribution$n_neighbours), 5L)
  expect_equal(nrow(res$neighbours), 5L)
  expect_equal(res$query$k, 5L)
  expect_equal(res$query$system, "wrb2022")
})


test_that("classify_by_spectral_neighbours() rejects mismatched widths", {
  lib <- make_demo_with_labels()
  expect_error(
    classify_by_spectral_neighbours(rnorm(100L), lib, k = 5L,
                                      verbose = FALSE),
    "wavelengths"
  )
})


test_that("classify_by_spectral_neighbours() requires the right label column", {
  lib <- make_demo_with_labels()
  lib$Yr$wrb_rsg <- NULL
  expect_error(
    classify_by_spectral_neighbours(lib$Xr[1, ], lib, k = 5L,
                                      verbose = FALSE),
    "wrb_rsg"
  )
})


test_that("region filter narrows the library", {
  lib <- make_demo_with_labels()
  query <- lib$Xr[1, ]
  res_full <- classify_by_spectral_neighbours(query, lib, k = 5L,
                                                 verbose = FALSE)
  res_local <- classify_by_spectral_neighbours(
    query, lib, k = 5L,
    region = list(lat = -22.7, lon = -43.7, radius_km = 200),
    verbose = FALSE
  )
  expect_lte(res_local$query$n_filtered, res_full$query$n_filtered)
})


test_that(".haversine_km is sane on known distances", {
  # Rio de Janeiro - Sao Paulo: ~360 km great-circle.
  d <- soilKey:::.haversine_km(-22.91, -43.17, -23.55, -46.63)
  expect_gt(d, 320)
  expect_lt(d, 380)
})


test_that("region filter falling back to global when nothing in radius", {
  lib <- make_demo_with_labels()
  query <- lib$Xr[1, ]
  res <- classify_by_spectral_neighbours(
    query, lib, k = 5L,
    region = list(lat = 60, lon = 100, radius_km = 50),
    verbose = FALSE
  )
  # Should fall back to global library.
  expect_equal(res$query$n_filtered, nrow(lib$Xr))
})
