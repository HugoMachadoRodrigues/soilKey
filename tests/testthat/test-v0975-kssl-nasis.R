# =============================================================================
# Tests for v0.9.75 -- bundled KSSL+NASIS morphological-enriched sample.
# =============================================================================


test_that("v0.9.75: load_kssl_nasis_sample() returns enriched pedons", {
  skip_on_cran()
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "kssl_nasis_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "kssl_nasis_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL+NASIS sample not present")
  s <- load_kssl_nasis_sample()
  expect_named(s, c("pedons", "pulled_on", "source", "join_helper",
                      "cross_walk"), ignore.order = TRUE)
  expect_true(length(s$pedons) >= 95L)   # head=100; 95-100 valid
  for (pr in s$pedons[1:5]) {
    expect_true(!is.null(pr$site$reference_usda))
    expect_true(!is.null(pr$site$reference_wrb_from_usda))
  }
})


test_that("v0.9.75: KSSL+NASIS sample has Munsell colour data (lift from 0% to ~90%)", {
  skip_on_cran()
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "kssl_nasis_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "kssl_nasis_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL+NASIS sample not present")
  s <- load_kssl_nasis_sample()
  has_munsell <- function(field) {
    mean(vapply(s$pedons, function(p) {
      if (!field %in% colnames(p$horizons)) return(0)
      sum(!is.na(p$horizons[[field]])) / nrow(p$horizons)
    }, numeric(1)))
  }
  # Should be >= 80% (we measured 89.6% on the live build)
  expect_gt(has_munsell("munsell_hue_moist"),    0.7)
  expect_gt(has_munsell("munsell_value_moist"),  0.7)
  expect_gt(has_munsell("munsell_chroma_moist"), 0.7)
})


test_that("v0.9.75: KSSL+NASIS sample has structure_grade / type", {
  skip_on_cran()
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "kssl_nasis_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "kssl_nasis_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL+NASIS sample not present")
  s <- load_kssl_nasis_sample()
  has_field <- function(field) {
    any(vapply(s$pedons, function(p) {
      if (!field %in% colnames(p$horizons)) return(FALSE)
      any(!is.na(p$horizons[[field]]) & nzchar(p$horizons[[field]]))
    }, logical(1)))
  }
  expect_true(has_field("structure_grade"))
  expect_true(has_field("structure_type"))
})


test_that("v0.9.75: classify_wrb2022 runs without error on every NASIS-enriched pedon", {
  skip_on_cran()
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "kssl_nasis_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "kssl_nasis_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL+NASIS sample not present")
  s <- load_kssl_nasis_sample()
  errors <- sum(vapply(s$pedons, function(pr) {
    res <- tryCatch(classify_wrb2022(pr, on_missing = "silent"),
                     error = function(e) NULL)
    is.null(res)
  }, logical(1)))
  expect_equal(errors, 0L)
})


test_that("v0.9.75: benchmark_wrb_vs_usda runs end-to-end on NASIS-enriched sample", {
  skip_on_cran()
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "kssl_nasis_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "kssl_nasis_sample.rds",
                                                package = "soilKey")),
                          "Bundled KSSL+NASIS sample not present")
  s <- load_kssl_nasis_sample()
  res <- benchmark_wrb_vs_usda(s$pedons[1:20], verbose = FALSE)
  expect_true(res$n_total == 20L)
  expect_true(is.numeric(res$accuracy))
})
