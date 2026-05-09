# =============================================================================
# Tests for v0.9.91 -- load_kssl_sample() and load_kssl_nasis_sample()
# now alias site$reference_wrb_from_usda -> site$reference_wrb so generic
# benchmark loops that read p$site[["reference_wrb"]] (strict access)
# work off-the-shelf on the bundled KSSL caches. v0.9.91 also hardens
# the WoSIS alias from v0.9.88 to use strict [[]] access (sidestepping
# R's $-partial-matching footgun).
# =============================================================================


test_that("v0.9.91: load_kssl_sample populates reference_wrb (strict access)", {
  fp <- system.file("extdata", "kssl_sample.rds", package = "soilKey")
  if (!nzchar(fp)) fp <- "inst/extdata/kssl_sample.rds"
  skip_if_not(file.exists(fp), "kssl_sample.rds not bundled")
  s <- load_kssl_sample()
  refs <- vapply(s$pedons, function(p) {
    val <- p$site[["reference_wrb"]]
    if (is.null(val) || is.na(val)) NA_character_ else as.character(val)
  }, character(1))
  expect_equal(sum(!is.na(refs)), length(s$pedons))
})


test_that("v0.9.91: load_kssl_nasis_sample populates reference_wrb (strict access)", {
  fp <- system.file("extdata", "kssl_nasis_sample.rds", package = "soilKey")
  if (!nzchar(fp)) fp <- "inst/extdata/kssl_nasis_sample.rds"
  skip_if_not(file.exists(fp), "kssl_nasis_sample.rds not bundled")
  s <- load_kssl_nasis_sample()
  refs <- vapply(s$pedons, function(p) {
    val <- p$site[["reference_wrb"]]
    if (is.null(val) || is.na(val)) NA_character_ else as.character(val)
  }, character(1))
  expect_equal(sum(!is.na(refs)), length(s$pedons))
})


test_that("v0.9.91: KSSL reference_wrb mirrors reference_wrb_from_usda verbatim", {
  fp <- system.file("extdata", "kssl_nasis_sample.rds", package = "soilKey")
  if (!nzchar(fp)) fp <- "inst/extdata/kssl_nasis_sample.rds"
  skip_if_not(file.exists(fp), "kssl_nasis_sample.rds not bundled")
  s <- load_kssl_nasis_sample()
  for (p in head(s$pedons, 5)) {
    expect_identical(p$site[["reference_wrb"]],
                      p$site[["reference_wrb_from_usda"]])
  }
})


test_that("v0.9.91: WoSIS strict-access alias still works (v0.9.88 + v0.9.91 hardening)", {
  fp <- system.file("extdata", "wosis_stratified_sample.rds", package = "soilKey")
  if (!nzchar(fp)) fp <- "inst/extdata/wosis_stratified_sample.rds"
  skip_if_not(file.exists(fp), "wosis_stratified_sample.rds not bundled")
  s <- load_wosis_stratified_sample()
  refs <- vapply(s$pedons, function(p) {
    val <- p$site[["reference_wrb"]]
    if (is.null(val) || is.na(val)) NA_character_ else as.character(val)
  }, character(1))
  expect_equal(sum(!is.na(refs)), length(s$pedons))
  # And mirrors wosis_rsg verbatim
  for (p in head(s$pedons, 5)) {
    expect_identical(p$site[["reference_wrb"]], p$site[["wosis_rsg"]])
  }
})


test_that("v0.9.91: KSSL+NASIS WRB benchmark non-zero with strict access", {
  fp <- system.file("extdata", "kssl_nasis_sample.rds", package = "soilKey")
  if (!nzchar(fp)) fp <- "inst/extdata/kssl_nasis_sample.rds"
  skip_if_not(file.exists(fp), "kssl_nasis_sample.rds not bundled")
  s <- load_kssl_nasis_sample()
  correct <- 0L; n <- 0L
  for (p in s$pedons) {
    ref <- p$site[["reference_wrb"]]
    if (is.null(ref) || is.na(ref) || !nzchar(ref)) next
    cls <- tryCatch(suppressMessages(suppressWarnings(
      classify_wrb2022(p, on_missing = "silent"))),
      error = function(e) NULL)
    pred <- if (!is.null(cls)) cls$rsg_or_order %||% NA_character_ else NA_character_
    ref_norm <- normalise_febr_wrb(ref)
    n <- n + 1L
    if (!is.na(pred) && !is.na(ref_norm) && pred == ref_norm) correct <- correct + 1L
  }
  expect_equal(n, length(s$pedons))
  expect_gt(correct, 15L)  # observed 21 in default canonical
})
