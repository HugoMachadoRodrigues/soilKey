# v0.9.195: the "Missing data" panel says WHERE an attribute is missing.
#
# Reported from ISRIC on reference profile ZW005: clay_pct, silt_pct, sand_pct,
# oc_pct, cec_cmol and caco3_pct were listed as missing on the Classification
# page even though the upload carried them. They were never rejected -- the
# profile's lowest two horizons (BCk, CBk) had no laboratory data, and a
# predicate that scans the whole profile records the attribute name as soon as
# ONE horizon lacks it. With only the name shown, a partially-filled column is
# indistinguishable from one that was thrown away.
#
# These tests pin the coverage helper the panel now annotates each entry with.

.cov_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = env)
  env
}

# The ISRIC shape: five horizons with laboratory data, two without.
.cov_pedon <- function() {
  soilKey::PedonRecord$new(
    site = list(id = "ZW005"),
    horizons = data.frame(
      designation = c("Ah", "AB", "BA", "Bt1", "Bt2", "BCk", "CBk"),
      top_cm      = c(0, 24, 39, 71, 88, 107, 146),
      bottom_cm   = c(24, 39, 71, 88, 107, 146, 175),
      clay_pct    = c(33.5, 43.3, 44.7, 47.8, 31.9, NA, NA),
      caco3_pct   = c(0, 0, 0, 1.6, 2.9, NA, NA),
      fe_ox_pct   = rep(NA_real_, 7)))       # never provided at all
}


test_that("a partially provided attribute reports its coverage", {
  skip_on_cran()
  cov <- get(".classify_attr_coverage", envir = .cov_env())
  pr  <- .cov_pedon()
  for (a in c("clay_pct", "caco3_pct")) {
    r <- cov(a, pr)
    expect_false(is.null(r))
    expect_equal(r$filled, 5L)
    expect_equal(r$n, 7L)
  }
})

test_that("an attribute the pedon never carries reports nothing", {
  skip_on_cran()
  # NULL means "no annotation": for these, 'missing' really is the whole story,
  # and a '0 of 7' badge would be noise on every unmeasured attribute.
  cov <- get(".classify_attr_coverage", envir = .cov_env())
  expect_null(cov("fe_ox_pct", .cov_pedon()))
})

test_that("non-horizon entries are left alone", {
  skip_on_cran()
  cov <- get(".classify_attr_coverage", envir = .cov_env())
  pr  <- .cov_pedon()
  # site fields and the composite hints the keys emit are not columns
  expect_null(cov("site$soil_moisture_regime", pr))
  expect_null(cov("al_sat_pct (or ca+mg+k+na+al_cmol)", pr))
  expect_null(cov("clay_pct", NULL))
})

test_that("a fully provided attribute still reports its full coverage", {
  skip_on_cran()
  # It can still be listed as missing for a reason other than absence (a
  # predicate wanting it at a depth the profile does not reach), and "7 of 7"
  # is exactly the signal that the upload was fine.
  cov <- get(".classify_attr_coverage", envir = .cov_env())
  r <- cov("top_cm", .cov_pedon())
  expect_equal(r$filled, 7L)
  expect_equal(r$n, 7L)
})

test_that("both locales carry the coverage string with two integer slots", {
  skip_on_cran()
  f <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(f)) f <- file.path("inst", "i18n", "translations.yaml")
  skip_if_not(file.exists(f))
  y <- readLines(f, warn = FALSE)
  hits <- grep('"classify\\.attr_coverage"', y, value = TRUE)
  expect_equal(length(hits), 2L)                    # en + pt
  for (h in hits) expect_equal(lengths(regmatches(h, gregexpr("%d", h))), 2L)
})
