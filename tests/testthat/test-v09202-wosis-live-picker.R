# v0.9.202: the WoSIS picker queries ISRIC live and filters by licence.
#
# soilKey distributes no soil observations (inst/DATA-PROVENANCE.md). The picker now
# asks ISRIC per query and keeps nothing. The licence filter is the load-bearing
# part: WoSIS is licensed PER PROFILE as each provider specified, and roughly
# half of wosis_latest is CC BY-NC. A picker without the filter would surface
# NonCommercial material silently, and a package that bundled it would be
# granting rights (MIT permits commercial use) that it does not hold.
#
# The network-touching tests are skipped unless explicitly opted into, so the
# suite never depends on ISRIC being up.

.wosis_live <- function() {
  isTRUE(as.logical(Sys.getenv("SOILKEY_TEST_WOSIS_LIVE", "false")))
}


test_that("the NonCommercial test recognises every CC BY-NC spelling", {
  f <- soilKey:::.wosis_is_permissive
  # Real strings returned by the endpoint.
  expect_false(f("Attribution-NonCommercial 4.0 International (CC BY-NC 4.0), https://creativecommons.org/licenses/by-nc/4.0/"))
  expect_false(f("Attribution-NonCommercial 3.0 International (CC BY-NC 3.0), https://creativecommons.org/licenses/by-nc/3.0/"))
  expect_true(f("Attribution 3.0 International (CC BY 3.0), https://creativecommons.org/licenses/by/3.0/"))
  expect_true(f("Attribution 4.0 International (CC BY 4.0), https://creativecommons.org/licenses/by/4.0/"))
  expect_true(f("U.S. Public Domain http://www.usa.gov/publicdomain/label/1.0/"))
})

test_that("an unknown or empty licence is NOT treated as permissive", {
  # Unknown terms are not permissive terms: failing open here would ship
  # somebody else's data under rights nobody granted.
  f <- soilKey:::.wosis_is_permissive
  expect_false(f(""))
  expect_false(f(NA))
  expect_false(f(NA_character_))
  # NULL is zero-length input, so zero-length output: the function is used
  # vectorised to subset a data.frame, and returning FALSE for "no rows" would
  # be a scalar answer to a vector question.
  expect_length(f(NULL), 0L)
  # vectorised, and NA inside a vector must not drag a real licence down
  expect_identical(f(c("CC BY 4.0", NA, "CC BY-NC 4.0")), c(TRUE, FALSE, FALSE))
})

test_that("licence strings get a short label for the table", {
  g <- soilKey:::.wosis_licence_short
  expect_identical(g("Attribution-NonCommercial 4.0 International (CC BY-NC 4.0)"), "CC BY-NC 4.0")
  expect_identical(g("Attribution 3.0 International (CC BY 3.0)"), "CC BY 3.0")
  expect_identical(g("U.S. Public Domain http://www.usa.gov/publicdomain/label/1.0/"), "Public domain")
  expect_true(is.na(g("")))
})

test_that("a network failure returns zero rows instead of throwing", {
  # An unreachable ISRIC is an expected condition in a Shiny app, not an error
  # that should take the session down.
  r <- read_wosis_profiles_graphql(country = "Nowhere", n_max = 1, timeout = 0.001)
  expect_s3_class(r, "data.frame")
  expect_equal(nrow(r), 0L)
  expect_false(is.null(attr(r, "wosis_error")))
})

test_that("the empty result still carries the documented columns", {
  r <- read_wosis_profiles_graphql(country = "Nowhere", n_max = 1, timeout = 0.001)
  expect_true(all(c("profile_code", "country", "lat", "lon", "wrb_rsg",
                    "dataset", "licence", "licence_short") %in% names(r)))
})

test_that("a pedon built from a profile row carries licence and attribution", {
  # CC BY requires attribution on reuse. The bundled snapshot that preceded this
  # dropped the licence and dataset fields, which is exactly what turned a
  # licensed use into an unattributed one.
  row <- data.frame(
    profile_id = "1", profile_code = "AR SC.P7", country = "Argentina",
    lat = -49.666, lon = -72.817, wrb_rsg = "Cambisols", usda_order = NA_character_,
    dataset = "AR-SOTER",
    licence = "Attribution 3.0 International (CC BY 3.0), https://creativecommons.org/licenses/by/3.0/",
    licence_short = "CC BY 3.0", stringsAsFactors = FALSE)
  p <- wosis_profile_to_pedon(row, fetch_layers = FALSE)   # site only, no network
  expect_s3_class(p, "PedonRecord")
  expect_identical(p$site$id, "AR SC.P7")
  expect_identical(p$site$dataset, "AR-SOTER")
  expect_match(p$site$licence, "CC BY 3.0", fixed = TRUE)
  expect_match(p$site$attribution, "WoSIS", fixed = TRUE)
  expect_match(p$site$attribution, "AR-SOTER", fixed = TRUE)
})

test_that("the app's source selector always offers WoSIS now", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  expect_true("wosis" %in% e$.sk_pedon_sources())
})

test_that("LIVE: the filter actually excludes NonCommercial profiles", {
  skip_on_cran()
  skip_if_not(.wosis_live(), "set SOILKEY_TEST_WOSIS_LIVE=true to query ISRIC")
  # Argentina (AR-SOTER) is CC BY 3.0; Brazil's first page (BR-Bernoux) is
  # CC BY-NC 4.0. The pair exercises both sides of the filter.
  ar <- read_wosis_profiles_graphql(country = "Argentina", n_max = 5)
  expect_gt(nrow(ar), 0L)
  expect_false(any(grepl("NC", ar$licence_short)))

  br_any <- read_wosis_profiles_graphql(country = "Brazil", n_max = 5,
                                        licence_filter = "any")
  br_ok  <- read_wosis_profiles_graphql(country = "Brazil", n_max = 5)
  expect_true(any(grepl("NC", br_any$licence_short)))
  expect_false(any(grepl("NC", br_ok$licence_short)))
  # and it says how many it withheld, so the UI can explain the empty table
  expect_gt(attr(br_ok, "wosis_excluded"), 0L)
})
