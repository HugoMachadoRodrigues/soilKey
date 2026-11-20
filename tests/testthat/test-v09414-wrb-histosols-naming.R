# v0.9.414 (Hugo, 2026-11-19): the WRB 2022 Histosol qualifiers read from the
# record.
#   * folic against histic (Ch 3.1.12, p. 46; Ch 3.1.15, p. 48): drainage
#     recorded or >= 30 days of saturation is histic, < 30 days and not
#     drained folic, neither recorded undecided;
#   * Floatic (Ch 5, p. 134) only when floating organic material is recorded;
#   * Ombric and Rheic (Ch 5, pp. 142, 145) need a histic horizon and the
#     recorded water supply, not the pH.

.v09414_peat <- function(site = list(id = "v09414"), wsd = c(NA, NA),
                         ph = c(3.8, 4.0), ...) {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Oa", "Oe"),
    oc_pct = c(40, 35), ph_h2o = ph, bulk_density_g_cm3 = c(0.10, 0.15),
    water_saturation_days = wsd, ...)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(hz))
}

test_that("folic or histic is decided by drainage and saturation", {
  testthat::skip_on_cran()
  drained <- .v09414_peat(site = list(id = "d", land_use = "mire (drained)"))
  drained2 <- .v09414_peat(site = list(id = "d2", artificially_drained = TRUE),
                           wsd = c(5, 5))
  wet <- .v09414_peat(wsd = c(120, 150))
  dry <- .v09414_peat(wsd = c(5, 5))
  none <- .v09414_peat()
  for (p in list(drained, drained2, wet)) {
    expect_true(isTRUE(histic_horizon(p, saturation = TRUE)$passed))
    expect_identical(folic(p)$passed, FALSE)
    expect_identical(soilKey:::qual_folic(p)$passed, FALSE)
  }
  expect_identical(histic_horizon(dry, saturation = TRUE)$passed, FALSE)
  expect_true(isTRUE(folic(dry)$passed))
  expect_true(isTRUE(soilKey:::qual_folic(dry)$passed))
  # neither recorded: undecided
  expect_true(is.na(histic_horizon(none, saturation = TRUE)$passed))
  expect_true(is.na(folic(none)$passed))
  expect_true(is.na(soilKey:::qual_folic(none)$passed))
  expect_false(isTRUE(soilKey:::qual_histic(none)$passed))   # not named
  # the default of histic_horizon() does not read the water, as before
  expect_true(isTRUE(histic_horizon(none)$passed))
  expect_true(isTRUE(histic_horizon(dry)$passed))
})

test_that("a natural drainage class is not a record of drainage", {
  testthat::skip_on_cran()
  p <- .v09414_peat(site = list(id = "n", drainage_class = "very poorly drained"))
  expect_false(soilKey:::.wrb_site_drained(p))
  expect_true(is.na(histic_horizon(p, saturation = TRUE)$passed))
  expect_true(isTRUE(soilKey:::qual_drainic(
    .v09414_peat(site = list(id = "a", artificially_drained = TRUE)))$passed))
})

test_that("Floatic needs floating organic material recorded", {
  testthat::skip_on_cran()
  # a very light peat is not floating on its own
  expect_identical(soilKey:::qual_floatic(.v09414_peat())$passed, FALSE)
  expect_true(isTRUE(soilKey:::qual_floatic(
    .v09414_peat(site = list(id = "f", floating_organic_material = TRUE)))$passed))
  expect_true(isTRUE(soilKey:::qual_floatic(
    .v09414_peat(site = list(id = "f2", landform = "floating mat on a lake")))$passed))
  expect_identical(soilKey:::qual_floatic(
    .v09414_peat(site = list(id = "f3", floating_organic_material = FALSE,
                             landform = "floating mat")))$passed, FALSE)
})

test_that("Ombric and Rheic read the water supply of a histic horizon, not the pH", {
  testthat::skip_on_cran()
  # acid peat, nothing recorded: neither is decided
  acid <- .v09414_peat(wsd = c(200, 200))
  expect_true(is.na(soilKey:::qual_ombric(acid)$passed))
  expect_true(is.na(soilKey:::qual_rheic(acid)$passed))
  # the record decides, whatever the pH
  bog <- .v09414_peat(site = list(id = "b", mire_water_supply = "rainwater"),
                      wsd = c(200, 200), ph = c(6.5, 7))
  fen <- .v09414_peat(site = list(id = "fe", mire_water_supply = "groundwater"),
                      wsd = c(200, 200))
  expect_true(isTRUE(soilKey:::qual_ombric(bog)$passed))
  expect_identical(soilKey:::qual_rheic(bog)$passed, FALSE)
  expect_true(isTRUE(soilKey:::qual_rheic(fen)$passed))
  expect_identical(soilKey:::qual_ombric(fen)$passed, FALSE)
  # peat_moor_type of the upper part, and a raised bog named at the site
  hm <- .v09414_peat(wsd = c(200, 200), peat_moor_type = c("high-moor", "low-moor"))
  expect_true(isTRUE(soilKey:::qual_ombric(hm)$passed))
  rb <- .v09414_peat(site = list(id = "rb", landform = "raised bog"), wsd = c(200, 200))
  expect_true(isTRUE(soilKey:::qual_ombric(rb)$passed))
  # a folic peat has no histic horizon
  folic_bog <- .v09414_peat(site = list(id = "fb", mire_water_supply = "rainwater"),
                            wsd = c(5, 5))
  expect_identical(soilKey:::qual_ombric(folic_bog)$passed, FALSE)
})

test_that("the canonical Histosol is no longer Folic, Floatic and Ombric at once", {
  testthat::skip_on_cran()
  r <- classify_wrb2022(make_histosol_canonical(), on_missing = "silent")
  expect_equal(r$rsg_or_order, "Histosols")
  expect_false(any(c("Folic", "Floatic", "Ombric", "Rheic") %in% r$qualifiers$principal))
  expect_true(all(c("Drainic", "Sapric") %in% r$qualifiers$principal))
})
