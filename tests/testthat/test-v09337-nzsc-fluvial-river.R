# v0.9.319: the NZSC fluvial features read a river deposit as fluvial, and
# alluvium also in the spelling aluvium (as NSD parent-rock notes write
# them; Hugo, 2026-10-30). Made-up profiles.

.fl37 <- function(origin) {
  PedonRecord$new(site = list(id = "fl37", country = "NZ", designation_system = "nzsc"),
    horizons = ensure_horizon_schema(data.table::data.table(
      designation = c("Ap", "Bw", "C"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
      layer_origin = origin)))
}

test_that("a river deposit and alluvium spelt aluvium are fluvial", {
  skip_on_cran()
  for (o in c("gravel from Wairau River", "RIVER ALL. DERIVED FROM G/W", "riverine silts",
              "QUARTZO-FELDSPATHIC ALUVIUM SANDS AND SILTS", "fine alluvium from greywacke")) {
    expect_true(isTRUE(fluvial_group_nzsc(.fl37(rep(o, 3)))$passed), info = o)
  }
})

test_that("an origin that names no fluvial deposit is not fluvial", {
  skip_on_cran()
  for (o in c("OLD HIGH TERRACE GRAVEL FROM GRANITE", "greywacke", "Taupo pumice", "Riversdale loess")) {
    expect_false(isTRUE(fluvial_group_nzsc(.fl37(rep(o, 3)))$passed), info = o)
  }
  # a cover of more than 30 cm of loess over river gravels
  expect_false(isTRUE(fluvial_group_nzsc(.fl37(c("loess", "loess", "river gravels")))$passed))
})

test_that("WRB Raptic reads riverine like river", {
  skip_on_cran()
  p <- PedonRecord$new(site = list(id = "rp37"), horizons = ensure_horizon_schema(data.table::data.table(
    designation = c("A", "2C"), top_cm = c(0, 40), bottom_cm = c(40, 90),
    layer_origin = c("riverine silts", "residual"))))
  expect_false(isTRUE(qual_raptic(p)$passed))
})
