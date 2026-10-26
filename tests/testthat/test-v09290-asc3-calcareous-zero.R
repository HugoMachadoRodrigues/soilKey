# v0.9.290: carbonate segregations recorded as none (0 %) are read like no
# record of them: the pH rule applies (a pHw below 7.0 is not calcareous).

test_that("a recorded 0 % of segregations is read like no record", {
  skip_on_cran()
  mk <- function(seg, ph, eff = NA_character_) {
    PedonRecord$new(site = list(id = "t", country = "AU", designation_system = "asc", drainage_class = "well drained"),
                    horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(
                      designation = "A1", top_cm = 0, bottom_cm = 20, secondary_carbonates_pct = seg, ph_h2o = ph,
                      effervescence_class = eff, stringsAsFactors = FALSE))))
  }
  calc <- function(p) soilKey:::.asc_calcareous(soilKey:::.asc_horizons(p))
  expect_false(calc(mk(0, 6)))
  expect_false(calc(mk(NA_real_, 6)))
  expect_true(is.na(calc(mk(0, 7.5))))
  expect_true(calc(mk(5, 6)))
  expect_true(calc(mk(0, 8, "strong")))
})
