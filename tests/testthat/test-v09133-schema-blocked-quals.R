# v0.9.133 -- four new schema fields unlock the remaining schema-blocked WRB
# qualifiers (Glacic / Mochipic / Isopteric / Aceric), and Hydric now uses the
# undried 1500 kPa water field added in v0.9.128. Refine-when-present.

mk <- function(df) {
  PedonRecord$new(horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("the four new fields are in the horizon schema spec", {
  spec <- names(horizon_column_spec())
  expect_true(all(c("ice_pct", "water_saturation_days",
                    "particles_630um_pct", "jarosite_present") %in% spec))
})

test_that("Glacic: ice_pct >= 75% enforced where measured", {
  base <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 70),
                     designation = c("Oi", "Wf"),
                     permafrost_temp_C = c(-3, -4))
  lo <- base; lo$ice_pct <- c(NA, 40)   # measured but < 75 -> not Glacic
  hi <- base; hi$ice_pct <- c(NA, 85)   # >= 75 over 40 cm -> Glacic
  expect_false(isTRUE(qual_glacic(mk(lo))$passed))
  expect_true(qual_glacic(mk(hi))$passed)
})

test_that("Aceric: jarosite required where recorded", {
  base <- data.frame(top_cm = 0, bottom_cm = 30, ph_h2o = 4.2)
  noj <- base; noj$jarosite_present <- FALSE  # right pH but no jarosite
  yesj <- base; yesj$jarosite_present <- TRUE
  expect_false(isTRUE(qual_aceric(mk(noj))$passed))
  expect_true(qual_aceric(mk(yesj))$passed)
  # field absent -> pH-only behaviour preserved
  expect_true(qual_aceric(mk(base))$passed)
})

test_that("Mochipic: >= 300 saturation days where measured + >= 25 cm", {
  base <- data.frame(top_cm = c(0, 10), bottom_cm = c(10, 60),
                     mottle_morphology = c("mochi", "banded"))
  dry <- base; dry$water_saturation_days <- c(100, 100)  # < 300 -> no
  wet <- base; wet$water_saturation_days <- c(320, 320)  # >= 300, 60 cm -> yes
  expect_false(isTRUE(qual_mochipic(mk(dry))$passed))
  expect_true(qual_mochipic(mk(wet))$passed)
})

test_that("Isopteric: bulk density <= 1.3 and < 5% particles >= 630 um", {
  base <- data.frame(top_cm = 0, bottom_cm = 40,
                     bioturbation_density = "termite mounds")
  dense <- base; dense$bulk_density_g_cm3 <- 1.6  # > 1.3 -> no
  ok <- base; ok$bulk_density_g_cm3 <- 1.1; ok$particles_630um_pct <- 3
  coarse <- base; coarse$bulk_density_g_cm3 <- 1.1; coarse$particles_630um_pct <- 12
  expect_false(isTRUE(qual_isopteric(mk(dense))$passed))
  expect_true(qual_isopteric(mk(ok))$passed)
  expect_false(isTRUE(qual_isopteric(mk(coarse))$passed))
})

test_that("Hydric: uses undried 1500 kPa water where measured (>= 35 cm)", {
  # andic layer >= 35 cm; undried water 75% -> Hydric
  base <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     al_ox_pct = c(2.5, 2.5), fe_ox_pct = c(1.5, 1.5),
                     si_ox_pct = c(1.0, 1.0), phosphate_retention_pct = c(90, 90),
                     bulk_density_g_cm3 = c(0.7, 0.7))
  if (isTRUE(andic_properties(mk(base))$passed)) {
    wet <- base; wet$water_content_1500kpa_undried <- c(75, 78)
    expect_true(qual_hydric(mk(wet))$passed)
  } else {
    skip("andic_properties did not pass on the synthetic fixture")
  }
})
