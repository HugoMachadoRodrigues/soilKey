# v0.9.428 (Hugo, 2026-11-23): the Vertic subgroups read a COLE not recorded
# from the field record. Cracks and slickensides are field observations,
# absent when not recorded; a profile that records neither has no linear
# extensibility of 6 cm or more, so its Vertic subgroup test is FALSE, not
# undecided. A recorded COLE still decides; any crack or slickenside recorded
# keeps the test undecided until COLE is measured.

.v09428_pedon <- function(cole = NA_real_, cw = NA_real_, cd = NA_real_,
                          slick = NA_character_) {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 50, 100), bottom_cm = c(20, 50, 100, 150),
    designation = c("Ap", "Bt1", "Bt2", "C"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 4, 5, 5),
    munsell_chroma_moist = c(2, 4, 4, 4),
    clay_pct = c(30, 45, 45, 40), sand_pct = 20, silt_pct = c(50, 35, 35, 40),
    oc_pct = c(2, 0.8, 0.4, 0.2), bs_pct = 80, ph_h2o = 7,
    structure_type = c("granular", "angular blocky", "angular blocky", "massive"),
    cole_value = cole, cracks_width_cm = cw, cracks_depth_cm = cd,
    slickensides = slick)
  PedonRecord$new(site = list(id = "v09428"), horizons = ensure_horizon_schema(hz))
}

.v09428_fns <- c("sg_alfisols_vertic_usda", "sg_aridisols_vertic_usda",
                 "sg_incept_vertic_usda", "sg_mollisol_vertic_usda",
                 "sg_ultisol_vertic_usda")

.v09428_run <- function(fn, p) get(fn, envir = asNamespace("soilKey"))(p)

test_that("no COLE, no crack, no slickenside: every Vertic test is FALSE", {
  testthat::skip_on_cran()
  p <- .v09428_pedon()
  for (fn in .v09428_fns) {
    r <- .v09428_run(fn, p)
    expect_false(r$passed, info = fn)
    expect_false("cole_value" %in% r$missing, info = fn)
  }
})

test_that("a crack or a slickenside recorded keeps a COLE not recorded undecided", {
  testthat::skip_on_cran()
  crack <- .v09428_pedon(cw = c(NA, 0.3, 0.3, NA), cd = c(NA, 20, 20, NA))
  slick <- .v09428_pedon(slick = c(NA, "few", NA, NA))
  expect_true(soilKey:::.usda_vertic_field_signs(crack$horizons))
  expect_true(soilKey:::.usda_vertic_field_signs(slick$horizons))
  for (fn in .v09428_fns) {
    expect_true(is.na(.v09428_run(fn, crack)$passed), info = fn)
  }
})

test_that("slickensides recorded as absent are no sign of shrink and swell", {
  testthat::skip_on_cran()
  p <- .v09428_pedon(slick = "none")
  expect_false(soilKey:::.usda_vertic_field_signs(p$horizons))
  for (fn in .v09428_fns) expect_false(.v09428_run(fn, p)$passed, info = fn)
})

test_that("a recorded COLE still decides", {
  testthat::skip_on_cran()
  hi <- .v09428_pedon(cole = c(0.08, 0.08, 0.08, 0.08))
  lo <- .v09428_pedon(cole = c(0.01, 0.01, 0.01, 0.01))
  for (fn in .v09428_fns) {
    expect_true(.v09428_run(fn, hi)$passed, info = fn)
    r <- .v09428_run(fn, lo)
    expect_false(r$passed, info = fn)
    expect_null(r$evidence$linear_extensibility_from_field, info = fn)
  }
})
