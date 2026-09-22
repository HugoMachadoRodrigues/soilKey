# =============================================================================
# Tests for v0.9.68 -- B_latossolico() engine propagation.
#
# v0.9.67 added an `engine` arg to ferralic() but B_latossolico() (the
# SiBCS Cap 2 Latossolos diagnostic) hard-coded `max_cec_per_clay = 17`,
# preventing the engine="aqp" relaxation (max_cec=20) from reaching
# Latossolos detection. v0.9.68 lets B_latossolico read the engine
# option (or accept an explicit `engine` arg) and forward it to ferralic.
# =============================================================================

.borderline_latossolo <- function(cec_per_clay = 18) {
  # CEC = cec_per_clay * 0.8 cmol_c, clay = 80% -> CEC/clay = cec_per_clay
  hz <- data.table::data.table(
    top_cm    = c(0, 30, 100, 180),
    bottom_cm = c(30, 100, 180, 250),
    designation = c("A", "Bw1", "Bw2", "BC"),
    coarse_fragments_pct = c(2, 3, 5, 4),
    clay_pct = c(60, 80, 80, 78),
    silt_pct = c(20, 12, 12, 14),
    sand_pct = c(20,  8,  8,  8),
    cec_cmol = c(NA_real_, cec_per_clay * 0.8, cec_per_clay * 0.8, NA_real_),
    oc_pct   = c(2.0, 0.6, 0.4, 0.2),
    ph_h2o   = c(5.0, 5.1, 5.2, 5.3),
    structure_grade = c("moderate", "moderate", "moderate", "weak"),
    structure_type  = c("granular", "subangular blocky", "subangular blocky", "blocky"),
    structure_size  = c("small", "small", "small", "medium")
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "RJ-borderline-lato", lat = -22, lon = -43, country = "BR"),
    horizons = hz
  )
}


test_that("v0.9.68: B_latossolico fails on borderline (CEC/clay=18) under soilkey engine", {
  # soilkey default: max_cec_per_clay = 17 -> 18 fails
  pr <- .borderline_latossolo(cec_per_clay = 18)
  res <- B_latossolico(pr, engine = "soilkey")
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.68: B_latossolico passes on same borderline under aqp engine", {
  # aqp default: max_cec_per_clay = 20 -> 18 passes
  pr <- .borderline_latossolo(cec_per_clay = 18)
  res <- B_latossolico(pr, engine = "aqp")
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.68: B_latossolico engine=NULL reads getOption('soilKey.diagnostic_engine')", {
  pr <- .borderline_latossolo(cec_per_clay = 18)
  withr::with_options(
    list(soilKey.diagnostic_engine = "aqp"),
    expect_true(isTRUE(B_latossolico(pr)$passed))
  )
  withr::with_options(
    list(soilKey.diagnostic_engine = "soilkey"),
    expect_false(isTRUE(B_latossolico(pr)$passed))
  )
})


test_that("v0.9.68: explicit max_cec_per_clay overrides the engine default", {
  pr <- .borderline_latossolo(cec_per_clay = 18)
  expect_false(isTRUE(B_latossolico(pr, max_cec_per_clay = 16)$passed))
  expect_true(isTRUE(B_latossolico(pr, max_cec_per_clay = 25)$passed))
})


test_that("v0.9.68: low-CEC Latossolo (CEC/clay=8) passes under both engines", {
  pr <- .borderline_latossolo(cec_per_clay = 8)
  expect_true(isTRUE(B_latossolico(pr, engine = "soilkey")$passed))
  expect_true(isTRUE(B_latossolico(pr, engine = "aqp")$passed))
})


test_that("v0.9.68: high-CEC profile (CEC/clay=25) fails under both engines", {
  pr <- .borderline_latossolo(cec_per_clay = 25)
  expect_false(isTRUE(B_latossolico(pr, engine = "soilkey")$passed))
  expect_false(isTRUE(B_latossolico(pr, engine = "aqp")$passed))
})


test_that("v0.9.68: B_latossolico backwards-compat -- old max_cec_per_clay=17 still works as kw", {
  pr <- .borderline_latossolo(cec_per_clay = 18)
  # Caller can still pin to 17 explicitly
  expect_false(isTRUE(B_latossolico(pr, max_cec_per_clay = 17)$passed))
})
