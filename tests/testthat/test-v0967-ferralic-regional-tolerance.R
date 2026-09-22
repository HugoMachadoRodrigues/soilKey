# =============================================================================
# Tests for v0.9.67 -- ferralic() regional CTC tolerance.
#
# BDsolos RJ benchmark (n=722 perfis) found that ~88/115 (76.5%) of
# Latossolos failed the strict WRB 16-cmol/kg-clay threshold because the
# Embrapa lab methodology often reads CEC at 17-20 on profiles that are
# unambiguously Latossolos by every other criterion.
#
# v0.9.67 adds an `engine` arg (defaults via getOption) and a tunable
# threshold via options(soilKey.ferralic_max_cec). soilkey-engine
# behaviour (16 cmol) is unchanged; aqp-engine relaxes to 20 cmol.
# =============================================================================

.make_borderline_latossolo <- function(cec_top = 18, cec_sub = 18) {
  # CEC = 18 cmol_c/kg with clay = 60% -> CEC/clay = 30 cmol/kg-clay,
  # which is far above 16 but only slightly above 20. We want a
  # CEC-per-clay around 17-19 (i.e. the borderline zone).
  # CEC/clay = cec_cmol / clay_pct * 100 (when both in standard units).
  # If we pick clay=80 and cec=14 we get CEC/clay = 17.5 cmol/kg-clay.
  hz <- data.table::data.table(
    top_cm    = c(0, 30, 100, 180),
    bottom_cm = c(30, 100, 180, 250),
    designation = c("A", "Bw1", "Bw2", "BC"),
    coarse_fragments_pct = c(2, 3, 5, 4),
    clay_pct = c(60, 80, 80, 78),
    silt_pct = c(20, 12, 12, 14),
    sand_pct = c(20,  8,  8,  8),
    cec_cmol = c(NA_real_, cec_top * 0.8, cec_sub * 0.8, NA_real_),
    oc_pct   = c(2.0, 0.6, 0.4, 0.2),
    ph_h2o   = c(5.0, 5.1, 5.2, 5.3)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "RJ-borderline", lat = -22, lon = -43, country = "BR"),
    horizons = hz
  )
}


test_that("v0.9.67: borderline Latossolo (CEC/clay ~17.5) FAILS ferralic in soilkey engine", {
  # CEC=14.4 cmol_c, clay=80% -> CEC/clay = 18 cmol/kg-clay -> > 16
  pr <- .make_borderline_latossolo(cec_top = 18, cec_sub = 18)
  res <- ferralic(pr, engine = "soilkey")
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.67: borderline Latossolo (CEC/clay ~18) PASSES ferralic in aqp engine", {
  pr <- .make_borderline_latossolo(cec_top = 18, cec_sub = 18)
  res <- ferralic(pr, engine = "aqp")
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.67: still fails when CEC/clay > 20 even under aqp engine", {
  # CEC=20 cmol_c, clay=60% -> CEC/clay = 33 cmol/kg-clay -> > 20
  hz <- data.table::data.table(
    top_cm    = c(0, 30, 100),
    bottom_cm = c(30, 100, 180),
    designation = c("A", "Bw1", "Bw2"),
    coarse_fragments_pct = c(2, 3, 5),
    clay_pct = c(50, 60, 60),
    silt_pct = c(25, 20, 20),
    sand_pct = c(25, 20, 20),
    cec_cmol = c(NA_real_, 20, 20),
    oc_pct   = c(2.0, 0.6, 0.4),
    ph_h2o   = c(5.0, 5.1, 5.2)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(
    site = list(id = "RJ-too-high", lat = -22, lon = -43, country = "BR"),
    horizons = hz
  )
  res <- ferralic(pr, engine = "aqp")
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.67: option soilKey.ferralic_max_cec overrides the engine default", {
  pr <- .make_borderline_latossolo(cec_top = 18, cec_sub = 18)
  withr::with_options(
    list(soilKey.ferralic_max_cec = 25),
    {
      res <- ferralic(pr, engine = "soilkey")
      expect_true(isTRUE(res$passed))
    }
  )
})


test_that("v0.9.67: explicit max_cec arg overrides both option and engine", {
  pr <- .make_borderline_latossolo(cec_top = 18, cec_sub = 18)
  res_strict <- ferralic(pr, engine = "aqp", max_cec = 16)
  expect_false(isTRUE(res_strict$passed))
  res_loose <- ferralic(pr, engine = "soilkey", max_cec = 24)
  expect_true(isTRUE(res_loose$passed))
})


test_that("v0.9.67: evidence$engine + evidence$max_cec_used record the active settings", {
  pr <- .make_borderline_latossolo(cec_top = 18, cec_sub = 18)
  res <- ferralic(pr, engine = "aqp")
  expect_identical(res$evidence$engine, "aqp")
  expect_equal(res$evidence$max_cec_used, 20)
})


test_that("v0.9.67: low-CEC profile passes ferralic on both engines", {
  # Real Ferralsol: CEC = 4 cmol_c, clay = 50% -> 8 cmol/kg-clay
  hz <- data.table::data.table(
    top_cm    = c(0, 30, 100, 180),
    bottom_cm = c(30, 100, 180, 260),
    designation = c("A", "Bw1", "Bw2", "BC"),
    coarse_fragments_pct = c(2, 3, 4, 3),
    clay_pct = c(40, 50, 50, 48),
    silt_pct = c(20, 15, 15, 17),
    sand_pct = c(40, 35, 35, 35),
    cec_cmol = c(NA_real_, 4, 4, NA_real_),
    oc_pct   = c(1.5, 0.5, 0.3, 0.2),
    ph_h2o   = c(5.0, 5.2, 5.3, 5.4)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(
    site = list(id = "true-ferralsol", lat = -22, lon = -43, country = "BR"),
    horizons = hz
  )
  expect_true(isTRUE(ferralic(pr, engine = "soilkey")$passed))
  expect_true(isTRUE(ferralic(pr, engine = "aqp")$passed))
})


test_that("v0.9.67: NULL engine reads getOption('soilKey.diagnostic_engine')", {
  pr <- .make_borderline_latossolo(cec_top = 18, cec_sub = 18)
  withr::with_options(
    list(soilKey.diagnostic_engine = "aqp"),
    {
      res <- ferralic(pr)  # engine = NULL -> reads option
      expect_true(isTRUE(res$passed))
    }
  )
  withr::with_options(
    list(soilKey.diagnostic_engine = "soilkey"),
    {
      res <- ferralic(pr)
      expect_false(isTRUE(res$passed))
    }
  )
})
