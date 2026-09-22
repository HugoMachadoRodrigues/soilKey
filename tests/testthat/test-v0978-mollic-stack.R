# =============================================================================
# Tests for v0.9.78 -- mollic() surface-anchored contiguous stack fix.
#
# v0.9.77 and earlier: candidate_layers <- which(top_cm <= 5).  This
# excluded contiguous A2 / AB layers below 5 cm that ARE part of the
# same morphological mollic horizon, so a profile with A1 (0-3) +
# A2 (3-15) + AB (15-40) of mollic-passing material had only 15 cm
# of "candidate" thickness instead of 40, failing the 20-cm minimum.
#
# v0.9.78: candidate_layers is the contiguous stack of mollic-color-
# passing layers starting at the surface.
# =============================================================================


.three_layer_dark_pedon <- function(values = c(2, 2, 2),
                                       chromas = c(1, 1, 1)) {
  hz <- data.table::data.table(
    top_cm    = c(0, 3, 15),
    bottom_cm = c(3, 15, 40),
    designation = c("A1", "A2", "AB"),
    munsell_hue_moist    = c("10YR","10YR","10YR"),
    munsell_value_moist  = values,
    munsell_chroma_moist = chromas,
    munsell_value_dry    = c(3, 3, 3),
    clay_pct = c(40, 41, 42),
    silt_pct = c(30, 31, 30),
    sand_pct = c(30, 28, 28),
    cec_cmol = c(20, 20, 20),
    bs_pct   = c(85, 85, 85),
    oc_pct   = c(2.0, 1.5, 0.8),
    ph_h2o   = c(7, 7.2, 7.5)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-mollic-stack"),
                   horizons = hz)
}


test_that("v0.9.78: mollic stack accumulates contiguous A1+A2+AB layers when each passes color", {
  pr <- .three_layer_dark_pedon()
  res <- mollic(pr)
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.78: mollic stack stops at first layer failing color (3+12=15cm < 20)", {
  # A1 (3 cm) + A2 (12 cm) = 15 cm of mollic-passing material;
  # AB has bright colour and fails color test -> stack stops at A2.
  # 15 cm < 20 cm threshold -> mollic FAILS.
  pr <- .three_layer_dark_pedon(values = c(2, 2, 5),
                                  chromas = c(1, 1, 4))
  res <- mollic(pr)
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.78: mollic fails when surface layer doesn't pass color", {
  pr <- .three_layer_dark_pedon(values = c(5, 5, 5))   # all bright
  res <- mollic(pr)
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.78: mollic stack handles small gap between contiguous layers (rounding)", {
  # Real soil data sometimes has 14.99 -> 15.0 type bottom_cm/top_cm
  hz <- data.table::data.table(
    top_cm    = c(0, 3.0, 15.0),
    bottom_cm = c(3.0, 15.0, 40),
    designation = c("A1","A2","AB"),
    munsell_hue_moist = rep("10YR", 3),
    munsell_value_moist = c(2,2,2),
    munsell_chroma_moist = c(1,1,1),
    clay_pct = c(40,41,42), silt_pct = c(30,31,30), sand_pct = c(30,28,28),
    cec_cmol = c(20,20,20), bs_pct = c(85,85,85),
    oc_pct = c(2,1.5,0.8), ph_h2o = c(7,7.2,7.5)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "test"), horizons = hz)
  res <- mollic(pr)
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.78: KE Phaeozem fixture (A11+A12=27cm) now passes mollic", {
  # Reproduces the real GeoTab_KE_W3_0289 case where A11+A12 = 27 cm
  # of mollic-passing material was failing because A12 (top_cm=10)
  # exceeded surface_top_cm=5 default.
  hz <- data.table::data.table(
    top_cm    = c(0, 10, 27, 40),
    bottom_cm = c(10, 27, 40, 60),
    designation = c("A11","A12","B1t","B21t"),
    munsell_hue_moist = rep("10YR", 4),
    munsell_value_moist = c(3, 3, 4, 4),
    munsell_chroma_moist = c(2, 2, 3, 6),
    clay_pct = c(28, 30, 43, 46), silt_pct = c(35,35,30,28),
    sand_pct = c(37,35,27,26),
    cec_cmol = c(13, 11, 13, 13), bs_pct = c(100,100,100,100),
    oc_pct = c(1.13, 0.61, 0.47, 0.44), ph_h2o = c(7.3,7.9,8.1,8.1)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "KE-Phaeozem-replica"),
                          horizons = hz)
  res <- mollic(pr)
  expect_true(isTRUE(res$passed))
  # The candidate-layer fix is what unblocks this case (was 0/5 in
  # AfSP Phaeozem; now passes mollic).
})
