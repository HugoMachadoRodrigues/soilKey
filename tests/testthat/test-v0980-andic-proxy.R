# =============================================================================
# Tests for v0.9.80 -- andic_properties() OC + BD proxy (opt-in).
# Field-described volcanic-ash soils often lack oxalate Al/Fe and
# phosphate retention. The proxy uses high SOC + low BD as a
# coarse-data substitute.
# =============================================================================


.andic_pedon_with_oxalate <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0, 30), bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    al_ox_pct = c(2.5, 1.5), fe_ox_pct = c(1.0, 0.5),
    bulk_density_g_cm3 = c(0.7, 0.8),
    clay_pct = c(20, 25), silt_pct = c(30, 30), sand_pct = c(50, 45),
    oc_pct = c(8, 4), ph_h2o = c(5.5, 5.7)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "andic-canonical"), horizons = hz)
}


.andic_pedon_proxy_only <- function(oc = c(8, 4), bd = c(0.85, 0.95)) {
  # No oxalate Al/Fe, no phosphate retention -- canonical paths fail.
  hz <- data.table::data.table(
    top_cm    = c(0, 30), bottom_cm = c(30, 100),
    designation = c("Ah", "Bw"),
    bulk_density_g_cm3 = bd,
    clay_pct = c(20, 25), silt_pct = c(30, 30), sand_pct = c(50, 45),
    oc_pct = oc, ph_h2o = c(5.5, 5.7),
    munsell_value_moist = c(3, 4), munsell_chroma_moist = c(2, 3)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "andic-proxy-only"), horizons = hz)
}


test_that("v0.9.80: andic_properties default (canonical) still requires oxalate or P-ret", {
  pr <- .andic_pedon_proxy_only()
  res <- andic_properties(pr)
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.80: andic_properties OC+BD proxy fires when opt-in (OC>=4 + BD<=0.9)", {
  pr <- .andic_pedon_proxy_only(oc = c(8, 4), bd = c(0.85, 0.95))
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    res <- andic_properties(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.80: andic OC+BD proxy DOES NOT fire on low-OC profiles even with low BD", {
  pr <- .andic_pedon_proxy_only(oc = c(1.5, 0.8), bd = c(0.85, 0.85))
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    res <- andic_properties(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.80: andic OC+BD proxy DOES NOT fire on high-OC profiles with high BD", {
  pr <- .andic_pedon_proxy_only(oc = c(8, 4), bd = c(1.3, 1.4))
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    res <- andic_properties(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.80: andic OC>=5 alone (BD missing) fires the proxy", {
  pr <- .andic_pedon_proxy_only(oc = c(7, 3), bd = c(NA_real_, NA_real_))
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    res <- andic_properties(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.80: canonical path STILL wins when oxalate data present", {
  pr <- .andic_pedon_with_oxalate()
  res <- andic_properties(pr)   # canonical, no opt-in
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.80: andic proxy evidence trace records the source", {
  pr <- .andic_pedon_proxy_only()
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    res <- andic_properties(pr)
    expect_identical(res$evidence$oc_bd_proxy$source, "high_oc_low_bd")
  })
})
