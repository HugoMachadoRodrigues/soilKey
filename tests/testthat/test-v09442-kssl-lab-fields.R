# =============================================================================
# v0.9.442 -- the KSSL GeoPackage loader reads salinity, gypsum and the pH 8.2
# exchange complex, and fills NH4OAc base saturation where the lab data
# settle it (.kssl_lab_horizons()).
# =============================================================================

kssl_layers <- function(...) {
  base <- data.table::data.table(
    hzn_top = c(0, 20, 50), hzn_bot = c(20, 50, 100),
    hzn_desgn = c("Ap", "Bt", "Bk"),
    clay_total = c(20, 35, 30), silt_total = c(40, 30, 30), sand_total = c(40, 35, 40),
    cec_nh4_ph_7 = c(20, 25, 15),
    ca_nh4_ph_7 = c(10, 12, NA), mg_nh4_ph_7 = c(3, 4, NA),
    k_nh4_ph_7 = c(0.5, 0.4, NA), na_nh4_ph_7 = c(0.5, 0.6, NA),
    base_sat_nh4oac_ph_7 = c(70, NA, NA),
    caco3_lt_2_mm = c(0, 0, 12),
    electrical_conductivity_satx = c(0.4, 1.2, 16),
    corrected_gypsum_lt_2_mm = c(NA, 0, 6),
    gypsum_lt_20_mm = c(NA, NA, 8),
    sum_of_cations_cec_pH_8_2 = c(24, 30, NA),
    acidity_bacl2_tea_ph_8_2 = c(10, 13, NA)
  )
  mods <- list(...)
  for (n in names(mods)) base[[n]] <- mods[[n]]
  base
}

test_that("v0.9.442: salinity, gypsum and pH 8.2 fields reach the horizons", {
  h <- soilKey:::.kssl_lab_horizons(kssl_layers())$horizons
  expect_equal(h$ec_dS_m, c(0.4, 1.2, 16))
  expect_equal(h$caso4_pct, c(NA, 0, 6))
  expect_equal(h$gypsum_lt20mm_pct, c(NA, NA, 8))
  expect_equal(h$cec_ph82_cmol, c(24, 30, NA))
  expect_equal(h$extractable_acidity_ph82_cmol, c(10, 13, NA))
})

test_that("v0.9.442: reported NH4OAc BS is kept; missing BS comes from the bases", {
  lh <- soilKey:::.kssl_lab_horizons(kssl_layers())
  expect_equal(lh$horizons$bs_pct[1], 70)                  # reported, untouched
  expect_equal(lh$horizons$bs_pct[2], 100 * 17 / 25)       # (12+4+0.4+0.6) / 25
  expect_true(is.na(lh$horizons$bs_pct[3]))                # left for the caller
  expect_equal(lh$bs_calcareous, c(FALSE, FALSE, TRUE))
})

test_that("v0.9.442: BS from bases is capped at 100", {
  h <- soilKey:::.kssl_lab_horizons(kssl_layers(
    ca_nh4_ph_7 = c(30, 12, NA), base_sat_nh4oac_ph_7 = c(NA, NA, NA)))$horizons
  expect_equal(h$bs_pct[1], 100)
})

test_that("v0.9.442: a non-calcareous layer with no bases keeps BS missing", {
  lh <- soilKey:::.kssl_lab_horizons(kssl_layers(
    mg_nh4_ph_7 = c(3, NA, NA), caco3_lt_2_mm = c(0, 0, 0)))
  expect_true(is.na(lh$horizons$bs_pct[2]))
  expect_false(any(lh$bs_calcareous))
})

test_that("v0.9.442: missing source columns give NA, not an error", {
  l <- kssl_layers()
  l$electrical_conductivity_satx <- NULL
  l$sum_of_cations_cec_pH_8_2 <- NULL
  h <- soilKey:::.kssl_lab_horizons(l)$horizons
  expect_true(all(is.na(h$ec_dS_m)))
  expect_true(all(is.na(h$cec_ph82_cmol)))
})

test_that("v0.9.442: the calcareous convention is recorded as inferred_prior", {
  lh <- soilKey:::.kssl_lab_horizons(kssl_layers())
  p <- PedonRecord$new(site = list(id = "t", lat = 40, lon = -100, country = "US"),
                       horizons = lh$horizons)
  soilKey:::.kssl_record_bs_calcareous(p, lh$bs_calcareous)
  expect_equal(p$horizons$bs_pct[3], 100)
  pr <- p$provenance[p$provenance$attribute == "bs_pct", ]
  expect_equal(nrow(pr), 1L)
  expect_equal(pr$source, "inferred_prior")
  expect_equal(pr$horizon_idx, 3L)
})
