# v0.9.370: the attributes, the shared readers and the regimes of the
# Chinese Soil Taxonomy (2001, English edition), system id cst2001.

.cst_p <- function(h, site = list()) {
  PedonRecord$new(site = c(list(id = "cst"), site), horizons = h)
}

.cst_h <- function() {
  data.frame(top_cm = c(0, 20, 50, 90), bottom_cm = c(20, 50, 90, 120),
             designation = c("Ap", "AB", "Bt", "R"),
             clay_pct = c(15, 22, 35, NA), silt_pct = c(50, 45, 30, NA),
             sand_pct = c(35, 33, 35, NA), oc_pct = c(2, 1, 0.5, 0),
             bulk_density_g_cm3 = c(NA, 1.35, NA, NA),
             cec_cmol = c(10, 9, 8, NA),
             munsell_hue_moist = c("10YR", NA, "5YR", NA),
             munsell_hue_dry = c("10YR", "7.5YR", NA, NA))
}

test_that("the new attributes and regime values are documented", {
  skip_on_cran()
  a <- horizon_attribute_spec()
  hz <- c("fine_clay_pct", "clay_films_thickness_mm", "reduced_matrix_pct",
          "ferricyanide_test", "eh_mv", "p_olsen_mg_kg", "ec_1to1_dS_m",
          "shajiang_pct", "pyroclastic_fragments_pct", "soluble_salts_sodium_pct")
  expect_true(all(hz %in% a$name))
  expect_true(all(a$section[a$name %in% hz] == "Chinese system (CST 2001)"))
  s <- site_attribute_spec()
  st <- c("aridity_index", "aridity_index_max_month", "climatic_moisture_regime",
          "udic_perudic_border", "mean_summer_soil_temp_C", "summer_saturated",
          "cracks_open_days", "irrigated", "freeze_thaw_days_pct",
          "surface_crust_thickness_cm", "silting_rate_cm_yr", "silting_kind",
          "paddy_years_frac", "lithic_contact_extent_pct")
  expect_true(all(st %in% s$name))
  smr <- strsplit(s$allowed[s$name == "soil_moisture_regime"], ", ")[[1]]
  expect_true(all(c("stagnic", "anthrostagnic", "peraquic") %in% smr))
  str <- strsplit(s$allowed[s$name == "soil_temperature_regime"], ", ")[[1]]
  expect_true("permagelic" %in% str)
  # a profile with the new site properties raises no unrecognised warning
  expect_silent(.cst_p(.cst_h(), list(aridity_index = 1.2, irrigated = TRUE)))
})

test_that("thresholds.yaml holds the book's numbers", {
  skip_on_cran()
  expect_equal(soilKey:::.cst_thr("units", "fe2o3_to_fe"), 0.699)
  expect_equal(soilKey:::.cst_thr("mixed_surface", "depth_cm"), 18)
  expect_equal(soilKey:::.cst_thr("storage", "bulk_density_default", "clay_loamy_or_clayey", "B"), 1.5)
  expect_equal(soilKey:::.cst_thr("moisture_regimes", "penman", "factor_may_aug"), 0.8)
  expect_equal(soilKey:::.cst_thr("temperature_regimes", "cryic", "summer_max_exclusive_C",
                                  "saturated_with_o"), 6)
  expect_error(soilKey:::.cst_thr("units", "nope"), "no entry")
  r <- yaml::read_yaml(system.file("rules", "cst2001", "readings.yaml", package = "soilKey"))
  expect_true(length(r$readings) >= 20L)
  expect_true(all(vapply(r$readings, function(x) all(c("topic", "page", "reading") %in% names(x)), TRUE)))
})

test_that("units, clay activity and free iron", {
  skip_on_cran()
  expect_equal(soilKey:::.cst_gkg(1.4), 14)
  expect_equal(soilKey:::.cst_pct(120), 12)
  h <- data.frame(clay_pct = c(50, 0, NA), cec_cmol = c(8, 5, 5),
                  ca_cmol = c(1, NA, 1), mg_cmol = c(0.5, NA, 1), k_cmol = c(0.2, NA, 1),
                  na_cmol = c(0.1, NA, 1), al_kcl_cmol = c(1.2, NA, NA),
                  ecec_cmol = c(NA, 2, NA), cec_method = c("acetate_ph7", NA, NA),
                  fe_dcb_pct = c(1.4, 2, NA), fe_total_pct = c(4, NA, 3))
  cec <- soilKey:::.cst_clay_cec(h)
  expect_equal(cec$value, c(16, NA, NA))
  expect_true(all(is.na(cec$flag)))
  h2 <- h; h2$cec_method <- c("cobaltihexammine", NA, NA)
  expect_equal(soilKey:::.cst_clay_cec(h2)$flag[1], "cec_method_substitute")
  ec <- soilKey:::.cst_clay_ecec(h)
  expect_equal(ec$value[1], 3 * 100 / 50)
  expect_equal(ec$source[1], "bases_plus_al")
  expect_true(is.na(ec$value[2]))
  expect_equal(soilKey:::.cst_fe_free_total(h)[1], 1.4 / (4 * 0.699))
  expect_true(is.na(soilKey:::.cst_fe_free_total(h)[2]))
})

test_that("texture classes, groups and particle-size classes", {
  skip_on_cran()
  h <- data.frame(clay_pct = c(5, 15, 30, 45, 22, 10), silt_pct = c(5, 50, 30, 30, NA, NA),
                  sand_pct = c(90, 35, 40, 25, NA, NA),
                  coarse_fragments_pct = c(NA, 40, 0, 0, 0, 95),
                  fine_sand_pct = c(30, NA, 20, NA, NA, NA))
  expect_equal(soilKey:::.cst_texture_group(h),
               c("loamy_or_sandier", "loamy_or_sandier", "clay_loamy_or_clayey",
                 "clay_loamy_or_clayey", NA, "loamy_or_sandier"))
  expect_equal(soilKey:::.cst_vfs_or_finer(h)[1:4], c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(soilKey:::.cst_psc(h)[1:4], c("sandy", "loamy-skeletal", "fine-loamy", "fine"))
  expect_equal(soilKey:::.cst_psc(h)[6], "fragmental")
})

test_that("hue ranks: moist first, dry as a flagged fallback", {
  skip_on_cran()
  expect_equal(soilKey:::.cst_hue_rank(c("10R", "2.5YR", "5YR", "7.5YR", "5Y", "N", NA)),
               c(10, 12.5, 15, 17.5, 25, NA, NA))
  hu <- soilKey:::.cst_hue(.cst_h())
  expect_equal(hu$rank, c(20, 17.5, 15, NA))
  expect_equal(hu$state, c("moist", "dry", "moist", NA))
  expect_equal(hu$flag[2], "hue_dry_for_moist")
  expect_equal(soilKey:::.cst_hue(.cst_h(), "dry")$rank, c(20, 17.5, NA, NA))
})

test_that("designations, surfaces, contacts and the solum", {
  skip_on_cran()
  expect_equal(soilKey:::.cst_horizon_kind(c("Ap", "AB", "BA", "2Bt", "BC", "E", "Oi", "R", "Cr", "C", NA)),
               c("A", "AB", "AB", "B", "BC", "E", "O", "R", "C", "C", NA))
  expect_equal(soilKey:::.cst_horizon_kind(c("Cr", "2Cr"), cr_contact = TRUE), c("R", "R"))
  h <- rbind(data.frame(top_cm = -5, bottom_cm = 0, designation = "Oe"),
             data.frame(top_cm = c(0, 25), bottom_cm = c(25, 60), designation = c("A", "Bw")),
             data.frame(top_cm = 60, bottom_cm = 80, designation = "C"))
  p <- .cst_p(h)
  expect_equal(soilKey:::.cst_surface_cm(p, "surface"), -5)
  expect_equal(soilKey:::.cst_surface_cm(p, "mineral"), 0)
  expect_equal(soilKey:::.cst_solum(p)$value, 60)
  expect_true(is.na(soilKey:::.cst_contact_cm(p)))
  expect_equal(soilKey:::.cst_contact_cm(.cst_p(.cst_h())), 90)
  nod <- .cst_p(data.frame(top_cm = 0, bottom_cm = 20, oc_pct = 1))
  expect_true(is.na(soilKey:::.cst_solum(nod)$value))
  expect_true("designation" %in% soilKey:::.cst_solum(nod)$missing)
})

test_that("weighted means, the mixed surface and windows", {
  skip_on_cran()
  p <- .cst_p(.cst_h())
  m <- soilKey:::.cst_mixed_surface(p, "oc_pct")
  expect_equal(m$value, 2)
  w <- soilKey:::.cst_weighted(p, "oc_pct", 0, 50)
  expect_equal(w$value, (2 * 20 + 1 * 30) / 50)
  expect_true(is.na(soilKey:::.cst_weighted(p, "clay_pct", 0, 100)$value))
  expect_true(is.na(soilKey:::.cst_weighted(p, "oc_pct", 0, 150)$value))
  expect_equal(soilKey:::.cst_window_every(p, c(TRUE, TRUE, NA, TRUE), 0, 50)$value, TRUE)
  expect_true(is.na(soilKey:::.cst_window_every(p, c(TRUE, TRUE, NA, TRUE), 0, 60)$value))
  expect_false(soilKey:::.cst_window_every(p, c(TRUE, FALSE, NA, TRUE), 0, 60)$value)
  sh <- soilKey:::.cst_window_share(p, c(TRUE, NA, FALSE, FALSE), 0, 100)
  expect_equal(unname(sh$value), c(0.2, 0.5))
  expect_false(soilKey:::.cst_share_at_least(sh, 0.5, strict = TRUE))
  expect_true(is.na(soilKey:::.cst_share_at_least(sh, 0.5)))
  expect_true(soilKey:::.cst_share_at_least(sh, 0.2))
})

test_that("storage with the book's default bulk densities", {
  skip_on_cran()
  p <- .cst_p(.cst_h())
  s <- soilKey:::.cst_storage(p, to = 100, what = "oc")
  # Ap SIL 1.2, AB measured 1.35, Bt CL 1.5; the window ends at R (90 cm)
  v <- 0.2 * 1200 * 0.02 + 0.3 * 1350 * 0.01 + 0.4 * 1500 * 0.005
  expect_equal(unname(s$value), c(v, v))
  expect_equal(s$flags, "bd_default_cst")
  expect_equal(s$bd_default_rows, c(1L, 3L))
  expect_equal(s$contact_cm, 90)
  om <- soilKey:::.cst_storage(p, to = 20, what = "om")
  expect_equal(unname(om$value), rep(0.2 * 1200 * 0.02 * 1.724, 2))
  # a horizon with carbon and no density or kind stays undecided
  h <- .cst_h(); h$designation[3] <- NA
  u <- soilKey:::.cst_storage(.cst_p(h), to = 100, what = "oc")
  expect_true(is.na(u$value))
  expect_true("bulk_density_g_cm3" %in% u$missing)
  # contact at 90 cm scales the isohumic depths (p. 58 footnote)
  expect_equal(unname(soilKey:::.cst_storage_depths(p)), c(18, 90))
  h2 <- .cst_h()[1:3, ]
  expect_equal(unname(soilKey:::.cst_storage_depths(.cst_p(h2))), c(20, 100))
})

test_that("paddy, irrigation and the arid region", {
  skip_on_cran()
  expect_true(soilKey:::.cst_paddy(list(land_use = "paddy rice"))$value)
  expect_false(soilKey:::.cst_paddy(list(land_use = "upland rice"))$value)
  expect_true(soilKey:::.cst_paddy(list(flood_irrigated = TRUE))$value)
  expect_true(soilKey:::.cst_paddy(list(land_use = "\u6c34\u7530"))$value)
  expect_true(is.na(soilKey:::.cst_paddy(list())$value))
  expect_true(soilKey:::.cst_irrigated(list(flood_irrigated = TRUE))$value)
  expect_true(is.na(soilKey:::.cst_irrigated(list())$value))
  ar <- function(site) soilKey:::.cst_arid_region(.cst_p(.cst_h(), site))
  expect_true(ar(list(climatic_moisture_regime = "aridic", aridity_index = 0.5))$value)
  expect_equal(ar(list(climatic_moisture_regime = "aridic"))$source, "climatic_moisture_regime")
  expect_true(ar(list(aridity_index = 3.2))$value)
  expect_false(ar(list(aridity_index = 3))$value)
  expect_true(ar(list(soil_moisture_regime = "torric"))$value)
  expect_false(ar(list(soil_moisture_regime = "udic"))$value)
  expect_true(is.na(ar(list(soil_moisture_regime = "aquic"))$value))
  expect_true(is.na(ar(list())$value))
})

test_that("Penman's aridity", {
  skip_on_cran()
  a <- soilKey:::.cst_penman_aridity(rep(100, 12), rep(50, 12))
  f <- c(0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.8, 0.8, 0.7, 0.7, 0.6, 0.6)
  expect_equal(a$annual, sum(f * 100) / 600)
  expect_equal(a$max_month, 1.6)
  expect_true(is.na(soilKey:::.cst_penman_aridity(c(NA, rep(1, 11)), rep(1, 12))$annual))
  expect_error(soilKey:::.cst_penman_aridity(1:3, 1:3), "twelve")
})

test_that("the moisture regime and its source", {
  skip_on_cran()
  mr <- function(site, reg) soilKey:::moisture_regime_cst(.cst_p(.cst_h(), site), reg)
  r <- mr(list(soil_moisture_regime = "Ustic"), "ustic")
  expect_true(r$passed); expect_equal(r$evidence$source, "recorded")
  expect_true(mr(list(soil_moisture_regime = "torric"), "aridic")$passed)
  expect_true(mr(list(soil_moisture_regime = "peraquic"), "aquic")$passed)
  expect_false(mr(list(soil_moisture_regime = "aquic"), "peraquic")$passed)
  # aridity index
  r <- mr(list(aridity_index = 3.2), "aridic")
  expect_true(r$passed); expect_equal(r$evidence$source, "aridity_index")
  expect_match(r$notes, "regime_from_aridity_index")
  expect_true(mr(list(aridity_index = 3), "ustic")$passed)
  expect_true(mr(list(aridity_index = 1), "ustic")$passed)
  r <- mr(list(aridity_index = 0.8), "udic")
  expect_true(is.na(r$passed)); expect_true("aridity_index_max_month" %in% r$missing)
  expect_true(mr(list(aridity_index = 0.8), c("udic", "perudic"))$passed)
  expect_true(mr(list(aridity_index = 0.8, aridity_index_max_month = 0.9), "perudic")$passed)
  expect_true(mr(list(aridity_index = 0.8, aridity_index_max_month = 1.4), "udic")$passed)
  # paddy records (Q12) before the index
  r <- mr(list(flood_irrigated = TRUE, aridity_index = 0.8), "anthrostagnic")
  expect_true(r$passed); expect_equal(r$evidence$source, "site_marker")
  expect_true(mr(list(flood_irrigated = TRUE, aridity_index = 0.8), c("udic", "perudic"))$passed)
  # site markers (Q5)
  r <- mr(list(groundwater_kind = "perched"), "stagnic")
  expect_true(r$passed); expect_equal(r$evidence$flags, "stagnic_from_perched_water")
  h <- .cst_h(); h$mottle_colors <- c(NA, "7.5YR 5/6", NA, NA)
  r <- soilKey:::moisture_regime_cst(.cst_p(h, list(groundwater_depth_cm = 70)), "aquic")
  expect_true(r$passed); expect_equal(r$evidence$flags, "aquic_from_water_table")
  expect_true(is.na(soilKey:::moisture_regime_cst(.cst_p(h, list(groundwater_depth_cm = 70)), "peraquic")$passed))
  expect_true(soilKey:::moisture_regime_cst(.cst_p(h, list(groundwater_depth_cm = 100)), "aquic")$passed)
  expect_true(is.na(soilKey:::moisture_regime_cst(.cst_p(h, list(groundwater_depth_cm = 101)), "aquic")$passed))
  expect_true(is.na(soilKey:::moisture_regime_cst(.cst_p(.cst_h(), list(groundwater_depth_cm = 70)), "aquic")$passed))
  # nothing recorded
  r <- mr(list(), "udic")
  expect_true(is.na(r$passed)); expect_true("soil_moisture_regime" %in% r$missing)
  # xeric has no CST counterpart: read from the other records
  r <- mr(list(soil_moisture_regime = "xeric", aridity_index = 2), "ustic")
  expect_true(r$passed); expect_match(r$notes, "xeric")
})

test_that("the temperature regime and its source", {
  skip_on_cran()
  tr <- function(site, reg, ...) soilKey:::temperature_regime_cst(.cst_p(.cst_h(), site), reg, ...)
  r <- tr(list(soil_temperature_regime = "isomesic"), "mesic")
  expect_true(r$passed); expect_equal(r$evidence$source, "recorded")
  expect_true(tr(list(soil_temperature_regime = "pergelic"), "gelic")$passed)
  expect_true(tr(list(soil_temperature_regime = "permagelic"), "gelic")$passed)
  expect_false(tr(list(soil_temperature_regime = "gelic"), "cryic")$passed)
  expect_true(tr(list(soil_temperature_regime = "gelic"), "cryic", or_colder = TRUE)$passed)
  expect_false(tr(list(soil_temperature_regime = "frigid"), "cryic", or_colder = TRUE)$passed)
  # measured soil temperature
  r <- tr(list(mean_annual_soil_temp_C = 16), "thermic")
  expect_true(r$passed); expect_equal(r$evidence$source, "mean_annual_soil_temp_C")
  expect_true(tr(list(mean_annual_soil_temp_C = 8), "mesic")$passed)
  expect_true(tr(list(mean_annual_soil_temp_C = 22), "hyperthermic")$passed)
  # the air + 1 C (Q6), flagged
  r <- tr(list(mean_annual_air_temp_C = 14.5), "thermic")
  expect_true(r$passed); expect_equal(r$evidence$source, "mean_annual_air_temp_C")
  expect_equal(r$evidence$flags, "mast_from_air_temperature")
  # cryic against frigid
  r <- tr(list(mean_annual_soil_temp_C = 5), "cryic")
  expect_true(is.na(r$passed)); expect_true("mean_summer_soil_temp_C" %in% r$missing)
  expect_true(tr(list(mean_annual_soil_temp_C = 5, mean_summer_soil_temp_C = 5), "cryic")$passed)
  expect_true(tr(list(mean_annual_soil_temp_C = 5, mean_summer_soil_temp_C = 16), "frigid")$passed)
  expect_true(is.na(tr(list(mean_annual_soil_temp_C = 5, mean_summer_soil_temp_C = 14), "cryic")$passed))
  expect_true(tr(list(mean_annual_soil_temp_C = 5, mean_summer_soil_temp_C = 14,
                      summer_saturated = FALSE), "cryic")$passed)
  expect_true(tr(list(mean_annual_soil_temp_C = 5, mean_summer_soil_temp_C = 14,
                      summer_saturated = TRUE), "frigid")$passed)
  expect_true(is.na(tr(list(mean_annual_soil_temp_C = 5, mean_summer_soil_temp_C = 5), "cryic",
                       organic = TRUE)$passed))
  # gelic includes permagelic; permafrost within 50 cm makes it permagelic
  r <- tr(list(mean_annual_soil_temp_C = -2), "gelic")
  expect_true(r$passed); expect_true(is.na(tr(list(mean_annual_soil_temp_C = -2), "permagelic")$passed))
  h <- .cst_h(); h$permafrost_temp_C <- c(NA, -1, NA, NA)
  r <- soilKey:::temperature_regime_cst(.cst_p(h, list(soil_temperature_regime = "gelic")), "permagelic")
  expect_true(r$passed); expect_equal(r$evidence$flags, "permagelic_from_permafrost")
  r <- soilKey:::temperature_regime_cst(.cst_p(h), "permagelic")
  expect_true(r$passed); expect_equal(r$evidence$source, "permafrost")
  expect_false(tr(list(mean_annual_soil_temp_C = -2, frost_regime = "seasonal"), "permagelic")$passed)
  # nothing recorded
  r <- tr(list(), "mesic")
  expect_true(is.na(r$passed)); expect_true("mean_annual_air_temp_C" %in% r$missing)
})

test_that("udic bordering on perudic (Q8)", {
  skip_on_cran()
  b <- function(site) soilKey:::.cst_udic_perudic_border(.cst_p(.cst_h(), site))$value
  expect_true(b(list(udic_perudic_border = TRUE)))
  expect_true(b(list(soil_moisture_regime = "perudic")))
  expect_false(b(list(udic_perudic_border = FALSE, soil_moisture_regime = "udic")))
  expect_false(b(list(soil_moisture_regime = "ustic")))
  expect_true(is.na(b(list(soil_moisture_regime = "udic"))))
  expect_true(is.na(b(list())))
})

test_that("the USDA key reads the new regime values as aquic and gelic", {
  skip_on_cran()
  p <- function(site) .cst_p(.cst_h(), site)
  for (v in c("stagnic", "anthrostagnic", "peraquic")) {
    expect_true(soilKey:::soil_moisture_regime_usda(p(list(soil_moisture_regime = v)), "aquic")$passed)
    expect_false(soilKey:::smr_udic_usda(p(list(soil_moisture_regime = v)))$passed)
  }
  r <- soilKey:::soil_temperature_regime_usda(p(list(soil_temperature_regime = "permagelic")), "gelic")
  expect_true(r$passed)
  expect_equal(r$evidence$soil_temperature_regime, "permagelic")
  expect_false(soilKey:::str_cryic_usda(p(list(soil_temperature_regime = "permagelic")))$passed)
  expect_equal(soilKey:::family_temperature_regime_usda(
    p(list(soil_temperature_regime = "permagelic")))$value, "gelic")
  # values recorded before read as they did
  expect_equal(soilKey:::family_temperature_regime_usda(
    p(list(soil_temperature_regime = "Mesic")))$value, "mesic")
  expect_true(soilKey:::smr_aridic_usda(p(list(soil_moisture_regime = "Torric")))$passed)
  expect_equal(soilKey:::smr_aridic_usda(p(list(soil_moisture_regime = "Torric")))$evidence$soil_moisture_regime,
               "Torric")
})


# --- the review of 2026-11-05: Hugo's decisions D1-D5, Q1-Q4 ------------------------

test_that("D1: the climatic and the saturation axes", {
  skip_on_cran()
  mr <- function(site, reg, h = .cst_h()) soilKey:::moisture_regime_cst(.cst_p(h, site), reg)
  # a paddy recorded udic, flood irrigated: anthrostagnic, and still udic
  site <- list(soil_moisture_regime = "udic", flood_irrigated = TRUE)
  r <- mr(site, "anthrostagnic")
  expect_true(r$passed); expect_equal(r$evidence$source, "site_marker")
  expect_true(mr(site, "udic")$passed)
  expect_false(mr(site, "ustic")$passed)
  # paddy land use with aridity 0.6: udic, and the perudic border not FALSE
  h <- data.frame(top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 150), designation = c("Ap", "AB", "Bs"))
  r <- mr(list(land_use = "paddy", aridity_index = 0.6, aridity_index_max_month = 1.4), "udic", h)
  expect_true(r$passed); expect_equal(r$evidence$source, "aridity_index")
  b <- soilKey:::.cst_udic_perudic_border(.cst_p(h, list(land_use = "paddy", aridity_index = 0.6,
                                                           aridity_index_max_month = 0.8)))
  expect_true(b$value)
  expect_true(is.na(soilKey:::.cst_udic_perudic_border(.cst_p(h, list(land_use = "paddy",
                                                                     aridity_index = 0.6)))$value))
  # a saline soil, aridity 8, water table at 80 cm, mottles: aquic and aridic
  hs <- data.frame(top_cm = c(0, 2, 30, 70), bottom_cm = c(2, 30, 70, 120),
                   designation = c("Az", "Bz", "Bzg", "Cg"),
                   mottle_colors = c(NA, NA, "10YR 5/6", "5Y 6/1"))
  site <- list(aridity_index = 8, groundwater_depth_cm = 80, groundwater_kind = "observed")
  expect_true(mr(site, "aquic", hs)$passed)
  expect_true(mr(site, "aridic", hs)$passed)
  expect_true(soilKey:::.cst_arid_region(.cst_p(hs, site))$value)
  # recorded aquic with aridity 0.5: the border is not FALSE
  b <- soilKey:::.cst_udic_perudic_border(.cst_p(h, list(soil_moisture_regime = "aquic", aridity_index = 0.5)))
  expect_true(is.na(b$value))
  # a saturation regime alone says nothing of the border or the climate
  p <- .cst_p(h, list(soil_moisture_regime = "aquic"))
  expect_true(is.na(soilKey:::.cst_udic_perudic_border(p)$value))
  expect_true(is.na(soilKey:::moisture_regime_cst(p, "udic")$passed))
  expect_true(is.na(soilKey:::moisture_regime_cst(.cst_p(h, list(soil_moisture_regime = "udic")), "aquic")$passed))
  expect_false(soilKey:::moisture_regime_cst(p, "stagnic")$passed)
  # the regional regime on the climatic axis; a test across both axes
  r <- mr(list(climatic_moisture_regime = "ustic"), "ustic")
  expect_true(r$passed); expect_equal(r$evidence$source, "climatic_moisture_regime")
  expect_true(mr(list(soil_moisture_regime = "stagnic"), c("ustic", "stagnic"))$passed)
  # the border recorded TRUE against an aridic regime fails, noted
  b <- soilKey:::.cst_udic_perudic_border(.cst_p(h, list(udic_perudic_border = TRUE, soil_moisture_regime = "aridic")))
  expect_false(b$value); expect_match(b$note, "against the aridic")
})

test_that("D2: peraquic from tidal flooding or surface water", {
  skip_on_cran()
  mr <- function(site, reg) soilKey:::moisture_regime_cst(.cst_p(.cst_h(), site), reg)
  r <- mr(list(tidal_zone = "intertidal", groundwater_depth_cm = 0), "peraquic")
  expect_true(r$passed); expect_equal(r$evidence$flags, "peraquic_from_tidal_or_surface_water")
  expect_true(mr(list(tidal_inundation = TRUE), "aquic")$passed)
  expect_true(mr(list(water_table_cm_above_surface = 5), "peraquic")$passed)
  expect_true(is.na(mr(list(tidal_zone = "supratidal"), "peraquic")$passed))
})

test_that("D3: permafrost and the soil temperature at 50 cm", {
  skip_on_cran()
  tr <- function(h, site, reg, ...) soilKey:::temperature_regime_cst(.cst_p(h, site), reg, ...)
  h <- data.frame(top_cm = c(0, 8, 25, 60), bottom_cm = c(8, 25, 60, 100),
                  designation = c("Ao", "A", "Bwf", "Cf"))
  # frozen at 25 cm with an air estimate above 0: permagelic ahead of the air
  r <- tr(h, list(mean_annual_air_temp_C = -0.5), "permagelic")
  expect_true(r$passed); expect_equal(r$evidence$source, "permafrost")
  # against a measured soil temperature above 0: a flagged conflict
  r <- tr(h, list(mean_annual_soil_temp_C = 2), "permagelic")
  expect_true(is.na(r$passed)); expect_equal(r$evidence$flags, "permafrost_soil_temperature_conflict")
  # a recorded cryic regime is kept with a note
  r <- tr(h, list(soil_temperature_regime = "cryic"), "cryic")
  expect_true(r$passed); expect_match(r$notes, "permafrost within 50 cm")
  # the permafrost temperature of the horizon at 50 cm is the soil temperature
  h2 <- data.frame(top_cm = c(0, 25), bottom_cm = c(25, 100), designation = c("A", "Bw"),
                   permafrost_temp_C = c(NA, 3))
  r <- tr(h2, list(mean_annual_air_temp_C = -3, mean_summer_soil_temp_C = 16), "frigid")
  expect_true(r$passed); expect_equal(r$evidence$source, "permafrost_temp_C")
  expect_equal(r$evidence$mast, 3)
  h3 <- data.frame(top_cm = c(0, 25), bottom_cm = c(25, 100), designation = c("A", "Bw"),
                   permafrost_temp_C = c(NA, -1))
  r <- tr(h3, list(), "permagelic")
  expect_true(r$passed); expect_equal(r$evidence$source, "permafrost_temp_C")
})

test_that("D4: the mineral surface of undesignated horizons", {
  skip_on_cran()
  ms <- function(oc, clay) soilKey:::.cst_surface_cm(
    .cst_p(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = NA,
                      oc_pct = c(oc, 1), clay_pct = c(clay, 20))), "mineral")
  expect_equal(ms(11, NA), 0)
  expect_equal(ms(25, NA), 20)
  expect_equal(ms(15, 0), 20)
  expect_equal(ms(15, 40), 0)
  expect_equal(ms(19, 70), 20)
  expect_true(is.na(ms(15, NA)))
  expect_true(is.na(ms(NA, 10)))
})

test_that("D5: paddy words", {
  skip_on_cran()
  pd <- function(lu) soilKey:::.cst_paddy(list(land_use = lu))$value
  expect_true(pd("paddy (rice-wheat rotation)"))
  expect_true(pd("sawah"))
  expect_true(pd("arroz irrigado"))
  expect_false(pd("rice straw mulch orchard"))
  expect_false(pd("rainfed rice"))
  expect_false(pd("dryland rice"))
  expect_false(pd("price-supported maize"))
  expect_true(pd("\u6c34\u7a3b"))
  expect_false(pd("\u65f1\u7a3b"))
})

test_that("Q1: loamy against silty for the loess", {
  skip_on_cran()
  h <- data.frame(clay_pct = c(20, 20, 20, 10), silt_pct = c(62, 62, 62, 80), sand_pct = c(18, 18, 18, 10),
                  fine_sand_pct = c(NA, 10, 1, NA))
  expect_equal(soilKey:::.cst_psc(h), c(NA, NA, "fine-loamy", "coarse-silty"))
  h$coarse_fragments_pct <- c(40, 0, 0, 0)
  expect_equal(soilKey:::.cst_psc(h)[1], "loamy-skeletal")
})

test_that("Q2: Cr is a paralithic contact only with a contact recorded or an FAO notation", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 15, 22, 50, 80), bottom_cm = c(15, 22, 50, 80, 120),
                  designation = c("Ap1", "Ap2", "Br1", "Br2", "Cr"))
  expect_true(is.na(soilKey:::.cst_contact_cm(.cst_p(h))))
  expect_equal(soilKey:::.cst_kinds(.cst_p(h))[5], "C")
  expect_equal(soilKey:::.cst_contact_cm(.cst_p(h, list(designation_system = "fao"))), 80)
  h$cementation_class <- c(NA, NA, NA, NA, "moderately")
  expect_equal(soilKey:::.cst_contact_cm(.cst_p(h)), 80)
  expect_equal(soilKey:::.cst_horizon_kind(c("P", "W", "G", "Gw", "R")), c("A", "B", "B", "B", "R"))
})

test_that("Q3: an O horizon takes a bulk-density range; the storage decides by its range", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 3, 15), bottom_cm = c(3, 15, 100), designation = c("Oi", "A", "Bs"),
                  clay_pct = c(NA, 40, 50), silt_pct = c(NA, 25, 20), sand_pct = c(NA, 35, 30),
                  oc_pct = c(30, 3, 0.6))
  s <- soilKey:::.cst_storage(.cst_p(h), 100, what = "oc")
  base <- 0.12 * 1300 * 0.03 + 0.85 * 1500 * 0.006
  expect_equal(unname(s$value), c(0.03 * 100 * 0.3 + base, 0.03 * 400 * 0.3 + base))
  expect_true("bd_organic_range_cst" %in% s$flags)
  expect_true(soilKey:::.cst_storage_at_least(s, 12))
  expect_false(soilKey:::.cst_storage_at_least(s, 20))
  expect_true(is.na(soilKey:::.cst_storage_at_least(s, unname(s$value[1]) + 0.5)))
  # an E with carbon and no density: missing the density, not the designation
  h2 <- data.frame(top_cm = c(0, 10), bottom_cm = c(10, 100), designation = c("E", "Bt"),
                   clay_pct = c(10, 30), silt_pct = c(40, 30), sand_pct = c(50, 40), oc_pct = c(1, 0.3))
  u <- soilKey:::.cst_storage(.cst_p(h2), 100, what = "oc")
  expect_true(is.na(u$value))
  expect_equal(u$missing, "bulk_density_g_cm3")
})

test_that("Q4: the solum stands for a contact between 50 and 100 cm", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 60, 75), bottom_cm = c(20, 60, 75, 90),
                  designation = c("A", "Bw", "C", "R"),
                  clay_pct = c(20, 25, 20, NA), silt_pct = 40, sand_pct = c(40, 35, 40, NA),
                  oc_pct = c(2, 0.8, 0.2, NA))
  p <- .cst_p(h)
  expect_equal(unname(soilKey:::.cst_storage_depths(p)), c(12, 60))
  s <- soilKey:::.cst_storage(p, 100, what = "oc")
  v <- 0.2 * 1200 * 0.02 + 0.4 * 1400 * 0.008
  expect_equal(unname(s$value), c(v, v))
  expect_match(s$note, "solum")
})
