# v0.9.417 (Hugo, 2026-11-20): the Andisols and the andic tests after the
# Soil Taxonomy audit (section 2.1, items A1 to A13; readings L1 to L4).

.v09417_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(
    site = utils::modifyList(list(id = "t417", lat = 0, lon = 0, country = "US",
                                  parent_material = "volcanic ash"), site),
    horizons = ensure_horizon_schema(hz))
}

test_that("andic soil properties: three-valued, phosphate retention required (A1, L1)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  # item 2 met
  p <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "A",
                     oc_pct = 6, bulk_density_g_cm3 = 0.7, al_ox_pct = 2.5,
                     fe_ox_pct = 1, phosphate_retention_pct = 90)
  expect_true(ns$andic_soil_properties_usda(p)$passed)
  # phosphate retention not measured: undecided, listed
  p2 <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "A",
                      oc_pct = 6, bulk_density_g_cm3 = 0.7, al_ox_pct = 2.5,
                      fe_ox_pct = 1)
  r <- ns$andic_soil_properties_usda(p2)
  expect_true(is.na(r$passed))
  expect_true("phosphate_retention_pct" %in% r$missing)
  # measured values that rule both items out: FALSE
  p3 <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "A",
                      oc_pct = 2, bulk_density_g_cm3 = 1.3, al_ox_pct = 0.1,
                      fe_ox_pct = 0.2)
  expect_false(ns$andic_soil_properties_usda(p3)$passed)
  # 25 % organic carbon or more is not andic (item 1, A2)
  p4 <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "Oa",
                      oc_pct = 30, bulk_density_g_cm3 = 0.3, al_ox_pct = 2.5,
                      fe_ox_pct = 1, phosphate_retention_pct = 95)
  expect_false(ns$andic_soil_properties_usda(p4)$passed)
})

test_that("pH NaF stands in for a missing bulk density (L2); item 3a and 3e (A2)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "A",
                     oc_pct = 6, al_ox_pct = 2.5, fe_ox_pct = 1,
                     phosphate_retention_pct = 90, ph_naf = 10.2)
  r <- ns$andic_soil_properties_usda(p)
  expect_true(r$passed)
  expect_match(r$notes, "pH NaF")
  # item 3: sand >= 30 % passes 3a; glass and Al + 1/2 Fe give 3e
  v <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "C",
                     oc_pct = 0.5, bulk_density_g_cm3 = 1.2, al_ox_pct = 0.6,
                     fe_ox_pct = 0.4, phosphate_retention_pct = 30,
                     volcanic_glass_pct = 30, sand_pct = 60, silt_pct = 30)
  expect_true(ns$andic_soil_properties_usda(v)$passed)
  # 3e fails: 15.625 * 0.8 + 6 < 36.25
  v2 <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "C",
                      oc_pct = 0.5, bulk_density_g_cm3 = 1.2, al_ox_pct = 0.6,
                      fe_ox_pct = 0.4, phosphate_retention_pct = 30,
                      volcanic_glass_pct = 6, sand_pct = 60, silt_pct = 30)
  expect_false(ns$andic_soil_properties_usda(v2)$passed)
  # 3a undecided with sand < 30 and no 0.02-2 mm fraction recorded
  v3 <- .v09417_pedon(top_cm = 0, bottom_cm = 30, designation = "C",
                      oc_pct = 0.5, bulk_density_g_cm3 = 1.2, al_ox_pct = 0.6,
                      fe_ox_pct = 0.4, phosphate_retention_pct = 30,
                      volcanic_glass_pct = 30, sand_pct = 20, silt_pct = 40)
  expect_true(is.na(ns$andic_soil_properties_usda(v3)$passed))
})

test_that("the Andisol window starts at the mineral surface and stops at a contact (A3)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  # an O layer above 30 cm of andic material on rock at 40 cm
  p <- .v09417_pedon(top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 60),
                     designation = c("Oi", "A", "R"),
                     oc_pct = c(40, 6, NA), bulk_density_g_cm3 = c(0.2, 0.7, NA),
                     al_ox_pct = c(NA, 2.5, NA), fe_ox_pct = c(NA, 1, NA),
                     phosphate_retention_pct = c(NA, 90, NA))
  r <- ns$andisol_qualifying_usda(p)
  expect_true(r$passed)
  expect_equal(r$evidence$window$top, 10)
  expect_equal(r$evidence$window$bottom, 40)
  # undecided layers that could carry the 60 %: NA
  q <- .v09417_pedon(top_cm = c(0, 20), bottom_cm = c(20, 80),
                     designation = c("A", "Bw"), oc_pct = c(6, 2),
                     bulk_density_g_cm3 = c(0.7, 0.8), al_ox_pct = c(2.5, 2.5),
                     fe_ox_pct = c(1, 1), phosphate_retention_pct = c(90, NA))
  expect_true(is.na(ns$andisol_qualifying_usda(q)$passed))
  expect_true(is.na(ns$andisol_usda(q)$passed))
})

test_that("Udands: Melanudands before Hydrudands; Hydrudands need undried 100 % (A4, A5, L4)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  rules <- load_rules("usda")
  gg <- vapply(rules$great_groups$DH, function(x) x$name, "")
  expect_identical(gg, c("Placudands", "Durudands", "Melanudands",
                         "Hydrudands", "Fulvudands", "Hapludands"))
  expect_identical(vapply(rules$great_groups$DH, function(x) x$code, "")[3:4],
                   c("DHC", "DHD"))
  r <- classify_usda(make_andosol_canonical(), on_missing = "silent")
  expect_identical(r$name, "Vitric Melanudands")
  # 1500 kPa on undried samples: weighted average over 35 cm
  p <- .v09417_pedon(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                     designation = c("A", "Bw1", "Bw2"),
                     water_content_1500kpa_undried = c(80, 120, 60))
  expect_true(ns$hydric_great_group_andisol_usda(p)$passed)
  p$horizons$water_content_1500kpa_undried <- c(80, 90, 60)
  expect_false(ns$hydric_great_group_andisol_usda(p)$passed)
  # a value of unknown sample state of 100 % or more is a lower bound of the
  # undried value, so it passes (reading N14); under 100 % it is undecided (L4)
  a <- .v09417_pedon(top_cm = c(0, 20), bottom_cm = c(20, 100),
                     designation = c("A", "Bw"),
                     water_content_1500kpa = c(150, 160))
  expect_true(ns$hydric_great_group_andisol_usda(a)$passed)
  a$horizons$water_content_1500kpa <- c(60, 70)
  r <- ns$hydric_great_group_andisol_usda(a)
  expect_true(is.na(r$passed))
  expect_identical(r$missing, "water_content_1500kpa_undried")
})

test_that("melanic epipedon: andic by p. 32, no designation needed, 30 in 40 cm (A6)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  base <- list(top_cm = c(0, 15, 35, 60), bottom_cm = c(15, 35, 60, 100),
               designation = c("Bw1", "Bw2", "Bw3", "C"),
               oc_pct = c(9, 7, 2, 1), munsell_value_moist = c(2, 2, 4, 5),
               munsell_chroma_moist = c(1, 2, 3, 4),
               bulk_density_g_cm3 = c(0.5, 0.6, 0.8, 1.0),
               al_ox_pct = c(3, 3, 2.5, 1), fe_ox_pct = c(1, 1, 1, 0.5),
               phosphate_retention_pct = c(95, 95, 90, 50))
  p <- do.call(.v09417_pedon, base)
  expect_true(ns$melanic_epipedon_usda(p)$passed)
  # Al + 1/2 Fe below 0.4: neither item 2 nor item 3, not melanic
  b2 <- base; b2$al_ox_pct <- c(0.2, 0.2, 0.2, 0.2); b2$fe_ox_pct <- c(0.1, 0.1, 0.1, 0.1)
  expect_false(ns$melanic_epipedon_usda(do.call(.v09417_pedon, b2))$passed)
  # colour not recorded: undecided
  b3 <- base; b3$munsell_value_moist <- c(NA, NA, 4, 5)
  expect_true(is.na(ns$melanic_epipedon_usda(do.call(.v09417_pedon, b3))$passed))
})

test_that("Fulv- great groups: melanic depth, thickness and carbon (A7)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09417_pedon(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                     designation = c("A1", "A2", "Bw"), oc_pct = c(8, 4.5, 2))
  expect_true(ns$fulvic_andisol_usda(p)$passed)          # 20*8 + 10*4.5 = 6.8 over 30
  p$horizons$oc_pct <- c(6.5, 3.5, 2)                     # a layer below 4 %
  expect_false(ns$fulvic_andisol_usda(p)$passed)
})

test_that("Thaptic needs lighter, poorer horizons above (A8)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  mk <- function(vm_top, oc_top) .v09417_pedon(
    top_cm = c(0, 30, 50), bottom_cm = c(30, 50, 100),
    designation = c("A", "Bw", "2Ab"), oc_pct = c(oc_top, 1, 5),
    munsell_value_moist = c(vm_top, 4, 2), munsell_chroma_moist = c(2, 4, 2),
    munsell_value_dry = c(4, 5, 3))
  expect_true(ns$thaptic_subgroup_usda(mk(4, 2))$passed)
  # the surface is as dark and as rich: only the Bw (20 cm) is lighter
  expect_true(ns$thaptic_subgroup_usda(mk(2, 6))$passed)
  q <- .v09417_pedon(top_cm = c(0, 30), bottom_cm = c(30, 100),
                     designation = c("A", "Bw"), oc_pct = c(8, 5),
                     munsell_value_moist = c(2, 2), munsell_chroma_moist = c(1, 2),
                     munsell_value_dry = c(3, 3))
  expect_false(ns$thaptic_subgroup_usda(q)$passed)
})

test_that("Dur- great groups: cemented horizon in 75 % of the pedon (A10)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09417_pedon(top_cm = c(0, 40), bottom_cm = c(40, 80),
                     designation = c("A", "Bqm"),
                     cementation_class = c(NA, "strongly"), duripan_pct = c(0, 95))
  expect_true(ns$duric_great_group_andisol_usda(p)$passed)
  p$horizons$duripan_pct <- c(0, 25)
  expect_false(ns$duric_great_group_andisol_usda(p)$passed)
  p$horizons$duripan_pct <- c(0, NA)
  expect_true(is.na(ns$duric_great_group_andisol_usda(p)$passed))
  rules <- load_rules("usda")
  expect_identical(names(rules$great_groups$DH[[2]]$tests$all_of[[1]]),
                   "duric_great_group_andisol_usda")
})

test_that("Ultic Andisols need base saturation < 35 % in the upper 50 cm (A9)", {
  testthat::skip_on_cran()
  rules <- load_rules("usda")
  ult <- unlist(lapply(rules$subgroups$DHF, function(x)
    if (grepl("Ultic", x$name)) names(x$tests$all_of[[length(x$tests$all_of)]])))
  expect_true(all(ult == "ultic_andisol_usda"))
})

test_that("Humic: melanic, mollic or umbric where the KST says so (A12)", {
  testthat::skip_on_cran()
  rules <- load_rules("usda")
  hu <- Filter(function(x) x$name == "Humic Udivitrands", rules$subgroups$DFB)[[1]]
  expect_true(isTRUE(hu$tests$all_of[[1]]$humic_andisol_usda$melanic))
  base <- list(top_cm = c(0, 15, 35, 60), bottom_cm = c(15, 35, 60, 100),
               designation = c("Bw1", "Bw2", "Bw3", "C"),
               oc_pct = c(9, 7, 2, 1), munsell_value_moist = c(2, 2, 4, 5),
               munsell_chroma_moist = c(1, 2, 3, 4),
               bulk_density_g_cm3 = c(0.5, 0.6, 0.8, 1.0),
               al_ox_pct = c(3, 3, 2.5, 1), fe_ox_pct = c(1, 1, 1, 0.5),
               phosphate_retention_pct = c(95, 95, 90, 50))
  p <- do.call(.v09417_pedon, base)
  expect_true(soilKey:::humic_andisol_usda(p, melanic = TRUE)$passed)
})

test_that("Andic, Vitrandic, Aquandic subgroups: 75 cm, cinders, three-valued (A13)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  # 20 cm of andic material from 70 to 90 cm: only 5 cm within 75 cm
  p <- .v09417_pedon(top_cm = c(0, 70), bottom_cm = c(70, 90),
                     designation = c("A", "Bw"), bulk_density_g_cm3 = c(1.4, 0.8),
                     al_ox_pct = c(0.2, 2), fe_ox_pct = c(0.2, 1))
  expect_false(ns$andic_subgroup_usda(p)$passed)
  # not measured: undecided
  q <- .v09417_pedon(top_cm = c(0, 30), bottom_cm = c(30, 90),
                     designation = c("A", "Bw"))
  expect_true(is.na(ns$andic_subgroup_usda(q)$passed))
  # coarse fragments over 35 % that are not cinders do not make it vitrandic
  v <- .v09417_pedon(top_cm = 0, bottom_cm = 40, designation = "C",
                     coarse_fragments_pct = 50, pyroclastic_fragments_pct = 0,
                     sand_pct = 10, silt_pct = 10)
  expect_false(ns$vitrandic_subgroup_usda(v)$passed)
  v$horizons$pyroclastic_fragments_pct <- 45
  expect_true(ns$vitrandic_subgroup_usda(v)$passed)
})

test_that("Spodosols with andic soil properties and no albic horizon are excluded (A5)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  res <- DiagnosticResult$new(name = "spodosol_usda", passed = TRUE,
                              layers = 2L, evidence = list(),
                              missing = character(0), reference = "")
  p <- .v09417_pedon(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
                     designation = c("A", "Bhs", "C"), oc_pct = c(6, 3, 1),
                     bulk_density_g_cm3 = c(0.6, 0.7, 0.8),
                     al_ox_pct = c(2.5, 3, 2.5), fe_ox_pct = c(1, 1, 1),
                     phosphate_retention_pct = c(90, 95, 90))
  expect_false(ns$.spodosol_no_andic_usda(p, res$clone())$passed)
  p$horizons$phosphate_retention_pct <- c(NA, NA, NA)
  expect_true(is.na(ns$.spodosol_no_andic_usda(p, res$clone())$passed))
})
