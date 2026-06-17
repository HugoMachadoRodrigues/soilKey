# Tests for v0.9.123: criteria-verified USDA intergrade subgroups
# (24 Humic Rhodic/Xanthic Oxisols via humic_oxisol_usda + the colour predicate;
# 1 Leptic Haplogypsids via gypsic_horizon_usda within 18 cm).

test_that("the v0.9.123 intergrade subgroups remain registered (count now 2049)", {
  cov <- coverage_report("usda_subgroup")
  expect_equal(cov$overall$covered_n, 2049L)  # 2003 (v0.9.123) +35 (v0.9.147) +11 (v0.9.149)
  reg <- .coverage_registered_usda_subgroups()
  expect_true("humic rhodic hapludox"  %in% reg)
  expect_true("humic xanthic hapludox" %in% reg)
  expect_true("leptic haplogypsids"    %in% reg)
})

test_that("humic_oxisol_usda matches the >=16 kg/m2 OC-in-100cm criterion", {
  rich <- PedonRecord$new(horizons = data.frame(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    oc_pct = c(3, 2), bulk_density_g_cm3 = c(1, 1)))
  expect_true(humic_oxisol_usda(rich)$passed)               # 9 + 14 = 23 kg/m2
  lean <- PedonRecord$new(horizons = data.frame(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    oc_pct = c(0.5, 0.3), bulk_density_g_cm3 = c(1, 1)))
  expect_false(humic_oxisol_usda(lean)$passed)
})

test_that("gypsic_horizon_usda honours the 18 cm Leptic depth window", {
  shallow <- PedonRecord$new(horizons = data.frame(
    top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 100), caso4_pct = c(1, 8, 8)))
  expect_true(gypsic_horizon_usda(shallow, max_top_cm = 18)$passed)
  deep <- PedonRecord$new(horizons = data.frame(
    top_cm = c(0, 40), bottom_cm = c(40, 100), caso4_pct = c(1, 8)))
  expect_false(gypsic_horizon_usda(deep, max_top_cm = 18)$passed)
})

test_that("canonical Gypsisol fixture refines Typic -> Leptic Haplogypsids", {
  # Validated refinement (great group invariant): its gypsic horizon (caso4 8%,
  # 35 cm thick) begins at 15 cm, within the 18 cm Leptic window.
  res <- classify_usda(make_gypsisol_canonical())
  expect_equal(res$trace$great_group_assigned$name, "Haplogypsids")
  expect_equal(res$name, "Leptic Haplogypsids")
})
