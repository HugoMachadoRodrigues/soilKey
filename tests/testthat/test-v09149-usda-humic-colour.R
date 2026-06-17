# v0.9.149 — Humic colour-value USDA predicate + the subgroups it unblocks
#
# humic_colour_usda() = the KST-13 "Humic" Inceptisol intergrade differentia
# (value moist <= 3 AND dry <= 5 throughout the upper 18 cm). It unblocks 11
# subgroups the v0.9.147 coverage slice had to exclude for want of a predicate.

mk <- function(df) ensure_horizon_schema(data.table::as.data.table(df))

test_that("humic_colour_usda fires only when the upper 18 cm is dark (moist & dry)", {
  dark <- PedonRecord$new(horizons = mk(data.frame(
    top_cm = c(0, 10, 25), bottom_cm = c(10, 25, 60),
    designation = c("A1", "A2", "Bw"),
    munsell_value_moist = c(2, 3, 4), munsell_value_dry = c(4, 5, 6))))
  expect_true(humic_colour_usda(dark)$passed)

  light <- PedonRecord$new(horizons = mk(data.frame(
    top_cm = c(0, 10), bottom_cm = c(10, 40), designation = c("A", "Bw"),
    munsell_value_moist = c(4, 5), munsell_value_dry = c(6, 7))))
  expect_false(humic_colour_usda(light)$passed)
})

test_that("humic_colour_usda needs EVERY upper-18cm layer dark (throughout)", {
  # second A layer too light in dry value -> not 'throughout'
  mixed <- PedonRecord$new(horizons = mk(data.frame(
    top_cm = c(0, 10), bottom_cm = c(10, 30), designation = c("A1", "A2"),
    munsell_value_moist = c(2, 3), munsell_value_dry = c(4, 6))))
  expect_false(humic_colour_usda(mixed)$passed)
})

test_that("humic_colour_usda is conservative: missing dry value cannot confirm", {
  no_dry <- PedonRecord$new(horizons = mk(data.frame(
    top_cm = 0, bottom_cm = 20, designation = "A",
    munsell_value_moist = 2, munsell_value_dry = NA_real_)))
  r <- humic_colour_usda(no_dry)
  expect_false(r$passed)
  expect_true("munsell_value_dry" %in% r$missing)
})

test_that("v0.9.149: the 11 Humic colour subgroups are registered (coverage 2049)", {
  cov <- coverage_report("usda_subgroup")
  expect_equal(cov$overall$covered_n, 2049L)        # 2038 (v0.9.147) + 11
  reg <- soilKey:::.coverage_registered_usda_subgroups()
  added <- tolower(c(
    "Humic Densiudepts", "Humic Dystroxerepts", "Humic Dystrustepts",
    "Humic Eutrudepts", "Humic Fragiudepts", "Humic Fragixerepts",
    "Humic Haploxerepts", "Humic Lithic Dystrudepts", "Humic Lithic Eutrudepts",
    "Humic Lithic Dystroxerepts", "Humic Lithic Haploxerepts"))
  expect_true(all(added %in% reg))
  expect_length(added, 11L)
})

test_that("v0.9.149: USDA rule base still loads with the additions", {
  expect_silent(suppressMessages(load_rules("usda")))
})
