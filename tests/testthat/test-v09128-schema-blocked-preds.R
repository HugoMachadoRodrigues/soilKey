# v0.9.128 -- four new schema fields unlock five schema-blocked predicates.
# Each refines a predicate that previously used an air-dried-only / proxy
# criterion. The contract: ABSENT field => prior behaviour (byte-identical);
# PRESENT field => the verbatim criterion is enforced.

mk <- function(df) {
  PedonRecord$new(horizons =
    ensure_horizon_schema(data.table::as.data.table(df)))
}

# ---- Vitrands: < 30% undried 1500 kPa water (KST 13ed Ch 6) ----------------

test_that("vitrand: undried field absent => air-dried-only behaviour", {
  p <- mk(data.frame(top_cm = c(0, 30), bottom_cm = c(30, 60),
                     water_content_1500kpa = c(10, 12)))
  expect_true(vitrand_qualifying_usda(p)$passed)
})

test_that("vitrand: undried >= 30% disqualifies, < 30% qualifies", {
  base <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 60),
                     water_content_1500kpa = c(10, 12))
  hi <- base; hi$water_content_1500kpa_undried <- c(40, 40)
  lo <- base; lo$water_content_1500kpa_undried <- c(20, 20)
  expect_false(vitrand_qualifying_usda(mk(hi))$passed)
  expect_true(vitrand_qualifying_usda(mk(lo))$passed)
})

# ---- Vitrandic: fine earth >= 30% in 0.02-2 mm (KST 13ed Ch 9) --------------

test_that("vitrandic: particles field absent => glass-only branch behaviour", {
  p <- mk(data.frame(top_cm = 0, bottom_cm = 20, al_ox_pct = 0.3,
                     fe_ox_pct = 0.2, volcanic_glass_pct = 8))
  expect_true(vitrandic_subgroup_usda(p)$passed)
})

test_that("vitrandic: < 30% particles 0.02-2 mm fails branch 2", {
  base <- data.frame(top_cm = 0, bottom_cm = 20, al_ox_pct = 0.3,
                     fe_ox_pct = 0.2, volcanic_glass_pct = 8)
  lo <- base; lo$particles_002_2mm_pct <- 10
  hi <- base; hi$particles_002_2mm_pct <- 35
  expect_false(vitrandic_subgroup_usda(mk(lo))$passed)
  expect_true(vitrandic_subgroup_usda(mk(hi))$passed)
})

# ---- Vertic: cracks within 125 cm (KST 13ed) -------------------------------

test_that("vertic: cracks_top absent => prior crack behaviour", {
  p <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     cracks_width_cm = c(NA, 1), cracks_depth_cm = c(NA, 40),
                     slickensides = c(NA, "common")))
  expect_true(vertic_subgroup_usda(p)$passed)
})

test_that("vertic: cracks starting below 125 cm do not qualify", {
  base <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     cracks_width_cm = c(NA, 1), cracks_depth_cm = c(NA, 40),
                     slickensides = c(NA, "common"))
  deep <- base; deep$cracks_top_cm <- c(NA, 150)
  shallow <- base; shallow$cracks_top_cm <- c(NA, 40)
  expect_false(vertic_subgroup_usda(mk(deep))$passed)
  expect_true(vertic_subgroup_usda(mk(shallow))$passed)
})

# ---- Sulfidic: incubation pH distinguishes hyper vs hypo (WRB 3.3.8/9) ------

test_that("sulfidic: incubation absent => potential hypersulfidic, hypo empty", {
  p <- mk(data.frame(top_cm = 0, bottom_cm = 30,
                     sulfidic_s_pct = 0.05, ph_h2o = 5))
  expect_true(hypersulfidic_material(p)$passed)
  expect_false(isTRUE(hyposulfidic_material(p)$passed))
})

test_that("sulfidic: incubation pH < 4 => hypersulfidic, not hyposulfidic", {
  p <- mk(data.frame(top_cm = 0, bottom_cm = 30, sulfidic_s_pct = 0.05,
                     ph_h2o = 5, incubation_ph = 3.5))
  expect_true(hypersulfidic_material(p)$passed)
  expect_false(isTRUE(hyposulfidic_material(p)$passed))
})

test_that("sulfidic: incubation pH >= 4 => hyposulfidic, not hypersulfidic", {
  p <- mk(data.frame(top_cm = 0, bottom_cm = 30, sulfidic_s_pct = 0.05,
                     ph_h2o = 5, incubation_ph = 6))
  expect_false(isTRUE(hypersulfidic_material(p)$passed))
  expect_true(hyposulfidic_material(p)$passed)
})

# ---- schema bookkeeping ----------------------------------------------------

test_that("the four new fields are in the horizon schema spec", {
  spec <- names(horizon_column_spec())
  expect_true(all(c("water_content_1500kpa_undried", "particles_002_2mm_pct",
                    "cracks_top_cm", "incubation_ph") %in% spec))
})
