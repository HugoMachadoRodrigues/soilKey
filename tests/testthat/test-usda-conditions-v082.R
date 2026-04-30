# v0.8.2 USDA Soil Taxonomy 13ed -- 5 diagnostic characteristics
# (Ch 3, pp 33-50). Reference: Soil Survey Staff (2022).

# ------------------------------------------------------------------
# 1. Aquic conditions (Ch 3, pp 41-44)
# ------------------------------------------------------------------

test_that("aquic_conditions_usda passes for redoximorphic features within 100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bg", "Cg"),
    redoximorphic_features_pct = c(0, 15, 25),
    munsell_chroma_moist = c(3, 1, 1)
  )
  pr <- PedonRecord$new(
    site = list(id="aq", lat=40, lon=-90, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- aquic_conditions_usda(pr)
  expect_true(isTRUE(res$passed))
  expect_true(res$evidence$saturation_type %in%
                c("endosaturation", "episaturation"))
})

test_that("aquic_conditions_usda fails for well-drained profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bw", "C"),
    redoximorphic_features_pct = c(0, 0, 0),
    munsell_chroma_moist = c(4, 5, 6)
  )
  pr <- PedonRecord$new(
    site = list(id="dry", lat=40, lon=-90, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(aquic_conditions_usda(pr)$passed))
})

test_that("aquic_conditions_usda missing reported when redoximorphic & chroma NA", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 80),
    designation = c("A", "Bw")
  )
  pr <- PedonRecord$new(
    site = list(id="na", lat=40, lon=-90, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- aquic_conditions_usda(pr)
  expect_false(isTRUE(res$passed))
  expect_true("redoximorphic_features_pct" %in% res$missing)
})

# ------------------------------------------------------------------
# 2. Anhydrous conditions (Ch 3, p 33)
# ------------------------------------------------------------------

test_that("anhydrous_conditions_usda passes for cold/dry/loose profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 120),
    designation = c("A", "Bw", "C"),
    permafrost_temp_C = c(-2, -3, -4),
    rupture_resistance = c("loose", "soft", "slightly hard")
  )
  pr <- PedonRecord$new(
    site = list(id="anh", lat=78, lon=20, country="NO", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(anhydrous_conditions_usda(pr)$passed))
})

test_that("anhydrous_conditions_usda fails when temperature warm", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 120),
    designation = c("A", "Bw", "C"),
    permafrost_temp_C = c(8, 7, 6),
    rupture_resistance = c("loose", "soft", "soft")
  )
  pr <- PedonRecord$new(
    site = list(id="warm", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(anhydrous_conditions_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 3. Cryoturbation (Ch 3, p 43)
# ------------------------------------------------------------------

test_that("cryoturbation_usda passes when 'jj' designation present", {
  hz <- data.table::data.table(
    top_cm = c(0, 20),
    bottom_cm = c(20, 80),
    designation = c("A", "Bjjf")
  )
  pr <- PedonRecord$new(
    site = list(id="cryo", lat=70, lon=-150, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(cryoturbation_usda(pr)$passed))
})

test_that("cryoturbation_usda passes when boundary_topography is irregular", {
  hz <- data.table::data.table(
    top_cm = c(0, 20),
    bottom_cm = c(20, 80),
    designation = c("A", "Bw"),
    boundary_topography = c("irregular", "broken")
  )
  pr <- PedonRecord$new(
    site = list(id="cryo2", lat=70, lon=-150, country="US",
                  parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(cryoturbation_usda(pr)$passed))
})

test_that("cryoturbation_usda fails for normal profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 20),
    bottom_cm = c(20, 80),
    designation = c("A", "Bw"),
    boundary_topography = c("smooth", "smooth")
  )
  pr <- PedonRecord$new(
    site = list(id="norm", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(cryoturbation_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 4. Glacic layer (Ch 3, p 45)
# ------------------------------------------------------------------

test_that("glacic_layer_usda passes for >=30 cm ice layer within 100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 120),
    designation = c("A", "Bf", "Wff")
  )
  pr <- PedonRecord$new(
    site = list(id="gla", lat=70, lon=-150, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(glacic_layer_usda(pr)$passed))
})

test_that("glacic_layer_usda fails for thin ice (<30 cm)", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 80),
    designation = c("A", "Bf", "Wff")
  )
  pr <- PedonRecord$new(
    site = list(id="gla2", lat=70, lon=-150, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(glacic_layer_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 5. Permafrost (Ch 3, p 47)
# ------------------------------------------------------------------

test_that("permafrost_within_usda passes when permafrost_temp_C <= 0 within 100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bw", "Cf"),
    permafrost_temp_C = c(2, 0, -3)
  )
  pr <- PedonRecord$new(
    site = list(id="pf", lat=70, lon=-150, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(permafrost_within_usda(pr)$passed))
})

test_that("permafrost_within_usda passes via 'ff' designation alone", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bw", "Cff")
  )
  pr <- PedonRecord$new(
    site = list(id="pf2", lat=70, lon=-150, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(permafrost_within_usda(pr)$passed))
})

test_that("permafrost_within_usda fails for warm profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bw", "C"),
    permafrost_temp_C = c(15, 12, 10)
  )
  pr <- PedonRecord$new(
    site = list(id="warm", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(permafrost_within_usda(pr)$passed))
})

test_that("permafrost_within_usda respects max_top_cm parameter", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 120),
    bottom_cm = c(30, 120, 200),
    designation = c("A", "Bw", "Cf"),
    permafrost_temp_C = c(2, 1, -3)
  )
  pr <- PedonRecord$new(
    site = list(id="deep", lat=70, lon=-150, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  # Default 100: permafrost layer at top=120 should NOT pass
  expect_false(isTRUE(permafrost_within_usda(pr)$passed))
  # Loosened: permafrost at 120 should pass
  expect_true(isTRUE(permafrost_within_usda(pr, max_top_cm = 200)$passed))
})

# ------------------------------------------------------------------
# 6. Backward compatibility
# ------------------------------------------------------------------

test_that("WRB unchanged after USDA conditions add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
