# v0.8.1 USDA Soil Taxonomy 13ed -- 6 epipedons (Cap 3, pp 13-21).
# Reference: Soil Survey Staff (2022), Keys to Soil Taxonomy 13th edition.

# ------------------------------------------------------------------
# 1. Histic Epipedon (Ch 3, pp 14-15)
# ------------------------------------------------------------------

test_that("histic_epipedon_usda passes for 30+ cm H horizon with OC>=12", {
  hz <- data.table::data.table(
    top_cm = c(0, 35),
    bottom_cm = c(35, 100),
    designation = c("Hi", "C"),
    oc_pct = c(35, 1.0)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(histic_epipedon_usda(pr)$passed))
})

test_that("histic_epipedon_usda fails for thin H horizon (<20 cm)", {
  hz <- data.table::data.table(
    top_cm = c(0, 15),
    bottom_cm = c(15, 100),
    designation = c("Hi", "C"),
    oc_pct = c(35, 1.0)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(histic_epipedon_usda(pr)$passed))
})

test_that("histic_epipedon_usda passes via Ap shortcut (OC >= 8%)", {
  hz <- data.table::data.table(
    top_cm = c(0, 25),
    bottom_cm = c(25, 100),
    designation = c("Ap", "Bg"),
    oc_pct = c(10, 0.5)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(histic_epipedon_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 2. Folistic Epipedon (Ch 3, pp 13-14)
# ------------------------------------------------------------------

test_that("folistic_epipedon_usda passes for 15+ cm O horizon with OC>=12", {
  hz <- data.table::data.table(
    top_cm = c(0, 20),
    bottom_cm = c(20, 100),
    designation = c("Oe", "C"),
    oc_pct = c(45, 0.5)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(folistic_epipedon_usda(pr)$passed))
})

test_that("folistic_epipedon_usda fails for thin O horizon", {
  hz <- data.table::data.table(
    top_cm = c(0, 10),
    bottom_cm = c(10, 100),
    designation = c("Oe", "C"),
    oc_pct = c(45, 0.5)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(folistic_epipedon_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 3. Mollic Epipedon (Ch 3, pp 15-17)
# ------------------------------------------------------------------

test_that("mollic_epipedon_usda passes for canonical Mollisol surface", {
  # Dark, base-rich, organic-rich, 25-cm A horizon
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    munsell_value_dry = c(4, 6),
    oc_pct = c(2.0, 0.4),
    bs_pct = c(75, 65),
    clay_pct = c(25, 20),
    silt_pct = c(30, 25),
    sand_pct = c(45, 55)
  )
  pr <- PedonRecord$new(
    site = list(id="moll", lat=40, lon=-100, country="US",
                  parent_material="loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(mollic_epipedon_usda(pr)$passed))
})

test_that("mollic_epipedon_usda fails for low base saturation (umbric territory)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    munsell_value_dry = c(4, 6),
    oc_pct = c(2.5, 0.4),
    bs_pct = c(30, 25),
    clay_pct = c(25, 20)
  )
  pr <- PedonRecord$new(
    site = list(id="umb", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(mollic_epipedon_usda(pr)$passed))
})

test_that("mollic_epipedon_usda fails for too-thin A horizon", {
  hz <- data.table::data.table(
    top_cm = c(0, 10),
    bottom_cm = c(10, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    munsell_value_dry = c(4, 6),
    oc_pct = c(2.5, 0.4),
    bs_pct = c(75, 65),
    clay_pct = c(25, 20)
  )
  pr <- PedonRecord$new(
    site = list(id="thin", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(mollic_epipedon_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 4. Umbric Epipedon (Ch 3, pp 18-20)
# ------------------------------------------------------------------

test_that("umbric_epipedon_usda passes for low BS canonical surface", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    munsell_value_dry = c(4, 6),
    oc_pct = c(2.5, 0.4),
    bs_pct = c(30, 25),  # < 50%
    clay_pct = c(25, 20)
  )
  pr <- PedonRecord$new(
    site = list(id="umb", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(umbric_epipedon_usda(pr)$passed))
})

test_that("umbric_epipedon_usda fails when BS >= 50 (mollic territory)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    munsell_value_dry = c(4, 6),
    oc_pct = c(2.5, 0.4),
    bs_pct = c(75, 65),
    clay_pct = c(25, 20)
  )
  pr <- PedonRecord$new(
    site = list(id="moll", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(umbric_epipedon_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 5. Ochric Epipedon (Ch 3, p 17) -- catch-all
# ------------------------------------------------------------------

test_that("ochric_epipedon_usda passes for thin/light A (not mollic, not umbric)", {
  hz <- data.table::data.table(
    top_cm = c(0, 5),
    bottom_cm = c(5, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(5, 5),
    munsell_chroma_moist = c(4, 4),
    oc_pct = c(0.3, 0.2),
    bs_pct = c(60, 60),
    clay_pct = c(20, 20)
  )
  pr <- PedonRecord$new(
    site = list(id="ochr", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(ochric_epipedon_usda(pr)$passed))
})

test_that("ochric_epipedon_usda fails when mollic passes", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    munsell_value_dry = c(4, 6),
    oc_pct = c(2.0, 0.4),
    bs_pct = c(75, 65),
    clay_pct = c(25, 20)
  )
  pr <- PedonRecord$new(
    site = list(id="moll", lat=40, lon=-100, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(ochric_epipedon_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 6. Melanic Epipedon (Ch 3, pp 15-16)
# ------------------------------------------------------------------

test_that("melanic_epipedon_usda passes for andic, very dark, OC-rich surface", {
  hz <- data.table::data.table(
    top_cm = c(0, 35),
    bottom_cm = c(35, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(1.5, 3),
    bulk_density_g_cm3 = c(0.7, 0.9),
    phosphate_retention_pct = c(95, 85),
    oc_pct = c(7, 1)
  )
  pr <- PedonRecord$new(
    site = list(id="mela", lat=35, lon=137, country="JP", parent_material="ash"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(melanic_epipedon_usda(pr)$passed))
})

test_that("melanic_epipedon_usda fails when bulk density too high", {
  hz <- data.table::data.table(
    top_cm = c(0, 35),
    bottom_cm = c(35, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(1.5, 3),
    bulk_density_g_cm3 = c(1.3, 1.4),  # too high for andic
    phosphate_retention_pct = c(95, 85),
    oc_pct = c(7, 1)
  )
  pr <- PedonRecord$new(
    site = list(id="mela", lat=35, lon=137, country="JP", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(melanic_epipedon_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 7. Backward compatibility
# ------------------------------------------------------------------

test_that("WRB / SiBCS unchanged after USDA epipedons add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
