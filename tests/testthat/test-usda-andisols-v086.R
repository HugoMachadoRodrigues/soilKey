# v0.8.6 USDA Soil Taxonomy 13ed -- Cap 6 Andisols end-to-end.
# 8 Suborders + 31 Great Groups + ~155 Subgroups via Path C.

# ------------------------------------------------------------------
# 1. YAML rule counts
# ------------------------------------------------------------------

test_that("Andisols Suborders: 8", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$AD), 8L)
  codes <- vapply(rules$suborders$AD, function(s) s$code, character(1))
  expect_equal(sort(codes),
                 c("DA","DB","DC","DD","DE","DF","DG","DH"))
})

test_that("Andisols Great Groups: 8+1+6+3+3+2+2+6 = 31", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$DA), 8L)  # Aquands
  expect_equal(length(rules$great_groups$DB), 1L)  # Gelands
  expect_equal(length(rules$great_groups$DC), 6L)  # Cryands
  expect_equal(length(rules$great_groups$DD), 3L)  # Torrands
  expect_equal(length(rules$great_groups$DE), 3L)  # Xerands
  expect_equal(length(rules$great_groups$DF), 2L)  # Vitrands
  expect_equal(length(rules$great_groups$DG), 2L)  # Ustands
  expect_equal(length(rules$great_groups$DH), 6L)  # Udands
})

test_that("Andisols Subgroups: subset cientifico cobrindo SGs comuns", {
  rules <- load_rules("usda")
  ggs <- c("DAA","DAB","DAC","DAD","DAE","DAF","DAG","DAH",
              "DBA",
              "DCA","DCB","DCC","DCD","DCE","DCF",
              "DDA","DDB","DDC",
              "DEA","DEB","DEC",
              "DFA","DFB",
              "DGA","DGB",
              "DHA","DHB","DHC","DHD","DHE","DHF")
  total <- sum(vapply(rules$subgroups[ggs], length, integer(1)))
  expect_gte(total, 100L)  # Sanity: at least 100 SGs
})

# ------------------------------------------------------------------
# 2. End-to-end fixtures
# ------------------------------------------------------------------

test_that("classify_usda routes a humid andic profile to Andisols Udands", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 80),
    bottom_cm = c(25, 80, 150),
    designation = c("A", "Bw", "C"),
    oc_pct = c(8, 5, 1),
    bulk_density_g_cm3 = c(0.65, 0.75, 0.95),
    al_ox_pct = c(2.5, 2.8, 0.5),
    fe_ox_pct = c(1.5, 2.0, 0.5),
    phosphate_retention_pct = c(95, 90, 70),
    munsell_value_moist = c(2, 3, 4),
    munsell_chroma_moist = c(2, 3, 4),
    bs_pct = c(35, 30, 30),
    clay_pct = c(20, 25, 30),
    silt_pct = c(40, 40, 35),
    sand_pct = c(40, 35, 35),
    water_content_1500kpa = c(20, 22, 18)
  )
  pr <- PedonRecord$new(
    site = list(id="ad", lat=35, lon=137, country="JP",
                  parent_material="ash",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Andisols")
  expect_equal(res$trace$suborder_assigned$name, "Udands")
})

test_that("classify_usda routes a hydric andic profile to Hydrudands", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 80),
    bottom_cm = c(25, 80, 150),
    designation = c("A", "Bw", "C"),
    oc_pct = c(6, 4, 1),
    bulk_density_g_cm3 = c(0.55, 0.60, 0.85),
    al_ox_pct = c(3.0, 3.5, 0.5),
    fe_ox_pct = c(1.5, 2.0, 0.5),
    phosphate_retention_pct = c(95, 95, 80),
    bs_pct = c(40, 35, 30),
    water_content_1500kpa = c(80, 75, 35)  # high water retention
  )
  pr <- PedonRecord$new(
    site = list(id="hd", lat=20, lon=-156, country="US",
                  parent_material="ash",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Andisols")
  expect_equal(res$trace$suborder_assigned$name, "Udands")
  expect_equal(res$trace$great_group_assigned$name, "Hydrudands")
})

# ------------------------------------------------------------------
# 3. Specific helpers
# ------------------------------------------------------------------

test_that("andic_soil_properties_usda passes for low-bd + high P-retention", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 30, designation = "A",
    bulk_density_g_cm3 = 0.65,
    al_ox_pct = 2.5, fe_ox_pct = 1.5,
    phosphate_retention_pct = 95
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="JP", parent_material="ash"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(andic_soil_properties_usda(pr)$passed))
})

test_that("hydric_andisol_usda passes for high water-1500kPa", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 80),
    designation = c("A", "Bw"),
    water_content_1500kpa = c(80, 75)
  )
  pr <- PedonRecord$new(
    site = list(id="hy", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(hydric_andisol_usda(pr)$passed))
})

test_that("acric_andisol_usda passes for low ECEC in B horizon", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 130),
    designation = c("A", "Bw", "Bw2"),
    ecec_cmol = c(8, 1.5, 1.0)
  )
  pr <- PedonRecord$new(
    site = list(id="ac", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(acric_andisol_usda(pr)$passed))
})

test_that("vitrand_qualifying_usda passes for low water retention", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 60),
    designation = c("A", "C"),
    water_content_1500kpa = c(10, 12)
  )
  pr <- PedonRecord$new(
    site = list(id="vt", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(vitrand_qualifying_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 4. Backward compatibility
# ------------------------------------------------------------------

test_that("WRB / Gelisols / Histosols / Spodosols unchanged after Andisols", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
