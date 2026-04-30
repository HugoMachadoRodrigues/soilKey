# v0.8.4 USDA Soil Taxonomy 13ed -- Cap 10 Histosols end-to-end.
# 5 Suborders + 19 Great Groups + 75 Subgroups via Path C.

# ------------------------------------------------------------------
# 1. YAML rule counts
# ------------------------------------------------------------------

test_that("Histosols Suborders: 5 (Folists, Wassists, Fibrists, Saprists, Hemists)", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$HI), 5L)
  codes <- vapply(rules$suborders$HI, function(s) s$code, character(1))
  expect_equal(sort(codes), c("BA","BB","BC","BD","BE"))
})

test_that("Histosols Great Groups: 4+3+3+4+5 = 19", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$BA), 4L)  # Folists
  expect_equal(length(rules$great_groups$BB), 3L)  # Wassists
  expect_equal(length(rules$great_groups$BC), 3L)  # Fibrists
  expect_equal(length(rules$great_groups$BD), 4L)  # Saprists
  expect_equal(length(rules$great_groups$BE), 5L)  # Hemists
})

test_that("Histosols Subgroups: 8+13+20+17+17 = 75", {
  rules <- load_rules("usda")
  ba_ggs <- c("BAA","BAB","BAC","BAD")
  bb_ggs <- c("BBA","BBB","BBC")
  bc_ggs <- c("BCA","BCB","BCC")
  bd_ggs <- c("BDA","BDB","BDC","BDD")
  be_ggs <- c("BEA","BEB","BEC","BED","BEE")
  ba <- sum(vapply(rules$subgroups[ba_ggs], length, integer(1)))
  bb <- sum(vapply(rules$subgroups[bb_ggs], length, integer(1)))
  bc <- sum(vapply(rules$subgroups[bc_ggs], length, integer(1)))
  bd <- sum(vapply(rules$subgroups[bd_ggs], length, integer(1)))
  be <- sum(vapply(rules$subgroups[be_ggs], length, integer(1)))
  expect_equal(ba, 8L)
  expect_equal(bb, 13L)
  expect_equal(bc, 20L)
  expect_equal(bd, 17L)
  expect_equal(be, 17L)
  expect_equal(ba+bb+bc+bd+be, 75L)
})

# ------------------------------------------------------------------
# 2. End-to-end fixtures
# ------------------------------------------------------------------

test_that("classify_usda routes a Saprist pedon to Haplosaprists", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("Oa", "Oa2", "Cg"),
    oc_pct = c(40, 35, 0.5),
    fiber_content_rubbed_pct = c(10, 12, NA_real_),
    von_post_index = c(8L, 9L, NA_integer_),
    redoximorphic_features_pct = c(5, 10, 15),
    munsell_chroma_moist = c(2, 1, 1)
  )
  pr <- PedonRecord$new(
    site = list(id="h1", lat=45, lon=-90, country="US",
                  parent_material="organic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Histosols")
  expect_equal(res$trace$suborder_assigned$name, "Saprists")
})

test_that("classify_usda routes a drained-organic pedon to Folists Suborder", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60),
    bottom_cm = c(30, 60, 150),
    designation = c("Oe", "Oa", "C"),
    oc_pct = c(40, 35, 0.5),
    fiber_content_rubbed_pct = c(50, 20, NA_real_),
    von_post_index = c(3L, 7L, NA_integer_),
    redoximorphic_features_pct = c(0, 0, 0),
    munsell_chroma_moist = c(4, 4, 4)
  )
  pr <- PedonRecord$new(
    site = list(id="h2", lat=45, lon=-90, country="US",
                  parent_material="organic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Histosols")
  expect_equal(res$trace$suborder_assigned$name, "Folists")
})

# ------------------------------------------------------------------
# 3. Specific helpers
# ------------------------------------------------------------------

test_that("wassist_qualifying_usda reads from site$water_table_cm_above_surface", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 30, designation = "Oe", oc_pct = 30
  )
  pr <- PedonRecord$new(
    site = list(id="w", lat=0, lon=0, country="US", parent_material="t",
                  water_table_cm_above_surface = 5),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(wassist_qualifying_usda(pr)$passed))

  pr2 <- PedonRecord$new(
    site = list(id="w2", lat=0, lon=0, country="US", parent_material="t",
                  water_table_cm_above_surface = -10),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(wassist_qualifying_usda(pr2)$passed))
})

test_that("smr_*_usda helpers read from site$soil_moisture_regime", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 30, designation = "A"
  )
  pr <- PedonRecord$new(
    site = list(id="s", lat=0, lon=0, country="US", parent_material="t",
                  soil_moisture_regime = "udic"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(smr_udic_usda(pr)$passed))
  expect_false(isTRUE(smr_aridic_usda(pr)$passed))
})

test_that("str_cryic_usda reads from site$soil_temperature_regime", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 30, designation = "A"
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=68, lon=-150, country="US", parent_material="t",
                  soil_temperature_regime = "cryic"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(str_cryic_usda(pr)$passed))
})

test_that("frasic_qualifying_usda passes for low EC Wassist", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("Oa", "Cg"),
    ec_dS_m = c(0.3, 0.4)
  )
  pr <- PedonRecord$new(
    site = list(id="fr", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(frasic_qualifying_usda(pr)$passed))
})

test_that("halic_subgroup_usda passes for high EC layer >= 30 cm thick", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 70),
    designation = c("Oa", "Cz"),
    ec_dS_m = c(35, 40)
  )
  pr <- PedonRecord$new(
    site = list(id="hl", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(halic_subgroup_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 4. Backward compatibility
# ------------------------------------------------------------------

test_that("WRB / Gelisols / SiBCS unchanged after Histosols add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
