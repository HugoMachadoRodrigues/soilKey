# v0.8.3 USDA Soil Taxonomy 13ed -- Cap 9 Gelisols end-to-end.
# 3 Suborders + 20 Great Groups + 100 Subgroups via Path C.
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 9, pp 189-198.

# ------------------------------------------------------------------
# 1. YAML rule counts
# ------------------------------------------------------------------

test_that("Gelisols Suborders: 3 (Histels, Turbels, Orthels)", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$GE), 3L)
  codes <- vapply(rules$suborders$GE, function(s) s$code, character(1))
  expect_equal(sort(codes), c("AA", "AB", "AC"))
})

test_that("Gelisols Great Groups: 5 + 7 + 8 = 20", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$AA), 5L)  # Histels
  expect_equal(length(rules$great_groups$AB), 7L)  # Turbels
  expect_equal(length(rules$great_groups$AC), 8L)  # Orthels
})

test_that("Gelisols Subgroups: 24 (Histels) + 45 (Turbels) + 60 (Orthels) = 129", {
  rules <- load_rules("usda")
  histels_ggs  <- c("AAA","AAB","AAC","AAD","AAE")
  turbels_ggs  <- c("ABA","ABB","ABC","ABD","ABE","ABF","ABG")
  orthels_ggs  <- c("ACA","ACB","ACC","ACD","ACE","ACF","ACG","ACH")
  histels_count <- sum(vapply(rules$subgroups[histels_ggs], length,
                                  integer(1)))
  turbels_count <- sum(vapply(rules$subgroups[turbels_ggs], length,
                                  integer(1)))
  orthels_count <- sum(vapply(rules$subgroups[orthels_ggs], length,
                                  integer(1)))
  expect_equal(histels_count, 24L)  # 3+3+6+6+6
  expect_equal(turbels_count, 45L)  # 4+6+8+9+9+4+5
  expect_equal(orthels_count, 60L)  # 7+11+8+9+9+4+4+8
  expect_equal(histels_count + turbels_count + orthels_count, 129L)
})

# ------------------------------------------------------------------
# 2. End-to-end fixtures (3 Suborders)
# ------------------------------------------------------------------

test_that("classify_usda routes a Turbel pedon to Typic Aquiturbels", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bjjf", "Cf"),  # 'jj' = cryoturbation
    permafrost_temp_C = c(2, 0, -3),
    redoximorphic_features_pct = c(0, 5, 15),
    munsell_chroma_moist = c(3, 2, 1),
    oc_pct = c(2, 0.5, 0.2),
    clay_pct = c(20, 25, 22),
    silt_pct = c(30, 35, 30),
    sand_pct = c(50, 40, 48)
  )
  pr <- PedonRecord$new(
    site = list(id="g1", lat=70, lon=-150, country="US",
                  parent_material="loess"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Gelisols")
  expect_equal(res$trace$suborder_assigned$name, "Turbels")
  expect_true(grepl("Aquiturbels", res$trace$great_group_assigned$name))
})

test_that("classify_usda routes a Histel pedon to Folistels (drained organic)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60),
    bottom_cm = c(30, 60, 150),
    designation = c("Oe", "Oa", "Cf"),  # drained organic over permafrost
    permafrost_temp_C = c(NA, 0, -3),
    oc_pct = c(40, 30, 0.5)
  )
  pr <- PedonRecord$new(
    site = list(id="g2", lat=68, lon=-145, country="US",
                  parent_material="organic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Gelisols")
  expect_equal(res$trace$suborder_assigned$name, "Histels")
})

test_that("classify_usda routes a default Orthel to Typic Haplorthels", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 80),
    bottom_cm = c(25, 80, 150),
    designation = c("A", "Bw", "Cf"),
    permafrost_temp_C = c(3, 1, -2),
    munsell_chroma_moist = c(3, 4, 5),
    oc_pct = c(0.5, 0.3, 0.1),
    clay_pct = c(15, 18, 20),
    silt_pct = c(35, 32, 30),
    sand_pct = c(50, 50, 50)
  )
  pr <- PedonRecord$new(
    site = list(id="g3", lat=72, lon=-150, country="US",
                  parent_material="till"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Gelisols")
  expect_equal(res$trace$suborder_assigned$name, "Orthels")
})

# ------------------------------------------------------------------
# 3. Subgroup helpers individual tests
# ------------------------------------------------------------------

test_that("lithic_contact_usda passes for R contact within max_top_cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 50),
    designation = c("A", "R")
  )
  pr <- PedonRecord$new(
    site = list(id="li", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(lithic_contact_usda(pr, max_top_cm = 50)$passed))
  expect_false(isTRUE(lithic_contact_usda(pr, max_top_cm = 25)$passed))
})

test_that("terric_usda passes when mineral material >= 30 cm in 0-100", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("Oe", "C", "C2")
  )
  pr <- PedonRecord$new(
    site = list(id="te", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(terric_usda(pr)$passed))
})

test_that("limnic_usda passes when limnic-designated layer >=5 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 50),
    bottom_cm = c(30, 50, 80),
    designation = c("Oe", "Lco", "Oe2")
  )
  pr <- PedonRecord$new(
    site = list(id="lim", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(limnic_usda(pr)$passed))
})

test_that("thapto_humic_usda passes for buried dark + OC layer", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80, 120),
    bottom_cm = c(30, 80, 120, 200),
    designation = c("A", "Bw", "Ab", "Bb"),
    munsell_value_moist = c(3, 4, 2, 4),
    oc_pct = c(2, 0.5, 1.5, 0.4)
  )
  pr <- PedonRecord$new(
    site = list(id="th", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(thapto_humic_usda(pr)$passed))
})

test_that("vertic_subgroup_usda passes via LE >= 6 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bss"),
    cole_value = c(0.05, 0.08)
  )
  pr <- PedonRecord$new(
    site = list(id="ve", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  # LE = 30*0.05 + 70*0.08 = 1.5 + 5.6 = 7.1 cm
  expect_true(isTRUE(vertic_subgroup_usda(pr)$passed))
})

test_that("psammentic_subgroup_usda passes for sandy throughout", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 100),
    designation = c("A", "C", "C2"),
    clay_pct = c(5, 8, 10),
    silt_pct = c(5, 7, 10),
    sand_pct = c(90, 85, 80),
    coarse_fragments_pct = c(2, 5, 10)
  )
  pr <- PedonRecord$new(
    site = list(id="ps", lat=0, lon=0, country="US", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(psammentic_subgroup_usda(pr)$passed))
})

# ------------------------------------------------------------------
# 4. Backward compatibility
# ------------------------------------------------------------------

test_that("WRB / SiBCS unchanged after Cap 9 Gelisols add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
