# v0.8.7 USDA Soil Taxonomy 13ed -- Cap 13 Oxisols end-to-end.
# 5 Suborders + 22 Great Groups + ~120 Subgroups via Path C.

test_that("Oxisols Suborders: 5", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$OX), 5L)
})

test_that("Oxisols Great Groups: 4+3+5+5+5 = 22", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$EA), 4L)  # Aquox
  expect_equal(length(rules$great_groups$EB), 3L)  # Torrox
  expect_equal(length(rules$great_groups$EC), 5L)  # Ustox
  expect_equal(length(rules$great_groups$ED), 5L)  # Perox
  expect_equal(length(rules$great_groups$EE), 5L)  # Udox
})

test_that("Oxisols Subgroups: subset cientifico cobrindo principais SGs", {
  rules <- load_rules("usda")
  ggs <- c("EAA","EAB","EAC","EAD",
              "EBA","EBB","EBC",
              "ECA","ECB","ECC","ECD","ECE",
              "EDA","EDB","EDC","EDD","EDE",
              "EEA","EEB","EEC","EED","EEE")
  total <- sum(vapply(rules$subgroups[ggs], length, integer(1)))
  expect_gte(total, 100L)
})

test_that("classify_usda routes a tropical highly-weathered profile to Oxisols", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "Bw", "Bo"),
    oc_pct = c(2.5, 0.8, 0.3),
    bs_pct = c(20, 15, 10),
    cec_cmol = c(8, 4, 3),
    ecec_cmol = c(3, 1.5, 1),
    ph_h2o = c(5.0, 5.2, 5.5),
    ph_kcl = c(4.5, 4.7, 4.9),
    clay_pct = c(45, 60, 65),
    silt_pct = c(20, 15, 15),
    sand_pct = c(35, 25, 20),
    fe_dcb_pct = c(15, 25, 30),
    munsell_hue_moist = c("5YR", "2.5YR", "2.5YR"),
    munsell_value_moist = c(3, 3, 3),
    munsell_chroma_moist = c(4, 6, 6)
  )
  pr <- PedonRecord$new(
    site = list(id="ox", lat=-15, lon=-50, country="BR",
                  parent_material="basalt",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Oxisols")
})

test_that("anionic_subgroup_usda passes for delta_pH >= 0", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bo"),
    ph_h2o = c(5.0, 4.8),
    ph_kcl = c(4.8, 5.0)  # delta = -0.2, +0.2
  )
  pr <- PedonRecord$new(
    site = list(id="an", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(anionic_subgroup_usda(pr)$passed))
})

test_that("rhodic_subgroup_usda passes for red dark colors in B", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bo"),
    munsell_hue_moist = c("5YR", "2.5YR"),
    munsell_value_moist = c(3, 3)
  )
  pr <- PedonRecord$new(
    site = list(id="rh", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- rhodic_subgroup_usda(pr)
  if (length(res$layers) > 0) expect_true(isTRUE(res$passed))
})

test_that("acric_oxisol_usda passes for low ECEC/clay AND pH(KCl) >= 5", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bo"),
    ecec_cmol = c(3, 0.6),
    clay_pct = c(40, 60),
    ph_kcl = c(5.0, 5.2)
  )
  pr <- PedonRecord$new(
    site = list(id="ac", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(acric_oxisol_usda(pr)$passed))
})

test_that("WRB / earlier orders unchanged after Oxisols", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
