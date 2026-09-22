# v0.8.11 USDA Soil Taxonomy 13ed -- Cap 12 Mollisols end-to-end.
# 8 Suborders + 37 Great Groups + ~115 Subgroups via Path C.

test_that("Mollisols Suborders: 8", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$MO), 8L)
})

test_that("Mollisols Great Groups: 2+7+2+1+6+6+7+6 = 37", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$IA), 2L)
  expect_equal(length(rules$great_groups$IB), 7L)
  expect_equal(length(rules$great_groups$IC), 2L)
  expect_equal(length(rules$great_groups$ID), 1L)
  expect_equal(length(rules$great_groups$IE), 6L)
  expect_equal(length(rules$great_groups$IF), 6L)
  expect_equal(length(rules$great_groups$IG), 7L)
  expect_equal(length(rules$great_groups$IH), 6L)
})

test_that("classify_usda routes a chernozem-like to Mollisols Ustolls", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "Bw", "Bk"),
    oc_pct = c(2.5, 0.8, 0.4),
    bs_pct = c(85, 90, 95),
    caco3_pct = c(2, 5, 20),
    ph_h2o = c(7.0, 7.5, 8.0),
    munsell_value_moist = c(2, 3, 4),
    munsell_chroma_moist = c(2, 3, 3),
    munsell_value_dry = c(4, 5, 6),
    clay_pct = c(20, 25, 22),
    silt_pct = c(40, 35, 35),
    sand_pct = c(40, 40, 43)
  )
  pr <- PedonRecord$new(
    site = list(id="mo", lat=42, lon=-100, country="US",
                  parent_material="loess",
                  soil_moisture_regime="ustic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Mollisols")
  expect_equal(res$trace$suborder_assigned$name, "Ustolls")
})

test_that("WRB unchanged after Mollisols add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
