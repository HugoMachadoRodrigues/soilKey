# v0.8.12 USDA Soil Taxonomy 13ed -- Cap 5 Alfisols end-to-end.
# 5 Suborders + 39 Great Groups + ~140 Subgroups via Path C.

test_that("Alfisols Suborders: 5", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$AF), 5L)
})

test_that("Alfisols Great Groups: 11+3+8+7+10 = 39", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$JA), 11L)
  expect_equal(length(rules$great_groups$JB), 3L)
  expect_equal(length(rules$great_groups$JC), 8L)
  expect_equal(length(rules$great_groups$JD), 7L)
  expect_equal(length(rules$great_groups$JE), 10L)
})

test_that("classify_usda routes ochric+argillic+highBS to Alfisols Udalfs", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "Bt", "Bt2"),
    oc_pct = c(0.8, 0.4, 0.2),
    bs_pct = c(60, 55, 50),
    cec_cmol = c(12, 18, 16),
    ph_h2o = c(6.0, 6.2, 6.5),
    clay_pct = c(15, 30, 32),
    silt_pct = c(40, 35, 33),
    sand_pct = c(45, 35, 35),
    munsell_value_moist = c(5, 4, 4),
    munsell_chroma_moist = c(4, 5, 5),
    clay_films_amount = c(NA_character_, "common", "many"),
    clay_films_strength = c(NA_character_, "distinct", "distinct")
  )
  pr <- PedonRecord$new(
    site = list(id="al", lat=40, lon=-80, country="US",
                  parent_material="loess",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Alfisols")
  expect_equal(res$trace$suborder_assigned$name, "Udalfs")
})

test_that("WRB unchanged after Alfisols add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
