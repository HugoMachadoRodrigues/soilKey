# v0.8.13 USDA Soil Taxonomy 13ed -- Cap 11 Inceptisols end-to-end.
# 6 Suborders + 36 Great Groups + ~110 Subgroups via Path C.

test_that("Inceptisols Suborders: 6", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$IN), 6L)
})

test_that("Inceptisols Great Groups: 11+3+4+6+5+7 = 36", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$KA), 11L)
  expect_equal(length(rules$great_groups$KB), 3L)
  expect_equal(length(rules$great_groups$KC), 4L)
  expect_equal(length(rules$great_groups$KD), 6L)
  expect_equal(length(rules$great_groups$KE), 5L)
  expect_equal(length(rules$great_groups$KF), 7L)
})

test_that("classify_usda routes a cambic-only profile to Inceptisols Udepts", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bw", "C"),
    oc_pct = c(1.5, 0.8, 0.3),
    bs_pct = c(70, 65, 60),
    cec_cmol = c(15, 12, 10),
    ph_h2o = c(6.5, 6.5, 6.8),
    clay_pct = c(20, 22, 21),
    silt_pct = c(40, 40, 40),
    sand_pct = c(40, 38, 39),
    munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
    munsell_value_moist = c(4, 4, 5),
    munsell_chroma_moist = c(3, 5, 4),
    structure_grade = c(NA_character_, "moderate", NA_character_),
    structure_size = c(NA_character_, "medium", NA_character_)
  )
  pr <- PedonRecord$new(
    site = list(id="ic", lat=45, lon=10, country="IT",
                  parent_material="alluvium",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Inceptisols")
  expect_equal(res$trace$suborder_assigned$name, "Udepts")
})

test_that("WRB unchanged after Inceptisols add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
