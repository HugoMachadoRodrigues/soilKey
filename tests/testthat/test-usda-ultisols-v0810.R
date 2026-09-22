# v0.8.10 USDA Soil Taxonomy 13ed -- Cap 15 Ultisols end-to-end.
# 5 Suborders + 30 Great Groups + ~110 Subgroups via Path C.

test_that("Ultisols Suborders: 5", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$UT), 5L)
})

test_that("Ultisols Great Groups: 9+6+7+6+2 = 30", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$HA), 9L)
  expect_equal(length(rules$great_groups$HB), 6L)
  expect_equal(length(rules$great_groups$HC), 7L)
  expect_equal(length(rules$great_groups$HD), 6L)
  expect_equal(length(rules$great_groups$HE), 2L)
})

test_that("classify_usda routes a tropical udic argillic + low BS to Ultisols", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "Bt", "Bt2"),
    oc_pct = c(2, 0.5, 0.3),
    bs_pct = c(20, 25, 30),
    cec_cmol = c(8, 6, 5),
    ph_h2o = c(5.0, 5.2, 5.3),
    clay_pct = c(15, 35, 30),
    silt_pct = c(30, 25, 25),
    sand_pct = c(55, 40, 45),
    clay_films_amount = c(NA_character_, "common", "many"),
    clay_films_strength = c(NA_character_, "distinct", "prominent")
  )
  pr <- PedonRecord$new(
    site = list(id="ut", lat=-20, lon=-45, country="BR",
                  parent_material="schist",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Ultisols")
  expect_equal(res$trace$suborder_assigned$name, "Udults")
})

test_that("WRB unchanged after Ultisols add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
