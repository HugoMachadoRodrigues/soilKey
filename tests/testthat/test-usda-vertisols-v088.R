# v0.8.8 USDA Soil Taxonomy 13ed -- Cap 16 Vertisols end-to-end.
# 6 Suborders + 24 Great Groups + ~85 Subgroups via Path C.

test_that("Vertisols Suborders: 6", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$VE), 6L)
})

test_that("Vertisols Great Groups: 8+2+3+4+5+2 = 24", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$FA), 8L)
  expect_equal(length(rules$great_groups$FB), 2L)
  expect_equal(length(rules$great_groups$FC), 3L)
  expect_equal(length(rules$great_groups$FD), 4L)
  expect_equal(length(rules$great_groups$FE), 5L)
  expect_equal(length(rules$great_groups$FF), 2L)
})

test_that("Vertisols Subgroups: subset cientifico", {
  rules <- load_rules("usda")
  ggs <- c("FAA","FAB","FAC","FAD","FAE","FAF","FAG","FAH",
              "FBA","FBB",
              "FCA","FCB","FCC",
              "FDA","FDB","FDC","FDD",
              "FEA","FEB","FEC","FED","FEE",
              "FFA","FFB")
  total <- sum(vapply(rules$subgroups[ggs], length, integer(1)))
  expect_gte(total, 70L)
})

test_that("classify_usda routes a vertic pedon to Vertisols Usterts", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bss", "Bss2"),
    clay_pct = c(50, 65, 60),
    silt_pct = c(30, 25, 25),
    sand_pct = c(20, 10, 15),
    cracks_width_cm = c(NA_real_, 1.5, 1.0),
    cracks_depth_cm = c(NA_real_, 80, 60),
    slickensides = c(NA_character_, "many", "common"),
    cole_value = c(0.05, 0.09, 0.07),
    bs_pct = c(80, 75, 70),
    munsell_value_moist = c(3, 4, 4),
    munsell_chroma_moist = c(2, 3, 3)
  )
  pr <- PedonRecord$new(
    site = list(id="ve", lat=15, lon=-90, country="MX",
                  parent_material="vertic",
                  soil_moisture_regime="ustic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Vertisols")
  expect_equal(res$trace$suborder_assigned$name, "Usterts")
})

test_that("WRB / earlier orders unchanged after Vertisols", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
