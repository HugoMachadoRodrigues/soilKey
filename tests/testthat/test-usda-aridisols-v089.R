# v0.8.9 USDA Soil Taxonomy 13ed -- Cap 7 Aridisols end-to-end.
# 7 Suborders + 27 Great Groups + ~110 Subgroups via Path C.

test_that("Aridisols Suborders: 7", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$AS), 7L)
})

test_that("Aridisols Great Groups: 6+2+3+5+6+2+3 = 27", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$GA), 6L)  # Cryids
  expect_equal(length(rules$great_groups$GB), 2L)  # Salids
  expect_equal(length(rules$great_groups$GC), 3L)  # Durids
  expect_equal(length(rules$great_groups$GD), 5L)  # Gypsids
  expect_equal(length(rules$great_groups$GE), 6L)  # Argids
  expect_equal(length(rules$great_groups$GF), 2L)  # Calcids
  expect_equal(length(rules$great_groups$GG), 3L)  # Cambids
})

test_that("Aridisols Subgroups: subset cientifico", {
  rules <- load_rules("usda")
  ggs <- c("GAA","GAB","GAC","GAD","GAE","GAF",
              "GBA","GBB",
              "GCA","GCB","GCC",
              "GDA","GDB","GDC","GDD","GDE",
              "GEA","GEB","GEC","GED","GEE","GEF",
              "GFA","GFB",
              "GGA","GGB","GGC")
  total <- sum(vapply(rules$subgroups[ggs], length, integer(1)))
  expect_gte(total, 90L)
})

test_that("classify_usda routes a calcic-rich aridic profile to Aridisols", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Btk", "Bk"),
    caco3_pct = c(2, 25, 35),
    oc_pct = c(0.4, 0.2, 0.1),
    ph_h2o = c(7.5, 8.0, 8.2),
    bs_pct = c(80, 90, 95),
    clay_pct = c(15, 28, 22),
    silt_pct = c(40, 30, 30),
    sand_pct = c(45, 42, 48)
  )
  pr <- PedonRecord$new(
    site = list(id="ar", lat=33, lon=-110, country="US",
                  parent_material="alluvium",
                  soil_moisture_regime="aridic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Aridisols")
})

test_that("WRB / earlier orders unchanged after Aridisols", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
