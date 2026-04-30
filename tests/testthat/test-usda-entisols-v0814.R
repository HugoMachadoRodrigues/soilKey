# v0.8.14 USDA Soil Taxonomy 13ed -- Cap 8 Entisols end-to-end.
# 5 Suborders + 32 Great Groups + ~95 Subgroups via Path C.
# FECHA O USDA PATH C COMPLETO!

test_that("Entisols Suborders: 5", {
  rules <- load_rules("usda")
  expect_equal(length(rules$suborders$EN), 5L)
})

test_that("Entisols Great Groups: 6+8+6+6+6 = 32", {
  rules <- load_rules("usda")
  expect_equal(length(rules$great_groups$LA), 6L)
  expect_equal(length(rules$great_groups$LB), 8L)
  expect_equal(length(rules$great_groups$LC), 6L)
  expect_equal(length(rules$great_groups$LD), 6L)
  expect_equal(length(rules$great_groups$LE), 6L)
})

test_that("classify_usda routes a young profile to Entisols Orthents", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "C", "C2"),
    oc_pct = c(1, 0.3, 0.1),
    bs_pct = c(60, 60, 60),
    cec_cmol = c(8, 6, 5),
    ph_h2o = c(6.5, 6.8, 7.0),
    clay_pct = c(15, 18, 20),
    silt_pct = c(35, 35, 35),
    sand_pct = c(50, 47, 45),
    munsell_value_moist = c(5, 6, 6),
    munsell_chroma_moist = c(4, 4, 4)
  )
  pr <- PedonRecord$new(
    site = list(id="en", lat=40, lon=-90, country="US",
                  parent_material="alluvium",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Entisols")
  expect_equal(res$trace$suborder_assigned$name, "Orthents")
})

test_that("classify_usda routes sandy profile to Psamments", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "C"),
    oc_pct = c(0.4, 0.2),
    bs_pct = c(60, 50),
    clay_pct = c(3, 5),
    silt_pct = c(5, 5),
    sand_pct = c(92, 90),
    munsell_value_moist = c(5, 6),
    munsell_chroma_moist = c(4, 4)
  )
  pr <- PedonRecord$new(
    site = list(id="ps", lat=30, lon=-100, country="US",
                  parent_material="sand",
                  soil_moisture_regime="udic"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Entisols")
  expect_equal(res$trace$suborder_assigned$name, "Psamments")
})

test_that("WRB unchanged after Entisols add (USDA Path C COMPLETE)", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})

test_that("USDA Path C COMPLETO: classify_usda nunca retorna NULL Order", {
  # Para todos os fixtures canonicos disponiveis, classify_usda deve
  # retornar uma Order valida (nao NA, nao NULL).
  fixtures <- list(
    fer = make_ferralsol_canonical(),
    sol = make_solonchak_canonical(),
    cam = make_cambisol_canonical(),
    his = make_histosol_canonical(),
    are = make_arenosol_canonical()
  )
  for (nm in names(fixtures)) {
    res <- classify_usda(fixtures[[nm]], on_missing = "silent")
    expect_true(!is.null(res$rsg_or_order) && !is.na(res$rsg_or_order),
                  info = paste("fixture:", nm))
  }
})
