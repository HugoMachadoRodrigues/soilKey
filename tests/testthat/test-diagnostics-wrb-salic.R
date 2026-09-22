test_that("salic passes on canonical Solonchak fixture", {
  pr <- make_solonchak_canonical()
  res <- salic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(1L %in% res$layers)   # Az (0-25 cm)
})

test_that("salic fails on Ferralsol, Calcisol, Gypsisol", {
  expect_false(isTRUE(salic(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(salic(make_calcisol_canonical())$passed))
  expect_false(isTRUE(salic(make_gypsisol_canonical())$passed))
})

test_that("salic NA when ec_dS_m missing everywhere", {
  pr <- make_solonchak_canonical()
  pr$horizons$ec_dS_m <- NA_real_
  res <- salic(pr)
  expect_true(is.na(res$passed))
  expect_true("ec_dS_m" %in% res$missing)
})

test_that("salic respects custom thresholds", {
  pr <- make_solonchak_canonical()
  # Tightening just the primary EC isn't enough -- the alkaline path
  # (EC >= 8 AND pH >= 8.5) still rescues some Solonchak layers; need
  # to disable that path too.
  expect_false(isTRUE(salic(pr,
                              min_ec_dS_m          = 50,
                              alkaline_min_ec_dS_m = NA_real_)$passed))
  expect_false(isTRUE(salic(pr, min_thickness = 100)$passed))
})

test_that("salic evidence carries ec / thickness / product (v0.3.1)", {
  # v0.3.1 added the EC * thickness product test.
  pr <- make_solonchak_canonical()
  res <- salic(pr)
  expect_named(res$evidence, c("ec", "thickness", "product"))
})

test_that("salic alkaline path works (EC >= 8 with pH(H2O) >= 8.5)", {
  # Build a layer that fails the primary 15 dS/m gate but passes the
  # alkaline 8 dS/m + pH 8.5 gate, with product 10 * 50 = 500 >= 240.
  hz <- data.table::data.table(
    top_cm    = c(0,    50),
    bottom_cm = c(50,   100),
    ec_dS_m   = c(10,   3),
    ph_h2o    = c(8.7,  7.5),
    clay_pct  = c(20,   20),
    silt_pct  = c(40,   40),
    sand_pct  = c(40,   40)
  )
  pr <- PedonRecord$new(
    site = list(id = "alk-salic", lat = 0, lon = 0, country = "TEST",
                 parent_material = "alluvium"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- salic(pr)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$ec$details[["1"]]$path, "alkaline")
  expect_equal(res$evidence$product$details[["1"]]$threshold, 240)
})

test_that("salic product gate blocks thin high-EC layers", {
  # 10 cm * 25 dS/m = 250 < 450 (primary), and 10 cm < 15 cm thickness
  # gate -- should fail.
  hz <- data.table::data.table(
    top_cm    = c(0,    10),
    bottom_cm = c(10,   60),
    ec_dS_m   = c(25,   2),
    ph_h2o    = c(8.0,  7.5),
    clay_pct  = c(20,   20),
    silt_pct  = c(40,   40),
    sand_pct  = c(40,   40)
  )
  pr <- PedonRecord$new(
    site = list(id = "thin-salic", lat = 0, lon = 0, country = "TEST",
                 parent_material = "alluvium"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- salic(pr)
  expect_false(isTRUE(res$passed))
})

test_that("test_ec_concentration handles thresholds correctly", {
  h <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 60),
    ec_dS_m = c(25, 12)
  )
  res <- test_ec_concentration(h, min_dS_m = 15)
  expect_equal(res$layers, 1L)
})

test_that("Solonchak fixture has expected ECEC structure (Na-dominated subsoil)", {
  pr <- make_solonchak_canonical()
  expect_true(pr$horizons$na_cmol[2] > pr$horizons$ca_cmol[2] / 2)
})
