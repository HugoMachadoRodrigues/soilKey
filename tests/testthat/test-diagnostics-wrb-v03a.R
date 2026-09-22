test_that("histic_horizon passes on canonical Histosol fixture", {
  res <- histic_horizon(make_histosol_canonical())
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
})

test_that("histic_horizon fails on Ferralsol / mineral profiles", {
  for (fn in list(make_ferralsol_canonical, make_luvisol_canonical,
                    make_chernozem_canonical, make_andosol_canonical)) {
    expect_false(isTRUE(histic_horizon(fn())$passed))
  }
})

test_that("histic_horizon respects min_thickness and min_oc", {
  pr <- make_histosol_canonical()
  # v0.9.15: histic_horizon now offers an OR-alternative cumulative
  # path (OC >= min_oc summing to >= 40 cm in upper 80 cm). To gate
  # the contiguous path, also gate the cumulative path -- otherwise
  # a canonical Histosol still qualifies via cumulative.
  expect_false(isTRUE(histic_horizon(pr, min_thickness    = 200,
                                          cumulative_min_cm = 1000)$passed))
  expect_false(isTRUE(histic_horizon(pr, min_oc = 50)$passed))
})

test_that("histic_horizon cumulative path catches folic / mossy variants (v0.9.15)", {
  # Build a profile with multiple thin organic layers (each below the
  # 10 cm contiguous threshold or interrupted by mineral lenses) that
  # together cumulate >= 40 cm of OC >= 12 within the upper 80 cm --
  # characteristic of folic / mossy Histosols on slopes.
  hz <- data.table::data.table(
    top_cm    = c(0,   8,  16, 24,  32, 40, 48, 90),
    bottom_cm = c(8,  16,  24, 32,  40, 48, 90, 120),
    designation = c("Of1","Of2","Of3","Of4","Of5","Of6","Cf","C"),
    oc_pct    = c(20,   22,   18,   16,   14,   13,   0.5,  0.3)
  )
  pr <- PedonRecord$new(
    site = list(id = "folic-cumulative-01"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- histic_horizon(pr)
  expect_true(isTRUE(res$passed))
  cum <- res$evidence$cumulative$cumulative_oc$details$cumulative_thickness_cm
  expect_gte(cum, 40)
})

test_that("leptic_features passes on canonical Leptosol fixture", {
  res <- leptic_features(make_leptosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("leptic_features fails on deep / mineral profiles", {
  expect_false(isTRUE(leptic_features(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(leptic_features(make_luvisol_canonical())$passed))
})

test_that("leptic_features respects max_depth", {
  pr <- make_leptosol_canonical()
  expect_true(isTRUE(leptic_features(pr, max_depth = 25)$passed))
  expect_false(isTRUE(leptic_features(pr, max_depth = 5)$passed))
})

test_that("arenic_texture passes on canonical Arenosol fixture", {
  res <- arenic_texture(make_arenosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("arenic_texture fails on clayey / loamy fixtures", {
  for (fn in list(make_ferralsol_canonical, make_luvisol_canonical,
                    make_vertisol_canonical)) {
    expect_false(isTRUE(arenic_texture(fn())$passed))
  }
})

test_that("umbric_horizon passes on canonical Umbrisol fixture", {
  res <- umbric_horizon(make_umbrisol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("umbric_horizon fails when BS is high (-> mollic)", {
  pr <- make_chernozem_canonical()
  expect_false(isTRUE(umbric_horizon(pr)$passed))
})

test_that("mollic distinguishes from umbric on the BS test", {
  expect_false(isTRUE(mollic(make_umbrisol_canonical())$passed))
  expect_true (isTRUE(umbric_horizon(make_umbrisol_canonical())$passed))
})

test_that("duric_horizon passes on canonical Durisol fixture", {
  res <- duric_horizon(make_durisol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("duric_horizon fails on profiles without duripan", {
  expect_false(isTRUE(duric_horizon(make_ferralsol_canonical())$passed))
  expect_false(isTRUE(duric_horizon(make_chernozem_canonical())$passed))
})

test_that("technic_features passes on canonical Technosol fixture", {
  res <- technic_features(make_technosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("technic_features respects custom artefacts threshold", {
  pr <- make_technosol_canonical()
  # v0.9.15: parameter renamed `min_pct` -> `artefacts_min_pct`
  # because technic_features now offers three OR-paths.
  expect_false(isTRUE(technic_features(pr, artefacts_min_pct = 50)$passed))
})

test_that("andic_properties passes on canonical Andosol fixture", {
  res <- andic_properties(make_andosol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("andic_properties fails when bulk density is high (v0.9.15: also clear glass + P-retention)", {
  # v0.9.15: andic_properties now offers three OR-alternatives:
  # (Al-Fe + low BD), phosphate retention >= 70%, or volcanic glass
  # >= 30%. The canonical Andosol fixture sets all three; to gate
  # the BD path, also clear the glass + P-retention paths.
  pr <- make_andosol_canonical()
  pr$horizons$bulk_density_g_cm3   <- 1.4
  pr$horizons$volcanic_glass_pct   <- 0
  pr$horizons$phosphate_retention_pct <- 0
  expect_false(isTRUE(andic_properties(pr)$passed))
})

test_that("andic_properties fails when Al/Fe oxalate is low (v0.9.15: also clear glass + P-retention)", {
  pr <- make_andosol_canonical()
  pr$horizons$al_ox_pct <- 0.1
  pr$horizons$fe_ox_pct <- 0.1
  pr$horizons$volcanic_glass_pct      <- 0
  pr$horizons$phosphate_retention_pct <- 0
  expect_false(isTRUE(andic_properties(pr)$passed))
})

test_that("andic_properties: phosphate-retention alternative qualifies (v0.9.15)", {
  # A profile with low Al-Fe + high BD but P-retention >= 70 still
  # qualifies as andic via the WRB 2022 alternative path.
  pr <- make_andosol_canonical()
  pr$horizons$al_ox_pct <- 0.1
  pr$horizons$fe_ox_pct <- 0.1
  pr$horizons$bulk_density_g_cm3   <- 1.5
  pr$horizons$volcanic_glass_pct   <- 0
  # phosphate_retention_pct already >= 70 in the fixture
  expect_true(isTRUE(andic_properties(pr)$passed))
})

test_that("andic_properties does NOT key on volcanic_glass (v0.9.15: that is vitric_properties)", {
  # Volcanic glass without Al-humus complex or P-retention is a
  # vitric, not an andic, profile. The Andosol RSG gate keys on
  # (andic OR vitric) -- but `andic_properties()` itself must
  # return FALSE here, otherwise tephric_material's exclusion
  # logic mis-fires.
  pr <- make_andosol_canonical()
  pr$horizons$al_ox_pct               <- 0.1
  pr$horizons$fe_ox_pct               <- 0.1
  pr$horizons$bulk_density_g_cm3      <- 1.5
  pr$horizons$phosphate_retention_pct <- 0
  # volcanic_glass_pct = c(35, 25, 10) -- still high but no longer
  # qualifies as andic.
  expect_false(isTRUE(andic_properties(pr)$passed))
})

test_that("andic threshold is 4x stricter than spodic", {
  # The Podzol fixture passes spodic (>= 0.5%) but should fail andic (>= 2.0%)
  pr <- make_podzol_canonical()
  expect_true (isTRUE(spodic(pr)$passed))
  expect_false(isTRUE(andic_properties(pr)$passed))
})

test_that("fluvic_material passes on canonical Fluvisol fixture", {
  res <- fluvic_material(make_fluvisol_canonical())
  expect_true(isTRUE(res$passed))
})

test_that("fluvic_material fails on monotone-textured profiles", {
  for (fn in list(make_ferralsol_canonical, make_luvisol_canonical,
                    make_chernozem_canonical)) {
    expect_false(isTRUE(fluvic_material(fn())$passed))
  }
})

test_that("test_oc_above counts only layers above threshold", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    oc_pct = c(15, 0.5)
  )
  res <- test_oc_above(h, min_pct = 12)
  expect_equal(res$layers, 1L)
})

test_that("test_designation_pattern matches case-insensitively", {
  h <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    designation = c("A", "Cr1")
  )
  res <- test_designation_pattern(h, pattern = "^Cr")
  expect_equal(res$layers, 2L)
})

test_that("test_andic_alfe correctly applies the 0.5x weight to Fe", {
  h <- data.table::data.table(
    top_cm = 0, bottom_cm = 30,
    al_ox_pct = 1.5, fe_ox_pct = 1.0
  )
  # 1.5 + 0.5 = 2.0
  res <- test_andic_alfe(h, min_pct = 2.0)
  expect_true(isTRUE(res$passed))
  expect_equal(res$details[[1]]$al_plus_half_fe, 2.0)
})
