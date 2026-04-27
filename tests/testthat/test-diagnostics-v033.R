# Tests for the v0.3.3 diagnostic additions (WRB 2022 Ch 3.1 / 3.2 / 3.3
# completeness pass).

# ---- helper -----------------------------------------------------------------

build_pedon <- function(...) {
  hz <- data.table::data.table(...)
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "v033-test", lat = 0, lon = 0, country = "TEST",
                 parent_material = "test"),
    horizons = hz
  )
}


# ---- new sub-tests ----------------------------------------------------------

test_that("test_numeric_above passes when threshold met", {
  h <- data.table::data.table(clay_pct = c(20, 35, 40))
  res <- soilKey:::test_numeric_above(h, "clay_pct", threshold = 30)
  expect_true(2L %in% res$layers)
  expect_true(3L %in% res$layers)
  expect_false(1L %in% res$layers)
})

test_that("test_numeric_above returns NA when column entirely missing", {
  h <- data.table::data.table(clay_pct = c(NA_real_, NA_real_))
  res <- soilKey:::test_numeric_above(h, "clay_pct", threshold = 30)
  expect_true(is.na(res$passed))
  expect_true("clay_pct" %in% res$missing)
})

test_that("test_pattern_match works on regex", {
  h <- data.table::data.table(designation = c("Ah", "Btg", "Bw"))
  res <- soilKey:::test_pattern_match(h, "designation", "g$")
  expect_equal(res$layers, 2L)
})

test_that("test_cemented respects the ordinal ladder", {
  h <- data.table::data.table(
    cementation_class = c("none", "weakly", "moderately", "strongly")
  )
  res <- soilKey:::test_cemented(h, min_class = "moderately")
  expect_equal(res$layers, c(3L, 4L))
})

test_that("test_claric_munsell catches a clearly bleached eluvial layer", {
  # WRB Ch 3.3.4: light colours -- value high, chroma low.
  h <- data.table::data.table(
    munsell_value_moist  = c(7, 5, 3),
    munsell_chroma_moist = c(2, 2, 2)
  )
  res <- soilKey:::test_claric_munsell(h)
  expect_true(1L %in% res$layers)   # value=7, chroma=2 hits (>=6, <=4)
  expect_true(2L %in% res$layers)   # value=5, chroma=2 hits (>=5, <=3)
  expect_false(3L %in% res$layers)  # value=3 too dark -- not claric
})

test_that("test_alfe_ox_above sums Al + 0.5 Fe correctly", {
  h <- data.table::data.table(
    al_ox_pct = c(1.0, 2.0, 0.2),
    fe_ox_pct = c(0.8, 0.5, 0.4)
  )
  # andic threshold: 2.0
  res <- soilKey:::test_alfe_ox_above(h, min_pct = 2.0)
  expect_equal(res$layers, 2L)        # 2.0 + 0.25 = 2.25
  # vitric threshold: 0.4
  res2 <- soilKey:::test_alfe_ox_above(h, min_pct = 0.4)
  expect_equal(res2$layers, c(1L, 2L, 3L))  # all >= 0.4
})


# ---- new horizons (Ch 3.1) --------------------------------------------------

test_that("albic catches a bleached E horizon (claric Munsell + thickness)", {
  pr <- build_pedon(
    top_cm                = c(0,  10,  40),
    bottom_cm             = c(10, 40,  100),
    designation           = c("Ah", "E",   "Bt"),
    munsell_value_moist   = c(3,  7,    4),
    munsell_chroma_moist  = c(2,  2,    4),
    clay_pct              = c(15, 12,   28),
    silt_pct              = c(40, 50,   30),
    sand_pct              = c(45, 38,   42),
    cec_cmol              = c(8,  5,    12),
    bs_pct                = c(60, 50,   65),
    ph_h2o                = c(5.0, 5.2, 5.8)
  )
  res <- albic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
  expect_true(2L %in% res$layers)
})

test_that("petrocalcic requires moderate-or-greater cementation", {
  pr <- build_pedon(
    top_cm    = c(0, 30, 60),
    bottom_cm = c(30, 60, 100),
    designation = c("A", "Bk", "Bkm"),
    caco3_pct  = c(2, 25, 30),
    cementation_class = c(NA, "weakly", "strongly"),
    clay_pct = c(20, 25, 25),
    silt_pct = c(40, 40, 40),
    sand_pct = c(40, 35, 35)
  )
  res <- petrocalcic(pr)
  expect_true(isTRUE(res$passed))
  expect_true(3L %in% res$layers)   # Bkm: caco3 >= 15 + strongly cemented
  expect_false(2L %in% res$layers)  # weakly cemented fails
})

test_that("vertic_horizon requires clay+slickensides+cracks+thickness", {
  pr <- build_pedon(
    top_cm      = c(0,  20,  60),
    bottom_cm   = c(20, 60, 120),
    designation = c("A", "Bss", "Css"),
    clay_pct    = c(45, 60, 60),
    silt_pct    = c(30, 25, 25),
    sand_pct    = c(25, 15, 15),
    slickensides = c(NA, "many", "many"),
    cracks_width_cm = c(NA, 1.0, 0.8)
  )
  res <- vertic_horizon(pr)
  expect_true(isTRUE(res$passed))
})

test_that("vertic_horizon fails when cracks too narrow", {
  pr <- build_pedon(
    top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
    designation = c("A", "Bw", "C"),
    clay_pct = c(45, 60, 55), silt_pct = c(30, 25, 25), sand_pct = c(25, 15, 20),
    slickensides = c(NA, "many", "few"),
    cracks_width_cm = c(NA, 0.2, 0.1)   # below 0.5
  )
  res <- vertic_horizon(pr)
  expect_false(isTRUE(res$passed))
})

test_that("thionic detects acidified sulfidic horizon", {
  pr <- build_pedon(
    top_cm = c(0, 20), bottom_cm = c(20, 50),
    designation = c("A", "Bj"),
    ph_h2o = c(3.5, 3.2),
    sulfidic_s_pct = c(0.05, 0.08),
    clay_pct = c(20, 30), silt_pct = c(30, 35), sand_pct = c(50, 35)
  )
  res <- thionic(pr)
  expect_true(isTRUE(res$passed))
})

test_that("hortic catches a high-P managed topsoil", {
  pr <- build_pedon(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("Ahh", "Bw"),
    oc_pct = c(2.5, 0.5),
    p_mehlich3_mg_kg = c(150, 30),
    clay_pct = c(20, 25), silt_pct = c(40, 40), sand_pct = c(40, 35),
    cec_cmol = c(15, 12), bs_pct = c(70, 60), ph_h2o = c(6.5, 6.0)
  )
  res <- hortic(pr)
  expect_true(isTRUE(res$passed))
  expect_true(1L %in% res$layers)
})


# ---- new properties (Ch 3.2) ------------------------------------------------

test_that("abrupt_textural_difference returns the abrupt-change layer", {
  pr <- build_pedon(
    top_cm    = c(0,   30,  50),
    bottom_cm = c(30,  50, 100),
    clay_pct  = c(15,  15,  45),   # 15 -> 45 = +30 absolute (>= 20pp)
    silt_pct  = c(40,  40,  35),
    sand_pct  = c(45,  45,  20)
  )
  res <- abrupt_textural_difference(pr)
  expect_true(isTRUE(res$passed))
  expect_true(3L %in% res$layers)
})

test_that("continuous_rock detects R designation", {
  pr <- build_pedon(
    top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 100),
    designation = c("A", "Bw", "R"),
    clay_pct = c(20, 25, NA), silt_pct = c(40, 40, NA), sand_pct = c(40, 35, NA)
  )
  res <- continuous_rock(pr)
  expect_true(isTRUE(res$passed))
  expect_true(3L %in% res$layers)
})

test_that("protocalcic_properties separates from calcic", {
  pr <- build_pedon(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("A", "Bk"),
    caco3_pct = c(0.2, 5),   # 5% -> protocalcic, not calcic (which needs >=15)
    clay_pct = c(20, 25), silt_pct = c(40, 40), sand_pct = c(40, 35)
  )
  res_proto <- protocalcic_properties(pr)
  res_calc  <- calcic(pr)
  expect_true(isTRUE(res_proto$passed))
  expect_false(isTRUE(res_calc$passed))
})

test_that("vitric_properties requires glass + Al/Fe + P retention", {
  pr <- build_pedon(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    volcanic_glass_pct = c(15, 8),
    al_ox_pct = c(0.5, 0.3),
    fe_ox_pct = c(0.4, 0.2),
    phosphate_retention_pct = c(40, 30),
    clay_pct = c(15, 20), silt_pct = c(50, 50), sand_pct = c(35, 30)
  )
  res <- vitric_properties(pr)
  expect_true(isTRUE(res$passed))
  expect_true(1L %in% res$layers)
})

test_that("yermic_properties detects desert pavement at the surface", {
  pr <- build_pedon(
    top_cm = c(0, 5, 30), bottom_cm = c(5, 30, 80),
    desert_pavement_pct = c(35, NA, NA),
    varnish_pct = c(20, NA, NA),
    clay_pct = c(10, 15, 18), silt_pct = c(30, 40, 40), sand_pct = c(60, 45, 42)
  )
  res <- yermic_properties(pr)
  expect_true(isTRUE(res$passed))
})


# ---- new materials (Ch 3.3) ------------------------------------------------

test_that("calcaric_material detects primary CaCO3 >= 2%", {
  pr <- build_pedon(
    top_cm = c(0, 20), bottom_cm = c(20, 60),
    caco3_pct = c(3, 8),
    clay_pct = c(20, 25), silt_pct = c(40, 40), sand_pct = c(40, 35)
  )
  res <- calcaric_material(pr)
  expect_true(isTRUE(res$passed))
})

test_that("hypersulfidic_material requires S>=0.01 + pH>=4", {
  pr <- build_pedon(
    top_cm = c(0, 20), bottom_cm = c(20, 60),
    sulfidic_s_pct = c(0.05, 0.02),
    ph_h2o = c(5.5, 4.2),
    clay_pct = c(20, 25), silt_pct = c(40, 40), sand_pct = c(40, 35)
  )
  res <- hypersulfidic_material(pr)
  expect_true(isTRUE(res$passed))
})

test_that("mineral_material excludes high-OC layers", {
  pr <- build_pedon(
    top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
    oc_pct = c(2, 25, 0.5),   # 25% = organic, not mineral
    clay_pct = c(20, 5, 30), silt_pct = c(40, 5, 35), sand_pct = c(40, 5, 35)
  )
  res <- mineral_material(pr)
  expect_true(isTRUE(res$passed))
  expect_false(2L %in% res$layers)   # OC=25% excluded
})

test_that("organic_material catches OC >= 20% layers", {
  pr <- build_pedon(
    top_cm = c(0, 20), bottom_cm = c(20, 60),
    oc_pct = c(35, 5),
    clay_pct = c(NA, 20), silt_pct = c(NA, 40), sand_pct = c(NA, 40)
  )
  res <- organic_material(pr)
  expect_true(isTRUE(res$passed))
  expect_equal(res$layers, 1L)
})

test_that("tephric_material requires glass>=30 + no andic/vitric", {
  pr <- build_pedon(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    volcanic_glass_pct = c(45, 5),
    al_ox_pct = c(0.1, 0.05),
    fe_ox_pct = c(0.1, 0.05),
    phosphate_retention_pct = c(15, 10),
    clay_pct = c(15, 20), silt_pct = c(45, 50), sand_pct = c(40, 30)
  )
  res <- tephric_material(pr)
  expect_true(isTRUE(res$passed))
  expect_equal(res$layers, 1L)
})


# ---- regression: all 31 canonical fixtures still classify ------------------

test_that("all 31 canonical fixtures still classify to their intended RSG", {
  expected <- c(
    HS = "Histosols", AT = "Anthrosols", TC = "Technosols",
    CR = "Cryosols",  LP = "Leptosols",  SN = "Solonetz",
    VR = "Vertisols", SC = "Solonchaks", GL = "Gleysols",
    AN = "Andosols",  PZ = "Podzols",    PT = "Plinthosols",
    PL = "Planosols", ST = "Stagnosols", NT = "Nitisols",
    FR = "Ferralsols",CH = "Chernozems", KS = "Kastanozems",
    PH = "Phaeozems", UM = "Umbrisols",  DU = "Durisols",
    GY = "Gypsisols", CL = "Calcisols",  RT = "Retisols",
    AC = "Acrisols",  LX = "Lixisols",   AL = "Alisols",
    LV = "Luvisols",  CM = "Cambisols",  AR = "Arenosols",
    FL = "Fluvisols"
  )
  fixfns <- list(
    HS = make_histosol_canonical,  AT = make_anthrosol_canonical,
    TC = make_technosol_canonical, CR = make_cryosol_canonical,
    LP = make_leptosol_canonical,  SN = make_solonetz_canonical,
    VR = make_vertisol_canonical,  SC = make_solonchak_canonical,
    GL = make_gleysol_canonical,   AN = make_andosol_canonical,
    PZ = make_podzol_canonical,    PT = make_plinthosol_canonical,
    PL = make_planosol_canonical,  ST = make_stagnosol_canonical,
    NT = make_nitisol_canonical,   FR = make_ferralsol_canonical,
    CH = make_chernozem_canonical, KS = make_kastanozem_canonical,
    PH = make_phaeozem_canonical,  UM = make_umbrisol_canonical,
    DU = make_durisol_canonical,   GY = make_gypsisol_canonical,
    CL = make_calcisol_canonical,  RT = make_retisol_canonical,
    AC = make_acrisol_canonical,   LX = make_lixisol_canonical,
    AL = make_alisol_canonical,    LV = make_luvisol_canonical,
    CM = make_cambisol_canonical,  AR = make_arenosol_canonical,
    FL = make_fluvisol_canonical
  )
  for (code in names(fixfns)) {
    pr <- fixfns[[code]]()
    res <- classify_wrb2022(pr, on_missing = "silent")
    expect_equal(res$rsg_or_order, expected[[code]],
                  info = sprintf("Fixture %s should classify as %s",
                                  code, expected[[code]]))
  }
})
