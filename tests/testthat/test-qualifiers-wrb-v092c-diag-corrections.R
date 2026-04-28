# v0.9.2.C -- diagnostic corrections (cambic depth-gate, plaggic
# anthropic-evidence gate at the diagnostic level, sombric OC-
# illuviation tightening).

# ---- Cambic now requires top_cm >= 5 cm -----------------------------------

test_that("cambic does NOT fire on a thick A horizon at top_cm = 0", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    designation = c("A", "C"),
    structure_grade = c("moderate", "massive"),
    structure_type  = c("granular", "massive"),
    consistence_moist = c("friable", "firm"),
    clay_pct = c(20, 18), silt_pct = c(40, 35), sand_pct = c(40, 47),
    oc_pct = c(2.0, 0.4), bs_pct = c(60, 70),
    bulk_density_g_cm3 = c(1.20, 1.45)
  )
  pr <- PedonRecord$new(
    site = list(id = "AonC", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- cambic(pr)
  expect_named(res$evidence,
               c("subsurface", "thickness", "texture",
                 "structure_development",
                 "not_argic", "not_ferralic"))
  expect_false(isTRUE(res$passed))
})

test_that("cambic still fires when the candidate layer is subsurface", {
  hz <- data.table::data.table(
    top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
    designation = c("A", "Bw", "C"),
    structure_grade = c("moderate", "moderate", "massive"),
    structure_type  = c("granular", "subangular blocky", "massive"),
    consistence_moist = c("friable", "firm", "firm"),
    clay_pct = c(20, 22, 18), silt_pct = c(40, 38, 35), sand_pct = c(40, 40, 47),
    oc_pct = c(2.0, 0.6, 0.3), bs_pct = c(60, 65, 70),
    bulk_density_g_cm3 = c(1.20, 1.40, 1.45)
  )
  pr <- PedonRecord$new(
    site = list(id = "Bw", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(cambic(pr)$passed))
})

test_that("Brunic respects cambic's depth-gate fix on A-over-C profiles", {
  # An A over C with no Bw -> cambic FAILS (subsurface gate) -> Brunic
  # FAILS too. Earlier (v0.9.1) Brunic could still fire because cambic
  # passed on the A horizon.
  hz <- data.table::data.table(
    top_cm = c(0, 20), bottom_cm = c(20, 150),
    designation = c("A", "C"),
    structure_grade = c("moderate", "single grain"),
    structure_type  = c("granular", "single grain"),
    clay_pct = c(8, 4), silt_pct = c(15, 5), sand_pct = c(77, 91),
    oc_pct = c(0.8, 0.1), bs_pct = c(40, 50),
    bulk_density_g_cm3 = c(1.30, 1.55)
  )
  pr <- PedonRecord$new(
    site = list(id = "AC", lat = 0, lon = 0, country = "TEST",
                  parent_material = "aeolian sand"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_brunic(pr)$passed))
  expect_true(isTRUE(qual_protic(pr)$passed))
})


# ---- Plaggic anthropic-evidence gate is now in the diagnostic --------------

test_that("plaggic FAILS without anthropogenic evidence (P / artefacts / Apl)", {
  # Mollic-style A horizon (high OC, low BD, thick) but no P, no
  # artefacts, no Apl-family designation -> plaggic must FAIL.
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 150),
    designation = c("Ah", "Bw", "C"),
    structure_grade = c("strong", "moderate", "weak"),
    structure_type  = c("granular", "subangular blocky", "massive"),
    clay_pct = c(20, 22, 18), silt_pct = c(45, 40, 35), sand_pct = c(35, 38, 47),
    oc_pct = c(3.0, 0.6, 0.3), bs_pct = c(75, 70, 70),
    bulk_density_g_cm3 = c(1.20, 1.40, 1.50)
  )
  pr <- PedonRecord$new(
    site = list(id = "natural", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(plaggic(pr)$passed))
  expect_false(isTRUE(qual_plaggic(pr)$passed))
})

test_that("plaggic FIRES when Mehlich-3 P >= 50 mg/kg in the candidate layer", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 150),
    designation = c("Ah", "Bw"),
    structure_grade = c("strong", "moderate"),
    structure_type  = c("granular", "subangular blocky"),
    clay_pct = c(20, 22), silt_pct = c(45, 40), sand_pct = c(35, 38),
    oc_pct = c(2.5, 0.6), bs_pct = c(75, 70),
    p_mehlich3_mg_kg = c(180, 30),  # high P from sustained sod input
    bulk_density_g_cm3 = c(1.15, 1.40)
  )
  pr <- PedonRecord$new(
    site = list(id = "P", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess + centuries of sod amendments"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(plaggic(pr)$passed))
  expect_true(isTRUE(qual_plaggic(pr)$passed))
})

test_that("plaggic FIRES when designation matches the Apl family", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 150),
    designation = c("Aplh", "Bw"),
    structure_grade = c("strong", "moderate"),
    structure_type  = c("granular", "subangular blocky"),
    clay_pct = c(20, 22), silt_pct = c(45, 40), sand_pct = c(35, 38),
    oc_pct = c(2.5, 0.6), bs_pct = c(75, 70),
    bulk_density_g_cm3 = c(1.15, 1.40)
  )
  pr <- PedonRecord$new(
    site = list(id = "Apl", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess + centuries of sod amendments"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(plaggic(pr)$passed))
})


# ---- Sombric requires OC accumulation vs the layer above ------------------

test_that("sombric FAILS when OC decreases monotonically with depth", {
  # Typical Ferralsol pattern: OC = 2.0, 1.2, 0.6, 0.3, 0.2 -- no
  # humus illuviation -> sombric must NOT fire even though the v0.3.3
  # bare OC + BS + thickness criteria would otherwise pass.
  pr <- make_ferralsol_canonical()
  expect_false(isTRUE(sombric(pr)$passed))
})

test_that("sombric FIRES when a deeper layer has higher OC than the layer above", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
    designation = c("A", "AB", "Bh"),
    munsell_hue_moist = c("10YR","10YR","10YR"),
    munsell_value_moist = c(4, 5, 3),
    munsell_chroma_moist = c(3, 3, 2),
    structure_grade = c("moderate","weak","moderate"),
    structure_type  = c("granular","subangular blocky","subangular blocky"),
    clay_pct = c(20, 18, 25), silt_pct = c(40, 35, 35), sand_pct = c(40, 47, 40),
    oc_pct = c(1.8, 0.8, 1.6),  # accumulation in Bh -- 0.8 -> 1.6
    bs_pct = c(35, 30, 25),
    cec_cmol = c(15, 12, 18),
    bulk_density_g_cm3 = c(1.10, 1.30, 1.20)
  )
  pr <- PedonRecord$new(
    site = list(id = "SOM", lat = -2, lon = 30, country = "TEST",
                  parent_material = "weathered basalt highland"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(sombric(pr)$passed))
})


# ---- 31-fixture regression check ------------------------------------------

test_that("v0.9.2.C diagnostic corrections do not regress 31 fixtures", {
  expected <- c(
    HS = "Histosols", AT = "Anthrosols", TC = "Technosols", CR = "Cryosols",
    LP = "Leptosols", SN = "Solonetz",   VR = "Vertisols", SC = "Solonchaks",
    GL = "Gleysols",  AN = "Andosols",   PZ = "Podzols",   PT = "Plinthosols",
    PL = "Planosols", ST = "Stagnosols", NT = "Nitisols",  FR = "Ferralsols",
    CH = "Chernozems", KS = "Kastanozems", PH = "Phaeozems", UM = "Umbrisols",
    DU = "Durisols",  GY = "Gypsisols", CL = "Calcisols", RT = "Retisols",
    AC = "Acrisols",  LX = "Lixisols",   AL = "Alisols",   LV = "Luvisols",
    CM = "Cambisols", AR = "Arenosols",  FL = "Fluvisols"
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
  for (k in names(fixfns)) {
    out <- classify_wrb2022(fixfns[[k]](), on_missing = "silent")$rsg_or_order
    expect_equal(out, expected[[k]],
                  info = sprintf("Fixture %s -> expected %s, got %s",
                                  k, expected[[k]], out))
  }
})
