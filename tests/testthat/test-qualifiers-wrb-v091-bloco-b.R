# v0.9.1 Bloco B -- canonical Ch 4 principal-qualifier coverage for
# Solonetz, Vertisols, Solonchaks, Gleysols, Andosols.

# ---- YAML structural contract ----------------------------------------------

test_that("v0.9.1 YAML lists the canonical Bloco B principal qualifiers", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  qrules <- yaml::read_yaml(qfile)

  expect_gt(length(qrules$rsg_qualifiers$SN$principal), 18L)
  expect_gt(length(qrules$rsg_qualifiers$VR$principal), 18L)
  expect_gt(length(qrules$rsg_qualifiers$SC$principal), 18L)
  expect_gt(length(qrules$rsg_qualifiers$GL$principal), 25L)
  expect_gt(length(qrules$rsg_qualifiers$AN$principal), 25L)

  # Anchor qualifiers per RSG.
  expect_true("Mazic"     %in% qrules$rsg_qualifiers$VR$principal)
  expect_true("Grumic"    %in% qrules$rsg_qualifiers$VR$principal)
  expect_true("Pellic"    %in% qrules$rsg_qualifiers$VR$principal)
  expect_true("Aluandic"  %in% qrules$rsg_qualifiers$AN$principal)
  expect_true("Silandic"  %in% qrules$rsg_qualifiers$AN$principal)
  expect_true("Hydric"    %in% qrules$rsg_qualifiers$AN$principal)
  expect_true("Melanic"   %in% qrules$rsg_qualifiers$AN$principal)
  expect_true("Aceric"    %in% qrules$rsg_qualifiers$SC$principal)
  expect_true("Tidalic"   %in% qrules$rsg_qualifiers$GL$principal)
  expect_true("Albic"     %in% qrules$rsg_qualifiers$SN$principal)

  # Gleyic / Salic are NOT principal qualifiers of GL / SC -- they are
  # the gating diagnostics of those RSGs and listing them as qualifier
  # would be redundant (per WRB Ch 4 convention).
  expect_false("Gleyic" %in% qrules$rsg_qualifiers$GL$principal)
  expect_false("Salic"  %in% qrules$rsg_qualifiers$SC$principal)
})


# ---- Per-fixture qualifier resolution --------------------------------------

test_that("SN canonical fixture resolves to a Solonetz with Albic", {
  pr  <- make_solonetz_canonical()
  res <- resolve_wrb_qualifiers(pr, "SN")
  expect_true("Albic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Solonetz")
  expect_match(cls$name, "Solonetz")
})

test_that("VR canonical fixture resolves to a (default) Haplic Vertisol", {
  pr  <- make_vertisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "VR")
  # The synthetic Mozambique Vertisol fixture has subangular blocky
  # surface structure with chroma 4 -- none of Mazic / Grumic / Pellic
  # is appropriate, so resolution should fall back to Haplic.
  expect_false("Mazic"  %in% res$principal)
  expect_false("Grumic" %in% res$principal)
  expect_false("Pellic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Vertisols")
  expect_match(cls$name, "Vertisol")
})

test_that("SC canonical fixture resolves to a Sodic Solonchak", {
  pr  <- make_solonchak_canonical()
  res <- resolve_wrb_qualifiers(pr, "SC")
  expect_true("Sodic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Solonchaks")
  expect_match(cls$name, "Solonchak")
})

test_that("GL canonical fixture resolves to a (default) Haplic Gleysol", {
  pr  <- make_gleysol_canonical()
  res <- resolve_wrb_qualifiers(pr, "GL")
  # Holocene fluvial-clay Gleysol with grassland: gleyic by RSG (so
  # Gleyic itself is not a qualifier) and no other Ch 4 principal fires.
  expect_equal(res$principal, "Haplic")

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Gleysols")
  expect_match(cls$name, "Haplic Gleysol")
})

test_that("AN canonical fixture resolves to a Silandic Hydric Melanic Andosol", {
  pr  <- make_andosol_canonical()
  res <- resolve_wrb_qualifiers(pr, "AN")
  expect_true("Vitric"   %in% res$principal)
  expect_true("Silandic" %in% res$principal)
  expect_true("Hydric"   %in% res$principal)
  expect_true("Melanic"  %in% res$principal)
  # Aluandic and Silandic are mutually exclusive -- AN fixture is Si-rich.
  expect_false("Aluandic" %in% res$principal)
  # Eutrosilic gates on BS >= 50%; AN fixture has BS=15-18% -> no.
  expect_false("Eutrosilic" %in% res$principal)
  # Acroxic gates on ECEC <= 2 cmol/kg; AN fixture has ECEC ~5-7.5 -> no.
  expect_false("Acroxic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Andosols")
  expect_match(cls$name, "Silandic")
  expect_match(cls$name, "Hydric")
  expect_match(cls$name, "Melanic")
})


# ---- Behavioural contracts of new qual_* functions -------------------------

test_that("Aluandic / Silandic split correctly on the Al/Si mass ratio", {
  base_hz <- function(al, si) data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("Ah", "Bw"),
    al_ox_pct  = c(al, al),
    si_ox_pct  = c(si, si),
    fe_ox_pct  = c(1.0, 0.8),
    phosphate_retention_pct = c(95, 90),
    bulk_density_g_cm3 = c(0.8, 0.9),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  mk <- function(al, si) PedonRecord$new(
    site = list(id = "AL", lat = 0, lon = 0, country = "TEST",
                  parent_material = "tephra"),
    horizons = ensure_horizon_schema(base_hz(al, si))
  )
  # Threshold: Aluandic when Si <= 2*Al, Silandic when Si > 2*Al.
  pr_al <- mk(al = 2.0, si = 1.5)   # Si < 2*Al -> Aluandic
  pr_si <- mk(al = 2.0, si = 5.0)   # Si > 2*Al -> Silandic
  expect_true(isTRUE(qual_aluandic(pr_al)$passed))
  expect_false(isTRUE(qual_silandic(pr_al)$passed))
  expect_false(isTRUE(qual_aluandic(pr_si)$passed))
  expect_true(isTRUE(qual_silandic(pr_si)$passed))
})

test_that("Hydric requires water_content_1500kpa in upper 100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("Ah", "Bw"),
    al_ox_pct = c(2.0, 1.5), si_ox_pct = c(1.0, 0.8),
    fe_ox_pct = c(1.0, 0.8), phosphate_retention_pct = c(95, 90),
    water_content_1500kpa = c(110, 80),
    bulk_density_g_cm3 = c(0.8, 0.9),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "HY", lat = 0, lon = 0, country = "TEST",
                  parent_material = "tephra"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_hydric(pr)$passed))

  # Below threshold -> fails.
  hz$water_content_1500kpa <- c(60, 40)
  pr2 <- PedonRecord$new(
    site = list(id = "HY2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "tephra"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_hydric(pr2)$passed))
})

test_that("Melanic requires dark high-OC andic over >= 30 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 35), bottom_cm = c(35, 80),
    designation = c("Ah", "Bw"),
    munsell_hue_moist = c("10YR", "10YR"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    al_ox_pct = c(2.0, 1.5), si_ox_pct = c(1.0, 0.8),
    fe_ox_pct = c(1.0, 0.8), phosphate_retention_pct = c(95, 90),
    oc_pct = c(8, 1.5),
    bulk_density_g_cm3 = c(0.8, 0.9),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "ME", lat = 0, lon = 0, country = "TEST",
                  parent_material = "tephra under grassland"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_melanic(pr)$passed))
})

test_that("Pachic fires only when mollic OR umbric thickness >= 50 cm", {
  # Thick mollic-style horizon (60 cm A with high OC, BS, dark).
  hz <- data.table::data.table(
    top_cm = c(0, 60), bottom_cm = c(60, 150),
    designation = c("A1", "Bw"),
    munsell_hue_moist = c("10YR", "10YR"),
    munsell_value_moist = c(2, 4),
    munsell_chroma_moist = c(2, 3),
    munsell_value_dry = c(3, 5), munsell_chroma_dry = c(2, 3),
    structure_grade = c("strong", "moderate"),
    structure_type = c("granular", "subangular blocky"),
    consistence_moist = c("friable", "firm"),
    oc_pct = c(2.5, 0.6), bs_pct = c(80, 65),
    cec_cmol = c(25, 18),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40),
    ph_h2o = c(6.5, 6.5),
    bulk_density_g_cm3 = c(1.1, 1.3)
  )
  pr <- PedonRecord$new(
    site = list(id = "PA", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_pachic(pr)$passed))
})

test_that("Pellic / Grumic / Mazic key on surface structure & color", {
  base <- function(v1, c1, v2, c2, grade1, type1, grade2 = "strong",
                   type2 = "wedge-shaped") {
    data.table::data.table(
      top_cm = c(0, 25), bottom_cm = c(25, 100),
      designation = c("A", "Bss"),
      munsell_hue_moist = c("10YR", "10YR"),
      munsell_value_moist = c(v1, v2),
      munsell_chroma_moist = c(c1, c2),
      structure_grade = c(grade1, grade2),
      structure_type  = c(type1,  type2),
      structure_size  = c("medium", "coarse"),
      clay_pct = c(50, 55), silt_pct = c(30, 28), sand_pct = c(20, 17)
    )
  }
  mk <- function(...) PedonRecord$new(
    site = list(id = "P", lat = 0, lon = 0, country = "TEST",
                  parent_material = "smectitic alluvium"),
    horizons = ensure_horizon_schema(base(...))
  )
  # Pellic-dark profile: every layer has low chroma -> Pellic fires;
  # subangular-blocky surface -> neither Grumic nor Mazic.
  pr_pel <- mk(v1 = 3, c1 = 1, v2 = 4, c2 = 2,
               grade1 = "weak", type1 = "subangular blocky")
  expect_true(isTRUE(qual_pellic(pr_pel)$passed))
  expect_false(isTRUE(qual_grumic(pr_pel)$passed))
  expect_false(isTRUE(qual_mazic(pr_pel)$passed))

  # Strong fine granular surface -> Grumic; surface chroma <= 2 ->
  # Pellic also fires legitimately (the two qualifiers are not
  # mutually exclusive in WRB -- Mazic / Grumic / Pellic gate on
  # different attributes).
  pr_gru <- mk(v1 = 3, c1 = 2, v2 = 4, c2 = 2,
               grade1 = "strong", type1 = "granular")
  expect_true(isTRUE(qual_grumic(pr_gru)$passed))
  expect_true(isTRUE(qual_pellic(pr_gru)$passed))
  expect_false(isTRUE(qual_mazic(pr_gru)$passed))

  # Massive (slaked-crust) surface, chroma >= 3 throughout the upper
  # 30 cm -> Mazic only.
  pr_maz <- mk(v1 = 5, c1 = 3, v2 = 5, c2 = 3,
               grade1 = "massive", type1 = "massive",
               grade2 = "moderate", type2 = "subangular blocky")
  expect_true(isTRUE(qual_mazic(pr_maz)$passed))
  expect_false(isTRUE(qual_grumic(pr_maz)$passed))
  expect_false(isTRUE(qual_pellic(pr_maz)$passed))
})

test_that("Aceric fires when pH H2O <= 5 in upper 50 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("Ay", "By"),
    ph_h2o = c(4.2, 5.5), oc_pct = c(2.0, 0.5),
    clay_pct = c(25, 30), silt_pct = c(40, 38), sand_pct = c(35, 32),
    sulfidic_s_pct = c(0.05, 0.02)
  )
  pr <- PedonRecord$new(
    site = list(id = "AC", lat = 0, lon = 0, country = "TEST",
                  parent_material = "former tidal flat"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_aceric(pr)$passed))
})

test_that("Acroxic requires andic + sum exch <= 2 cmol/kg", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("Ah", "Bw"),
    al_ox_pct = c(2.5, 2.0), si_ox_pct = c(0.5, 0.4),
    fe_ox_pct = c(1.0, 0.8), phosphate_retention_pct = c(98, 96),
    ca_cmol = c(0.4, 0.3), mg_cmol = c(0.3, 0.2),
    k_cmol  = c(0.2, 0.1), na_cmol = c(0.1, 0.1),
    al_kcl_cmol = c(0.5, 0.4),
    bulk_density_g_cm3 = c(0.8, 0.85),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "AX", lat = 0, lon = 0, country = "TEST",
                  parent_material = "tephra"),
    horizons = ensure_horizon_schema(hz)
  )
  # Sum of bases + Al = 1.5 / 1.1 -> both <= 2 -> Acroxic passes.
  expect_true(isTRUE(qual_acroxic(pr)$passed))
})

test_that("Eutrosilic requires silandic + BS >= 50%", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("Ah", "Bw"),
    al_ox_pct = c(2.0, 1.5), si_ox_pct = c(5.0, 4.0),
    fe_ox_pct = c(1.0, 0.8), phosphate_retention_pct = c(98, 96),
    bs_pct = c(65, 70),
    bulk_density_g_cm3 = c(0.8, 0.9),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "EU", lat = 0, lon = 0, country = "TEST",
                  parent_material = "fertile tephra"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_silandic(pr)$passed))
  expect_true(isTRUE(qual_eutrosilic(pr)$passed))

  # Same fixture but low BS -> Silandic still fires, Eutrosilic does not.
  hz$bs_pct <- c(20, 15)
  pr2 <- PedonRecord$new(
    site = list(id = "EU2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "fertile tephra"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_silandic(pr2)$passed))
  expect_false(isTRUE(qual_eutrosilic(pr2)$passed))
})

test_that("Plaggic anthropic-evidence gate: rejects mollic-only A horizons", {
  # Thick OC-rich A with no anthropic evidence (no P, no artefacts,
  # no Apl-family designation) -> Plaggic must NOT fire even though
  # the v0.3.3 plaggic diagnostic itself would pass.
  hz <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 100),
    designation = c("A1", "Bw"),
    oc_pct = c(2.5, 0.6),
    bulk_density_g_cm3 = c(1.2, 1.4),
    clay_pct = c(25, 28), silt_pct = c(40, 38), sand_pct = c(35, 34)
  )
  pr_natural <- PedonRecord$new(
    site = list(id = "N", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  # The bare diagnostic might pass on this profile, but the qualifier
  # gate requires anthropic evidence -- so qual_plaggic should fail.
  expect_false(isTRUE(qual_plaggic(pr_natural)$passed))

  # Add elevated P -> Plaggic now fires.
  hz$p_mehlich3_mg_kg <- c(120, 30)
  pr_anthro <- PedonRecord$new(
    site = list(id = "A", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess + sustained sod amendments"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_plaggic(pr_anthro)$passed))
})


# ---- Engine handles unimplemented Ch 4 names without breaking --------------

test_that("resolve_wrb_qualifiers tags missing functions across Bloco B", {
  for (rsg in c("SN","VR","SC","GL","AN")) {
    qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
    if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
    expected <- yaml::read_yaml(qfile)$rsg_qualifiers[[rsg]]$principal
    fx <- switch(rsg,
      SN = make_solonetz_canonical(), VR = make_vertisol_canonical(),
      SC = make_solonchak_canonical(), GL = make_gleysol_canonical(),
      AN = make_andosol_canonical())
    res <- resolve_wrb_qualifiers(fx, rsg)
    expect_true(all(expected %in% names(res$trace)),
                  info = sprintf("RSG %s: trace missing some YAML names", rsg))
  }
})


# ---- 31-fixture regression check after Bloco B expansion ------------------

test_that("Bloco B YAML / qualifier additions do not regress 31 fixtures", {
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
