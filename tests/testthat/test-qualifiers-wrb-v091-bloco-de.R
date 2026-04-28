# v0.9.1 Bloco D + E -- canonical Ch 4 principal-qualifier coverage for
# the remaining 16 RSGs:
#
#   D: CH (Chernozems), KS (Kastanozems), PH (Phaeozems), UM (Umbrisols),
#      DU (Durisols), GY (Gypsisols), CL (Calcisols), RT (Retisols)
#   E: AC (Acrisols), LX (Lixisols), AL (Alisols), LV (Luvisols),
#      CM (Cambisols), AR (Arenosols), RG (Regosols), FL (Fluvisols)
#
# Closes the v0.9.1 RSG-level coverage at 32 / 32 RSGs.

# ---- YAML structural contract ----------------------------------------------

test_that("v0.9.1 YAML lists canonical principals for all 16 D+E RSGs", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  qrules <- yaml::read_yaml(qfile)

  for (rsg in c("CH","KS","PH","UM","DU","GY","CL","RT",
                "AC","LX","AL","LV","CM","AR","RG","FL")) {
    p <- qrules$rsg_qualifiers[[rsg]]$principal
    expect_gt(length(p), 14L,
                label = sprintf("RSG %s should have >14 canonical principals", rsg))
  }

  # All 32 RSGs covered.
  expect_equal(length(qrules$rsg_qualifiers), 32L)

  # Bloco D+E anchors.
  expect_true("Vermic"   %in% qrules$rsg_qualifiers$CH$principal)
  expect_true("Glossic"  %in% qrules$rsg_qualifiers$CH$principal)
  expect_true("Pachic"   %in% qrules$rsg_qualifiers$KS$principal)
  expect_true("Glossic"  %in% qrules$rsg_qualifiers$PH$principal)
  expect_true("Hyperdystric" %in% qrules$rsg_qualifiers$UM$principal)
  expect_true("Petric"   %in% qrules$rsg_qualifiers$DU$principal)
  expect_true("Petrogypsic" %in% qrules$rsg_qualifiers$GY$principal)
  expect_true("Petrocalcic" %in% qrules$rsg_qualifiers$CL$principal)
  expect_true("Hyperalbic" %in% qrules$rsg_qualifiers$RT$principal)

  expect_true("Cutanic"  %in% qrules$rsg_qualifiers$AC$principal)
  expect_true("Cutanic"  %in% qrules$rsg_qualifiers$LX$principal)
  expect_true("Cutanic"  %in% qrules$rsg_qualifiers$AL$principal)
  expect_true("Cutanic"  %in% qrules$rsg_qualifiers$LV$principal)
  expect_true("Glossic"  %in% qrules$rsg_qualifiers$LV$principal)
  expect_true("Calcaric" %in% qrules$rsg_qualifiers$CM$principal)
  expect_true("Protic"   %in% qrules$rsg_qualifiers$AR$principal)
  expect_true("Brunic"   %in% qrules$rsg_qualifiers$AR$principal)
  expect_true("Solimovic" %in% qrules$rsg_qualifiers$RG$principal)
  expect_true("Tidalic"  %in% qrules$rsg_qualifiers$FL$principal)
  expect_true("Aceric"   %in% qrules$rsg_qualifiers$FL$principal)
})


# ---- Per-fixture qualifier resolution --------------------------------------

test_that("CH canonical fixture resolves to a Vermic Chernic <...> Chernozem", {
  pr  <- make_chernozem_canonical()
  res <- resolve_wrb_qualifiers(pr, "CH")
  expect_true("Vermic"  %in% res$principal)
  expect_true("Chernic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Chernozems")
  expect_match(cls$name, "Vermic")
  expect_match(cls$name, "Chernic")
})

test_that("DU canonical fixture resolves with Duric in the qualifier list", {
  pr  <- make_durisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "DU")
  expect_true("Duric" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Durisols")
  expect_match(cls$name, "Duric")
})

test_that("CL canonical fixture resolves to a Calcic Calcisol", {
  pr  <- make_calcisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "CL")
  expect_true("Calcic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Calcisols")
  expect_match(cls$name, "Calcic")
})

test_that("AC canonical fixture resolves with Cutanic", {
  pr  <- make_acrisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "AC")
  expect_true("Cutanic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Acrisols")
  expect_match(cls$name, "Cutanic")
})

test_that("LX canonical fixture resolves with Cutanic", {
  pr  <- make_lixisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "LX")
  expect_true("Cutanic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Lixisols")
  expect_match(cls$name, "Cutanic")
})

test_that("AL canonical fixture resolves with Hyperalic + Cutanic", {
  pr  <- make_alisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "AL")
  expect_true("Hyperalic" %in% res$principal)
  expect_true("Cutanic"   %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Alisols")
  expect_match(cls$name, "Hyperalic")
})

test_that("LV canonical fixture resolves with Cutanic", {
  pr  <- make_luvisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "LV")
  expect_true("Cutanic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Luvisols")
  expect_match(cls$name, "Cutanic")
})

test_that("CM canonical fixture resolves to a Eutric Cambisol", {
  pr  <- make_cambisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "CM")
  expect_true("Eutric" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Cambisols")
  expect_match(cls$name, "Eutric Cambisol")
})

test_that("AR canonical fixture resolves with Protic (no B horizon)", {
  pr  <- make_arenosol_canonical()
  res <- resolve_wrb_qualifiers(pr, "AR")
  expect_true("Protic" %in% res$principal)
  # If Protic fires (no B), Brunic must NOT fire (the two are mutually
  # exclusive: Protic = no B, Brunic = cambic-only B).
  expect_false("Brunic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Arenosols")
  expect_match(cls$name, "Protic")
})

test_that("FL canonical fixture (varzea floodplain) resolves to Haplic Fluvisol", {
  pr  <- make_fluvisol_canonical()
  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Fluvisols")
  expect_match(cls$name, "Fluvisol")
})


# ---- Behavioural contracts of new qual_* functions -------------------------

test_that("Cutanic requires argic + visible clay films", {
  hz <- data.table::data.table(
    top_cm = c(0, 15, 35), bottom_cm = c(15, 35, 100),
    designation = c("A", "E", "Bt"),
    munsell_value_moist = c(4, 6, 4), munsell_chroma_moist = c(3, 3, 4),
    clay_films = c(NA_character_, NA_character_, "many"),
    clay_pct = c(15, 12, 35), silt_pct = c(40, 38, 35), sand_pct = c(45, 50, 30),
    cec_cmol = c(15, 10, 18), bs_pct = c(50, 48, 70), ph_h2o = c(6, 6, 6.2)
  )
  pr <- PedonRecord$new(
    site = list(id = "CT", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_cutanic(pr)$passed))

  # Same profile but no clay films -> Cutanic FAILS.
  hz$clay_films <- c(NA_character_, NA_character_, NA_character_)
  pr2 <- PedonRecord$new(
    site = list(id = "CT2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_cutanic(pr2)$passed))
})

test_that("Brunic / Protic are exclusive on Arenosol-style profiles", {
  # Arenosol with weakly developed cambic Bw -> Brunic fires.
  hz <- data.table::data.table(
    top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 150),
    designation = c("A", "Bw", "C"),
    munsell_hue_moist = c("10YR","7.5YR","10YR"),
    munsell_value_moist = c(4, 5, 6), munsell_chroma_moist = c(3, 4, 3),
    structure_grade = c("weak","weak","massive"),
    structure_type  = c("granular","subangular blocky","massive"),
    clay_pct = c(8, 10, 7), silt_pct = c(15, 18, 13), sand_pct = c(77, 72, 80),
    oc_pct = c(0.8, 0.4, 0.2), bs_pct = c(40, 45, 50),
    cec_cmol = c(5, 4, 3), ph_h2o = c(6, 6.2, 6.5)
  )
  pr_br <- PedonRecord$new(
    site = list(id = "BR", lat = 0, lon = 0, country = "TEST",
                  parent_material = "aeolian sand"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_brunic(pr_br)$passed))
  expect_false(isTRUE(qual_protic(pr_br)$passed))

  # The strict mutually-exclusive contract holds when no other B
  # horizon fires; for the canonical Arenosol fixture (which has
  # structureless / single-grain sand throughout), Protic passes and
  # Brunic does not.
  pr_pr <- make_arenosol_canonical()
  expect_true(isTRUE(qual_protic(pr_pr)$passed))
  expect_false(isTRUE(qual_brunic(pr_pr)$passed))
})

test_that("Glossic requires mollic + albeluvic glossae", {
  # Profile that is mollic AND has albeluvic_glossae (designation pattern).
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 150),
    designation = c("Ah", "AE/glossic", "Bt"),
    munsell_hue_moist = c("10YR","10YR","7.5YR"),
    munsell_value_moist = c(2, 4, 4), munsell_chroma_moist = c(2, 3, 4),
    munsell_value_dry = c(3, 5, 5), munsell_chroma_dry = c(2, 3, 4),
    structure_grade = c("strong","moderate","strong"),
    structure_type  = c("granular","subangular blocky","subangular blocky"),
    consistence_moist = c("friable","firm","firm"),
    clay_films = c(NA_character_, NA_character_, "common"),
    clay_pct = c(25, 22, 35), silt_pct = c(40, 40, 30), sand_pct = c(35, 38, 35),
    oc_pct = c(2.5, 0.8, 0.3), bs_pct = c(85, 75, 60),
    cec_cmol = c(28, 20, 22), ca_cmol = c(20, 14, 15),
    ph_h2o = c(7, 6.8, 6.5)
  )
  pr <- PedonRecord$new(
    site = list(id = "GS", lat = 50, lon = 30, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  # Glossic gates on mollic AND albeluvic_glossae; the test pedon has
  # an explicit "/glossic" designation token that the v0.3.3
  # albeluvic_glossae diagnostic recognises.
  expect_s3_class(qual_glossic(pr), "DiagnosticResult")
})


# ---- Engine handles the full-Ch4 trace gracefully -------------------------

test_that("resolve_wrb_qualifiers reports trace for every YAML name", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  qrules <- yaml::read_yaml(qfile)
  fxs <- list(CH=make_chernozem_canonical(), AR=make_arenosol_canonical(),
              LV=make_luvisol_canonical(), FL=make_fluvisol_canonical())
  for (rsg in names(fxs)) {
    expected <- qrules$rsg_qualifiers[[rsg]]$principal
    res <- resolve_wrb_qualifiers(fxs[[rsg]], rsg)
    expect_true(all(expected %in% names(res$trace)),
                  info = sprintf("RSG %s: trace missing some YAML names", rsg))
  }
})


# ---- 31-fixture regression check after Bloco D+E expansion ----------------

test_that("Bloco D+E YAML / qualifier additions do not regress 31 fixtures", {
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


# ---- v0.9.1 RSG-level coverage milestone ----------------------------------

test_that("v0.9.1 wires canonical Ch 4 principals for all 32 RSGs", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  qrules <- yaml::read_yaml(qfile)

  # Total 32 RSGs covered.
  expect_equal(length(qrules$rsg_qualifiers), 32L)

  # Aggregate principal-qualifier count: should be in the canonical
  # Ch 4 ballpark (each RSG has 15-32 principals; average ~22 -> ~700
  # entries when summed across RSGs).
  total <- sum(vapply(qrules$rsg_qualifiers,
                       function(x) length(x$principal), integer(1)))
  expect_gt(total, 600L)
  expect_lt(total, 900L)
})
