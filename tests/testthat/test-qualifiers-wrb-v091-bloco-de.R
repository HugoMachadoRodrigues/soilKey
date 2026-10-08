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

  # v0.9.217: the lists are WRB 2022 Chapter 4's, shorter than the WRB 2014
  # lists the old ">14 principals" check was written for (Durisols list 7,
  # for instance). Alternatives share one entry, so names are looked up in the
  # flattened lists.
  flat <- function(x) unlist(strsplit(unlist(x), "/", fixed = TRUE))
  p   <- function(r) flat(qrules$rsg_qualifiers[[r]]$principal)
  sup <- function(r) flat(qrules$rsg_qualifiers[[r]]$supplementary)

  # All 32 RSGs covered.
  expect_equal(length(qrules$rsg_qualifiers), 32L)

  # Bloco D+E anchors, where WRB 2022 Chapter 4 puts them.
  expect_true("Vermic"    %in% p("CH"))
  expect_true("Glossic"   %in% p("PH"))
  expect_true("Petric"    %in% p("DU"))
  expect_true("Calcaric"  %in% p("CM"))
  expect_true("Protic"    %in% p("AR"))
  expect_true("Brunic"    %in% p("AR"))
  expect_true("Solimovic" %in% p("RG"))
  expect_true("Tidalic"   %in% p("FL"))
  expect_true("Pachic"    %in% sup("KS"))
  expect_true("Hyperdystric" %in% sup("UM"))
  for (r in c("AC", "LX", "AL", "LV"))
    expect_true("Cutanic" %in% sup(r), info = r)
  # in the WRB 2014 lists, not in WRB 2022's
  expect_false("Glossic"     %in% c(p("CH"), sup("CH")))
  expect_false("Glossic"     %in% c(p("LV"), sup("LV")))
  expect_false("Petrogypsic" %in% c(p("GY"), sup("GY")))
  expect_false("Petrocalcic" %in% c(p("CL"), sup("CL")))
  expect_false("Aceric"      %in% c(p("FL"), sup("FL")))
  expect_false("Hyperalbic"  %in% c(p("RT"), sup("RT")))  # not in WRB 2022 (v0.9.216)
})


# ---- Per-fixture qualifier resolution --------------------------------------

test_that("CH canonical fixture resolves to a Vermic Chernozem", {
  pr  <- make_chernozem_canonical()
  res <- resolve_wrb_qualifiers(pr, "CH")
  expect_true("Vermic"  %in% res$principal)
  # v0.9.217: the chernic horizon defines Chernozems, so WRB 2022 Chapter 4
  # does not list Chernic for them (the WRB 2014 list did)
  expect_false("Chernic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Chernozems")
  expect_match(cls$name, "Vermic")
})

test_that("DU canonical fixture: the duric horizon gives Epic, not Duric", {
  pr  <- make_durisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "DU")
  # v0.9.217: the duric horizon defines Durisols, so Chapter 4 does not list
  # Duric for them; where it starts is told by Epic/Endic (here <= 50 cm)
  expect_false("Duric" %in% res$principal)
  expect_true("Epic" %in% res$supplementary)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Durisols")
  expect_match(cls$name, "Durisol")
})

test_that("CL canonical fixture: the calcic horizon gives Epic, not Calcic", {
  pr  <- make_calcisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "CL")
  # v0.9.217: the calcic horizon defines Calcisols, so Chapter 4 does not list
  # Calcic for them; Epic says it starts <= 50 cm
  expect_false("Calcic" %in% res$principal)
  expect_true("Epic" %in% res$supplementary)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Calcisols")
  expect_match(cls$name, "Calcisol")
})

test_that("AC canonical fixture resolves with Cutanic", {
  pr  <- make_acrisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "AC")
  # v0.9.217: Cutanic is a supplementary qualifier in WRB 2022 Chapter 4
  expect_true("Cutanic" %in% res$supplementary)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Acrisols")
  expect_match(cls$name, "Cutanic")
})

test_that("LX canonical fixture resolves with Cutanic", {
  pr  <- make_lixisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "LX")
  # v0.9.217: Cutanic is a supplementary qualifier in WRB 2022 Chapter 4
  expect_true("Cutanic" %in% res$supplementary)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Lixisols")
  expect_match(cls$name, "Cutanic")
})

test_that("AL canonical fixture resolves with Hyperalic + Cutanic", {
  pr  <- make_alisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "AL")
  # v0.9.217: both are supplementary qualifiers in WRB 2022 Chapter 4
  expect_true("Hyperalic" %in% res$supplementary)
  expect_true("Cutanic"   %in% res$supplementary)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Alisols")
  expect_match(cls$name, "Hyperalic")
})

test_that("LV canonical fixture resolves with Cutanic", {
  pr  <- make_luvisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "LV")
  # v0.9.217: Cutanic is a supplementary qualifier in WRB 2022 Chapter 4
  expect_true("Cutanic" %in% res$supplementary)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Luvisols")
  expect_match(cls$name, "Cutanic")
})

test_that("CM canonical fixture resolves to a Eutric Cambisol (WRB 2022)", {
  # Under the WRB 2022 exchangeable-Al criterion the fixture (al_cmol ~0.1 vs
  # bases ~10.5, al_sat ~1%) is base-dominated by far more than 4x throughout,
  # i.e. Hypereutric -- a stricter, more-specific form than the old BS>=50
  # Eutric. The deeper bs<80 meant the old BS test could only see Eutric.
  # v0.9.217: Chapter 4 lists Dystric/Eutric for Cambisols; Hypereutric, its
  # optional subqualifier (Ch 2.3, rule 1), is not used in place of it.
  pr  <- make_cambisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "CM")
  expect_true("Eutric" %in% res$principal)
  expect_true(isTRUE(qual_hypereutric(pr)$passed))

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Cambisols")
  expect_match(cls$name, "Eutric Cambisol")
})

test_that("AR canonical fixture: sideralic properties, so not Protic", {
  pr  <- make_arenosol_canonical()
  res <- resolve_wrb_qualifiers(pr, "AR")
  # v0.9.217: WRB 2022 Protic is "showing no soil horizon development"; this
  # Arenosol has sideralic properties (and an A horizon), so it is Sideralic
  # and not Protic. Brunic and Protic still never come together.
  expect_true("Sideralic" %in% res$principal)
  expect_false("Protic" %in% res$principal)
  expect_false(all(c("Brunic", "Protic") %in% res$principal))

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Arenosols")
  expect_match(cls$name, "Sideralic Arenosol")
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
    clay_films_amount = c(NA_character_, NA_character_, "many"),
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
  hz$clay_films_amount <- c(NA_character_, NA_character_, NA_character_)
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

  # v0.9.217: WRB 2022 Protic is "showing no soil horizon development". The
  # canonical Arenosol has an A horizon and sideralic properties, so it is
  # not Protic; a sand described as C horizons only is.
  pr_pr <- make_arenosol_canonical()
  expect_false(isTRUE(qual_protic(pr_pr)$passed))
  expect_false(isTRUE(qual_brunic(pr_pr)$passed))
  bare <- data.table::data.table(top_cm = c(0, 40), bottom_cm = c(40, 150),
                                 designation = c("C1", "C2"),
                                 clay_pct = c(3, 3), sand_pct = c(95, 95))
  pr_bare <- PedonRecord$new(site = list(id = "C", country = "TEST"),
                             horizons = ensure_horizon_schema(bare))
  expect_true(isTRUE(qual_protic(pr_bare)$passed))
  expect_false(isTRUE(qual_brunic(pr_bare)$passed))
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
    clay_films_amount = c(NA_character_, NA_character_, "common"),
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
    # v0.9.217: every alternative of a slash group is traced; Haplic is not
    expected <- setdiff(unlist(strsplit(unlist(
      qrules$rsg_qualifiers[[rsg]]$principal), "/", fixed = TRUE)), "Haplic")
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

  # Aggregate principal-qualifier count, alternatives counted one by one and
  # Haplic (16 lists) left out: 643 in WRB 2022 Chapter 4 (v0.9.217).
  total <- sum(vapply(qrules$rsg_qualifiers, function(x)
    length(setdiff(unlist(strsplit(unlist(x$principal), "/", fixed = TRUE)), "Haplic")),
    integer(1)))
  expect_equal(total, 643L)
})
