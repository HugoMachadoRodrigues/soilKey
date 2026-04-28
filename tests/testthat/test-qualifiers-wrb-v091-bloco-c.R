# v0.9.1 Bloco C -- canonical Ch 4 principal-qualifier coverage for
# Podzols, Plinthosols, Planosols, Stagnosols, Nitisols, Ferralsols
# (the bloco brasileiro / tropical -- where Latossolos and Argissolos
# of the SiBCS live as Ferralsols / Acrisols / Lixisols / Alisols).

# ---- YAML structural contract ----------------------------------------------

test_that("v0.9.1 YAML lists the canonical Bloco C principal qualifiers", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  qrules <- yaml::read_yaml(qfile)

  expect_gt(length(qrules$rsg_qualifiers$PZ$principal), 18L)
  expect_gt(length(qrules$rsg_qualifiers$PT$principal), 18L)
  expect_gt(length(qrules$rsg_qualifiers$PL$principal), 25L)
  expect_gt(length(qrules$rsg_qualifiers$ST$principal), 22L)
  expect_gt(length(qrules$rsg_qualifiers$NT$principal), 18L)
  expect_gt(length(qrules$rsg_qualifiers$FR$principal), 25L)

  # Spodic family on Podzols
  expect_true("Hyperspodic" %in% qrules$rsg_qualifiers$PZ$principal)
  expect_true("Carbic"      %in% qrules$rsg_qualifiers$PZ$principal)
  expect_true("Rustic"      %in% qrules$rsg_qualifiers$PZ$principal)
  expect_true("Ortsteinic"  %in% qrules$rsg_qualifiers$PZ$principal)
  expect_true("Placic"      %in% qrules$rsg_qualifiers$PZ$principal)
  expect_true("Densic"      %in% qrules$rsg_qualifiers$PZ$principal)

  # Low-CEC family on tropical RSGs
  expect_true("Geric"  %in% qrules$rsg_qualifiers$FR$principal)
  expect_true("Vetic"  %in% qrules$rsg_qualifiers$FR$principal)
  expect_true("Posic"  %in% qrules$rsg_qualifiers$FR$principal)
  expect_true("Geric"  %in% qrules$rsg_qualifiers$NT$principal)
  expect_true("Vetic"  %in% qrules$rsg_qualifiers$NT$principal)
  expect_true("Geric"  %in% qrules$rsg_qualifiers$PT$principal)

  # Hyperalbic for deep-bleach RSGs
  expect_true("Hyperalbic" %in% qrules$rsg_qualifiers$PL$principal)
  expect_true("Hyperalbic" %in% qrules$rsg_qualifiers$ST$principal)
  expect_true("Hyperalbic" %in% qrules$rsg_qualifiers$PZ$principal)

  # Sombric on FR (and not on PZ since spodic excludes sombric)
  expect_true("Sombric" %in% qrules$rsg_qualifiers$FR$principal)
})


# ---- Per-fixture qualifier resolution --------------------------------------

test_that("PZ canonical fixture resolves to an Albic Podzol", {
  pr  <- make_podzol_canonical()
  res <- resolve_wrb_qualifiers(pr, "PZ")
  expect_true("Albic" %in% res$principal)
  # The boreal-till PZ fixture has moderate Al-Fe-OC accumulation (Al_ox +
  # 0.5 * Fe_ox = 0.6 in Bs), so Hyperspodic (>= 1.5) must NOT fire.
  expect_false("Hyperspodic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Podzols")
  expect_match(cls$name, "Albic Podzol")
})

test_that("PT canonical fixture resolves to a Plinthic Plinthosol", {
  pr  <- make_plinthosol_canonical()
  res <- resolve_wrb_qualifiers(pr, "PT")
  expect_true("Plinthic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Plinthosols")
  expect_match(cls$name, "Plinthosol")
  expect_match(cls$name, "Plinthic")
})

test_that("PL canonical fixture resolves to an Albic Stagnic <chemistry> Planosol", {
  pr  <- make_planosol_canonical()
  res <- resolve_wrb_qualifiers(pr, "PL")
  expect_true("Albic"  %in% res$principal)
  expect_true("Stagnic" %in% res$principal)
  # Hyperalbic must NOT fire on a 10-cm E (sand 50%, clay 12%).
  expect_false("Hyperalbic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Planosols")
  expect_match(cls$name, "Planosol")
  expect_match(cls$name, "Albic")
})

test_that("ST canonical fixture resolves to an Albic Stagnosol", {
  pr  <- make_stagnosol_canonical()
  res <- resolve_wrb_qualifiers(pr, "ST")
  expect_true("Albic" %in% res$principal)
  # Hyperalbic must NOT fire: the "albic-like" run includes BC / C
  # loess parent material that the v0.9.1 eluvial guard rejects.
  expect_false("Hyperalbic" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Stagnosols")
  expect_match(cls$name, "Stagnosol")
})

test_that("NT canonical fixture resolves to a Luvic Ferric Chromic Nitisol", {
  pr  <- make_nitisol_canonical()
  res <- resolve_wrb_qualifiers(pr, "NT")
  expect_true("Luvic"   %in% res$principal)
  expect_true("Ferric"  %in% res$principal)
  expect_true("Chromic" %in% res$principal)
  # NT fixture has CEC/clay ~ 32 cmol+/kg clay -> NOT Vetic (threshold 6).
  expect_false("Vetic" %in% res$principal)
  # ECEC ~ 6-8 cmol+/kg fine earth -> NOT Geric (threshold 1.5).
  expect_false("Geric" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Nitisols")
  expect_match(cls$name, "Nitisol")
})

test_that("FR canonical fixture resolves to a Geric Ferric Rhodic Chromic Ferralsol", {
  pr  <- make_ferralsol_canonical()
  res <- resolve_wrb_qualifiers(pr, "FR")
  expect_true("Geric"   %in% res$principal)
  expect_true("Ferric"  %in% res$principal)
  expect_true("Rhodic"  %in% res$principal)
  expect_true("Chromic" %in% res$principal)
  # ECEC at layer 4 (Bw1, top=65) ~ 1.18 cmol+/kg < 1.5 -> Geric YES.
  # CEC/clay at layer 4 ~ 8.3 cmol+/kg clay > 6 -> Vetic NO.
  expect_false("Vetic" %in% res$principal)
  # Delta pH = pH_KCl - pH_H2O ~ -0.7 -> Posic NO (needs > 0).
  expect_false("Posic" %in% res$principal)
  # Spodic / ferralic exclusion -> Sombric NO on FR fixture.
  expect_false("Sombric" %in% res$principal)
  # BS = 13-24% -> not Hyperdystric (< 5) and not Hypereutric (>= 80).
  expect_false("Hyperdystric" %in% res$principal)
  expect_false("Hypereutric" %in% res$principal)

  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(cls$rsg_or_order, "Ferralsols")
  expect_match(cls$name, "Geric")
  expect_match(cls$name, "Ferralsol")
})


# ---- Behavioural contracts of new qual_* functions -------------------------

test_that("Hyperspodic, Carbic, Rustic split spodic illuviation styles", {
  base <- function(al, fe, oc) data.table::data.table(
    top_cm = c(0, 5, 30), bottom_cm = c(5, 30, 80),
    designation = c("Oa", "E", "Bs"),
    munsell_hue_moist = c("10YR", "10YR", "7.5YR"),
    munsell_value_moist = c(2, 6, 3),
    munsell_chroma_moist = c(1, 2, 4),
    al_ox_pct = c(0.05, 0.05, al), fe_ox_pct = c(0.05, 0.05, fe),
    oc_pct = c(20, 0.5, oc), bs_pct = c(6, 8, 8),
    ph_h2o = c(4.0, 4.2, 4.5), ph_kcl = c(3.5, 3.7, 4.0),
    cec_cmol = c(40, 3, 5),
    clay_pct = c(8, 5, 8), silt_pct = c(20, 10, 12), sand_pct = c(72, 85, 80),
    bulk_density_g_cm3 = c(0.4, 1.4, 1.3)
  )
  mk <- function(al, fe, oc, drainage = "well drained") PedonRecord$new(
    site = list(id = "P", lat = 0, lon = 0, country = "TEST",
                  parent_material = "sandy till",
                  drainage_class = drainage),
    horizons = ensure_horizon_schema(base(al, fe, oc))
  )
  # Strong spodic accumulation -> Hyperspodic.
  pr_hyper <- mk(al = 1.0, fe = 1.0, oc = 1.5)  # active = 1.5
  expect_true(isTRUE(qual_hyperspodic(pr_hyper)$passed))

  # Humus-dominated -> Carbic.
  pr_carb <- mk(al = 0.5, fe = 0.4, oc = 8.0)
  expect_true(isTRUE(qual_carbic(pr_carb)$passed))
  expect_false(isTRUE(qual_rustic(pr_carb)$passed))

  # Iron-dominated, low OC -> Rustic.
  pr_rust <- mk(al = 0.4, fe = 0.6, oc = 0.5)
  expect_true(isTRUE(qual_rustic(pr_rust)$passed))
  expect_false(isTRUE(qual_carbic(pr_rust)$passed))
})

test_that("Geric fires on FR via ECEC <= 1.5 cmol+/kg fine earth", {
  pr <- make_ferralsol_canonical()
  ge <- qual_geric(pr)
  expect_true(isTRUE(ge$passed))
  # The matching layers must come from the upper 100 cm.
  h <- pr$horizons
  expect_true(all(h$top_cm[ge$layers] <= 100))
})

test_that("Geric also fires when delta pH > 0 even with non-trivial ECEC", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("A", "Bw"),
    ca_cmol = c(0.5, 0.4), mg_cmol = c(0.2, 0.2),
    k_cmol  = c(0.1, 0.1), na_cmol = c(0.05, 0.05),
    al_kcl_cmol = c(0.5, 0.4),
    ph_h2o = c(5.0, 4.8), ph_kcl = c(5.2, 5.0),  # delta_ph = +0.2
    clay_pct = c(60, 65), silt_pct = c(15, 12), sand_pct = c(25, 23),
    bulk_density_g_cm3 = c(1.0, 1.1)
  )
  pr <- PedonRecord$new(
    site = list(id = "POS", lat = 0, lon = 0, country = "TEST",
                  parent_material = "deeply weathered basalt"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_geric(pr)$passed))
  expect_true(isTRUE(qual_posic(pr)$passed))
})

test_that("Vetic requires CEC/clay <= 6 cmol+/kg clay", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("A", "Bw"),
    cec_cmol = c(2.5, 2.0), clay_pct = c(50, 55),
    silt_pct = c(20, 18), sand_pct = c(30, 27),
    ph_h2o = c(5.0, 5.0)
  )
  pr <- PedonRecord$new(
    site = list(id = "VET", lat = 0, lon = 0, country = "TEST",
                  parent_material = "ancient ferralitic"),
    horizons = ensure_horizon_schema(hz)
  )
  # CEC/clay = 2.5/50*100 = 5 -> Vetic.
  expect_true(isTRUE(qual_vetic(pr)$passed))

  # Same fixture but higher CEC -> not Vetic.
  hz$cec_cmol <- c(8, 7)
  pr2 <- PedonRecord$new(
    site = list(id = "VET2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "ancient ferralitic"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_vetic(pr2)$passed))
})

test_that("Hyperdystric requires BS < 5% throughout 20-100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
    bs_pct = c(15, 3, 2),
    clay_pct = c(20, 25, 25), silt_pct = c(40, 35, 35), sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "HD", lat = 0, lon = 0, country = "TEST",
                  parent_material = "deeply weathered"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_hyperdystric(pr)$passed))

  # If any 20-100 layer has BS >= 5, fails.
  hz$bs_pct <- c(15, 8, 2)
  pr2 <- PedonRecord$new(
    site = list(id = "HD2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "deeply weathered"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_hyperdystric(pr2)$passed))
})

test_that("Hypereutric requires BS >= 80% throughout 20-100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
    bs_pct = c(60, 90, 95),
    clay_pct = c(20, 25, 25), silt_pct = c(40, 35, 35), sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "HE", lat = 0, lon = 0, country = "TEST",
                  parent_material = "carbonate-rich"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_hypereutric(pr)$passed))
})

test_that("Hyperalic requires argic horizon + Al saturation >= 50%", {
  hz <- data.table::data.table(
    top_cm = c(0, 15, 35), bottom_cm = c(15, 35, 100),
    designation = c("A", "E", "Bt"),
    munsell_value_moist = c(4, 6, 4), munsell_chroma_moist = c(3, 3, 4),
    clay_films = c(NA_character_, NA_character_, "common"),
    clay_pct = c(15, 12, 35), silt_pct = c(40, 38, 35), sand_pct = c(45, 50, 30),
    al_sat_pct = c(20, 35, 65),
    bs_pct = c(40, 30, 25),
    cec_cmol = c(15, 10, 18),
    ph_h2o = c(4.5, 4.5, 4.5)
  )
  pr <- PedonRecord$new(
    site = list(id = "HAl", lat = 0, lon = 0, country = "TEST",
                  parent_material = "acid weathering"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_hyperalic(pr)$passed))
})

test_that("Hyperalbic accumulates only contiguous eluvial-evidence albic", {
  # Giant tropical white-sand Podzol: deep contiguous E with very high
  # value, very low chroma, sand-rich; spodic Bs only at the very bottom.
  hz <- data.table::data.table(
    top_cm = c(0, 5, 30, 80, 130),
    bottom_cm = c(5, 30, 80, 130, 200),
    designation = c("Oa", "E1", "E2", "E3", "Bs"),
    munsell_hue_moist = c("10YR", "10YR", "10YR", "10YR", "7.5YR"),
    munsell_value_moist = c(2, 7, 7, 7, 4),
    munsell_chroma_moist = c(1, 1, 1, 1, 4),
    oc_pct = c(20, 0.3, 0.2, 0.1, 1.5),
    al_ox_pct = c(0.05, 0.02, 0.02, 0.02, 0.5),
    fe_ox_pct = c(0.05, 0.02, 0.02, 0.02, 0.4),
    clay_pct = c(2, 1, 1, 1, 5), silt_pct = c(5, 3, 3, 2, 5),
    sand_pct = c(93, 96, 96, 97, 90),
    ph_h2o = c(4.0, 4.5, 4.7, 4.8, 5.0),
    bulk_density_g_cm3 = c(0.4, 1.45, 1.50, 1.55, 1.40)
  )
  pr <- PedonRecord$new(
    site = list(id = "GIANT", lat = -1, lon = -65, country = "BR",
                  parent_material = "Pleistocene white sand campinarana"),
    horizons = ensure_horizon_schema(hz)
  )
  # E1+E2+E3 contiguous = 25+50+50 = 125 cm >= 100 -> Hyperalbic.
  expect_true(isTRUE(qual_hyperalbic(pr)$passed))

  # Same profile but break the contiguity (a Bt in the middle) -> NO.
  hz$designation[3] <- "Bt"; hz$clay_pct[3] <- 25
  pr2 <- PedonRecord$new(
    site = list(id = "BROKEN", lat = -1, lon = -65, country = "BR",
                  parent_material = "Pleistocene white sand campinarana"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_hyperalbic(pr2)$passed))
})

test_that("Sombric excludes layers that simultaneously meet spodic / ferralic", {
  # The FR canonical fixture passes the v0.3.3 sombric() Munsell test
  # but its candidate layers also pass ferralic; v0.9.1's qual_sombric
  # therefore correctly returns FALSE.
  pr <- make_ferralsol_canonical()
  expect_false(isTRUE(qual_sombric(pr)$passed))
  expect_true(isTRUE(sombric(pr)$passed))   # bare diagnostic still passes
})

test_that("Densic / Placic / Ortsteinic key on physical/cementation fields", {
  # Densic: BD >= 1.8 in any upper-100 cm layer.
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 80),
    designation = c("A", "C"),
    bulk_density_g_cm3 = c(1.4, 1.85),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr_dn <- PedonRecord$new(
    site = list(id = "DN", lat = 0, lon = 0, country = "TEST",
                  parent_material = "compacted till"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_densic(pr_dn)$passed))

  # Placic: thin (<= 2.5 cm) cemented Fe pan.
  hz2 <- data.table::data.table(
    top_cm = c(0, 30, 32), bottom_cm = c(30, 32, 100),
    designation = c("A", "Bsm", "BC"),
    cementation_class = c("none", "indurated", "weakly"),
    clay_pct = c(8, 5, 7), silt_pct = c(20, 10, 18), sand_pct = c(72, 85, 75)
  )
  pr_pl <- PedonRecord$new(
    site = list(id = "PLA", lat = 0, lon = 0, country = "TEST",
                  parent_material = "sandy till with iron pan"),
    horizons = ensure_horizon_schema(hz2)
  )
  expect_true(isTRUE(qual_placic(pr_pl)$passed))
})


# ---- 31-fixture regression check ------------------------------------------

test_that("Bloco C YAML / qualifier additions do not regress 31 fixtures", {
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
