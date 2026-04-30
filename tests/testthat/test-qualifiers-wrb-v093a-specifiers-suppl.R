# v0.9.3.A -- the remaining 5 specifiers (Kato / Amphi / Poly / Supra
# / Thapto) and supplementary-qualifier engine wiring.

# ---- New specifier detection -----------------------------------------------

test_that(".detect_specifier handles Kato / Amphi / Poly / Supra / Thapto", {
  for (sp in c("Kato", "Amphi", "Poly", "Supra", "Thapto")) {
    s <- soilKey:::.detect_specifier(paste0(sp, "Gleyic"))
    expect_equal(s$prefix, sp,
                  info = sprintf("expected prefix %s", sp))
    expect_equal(s$base,   "Gleyic")
    expect_equal(s$spec$kind, "filter",
                  info = sprintf("specifier %s should be filter-kind", sp))
    expect_true(is.function(s$spec$filter))
  }
})


# ---- Kato- (lower part) ---------------------------------------------------

test_that("Katoalbic keeps only albic layers with top_cm >= 50", {
  hz <- data.table::data.table(
    top_cm = c(0, 5, 30, 80, 130),
    bottom_cm = c(5, 30, 80, 130, 200),
    designation = c("Oa", "E1", "E2", "E3", "Bs"),
    munsell_value_moist = c(2, 7, 7, 7, 4),
    munsell_chroma_moist = c(1, 1, 1, 1, 4),
    al_ox_pct = c(0.05, 0.02, 0.02, 0.02, 0.5),
    fe_ox_pct = c(0.05, 0.02, 0.02, 0.02, 0.4),
    clay_pct = c(2, 1, 1, 1, 5), silt_pct = c(5, 3, 3, 2, 5),
    sand_pct = c(93, 96, 96, 97, 90),
    bulk_density_g_cm3 = c(0.4, 1.45, 1.5, 1.55, 1.4),
    ph_h2o = c(4, 4.5, 4.7, 4.8, 5.0)
  )
  pr <- PedonRecord$new(
    site = list(id = "K", lat = -1, lon = -65, country = "BR",
                  parent_material = "white sand"),
    horizons = ensure_horizon_schema(hz)
  )
  spec_kato <- soilKey:::.wrb_specifiers$Kato
  res <- soilKey:::.apply_specifier(pr, "Kato", "Albic", spec_kato)
  # Albic E1 starts at 5 (excluded), E2 at 30 (excluded),
  # E3 at 80 (kept). Kept set must contain only the layers with top >= 50.
  h <- pr$horizons
  expect_true(all(h$top_cm[res$layers] >= 50))
})


# ---- Amphi- (both upper and lower) ----------------------------------------

test_that("Amphicalcic requires calcic in BOTH 0-50 and 50-100 bands", {
  # Single calcic layer at top=20 (only upper) -> Amphi must FAIL.
  hz <- data.table::data.table(
    top_cm = c(0, 20), bottom_cm = c(20, 100),
    designation = c("A", "Bk"),
    caco3_pct = c(0, 25),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "AC", lat = 0, lon = 0, country = "TEST",
                  parent_material = "calcareous"),
    horizons = ensure_horizon_schema(hz)
  )
  spec_amphi <- soilKey:::.wrb_specifiers$Amphi
  expect_false(isTRUE(
    soilKey:::.apply_specifier(pr, "Amphi", "Calcic", spec_amphi)$passed))

  # Calcic in BOTH a 0-50 layer (top=20) and a 50-100 layer (top=60)
  # -> Amphi PASSES.
  hz2 <- data.table::data.table(
    top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
    designation = c("A", "Bk1", "Bk2"),
    caco3_pct = c(0, 22, 25),
    clay_pct = c(20, 22, 22), silt_pct = c(40, 38, 38), sand_pct = c(40, 40, 40)
  )
  pr2 <- PedonRecord$new(
    site = list(id = "AC2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "calcareous"),
    horizons = ensure_horizon_schema(hz2)
  )
  expect_true(isTRUE(
    soilKey:::.apply_specifier(pr2, "Amphi", "Calcic", spec_amphi)$passed))
})


# ---- Poly- (multiple non-contiguous occurrences) --------------------------

test_that("Polyalbic requires >= 2 disjoint contiguous albic runs", {
  # Single contiguous E (5-30) -> Poly FAILS.
  hz <- data.table::data.table(
    top_cm = c(0, 5, 30), bottom_cm = c(5, 30, 100),
    designation = c("Oa", "E", "Bs"),
    munsell_value_moist = c(2, 7, 4),
    munsell_chroma_moist = c(1, 1, 4),
    al_ox_pct = c(0.05, 0.02, 0.5),
    fe_ox_pct = c(0.05, 0.02, 0.4),
    clay_pct = c(2, 1, 5), silt_pct = c(5, 3, 5), sand_pct = c(93, 96, 90),
    bulk_density_g_cm3 = c(0.4, 1.5, 1.4)
  )
  pr <- PedonRecord$new(
    site = list(id = "P1", lat = 0, lon = 0, country = "TEST",
                  parent_material = "sand"),
    horizons = ensure_horizon_schema(hz)
  )
  spec_poly <- soilKey:::.wrb_specifiers$Poly
  expect_false(isTRUE(
    soilKey:::.apply_specifier(pr, "Poly", "Albic", spec_poly)$passed))

  # Two disjoint E runs separated by a Bt -> Poly PASSES.
  hz2 <- data.table::data.table(
    top_cm = c(0, 5, 30, 60, 100),
    bottom_cm = c(5, 30, 60, 100, 200),
    designation = c("Oa", "E1", "Bt", "E2", "BC"),
    munsell_value_moist = c(2, 7, 4, 7, 5),
    munsell_chroma_moist = c(1, 1, 4, 1, 3),
    clay_pct = c(2, 1, 25, 1, 5),
    silt_pct = c(5, 3, 30, 3, 5),
    sand_pct = c(93, 96, 45, 96, 90),
    bulk_density_g_cm3 = c(0.4, 1.5, 1.4, 1.5, 1.5)
  )
  pr2 <- PedonRecord$new(
    site = list(id = "P2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "sand"),
    horizons = ensure_horizon_schema(hz2)
  )
  expect_true(isTRUE(
    soilKey:::.apply_specifier(pr2, "Poly", "Albic", spec_poly)$passed))
})


# ---- Thapto- (in a buried soil) -------------------------------------------

test_that("Thapto- filters base layers by buried-soil 'b' designation", {
  # Profile with a buried Bwb that meets cambic. Designations 'b'
  # suffix marks the buried portion; the Thapto- filter retains those
  # layers and drops the active soil. Note: layer 2 has clay 16 to
  # avoid a ratio >= 1.4 vs. layer 3 (which would trigger argic and
  # thereby exclude cambic).
  hz <- data.table::data.table(
    top_cm = c(0, 25, 60, 110), bottom_cm = c(25, 60, 110, 200),
    designation = c("A", "C", "Bwb", "Cb"),
    structure_grade = c("moderate", "weak", "moderate", "single grain"),
    structure_type  = c("granular", "subangular blocky",
                          "subangular blocky", "single grain"),
    clay_pct = c(15, 16, 18, 6), silt_pct = c(40, 38, 38, 10),
    sand_pct = c(45, 46, 44, 84),
    oc_pct = c(1.0, 0.4, 1.5, 0.3),
    bs_pct = c(60, 65, 65, 70),
    cec_cmol = c(10, 9, 12, 5),
    bulk_density_g_cm3 = c(1.4, 1.5, 1.4, 1.6)
  )
  pr <- PedonRecord$new(
    site = list(id = "TH", lat = 0, lon = 0, country = "TEST",
                  parent_material = "alluvium over buried soil"),
    horizons = ensure_horizon_schema(hz)
  )
  spec_thapto <- soilKey:::.wrb_specifiers$Thapto
  res <- soilKey:::.apply_specifier(pr, "Thapto", "Cambic", spec_thapto)
  # Bwb (layer 3) and Cb (layer 4) end in 'b'. Cambic passes only on
  # Bwb (layer 4 is single-grain massive sand -> no cambic).
  expect_true(isTRUE(res$passed))
  expect_true(3L %in% res$layers)

  # Same profile but no buried marker -> Thapto FAILS.
  hz$designation <- c("A", "C1", "Bw", "C2")
  pr2 <- PedonRecord$new(
    site = list(id = "TH2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "alluvium"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(
    soilKey:::.apply_specifier(pr2, "Thapto", "Cambic", spec_thapto)$passed))
})


# ---- Supra- (above a barrier) ---------------------------------------------

test_that("Supragleyic requires gleyic layers above a barrier (rock or petric)", {
  # Profile with a continuous-rock at 60 cm and gleyic features at 30 cm.
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 150),
    designation = c("A", "Bg", "R"),
    munsell_hue_moist = c("10YR","2.5Y", NA_character_),
    munsell_value_moist = c(4, 5, NA_real_),
    munsell_chroma_moist = c(2, 1, NA_real_),
    redoximorphic_features_pct = c(0, 30, NA_real_),
    coarse_fragments_pct = c(2, 5, 100),
    clay_pct = c(20, 22, NA_real_),
    silt_pct = c(40, 38, NA_real_),
    sand_pct = c(40, 40, NA_real_),
    bulk_density_g_cm3 = c(1.3, 1.5, NA_real_)
  )
  pr <- PedonRecord$new(
    site = list(id = "SU", lat = 0, lon = 0, country = "TEST",
                  parent_material = "schist"),
    horizons = ensure_horizon_schema(hz)
  )
  spec_supra <- soilKey:::.wrb_specifiers$Supra
  res <- soilKey:::.apply_specifier(pr, "Supra", "Gleyic", spec_supra)
  # Bg is at 30-60 cm, the rock is at 60 cm -> Bg ABOVE the barrier.
  expect_true(isTRUE(res$passed))

  # No barrier -> Supra fails.
  hz2 <- hz[1:2, ]
  pr2 <- PedonRecord$new(
    site = list(id = "SU2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "alluvium"),
    horizons = ensure_horizon_schema(hz2)
  )
  expect_false(isTRUE(
    soilKey:::.apply_specifier(pr2, "Supra", "Gleyic", spec_supra)$passed))
})


# ---- Supplementary-qualifier engine path ----------------------------------

test_that("resolve_wrb_qualifiers exposes a supplementary slot", {
  pr <- make_ferralsol_canonical()
  res <- resolve_wrb_qualifiers(pr, "FR")
  expect_true("supplementary" %in% names(res))
  expect_type(res$supplementary, "character")
  # All 32 RSGs now have supplementary slots after v0.9.5 (Bloco F);
  # an RSG code that is *not* in the YAML at all returns character(0)
  # via the "No qualifiers defined for RSG ..." branch.
  res_unknown <- resolve_wrb_qualifiers(make_gleysol_canonical(), "ZZ")
  expect_equal(length(res_unknown$supplementary), 0L)
  expect_equal(length(res_unknown$principal),     0L)
})

test_that("classify_wrb2022 stores supplementary in qualifiers and renders the name", {
  pr  <- make_ferralsol_canonical()
  cls <- classify_wrb2022(pr, on_missing = "silent")
  expect_true("supplementary" %in% names(cls$qualifiers))
  # FR has supplementary qualifiers wired in v0.9.3.B -- the rendered
  # name carries the parenthesised tag block.
  expect_match(cls$name, "Ferralsol \\(")
  expect_match(cls$name, "Clayic")
})


# ---- 31-fixture regression check ------------------------------------------

test_that("v0.9.3.A specifier + engine extensions do not regress 31 fixtures", {
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
