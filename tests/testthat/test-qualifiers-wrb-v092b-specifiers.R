# v0.9.2.B -- Specifier infrastructure (Ano- / Epi- / Endo- / Bathy- /
# Panto- depth-band prefixes that compose with base qualifiers).

# ---- Specifier-detection unit tests --------------------------------------

test_that(".detect_specifier recognises Ano/Epi/Endo/Bathy/Panto prefixes", {
  s <- soilKey:::.detect_specifier("Endogleyic")
  expect_equal(s$prefix, "Endo")
  expect_equal(s$base,   "Gleyic")
  # v0.9.3.A moved depth params under spec$.
  expect_equal(s$spec$kind,       "depth")
  expect_equal(s$spec$min_top_cm,  50)
  expect_equal(s$spec$max_top_cm, 100)

  s <- soilKey:::.detect_specifier("Bathysalic")
  expect_equal(s$prefix, "Bathy")
  expect_equal(s$base,   "Salic")
  expect_equal(s$spec$min_top_cm, 100)
  expect_true(is.infinite(s$spec$max_top_cm))

  s <- soilKey:::.detect_specifier("Episalic")
  expect_equal(s$prefix, "Epi")
  expect_equal(s$spec$min_top_cm, 0)
  expect_equal(s$spec$max_top_cm, 50)

  s <- soilKey:::.detect_specifier("Anocalcic")
  expect_equal(s$prefix, "Ano")

  s <- soilKey:::.detect_specifier("Pantocalcic")
  expect_equal(s$prefix, "Panto")

  # Plain qualifier has no specifier.
  expect_null(soilKey:::.detect_specifier("Calcic"))
  expect_null(soilKey:::.detect_specifier("Vermic"))

  # Prefix alone (no base) is rejected.
  expect_null(soilKey:::.detect_specifier("Endo"))
})


# ---- End-to-end specifier dispatch -----------------------------------------

test_that(".apply_specifier filters base qualifier layers by depth band", {
  # Profile with calcic accumulation only in the 60-100 cm band.
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
    designation = c("A", "AB", "Bk"),
    caco3_pct = c(0, 0, 25),
    clay_pct = c(20, 22, 22), silt_pct = c(40, 38, 38),
    sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "EC", lat = 0, lon = 0, country = "TEST",
                  parent_material = "calcareous loess"),
    horizons = ensure_horizon_schema(hz)
  )
  spec_endo  <- list(kind = "depth", min_top_cm = 50,  max_top_cm = 100)
  spec_epi   <- list(kind = "depth", min_top_cm =  0,  max_top_cm =  50)
  spec_bathy <- list(kind = "depth", min_top_cm = 100, max_top_cm = Inf)
  endo <- soilKey:::.apply_specifier(pr, "Endo", "Calcic", spec_endo)
  expect_true(isTRUE(endo$passed))
  # Only layer 3 (Bk, top=60, in 50-100 band) survives the filter.
  expect_equal(endo$layers, 3L)

  epi <- soilKey:::.apply_specifier(pr, "Epi", "Calcic", spec_epi)
  # Calcic passes only on layer 3 (top=60 -- outside 0-50) -> Epi fails.
  expect_false(isTRUE(epi$passed))

  bathy <- soilKey:::.apply_specifier(pr, "Bathy", "Calcic", spec_bathy)
  # Layer 3 top=60 is NOT >= 100 -> Bathy fails.
  expect_false(isTRUE(bathy$passed))
})


# ---- resolve_wrb_qualifiers handles specifier-prefixed YAML names ---------

test_that("resolve_wrb_qualifiers dispatches specifier-prefixed names", {
  # Synthetic Chernozem-like profile with calcic at depth (60+ cm).
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60, 100),
    bottom_cm = c(30, 60, 100, 150),
    designation = c("Ah1", "Ah2", "Bk", "Ck"),
    munsell_value_moist = c(2, 3, 4, 5),
    munsell_chroma_moist = c(1, 2, 3, 3),
    munsell_value_dry = c(3, 4, 6, 7), munsell_chroma_dry = c(2, 3, 3, 3),
    structure_grade = c("strong","moderate","weak","weak"),
    structure_type  = c("granular","subangular blocky",
                          "subangular blocky","massive"),
    consistence_moist = c("friable","friable","firm","firm"),
    clay_pct = c(25, 27, 27, 25), silt_pct = c(50, 50, 49, 50),
    sand_pct = c(25, 23, 24, 25),
    ph_h2o = c(7.2, 7.4, 8.0, 8.2), ph_kcl = c(6.8, 7.0, 7.3, 7.5),
    oc_pct = c(4.0, 2.0, 0.8, 0.4),
    cec_cmol = c(30, 28, 25, 22),
    ca_cmol = c(22, 20, 20, 17), mg_cmol = c(4, 4, 4, 3),
    k_cmol  = c(0.5, 0.4, 0.2, 0.2), na_cmol = c(0.1, 0.1, 0.1, 0.1),
    bs_pct = c(89, 87, 97, 95),
    caco3_pct = c(0, 0, 22, 30),  # calcic only in 60+ cm
    worm_holes_pct = c(60, 50, 20, 5),
    bulk_density_g_cm3 = c(1.05, 1.15, 1.30, 1.35)
  )
  pr <- PedonRecord$new(
    site = list(id = "EC", lat = 47, lon = 30, country = "TEST",
                  parent_material = "loess",
                  drainage_class = "well drained"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- resolve_wrb_qualifiers(pr, "CH")
  # Endocalcic must appear in trace AND fire (Calcic passes on layer 3
  # at top 60 cm; the Endo- band 50-100 contains it).
  expect_true("Endocalcic" %in% names(res$trace))
  expect_true(isTRUE(res$trace$Endocalcic$passed))
  expect_true("Endocalcic" %in% res$principal)
})


# ---- Specifier path coexists with family suppression ----------------------

test_that("Family suppression operates after specifier dispatch", {
  # If both Calcic and Endocalcic pass, family suppression is applied
  # on the resolved name list -- but Endocalcic is NOT in the calcic
  # family table (only the un-prefixed names are) so both can survive.
  matched <- c("Vermic", "Calcic", "Endocalcic", "Cambic")
  kept <- soilKey:::.suppress_qualifier_siblings(matched)
  expect_setequal(kept, c("Vermic", "Calcic", "Endocalcic", "Cambic"))
})


# ---- 31-fixture regression check ------------------------------------------

test_that("v0.9.2.B specifier infrastructure does not regress 31 fixtures", {
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
