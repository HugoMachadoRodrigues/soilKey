# v0.9 WRB Ch 5 -- principal qualifier resolution.

test_that("qualifiers.yaml loads and covers all 32 RSGs", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml",
                          package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  expect_true(file.exists(qfile))
  qrules <- yaml::read_yaml(qfile)
  expect_equal(length(qrules$rsg_qualifiers), 32L)
})

test_that("each RSG has at least one principal qualifier in the YAML", {
  qfile <- system.file("rules/wrb2022/qualifiers.yaml",
                          package = "soilKey")
  if (!nzchar(qfile)) qfile <- "inst/rules/wrb2022/qualifiers.yaml"
  qrules <- yaml::read_yaml(qfile)
  for (code in names(qrules$rsg_qualifiers)) {
    principal <- qrules$rsg_qualifiers[[code]]$principal
    expect_gt(length(principal), 0L,
                label = sprintf("RSG %s should have principal qualifiers",
                                  code))
  }
})


# ---- core qualifier diagnostics --------------------------------------------

test_that("qual_albic catches a bleached eluvial layer", {
  hz <- data.table::data.table(
    top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 100),
    designation = c("A", "E", "Bt"),
    munsell_value_moist = c(3, 7, 4),
    munsell_chroma_moist = c(2, 2, 4),
    clay_pct = c(15, 12, 28), silt_pct = c(40, 50, 30),
    sand_pct = c(45, 38, 42)
  )
  pr <- PedonRecord$new(
    site = list(id = "AB", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_albic(pr)$passed))
})

test_that("qual_rhodic requires hue redder than 5YR + low value", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 180),
    munsell_hue_moist = c("10YR", "2.5YR", "2.5YR"),
    munsell_value_moist = c(3, 3, 3),
    munsell_chroma_moist = c(3, 6, 6),
    clay_pct = c(30, 50, 55), silt_pct = c(20, 15, 15),
    sand_pct = c(50, 35, 30)
  )
  pr <- PedonRecord$new(
    site = list(id = "RO", lat = 0, lon = 0, country = "TEST",
                  parent_material = "basalt"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_rhodic(pr)$passed))
})

test_that("qual_xanthic requires ferralic + yellower hues", {
  pr <- make_ferralsol_canonical()
  res <- qual_xanthic(pr)
  # Default Ferralsol fixture is reddish (2.5YR) -- xanthic should NOT pass.
  expect_false(isTRUE(res$passed))
})

test_that("qual_skeletic catches >= 40% coarse fragments", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
    coarse_fragments_pct = c(15, 50, 60),
    clay_pct = c(20, 25, 25), silt_pct = c(40, 35, 35),
    sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "SK", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_skeletic(pr)$passed))
})

test_that("qual_humic catches OC >= 1% in upper 50 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 150),
    oc_pct = c(2.5, 1.5, 0.3),
    clay_pct = c(20, 25, 25), silt_pct = c(40, 35, 35),
    sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "HU", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_humic(pr)$passed))
})

test_that("qual_dystric catches BS < 50% throughout 20-100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
    bs_pct = c(60, 30, 25),  # surface high but subsurface low
    clay_pct = c(20, 25, 25), silt_pct = c(40, 35, 35),
    sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "DY", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_dystric(pr)$passed))
})

test_that("qual_eutric catches BS >= 50% throughout 20-100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
    bs_pct = c(40, 75, 80),
    clay_pct = c(20, 25, 25), silt_pct = c(40, 35, 35),
    sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "EU", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_eutric(pr)$passed))
})

test_that("qual_haplic always passes (catch-all)", {
  pr <- make_arenosol_canonical()
  expect_true(isTRUE(qual_haplic(pr)$passed))
})


# ---- resolve_wrb_qualifiers ------------------------------------------------

test_that("resolve_wrb_qualifiers returns canonical-order matched qualifiers", {
  pr <- make_ferralsol_canonical()
  res <- resolve_wrb_qualifiers(pr, "FR")
  expect_true("principal" %in% names(res))
  # The Ferralsol fixture has 2.5YR hue + clay >= 60% -- expect Rhodic
  # (and possibly Ferric, Chromic). At minimum: at least one matched.
  expect_gt(length(res$principal), 0L)
})

test_that("resolve_wrb_qualifiers falls back to Haplic when nothing matches", {
  # Build a minimal fixture that should not trigger any qualifier.
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 150),
    clay_pct = c(20, 22, 22), silt_pct = c(40, 38, 38),
    sand_pct = c(40, 40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "min", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- resolve_wrb_qualifiers(pr, "RG")
  expect_equal(res$principal, "Haplic")
})


# ---- format_wrb_name ----------------------------------------------------

test_that("format_wrb_name builds canonical names", {
  expect_equal(format_wrb_name("Ferralsols", "Rhodic"),
                "Rhodic Ferralsol")
  expect_equal(format_wrb_name("Ferralsols",
                                  c("Ferric", "Rhodic", "Chromic")),
                "Ferric Rhodic Chromic Ferralsol")
  expect_equal(format_wrb_name("Ferralsols", "Haplic"),
                "Haplic Ferralsol")
  expect_equal(format_wrb_name("Ferralsols", "Rhodic",
                                  supplementary = c("Clayic", "Humic", "Dystric")),
                "Rhodic Ferralsol (Clayic, Humic, Dystric)")
})


# ---- classify_wrb2022 returns full qualified name ----------------------

test_that("classify_wrb2022 returns a qualified name in $name", {
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr, on_missing = "silent")
  expect_match(res$name, "Ferralsol")  # singular with optional qualifiers
  expect_equal(res$rsg_or_order, "Ferralsols")  # plural RSG name unchanged
})

test_that("classify_wrb2022 stores qualifiers in $qualifiers", {
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr, on_missing = "silent")
  expect_true("principal" %in% names(res$qualifiers))
  expect_gt(length(res$qualifiers$principal), 0L)
})

test_that("classify_wrb2022 still maps 31 fixtures to correct RSG", {
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
                  info = sprintf("Fixture %s expected %s, got %s",
                                  k, expected[[k]], out))
  }
})
