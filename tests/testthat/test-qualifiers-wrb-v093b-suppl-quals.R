# v0.9.3.B -- new supplementary qualifier functions (Aric / Cumulic /
# Profondic / Rubic / Lamellic) and supplementary YAML wiring for the
# argic-clay-rich RSGs (FR, AC, LX, AL, LV, CM, NT).

# ---- Aric ------------------------------------------------------------------

test_that("Aric fires on a designation matching ^Ap in the upper 30 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 25), bottom_cm = c(25, 100),
    designation = c("Ap", "Bw"),
    structure_grade = c("moderate", "moderate"),
    structure_type  = c("granular", "subangular blocky"),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "AR", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess (cultivated)"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_aric(pr)$passed))

  hz$designation <- c("A", "Bw")
  pr2 <- PedonRecord$new(
    site = list(id = "AR2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess (uncultivated)"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_aric(pr2)$passed))
})


# ---- Profondic -------------------------------------------------------------

test_that("Profondic requires argic that continues to bottom_cm >= 150", {
  # Argic that continues to 200 cm -> Profondic passes.
  hz <- data.table::data.table(
    top_cm = c(0, 15, 35), bottom_cm = c(15, 35, 200),
    designation = c("A", "E", "Bt"),
    munsell_value_moist = c(4, 6, 4), munsell_chroma_moist = c(3, 3, 4),
    clay_films = c(NA_character_, NA_character_, "common"),
    clay_pct = c(15, 12, 35), silt_pct = c(40, 38, 35), sand_pct = c(45, 50, 30),
    cec_cmol = c(15, 10, 18), bs_pct = c(50, 48, 65),
    ph_h2o = c(6, 6, 6.2)
  )
  pr <- PedonRecord$new(
    site = list(id = "PF", lat = 0, lon = 0, country = "TEST",
                  parent_material = "deeply weathered"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_profondic(pr)$passed))

  # Same fixture but argic ends at 100 -> Profondic fails.
  hz$bottom_cm[3] <- 100
  pr2 <- PedonRecord$new(
    site = list(id = "PF2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "shallow weathered"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_profondic(pr2)$passed))
})


# ---- Rubic -----------------------------------------------------------------

test_that("Rubic accepts hue <= 5YR + chroma >= 4 (looser than Rhodic)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_hue_moist = c("5YR", "5YR"),
    munsell_value_moist = c(4, 4), munsell_chroma_moist = c(6, 6),
    clay_pct = c(20, 22), silt_pct = c(40, 38), sand_pct = c(40, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "RU", lat = 0, lon = 0, country = "TEST",
                  parent_material = "weathered basalt"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_rubic(pr)$passed))
  # Rhodic is stricter (needs <= 2.5YR + value < 4) and FAILS for the
  # same fixture (hue 5YR + value 4 -> not Rhodic).
  expect_false(isTRUE(qual_rhodic(pr)$passed))

  # Yellow soil -> neither Rubic nor Rhodic.
  hz$munsell_hue_moist <- c("10YR", "10YR")
  pr2 <- PedonRecord$new(
    site = list(id = "RU2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_rubic(pr2)$passed))
  expect_false(isTRUE(qual_rhodic(pr2)$passed))
})


# ---- Lamellic --------------------------------------------------------------

test_that("Lamellic recognises common lamellae designation patterns", {
  hz <- data.table::data.table(
    top_cm = c(0, 5, 30), bottom_cm = c(5, 30, 100),
    designation = c("A", "E&Bt", "C"),
    clay_pct = c(8, 12, 5), silt_pct = c(15, 20, 8), sand_pct = c(77, 68, 87)
  )
  pr <- PedonRecord$new(
    site = list(id = "L", lat = 0, lon = 0, country = "TEST",
                  parent_material = "aeolian sand"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_lamellic(pr)$passed))
})


# ---- Cumulic ---------------------------------------------------------------

test_that("Cumulic fires on a fluvic / cumulic surface marker", {
  hz <- data.table::data.table(
    top_cm = c(0, 20), bottom_cm = c(20, 100),
    designation = c("Au", "Bw"),  # Au = cumulic-style
    clay_pct = c(20, 22), silt_pct = c(45, 38), sand_pct = c(35, 40),
    layer_origin = c("fluvic", NA_character_)
  )
  pr <- PedonRecord$new(
    site = list(id = "CU", lat = 0, lon = 0, country = "TEST",
                  parent_material = "Holocene alluvium"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(qual_cumulic(pr)$passed))

  hz$designation <- c("A", "Bw"); hz$layer_origin <- c(NA_character_, NA_character_)
  pr2 <- PedonRecord$new(
    site = list(id = "CU2", lat = 0, lon = 0, country = "TEST",
                  parent_material = "loess"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(qual_cumulic(pr2)$passed))
})


# ---- End-to-end canonical Ferralsol with parenthesised supplementary ------

test_that("FR canonical name carries supplementary tags in WRB Ch 6 form", {
  cls <- classify_wrb2022(make_ferralsol_canonical(), on_missing = "silent")
  expect_match(cls$name, "^Geric Ferric Rhodic Chromic Ferralsol \\(")
  expect_match(cls$name, "Clayic")
  expect_match(cls$name, "Humic")
  expect_match(cls$name, "Dystric")

  # The supplementary list is exposed on the ClassificationResult.
  expect_true("Clayic"  %in% cls$qualifiers$supplementary)
  expect_true("Humic"   %in% cls$qualifiers$supplementary)
  expect_true("Dystric" %in% cls$qualifiers$supplementary)
})

test_that("AC / LX / LV / NT canonical names carry parenthesised supplementary", {
  for (rsg in c("AC", "LX", "LV", "NT")) {
    fx <- switch(rsg,
      AC = make_acrisol_canonical(), LX = make_lixisol_canonical(),
      LV = make_luvisol_canonical(), NT = make_nitisol_canonical())
    cls <- classify_wrb2022(fx, on_missing = "silent")
    expect_match(cls$name, " \\(",
                  info = sprintf("RSG %s: name should carry parenthesised supplementary block",
                                  rsg))
    expect_true(length(cls$qualifiers$supplementary) > 0L,
                  info = sprintf("RSG %s: supplementary list should be non-empty",
                                  rsg))
  }
})


# ---- 31-fixture regression check ------------------------------------------

test_that("v0.9.3.B supplementary additions do not regress 31 fixtures", {
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
