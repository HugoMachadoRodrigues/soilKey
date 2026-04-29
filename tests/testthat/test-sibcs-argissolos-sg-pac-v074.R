# v0.7.4.B.2 SiBCS Cap 5 (Argissolos) -- Stage B.2: 21 PAC Subgrupos.
#
# Discriminantes: carater_arenico (NOVO), carater_durico (NOVO),
# carater_latossolico (NOVO), mudanca_textural_abrupta, fragipa,
# carater_plintico.


# ---- Helper: PAC pedon factory ---------------------------------------

.make_pac_subgrupo <- function(
  bs_pct_b      = 30,            # 30 = distrofico, 80 = eutrofico
  rupture_dry_b = NA_character_,
  consistence_moist_b = NA_character_,
  abrupt_clay_jump = FALSE,
  arenic_layer_pattern = FALSE,  # textura arenosa de 0 a 60 cm
  durico_present       = FALSE,
  fragipa_present      = FALSE,
  plintic_pct          = 0,
  latossolic_below     = FALSE
) {
  # PAC = Acinzentados: hue 7.5YR, value >= 5, chroma < 4
  # Para arenico: clay 5-10 nas 2 primeiras camadas (0-60), clay alta em B
  if (arenic_layer_pattern) {
    clay_a <- 8; clay_ba <- 10
  } else {
    clay_a <- if (abrupt_clay_jump) 12 else 22
    clay_ba <- if (abrupt_clay_jump) 38 else 32
  }
  clay_b <- 35
  cementation_b <- if (durico_present) "weakly" else NA_character_
  duripan_b <- if (durico_present) 5 else 0
  # B latossolico abaixo do B textural: simulate via deeper Bw with low cec
  designation_b <- if (latossolic_below) c("A", "BA", "Bt", "Bw") else c("A", "BA", "Bt", "BC")
  cec_b_textural <- 6  # Tb
  cec_b_lat      <- if (latossolic_below) 3 else 6  # latossolico needs CEC <= 17 cmolc/kg argila
  bot_4 <- if (latossolic_below) 200 else 200

  hz <- data.table::data.table(
    top_cm    = c(0,    20,   60,   130),
    bottom_cm = c(20,   60,   130,  bot_4),
    designation = designation_b,
    munsell_hue_moist    = c("7.5YR", "7.5YR", "7.5YR", "7.5YR"),
    munsell_value_moist  = c(5,    5,    5,    5),
    munsell_chroma_moist = c(2,    3,    3,    3),
    structure_grade      = c("moderate","moderate","strong","moderate"),
    structure_type       = c("granular", "subangular blocky",
                                "subangular blocky", "subangular blocky"),
    rupture_resistance   = c(NA_character_, rupture_dry_b, rupture_dry_b,
                                NA_character_),
    consistence_moist    = c(NA_character_, consistence_moist_b,
                                consistence_moist_b, NA_character_),
    boundary_distinctness = c(if (abrupt_clay_jump) "abrupt" else "clear",
                                 "clear", "gradual", "gradual"),
    clay_pct             = c(clay_a, clay_ba, clay_b, clay_b),
    silt_pct             = c(30,   25,   20,   22),
    sand_pct             = c(100 - clay_a - 30,
                                100 - clay_ba - 25,
                                100 - clay_b - 20,
                                100 - clay_b - 22),
    cec_cmol             = c(10,   cec_b_textural, cec_b_textural, cec_b_lat),
    bs_pct               = c(40,   bs_pct_b, bs_pct_b, bs_pct_b),
    al_cmol              = c(0.3,  0.5,  0.5,  0.4),
    duripan_pct          = c(NA_real_, NA_real_, duripan_b, NA_real_),
    cementation_class    = c(NA_character_, NA_character_, cementation_b,
                                NA_character_),
    plinthite_pct        = c(NA_real_, NA_real_, plintic_pct, plintic_pct),
    ph_h2o               = c(5.5,  5.0,  5.0,  5.0),
    oc_pct               = c(1.5, 0.6, 0.3, 0.2)
  )
  PedonRecord$new(
    site = list(id = "PAC-TEST", lat = -7, lon = -34, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
}


# ============================================================================
# 1. carater_arenico
# ============================================================================

test_that("carater_arenico passes for sandy 0-60 cm + clayey 60+ profile", {
  pr <- .make_pac_subgrupo(arenic_layer_pattern = TRUE)
  res <- carater_arenico(pr)
  expect_true(isTRUE(res$passed))
  expect_equal(res$evidence$boundary_top_cm, 60)
})

test_that("carater_arenico FAILS quando boundary < 50 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60),
    bottom_cm = c(30, 60, 150),
    designation = c("A", "Bt", "BC"),
    clay_pct = c(8, 35, 40)   # boundary at 30 cm < 50
  )
  pr <- PedonRecord$new(
    site = list(id = "AREN-FAIL", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_arenico(pr)$passed))
})

test_that("carater_arenico FAILS quando boundary > 100 cm (espessarenico)", {
  hz <- data.table::data.table(
    top_cm = c(0, 50, 120),
    bottom_cm = c(50, 120, 200),
    designation = c("A", "BA", "Bt"),
    clay_pct = c(8, 10, 40)   # boundary at 120 cm > 100
  )
  pr <- PedonRecord$new(
    site = list(id = "AREN-ESP", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_arenico(pr)$passed))
})

test_that("carater_arenico retorna NA quando clay_pct totalmente missing", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "BA"),
    clay_pct = c(NA_real_, NA_real_)
  )
  pr <- PedonRecord$new(
    site = list(id = "NA-clay", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_arenico(pr)
  expect_true(is.na(res$passed))
  expect_true("clay_pct" %in% res$missing)
})


# ============================================================================
# 2. carater_durico
# ============================================================================

test_that("carater_durico passes via duripan_pct > 0", {
  pr <- .make_pac_subgrupo(durico_present = TRUE)
  res <- carater_durico(pr)
  expect_true(isTRUE(res$passed))
})

test_that("carater_durico passes via cementation_class 'weakly'", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    cementation_class = c(NA_character_, "weakly")
  )
  pr <- PedonRecord$new(
    site = list(id = "DUR", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_durico(pr)$passed))
})

test_that("carater_durico FAILS para 'strongly' (eh duripa)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    cementation_class = c(NA_character_, "strongly")  # ja eh duripa
  )
  pr <- PedonRecord$new(
    site = list(id = "DUR-STR", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_durico(pr)$passed))
})


# ============================================================================
# 3. carater_latossolico
# ============================================================================

test_that("carater_latossolico FAILS quando B_textural nao passa", {
  pr <- .make_pac_subgrupo()  # No abrupt clay jump means may not have B_textural
  # If B_textural doesn't pass, carater_latossolico returns FALSE
  res <- carater_latossolico(pr)
  if (!isTRUE(B_textural(pr)$passed)) {
    expect_false(isTRUE(res$passed))
  } else {
    skip("B_textural passes; cant test the no-textural path")
  }
})


# ============================================================================
# 4. YAML structural integrity para PAC
# ============================================================================

test_that("PAC subgrupos: 9+9+3 = 21 classes em 3 GGs", {
  rules <- load_rules("sibcs5")
  for (gg in c("PACdc", "PACd", "PACe")) {
    expect_true(gg %in% names(rules$subgrupos),
                  info = sprintf("GG %s ausente", gg))
  }
  expect_equal(length(rules$subgrupos$PACdc), 9L)
  expect_equal(length(rules$subgrupos$PACd),  9L)
  expect_equal(length(rules$subgrupos$PACe),  3L)
  total <- length(rules$subgrupos$PACdc) +
             length(rules$subgrupos$PACd) +
             length(rules$subgrupos$PACe)
  expect_equal(total, 21L)
})

test_that("cada PAC GG termina em 'Tp' (catch-all 'tipicos')", {
  rules <- load_rules("sibcs5")
  for (gg in c("PACdc", "PACd", "PACe")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s ultima entrada deveria ser default:true; got %s",
                                  gg, last$code))
    expect_true(endsWith(last$code, "Tp"),
                  info = sprintf("GG %s ultimo code nao termina em 'Tp': %s",
                                  gg, last$code))
  }
})

test_that("PACdc tem ordem canonica Ar > AbDu > AbFr > Ab > Du > Fr > Pl > La > Tp", {
  rules <- load_rules("sibcs5")
  codes <- vapply(rules$subgrupos$PACdc, function(x) x$code, character(1))
  expect_equal(codes,
                 c("PACdcAr", "PACdcAbDu", "PACdcAbFr", "PACdcAb",
                   "PACdcDu", "PACdcFr", "PACdcPl", "PACdcLa", "PACdcTp"))
})


# ============================================================================
# 5. run_sibcs_subgrupo dispatcher para PAC
# ============================================================================

test_that("PACdcAr (arenicos) catches sandy 0-60 + distrofico profile", {
  pr <- .make_pac_subgrupo(arenic_layer_pattern = TRUE)
  res <- run_sibcs_subgrupo(pr, "PACdc")
  expect_equal(res$assigned$code, "PACdcAr")
})

test_that("PACdcAb (abrupticos) catches mudanca abrupta + distrofico profile", {
  pr <- .make_pac_subgrupo(abrupt_clay_jump = TRUE)
  res <- run_sibcs_subgrupo(pr, "PACdc")
  expect_equal(res$assigned$code, "PACdcAb")
})

test_that("PACdcDu (duricos) catches profile com cementation 'weakly'", {
  pr <- .make_pac_subgrupo(durico_present = TRUE)
  res <- run_sibcs_subgrupo(pr, "PACdc")
  expect_equal(res$assigned$code, "PACdcDu")
})

test_that("PACdcAbDu (abrupticos duricos) catches mudanca + durico combinacao", {
  pr <- .make_pac_subgrupo(abrupt_clay_jump = TRUE, durico_present = TRUE)
  res <- run_sibcs_subgrupo(pr, "PACdc")
  expect_equal(res$assigned$code, "PACdcAbDu")
})

test_that("PACdcTp (catch-all) selected para profile generico distrofico", {
  pr <- .make_pac_subgrupo()   # default sem outros criterios
  res <- run_sibcs_subgrupo(pr, "PACdc")
  expect_equal(res$assigned$code, "PACdcTp")
})

test_that("PACeAb (eutroficos abrupticos) catches eutrofico + mudanca", {
  pr <- .make_pac_subgrupo(bs_pct_b = 75, abrupt_clay_jump = TRUE)
  res <- run_sibcs_subgrupo(pr, "PACe")
  expect_equal(res$assigned$code, "PACeAb")
})


# ============================================================================
# 6. Backward-compat
# ============================================================================

test_that("PBAC subgrupos ainda funcionam apos PAC SGs add", {
  rules <- load_rules("sibcs5")
  expect_true("PBACtal" %in% names(rules$subgrupos))
  expect_equal(length(rules$subgrupos$PBACtal), 4L)
})

test_that("WRB / USDA / Cap 14 nao quebraram apos PAC subgrupos add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
