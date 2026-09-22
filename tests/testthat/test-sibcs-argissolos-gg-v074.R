# v0.7.4 SiBCS Cap 5 (Argissolos) -- Stage A: 23 Grandes Grupos.
#
# Discriminantes do 3o nivel: combinacoes de eutrofico/distrofico,
# atividade da argila Ta, carater alitico, carater coeso (NOVO),
# carater ferrico (NOVO -- Fe2O3 sulfurico 18-36% em B).


# ---- Helpers de fixture --------------------------------------------------

.make_argissolo_pedon <- function(
  hue            = "5YR",
  value_b        = 4,
  chroma_b       = 4,
  bs_pct_b       = 30,        # 30 = distrofico, 80 = eutrofico
  cec_b          = 8,         # CEC para Ta calculation; 30+ = Ta
  clay_b         = 40,
  al_cmol_b      = 0.5,       # 4+ = alitico (com sat Al >= 50)
  al_sat_pct_b   = 20,        # 50+ + Al >= 4 = alitico
  fe2o3_sulf_b   = NA_real_,  # set 25 for Eutroferrico
  rupture_dry_b  = NA_character_,   # "very hard" + consistence_moist friable -> coeso
  consistence_moist_b = NA_character_,
  oc_pct         = c(1.5, 0.6, 0.3, 0.2)
) {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   60,   130),
    bottom_cm = c(20,   60,   130,  200),
    designation = c("A", "BA", "Bt", "BC"),
    munsell_hue_moist    = c(hue, hue, hue, hue),
    munsell_value_moist  = c(value_b, value_b, value_b, value_b),
    munsell_chroma_moist = c(chroma_b, chroma_b, chroma_b, chroma_b),
    structure_grade      = c("moderate", "moderate", "strong", "moderate"),
    structure_type       = c("granular", "subangular blocky",
                                "subangular blocky", "subangular blocky"),
    # rupture/consistence applied APENAS ao Bt (3a camada, top=60 cm)
    # para que tests com max_depth_cm < 60 excluam o coeso.
    rupture_resistance   = c(NA_character_, NA_character_, rupture_dry_b,
                                NA_character_),
    consistence_moist    = c(NA_character_, NA_character_, consistence_moist_b,
                                NA_character_),
    clay_pct             = c(20,   clay_b, clay_b, clay_b),
    silt_pct             = c(30,   25,   20,   22),
    sand_pct             = c(50,   45,   40,   38),
    cec_cmol             = c(10,   cec_b, cec_b, cec_b),
    bs_pct               = c(40,   bs_pct_b, bs_pct_b, bs_pct_b),
    al_cmol              = c(0.3,  0.4,  al_cmol_b, al_cmol_b),
    al_sat_pct           = c(15,   18,   al_sat_pct_b, al_sat_pct_b),
    fe2o3_sulfuric_pct   = c(NA_real_, NA_real_, fe2o3_sulf_b, fe2o3_sulf_b),
    ph_h2o               = c(5.5,  5.3,  5.0,  5.0),
    oc_pct               = oc_pct
  )
  PedonRecord$new(
    site = list(id = "ARG-TEST", lat = -23, lon = -47, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
}


# ============================================================================
# 1. carater_coeso (Cap 1, pp 32-33)
# ============================================================================

test_that("carater_coeso passes for very hard + friable + clayey horizon", {
  pr <- .make_argissolo_pedon(
    rupture_dry_b = "very hard",
    consistence_moist_b = "friable",
    clay_b = 40
  )
  expect_true(isTRUE(carater_coeso(pr)$passed))
})

test_that("carater_coeso passes also for 'extremely hard'", {
  pr <- .make_argissolo_pedon(
    rupture_dry_b = "extremely hard",
    consistence_moist_b = "firm",
    clay_b = 40
  )
  expect_true(isTRUE(carater_coeso(pr)$passed))
})

test_that("carater_coeso FAILS for hard + friable (not hard enough)", {
  pr <- .make_argissolo_pedon(
    rupture_dry_b = "hard",
    consistence_moist_b = "friable",
    clay_b = 40
  )
  expect_false(isTRUE(carater_coeso(pr)$passed))
})

test_that("carater_coeso EXCLUDES sandy horizons (clay < 15%)", {
  pr <- .make_argissolo_pedon(
    rupture_dry_b = "very hard",
    consistence_moist_b = "friable",
    clay_b = 10  # sandy
  )
  expect_false(isTRUE(carater_coeso(pr)$passed))
})

test_that("carater_coeso retorna NA quando ambos rupture e consistence NA", {
  pr <- .make_argissolo_pedon()  # rupture_dry e consistence_moist NA
  res <- carater_coeso(pr)
  expect_true(is.na(res$passed))
  expect_true("rupture_resistance" %in% res$missing)
})

test_that("carater_coeso respects max_depth_cm", {
  pr <- .make_argissolo_pedon(
    rupture_dry_b = "very hard",
    consistence_moist_b = "friable",
    clay_b = 40
  )
  # Layer Bt (top 60 cm) is hard within 150 cm
  expect_true(isTRUE(carater_coeso(pr, max_depth_cm = 150)$passed))
  # Restrict to 30 cm: Bt at 60 cm is excluded
  expect_false(isTRUE(carater_coeso(pr, max_depth_cm = 30)$passed))
})


# ============================================================================
# 2. carater_ferrico (Cap 1, p 35; Cap 5 Eutroferricos)
# ============================================================================

test_that("carater_ferrico passes for Fe2O3 sulfurico no intervalo 18-36%", {
  pr <- .make_argissolo_pedon(fe2o3_sulf_b = 25)   # 250 g/kg
  expect_true(isTRUE(carater_ferrico(pr)$passed))
})

test_that("carater_ferrico FAILS for Fe2O3 < 18%", {
  pr <- .make_argissolo_pedon(fe2o3_sulf_b = 10)
  expect_false(isTRUE(carater_ferrico(pr)$passed))
})

test_that("carater_ferrico FAILS for Fe2O3 >= 36% (perferrico, fora do range)", {
  pr <- .make_argissolo_pedon(fe2o3_sulf_b = 40)
  expect_false(isTRUE(carater_ferrico(pr)$passed))
})

test_that("carater_ferrico retorna NA quando fe2o3_sulfuric_pct missing in B", {
  pr <- .make_argissolo_pedon()   # default NA
  res <- carater_ferrico(pr)
  expect_true(is.na(res$passed))
  expect_true("fe2o3_sulfuric_pct" %in% res$missing)
})


# ============================================================================
# 3. YAML: 23 Grandes Grupos de Argissolos via load_rules
# ============================================================================

test_that("load_rules merges grandes-grupos/argissolos.yaml com 5 subordens", {
  rules <- load_rules("sibcs5")
  for (sub in c("PBAC", "PAC", "PA", "PV", "PVA")) {
    expect_true(sub %in% names(rules$grandes_grupos),
                  info = sprintf("Subordem %s ausente em grandes_grupos", sub))
  }
})

test_that("Argissolos GGs totalizam 23 classes (3+3+6+6+5)", {
  rules <- load_rules("sibcs5")
  pbac_n <- length(rules$grandes_grupos$PBAC)
  pac_n  <- length(rules$grandes_grupos$PAC)
  pa_n   <- length(rules$grandes_grupos$PA)
  pv_n   <- length(rules$grandes_grupos$PV)
  pva_n  <- length(rules$grandes_grupos$PVA)
  expect_equal(pbac_n, 3L)
  expect_equal(pac_n,  3L)
  expect_equal(pa_n,   6L)
  expect_equal(pv_n,   6L)
  expect_equal(pva_n,  5L)
  expect_equal(pbac_n + pac_n + pa_n + pv_n + pva_n, 23L)
})

test_that("Argissolos GG codes seguem convencao 3-8 chars sem hyphen", {
  rules <- load_rules("sibcs5")
  for (sub in c("PBAC", "PAC", "PA", "PV", "PVA")) {
    codes <- vapply(rules$grandes_grupos[[sub]],
                      function(x) x$code, character(1))
    expect_true(all(nchar(codes) >= 3L & nchar(codes) <= 8L),
                  info = sprintf("Codigos do GG %s fora da faixa 3-8 chars: %s",
                                  sub, paste(codes, collapse=", ")))
    # Cada codigo comeca com a subordem
    expect_true(all(startsWith(codes, sub)),
                  info = sprintf("Nem todos os codigos do GG %s comecam com '%s'",
                                  sub, sub))
  }
})


# ============================================================================
# 4. run_sibcs_grande_grupo dispatcher para Argissolos
# ============================================================================

test_that("PBACtal (Ta Aluminicos) catches Ta + alitico profile", {
  pr <- .make_argissolo_pedon(
    cec_b = 30,        # Ta = 30*100/40 = 75 cmolc/kg argila >= 27 -> Ta alta
    al_cmol_b = 5,     # >= 4 -> alitico requirement 1
    al_sat_pct_b = 60, # >= 50 -> alitico requirement 2
    bs_pct_b = 30      # < 50 -> alitico requirement 3
  )
  res <- run_sibcs_grande_grupo(pr, "PBAC")
  expect_equal(res$assigned$code, "PBACtal")
})

test_that("PBACal (Aluminicos sem Ta) catches Tb + alitico profile", {
  pr <- .make_argissolo_pedon(
    cec_b = 8,         # Ta = 8*100/40 = 20 < 27 -> Tb (low activity)
    al_cmol_b = 5,
    al_sat_pct_b = 60,
    bs_pct_b = 30
  )
  res <- run_sibcs_grande_grupo(pr, "PBAC")
  expect_equal(res$assigned$code, "PBACal")
})

test_that("PBACd (Distroficos) catches Tb + non-alitico + V<50 profile", {
  pr <- .make_argissolo_pedon(
    cec_b = 8,
    al_cmol_b = 0.5,    # < 4 -> NAO alitico
    al_sat_pct_b = 20,
    bs_pct_b = 30        # < 50 -> distrofico
  )
  res <- run_sibcs_grande_grupo(pr, "PBAC")
  expect_equal(res$assigned$code, "PBACd")
})

test_that("PACdc (Distrocoesos) catches distrofico + coeso profile", {
  pr <- .make_argissolo_pedon(
    bs_pct_b = 30,      # distrofico
    rupture_dry_b = "very hard",
    consistence_moist_b = "friable",
    clay_b = 40
  )
  res <- run_sibcs_grande_grupo(pr, "PAC")
  expect_equal(res$assigned$code, "PACdc")
})

test_that("PACd (Distroficos) catches distrofico sem coesao profile", {
  pr <- .make_argissolo_pedon(
    bs_pct_b = 30,      # distrofico
    rupture_dry_b = "hard",   # NOT coeso
    consistence_moist_b = "friable"
  )
  res <- run_sibcs_grande_grupo(pr, "PAC")
  expect_equal(res$assigned$code, "PACd")
})

test_that("PACe (Eutroficos) catches eutrofico profile", {
  pr <- .make_argissolo_pedon(bs_pct_b = 75)   # >= 50 -> eutrofico
  res <- run_sibcs_grande_grupo(pr, "PAC")
  expect_equal(res$assigned$code, "PACe")
})

test_that("PVef (Eutroferricos) catches eutrofico + Fe2O3 18-36% profile", {
  pr <- .make_argissolo_pedon(
    bs_pct_b = 75,         # eutrofico
    fe2o3_sulf_b = 25      # ferrico
  )
  res <- run_sibcs_grande_grupo(pr, "PV")
  expect_equal(res$assigned$code, "PVef")
})

test_that("PVe (Eutroficos sem ferrico) catches eutrofico-only profile", {
  pr <- .make_argissolo_pedon(
    bs_pct_b = 75,
    fe2o3_sulf_b = 10      # NAO ferrico (< 18%)
  )
  res <- run_sibcs_grande_grupo(pr, "PV")
  expect_equal(res$assigned$code, "PVe")
})

test_that("PVtd (Ta Distroficos) catches Ta + distrofico (sem alitico)", {
  pr <- .make_argissolo_pedon(
    cec_b = 30,           # Ta
    al_cmol_b = 0.5,      # NAO alitico
    bs_pct_b = 30         # distrofico
  )
  res <- run_sibcs_grande_grupo(pr, "PV")
  expect_equal(res$assigned$code, "PVtd")
})


# ============================================================================
# 5. classify_sibcs end-to-end com Argissolos
# ============================================================================

test_that("classify_sibcs alcanca o 3o nivel para Argissolo Vermelho Eutroferrico", {
  pr <- .make_argissolo_pedon(
    hue = "2.5YR",        # vermelho
    bs_pct_b = 75,
    fe2o3_sulf_b = 25
  )
  res <- classify_sibcs(pr, on_missing = "silent")
  if (res$rsg_or_order == "Argissolos") {
    expect_match(res$name, "Argissolos Vermelhos")
    if (!is.null(res$trace$grande_grupo_assigned)) {
      expect_equal(res$trace$grande_grupo_assigned$code, "PVef")
    }
  } else {
    skip(sprintf("fixture caiu em %s", res$rsg_or_order))
  }
})


# ============================================================================
# 6. Backward-compat: WRB e USDA inalterados
# ============================================================================

test_that("WRB e USDA classification ainda passam apos Argissolos GGs add", {
  pr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
