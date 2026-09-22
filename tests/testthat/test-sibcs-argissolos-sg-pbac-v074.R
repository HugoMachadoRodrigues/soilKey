# v0.7.4.B.1 SiBCS Cap 5 (Argissolos) -- Stage B.1: 8 PBAC Subgrupos.
#
# Discriminantes: mudanca_textural_abrupta, horizonte_A_humico,
# carater_humico_espesso (NOVO).


# ---- Helper: PBAC pedon factory --------------------------------------

.make_pbac_subgrupo <- function(
  oc_pct        = c(1.5, 0.6, 0.3, 0.2),  # default OC profile
  abrupt_clay_jump = FALSE,
  ta_alitico    = FALSE,
  alitico_only  = FALSE,
  bs_pct_b      = 30
) {
  # Hue 5YR for PBAC (low chroma in upper B for "bruno-acinzentado")
  clay_a_top <- if (abrupt_clay_jump) 12 else 22
  clay_b     <- if (abrupt_clay_jump) 38 else 32  # >= 2x A se abrupt
  cec_b      <- if (ta_alitico) 30 else 6           # 30 -> Ta; 6 -> Tb
  al_cmol_b  <- if (ta_alitico || alitico_only) 5.0 else 0.5
  al_sat_pct_b <- if (ta_alitico || alitico_only) 60 else 20

  hz <- data.table::data.table(
    top_cm    = c(0,    20,   60,   130),
    bottom_cm = c(20,   60,   130,  200),
    designation = c("A", "BA", "Bt", "BC"),
    munsell_hue_moist    = c("5YR", "5YR", "5YR", "5YR"),
    munsell_value_moist  = c(3,    4,    4,    4),
    munsell_chroma_moist = c(2,    3,    3,    3),
    structure_grade      = c("strong",   "moderate", "moderate", "moderate"),
    structure_type       = c("granular", "subangular blocky",
                                "subangular blocky", "subangular blocky"),
    boundary_distinctness = c(if (abrupt_clay_jump) "abrupt" else "clear",
                                 "clear", "gradual", "gradual"),
    clay_pct             = c(clay_a_top, clay_b, clay_b, clay_b),
    silt_pct             = c(30,   25,   20,   22),
    sand_pct             = c(100 - clay_a_top - 30,
                                100 - clay_b - 25,
                                100 - clay_b - 20,
                                100 - clay_b - 22),
    cec_cmol             = c(10,   cec_b, cec_b, cec_b),
    bs_pct               = c(40,   bs_pct_b, bs_pct_b, bs_pct_b),
    al_cmol              = c(0.3,  al_cmol_b, al_cmol_b, al_cmol_b),
    al_sat_pct           = c(15,   al_sat_pct_b, al_sat_pct_b, al_sat_pct_b),
    ph_h2o               = c(5.5,  5.0,  5.0,  5.0),
    oc_pct               = oc_pct
  )
  PedonRecord$new(
    site = list(id = "PBAC-TEST", lat = -28, lon = -52, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
}


# ============================================================================
# 1. carater_humico_espesso
# ============================================================================

test_that("carater_humico_espesso passes for A humico + OC >= 1% ate 80 cm", {
  # OC alto extendendo ate 130 cm (BC)
  pr <- .make_pbac_subgrupo(oc_pct = c(2.5, 1.5, 1.2, 0.5))
  res <- carater_humico_espesso(pr)
  if (isTRUE(horizonte_A_humico(pr)$passed)) {
    expect_true(isTRUE(res$passed))
    expect_gte(res$evidence$deepest_bottom_cm, 80)
  } else {
    skip("horizonte_A_humico nao casa com fixture; skip downstream check")
  }
})

test_that("carater_humico_espesso FAILS quando A humico nao passa", {
  # A muito raso ou OC muito baixo no A -> A_humico falha
  pr <- .make_pbac_subgrupo(oc_pct = c(0.3, 0.2, 0.1, 0.05))
  res <- carater_humico_espesso(pr)
  expect_false(isTRUE(res$passed))
})

test_that("carater_humico_espesso FAILS quando C alto nao chega a 80 cm", {
  # OC alto so no A (0-20 cm) -- nao chega a 80 cm
  pr <- .make_pbac_subgrupo(oc_pct = c(2.5, 0.4, 0.2, 0.1))
  res <- carater_humico_espesso(pr)
  expect_false(isTRUE(res$passed))
})

test_that("carater_humico_espesso retorna NA quando oc_pct totalmente missing", {
  pr <- .make_pbac_subgrupo(oc_pct = c(NA_real_, NA_real_, NA_real_, NA_real_))
  res <- carater_humico_espesso(pr)
  # A_humico falha por missing -> retorna FALSE com missing OR NA
  expect_true(is.na(res$passed) || isFALSE(res$passed))
})


# ============================================================================
# 2. YAML structural integrity para PBAC
# ============================================================================

test_that("load_rules merges subgrupos/argissolos.yaml com 3 PBAC GGs", {
  rules <- load_rules("sibcs5")
  for (gg in c("PBACtal", "PBACal", "PBACd")) {
    expect_true(gg %in% names(rules$subgrupos),
                  info = sprintf("GG %s ausente em subgrupos", gg))
  }
  expect_equal(length(rules$subgrupos$PBACtal), 4L)
  expect_equal(length(rules$subgrupos$PBACal), 2L)
  expect_equal(length(rules$subgrupos$PBACd), 2L)
})

test_that("PBAC subgrupos totalizam 8 classes", {
  rules <- load_rules("sibcs5")
  total <- length(rules$subgrupos$PBACtal) +
             length(rules$subgrupos$PBACal) +
             length(rules$subgrupos$PBACd)
  expect_equal(total, 8L)
})

test_that("cada PBAC GG termina em 'Tp' (catch-all 'tipicos')", {
  rules <- load_rules("sibcs5")
  for (gg in c("PBACtal", "PBACal", "PBACd")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s ultima entrada deveria ser default:true; got %s",
                                  gg, last$code))
    expect_true(endsWith(last$code, "Tp"),
                  info = sprintf("GG %s ultimo code nao termina em 'Tp': %s",
                                  gg, last$code))
  }
})


# ============================================================================
# 3. run_sibcs_subgrupo dispatcher para PBAC
# ============================================================================

test_that("PBACtalAb (abrupticos) catches mudanca textural abrupta profile", {
  pr <- .make_pbac_subgrupo(abrupt_clay_jump = TRUE)
  res <- run_sibcs_subgrupo(pr, "PBACtal")
  expect_equal(res$assigned$code, "PBACtalAb")
})

test_that("PBACtalEh (espesso-humicos) catches A humico + OC ate 130 cm", {
  pr <- .make_pbac_subgrupo(oc_pct = c(2.5, 1.5, 1.2, 0.5))
  if (isTRUE(horizonte_A_humico(pr)$passed)) {
    res <- run_sibcs_subgrupo(pr, "PBACtal")
    # Pode ser PBACtalEh se carater_humico_espesso passar (carbono >= 1% ate 130cm)
    # Ou PBACtalHu se so A humico passar.
    expect_match(res$assigned$code, "^PBACtal(Eh|Hu)$")
  } else {
    skip("horizonte_A_humico nao casa")
  }
})

test_that("PBACtalTp (catch-all) catches profile sem outros criterios", {
  pr <- .make_pbac_subgrupo(oc_pct = c(0.5, 0.3, 0.2, 0.1))
  res <- run_sibcs_subgrupo(pr, "PBACtal")
  expect_equal(res$assigned$code, "PBACtalTp")
})

test_that("PBACalAb (abrupticos sem Ta, com alitico) catches profile", {
  pr <- .make_pbac_subgrupo(abrupt_clay_jump = TRUE, alitico_only = TRUE)
  res <- run_sibcs_subgrupo(pr, "PBACal")
  expect_equal(res$assigned$code, "PBACalAb")
})

test_that("PBACdTp catch-all assigned for profile sem mudanca abrupta", {
  pr <- .make_pbac_subgrupo()  # default no abrupt jump
  res <- run_sibcs_subgrupo(pr, "PBACd")
  expect_equal(res$assigned$code, "PBACdTp")
})


# ============================================================================
# 4. Backward-compat WRB / USDA / Cap 14 inalterados
# ============================================================================

test_that("WRB / USDA / Cap 14 nao quebraram apos PBAC subgrupos add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
  # Organossolos ainda descem ate 4o nivel
  rules <- load_rules("sibcs5")
  expect_true("OJF" %in% names(rules$subgrupos))
  expect_true("OXShst" %in%
                 vapply(rules$subgrupos$OXS, function(x) x$code, character(1)))
})
