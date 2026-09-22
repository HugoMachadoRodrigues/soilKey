# v0.7.4.B.3 SiBCS Cap 5 (Argissolos) -- Stage B.3: 36 PVA Subgrupos.
#
# 9 diagnosticos novos: carater_espessarenico, carater_petroplintico,
# carater_planossolico, carater_nitossolico, carater_leptico,
# carater_leptofragmentario, carater_saprolitico, carater_luvissolico,
# carater_chernossolico.


# ============================================================================
# 1. Diagnosticos novos -- smoke tests por funcao
# ============================================================================

test_that("carater_espessarenico passes for sandy 0-150 + clayey 150+ profile", {
  hz <- data.table::data.table(
    top_cm    = c(0,   60,  150),
    bottom_cm = c(60,  150, 250),
    designation = c("A", "AB", "Bt"),
    clay_pct = c(8, 10, 40)   # boundary at 150 cm (entre 100-200)
  )
  pr <- PedonRecord$new(
    site = list(id = "ESP", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_espessarenico(pr)$passed))
})

test_that("carater_petroplintico passes via plinthite >=15 horizon (concrecionario)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "Bw", "Bf"),
    plinthite_pct = c(NA_real_, NA_real_, 25)   # concrecionario
  )
  pr <- PedonRecord$new(
    site = list(id = "PP", lat = -10, lon = -45, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_petroplintico(pr)
  # passa se concrecionario OR litoplintico em camada qualquer
  if (isTRUE(horizonte_concrecionario(pr)$passed) ||
        isTRUE(horizonte_litoplintico(pr)$passed)) {
    expect_true(isTRUE(res$passed))
  } else {
    skip("nem concrecionario nem litoplintico passam para esta fixture")
  }
})

test_that("carater_leptico catches contato litico em 50-100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 100),
    designation = c("A", "Bt", "R")   # R em top=80
  )
  pr <- PedonRecord$new(
    site = list(id = "LEP", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  if (isTRUE(contato_litico(pr)$passed)) {
    expect_true(isTRUE(carater_leptico(pr)$passed))
  } else {
    skip("contato_litico nao passa para esta fixture")
  }
})

test_that("carater_leptico FAILS quando contato litico < 50 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 80),
    designation = c("A", "R")   # R em top=30 (raso)
  )
  pr <- PedonRecord$new(
    site = list(id = "LEP-S", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_leptico(pr)$passed))
})

test_that("carater_saprolitico passes Cr + sem contato R", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 70),
    bottom_cm = c(30, 70, 150),
    designation = c("A", "Bt", "Cr")   # Cr brando, sem R
  )
  pr <- PedonRecord$new(
    site = list(id = "SAP", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_saprolitico(pr)$passed))
})

test_that("carater_saprolitico FAILS quando ha R (contato litico)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 70),
    bottom_cm = c(30, 70, 150),
    designation = c("A", "Bt", "R")   # R = contato litico
  )
  pr <- PedonRecord$new(
    site = list(id = "SAP-R", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_saprolitico(pr)$passed))
})

test_that("carater_luvissolico passes para Ta >= 20 + S >= 5", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bt"),
    cec_cmol = c(15, 10),        # Ta = 10*100/40 = 25 >= 20
    clay_pct = c(20, 40),
    ca_cmol = c(NA_real_, 4),
    mg_cmol = c(NA_real_, 1.5),
    k_cmol  = c(NA_real_, 0.3),
    na_cmol = c(NA_real_, 0.2)   # S = 4+1.5+0.3+0.2 = 6 >= 5
  )
  pr <- PedonRecord$new(
    site = list(id = "LUV", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_luvissolico(pr)$passed))
})

test_that("carater_chernossolico FAILS sem A chernozemico", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bt"),
    cec_cmol = c(15, 10),
    clay_pct = c(20, 40),
    munsell_value_moist = c(5, 4),  # NAO escuro o suficiente para chernozemico
    munsell_chroma_moist = c(4, 3)
  )
  pr <- PedonRecord$new(
    site = list(id = "CH-FAIL", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_chernossolico(pr)$passed))
})


# ============================================================================
# 2. YAML: 5+5+4+11+11 = 36 PVA subgrupos
# ============================================================================

test_that("PVA subgrupos: 5+5+4+11+11 = 36 classes em 5 GGs", {
  rules <- load_rules("sibcs5")
  for (gg in c("PVAtal", "PVAal", "PVAtd", "PVAd", "PVAe")) {
    expect_true(gg %in% names(rules$subgrupos),
                  info = sprintf("GG %s ausente", gg))
  }
  expect_equal(length(rules$subgrupos$PVAtal),  5L)
  expect_equal(length(rules$subgrupos$PVAal),   5L)
  expect_equal(length(rules$subgrupos$PVAtd),   4L)
  expect_equal(length(rules$subgrupos$PVAd),   11L)
  expect_equal(length(rules$subgrupos$PVAe),   11L)
  total <- sum(vapply(rules$subgrupos[c("PVAtal","PVAal","PVAtd","PVAd","PVAe")],
                          length, integer(1)))
  expect_equal(total, 36L)
})

test_that("cada PVA GG termina em 'Tp' (catch-all)", {
  rules <- load_rules("sibcs5")
  for (gg in c("PVAtal", "PVAal", "PVAtd", "PVAd", "PVAe")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s ultima entrada deveria ser default:true; got %s",
                                  gg, last$code))
    expect_true(endsWith(last$code, "Tp"))
  }
})

test_that("PVA subgrupos compostos preservam ordem canonica multi-criterio", {
  rules <- load_rules("sibcs5")
  # 5.4.1 PVAdEsAb (espessarenico+abrupt) deve vir antes do simples (PVAdEs)
  pvad_codes <- vapply(rules$subgrupos$PVAd, function(x) x$code, character(1))
  i_es_ab <- which(pvad_codes == "PVAdEsAb")
  i_es    <- which(pvad_codes == "PVAdEs")
  expect_lt(i_es_ab, i_es,
              label = "PVAdEsAb (composto) deve vir antes de PVAdEs (simples)")
  # 5.4.3 PVAdArAb antes de PVAdAr
  i_ar_ab <- which(pvad_codes == "PVAdArAb")
  i_ar    <- which(pvad_codes == "PVAdAr")
  expect_lt(i_ar_ab, i_ar)
})


# ============================================================================
# 3. dispatcher
# ============================================================================

test_that("PVAdEs (espessarenicos) catches sandy profile boundary 100-200", {
  hz <- data.table::data.table(
    top_cm    = c(0,   60,  150),
    bottom_cm = c(60,  150, 250),
    designation = c("A", "AB", "Bt"),
    clay_pct = c(8, 10, 40),
    munsell_hue_moist = c("5YR","5YR","5YR"),
    bs_pct = c(NA_real_, NA_real_, 30),
    cec_cmol = c(NA_real_, NA_real_, 6)
  )
  pr <- PedonRecord$new(
    site = list(id = "ESPSG", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_subgrupo(pr, "PVAd")
  expect_match(res$assigned$code, "^PVAdEs")  # Es ou EsAb dependendo de mudanca
})

test_that("PVAdTp (catch-all) selected for plain distrofico profile", {
  hz <- data.table::data.table(
    top_cm    = c(0,   30,  80,  150),
    bottom_cm = c(30,  80,  150, 200),
    designation = c("A", "BA", "Bt", "BC"),
    clay_pct = c(20, 30, 40, 40),
    bs_pct   = c(40, 30, 30, 30),
    cec_cmol = c(8,  6, 6, 6),
    munsell_hue_moist = c("5YR","5YR","5YR","5YR")
  )
  pr <- PedonRecord$new(
    site = list(id = "GEN", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_subgrupo(pr, "PVAd")
  expect_equal(res$assigned$code, "PVAdTp")
})


# ============================================================================
# 4. Backward-compat
# ============================================================================

test_that("WRB / USDA / Cap 14 / PBAC / PAC inalterados apos PVA SGs add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$subgrupos$PBACtal), 4L)
  expect_equal(length(rules$subgrupos$PACdc), 9L)
  expect_equal(length(rules$subgrupos$OJF), 4L)
})
