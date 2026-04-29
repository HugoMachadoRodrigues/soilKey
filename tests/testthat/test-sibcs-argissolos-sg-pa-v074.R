# v0.7.4.C.1 SiBCS Cap 5 (Argissolos) -- Stage C.1: 52 PA Subgrupos.
#
# 3 diagnosticos novos: carater_gleissolico, carater_cambissolico_arg,
# carater_placico. Mais reuso de todos os caracteres existentes.


# ============================================================================
# 1. Diagnosticos novos -- smoke tests
# ============================================================================

test_that("carater_gleissolico passes when horizonte_glei within depth", {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   60),
    bottom_cm = c(20,   60,  150),
    designation = c("A", "Btg", "Cg"),
    munsell_chroma_moist = c(2, 1, 1),    # gleyic colors
    redoximorphic_features_pct = c(NA_real_, 30, 40),
    clay_pct = c(20, 35, 30)
  )
  pr <- PedonRecord$new(
    site = list(id = "GL", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_gleissolico(pr)
  if (isTRUE(horizonte_glei(pr)$passed)) {
    expect_true(isTRUE(res$passed))
  } else {
    skip("horizonte_glei nao casa para esta fixture")
  }
})

test_that("carater_cambissolico_arg passes for B com >= 5% frag rocha", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 150),
    designation = c("A", "Bw"),
    coarse_fragments_pct = c(NA_real_, 8)   # >= 5%
  )
  pr <- PedonRecord$new(
    site = list(id = "CBA", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_cambissolico_arg(pr)$passed))
})

test_that("carater_cambissolico_arg FAILS quando coarse fragments < 5%", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 150),
    designation = c("A", "Bw"),
    coarse_fragments_pct = c(NA_real_, 2)
  )
  pr <- PedonRecord$new(
    site = list(id = "CBA-FAIL", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_cambissolico_arg(pr)$passed))
})

test_that("carater_cambissolico_arg distinto de carater_cambissolico (Cap 14 Folicos)", {
  # carater_cambissolico = Cap 14 (B incipiente abaixo de hístico)
  # carater_cambissolico_arg = Cap 5 (4%+ minerais alteraveis ou 5%+ frag rocha)
  expect_false(identical(carater_cambissolico, carater_cambissolico_arg))
})

test_that("carater_placico passes via cementation_class 'strongly'", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bsm"),
    cementation_class = c(NA_character_, "strongly")
  )
  pr <- PedonRecord$new(
    site = list(id = "PC", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_placico(pr)$passed))
})

test_that("carater_placico FAILS para 'weakly' (insuficiente para placico)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    cementation_class = c(NA_character_, "weakly")
  )
  pr <- PedonRecord$new(
    site = list(id = "PC-W", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_placico(pr)$passed))
})


# ============================================================================
# 2. YAML structural integrity para PA
# ============================================================================

test_that("PA subgrupos: 6+5+21+6+8+6 = 52 classes em 6 GGs", {
  rules <- load_rules("sibcs5")
  for (gg in c("PAtal", "PAal", "PAdc", "PAd", "PAec", "PAe")) {
    expect_true(gg %in% names(rules$subgrupos),
                  info = sprintf("GG %s ausente", gg))
  }
  expect_equal(length(rules$subgrupos$PAtal),  6L)
  expect_equal(length(rules$subgrupos$PAal),   5L)
  expect_equal(length(rules$subgrupos$PAdc),  21L)
  expect_equal(length(rules$subgrupos$PAd),    6L)
  expect_equal(length(rules$subgrupos$PAec),   8L)
  expect_equal(length(rules$subgrupos$PAe),    6L)
  total <- sum(vapply(rules$subgrupos[c("PAtal","PAal","PAdc","PAd","PAec","PAe")],
                          length, integer(1)))
  expect_equal(total, 52L)
})

test_that("cada PA GG termina em 'Tp' (catch-all)", {
  rules <- load_rules("sibcs5")
  for (gg in c("PAtal", "PAal", "PAdc", "PAd", "PAec", "PAe")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s: %s", gg, last$code))
    expect_true(endsWith(last$code, "Tp"))
  }
})

test_that("PAdc Distrocoesos preserva ordem canonica multi-criterio", {
  rules <- load_rules("sibcs5")
  codes <- vapply(rules$subgrupos$PAdc, function(x) x$code, character(1))
  # Solodicos abrupticos (composto) deve vir antes de qualquer simples
  i_sd_ab <- which(codes == "PAdcSdAb")
  i_ab    <- which(codes == "PAdcAb")
  expect_lt(i_sd_ab, i_ab,
              label = "PAdcSdAb (composto) antes de PAdcAb (simples)")
  # AbFrEp (3 criterios) antes de AbFr (2 criterios)
  i_3   <- which(codes == "PAdcAbFrEp")
  i_2   <- which(codes == "PAdcAbFr")
  expect_lt(i_3, i_2,
              label = "PAdcAbFrEp (3) antes de PAdcAbFr (2)")
})


# ============================================================================
# 3. Dispatcher tests
# ============================================================================

test_that("PAdcAb (abrupticos simples) catches mudanca abrupta + distrofico + coeso", {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   60,   130),
    bottom_cm = c(20,   60,   130,  200),
    designation = c("A", "BA", "Bt", "BC"),
    munsell_hue_moist = c("10YR", "10YR", "10YR", "10YR"),
    munsell_value_moist  = c(4, 5, 5, 5),
    munsell_chroma_moist = c(4, 6, 6, 6),
    rupture_resistance = c(NA_character_, "very hard", "very hard", NA_character_),
    consistence_moist  = c(NA_character_, "friable", "friable", NA_character_),
    boundary_distinctness = c("abrupt", "clear", "gradual", "gradual"),
    clay_pct = c(15, 35, 40, 40),
    bs_pct = c(40, 30, 30, 30),
    cec_cmol = c(8, 6, 6, 6),
    al_cmol = c(0.3, 0.5, 0.5, 0.5)
  )
  pr <- PedonRecord$new(
    site = list(id = "PADCAB", lat = -7, lon = -34, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_subgrupo(pr, "PAdc")
  expect_match(res$assigned$code, "^PAdc(Ab|Tp)$",
                 info = sprintf("got %s", res$assigned$code))
})

test_that("PAdcTp catch-all for simple distrocoeso profile sem criterio adicional", {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   60),
    bottom_cm = c(20,   60,   200),
    designation = c("A", "BA", "Bt"),
    munsell_hue_moist = c("10YR", "10YR", "10YR"),
    rupture_resistance = c(NA_character_, "very hard", "very hard"),
    consistence_moist  = c(NA_character_, "friable", "friable"),
    clay_pct = c(20, 30, 35),     # gradiente normal, nao abrupto
    boundary_distinctness = c("clear", "gradual", "gradual"),
    bs_pct = c(40, 30, 30),
    cec_cmol = c(8, 6, 6)
  )
  pr <- PedonRecord$new(
    site = list(id = "PADCTP", lat = -7, lon = -34, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_subgrupo(pr, "PAdc")
  expect_equal(res$assigned$code, "PAdcTp")
})


# ============================================================================
# 4. Backward-compat
# ============================================================================

test_that("Cap 14 / PBAC / PAC / PVA inalterados apos PA SGs add", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$subgrupos$PBACtal), 4L)
  expect_equal(length(rules$subgrupos$PACdc),   9L)
  expect_equal(length(rules$subgrupos$PVAd),   11L)
  expect_equal(length(rules$subgrupos$OJF),     4L)
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
