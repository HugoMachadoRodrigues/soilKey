# v0.7.4.C.2 SiBCS Cap 5 (Argissolos) -- Stage C.2: 48 PV Subgrupos.
#
# 1 diagnostico novo: carater_sombrico (Cap 5 PV 4.2.6 Aluminicos sombricos).
# COMPLETA Cap 5 end-to-end: 23 GGs + 165 SGs.


# ============================================================================
# 1. carater_sombrico (NOVO)
# ============================================================================

test_that("carater_sombrico passes for dark B + OC >= 0.5%", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bsm"),
    munsell_value_moist  = c(3, 3),    # escuro
    munsell_chroma_moist = c(3, 2),    # baixo chroma
    oc_pct = c(2.0, 1.2)               # OC alto em B
  )
  pr <- PedonRecord$new(
    site = list(id = "SOMB", lat = -27, lon = -49, country = "BR",
                  parent_material = "altitudinal"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_sombrico(pr)$passed))
})

test_that("carater_sombrico FAILS para B claro (value > 4)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(4, 5),     # B claro
    munsell_chroma_moist = c(4, 4),
    oc_pct = c(1.5, 0.8)
  )
  pr <- PedonRecord$new(
    site = list(id = "S-FAIL", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_sombrico(pr)$passed))
})

test_that("carater_sombrico FAILS quando OC subsuperficial < 0.5%", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_value_moist = c(3, 3),     # escuro
    munsell_chroma_moist = c(2, 2),
    oc_pct = c(2.0, 0.2)               # OC baixo em B
  )
  pr <- PedonRecord$new(
    site = list(id = "S-OC", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_sombrico(pr)$passed))
})


# ============================================================================
# 2. YAML structural integrity para PV
# ============================================================================

test_that("PV subgrupos: 6+7+4+9+6+16 = 48 classes em 6 GGs", {
  rules <- load_rules("sibcs5")
  for (gg in c("PVtal", "PVal", "PVtd", "PVd", "PVef", "PVe")) {
    expect_true(gg %in% names(rules$subgrupos),
                  info = sprintf("GG %s ausente", gg))
  }
  expect_equal(length(rules$subgrupos$PVtal),  6L)
  expect_equal(length(rules$subgrupos$PVal),   7L)   # inclui sombricos
  expect_equal(length(rules$subgrupos$PVtd),   4L)
  expect_equal(length(rules$subgrupos$PVd),    9L)
  expect_equal(length(rules$subgrupos$PVef),   6L)
  expect_equal(length(rules$subgrupos$PVe),   16L)
  total <- sum(vapply(rules$subgrupos[c("PVtal","PVal","PVtd","PVd","PVef","PVe")],
                          length, integer(1)))
  expect_equal(total, 48L)
})

test_that("Cap 5 Argissolos COMPLETO: 165 subgrupos em 23 GGs", {
  rules <- load_rules("sibcs5")
  argissolos_ggs <- c(
    "PBACtal", "PBACal", "PBACd",
    "PACdc", "PACd", "PACe",
    "PAtal", "PAal", "PAdc", "PAd", "PAec", "PAe",
    "PVtal", "PVal", "PVtd", "PVd", "PVef", "PVe",
    "PVAtal", "PVAal", "PVAtd", "PVAd", "PVAe"
  )
  expect_equal(length(argissolos_ggs), 23L)
  total_sgs <- sum(vapply(rules$subgrupos[argissolos_ggs],
                              length, integer(1)))
  expect_equal(total_sgs, 165L,
                 info = sprintf("Cap 5 Argissolos esperado 165 SGs; got %d",
                                  total_sgs))
})

test_that("PValSm (sombricos) presente em PV Aluminicos", {
  rules <- load_rules("sibcs5")
  pval_codes <- vapply(rules$subgrupos$PVal,
                          function(x) x$code, character(1))
  expect_true("PValSm" %in% pval_codes)
})


# ============================================================================
# 3. Dispatcher tests
# ============================================================================

test_that("PVtalAb (Ta Aluminicos abrupticos) catches abrupt + Ta + alitico", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60, 130),
    bottom_cm = c(20, 60, 130, 200),
    designation = c("A", "BA", "Bt", "BC"),
    munsell_hue_moist = c("2.5YR", "2.5YR", "2.5YR", "2.5YR"),  # vermelho
    boundary_distinctness = c("abrupt", "clear", "gradual", "gradual"),
    clay_pct = c(15, 35, 40, 40),
    cec_cmol = c(8, 30, 30, 30),       # Ta
    bs_pct   = c(40, 30, 30, 30),
    al_cmol  = c(0.3, 5, 5, 5),        # alitico
    al_sat_pct = c(15, 60, 60, 60)
  )
  pr <- PedonRecord$new(
    site = list(id = "PVTAL", lat = -23, lon = -47, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_subgrupo(pr, "PVtal")
  expect_match(res$assigned$code, "^PVtal(Ab|Tp)$")
})

test_that("PVeTp (catch-all) selected for plain eutrofico vermelho profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 200),
    designation = c("A", "BA", "Bt"),
    munsell_hue_moist = c("2.5YR", "2.5YR", "2.5YR"),
    clay_pct = c(20, 30, 35),
    cec_cmol = c(8, 6, 6),
    bs_pct = c(75, 70, 70),    # eutrofico
    boundary_distinctness = c("clear", "gradual", "gradual")
  )
  pr <- PedonRecord$new(
    site = list(id = "PVE-TP", lat = -23, lon = -47, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_subgrupo(pr, "PVe")
  expect_equal(res$assigned$code, "PVeTp")
})


# ============================================================================
# 4. Backward-compat & cap 5 final verification
# ============================================================================

test_that("Cap 14 Organossolos inalterado apos Cap 5 completo", {
  rules <- load_rules("sibcs5")
  # Organossolos GGs (9) + SGs (42) preservados
  org_ggs <- c("OJ", "OO", "OX")
  total_org_ggs <- sum(vapply(rules$grandes_grupos[org_ggs],
                                  length, integer(1)))
  expect_equal(total_org_ggs, 9L)
  org_sgs <- c("OJF", "OJH", "OJS", "OOF", "OOH", "OOS", "OXF", "OXH", "OXS")
  total_org_sgs <- sum(vapply(rules$subgrupos[org_sgs],
                                  length, integer(1)))
  expect_equal(total_org_sgs, 42L)
})

test_that("WRB / USDA classificacao inalterada apos Cap 5 final", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
