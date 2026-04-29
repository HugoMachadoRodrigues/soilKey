# v0.7.5.A SiBCS Cap 6 (Cambissolos) -- Stage A: 26 Grandes Grupos.
#
# 3 diagnosticos novos: carater_perferrico, carater_vertissolico,
# carater_argiluvico.


# ============================================================================
# 1. Diagnosticos novos -- smoke tests
# ============================================================================

test_that("carater_perferrico passes for Fe2O3 sulfurico >= 36%", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    fe2o3_sulfuric_pct = c(NA_real_, 40)   # 400 g/kg = perferrico
  )
  pr <- PedonRecord$new(
    site = list(id = "PF", lat = -20, lon = -45, country = "BR",
                  parent_material = "basalto"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_perferrico(pr)$passed))
})

test_that("carater_perferrico FAILS para Fe2O3 entre 18-36% (= ferrico, nao perferrico)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    fe2o3_sulfuric_pct = c(NA_real_, 25)   # ferrico, NOT perferrico
  )
  pr <- PedonRecord$new(
    site = list(id = "PF-low", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_perferrico(pr)$passed))
  # Mas carater_ferrico passa
  expect_true(isTRUE(carater_ferrico(pr)$passed))
})

test_that("carater_vertissolico passes para horizonte vertico < 150 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "Bw", "Bv"),
    clay_pct = c(40, 50, 60),     # >= 30%
    cracks_width_cm = c(NA_real_, NA_real_, 2),
    cole_value = c(NA_real_, NA_real_, 0.10),   # COLE alto
    slickensides = c(NA_character_, NA_character_, "common")
  )
  pr <- PedonRecord$new(
    site = list(id = "VS", lat = -10, lon = -45, country = "BR",
                  parent_material = "basalto"),
    horizons = ensure_horizon_schema(hz)
  )
  if (isTRUE(horizonte_vertico(pr)$passed)) {
    expect_true(isTRUE(carater_vertissolico(pr)$passed))
  } else {
    skip("horizonte_vertico nao casa para esta fixture")
  }
})

test_that("carater_argiluvico passes para B textural < 150 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 200),
    designation = c("A", "BA", "Bt"),
    clay_pct = c(15, 25, 38),    # gradiente clay
    boundary_distinctness = c("clear", "clear", "gradual"),
    cec_cmol = c(8, 6, 6),
    bs_pct = c(40, 30, 30),
    silt_pct = c(30, 25, 22),
    sand_pct = c(55, 50, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "AL", lat = -23, lon = -47, country = "BR",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  if (isTRUE(B_textural(pr)$passed)) {
    expect_true(isTRUE(carater_argiluvico(pr)$passed))
  } else {
    skip("B_textural nao casa para esta fixture")
  }
})


# ============================================================================
# 2. YAML structural integrity para Cap 6 GGs
# ============================================================================

test_that("Cap 6 Cambissolos GGs: 2+4+8+12 = 26 classes em 4 subordens", {
  rules <- load_rules("sibcs5")
  for (sub in c("CH", "CHU", "CY", "CX")) {
    expect_true(sub %in% names(rules$grandes_grupos),
                  info = sprintf("Subordem %s ausente em grandes_grupos", sub))
  }
  expect_equal(length(rules$grandes_grupos$CH),   2L)
  expect_equal(length(rules$grandes_grupos$CHU),  4L)
  expect_equal(length(rules$grandes_grupos$CY),   8L)
  expect_equal(length(rules$grandes_grupos$CX),  12L)
  total <- sum(vapply(rules$grandes_grupos[c("CH","CHU","CY","CX")],
                          length, integer(1)))
  expect_equal(total, 26L)
})

test_that("Cambissolos GG codes seguem convencao CamelCase", {
  rules <- load_rules("sibcs5")
  # CHUaf = Cambissolos Humicos Aluminoferricos
  chu_codes <- vapply(rules$grandes_grupos$CHU,
                         function(x) x$code, character(1))
  expect_true("CHUaf" %in% chu_codes)
  expect_true("CHUdf" %in% chu_codes)
  # CXpf = Cambissolos Haplicos Perferricos
  cx_codes <- vapply(rules$grandes_grupos$CX,
                        function(x) x$code, character(1))
  expect_true("CXpf" %in% cx_codes)
  expect_true("CXtef" %in% cx_codes)
})


# ============================================================================
# 3. Dispatcher tests
# ============================================================================

test_that("CHUaf (Aluminoferricos) catches alitico + ferrico profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    al_cmol = c(0.3, 5),         # alitico
    al_sat_pct = c(15, 60),
    bs_pct = c(40, 30),          # < 50 (alitico requirement)
    fe2o3_sulfuric_pct = c(NA_real_, 25)   # 18-36% = ferrico
  )
  pr <- PedonRecord$new(
    site = list(id = "CHUAF", lat = -28, lon = -50, country = "BR",
                  parent_material = "basalto"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_grande_grupo(pr, "CHU")
  expect_equal(res$assigned$code, "CHUaf")
})

test_that("CXpf (Perferricos) catches Fe2O3 >= 36% profile", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    fe2o3_sulfuric_pct = c(NA_real_, 40),
    bs_pct = c(40, 30)
  )
  pr <- PedonRecord$new(
    site = list(id = "CXPF", lat = -10, lon = -45, country = "BR",
                  parent_material = "basalto"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_grande_grupo(pr, "CX")
  expect_equal(res$assigned$code, "CXpf")
})

test_that("CXtef (Ta Eutroferricos) catches Ta + V>=50 + Fe2O3 18-36%", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    cec_cmol = c(15, 30),       # Ta = 30*100/40 = 75
    clay_pct = c(20, 40),
    bs_pct = c(75, 75),          # eutrofico
    fe2o3_sulfuric_pct = c(NA_real_, 25)
  )
  pr <- PedonRecord$new(
    site = list(id = "CXTEF", lat = -22, lon = -45, country = "BR",
                  parent_material = "basalto"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_grande_grupo(pr, "CX")
  expect_equal(res$assigned$code, "CXtef")
})


# ============================================================================
# 4. Backward-compat
# ============================================================================

test_that("Cap 14 + Cap 5 GGs preservados apos Cap 6 GGs add", {
  rules <- load_rules("sibcs5")
  # Cap 14 Organossolos
  expect_equal(length(rules$grandes_grupos$OJ), 3L)
  # Cap 5 Argissolos
  expect_equal(length(rules$grandes_grupos$PVA), 5L)
  # Total GGs nos systemas: 9 (Cap 14) + 23 (Cap 5) + 26 (Cap 6) = 58
  total_ggs <- sum(vapply(rules$grandes_grupos, length, integer(1)))
  expect_equal(total_ggs, 58L)
})

test_that("WRB / USDA classificacao inalterada apos Cap 6 GGs add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
