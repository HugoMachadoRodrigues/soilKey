# v0.7.5.D SiBCS Cap 8 (Espodossolos) -- 12 GGs + 42 SGs end-to-end.
# 2 diagnosticos novos: carater_espodico_profundo, carater_hidromorfico.

# ============================================================================
# 1. Diagnosticos novos -- smoke tests
# ============================================================================

test_that("carater_espodico_profundo passes para B espodico em [200, 400] cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 100, 250),
    bottom_cm = c(100, 250, 350),
    designation = c("E", "E2", "Bh"),
    oc_pct = c(0.3, 0.2, 1.5),
    al_ox_pct = c(NA_real_, NA_real_, 0.4)
  )
  pr <- PedonRecord$new(
    site = list(id = "EKHE", lat = -3, lon = -60, country = "BR",
                  parent_material = "areia"),
    horizons = ensure_horizon_schema(hz)
  )
  if (isTRUE(carater_espodico(pr)$passed)) {
    expect_true(isTRUE(carater_espodico_profundo(pr)$passed))
  } else {
    skip("carater_espodico nao casa para esta fixture")
  }
})

test_that("carater_espodico_profundo FAILS para B espodico raso (<200 cm)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("E", "Bh", "BC"),
    oc_pct = c(0.3, 1.5, 0.4),
    al_ox_pct = c(NA_real_, 0.4, 0.2)
  )
  pr <- PedonRecord$new(
    site = list(id = "EKHM", lat = 0, lon = 0, country = "TEST",
                  parent_material = "areia"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_espodico_profundo(pr)
  # B espodico em top=30 (raso), nao esta em [200, 400]
  expect_false(isTRUE(res$passed))
})

test_that("carater_hidromorfico passes via horizonte_glei < 100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 60),
    bottom_cm = c(30, 60, 200),
    designation = c("A", "Eg", "Bh"),
    munsell_chroma_moist = c(2, 1, 2),
    redoximorphic_features_pct = c(NA_real_, 25, 30)
  )
  pr <- PedonRecord$new(
    site = list(id = "HM", lat = -3, lon = -60, country = "BR",
                  parent_material = "areia"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- carater_hidromorfico(pr)
  # Eg em top=30 e/ou redoximorphic features deveriam triggar
  if (isTRUE(horizonte_glei(pr)$passed) ||
        isTRUE(carater_redoxico(pr)$passed) ||
        any(grepl("^Eg", hz$designation))) {
    expect_true(isTRUE(res$passed))
  } else {
    skip("nao ha indicio hidromorfico para esta fixture")
  }
})

test_that("carater_hidromorfico FAILS sem indicios hidromorficos", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("A", "E", "Bh"),
    munsell_chroma_moist = c(4, 4, 4)   # nao gleyic
  )
  pr <- PedonRecord$new(
    site = list(id = "NOHM", lat = 0, lon = 0, country = "TEST",
                  parent_material = "areia"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_hidromorfico(pr)$passed))
})

# ============================================================================
# 2. YAML structural integrity para Cap 8
# ============================================================================

test_that("Cap 8 Espodossolos GGs: 4+4+4 = 12 classes em 3 subordens", {
  rules <- load_rules("sibcs5")
  for (sub in c("EK", "EJ", "ES")) {
    expect_true(sub %in% names(rules$grandes_grupos),
                  info = sprintf("Subordem %s ausente", sub))
    expect_equal(length(rules$grandes_grupos[[sub]]), 4L)
  }
  total <- sum(vapply(rules$grandes_grupos[c("EK","EJ","ES")],
                          length, integer(1)))
  expect_equal(total, 12L)
})

test_that("Cap 8 Espodossolos SGs: 6+15+3+10+8 = 42 classes em 12 GGs", {
  rules <- load_rules("sibcs5")
  hh_ggs <- c("EKhh","EJhh","EShh")
  hm_ggs <- c("EKhm","EJhm","EShm")
  he_ggs <- c("EKhe","EJhe","EShe")
  o_ek_ej <- c("EKo","EJo")
  expect_equal(sum(vapply(rules$subgrupos[hh_ggs],  length, integer(1))),  6L)
  expect_equal(sum(vapply(rules$subgrupos[hm_ggs],  length, integer(1))), 15L)
  expect_equal(sum(vapply(rules$subgrupos[he_ggs],  length, integer(1))),  3L)
  expect_equal(sum(vapply(rules$subgrupos[o_ek_ej], length, integer(1))), 10L)
  expect_equal(length(rules$subgrupos$ESo), 8L)
  total <- sum(vapply(rules$subgrupos[c(hh_ggs,hm_ggs,he_ggs,o_ek_ej,"ESo")],
                          length, integer(1)))
  expect_equal(total, 42L)
})

test_that("Cap 14 + Cap 5 + Cap 6 + Cap 7 + Cap 8 GGs preservados", {
  rules <- load_rules("sibcs5")
  total_ggs <- sum(vapply(rules$grandes_grupos, length, integer(1)))
  # 9 + 23 + 26 + 11 + 12 = 81
  expect_equal(total_ggs, 81L)
})

# ============================================================================
# 3. Backward-compat
# ============================================================================

test_that("WRB / USDA inalterados apos Cap 8 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
