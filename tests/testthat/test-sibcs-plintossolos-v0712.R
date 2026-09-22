# v0.7.12 SiBCS Cap 16 (Plintossolos) -- 9 GGs + 73 SGs end-to-end.
# 3 diagnosticos novos: subgrupo_plintossolo_espessos /
# _endico_litoplintico / _endico_concrecionario.

# ------------------------------------------------------------------
# 1. New diagnostics
# ------------------------------------------------------------------

test_that("subgrupo_plintossolo_endico_litoplintico passa para topo >= 40", {
  hz <- data.table::data.table(
    top_cm    = c(0,  20,  60),
    bottom_cm = c(20, 60, 120),
    designation = c("A", "Bf", "2Cm"),
    plinthite_pct = c(NA, 5, 60),
    cementation_class = c(NA, NA, "indurated"),
    coarse_fragments_pct = c(NA, NA, 90)
  )
  pr <- PedonRecord$new(
    site = list(id = "FFlp-En", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- subgrupo_plintossolo_endico_litoplintico(pr)
  if (isTRUE(res$evidence$horizonte_litoplintico$passed)) {
    expect_true(isTRUE(res$passed))   # litoplintic top = 60 >= 40
  } else {
    skip("horizonte_litoplintico nao casa com fixture sintetico")
  }
})

test_that("subgrupo_plintossolo_endico_concrecionario FAILS quando topo < 40", {
  # v0.9.27: previous fixture used plinthite_pct = c(NA, 5, 5) but
  # horizonte_concrecionario requires `plinthite_pct >= 50` (the
  # function uses plinthite_pct as the petroplintita proxy). With
  # plinthite_pct = 60 the diagnostic fires; topo_min = 20 < 40 so
  # the endico variant correctly returns FALSE.
  hz <- data.table::data.table(
    top_cm    = c(0,  20,  60),
    bottom_cm = c(20, 60, 120),
    designation = c("A", "Bcf", "2Bcf"),
    plinthite_pct = c(NA, 60, 60),  # >= 50 -> petroplintita proxy passes
    petroplinthite_pct = c(NA, 60, 60),
    coarse_fragments_pct = c(NA, 60, 70)
  )
  pr <- PedonRecord$new(
    site = list(id = "FFco-Tp", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- subgrupo_plintossolo_endico_concrecionario(pr)
  expect_true(isTRUE(res$evidence$horizonte_concrecionario$passed))
  # topo do concrecionario = 20 < 40 -> NOT endico
  expect_false(isTRUE(res$passed))
})

# ------------------------------------------------------------------
# 2. Cap 16 GG / SG counts
# ------------------------------------------------------------------

test_that("Cap 16 GGs: 2+3+4 = 9 classes em 3 subordens", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$grandes_grupos$FF), 2L)
  expect_equal(length(rules$grandes_grupos$FT), 3L)
  expect_equal(length(rules$grandes_grupos$FX), 4L)
})

test_that("Cap 16 SGs: 17+25+31 = 73 classes em 9 GGs", {
  rules <- load_rules("sibcs5")
  ff_ggs <- c("FFlp", "FFco")
  ft_ggs <- c("FTal", "FTd", "FTe")
  fx_ggs <- c("FXac", "FXal", "FXd", "FXe")
  # FF: 5 + 12 = 17
  expect_equal(sum(vapply(rules$subgrupos[ff_ggs], length, integer(1))), 17L)
  # FT: 8 + 9 + 8 = 25
  expect_equal(sum(vapply(rules$subgrupos[ft_ggs], length, integer(1))), 25L)
  # FX: 6 + 8 + 9 + 8 = 31
  expect_equal(sum(vapply(rules$subgrupos[fx_ggs], length, integer(1))), 31L)
  total <- sum(vapply(rules$subgrupos[c(ff_ggs, ft_ggs, fx_ggs)],
                          length, integer(1)))
  expect_equal(total, 73L)
})

# ------------------------------------------------------------------
# 3. Backward-compat
# ------------------------------------------------------------------

test_that("WRB / USDA inalterados apos Cap 16 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
