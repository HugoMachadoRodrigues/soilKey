# v0.7.14.A SiBCS Cap 18 (Familia, 5o nivel) -- Parte A:
# motor + 5 dimensoes (texturais, cascalhos, esqueletica, tipo A).

# ------------------------------------------------------------------
# 1. FamilyAttribute: classe e contrato
# ------------------------------------------------------------------

test_that("FamilyAttribute aceita value NULL e retorna campos esperados", {
  fa <- FamilyAttribute$new(
    name = "grupamento_textural", value = "argilosa",
    evidence = list(clay = 50)
  )
  expect_equal(fa$name, "grupamento_textural")
  expect_equal(fa$value, "argilosa")
  expect_true(is.list(fa$evidence))

  fa_null <- FamilyAttribute$new(name = "x", value = NULL)
  expect_null(fa_null$value)
})

# ------------------------------------------------------------------
# 2. familia_grupamento_textural (Cap 1, p 46)
# ------------------------------------------------------------------

test_that("grupamento textural: argilosa quando clay 35-60%", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 45, silt_pct = 25, sand_pct = 30
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_grupamento_textural(pr)
  expect_equal(fa$value, "argilosa")
})

test_that("grupamento textural: muito_argilosa quando clay > 60%", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 70, silt_pct = 20, sand_pct = 10
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_grupamento_textural(pr)
  expect_equal(fa$value, "muito_argilosa")
})

test_that("grupamento textural: arenosa quando sand-clay > 70%", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 8, silt_pct = 5, sand_pct = 87
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_grupamento_textural(pr)
  expect_equal(fa$value, "arenosa")
})

test_that("grupamento textural: media quando 15<sand<70 e clay<35", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 25, silt_pct = 30, sand_pct = 45
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_grupamento_textural(pr)
  expect_equal(fa$value, "media")
})

test_that("grupamento textural: siltosa quando clay<35 e sand<15", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 25, silt_pct = 65, sand_pct = 10
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_grupamento_textural(pr)
  expect_equal(fa$value, "siltosa")
})

test_that("grupamento textural: NULL quando textura toda NA, missing reportado", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = NA_real_,
    silt_pct = NA_real_, sand_pct = NA_real_
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_grupamento_textural(pr)
  expect_null(fa$value)
  expect_true("clay_pct" %in% fa$missing)
})

# ------------------------------------------------------------------
# 3. familia_subgrupamento_textural (Cap 18, p 283)
# ------------------------------------------------------------------

test_that("subgrupamento textural: muito_arenosa para sand >= 85, clay <= 10", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "C", clay_pct = 5, silt_pct = 5, sand_pct = 90
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_subgrupamento_textural(pr)
  expect_equal(fa$value, "muito_arenosa")
})

test_that("subgrupamento textural: arenosa-media para areia franca", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "C", clay_pct = 8, silt_pct = 12, sand_pct = 80
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_subgrupamento_textural(pr)
  expect_equal(fa$value, "arenosa-media")
})

test_that("subgrupamento textural: media-argilosa para franco-argiloarenosa", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 28, silt_pct = 12, sand_pct = 60
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_subgrupamento_textural(pr)
  expect_equal(fa$value, "media-argilosa")
})

# ------------------------------------------------------------------
# 4. familia_distribuicao_cascalhos
# ------------------------------------------------------------------

test_that("cascalhos: NULL quando coarse < 8%", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 30, silt_pct = 20, sand_pct = 50,
    coarse_fragments_pct = 5
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_distribuicao_cascalhos(pr)
  expect_null(fa$value)
})

test_that("cascalhos: pouco_cascalhenta quando 8 <= cf < 15", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 30, silt_pct = 20, sand_pct = 50,
    coarse_fragments_pct = 12
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_distribuicao_cascalhos(pr)
  expect_equal(fa$value, "pouco_cascalhenta")
})

test_that("cascalhos: muito_cascalhenta quando cf > 50", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 30, silt_pct = 20, sand_pct = 50,
    coarse_fragments_pct = 60
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_distribuicao_cascalhos(pr)
  expect_equal(fa$value, "muito_cascalhenta")
})

# ------------------------------------------------------------------
# 5. familia_constituicao_esqueletica
# ------------------------------------------------------------------

test_that("esqueletica: passa para coarse 35-90%", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 30, silt_pct = 20, sand_pct = 50,
    coarse_fragments_pct = 50
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_constituicao_esqueletica(pr)
  expect_equal(fa$value, "esqueletica")
})

# ------------------------------------------------------------------
# 6. classify_sibcs_familia (motor)
# ------------------------------------------------------------------

test_that("motor classify_sibcs_familia retorna lista nomeada", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 50, silt_pct = 25, sand_pct = 25
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "P")
  expect_true(is.list(out))
  expect_true("grupamento_textural" %in% names(out))
  expect_true("distribuicao_cascalhos" %in% names(out))
  expect_true("constituicao_esqueletica" %in% names(out))
  expect_true("tipo_horizonte_superficial" %in% names(out))
})

test_that("motor: ordem E (Espodossolos) usa subgrupamento textural", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "Bs", clay_pct = 5, silt_pct = 5, sand_pct = 90
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "E")
  expect_true("subgrupamento_textural" %in% names(out))
  expect_false("grupamento_textural" %in% names(out))
})

test_that("motor: ordem M (Chernossolos) skip tipo de A", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B", clay_pct = 30, silt_pct = 20, sand_pct = 50
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "M")
  expect_false("tipo_horizonte_superficial" %in% names(out))
})

test_that("motor: SG arenico forca subgrupamento textural mesmo em P", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "Bt", clay_pct = 12, silt_pct = 8, sand_pct = 80
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, sg_code = "PVdAr")
  expect_true("subgrupamento_textural" %in% names(out))
  expect_false("grupamento_textural" %in% names(out))
})

# ------------------------------------------------------------------
# 7. familia_label
# ------------------------------------------------------------------

test_that("familia_label concatena valores nao-nulos com virgula", {
  fa1 <- FamilyAttribute$new(name = "a", value = "argilosa")
  fa2 <- FamilyAttribute$new(name = "b", value = NULL)
  fa3 <- FamilyAttribute$new(name = "c", value = "moderado")
  out <- list(fa1, fa2, fa3)
  expect_equal(familia_label(out), "argilosa, moderado")
})

test_that("familia_label vazio quando nenhum value definido", {
  fa1 <- FamilyAttribute$new(name = "a", value = NULL)
  expect_equal(familia_label(list(fa1)), "")
})

# ------------------------------------------------------------------
# 8. Backward-compat
# ------------------------------------------------------------------

test_that("WRB / USDA inalterados apos Cap 18A add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
