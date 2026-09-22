# v0.7.14.D SiBCS Cap 18 (Familia, 5o nivel) -- Parte D:
# Organossolos especificos (material subjacente / espessura > 100 cm /
# lenhosidade) + integracao classify_sibcs(include_familia=TRUE).

# ------------------------------------------------------------------
# 1. familia_organossolo_material_subjacente
# ------------------------------------------------------------------

test_that("material_subjacente: 'argiloso' quando 1a camada mineral abaixo do organico tem clay >= 35", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("H", "H2", "Cg"),
    clay_pct = c(NA_real_, NA_real_, 50),
    silt_pct = c(NA_real_, NA_real_, 30),
    sand_pct = c(NA_real_, NA_real_, 20)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_material_subjacente(pr)
  expect_equal(fa$value, "argiloso")
})

test_that("material_subjacente: 'arenoso' quando subjacente tem sand-clay > 70", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 200),
    designation = c("H", "H2", "C"),
    clay_pct = c(NA_real_, NA_real_, 5),
    silt_pct = c(NA_real_, NA_real_, 5),
    sand_pct = c(NA_real_, NA_real_, 90)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_material_subjacente(pr)
  expect_equal(fa$value, "arenoso")
})

test_that("material_subjacente: NULL quando nao ha camadas organicas", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "B"),
    clay_pct = c(20, 30),
    silt_pct = c(20, 30),
    sand_pct = c(60, 40)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_material_subjacente(pr)
  expect_null(fa$value)
})

# ------------------------------------------------------------------
# 2. familia_organossolo_espessura
# ------------------------------------------------------------------

test_that("espessura_organica: 'espesso' quando soma organico > 100 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 50, 110),
    bottom_cm = c(50, 110, 200),
    designation = c("H", "H2", "C")
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_espessura(pr)
  expect_equal(fa$value, "espesso")
  expect_equal(fa$evidence$thickness_cm, 110)
})

test_that("espessura_organica: NULL quando soma organico <= 100", {
  hz <- data.table::data.table(
    top_cm = c(0, 50),
    bottom_cm = c(50, 90),
    designation = c("H", "H2")
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_espessura(pr)
  expect_null(fa$value)
})

# ------------------------------------------------------------------
# 3. familia_organossolo_lenhosidade
# ------------------------------------------------------------------

test_that("lenhosidade: 'lenhoso' quando woody 10-30%", {
  hz <- data.table::data.table(
    top_cm = c(0, 50),
    bottom_cm = c(50, 100),
    designation = c("H", "H2"),
    woody_fragments_pct = c(15, 12)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_lenhosidade(pr)
  expect_equal(fa$value, "lenhoso")
})

test_that("lenhosidade: 'muito_lenhoso' quando max em [30, 50]", {
  hz <- data.table::data.table(
    top_cm = c(0, 50),
    bottom_cm = c(50, 100),
    designation = c("H", "H2"),
    woody_fragments_pct = c(40, 25)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_lenhosidade(pr)
  expect_equal(fa$value, "muito_lenhoso")
})

test_that("lenhosidade: 'extremamente_lenhoso' quando woody > 50", {
  hz <- data.table::data.table(
    top_cm = c(0, 50),
    bottom_cm = c(50, 100),
    designation = c("H", "H2"),
    woody_fragments_pct = c(70, 30)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_organossolo_lenhosidade(pr)
  expect_equal(fa$value, "extremamente_lenhoso")
})

# ------------------------------------------------------------------
# 4. classify_sibcs_familia(O) inclui dimensoes Organossolo
# ------------------------------------------------------------------

test_that("motor: ordem O inclui as 3 dimensoes Organossolo", {
  hz <- data.table::data.table(
    top_cm = c(0, 50, 110),
    bottom_cm = c(50, 110, 200),
    designation = c("H", "H2", "C"),
    clay_pct = c(NA_real_, NA_real_, 50),
    silt_pct = c(NA_real_, NA_real_, 30),
    sand_pct = c(NA_real_, NA_real_, 20),
    woody_fragments_pct = c(20, 25, NA_real_)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "O")
  expect_true("material_subjacente" %in% names(out))
  expect_true("espessura_organica" %in% names(out))
  expect_true("lenhosidade" %in% names(out))
  expect_equal(out$material_subjacente$value, "argiloso")
  expect_equal(out$espessura_organica$value, "espesso")
  expect_equal(out$lenhosidade$value, "lenhoso")
})

# ------------------------------------------------------------------
# 5. Integracao classify_sibcs(include_familia=TRUE)
# ------------------------------------------------------------------

test_that("classify_sibcs include_familia=FALSE (default) nao adiciona familia", {
  pr_fr <- make_ferralsol_canonical()
  res <- classify_sibcs(pr_fr, on_missing = "silent")
  expect_null(res$trace$familia)
  expect_null(res$trace$familia_label)
  expect_false(grepl(",", res$name))
})

test_that("classify_sibcs include_familia=TRUE adiciona familia ao trace e ao name", {
  pr_fr <- make_ferralsol_canonical()
  res <- classify_sibcs(pr_fr, on_missing = "silent",
                          include_familia = TRUE)
  expect_true(is.list(res$trace$familia))
  expect_true(is.character(res$trace$familia_label))
  # Nome deve conter virgula se a familia gerou ao menos um adjetivo
  if (nzchar(res$trace$familia_label)) {
    expect_true(grepl(",", res$name))
  }
})

# ------------------------------------------------------------------
# 6. Backward-compat
# ------------------------------------------------------------------

test_that("WRB / USDA inalterados apos Cap 18D add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
