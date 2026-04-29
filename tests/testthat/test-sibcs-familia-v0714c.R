# v0.7.14.C SiBCS Cap 18 (Familia, 5o nivel) -- Parte C:
# mineralogia (areia + argila Latossolos) + atividade da argila +
# teor de oxidos de ferro + propriedades andicas.

# ------------------------------------------------------------------
# 1. familia_mineralogia_areia
# ------------------------------------------------------------------

test_that("mineralogia_areia: 'micacea' quando sand_mica_pct >= 15", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B",
    sand_mica_pct = 20, sand_amphibole_pct = 5, sand_feldspar_pct = 3
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_mineralogia_areia(pr)
  expect_equal(fa$value, "micacea")
})

test_that("mineralogia_areia: 'feldspatica' via maior valor quando varios passam", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B",
    sand_mica_pct = 16, sand_amphibole_pct = 17, sand_feldspar_pct = 30
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_mineralogia_areia(pr)
  expect_equal(fa$value, "feldspatica")
})

test_that("mineralogia_areia: NULL quando todos < threshold", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B",
    sand_mica_pct = 5, sand_amphibole_pct = 3, sand_feldspar_pct = 2
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_mineralogia_areia(pr)
  expect_null(fa$value)
})

test_that("mineralogia_areia: fallback para sand_mineralogy quando %s NA", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "B",
    sand_mineralogy = "anfibolitica"
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_mineralogia_areia(pr)
  expect_equal(fa$value, "anfibolitica")
})

# ------------------------------------------------------------------
# 2. familia_mineralogia_argila_latossolo (Ki / Kr)
# ------------------------------------------------------------------

test_that("mineralogia_argila: 'caulinitico' quando Ki>0.75 e Kr>0.75", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "Bw",
    sio2_sulfuric_pct = 25, al2o3_sulfuric_pct = 30,
    fe2o3_sulfuric_pct = 8
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_mineralogia_argila_latossolo(pr)
  ki <- fa$evidence$ki; kr <- fa$evidence$kr
  expect_true(!is.na(ki) && ki > 0.75)
  expect_equal(fa$value, "caulinitico")
})

test_that("mineralogia_argila: 'gibsitico-oxidico' quando Ki<=0.75 e Kr<=0.75", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "Bw",
    # SiO2 baixo, Al2O3 alto, Fe2O3 alto -> Ki e Kr pequenos
    sio2_sulfuric_pct = 5, al2o3_sulfuric_pct = 40,
    fe2o3_sulfuric_pct = 20
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_mineralogia_argila_latossolo(pr)
  ki <- fa$evidence$ki; kr <- fa$evidence$kr
  expect_true(ki <= 0.75 && kr <= 0.75)
  expect_equal(fa$value, "gibsitico-oxidico")
})

test_that("mineralogia_argila: missing reportado quando sulfurico todo NA", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 200,
    designation = "Bw"
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_mineralogia_argila_latossolo(pr)
  expect_null(fa$value)
  expect_true("sio2_sulfuric_pct" %in% fa$missing)
})

# ------------------------------------------------------------------
# 3. familia_atividade_argila
# ------------------------------------------------------------------

test_that("atividade_argila: Tmb quando T < 8", {
  # cec_cmol = 5, clay = 70 -> T = 5*100/70 = 7.14 < 8
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "B", clay_pct = 70, cec_cmol = 5
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_atividade_argila(pr)
  expect_equal(fa$value, "Tmb")
})

test_that("atividade_argila: Tm quando 17 <= T < 27", {
  # cec=15, clay=70 -> T = 21.4
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "B", clay_pct = 70, cec_cmol = 15
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_atividade_argila(pr)
  expect_equal(fa$value, "Tm")
})

test_that("atividade_argila: Tma quando T >= 40", {
  # cec=20, clay=40 -> T = 50
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "B", clay_pct = 40, cec_cmol = 20
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_atividade_argila(pr)
  expect_equal(fa$value, "Tma")
})

test_that("atividade_argila: NULL quando textura areia/areia franca", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "C", clay_pct = 1, cec_cmol = 5
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_atividade_argila(pr)
  expect_null(fa$value)
})

# ------------------------------------------------------------------
# 4. familia_oxidos_ferro
# ------------------------------------------------------------------

test_that("oxidos_ferro: hipoferrico quando Fe2O3 < 8%", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "B", fe2o3_sulfuric_pct = 5
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_oxidos_ferro(pr)
  expect_equal(fa$value, "hipoferrico")
})

test_that("oxidos_ferro: mesoferrico quando 8 <= Fe2O3 < 18", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "B", fe2o3_sulfuric_pct = 12
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_oxidos_ferro(pr)
  expect_equal(fa$value, "mesoferrico")
})

test_that("oxidos_ferro: ferrico quando 18 <= Fe2O3 < 36", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "B", fe2o3_sulfuric_pct = 25
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_oxidos_ferro(pr)
  expect_equal(fa$value, "ferrico")
})

test_that("oxidos_ferro: perferrico quando Fe2O3 >= 36", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 150,
    designation = "B", fe2o3_sulfuric_pct = 40
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_oxidos_ferro(pr)
  expect_equal(fa$value, "perferrico")
})

# ------------------------------------------------------------------
# 5. familia_andico
# ------------------------------------------------------------------

test_that("andico: passa quando todas 3 condicoes satisfeitas em alguma camada", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 80),
    designation = c("Oh", "Bs"),
    bulk_density_g_cm3 = c(0.7, 0.85),
    phosphate_retention_pct = c(90, 88),
    al_ox_pct = c(2.5, 1.5),
    fe_ox_pct = c(1.0, 2.0)   # 2.5 + 0.5*1 = 3 OK; 1.5 + 0.5*2 = 2.5 OK
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_andico(pr)
  expect_equal(fa$value, "andico")
})

test_that("andico: NULL quando densidade > 0.9", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 80),
    designation = c("Oh", "Bs"),
    bulk_density_g_cm3 = c(1.1, 1.2),
    phosphate_retention_pct = c(90, 88),
    al_ox_pct = c(2.5, 1.5),
    fe_ox_pct = c(1.0, 2.0)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_andico(pr)
  expect_null(fa$value)
})

# ------------------------------------------------------------------
# 6. classify_sibcs_familia integra novas dimensoes (parte C)
# ------------------------------------------------------------------

test_that("motor: ordem L (Latossolos) inclui mineralogia_argila e oxidos_ferro mas skipa atividade", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 200),
    designation = c("A", "Bw"),
    clay_pct = c(50, 60), silt_pct = c(20, 15), sand_pct = c(30, 25),
    sio2_sulfuric_pct = c(NA, 12),
    al2o3_sulfuric_pct = c(NA, 28),
    fe2o3_sulfuric_pct = c(NA, 14),
    cec_cmol = c(NA, 5)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "L")
  expect_true("mineralogia_argila" %in% names(out))
  expect_true("oxidos_ferro" %in% names(out))
  expect_false("atividade_argila" %in% names(out))
  expect_false("mineralogia_areia" %in% names(out))
})

test_that("motor: ordem C (Cambissolos) inclui mineralogia_areia e andico (Histicos)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bi"),
    clay_pct = c(20, 30), silt_pct = c(30, 30), sand_pct = c(50, 40),
    sand_mineralogy = c(NA, "micacea")
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "C")
  expect_true("mineralogia_areia" %in% names(out))
  expect_equal(out$mineralogia_areia$value, "micacea")
  expect_true("andico" %in% names(out))
})

# ------------------------------------------------------------------
# 7. Backward-compat
# ------------------------------------------------------------------

test_that("WRB / USDA inalterados apos Cap 18C add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
