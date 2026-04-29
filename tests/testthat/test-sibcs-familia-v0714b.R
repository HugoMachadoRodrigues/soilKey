# v0.7.14.B SiBCS Cap 18 (Familia, 5o nivel) -- Parte B:
# prefixos epi/meso/endo + saturacao bases (eutrofico/distrofico) +
# saturacao por aluminio (alico).

# ------------------------------------------------------------------
# 1. familia_prefixo_profundidade
# ------------------------------------------------------------------

test_that("prefixo: epi quando topo < 50 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 80),
    bottom_cm = c(30, 80, 150),
    designation = c("A", "Bc", "Bw")
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  diag_fake <- DiagnosticResult$new(name = "fake", passed = TRUE,
                                       layers = c(2L, 3L))
  expect_equal(familia_prefixo_profundidade(diag_fake, pr$horizons),
                 "epi")
})

test_that("prefixo: meso quando topo >= 50 e < 100", {
  hz <- data.table::data.table(
    top_cm = c(0, 60, 120),
    bottom_cm = c(60, 120, 200),
    designation = c("A", "Bc", "C")
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  diag_fake <- DiagnosticResult$new(name = "fake", passed = TRUE,
                                       layers = c(2L, 3L))
  expect_equal(familia_prefixo_profundidade(diag_fake, pr$horizons),
                 "meso")
})

test_that("prefixo: endo quando topo >= 100", {
  hz <- data.table::data.table(
    top_cm = c(0, 30, 110),
    bottom_cm = c(30, 110, 200),
    designation = c("A", "B", "Cc")
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  diag_fake <- DiagnosticResult$new(name = "fake", passed = TRUE,
                                       layers = 3L)
  expect_equal(familia_prefixo_profundidade(diag_fake, pr$horizons),
                 "endo")
})

test_that("prefixo: NULL quando diagnostico nao passa", {
  hz <- data.table::data.table(
    top_cm = 0, bottom_cm = 30, designation = "A"
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  diag_fake <- DiagnosticResult$new(name = "fake", passed = FALSE,
                                       layers = integer(0))
  expect_null(familia_prefixo_profundidade(diag_fake, pr$horizons))
})

# ------------------------------------------------------------------
# 2. familia_saturacao_bases (eutrofico/distrofico)
# ------------------------------------------------------------------

test_that("saturacao_bases: eutrofico quando V media >= 50", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 150),
    designation = c("A", "B"),
    bs_pct = c(60, 70)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_saturacao_bases(pr)
  expect_equal(fa$value, "eutrofico")
})

test_that("saturacao_bases: distrofico quando V media < 50", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 150),
    designation = c("A", "B"),
    bs_pct = c(40, 35)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_saturacao_bases(pr)
  expect_equal(fa$value, "distrofico")
})

test_that("saturacao_bases: NULL e missing reportado quando bs_pct todo NA", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 150),
    designation = c("A", "B"),
    bs_pct = c(NA_real_, NA_real_)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_saturacao_bases(pr)
  expect_null(fa$value)
  expect_true("bs_pct" %in% fa$missing)
})

# ------------------------------------------------------------------
# 3. familia_saturacao_aluminio (alico com prefixos)
# ------------------------------------------------------------------

test_that("alico: 'epialico' quando primeira camada B com Al passa < 50 cm", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 80),
    bottom_cm = c(20, 80, 150),
    designation = c("A", "Bt", "BC"),
    al_sat_pct = c(0, 70, 65),
    al_cmol = c(0.1, 2.0, 1.5)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_saturacao_aluminio(pr)
  expect_equal(fa$value, "epialico")
})

test_that("alico: 'mesoalico' quando primeira camada B com Al passa em [50,100)", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 60),
    bottom_cm = c(20, 60, 150),
    designation = c("A", "AB", "Bt"),
    al_sat_pct = c(0, 30, 70),
    al_cmol = c(0.1, 0.3, 2.0)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_saturacao_aluminio(pr)
  expect_equal(fa$value, "mesoalico")
})

test_that("alico: 'endoalico' quando primeira camada B com Al passa >= 100", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 120),
    bottom_cm = c(20, 120, 200),
    designation = c("A", "Bt1", "Bt2"),
    al_sat_pct = c(0, 0, 70),
    al_cmol = c(0.1, 0.1, 2.0)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_saturacao_aluminio(pr)
  expect_equal(fa$value, "endoalico")
})

test_that("alico: NULL quando nenhuma camada satisfaz al_sat>=50 + al_cmol>0.5", {
  hz <- data.table::data.table(
    top_cm = c(0, 20),
    bottom_cm = c(20, 150),
    designation = c("A", "Bt"),
    al_sat_pct = c(20, 30),
    al_cmol = c(0.2, 0.4)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  fa <- familia_saturacao_aluminio(pr)
  expect_null(fa$value)
})

# ------------------------------------------------------------------
# 4. classify_sibcs_familia inclui novas dimensoes (parte B)
# ------------------------------------------------------------------

test_that("motor: ordem G inclui saturacao_bases e saturacao_aluminio", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 150),
    designation = c("A", "Bg"),
    clay_pct = c(20, 25), silt_pct = c(30, 30), sand_pct = c(50, 45),
    bs_pct = c(40, 35),
    al_sat_pct = c(60, 70), al_cmol = c(1.5, 2.0)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "G")
  expect_true("saturacao_bases" %in% names(out))
  expect_true("saturacao_aluminio" %in% names(out))
  expect_equal(out$saturacao_bases$value, "distrofico")
})

test_that("motor: ordem L (Latossolos) skipa saturacao_bases e alico", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 200),
    designation = c("A", "Bw"),
    clay_pct = c(50, 60), silt_pct = c(20, 15), sand_pct = c(30, 25)
  )
  pr <- PedonRecord$new(
    site = list(id="t", lat=0, lon=0, country="BR", parent_material="t"),
    horizons = ensure_horizon_schema(hz)
  )
  out <- classify_sibcs_familia(pr, ordem_code = "L")
  expect_false("saturacao_bases" %in% names(out))
  expect_false("saturacao_aluminio" %in% names(out))
})

# ------------------------------------------------------------------
# 5. Backward-compat
# ------------------------------------------------------------------

test_that("WRB / USDA inalterados apos Cap 18B add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
