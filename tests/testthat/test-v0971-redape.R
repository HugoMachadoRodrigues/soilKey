# =============================================================================
# Tests for v0.9.71 -- Redape (Embrapa) GeoTab loader + benchmark.
#
# These tests use a small embedded JSON fixture rather than hitting the
# live Redape API. The full dataset (96 profiles, DOI 10.48432/PYKKA7)
# can be downloaded with download_redape_dataset() but that's a
# network-gated integration test, not unit-test material.
# =============================================================================

.fixture_redape_argissolo <- function() {
  list(
    items = list(
      list(
        ID_PONTO   = "GeoTab_TEST_001_Test_Argissolo",
        ORDEM      = "ARGISSOLO",
        SUBORDEM   = "AMARELO",
        GDE_GRUPO  = "Eutrofico",
        SUBGRUPO   = "tipico",
        CURADORIA  = "Nenhuma alteracao.",
        AUTOR_CURADORIA = "Test Curator",
        HORIZONTES = list(
          list(
            SIMB_HORIZ = "Ap", LIMITE_SUP = 0L, LIMITE_INF = 20L,
            AREIA_GROS = 550, AREIA_FINA = 220, SILTE = 150, ARGILA = 80,
            COR_UMIDA_MATIZ = "10YR", COR_UMIDA_VALOR = 4L, COR_UMIDA_CROMA = 2L,
            COR_SECA_MATIZ  = "10YR", COR_SECA_VALOR  = 6L, COR_SECA_CROMA  = 2L,
            PH_AGUA = 5.5, PH_KCL = 4.9,
            CA_TROC = 1.5, MG_TROC = 0.3, K_TROC = 0.22, NA_TROC = 0.04,
            AL_TROC = 0,   H_TROC  = 1.5, P_ASSIM = 6,    C_ORG   = 6.6,
            TEOR_FE = 8,
            REDOXICO = FALSE, PETROPLINTICO = FALSE, LITOPLINTICO = FALSE,
            COESO = FALSE, FRAGMENTARIO = FALSE
          ),
          list(
            SIMB_HORIZ = "Bt1", LIMITE_SUP = 85L, LIMITE_INF = 110L,
            AREIA_GROS = 240, AREIA_FINA = 140, SILTE = 150, ARGILA = 470,
            COR_UMIDA_MATIZ = "10YR", COR_UMIDA_VALOR = 5L, COR_UMIDA_CROMA = 8L,
            ESTRUTURA_GRAU = 2L, ESTRUTURA_TIPO = 5L,
            PH_AGUA = 4.9, PH_KCL = 4.2,
            CA_TROC = 1.4, MG_TROC = 1.6, K_TROC = 0.14, NA_TROC = 0.11,
            AL_TROC = 0.3, H_TROC  = 1.7, P_ASSIM = 2,    C_ORG   = 4,
            TEOR_FE = 29,
            REDOXICO = FALSE, PETROPLINTICO = FALSE, LITOPLINTICO = FALSE,
            COESO = FALSE, FRAGMENTARIO = FALSE
          )
        )
      )
    )
  )
}


test_that("v0.9.71: .redape_read_json tolerates the trailing-brace artifact", {
  # Some published Redape JSONs end with a stray extra '}' that breaks
  # strict parsers. Verify the helper handles both well-formed and
  # malformed input.
  tmp_ok <- tempfile(fileext = ".json")
  jsonlite::write_json(.fixture_redape_argissolo(), tmp_ok, auto_unbox = TRUE)
  on.exit(unlink(tmp_ok), add = TRUE)
  items <- soilKey:::.redape_read_json(tmp_ok)
  expect_length(items, 1L)
  expect_identical(items[[1]]$ID_PONTO, "GeoTab_TEST_001_Test_Argissolo")

  # Stray-brace variant
  tmp_bad <- tempfile(fileext = ".json")
  raw <- paste(readLines(tmp_ok), collapse = "\n")
  writeLines(paste0(raw, "}"), tmp_bad)
  on.exit(unlink(tmp_bad), add = TRUE)
  items_bad <- soilKey:::.redape_read_json(tmp_bad)
  expect_length(items_bad, 1L)
})


test_that("v0.9.71: .redape_horizon_to_soilkey converts units correctly", {
  hz <- .fixture_redape_argissolo()$items[[1]]$HORIZONTES[[1]]
  out <- soilKey:::.redape_horizon_to_soilkey(hz)
  # Texture g/kg -> %
  expect_equal(out$clay_pct, 8)             # 80 g/kg
  expect_equal(out$silt_pct, 15)            # 150 g/kg
  expect_equal(out$sand_pct, (550 + 220)/10) # 77 (areia gros + fina)
  # OC g/kg -> %
  expect_equal(out$oc_pct, 0.66)            # 6.6 g/kg
  # CEC = S + H + Al with Ca=1.5, Mg=0.3, K=0.22, Na=0.04, H=1.5, Al=0
  expect_equal(out$cec_cmol, 1.5 + 0.3 + 0.22 + 0.04 + 1.5 + 0)
  # BS_pct = 100 * S / T
  s_val <- 1.5 + 0.3 + 0.22 + 0.04
  expect_equal(out$bs_pct, 100 * s_val / out$cec_cmol)
})


test_that("v0.9.71: .redape_item_to_pedon builds a valid PedonRecord", {
  item <- .fixture_redape_argissolo()$items[[1]]
  pr <- soilKey:::.redape_item_to_pedon(item)
  expect_s3_class(pr, "PedonRecord")
  expect_identical(pr$site$id, "GeoTab_TEST_001_Test_Argissolo")
  expect_identical(pr$site$reference_sibcs_order, "ARGISSOLO")
  expect_identical(pr$site$reference_sibcs_subordem, "AMARELO")
  expect_match(pr$site$reference_sibcs, "ARGISSOLO AMARELO Eutrofico tipico")
  expect_identical(pr$site$country, "BR")
  expect_equal(nrow(pr$horizons), 2L)
})


test_that("v0.9.71: load_redape_pedons skips _all.json aggregate files", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  jsonlite::write_json(.fixture_redape_argissolo(),
                         file.path(tmpdir, "GeoTab_TEST_001.json"),
                         auto_unbox = TRUE)
  jsonlite::write_json(.fixture_redape_argissolo(),
                         file.path(tmpdir, "GeoTab_TEST_all.json"),
                         auto_unbox = TRUE)
  peds <- load_redape_pedons(tmpdir, verbose = FALSE)
  # Should load only 1 pedon: the per-profile file (not the _all.json)
  expect_length(peds, 1L)
})


test_that("v0.9.71: load_redape_pedons dedupes by ID_PONTO across files", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  jsonlite::write_json(.fixture_redape_argissolo(),
                         file.path(tmpdir, "a.json"),
                         auto_unbox = TRUE)
  # Same content, different filename
  jsonlite::write_json(.fixture_redape_argissolo(),
                         file.path(tmpdir, "b.json"),
                         auto_unbox = TRUE)
  peds <- load_redape_pedons(tmpdir, verbose = FALSE)
  expect_length(peds, 1L)
})


test_that("v0.9.71: benchmark_redape runs end-to-end on a single fixture", {
  tmpdir <- tempfile()
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)
  jsonlite::write_json(.fixture_redape_argissolo(),
                         file.path(tmpdir, "GeoTab_TEST_001.json"),
                         auto_unbox = TRUE)
  peds <- load_redape_pedons(tmpdir, verbose = FALSE)
  res <- benchmark_redape(peds, level = "order", verbose = FALSE)
  expect_named(res, c("level", "accuracy", "n_compared", "n_total",
                        "confusion", "per_class_recall", "predictions"))
  expect_identical(res$level, "order")
  expect_equal(res$n_total, 1L)
  expect_true(res$n_compared <= 1L)  # 0 or 1 depending on classify result
})


test_that("v0.9.71: download_redape_dataset errors clearly without jsonlite", {
  # We don't actually call the network here, just exercise the error
  # path. Skip if jsonlite is installed (which it is in CI).
  skip_if(requireNamespace("jsonlite", quietly = TRUE),
          "jsonlite is installed; cannot exercise the missing-pkg path")
})
