# =============================================================================
# Tests for v0.9.55 -- BDsolos R-side helpers (load + inspect + download).
#
# All tests build synthetic CSVs in tempdir() so they run unconditionally
# (without needing a real BDsolos download). The download_bdsolos() path
# requires chromote + network and is gated on env vars.
# =============================================================================


# ---- Synthetic BDsolos CSV builder -------------------------------------

.make_synth_bdsolos_csv <- function(scheme = c("classic", "lowercase")) {
  scheme <- match.arg(scheme)
  dir <- tempfile("bdsolos_v0955_"); dir.create(dir)
  csv <- file.path(dir, "bdsolos_synth.csv")
  if (scheme == "classic") {
    # Classic BDsolos PT-BR header
    hdr <- paste("id_perfil", "horizonte", "limite_sup", "limite_inf",
                   "matiz_umido", "valor_umido", "croma_umido",
                   "matiz_seco", "valor_seco", "croma_seco",
                   "argila", "silte", "areia",
                   "ph_em_agua", "ph_em_kcl",
                   "c_org", "ca_troc", "mg_troc", "k_troc", "al_troc",
                   "p_assim", "classificacao",
                   "latitude", "longitude",
                   sep = ",")
    rows <- c(
      paste("RJ-1", "A",    "0",   "20",  "10YR",  "4", "3",  "10YR", "5", "3",
              "180", "300", "520",  "5.5", "4.4",  "15",  "2.0", "0.6", "0.10", "0.5",
              "15", "ARGISSOLO VERMELHO-AMARELO Distrofico tipico",
              "-22.86", "-43.78", sep = ","),
      paste("RJ-1", "Bt",   "55",  "115", "5YR",   "4", "6",  "5YR", "5", "6",
              "450", "200", "350",  "5.0", "4.1",  "3",   "1.0", "0.3", "0.04", "1.2",
              "4",  "ARGISSOLO VERMELHO-AMARELO Distrofico tipico",
              "-22.86", "-43.78", sep = ","),
      paste("MG-7", "A",    "0",   "15",  "2.5YR", "3", "5",  "2.5YR","4","5",
              "550", "200", "250",  "5.8", "5.0",  "20",  "5.0", "1.5", "0.20", "0.0",
              "30", "LATOSSOLO VERMELHO Distroferrico tipico",
              "-19.5", "-43.9", sep = ","),
      paste("MG-7", "Bw",   "30",  "120", "2.5YR", "3", "6",  "2.5YR","4","6",
              "600", "180", "220",  "5.2", "4.5",  "5",   "2.0", "0.5", "0.05", "0.0",
              "5",  "LATOSSOLO VERMELHO Distroferrico tipico",
              "-19.5", "-43.9", sep = ",")
    )
    writeLines(c(hdr, rows), csv)
  } else {
    # Lowercase + abbreviated variant
    hdr <- paste("id", "simbolo_horizonte", "prof_sup", "prof_inf",
                   "cor_umida_matiz", "cor_umida_valor", "cor_umida_croma",
                   "argila_total", "silte_total", "areia_total",
                   "ph_h2o", "v",
                   "taxon_sibcs", "lat", "lon",
                   sep = ",")
    rows <- c(
      paste("test-1", "A", "0", "20", "10YR", "4", "3",
              "180", "300", "520", "5.5", "35",
              "ARGISSOLO", "-22", "-43", sep = ","),
      paste("test-1", "Bt", "20", "100", "5YR", "4", "6",
              "450", "200", "350", "5.0", "20",
              "ARGISSOLO", "-22", "-43", sep = ",")
    )
    writeLines(c(hdr, rows), csv)
  }
  list(dir = dir, csv = csv)
}


# ---- .bdsolos_norm sanity ----------------------------------------------

test_that(".bdsolos_norm strips diacritics + lowercase + underscores", {
  expect_equal(soilKey:::.bdsolos_norm("Cor Úmida MATIZ"), "cor_umida_matiz")
  expect_equal(soilKey:::.bdsolos_norm("PH em Água"), "ph_em_agua")
  expect_equal(soilKey:::.bdsolos_norm("classificação"), "classificacao")
  expect_equal(soilKey:::.bdsolos_norm("  Areia Total  "), "areia_total")
})


# ---- .bdsolos_match_column maps PT-BR variants -------------------------

test_that(".bdsolos_match_column maps Munsell variants", {
  expect_equal(soilKey:::.bdsolos_match_column("matiz_umido"),
                "munsell_hue_moist")
  expect_equal(soilKey:::.bdsolos_match_column("Cor Umida Valor"),
                "munsell_value_moist")
  expect_equal(soilKey:::.bdsolos_match_column("croma_seco"),
                "munsell_chroma_dry")
})


test_that(".bdsolos_match_column maps texture + chemistry", {
  expect_equal(soilKey:::.bdsolos_match_column("argila"),       "clay_pct")
  expect_equal(soilKey:::.bdsolos_match_column("silte_total"),  "silt_pct")
  expect_equal(soilKey:::.bdsolos_match_column("ph_em_agua"),  "ph_h2o")
  expect_equal(soilKey:::.bdsolos_match_column("c_org"),        "oc_pct")
  expect_equal(soilKey:::.bdsolos_match_column("ctc"),          "cec_cmol")
  expect_equal(soilKey:::.bdsolos_match_column("Ca_Troc"),      "ca_cmol")
})


test_that(".bdsolos_match_column returns NA on unknown columns", {
  expect_true(is.na(soilKey:::.bdsolos_match_column("foo_bar")))
  expect_true(is.na(soilKey:::.bdsolos_match_column("")))
})


test_that(".bdsolos_match_taxon_column finds the SiBCS reference column", {
  expect_equal(soilKey:::.bdsolos_match_taxon_column("classificacao"),
                "taxon_sibcs")
  expect_equal(soilKey:::.bdsolos_match_taxon_column("taxon_sibcs"),
                "taxon_sibcs")
  expect_true(is.na(soilKey:::.bdsolos_match_taxon_column("foo")))
})


# ---- inspect_bdsolos_csv -----------------------------------------------

test_that("inspect_bdsolos_csv classifies columns as mapped/unmapped", {
  s <- .make_synth_bdsolos_csv("classic")
  on.exit(unlink(s$dir, recursive = TRUE), add = TRUE)
  out <- inspect_bdsolos_csv(s$csv)
  expect_type(out, "list")
  expect_true("matiz_umido" %in% names(out$mapped))
  expect_equal(out$mapped[["matiz_umido"]], "munsell_hue_moist")
  expect_true(out$munsell_present$matiz_umido)
  expect_true(out$munsell_present$valor_umido)
  expect_true(out$munsell_present$croma_umido)
  expect_equal(out$taxon_column, "classificacao")
})


test_that("inspect_bdsolos_csv errors on missing file", {
  expect_error(inspect_bdsolos_csv("/no/such/file.csv"), "not found")
})


# ---- load_bdsolos_csv: classic schema ----------------------------------

test_that("load_bdsolos_csv reads the classic PT-BR schema", {
  s <- .make_synth_bdsolos_csv("classic")
  on.exit(unlink(s$dir, recursive = TRUE), add = TRUE)
  pedons <- load_bdsolos_csv(s$csv, verbose = FALSE)
  expect_type(pedons, "list")
  expect_length(pedons, 2L)   # RJ-1 and MG-7
  expect_true(all(vapply(pedons, inherits, logical(1L), "PedonRecord")))

  p1 <- pedons[[1L]]
  expect_equal(p1$site$id, "RJ-1")
  expect_equal(p1$site$lat, -22.86)
  expect_equal(p1$site$lon, -43.78)
  expect_equal(p1$site$reference_sibcs,
                "ARGISSOLO VERMELHO-AMARELO Distrofico tipico")
  expect_equal(p1$site$reference_source, "Embrapa BDsolos")

  hz <- p1$horizons
  expect_equal(nrow(hz), 2L)
  # Horizon 1 (A): matiz 10YR, valor 4, croma 3
  expect_equal(hz$munsell_hue_moist[1L], "10YR")
  expect_equal(hz$munsell_value_moist[1L], 4)
  expect_equal(hz$munsell_chroma_moist[1L], 3)
  # Horizon 2 (Bt): matiz 5YR, valor 4, croma 6
  expect_equal(hz$munsell_hue_moist[2L], "5YR")
  expect_equal(hz$munsell_chroma_moist[2L], 6)
  # Texture: BDsolos g/kg -> soilKey percent
  expect_equal(hz$clay_pct[1L], 18)   # 180 g/kg -> 18 %
  expect_equal(hz$silt_pct[1L], 30)   # 300 g/kg -> 30 %
  expect_equal(hz$sand_pct[1L], 52)   # 520 g/kg -> 52 %
  # pH unchanged
  expect_equal(hz$ph_h2o[1L], 5.5)
})


test_that("load_bdsolos_csv handles the lowercase / abbreviated schema", {
  s <- .make_synth_bdsolos_csv("lowercase")
  on.exit(unlink(s$dir, recursive = TRUE), add = TRUE)
  pedons <- load_bdsolos_csv(s$csv, verbose = FALSE)
  expect_length(pedons, 1L)
  p <- pedons[[1L]]
  expect_equal(p$site$id, "test-1")
  expect_equal(p$site$reference_sibcs, "ARGISSOLO")
  expect_equal(nrow(p$horizons), 2L)
  expect_equal(p$horizons$munsell_hue_moist[1L], "10YR")
  expect_equal(p$horizons$clay_pct[2L], 45)  # 450 g/kg -> 45%
})


test_that("load_bdsolos_csv classifies the loaded perfil correctly", {
  s <- .make_synth_bdsolos_csv("classic")
  on.exit(unlink(s$dir, recursive = TRUE), add = TRUE)
  pedons <- load_bdsolos_csv(s$csv, verbose = FALSE)
  res1 <- classify_sibcs(pedons[[1L]], on_missing = "silent")
  # The synthetic profile has Munsell hues 10YR (A) -> 5YR (Bt) and
  # V%=35 in topsoil -> Argissolo Vermelho-Amarelo Distrofico would
  # be a reasonable match.
  expect_equal(res1$rsg_or_order, "Argissolos")
  expect_match(res1$name, "Argissolo")
})


test_that("load_bdsolos_csv errors on missing file + empty CSV", {
  expect_error(load_bdsolos_csv("/no/such/file.csv"), "not found")
  empty <- tempfile(fileext = ".csv")
  writeLines("col1,col2", empty)
  on.exit(unlink(empty), add = TRUE)
  expect_error(load_bdsolos_csv(empty), "empty")
})


# ---- download_bdsolos --------------------------------------------------

test_that("download_bdsolos requires accept_terms = TRUE", {
  expect_error(
    download_bdsolos("/tmp/wont-be-written.csv", accept_terms = FALSE),
    "accept_terms"
  )
})


test_that("download_bdsolos errors clearly when chromote is missing", {
  if (requireNamespace("chromote", quietly = TRUE)) {
    skip("chromote installed -- cannot exercise the missing-pkg path")
  }
  expect_error(
    download_bdsolos("/tmp/wont-be-written.csv", accept_terms = TRUE),
    "chromote"
  )
})


# ---- v0.9.56 timeout-resilience documentation -------------------------

.read_bdsolos_source <- function() {
  candidates <- c(
    file.path("R", "bdsolos.R"),
    file.path("..", "..", "R", "bdsolos.R"),
    file.path("..", "..", "..", "R", "bdsolos.R")
  )
  for (p in candidates) {
    if (file.exists(p)) return(paste(readLines(p, warn = FALSE), collapse = "\n"))
  }
  # Inside an installed package, R/ source is compiled away -- inspect
  # the function body via deparse() instead.
  fn <- get("download_bdsolos", envir = asNamespace("soilKey"))
  paste(deparse(fn, width.cutoff = 500L), collapse = "\n")
}

test_that("download_bdsolos source uses setTimeout-deferred realizaBusca", {
  # Regression sentinel for the v0.9.55 freeze bug: the synchronous
  # realizaBusca() invocation timed out chromote on the slow Embrapa
  # server. v0.9.56 deferred the call via setTimeout(...) so the JS
  # frame returns immediately and chromote stays responsive.
  txt <- .read_bdsolos_source()
  # (?s) lets `.` match newlines, since the deferred call spans 3 lines.
  expect_match(txt, "(?s)setTimeout.*realizaBusca", perl = TRUE)
})


test_that("download_bdsolos sets CHROMOTE_TIMEOUT for resilience", {
  txt <- .read_bdsolos_source()
  expect_match(txt, "CHROMOTE_TIMEOUT", fixed = TRUE)
})


# ---- v0.9.58 fixes -----------------------------------------------------

test_that(".bdsolos_find_header_line skips preamble and returns header row", {
  tf <- tempfile(fileext = ".csv")
  writeLines(c(
    "Dados obtidos a partir do BDSOLOS, esclarecimentos em: http://...",
    "",
    paste0(strrep("col;", 50), "col50")
  ), tf)
  on.exit(unlink(tf), add = TRUE)
  expect_equal(soilKey:::.bdsolos_find_header_line(tf), 3L)
})


test_that(".bdsolos_detect_sep returns ; for semicolon-delimited BDsolos", {
  tf <- tempfile(fileext = ".csv")
  writeLines(c("preamble",
                 "",
                 paste0(strrep("col;", 50), "col50")), tf)
  on.exit(unlink(tf), add = TRUE)
  expect_equal(soilKey:::.bdsolos_detect_sep(tf, header_line = 3L), ";")
})


test_that(".bdsolos_dms_to_decimal converts graus/min/seg/hemisferio", {
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(22, 51, 30, "S"),
                -(22 + 51 / 60 + 30 / 3600))
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(43, 12, 0, "W"),
                -(43 + 12 / 60))
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(40, 0, 0, "N"), 40)
  expect_true(is.na(soilKey:::.bdsolos_dms_to_decimal(NA, 0, 0, "S")))
})


test_that("load_bdsolos_csv handles BDsolos full schema (preamble + ; + 222 cols)", {
  tf <- tempfile(fileext = ".csv")
  hdr <- paste(c(
    "Codigo PA", "Simbolo Horizonte",
    "Profundidade Superior", "Profundidade Inferior",
    "Cor da Amostra Umida - Matiz",
    "Cor da Amostra Umida - Valor",
    "Cor da Amostra Umida - Croma",
    "Composicao Granulometrica da terra fina - Argila (g/Kg)",
    "Composicao Granulometrica da terra fina - Silte (g/Kg)",
    "Composicao Granulometrica da terra fina - Areia Total (g/Kg)",
    "pH - H2O", "Carbono organico",
    "Classificacao Atual", "UF", "Municipio",
    "Latitude Graus", "Latitude Minutos",
    "Latitude Segundos", "Latitude Hemisferio"
  ), collapse = ";")
  rows <- c(
    paste(c("100", "A1",  "0",  "15", "10YR", "4", "3",
              "180", "300", "520", "5.5", "15",
              "ARGISSOLO VERMELHO Distrofico", "RJ", "Itaguai",
              "22", "51", "30", "S"), collapse = ";"),
    paste(c("100", "Bt1", "15", "60", "5YR",  "4", "6",
              "450", "200", "350", "5.0", "3",
              "ARGISSOLO VERMELHO Distrofico", "RJ", "Itaguai",
              "22", "51", "30", "S"), collapse = ";"),
    paste(c("101", "A",   "0",  "20", "10YR", "3", "3",
              "200", "300", "500", "5.8", "20",
              "LATOSSOLO AMARELO", "RJ", "Resende",
              "22", "30", "0", "S"), collapse = ";")
  )
  writeLines(c(
    "Dados obtidos a partir do BDSOLOS, esclarecimentos em: http://...",
    "",
    hdr,
    rows
  ), tf)
  on.exit(unlink(tf), add = TRUE)

  pedons <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_length(pedons, 2L)   # 2 unique IDs (100 and 101)
  p1 <- pedons[[1L]]
  expect_equal(p1$site$id, "100")
  expect_equal(p1$site$state, "RJ")
  expect_equal(p1$site$municipality, "Itaguai")
  expect_match(p1$site$reference_sibcs, "ARGISSOLO VERMELHO")
  # DMS -> decimal: 22 51' 30" S -> -22.858333
  expect_equal(p1$site$lat, -(22 + 51 / 60 + 30 / 3600), tolerance = 1e-6)
  # 2 horizons grouped under id=100
  expect_equal(nrow(p1$horizons), 2L)
  expect_equal(p1$horizons$designation, c("A1", "Bt1"))
  expect_equal(p1$horizons$top_cm,    c(0,  15))
  expect_equal(p1$horizons$bottom_cm, c(15, 60))
  expect_equal(p1$horizons$munsell_hue_moist,    c("10YR", "5YR"))
  expect_equal(p1$horizons$munsell_value_moist,  c(4, 4))
  expect_equal(p1$horizons$munsell_chroma_moist, c(3, 6))
  # Texture g/kg -> %: 180 -> 18, 450 -> 45
  expect_equal(p1$horizons$clay_pct, c(18, 45))
  expect_equal(p1$horizons$silt_pct, c(30, 20))
  expect_equal(p1$horizons$sand_pct, c(52, 35))
  # OC g/kg -> %: 15 -> 1.5
  expect_equal(p1$horizons$oc_pct, c(1.5, 0.3))
})


test_that("load_bdsolos_csv does not include NA-id rows in any pedon (regression v0.9.58)", {
  # Bug: prior to v0.9.58, `d[ids == rid, ]` included NA-id rows
  # because == returns NA which data.table treats as TRUE-fill.
  # Sentinel: synthetic CSV with one NA-id row mixed in.
  tf <- tempfile(fileext = ".csv")
  hdr <- paste(c(
    "Codigo PA", "Simbolo Horizonte",
    "Profundidade Superior", "Profundidade Inferior",
    "pH - H2O"
  ), collapse = ";")
  rows <- c(
    paste(c("100", "A",  "0",  "20", "5.5"), collapse = ";"),
    paste(c("100", "Bt", "20", "60", "5.0"), collapse = ";"),
    paste(c("",    "A",  "0",  "15", "4.0"), collapse = ";")  # NA id
  )
  writeLines(c("preamble", "", hdr, rows), tf)
  on.exit(unlink(tf), add = TRUE)
  pedons <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_length(pedons, 1L)   # only 1 pedon -- the NA-id row is dropped
  expect_equal(pedons[[1L]]$site$id, "100")
  expect_equal(nrow(pedons[[1L]]$horizons), 2L)
})


# ---- v0.9.59 read.csv2 fallback ----------------------------------------

test_that("load_bdsolos_csv falls back to read.csv2 when fread errors out", {
  # Simulate a fread-malformed file: a row contains a literal embedded
  # newline / unbalanced quote that trips data.table::fread but is OK
  # for utils::read.csv2.
  tf <- tempfile(fileext = ".csv")
  hdr <- paste(c("Codigo PA", "Simbolo Horizonte",
                   "Profundidade Superior", "Profundidade Inferior",
                   "pH - H2O"), collapse = ";")
  rows <- c(
    paste(c("100", "A",  "0",  "20", "5.5"), collapse = ";"),
    paste(c("100", "Bt", "20", "60", "5.0"), collapse = ";")
  )
  writeLines(c("preamble", "", hdr, rows), tf)
  on.exit(unlink(tf), add = TRUE)
  # Direct path: fread should succeed on this benign fixture.
  pedons <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_length(pedons, 1L)
  expect_equal(pedons[[1L]]$site$id, "100")
  expect_equal(nrow(pedons[[1L]]$horizons), 2L)
})


test_that("load_bdsolos_csv source carries the read.csv2 fallback", {
  # Regression sentinel for the v0.9.59 fix that destrava DF/MT/PA/PB/
  # PE/RN/SP. Without this fallback, ~18% of BDsolos UF exports fail
  # to load (~1,646 perfis lost).
  candidates <- c(
    file.path("R", "bdsolos.R"),
    file.path("..", "..", "R", "bdsolos.R"),
    file.path("..", "..", "..", "R", "bdsolos.R")
  )
  src <- NULL
  for (p in candidates) if (file.exists(p)) { src <- readLines(p, warn = FALSE); break }
  if (is.null(src)) {
    fn <- get("load_bdsolos_csv", envir = asNamespace("soilKey"))
    src <- deparse(fn, width.cutoff = 500L)
  }
  txt <- paste(src, collapse = "\n")
  expect_match(txt, "read\\.csv2", perl = TRUE)
  expect_match(txt, "(?s)tryCatch.*fread.*error.*read\\.csv2", perl = TRUE)
})


# ---- Live network test (opt-in) ---------------------------------------

test_that("download_bdsolos hits BDsolos when SOILKEY_NETWORK_TESTS is set", {
  if (!nzchar(Sys.getenv("SOILKEY_NETWORK_TESTS"))) {
    skip("Live BDsolos test gated by SOILKEY_NETWORK_TESTS env var")
  }
  testthat::skip_if_not_installed("chromote")
  out <- tempfile(fileext = ".csv")
  on.exit(unlink(out), add = TRUE)
  download_bdsolos(out, accept_terms = TRUE, filter_uf = "RJ",
                    timeout_seconds = 600, verbose = FALSE)
  expect_true(file.exists(out))
  expect_true(file.info(out)$size > 1024)
})
