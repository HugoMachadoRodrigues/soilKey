# =============================================================================
# Tests for v0.9.60 -- benchmark_bdsolos() + .bdsolos_find_header_line
# quote-aware fix.
#
# Two concerns covered:
#
#   1. Regression for the v0.9.58 header-line bug: data rows in real
#      Embrapa exports embed `;` inside quoted descriptions (surveyor
#      names, geology remarks). The v0.9.58 strsplit(fixed = TRUE)
#      counter inflated their field count above the true header,
#      so which.max() picked a data row as the "header" and the loader
#      reported 0% taxon / 0% Munsell on the real RJ.csv (722 perfis
#      affected). v0.9.60 switches to scan(quote = "\"") per line.
#
#   2. Smoke + coverage tests for benchmark_bdsolos():
#      - all 3 systems on a SiBCS-only fixture -> wrb/usda report
#        no_reference_labels, sibcs reports a real accuracy.
#      - max_n truncates correctly.
#      - the soilKey_version is captured in $config.
# =============================================================================


# ---- 1. .bdsolos_find_header_line quote-awareness ----------------------

test_that(".bdsolos_find_header_line picks the true header even when data rows embed ; in quotes", {
  skip_on_cran()
  # Build a 5-line fixture mimicking the BDsolos full export:
  #   line 1: preamble (comma)
  #   line 2: blank
  #   line 3: header (8 quoted fields, ; separator)
  #   line 4-5: data rows whose `Responsavel` field carries a literal
  #             `;` inside the quoted string -- inflating the
  #             quote-unaware semicolon count.
  tf <- tempfile(fileext = ".csv")
  hdr <- paste(c('"Codigo PA"', '"Responsavel"', '"Tipo"',
                  '"UF"', '"Classificacao Atual"',
                  '"Cor da Amostra Umida - Matiz"',
                  '"Cor da Amostra Umida - Valor"',
                  '"Cor da Amostra Umida - Croma"'),
                collapse = ";")
  data1 <- paste(c('"100"',
                    '"Klaus Wittern; Elias Mothci; Bras Calderano"',  # 2 embedded ;
                    '"Perfil completo"', '"RJ"',
                    '"ARGISSOLO VERMELHO Distrofico"',
                    '"10YR"', '"4"', '"3"'), collapse = ";")
  data2 <- paste(c('"101"',
                    '"R. Santos; M. Silva; J. Costa; A. Lima"',       # 3 embedded ;
                    '"Perfil completo"', '"RJ"',
                    '"LATOSSOLO VERMELHO Distrofico"',
                    '"2.5YR"', '"3"', '"6"'), collapse = ";")
  writeLines(c(
    "Dados obtidos a partir do BDSOLOS, esclarecimentos em: http://...",
    "",
    hdr,
    data1,
    data2
  ), tf)
  on.exit(unlink(tf), add = TRUE)

  expect_equal(soilKey:::.bdsolos_find_header_line(tf), 3L)
})


test_that("load_bdsolos_csv populates triple-system reference labels on full schema", {
  skip_on_cran()
  # End-to-end check: the same kind of fixture that the v0.9.60 fix
  # destravas -- 3 reference columns + Munsell + DMS coords.
  tf <- tempfile(fileext = ".csv")
  hdr <- paste(c(
    '"Codigo PA"', '"Simbolo Horizonte"',
    '"Profundidade Superior"', '"Profundidade Inferior"',
    '"Cor da Amostra Umida - Matiz"',
    '"Cor da Amostra Umida - Valor"',
    '"Cor da Amostra Umida - Croma"',
    '"Composicao Granulometrica da terra fina - Argila (g/Kg)"',
    '"Composicao Granulometrica da terra fina - Silte (g/Kg)"',
    '"Composicao Granulometrica da terra fina - Areia Total (g/Kg)"',
    '"pH - H2O"', '"Carbono organico"',
    '"Classificacao Atual"',
    '"Classificacao FAO/WRB"',
    '"Classificacao Soil Taxonomy"',
    '"UF"', '"Municipio"',
    '"Latitude Graus"', '"Latitude Minutos"',
    '"Latitude Segundos"', '"Latitude Hemisferio"'
  ), collapse = ";")
  rows <- c(
    paste(c('"100"', '"A"', '"0"', '"15"',
              '"10YR"', '"4"', '"3"',
              '"180"', '"300"', '"520"', '"5.5"', '"15"',
              '"ARGISSOLO VERMELHO Tb Distrofico"',
              '"Haplic Acrisol"',
              '"Typic Hapludult"',
              '"RJ"', '"Itaguai"',
              '"22"', '"51"', '"30"', '"S"'), collapse = ";"),
    paste(c('"100"', '"Bt"', '"15"', '"60"',
              '"5YR"', '"4"', '"6"',
              '"450"', '"200"', '"350"', '"5.0"', '"3"',
              '"ARGISSOLO VERMELHO Tb Distrofico"',
              '"Haplic Acrisol"',
              '"Typic Hapludult"',
              '"RJ"', '"Itaguai"',
              '"22"', '"51"', '"30"', '"S"'), collapse = ";"),
    paste(c('"101"', '"A"', '"0"', '"20"',
              '"2.5YR"', '"3"', '"5"',
              '"550"', '"200"', '"250"', '"5.8"', '"20"',
              '"LATOSSOLO VERMELHO Distroferrico tipico"',
              '"Rhodic Ferralsol"',
              '"Typic Haplorthox"',
              '"MG"', '"Sete Lagoas"',
              '"19"', '"30"', '"0"', '"S"'), collapse = ";")
  )
  writeLines(c(
    "Dados obtidos a partir do BDSOLOS, esclarecimentos em: http://...",
    "",
    hdr,
    rows
  ), tf)
  on.exit(unlink(tf), add = TRUE)

  pedons <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_length(pedons, 2L)
  expect_equal(pedons[[1L]]$site$reference_sibcs,
                "ARGISSOLO VERMELHO Tb Distrofico")
  expect_equal(pedons[[1L]]$site$reference_wrb, "Haplic Acrisol")
  expect_equal(pedons[[1L]]$site$reference_st,  "Typic Hapludult")
  expect_equal(pedons[[2L]]$site$reference_wrb, "Rhodic Ferralsol")
  expect_equal(pedons[[2L]]$site$reference_st,  "Typic Haplorthox")
})


# ---- 2. benchmark_bdsolos() -------------------------------------------

# Helper: build 3 PedonRecord stubs with various label combinations.
.make_bdsolos_test_pedons <- function() {
  hz_argis <- data.frame(
    top_cm    = c(0,  20),
    bottom_cm = c(20, 80),
    designation = c("A", "Bt"),
    munsell_hue_moist    = c("10YR", "5YR"),
    munsell_value_moist  = c(4, 4),
    munsell_chroma_moist = c(3, 6),
    clay_pct = c(18, 45),
    silt_pct = c(30, 20),
    sand_pct = c(52, 35),
    ph_h2o   = c(5.5, 5.0),
    oc_pct   = c(1.5, 0.3),
    base_saturation_pct = c(40, 25),
    stringsAsFactors = FALSE
  )
  hz_lato <- data.frame(
    top_cm    = c(0,  20),
    bottom_cm = c(20, 100),
    designation = c("A", "Bw"),
    munsell_hue_moist    = c("2.5YR", "2.5YR"),
    munsell_value_moist  = c(3, 3),
    munsell_chroma_moist = c(5, 6),
    clay_pct = c(55, 60),
    silt_pct = c(20, 18),
    sand_pct = c(25, 22),
    ph_h2o   = c(5.8, 5.2),
    oc_pct   = c(2.0, 0.5),
    base_saturation_pct = c(20, 15),
    stringsAsFactors = FALSE
  )
  list(
    PedonRecord$new(site = list(
      id = "rj-1", lat = -22.86, lon = -43.78, country = "BR",
      reference_sibcs = "ARGISSOLO VERMELHO Distrofico tipico",
      reference_wrb   = "Haplic Acrisol",
      reference_st    = "Typic Hapludult"
    ), horizons = hz_argis),
    PedonRecord$new(site = list(
      id = "mg-7", lat = -19.5, lon = -43.9, country = "BR",
      reference_sibcs = "LATOSSOLO VERMELHO Distroferrico tipico",
      reference_wrb   = NA_character_,
      reference_st    = NA_character_
    ), horizons = hz_lato),
    PedonRecord$new(site = list(
      id = "rj-2", lat = -22.50, lon = -43.20, country = "BR",
      reference_sibcs = "ARGISSOLO AMARELO Distrofico",
      reference_wrb   = "Xanthic Ferralsol",
      reference_st    = NA_character_
    ), horizons = hz_argis)
  )
}


test_that("benchmark_bdsolos errors on empty / non-list input", {
  skip_on_cran()
  expect_error(benchmark_bdsolos(NULL),       "non-empty list")
  expect_error(benchmark_bdsolos(list()),     "non-empty list")
  expect_error(benchmark_bdsolos("not list"), "non-empty list")
})


test_that("benchmark_bdsolos reports per-system label coverage", {
  skip_on_cran()
  peds <- .make_bdsolos_test_pedons()
  out  <- benchmark_bdsolos(peds, verbose = FALSE)
  expect_named(out$coverage, c("wrb2022", "sibcs", "usda"))
  # All 3 SiBCS, 2 WRB, 1 USDA
  expect_equal(out$coverage$sibcs$n_with_ref,   3L)
  expect_equal(out$coverage$wrb2022$n_with_ref, 2L)
  expect_equal(out$coverage$usda$n_with_ref,    1L)
  expect_equal(out$coverage$sibcs$pct,   100)
  expect_equal(out$coverage$wrb2022$pct, round(100 * 2 / 3, 1))
  expect_equal(out$coverage$usda$pct,    round(100 * 1 / 3, 1))
})


test_that("benchmark_bdsolos returns NA accuracy + message on no-label systems", {
  skip_on_cran()
  peds <- .make_bdsolos_test_pedons()
  # Strip WRB + USDA labels from all pedons -> per_system$wrb/usda should
  # carry message = "no_reference_labels"
  for (p in peds) {
    p$site$reference_wrb <- NA_character_
    p$site$reference_st  <- NA_character_
  }
  out <- benchmark_bdsolos(peds, verbose = FALSE)
  expect_true(is.na(out$per_system$wrb2022$accuracy))
  expect_true(is.na(out$per_system$usda$accuracy))
  expect_equal(out$per_system$wrb2022$message, "no_reference_labels")
  expect_equal(out$per_system$usda$message,    "no_reference_labels")
  expect_false(is.na(out$per_system$sibcs$accuracy))
})


test_that("benchmark_bdsolos compares SiBCS Order at the right granularity", {
  skip_on_cran()
  peds <- .make_bdsolos_test_pedons()
  out  <- benchmark_bdsolos(peds, systems = "sibcs", sibcs_level = "order",
                              verbose = FALSE)
  ps <- out$per_system$sibcs
  expect_equal(ps$n_compared, 3L)   # all 3 carry SiBCS labels
  expect_true(ps$n_correct >= 0L && ps$n_correct <= 3L)
  expect_true(ps$accuracy >= 0 && ps$accuracy <= 1)
  # Confusion: Order labels are normalised to "Argissolos" / "Latossolos"
  cm <- ps$confusion
  expect_true(!is.null(cm))
  expect_true(any(rownames(cm) %in% c("Argissolos", "Latossolos")))
})


test_that("benchmark_bdsolos respects max_n", {
  skip_on_cran()
  peds <- .make_bdsolos_test_pedons()
  out  <- benchmark_bdsolos(peds, max_n = 1L, verbose = FALSE)
  expect_equal(out$config$n_pedons, 1L)
  expect_equal(out$coverage$sibcs$n_total, 1L)
})


test_that("benchmark_bdsolos $config captures soilKey_version + timestamp", {
  skip_on_cran()
  peds <- .make_bdsolos_test_pedons()
  out  <- benchmark_bdsolos(peds, verbose = FALSE)
  expect_true(grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$",
                      out$config$soilKey_version))
  expect_s3_class(out$config$timestamp, "POSIXct")
  expect_equal(out$config$systems,
                c("wrb2022", "sibcs", "usda"))
  expect_equal(out$config$sibcs_level, "order")
})


# ---- 3. SiBCS legacy -> modern Order map (v0.9.60 lift) ---------------

test_that("normalise_febr_sibcs maps legacy Podzolicos to Argissolos", {
  skip_on_cran()
  expect_equal(normalise_febr_sibcs("PODZOLICO VERMELHO-AMARELO",
                                       level = "order"),
                "Argissolos")
  expect_equal(normalise_febr_sibcs("PODZOLICOS VERMELHOS",
                                       level = "order"),
                "Argissolos")
})


test_that("normalise_febr_sibcs maps legacy Glei to Gleissolos", {
  skip_on_cran()
  expect_equal(normalise_febr_sibcs("GLEI HUMICO",      level = "order"),
                "Gleissolos")
  expect_equal(normalise_febr_sibcs("GLEI POUCO HUMICO", level = "order"),
                "Gleissolos")
})


test_that("normalise_febr_sibcs maps legacy Aluvial to Neossolos", {
  skip_on_cran()
  expect_equal(normalise_febr_sibcs("ALUVIAL EUTROFICO", level = "order"),
                "Neossolos")
  expect_equal(normalise_febr_sibcs("ALUVIAIS DISTROFICOS", level = "order"),
                "Neossolos")
})


test_that("normalise_febr_sibcs returns NA for orphan 'Solos' fragments", {
  skip_on_cran()
  expect_true(is.na(normalise_febr_sibcs("SOLOS HALOMORFICOS",
                                            level = "order")))
  expect_true(is.na(normalise_febr_sibcs("SOLOS HIDROMORFICOS",
                                            level = "order")))
})


test_that("normalise_febr_sibcs subordem also propagates legacy map", {
  skip_on_cran()
  expect_equal(normalise_febr_sibcs("PODZOLICO VERMELHO-AMARELO",
                                       level = "subordem"),
                "Argissolos Vermelho-amarelos")
  # Out-of-scope Order at subordem level returns NA, not "NA <token>"
  expect_true(is.na(normalise_febr_sibcs("SOLOS HALOMORFICOS",
                                            level = "subordem")))
})


test_that("normalise_febr_sibcs preserves modern names unchanged", {
  skip_on_cran()
  expect_equal(normalise_febr_sibcs("ARGISSOLO VERMELHO", level = "order"),
                "Argissolos")
  expect_equal(normalise_febr_sibcs("LATOSSOLO VERMELHO", level = "order"),
                "Latossolos")
  expect_equal(normalise_febr_sibcs("GLEISSOLO HAPLICO", level = "order"),
                "Gleissolos")
  expect_equal(normalise_febr_sibcs("NEOSSOLO LITOLICO", level = "order"),
                "Neossolos")
})


# ---- 4. Per-pedon error tolerance + max_n -----------------------------

test_that("benchmark_bdsolos does not abort when classifier raises per-pedon", {
  skip_on_cran()
  # Force an error: blank horizons table on one pedon -> classify_sibcs
  # may raise. The benchmark must catch it and tally n_errors instead.
  peds <- .make_bdsolos_test_pedons()
  peds[[1L]]$horizons <- peds[[1L]]$horizons[0, ]   # zero-row hz
  out <- tryCatch(
    benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE),
    error = function(e) NULL
  )
  expect_false(is.null(out))
  # Either the classifier handles zero rows gracefully (n_errors = 0)
  # or it errors and is tallied -- we just require the run completes.
  expect_true(out$per_system$sibcs$n_errors >= 0L)
  expect_true(out$per_system$sibcs$n_compared <= 3L)
})
