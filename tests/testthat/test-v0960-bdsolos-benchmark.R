# =============================================================================
# Tests for v0.9.60 -- benchmark_bdsolos_sibcs() + .bdsolos_normalize_ordem().
# All tests run unconditionally (no real BDsolos data required).
# =============================================================================


# ---- .bdsolos_normalize_ordem -----------------------------------------

test_that(".bdsolos_normalize_ordem maps modern Ordens", {
  expect_equal(soilKey:::.bdsolos_normalize_ordem("ARGISSOLO"),    "Argissolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("LATOSSOLO"),    "Latossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("CAMBISSOLO"),   "Cambissolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("NEOSSOLO"),     "Neossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("PLINTOSSOLO"),  "Plintossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("VERTISSOLO"),   "Vertissolos")
})


test_that(".bdsolos_normalize_ordem strips trailing taxonomic descriptors", {
  expect_equal(soilKey:::.bdsolos_normalize_ordem("LATOSSOLO VERMELHO"), "Latossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("ARGISSOLO AMARELO Distrofico"),
                "Argissolos")
})


test_that(".bdsolos_normalize_ordem maps legacy / pre-1999 names", {
  # Pre-SiBCS names that BDsolos preserves from old surveys
  expect_equal(soilKey:::.bdsolos_normalize_ordem("PODZOLICO"), "Argissolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("LATOSOL"),   "Latossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("GLEI"),      "Gleissolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("ALUVIAL"),   "Neossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("BRUNIZEM"),  "Chernossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("RENDZINA"),  "Chernossolos")
  expect_equal(soilKey:::.bdsolos_normalize_ordem("AREIA"),     "Neossolos")
})


test_that(".bdsolos_normalize_ordem handles diacritics in legacy names", {
  expect_equal(soilKey:::.bdsolos_normalize_ordem("PODZÓLICO"),   "Argissolos")  # ó
  expect_equal(soilKey:::.bdsolos_normalize_ordem("BRUNIZÉM"),    "Chernossolos")# é
})


test_that(".bdsolos_normalize_ordem returns NA on unknown / empty", {
  expect_true(is.na(soilKey:::.bdsolos_normalize_ordem(NA)))
  expect_true(is.na(soilKey:::.bdsolos_normalize_ordem("")))
  expect_true(is.na(soilKey:::.bdsolos_normalize_ordem("   ")))
  expect_true(is.na(soilKey:::.bdsolos_normalize_ordem("XYZ_NONSENSE")))
  expect_true(is.na(soilKey:::.bdsolos_normalize_ordem("SOLO")))   # too generic
})


# ---- benchmark_bdsolos_sibcs ------------------------------------------

.make_bdsolos_pedon_for_benchmark <- function(id = "1001",
                                                 reference_n1 = "ARGISSOLO",
                                                 hue = "5YR", value = 4, chroma = 6,
                                                 clay = c(20, 28, 45, 42)) {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   55,   115),
    bottom_cm = c(20,   55,   115,  170),
    designation          = c("A", "AB", "Bt1", "Bt2"),
    munsell_hue_moist    = c("10YR", "7.5YR", hue, hue),
    munsell_value_moist  = c(4, 4, value, value),
    munsell_chroma_moist = c(3, 4, chroma, chroma),
    structure_grade      = c("moderate","moderate","strong","strong"),
    structure_type       = c("granular","subangular blocky",
                                "subangular blocky","subangular blocky"),
    clay_films_amount    = c(NA, "few", "common", "common"),
    clay_pct             = clay,
    silt_pct             = c(30, 25, 20, 22),
    sand_pct             = 100 - clay - c(30, 25, 20, 22),
    ph_h2o               = c(5.5, 5.3, 5.0, 5.0),
    oc_pct               = c(1.5, 0.6, 0.3, 0.2),
    cec_cmol             = c(8, 6, 5.5, 4.5),
    bs_pct               = c(35, 25, 20, 18),
    al_cmol              = c(0.5, 0.8, 1.2, 1.5)
  )
  PedonRecord$new(
    site = list(
      id                = id,
      lat               = -22.86, lon = -43.78, country = "BR",
      reference_sibcs   = sprintf("%s VERMELHO Distrofico tipico", reference_n1),
      reference_nivel_1 = reference_n1,
      reference_nivel_2 = sprintf("%s VERMELHO", reference_n1),
      reference_nivel_3 = sprintf("%s VERMELHO Distrofico", reference_n1),
      reference_source  = "synthetic"
    ),
    horizons = ensure_horizon_schema(hz)
  )
}


test_that("benchmark_bdsolos_sibcs returns the documented schema", {
  pedons <- list(
    .make_bdsolos_pedon_for_benchmark("A", "ARGISSOLO"),
    .make_bdsolos_pedon_for_benchmark("L", "LATOSSOLO")
  )
  bench <- benchmark_bdsolos_sibcs(pedons, verbose = FALSE)
  # v0.9.61: top-level adds accuracy_subordem; predictions adds the
  # canonical-code subordem agreement triplet.
  expect_named(bench, c("predictions", "confusion", "accuracy",
                          "accuracy_subordem", "per_ordem",
                          "summary", "errors"))
  expect_s3_class(bench$predictions, "data.frame")
  expect_setequal(names(bench$predictions),
                    c("point_id", "predicted_ordem", "reference_ordem",
                      "agree_ordem", "predicted_subordem",
                      "reference_subordem",
                      "predicted_subordem_code",
                      "reference_subordem_code",
                      "agree_subordem",
                      "predicted_gg",
                      "reference_gg", "reference_raw"))
})


test_that("benchmark_bdsolos_sibcs computes Ordem accuracy", {
  pedons <- list(
    .make_bdsolos_pedon_for_benchmark("A", "ARGISSOLO")
  )
  bench <- benchmark_bdsolos_sibcs(pedons, verbose = FALSE)
  # The synthetic profile has B textural + Munsell -> Argissolos
  expect_equal(bench$predictions$reference_ordem[1L], "Argissolos")
  expect_equal(bench$summary$n_total, 1L)
  expect_true(is.numeric(bench$accuracy))
  expect_true(bench$accuracy >= 0 && bench$accuracy <= 1)
})


test_that("benchmark_bdsolos_sibcs builds a confusion matrix", {
  pedons <- list(
    .make_bdsolos_pedon_for_benchmark("a1", "ARGISSOLO"),
    .make_bdsolos_pedon_for_benchmark("a2", "ARGISSOLO"),
    .make_bdsolos_pedon_for_benchmark("l1", "LATOSSOLO"),
    .make_bdsolos_pedon_for_benchmark("l2", "LATOSSOLO")
  )
  bench <- benchmark_bdsolos_sibcs(pedons, verbose = FALSE)
  expect_true(!is.null(bench$confusion) || bench$summary$n_in_scope == 0L)
  if (!is.null(bench$confusion)) {
    expect_true(sum(bench$confusion) <= length(pedons))
  }
  expect_true(is.data.frame(bench$per_ordem) || is.null(bench$per_ordem))
})


test_that("benchmark_bdsolos_sibcs handles errors gracefully", {
  bad <- PedonRecord$new(site = list(id = "bad"),
                         horizons = ensure_horizon_schema(
                           data.table::data.table(top_cm = 0, bottom_cm = 10)))
  pedons <- list(.make_bdsolos_pedon_for_benchmark("ok"), bad)
  bench <- benchmark_bdsolos_sibcs(pedons, verbose = FALSE)
  expect_equal(bench$summary$n_total, 2L)
  # Errors counter sums everything that didn't produce a prediction
  expect_true(is.numeric(bench$summary$n_errors))
})


test_that("benchmark_bdsolos_sibcs reports n_unmapped for unrecognised reference Ordens", {
  unmapped_pedon <- .make_bdsolos_pedon_for_benchmark("u1", "XYZ_UNKNOWN")
  unmapped_pedon$site$reference_nivel_1 <- "XYZ_UNKNOWN"
  pedons <- list(unmapped_pedon)
  bench <- benchmark_bdsolos_sibcs(pedons, verbose = FALSE)
  expect_equal(bench$summary$n_unmapped, 1L)
})


test_that("benchmark_bdsolos_sibcs respects max_n", {
  pedons <- replicate(10, .make_bdsolos_pedon_for_benchmark("x", "ARGISSOLO"),
                       simplify = FALSE)
  bench <- benchmark_bdsolos_sibcs(pedons, max_n = 3L, verbose = FALSE)
  expect_equal(bench$summary$n_total, 3L)
})


test_that("benchmark_bdsolos_sibcs errors clearly on bad input", {
  expect_error(benchmark_bdsolos_sibcs(list()), "non-empty")
  expect_error(benchmark_bdsolos_sibcs(list("not_a_pedon")), "PedonRecord")
})


# ---- Loader integration: nivel_1/2/3 captured ------------------------

test_that("load_bdsolos_csv captures Classe de Solos Nivel 1/2/3", {
  tf <- tempfile(fileext = ".csv")
  hdr <- paste(c("Codigo PA", "Simbolo Horizonte",
                   "Profundidade Superior", "Profundidade Inferior",
                   "Cor da Amostra Umida - Matiz",
                   "Cor da Amostra Umida - Valor",
                   "Cor da Amostra Umida - Croma",
                   "Classe de Solos Nivel 1",
                   "Classe de Solos Nivel 2",
                   "Classe de Solos Nivel 3",
                   "Classificacao Atual"), collapse = ";")
  rows <- c(
    paste(c("100", "A1", "0", "20", "10YR", "4", "3",
              "ARGISSOLO", "ARGISSOLO VERMELHO",
              "ARGISSOLO VERMELHO Distrofico",
              "ARGISSOLO VERMELHO Distrofico tipico A moderado"),
            collapse = ";"),
    paste(c("100", "Bt1", "20", "60", "5YR", "4", "6",
              "ARGISSOLO", "ARGISSOLO VERMELHO",
              "ARGISSOLO VERMELHO Distrofico",
              "ARGISSOLO VERMELHO Distrofico tipico A moderado"),
            collapse = ";")
  )
  writeLines(c("preamble", "", hdr, rows), tf)
  on.exit(unlink(tf), add = TRUE)
  pedons <- load_bdsolos_csv(tf, verbose = FALSE)
  expect_length(pedons, 1L)
  p <- pedons[[1L]]
  expect_equal(p$site$reference_nivel_1, "ARGISSOLO")
  expect_equal(p$site$reference_nivel_2, "ARGISSOLO VERMELHO")
  expect_equal(p$site$reference_nivel_3, "ARGISSOLO VERMELHO Distrofico")
})
