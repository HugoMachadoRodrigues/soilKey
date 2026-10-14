# v0.9.232: the BDsolos loader reads the slickensides, the gravel in per cent
# and the Munsell hues with a dot.
#
# Three things the export records and the profiles did not carry as the
# diagnostics read them:
#   - "Superficie de friccao - Quantidade" was not loaded, so the vertic
#     horizon had no slickensides to test in any BDsolos profile: of the 72
#     profiles of the national export whose reference order is Vertissolos,
#     the key agreed with none;
#   - "Fracoes da amostra total - Cascalho (g/kg)" went into
#     `coarse_fragments_pct` as it came, so that 560 g/kg of gravel read as
#     560 %: 2,677 horizons of 1,285 profiles had more than "100 %";
#   - the hues are typed with a decimal comma ("7,5YR") in 7,492 horizons of
#     3,430 profiles, and the patterns of the diagnostics are written with a
#     dot.
# The amount of slickensides is kept as the sheet words it ("Comum",
# "Abundante"), and the tests that read it now read both languages.

.fg_csv <- function() {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior",
           "Composição Granulométrica da terra fina - Argila (g/kg)",
           "Cor da Amostra Úmida - Matiz", "Cor da Amostra Úmida - Valor",
           "Cor da Amostra Úmida - Croma",
           "Frações da amostra total - Cascalho (g/kg)",
           "Superfície de fricção - Quantidade",
           "Superfície de fricção - Grau de desenvolvimento")
  rows <- c(
    q("901", "BA", "VERTISSOLO HÁPLICO Órtico típico", "A",   "0",  "20", "450", "10YR",   "3", "2", "0",   "",              ""),
    q("901", "BA", "VERTISSOLO HÁPLICO Órtico típico", "Bv",  "20", "70", "520", "2,5Y",   "4", "2", "20",  "Comum",         "Moderada"),
    q("901", "BA", "VERTISSOLO HÁPLICO Órtico típico", "Cv",  "70", "120", "500", "7,5 YR", "4", "3", "560", "Abundante",     "Forte"),
    q("902", "BA", "LATOSSOLO VERMELHO Distrófico típico", "A",  "0",  "30", "400", "2,5YR", "3", "4", "0", "", ""),
    q("902", "BA", "LATOSSOLO VERMELHO Distrófico típico", "Bw", "30", "150", "450", "10R",  "3", "6", "5", "Pouca a comum", "Fraca"))
  f <- tempfile(fileext = ".csv")
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  f
}

test_that("a hue typed with a decimal comma is the hue with a dot", {
  expect_equal(.bdsolos_hue(c("7,5YR", "2,5Y", "7,5 YR", "10YR", "7.5YR", "N", "2,5", "10", NA)),
               c("7.5YR", "2.5Y", "7.5YR", "10YR", "7.5YR", "N", "2.5", "10", NA))
})

test_that("an amount is read in either language, a range by its lower end", {
  strong <- c("common", "many", "continuous")
  x <- c("Comum", "Abundante", "comum", "ABUNDANTE", "Comum a abundante",
         "common", "many", "continuous")
  expect_true(all(.amount_at_least(x, strong)))
  low <- c("Pouca", "Pouca a comum", "few", "Ausente", "absent", "", NA)
  expect_false(any(.amount_at_least(low, strong)))
  # the lowest term listed sets the threshold
  expect_true(.amount_at_least("Pouca", "few"))
  expect_false(.amount_at_least("Comum", c("many")))
})

test_that("the loader brings the slickensides, the gravel in per cent and the hues with a dot", {
  f <- .fg_csv(); on.exit(unlink(f), add = TRUE)
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))
  expect_length(peds, 2L)
  id <- vapply(peds, function(p) as.character(p$site$id), "")
  v <- peds[[which(id == "901")]]$horizons
  l <- peds[[which(id == "902")]]$horizons
  # as the sheet words them
  expect_equal(v$slickensides, c(NA, "Comum", "Abundante"))
  expect_equal(l$slickensides, c(NA, "Pouca a comum"))
  # g/kg -> %
  expect_equal(v$coarse_fragments_pct, c(0, 2, 56))
  expect_equal(l$coarse_fragments_pct, c(0, 0.5))
  expect_true(all(c(v$coarse_fragments_pct, l$coarse_fragments_pct) <= 100))
  # the clay was already converted; the two fractions are now in the same unit
  expect_equal(v$clay_pct, c(45, 52, 50))
  expect_equal(v$munsell_hue_moist, c("10YR", "2.5Y", "7.5YR"))
  expect_equal(l$munsell_hue_moist, c("2.5YR", "10R"))
})

test_that("slickensides worded in Portuguese are slickensides for the vertic horizon", {
  skip_on_cran()
  p <- make_vertissolo_canonical()
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  pt <- p$clone(deep = TRUE)
  pt$horizons$slickensides <- c(NA, "Comum", "Abundante")
  pt$horizons$cracks_width_cm <- NA_real_      # the export has no cracks: the slickensides stand for them
  expect_true(isTRUE(horizonte_vertico(pt)$passed))
  expect_equal(classify_sibcs(pt, on_missing = "silent")$rsg_or_order, "Vertissolos")
  # the book asks for "no minimo comum". v0.9.245: "e/ou unidades estruturais
  # cuneiformes", which the canonical profile has; with them the horizon is
  # vertic whatever its slickensides, so they are taken out here.
  for (low in c("Pouca", "Pouca a comum")) {
    q <- pt$clone(deep = TRUE)
    hq <- data.table::copy(q$horizons); hq$slickensides <- c(NA, low, low)
    q$horizons <- hq
    expect_true(isTRUE(horizonte_vertico(q)$passed), info = low)
    hq$structure_type <- c("subangular blocky", "prismatic", "prismatic")
    q$horizons <- hq
    expect_false(isTRUE(horizonte_vertico(q)$passed), info = low)
  }
  # the English terms are read as before
  en <- p$clone(deep = TRUE)
  he <- data.table::copy(en$horizons); he$slickensides <- c("absent", "few", "few")
  he$cracks_width_cm <- NA_real_
  he$structure_type <- c("subangular blocky", "prismatic", "prismatic")
  en$horizons <- he
  expect_false(isTRUE(horizonte_vertico(en)$passed))
})

test_that("a Vertissolo of the export is reached through its slickensides", {
  skip_on_cran()
  f <- .fg_csv(); on.exit(unlink(f), add = TRUE)
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))
  r <- benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)$per_system$sibcs
  expect_equal(r$n_compared, 2L)
  expect_gte(r$confusion["Vertissolos", "Vertissolos"], 1L)
})
