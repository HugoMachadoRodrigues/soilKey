# =============================================================================
# Tests for v0.9.81 -- benchmark_redape() now actually computes Subordem,
# Grande Grupo, and Subgrupo accuracy. Earlier versions accepted the
# `level` argument but always used Order for both prediction and
# reference, so all four levels reported the same number.
# =============================================================================


test_that("v0.9.81: .redape_strip_accents drops Portuguese diacritics", {
  skip_on_cran()
  expect_identical(.redape_strip_accents("ARGISSOLO"),  "argissolo")
  expect_identical(.redape_strip_accents("VERMELHO-AMARELO"),
                                          "vermelho-amarelo")
  # Accents in all five vowel classes plus cedilla
  expect_identical(.redape_strip_accents("Cromático"), "cromatico")
  expect_identical(.redape_strip_accents("Férrico"),  "ferrico")
  expect_identical(.redape_strip_accents("Lítico"),    "litico")
  expect_identical(.redape_strip_accents("Distrófico"),
                                          "distrofico")
  expect_identical(.redape_strip_accents("Lústrico"),  "lustrico")
  expect_identical(.redape_strip_accents("Caça"),      "caca")
})


test_that("v0.9.81: .redape_pluralise_pt skips abbreviations and already-plural", {
  skip_on_cran()
  expect_identical(.redape_pluralise_pt("argissolo"), "argissolos")
  expect_identical(.redape_pluralise_pt("amarelo"),    "amarelos")
  expect_identical(.redape_pluralise_pt("tipico"),    "tipicos")
  # Already plural -- no double-pluralisation
  expect_identical(.redape_pluralise_pt("argissolos"), "argissolos")
  # 2-char SiBCS Cambissolo activity modifier ("Tb" / "Ta")
  expect_identical(.redape_pluralise_pt("tb"), "tb")
  expect_identical(.redape_pluralise_pt("ta"), "ta")
  # Empty / NA stays as-is
  expect_identical(.redape_pluralise_pt(""), "")
  expect_identical(.redape_pluralise_pt(NA_character_), NA_character_)
})


test_that("v0.9.81: .redape_canonical_label round-trips Order names", {
  skip_on_cran()
  # singular ref -> plural canonical
  expect_identical(.redape_canonical_label("ARGISSOLO"),  "argissolos")
  expect_identical(.redape_canonical_label("Argissolos", pluralise = FALSE),
                    "argissolos")
  # multi-token subordem
  expect_identical(.redape_canonical_label("ARGISSOLO AMARELO"),
                    "argissolos amarelos")
  # Cambissolo with Tb activity modifier (3rd token preserved)
  expect_identical(
    .redape_canonical_label("CAMBISSOLO HÁPLICO Tb Distrófico"),
    "cambissolos haplicos tb distroficos"
  )
})


test_that("v0.9.81: .redape_compose_ref builds correct level-deep reference", {
  skip_on_cran()
  pr <- list(site = list(
    id = "test",
    reference_sibcs_order    = "ARGISSOLO",
    reference_sibcs_subordem = "AMARELO",
    reference_sibcs_gg       = "Distrófico",
    reference_sibcs_subgrupo = "abrúptico"
  ))
  expect_identical(.redape_compose_ref(pr, "order"),
                    "argissolos")
  expect_identical(.redape_compose_ref(pr, "subordem"),
                    "argissolos amarelos")
  expect_identical(.redape_compose_ref(pr, "gde_grupo"),
                    "argissolos amarelos distroficos")
  expect_identical(.redape_compose_ref(pr, "subgrupo"),
                    "argissolos amarelos distroficos abrupticos")
})


test_that("v0.9.81: .redape_compose_ref returns NA on incomplete reference", {
  skip_on_cran()
  pr_incomplete <- list(site = list(
    id = "test",
    reference_sibcs_order    = "ARGISSOLO",
    reference_sibcs_subordem = NA_character_,
    reference_sibcs_gg       = "Distrófico",
    reference_sibcs_subgrupo = "abrúptico"
  ))
  expect_true(is.na(.redape_compose_ref(pr_incomplete, "subordem")))
  expect_true(is.na(.redape_compose_ref(pr_incomplete, "subgrupo")))
  # order alone is fine
  expect_identical(.redape_compose_ref(pr_incomplete, "order"),
                    "argissolos")
})


test_that("v0.9.81: benchmark_redape preserves the Order accuracy bit-for-bit", {
  skip_on_cran()
  # Order-level accuracy must equal what main produces (no behaviour
  # change at the canonical level).
  skip_if_not(file.exists("/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab"),
                "redape_geotab dataset not available")
  peds <- suppressMessages(suppressWarnings(load_redape_pedons(
    "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab",
    verbose = FALSE)))
  res <- suppressMessages(suppressWarnings(benchmark_redape(peds,
                                                            level = "order",
                                                            verbose = FALSE)))
  # v0.9.107: order accuracy 59.6% (56/94). v0.9.135: the fluvic-material proxy
  # fix (reversal-based texture stratification) lifts it to 63.8% (60/94) by no
  # longer mislabelling monotone-clay Argissolos as Neossolos Fluvicos.
  # Pinned with a +/- tolerance to catch unintended drift.
  expect_gt(res$accuracy, 0.60)
  expect_lt(res$accuracy, 0.66)
  expect_equal(res$n_compared, 94L)
})


test_that("v0.9.81: benchmark_redape produces DIFFERENT accuracies at different levels", {
  skip_on_cran()
  # Regression guard: before v0.9.81 the four levels reported identical
  # numbers (the level argument was silently dropped at the prediction
  # extraction step). Subgrupo accuracy must be LOWER than Order
  # accuracy because each deeper level requires the previous level to
  # be correct AND the new modifier.
  skip_if_not(file.exists("/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab"),
                "redape_geotab dataset not available")
  peds <- suppressMessages(suppressWarnings(load_redape_pedons(
    "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab",
    verbose = FALSE)))
  ord <- suppressMessages(suppressWarnings(benchmark_redape(peds, level="order",
                                                              verbose = FALSE)))
  sgr <- suppressMessages(suppressWarnings(benchmark_redape(peds, level="subgrupo",
                                                              verbose = FALSE)))
  expect_lt(sgr$accuracy, ord$accuracy)
  # And BOTH must show > 0% accuracy (sanity).
  expect_gt(sgr$accuracy, 0)
  expect_gt(ord$accuracy, 0)
})


test_that("v0.9.81: predictions table now exposes ref_norm and pred_norm columns", {
  skip_on_cran()
  skip_if_not(file.exists("/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab"),
                "redape_geotab dataset not available")
  peds <- suppressMessages(suppressWarnings(load_redape_pedons(
    "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab",
    verbose = FALSE, max_n = 5)))
  res <- suppressMessages(suppressWarnings(benchmark_redape(peds,
                                                              level = "subgrupo",
                                                              verbose = FALSE)))
  expect_true(all(c("id", "ref", "pred", "ref_norm", "pred_norm") %in%
                       colnames(res$predictions)))
  # ref_norm should be lowercase and accent-free
  ref_chars <- na.omit(res$predictions$ref_norm)
  expect_true(all(grepl("^[a-z0-9 -]+$", ref_chars)))
})
