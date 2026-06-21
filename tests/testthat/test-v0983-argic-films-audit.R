# =============================================================================
# Tests for v0.9.83 -- argic strong-films audit helpers + B_latossolico
# refactor that delegates to argic_with_strong_clay_films().
# Behaviour must be bit-for-bit identical to v0.9.61 / v0.9.82 main.
# =============================================================================


# ---- .argic_strong_films_match (low-level token matcher) -----------------

test_that("v0.9.83: .argic_strong_films_match returns FALSE for empty / NA-only", {
  skip_on_cran()
  expect_false(.argic_strong_films_match(character(0)))
  expect_false(.argic_strong_films_match(NA_character_))
  expect_false(.argic_strong_films_match(c(NA_character_, NA_character_)))
  expect_false(.argic_strong_films_match(c("", " ")))
})


test_that("v0.9.83: .argic_strong_films_match flags strong Portuguese qualifiers", {
  skip_on_cran()
  expect_true(.argic_strong_films_match("comum"))
  expect_true(.argic_strong_films_match("Comum"))
  expect_true(.argic_strong_films_match("COMUM"))
  expect_true(.argic_strong_films_match("abundante"))
  expect_true(.argic_strong_films_match("Abundante"))
  # Mixed-content vector with at least one strong token
  expect_true(.argic_strong_films_match(c("pouca", "comum", "fraca")))
  expect_true(.argic_strong_films_match(c("fraca", "abundante")))
})


test_that("v0.9.83: .argic_strong_films_match flags strong English qualifiers", {
  skip_on_cran()
  expect_true(.argic_strong_films_match("common"))
  expect_true(.argic_strong_films_match("Common"))
  expect_true(.argic_strong_films_match("abundant"))
  expect_true(.argic_strong_films_match(c("few", "common")))
})


test_that("v0.9.83: .argic_strong_films_match REJECTS weak qualifiers", {
  skip_on_cran()
  expect_false(.argic_strong_films_match("pouca"))
  expect_false(.argic_strong_films_match("fraca"))
  expect_false(.argic_strong_films_match("few"))
  expect_false(.argic_strong_films_match("weak"))
  expect_false(.argic_strong_films_match(c("pouca", "fraca", "few", "weak")))
})


test_that("v0.9.83: .argic_strong_films_match strips A-class Portuguese accents before matching", {
  skip_on_cran()
  # The v0.9.61 strong-qualifier tokens (\\babunda, \\bcomu) are
  # A-class only; the helper strips A-acute / A-grave / A-circumflex /
  # A-tilde so surveyor-encoded "Abundânte" still matches "abundan".
  expect_true(.argic_strong_films_match("Abundânte"))   # â -> a
  expect_true(.argic_strong_films_match("ABUNDÃNTE"))   # Ã -> a
  # Non-A accent classes (ú, í, ó) are left unchanged, but the SiBCS
  # strong-qualifier vocabulary doesn't use them in any documented
  # form, so match still works on the canonical "comum" / "comuns" /
  # "comumamente" stems.
  expect_true(.argic_strong_films_match("Comum"))
  expect_true(.argic_strong_films_match("comuns"))
})


# ---- argic_with_strong_clay_films (pedon-level wrapper) ------------------

.fix_with_films <- function(films) {
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   55,   115),
    bottom_cm = c(20,   55,   115,  170),
    designation = c("A", "AB", "Bt1", "Bt2"),
    munsell_hue_moist    = c("10YR","7.5YR","5YR","2.5YR"),
    munsell_value_moist  = c(4, 4, 4, 3),
    munsell_chroma_moist = c(3, 5, 6, 6),
    structure_grade  = c("moderate","moderate","strong","strong"),
    structure_size   = c("medium","medium","medium","medium"),
    structure_type   = c("granular","subangular","subangular","subangular"),
    consistence_moist = c("friable","friable","firm","firm"),
    clay_pct = c(15, 25, 40, 45),
    sand_pct = c(60, 50, 30, 25),
    silt_pct = c(25, 25, 30, 30),
    cec_cmolc_kg = c(8, 6, 5, 4),
    bs_pct  = c(60, 55, 50, 45),
    oc_pct  = c(2.0, 1.0, 0.5, 0.3),
    ph_h2o  = c(5.0, 5.5, 5.8, 6.0),
    bulk_density_g_cm3 = c(1.3, 1.4, 1.5, 1.5),
    al_cmolc_kg = c(0.3, 0.2, 0.1, 0.0),
    coarse_fragments_pct = c(0, 0, 0, 0),
    clay_films_amount  = films
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "fix"), horizons = hz)
}


test_that("v0.9.83: argic_with_strong_clay_films fires on Bt with comum/abundante films", {
  skip_on_cran()
  pr <- .fix_with_films(c(NA_character_, NA_character_, "comum", "abundante"))
  res <- argic_with_strong_clay_films(pr)
  expect_true(isTRUE(res$passed))
  expect_true(length(res$layers) > 0L)
  expect_true("comum" %in% tolower(res$films) ||
                "abundante" %in% tolower(res$films))
})


test_that("v0.9.83: argic_with_strong_clay_films stays FALSE on weak films", {
  skip_on_cran()
  pr <- .fix_with_films(c(NA_character_, NA_character_, "pouca", "fraca"))
  res <- argic_with_strong_clay_films(pr)
  expect_false(isTRUE(res$passed))
  expect_equal(length(res$layers), 0L)
})


test_that("v0.9.83: argic_with_strong_clay_films stays FALSE on missing films", {
  skip_on_cran()
  pr <- .fix_with_films(c(NA_character_, NA_character_, NA_character_, NA_character_))
  res <- argic_with_strong_clay_films(pr)
  expect_false(isTRUE(res$passed))
  expect_equal(length(res$layers), 0L)
})


# ---- audit_argic_strong_films (data.frame audit) -------------------------

test_that("v0.9.83: audit_argic_strong_films returns expected schema", {
  skip_on_cran()
  pr <- .fix_with_films(c(NA_character_, NA_character_, "comum", "abundante"))
  pr$site$reference_sibcs <- "ARGISSOLO VERMELHO Distrofico tipico"
  audit <- audit_argic_strong_films(list(pr))
  expect_identical(colnames(audit),
                    c("id", "reference_sibcs", "argic_passed",
                      "has_films_at_argic", "strong_films_at_argic",
                      "would_exclude_from_latossolo"))
  expect_equal(nrow(audit), 1L)
  expect_true(audit$strong_films_at_argic)
})


test_that("v0.9.83: audit_argic_strong_films reference_filter selects subset", {
  skip_on_cran()
  prL <- .fix_with_films(c(NA_character_, NA_character_, "pouca", "fraca"))
  prL$site$reference_sibcs <- "LATOSSOLO AMARELO Distrofico tipico"
  prA <- .fix_with_films(c(NA_character_, NA_character_, "comum", "abundante"))
  prA$site$reference_sibcs <- "ARGISSOLO AMARELO Distrofico tipico"
  audit_lat <- audit_argic_strong_films(list(prL, prA),
                                          reference_filter = "LATOSSOLO")
  expect_equal(nrow(audit_lat), 1L)
  expect_true(grepl("LATOSSOLO", audit_lat$reference_sibcs[1]))
  expect_false(audit_lat$strong_films_at_argic)
  audit_arg <- audit_argic_strong_films(list(prL, prA),
                                          reference_filter = "ARGISSOLO")
  expect_equal(nrow(audit_arg), 1L)
  expect_true(audit_arg$strong_films_at_argic)
})


test_that("v0.9.83: audit_argic_strong_films errors on empty input", {
  skip_on_cran()
  expect_error(audit_argic_strong_films(list()), "non-empty list")
})


# ---- B_latossolico bit-for-bit preservation ------------------------------
# These tests run on BDsolos RJ if the dataset is available locally. The
# benchmark confusion matrix on n = 722 pedons must match v0.9.82 main
# bit-for-bit -- the v0.9.83 helper extraction never changes behaviour.

test_that("v0.9.83: B_latossolico Latossolo / Argissolo confusion preserved bit-for-bit", {
  skip_on_cran()
  RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
  skip_if_not(file.exists(RJ), "BDsolos RJ.csv not available")
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ, verbose = FALSE)))
  res <- suppressMessages(suppressWarnings(
    benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)))
  conf <- res$per_system$sibcs$confusion
  # v0.9.135: the fluvic-material proxy fix (reversal-based texture
  # stratification -- a monotone A->Bt clay increase is no longer "stratified")
  # stops false-fluvic Argissolos: Argissolo recall lifts 166 -> 175 and
  # Argissolo->Neossolo confusion drops 60 -> 50.
  expect_equal(conf["Latossolos","Latossolos"], 17L)
  expect_equal(conf["Latossolos","Argissolos"], 17L)
  expect_equal(conf["Latossolos","Cambissolos"], 43L)
  expect_equal(conf["Latossolos","Neossolos"], 37L)
  expect_equal(conf["Argissolos","Latossolos"], 5L)
  expect_equal(conf["Argissolos","Argissolos"], 175L)
  expect_equal(conf["Argissolos","Cambissolos"], 1L)
  expect_equal(conf["Argissolos","Neossolos"], 50L)
})


test_that("v0.9.83: BDsolos RJ audit -- minimal Latossolo false-positive exclusion", {
  skip_on_cran()
  RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
  skip_if_not(file.exists(RJ), "BDsolos RJ.csv not available")
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ, verbose = FALSE)))
  audit <- audit_argic_strong_films(peds, reference_filter = "LATOSSOLO|Latossolo")
  # On RJ (n=115 Latossolo references), at most 2 are excluded by the
  # strong-films rule. Tight upper bound -- this is a regression guard
  # so that a future loosening of the strong-qualifier match doesn't
  # silently shed Latossolos.
  expect_equal(nrow(audit), 115L)
  expect_lte(sum(audit$would_exclude_from_latossolo), 2L)
})
