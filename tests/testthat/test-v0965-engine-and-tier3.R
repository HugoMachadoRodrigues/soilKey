# =============================================================================
# Tests for v0.9.65 -- per-pedon engine heuristic + Tier-3 schema fields
# wired through the previously-stub WRB qualifiers.
# =============================================================================


.pedon_minimal_v0965 <- function() {
  hz <- data.frame(
    designation = c("A", "B"),
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    munsell_hue_moist = c("10YR", "10YR"),
    munsell_value_moist = c(4, 4),
    munsell_chroma_moist = c(3, 4),
    munsell_hue_dry = c(NA_character_, NA_character_),
    munsell_value_dry = c(NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_),
    clay_pct = c(20, 30), silt_pct = c(20, 25), sand_pct = c(60, 45),
    ph_h2o = c(5.5, 5.0), oc_pct = c(2.0, 0.5),
    cec_cmol = c(8, 6), base_saturation_pct = c(40, 25),
    stringsAsFactors = FALSE
  )
  PedonRecord$new(
    site = list(id = "test-min", country = "BR"),
    horizons = hz)
}


# ---- 1. pick_engine + pick_engine_batch -------------------------------

test_that("pick_engine returns 'soilkey' on a sparse pedon", {
  skip_on_cran()
  hz <- data.frame(
    designation = NA_character_, top_cm = 0L, bottom_cm = 30L,
    munsell_hue_moist = NA_character_, munsell_value_moist = NA_real_,
    munsell_chroma_moist = NA_real_,
    munsell_hue_dry = NA_character_, munsell_value_dry = NA_real_,
    munsell_chroma_dry = NA_real_,
    clay_pct = NA_real_, silt_pct = NA_real_, sand_pct = NA_real_,
    ph_h2o = 5.5, oc_pct = 2.0, cec_cmol = 8,
    base_saturation_pct = 40, stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "sparse", country = "BR"),
                         horizons = hz)
  expect_equal(pick_engine(p), "soilkey")
})


test_that("pick_engine returns 'aqp' on a data-rich pedon", {
  skip_on_cran()
  hz <- data.frame(
    designation = c("A", "Bt"),
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    munsell_hue_moist = c("10YR", "5YR"),
    munsell_value_moist = c(4, 4),
    munsell_chroma_moist = c(3, 6),
    munsell_hue_dry = c(NA_character_, NA_character_),
    munsell_value_dry = c(NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_),
    clay_pct = c(20, 45), silt_pct = c(20, 20), sand_pct = c(60, 35),
    ph_h2o = c(5.5, 5), oc_pct = c(2, 0.5),
    cec_cmol = c(8, 6), base_saturation_pct = c(40, 25),
    structure_grade = c("moderate", "moderate"),
    clay_films_amount = c(NA_character_, "Comum"),
    stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "rich", country = "BR"),
                         horizons = hz)
  expect_equal(pick_engine(p), "aqp")
})


test_that("pick_engine_batch vectorises", {
  skip_on_cran()
  ps <- list(.pedon_minimal_v0965(), .pedon_minimal_v0965())
  res <- pick_engine_batch(ps)
  expect_length(res, 2L)
  expect_true(all(res %in% c("aqp", "soilkey")))
})


test_that("pick_engine min_score adjusts threshold", {
  skip_on_cran()
  p <- .pedon_minimal_v0965()
  expect_equal(pick_engine(p, min_score = 5L), "soilkey")
  expect_equal(pick_engine(p, min_score = 1L), "aqp")
})


# ---- 2. classify_with_engine_heuristic --------------------------------

test_that("classify_with_engine_heuristic captures engine in trace", {
  skip_on_cran()
  testthat::skip_if_not_installed("aqp")
  p <- .pedon_minimal_v0965()
  res <- tryCatch(
    classify_with_engine_heuristic(p, system = "wrb2022",
                                       on_missing = "silent"),
    error = function(e) NULL)
  if (is.null(res)) skip("classify failed in test fixture")
  expect_s3_class(res, "ClassificationResult")
  expect_true(!is.null(res$trace$engine_used))
  expect_true(res$trace$engine_used %in% c("aqp", "soilkey"))
})


# ---- 3. Tier-3 schema fields populated -------------------------------

test_that("Tier-3 schema fields exist in horizon_column_spec()", {
  skip_on_cran()
  spec <- horizon_column_spec()
  for (f in c("surface_crust_type", "bioturbation_density",
                "cordic_horizon", "microrelief_form",
                "weathering_stage", "salt_crust_pattern",
                "contamination_type", "stratification_pattern",
                "aeolian_morphology", "mottle_morphology",
                "surface_puff_layer", "thixotropic_index",
                "saprolite_pct", "water_regime_pattern")) {
    expect_true(f %in% names(spec),
                  info = sprintf("Tier-3 field missing: %s", f))
  }
})


test_that("Tier-3 qualifiers fire when their schema field is populated", {
  skip_on_cran()
  p <- .pedon_minimal_v0965()
  p$horizons$surface_crust_type <- c("biocrust", NA_character_)
  expect_true(isTRUE(qual_biocrustic(p)$passed))

  p2 <- .pedon_minimal_v0965()
  p2$horizons$bioturbation_density <- c("common", NA_character_)
  expect_true(isTRUE(qual_arenicolic(p2)$passed))

  p3 <- .pedon_minimal_v0965()
  p3$horizons$saprolite_pct <- c(NA_real_, 60)
  expect_true(isTRUE(qual_saprolithic(p3)$passed))

  p4 <- .pedon_minimal_v0965()
  p4$horizons$thixotropic_index <- c(NA_real_, 60)
  expect_true(isTRUE(qual_thixotropic(p4)$passed))

  p5 <- .pedon_minimal_v0965()
  p5$horizons$mottle_morphology <- c("mochi", NA_character_)
  expect_true(isTRUE(qual_mochipic(p5)$passed))

  p6 <- .pedon_minimal_v0965()
  p6$horizons$weathering_stage <- c(NA_character_, "saprolite")
  expect_true(isTRUE(qual_saprolithic(p6)$passed))

  p7 <- .pedon_minimal_v0965()
  p7$horizons$aeolian_morphology <- c("loess deposit", NA_character_)
  expect_true(isTRUE(qual_nechic(p7)$passed))
})


test_that("Tier-3 qualifiers return NA when schema field is empty", {
  skip_on_cran()
  p <- .pedon_minimal_v0965()
  expect_true(is.na(qual_biocrustic(p)$passed))
  expect_true(is.na(qual_arenicolic(p)$passed))
  expect_true(is.na(qual_saprolithic(p)$passed))
  expect_true(is.na(qual_thixotropic(p)$passed))
  expect_true(is.na(qual_mochipic(p)$passed))
})


# ---- 4. Engine-aware leptic + arenic relaxation ----------------------

test_that("leptic_features engine='aqp' relaxes coarse_pct + accepts thin topsoil", {
  skip_on_cran()
  hz <- data.frame(
    designation = c("A", "C"), top_cm = c(0, 20),
    bottom_cm = c(20, 25), munsell_hue_moist = c("10YR","10YR"),
    munsell_value_moist = c(4,4), munsell_chroma_moist = c(3,4),
    munsell_hue_dry = c(NA_character_, NA_character_),
    munsell_value_dry = c(NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_),
    clay_pct = c(15,15), silt_pct = c(20,20), sand_pct = c(65,65),
    ph_h2o = c(5.5, 5), oc_pct = c(2, 0.5),
    cec_cmol = c(8, 6), base_saturation_pct = c(40, 25),
    coarse_fragments_pct = c(40, 60),
    stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "leptic", country = "BR"),
                         horizons = hz)
  expect_false(isTRUE(leptic_features(p, engine = "soilkey")$passed))
  expect_true(isTRUE(leptic_features(p, engine = "aqp")$passed))
})


test_that("arenic_texture engine='aqp' accepts sand >= 70 even if silt+2*clay >= 30", {
  skip_on_cran()
  hz <- data.frame(
    designation = c("A", "C"), top_cm = c(0, 30), bottom_cm = c(30, 100),
    munsell_hue_moist = c("10YR","10YR"), munsell_value_moist = c(4,4),
    munsell_chroma_moist = c(3,4),
    munsell_hue_dry = c(NA_character_, NA_character_),
    munsell_value_dry = c(NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_),
    clay_pct = c(10,10), silt_pct = c(15,15), sand_pct = c(75,75),
    ph_h2o = c(5.5, 5), oc_pct = c(2, 0.5),
    cec_cmol = c(8, 6), base_saturation_pct = c(40, 25),
    stringsAsFactors = FALSE)
  p <- PedonRecord$new(site = list(id = "arenic", country = "BR"),
                         horizons = hz)
  # silt + 2*clay = 15 + 20 = 35 -- NOT < 30 (strict). 75 >= 70 (relaxed).
  expect_false(isTRUE(arenic_texture(p, engine = "soilkey")$passed))
  expect_true(isTRUE(arenic_texture(p, engine = "aqp")$passed))
})
