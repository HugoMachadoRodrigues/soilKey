# =============================================================================
# Tests for v0.9.72 -- designation-suffix morphological inference paths.
#
# Brazilian field-described profiles (e.g. the Embrapa Redape curated
# dataset) encode key diagnostic features via lowercase modifier
# letters in the horizon designation:
#   - 'g' (Cg, Cgn, Apg)         -- gleyic
#   - 'f' (Btf, 2Btf, Cf)        -- plinthic
#   - 'v' (Bv, Bvk1, Cv, Cvz)    -- vertic
# without recording the corresponding numeric inputs (redoximorphic_pct,
# plinthite_pct, slickensides). v0.9.72 adds opt-in
# `*_designation_inference` paths that accept these signals, gated
# by per-rule options. Default is FALSE (canonical behaviour preserved).
# =============================================================================


.gleyic_pedon <- function(designations = c("Ap", "Cg1", "Cg2"),
                            chromas      = c(2, 1, 1)) {
  hz <- data.table::data.table(
    top_cm    = c(0, 12, 30),
    bottom_cm = c(12, 30, 80),
    designation = designations,
    munsell_hue_moist    = rep("10YR", 3),
    munsell_value_moist  = c(3, 5, 5),
    munsell_chroma_moist = chromas,
    clay_pct = c(40, 50, 50), silt_pct = c(30, 25, 25), sand_pct = c(30, 25, 25),
    cec_cmol = c(15, 12, 12), oc_pct = c(2, 0.5, 0.3), ph_h2o = c(5.4, 5.0, 4.8)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-gleyic", country = "BR"),
                   horizons = hz)
}


.plinthic_pedon <- function(designations = c("Ap", "Bt", "Btf")) {
  hz <- data.table::data.table(
    top_cm    = c(0, 30, 70),
    bottom_cm = c(30, 70, 140),
    designation = designations,
    clay_pct = c(20, 30, 45), silt_pct = c(30, 30, 25), sand_pct = c(50, 40, 30),
    cec_cmol = c(8, 6, 4), oc_pct = c(1.5, 0.6, 0.2), ph_h2o = c(5.5, 5.0, 4.8)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-plinthic", country = "BR"),
                   horizons = hz)
}


.vertic_pedon <- function(designations = c("Ap", "Bv1", "Bv2"),
                             clays       = c(50, 60, 65)) {
  hz <- data.table::data.table(
    top_cm    = c(0, 20, 60),
    bottom_cm = c(20, 60, 120),
    designation = designations,
    clay_pct = clays, silt_pct = c(20, 20, 18), sand_pct = c(30, 20, 17),
    cec_cmol = c(35, 40, 42), oc_pct = c(1.5, 0.5, 0.3),
    ph_h2o = c(7.5, 7.8, 7.9)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-vertic", country = "BR"),
                   horizons = hz)
}


# ---- Gleyic: g-suffix designation inference --------------------------------

test_that("v0.9.72: gleyic_properties default behaviour unchanged (NA when redox missing)", {
  pr <- .gleyic_pedon()
  res <- gleyic_properties(pr)
  expect_true(is.na(res$passed))
})


test_that("v0.9.72: gleyic_properties accepts g-suffix Cg / Cgn / Apg with low chroma when opt-in", {
  pr <- .gleyic_pedon(designations = c("Ap", "Cg1", "Cg2"),
                        chromas      = c(2, 1, 1))
  withr::with_options(list(soilKey.gleyic_designation_inference = TRUE), {
    res <- gleyic_properties(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.72: gleyic accepts Apg (lowercase modifier between uppercase and g)", {
  pr <- .gleyic_pedon(designations = c("Apg", "Cg1", "Cg2"),
                        chromas      = c(2, 1, 1))
  withr::with_options(list(soilKey.gleyic_designation_inference = TRUE), {
    res <- gleyic_properties(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.72: gleyic accepts 11C1g (digit in middle) -- the GeoTab edge case", {
  pr <- .gleyic_pedon(designations = c("AP", "11C1g", "111C2"),
                        chromas      = c(1, 1, 1))
  withr::with_options(list(soilKey.gleyic_designation_inference = TRUE), {
    res <- gleyic_properties(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.72: gleyic g-suffix REJECTS high-chroma layers (chroma > 2)", {
  pr <- .gleyic_pedon(designations = c("Ap", "Cg1", "Cg2"),
                        chromas      = c(4, 5, 4))   # all bright
  withr::with_options(list(soilKey.gleyic_designation_inference = TRUE), {
    res <- gleyic_properties(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.72: gleyic g-suffix path requires top_cm <= max_top_cm", {
  hz <- data.table::data.table(
    top_cm    = c(0, 80),         # subsoil Cg below 50 cm
    bottom_cm = c(80, 200),
    designation = c("Ap", "Cg"),
    munsell_hue_moist    = c("10YR", "10YR"),
    munsell_value_moist  = c(3, 5),
    munsell_chroma_moist = c(2, 1),
    clay_pct = c(30, 40), silt_pct = c(30, 30), sand_pct = c(40, 30),
    cec_cmol = c(15, 10), oc_pct = c(2, 0.3), ph_h2o = c(5.5, 5.0)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "deep-Cg"), horizons = hz)
  withr::with_options(list(soilKey.gleyic_designation_inference = TRUE), {
    res <- gleyic_properties(pr)
    # Only the Cg layer at top_cm=80 has g; max_top_cm=50 -> doesn't pass
    expect_false(isTRUE(res$passed))
  })
})


# ---- Plinthic: f-suffix designation inference ------------------------------

test_that("v0.9.72: plinthic default behaviour unchanged (NA when plinthite_pct missing)", {
  pr <- .plinthic_pedon()
  res <- plinthic(pr)
  expect_true(is.na(res$passed) || isFALSE(res$passed))
})


test_that("v0.9.72: plinthic accepts Btf when opt-in", {
  pr <- .plinthic_pedon(designations = c("Ap", "Bt", "Btf"))
  withr::with_options(list(soilKey.plinthic_designation_inference = TRUE), {
    res <- plinthic(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.72: plinthic accepts 2Btf (digit prefix) when opt-in", {
  pr <- .plinthic_pedon(designations = c("Ap", "Bt", "2Btf"))
  withr::with_options(list(soilKey.plinthic_designation_inference = TRUE), {
    res <- plinthic(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.72: plinthic rejects f-suffix when total layer thickness < min_thickness", {
  hz <- data.table::data.table(
    top_cm = c(0, 80, 85), bottom_cm = c(80, 85, 95),  # Btf only 5 cm thick
    designation = c("Ap", "Bt", "Btf"),
    clay_pct = c(20, 30, 45), silt_pct = c(30, 30, 25), sand_pct = c(50, 40, 30),
    cec_cmol = c(8, 6, 4), oc_pct = c(1.5, 0.6, 0.2), ph_h2o = c(5.5, 5.0, 4.8)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "thin-Btf"), horizons = hz)
  withr::with_options(list(soilKey.plinthic_designation_inference = TRUE), {
    res <- plinthic(pr)
    expect_false(isTRUE(res$passed))
  })
})


# ---- Vertic: v-suffix designation inference --------------------------------

test_that("v0.9.72: vertic_horizon default behaviour unchanged (no slickensides -> NA/FALSE)", {
  pr <- .vertic_pedon()
  res <- vertic_horizon(pr)
  # Without slickensides + cracks data and no COLE, expect NA or FALSE
  expect_true(is.na(res$passed) || isFALSE(res$passed))
})


test_that("v0.9.72: vertic_horizon accepts Bv / Bvk / Cv when opt-in (clay >= min_clay)", {
  pr <- .vertic_pedon(designations = c("Ak", "BvK1", "Bvk2"),
                        clays       = c(35, 40, 45))
  withr::with_options(list(soilKey.vertic_designation_inference = TRUE), {
    res <- vertic_horizon(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.72: vertic v-suffix REJECTS low-clay profiles (< min_clay)", {
  pr <- .vertic_pedon(designations = c("Ap", "Bv1", "Bv2"),
                        clays       = c(15, 20, 25))   # all < 30
  withr::with_options(list(soilKey.vertic_designation_inference = TRUE), {
    res <- vertic_horizon(pr)
    expect_false(isTRUE(res$passed))
  })
})


# ---- Negative / regression tests -------------------------------------------

test_that("v0.9.72: g-suffix inference does NOT mistake albic E (E in designation, no g)", {
  hz <- data.table::data.table(
    top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
    designation = c("A", "E", "Bt"),  # albic E, no gleyic
    munsell_hue_moist = c("10YR","10YR","10YR"),
    munsell_value_moist = c(3, 6, 4),
    munsell_chroma_moist = c(2, 2, 4),  # E is low-chroma but designation lacks g
    clay_pct = c(15, 8, 35), silt_pct = c(20, 10, 25), sand_pct = c(65, 82, 40),
    cec_cmol = c(10, 4, 8), oc_pct = c(1.5, 0.3, 0.6), ph_h2o = c(5, 5, 5.5)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "albic-E"), horizons = hz)
  withr::with_options(list(soilKey.gleyic_designation_inference = TRUE), {
    res <- gleyic_properties(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.72: f-suffix inference doesn't fire on Bt without f", {
  pr <- .plinthic_pedon(designations = c("Ap", "Bt1", "Bt2"))
  withr::with_options(list(soilKey.plinthic_designation_inference = TRUE), {
    res <- plinthic(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.72: v-suffix inference doesn't fire on Bw without v", {
  pr <- .vertic_pedon(designations = c("Ap", "Bw1", "Bw2"),
                        clays       = c(50, 60, 65))
  withr::with_options(list(soilKey.vertic_designation_inference = TRUE), {
    res <- vertic_horizon(pr)
    expect_false(isTRUE(res$passed))
  })
})


# ---- All three options together --------------------------------------------

test_that("v0.9.72: all three opt-in options can be combined safely", {
  # Each option is per-rule; enabling all three together should not
  # cause cross-talk (e.g. a Plintossolo profile must not accidentally
  # also pass gleyic via this).
  pr <- .plinthic_pedon(designations = c("Ap", "Bt", "Btf"))
  withr::with_options(
    list(
      soilKey.gleyic_designation_inference   = TRUE,
      soilKey.plinthic_designation_inference = TRUE,
      soilKey.vertic_designation_inference   = TRUE
    ), {
      g_res <- gleyic_properties(pr)
      p_res <- plinthic(pr)
      v_res <- vertic_horizon(pr)
      expect_false(isTRUE(g_res$passed))   # no g suffix
      expect_true( isTRUE(p_res$passed))   # has Btf
      expect_false(isTRUE(v_res$passed))   # no v suffix
    }
  )
})
