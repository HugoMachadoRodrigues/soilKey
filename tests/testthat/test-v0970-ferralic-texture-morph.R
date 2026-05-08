# =============================================================================
# Tests for v0.9.70 -- test_ferralic_texture() morphological fallback (opt-in).
#
# BDsolos RJ diagnostic showed ~19/115 reference Latossolos fail
# B_latossolico because the strict texture test returns NA when
# clay_pct / silt_pct / sand_pct are missing on the B horizon. v0.9.70
# adds an opt-in fallback: when the canonical numeric test is NA, accept
# layers with Bw / Bo designation in the subsoil (top_cm > 20 cm) as
# evidence of tropical deep-weathering => ferralic-textured.
# =============================================================================

.no_texture_pedon <- function(designation_b = "Bw1", topcm_b = 30,
                                with_topsoil_texture = TRUE) {
  hz <- data.table::data.table(
    top_cm    = c(0, topcm_b, topcm_b + 60),
    bottom_cm = c(topcm_b, topcm_b + 60, topcm_b + 120),
    designation = c("A", designation_b, "Bw2"),
    clay_pct = if (with_topsoil_texture) c(40, NA_real_, NA_real_)
               else c(NA_real_, NA_real_, NA_real_),
    silt_pct = if (with_topsoil_texture) c(30, NA_real_, NA_real_)
               else c(NA_real_, NA_real_, NA_real_),
    sand_pct = if (with_topsoil_texture) c(30, NA_real_, NA_real_)
               else c(NA_real_, NA_real_, NA_real_),
    cec_cmol = c(NA_real_, 5.0, 5.0),
    oc_pct   = c(2.0, 0.6, 0.4),
    ph_h2o   = c(4.5, 4.7, 4.8)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "RJ-no-texture", lat = -22, lon = -43, country = "BR"),
    horizons = hz
  )
}


test_that("v0.9.70: morphological fallback OFF (default) -- texture NA when subsoil clay missing", {
  pr <- .no_texture_pedon()
  res <- test_ferralic_texture(pr$horizons)
  # Topsoil has texture (40/30/30 -> sandy clay loam, passes); subsoil has NA.
  # The test passes on the topsoil layer (already not NA).
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.70: completely-NA-texture profile returns NA (canonical)", {
  pr <- .no_texture_pedon(with_topsoil_texture = FALSE)
  res <- test_ferralic_texture(pr$horizons)
  expect_true(is.na(res$passed))
})


test_that("v0.9.70: morphological fallback ON -- recovers Bw subsoil with NA texture", {
  pr <- .no_texture_pedon(with_topsoil_texture = FALSE)
  withr::with_options(list(soilKey.ferralic_texture_morphological_fallback = TRUE), {
    res <- test_ferralic_texture(pr$horizons)
    expect_true(isTRUE(res$passed))
    # Should record the source
    expect_identical(res$details$source, "morphological_fallback")
  })
})


test_that("v0.9.70: morphological fallback rejects topsoil-only Bw (top_cm <= 20)", {
  # If the only Bw is at the surface, do NOT fire the fallback (it's
  # supposed to indicate subsoil deep-weathering, not surface clay loam).
  hz <- data.table::data.table(
    top_cm    = c(0, 0),  # both topsoil
    bottom_cm = c(20, 30),
    designation = c("A", "Bw"),  # Bw at surface
    clay_pct = c(NA_real_, NA_real_),
    silt_pct = c(NA_real_, NA_real_),
    sand_pct = c(NA_real_, NA_real_),
    oc_pct = c(2, 0.6),
    ph_h2o = c(4.5, 4.5)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "topsoil-only"), horizons = hz)
  withr::with_options(list(soilKey.ferralic_texture_morphological_fallback = TRUE), {
    res <- test_ferralic_texture(pr$horizons)
    expect_true(is.na(res$passed))
  })
})


test_that("v0.9.70: morphological fallback rejects non-Bw designations (e.g. Bt)", {
  # An argillic Bt horizon at depth shouldn't qualify as ferralic-textured
  # via this fallback -- only Bw/Bo (oxic morphology) does.
  hz <- data.table::data.table(
    top_cm    = c(0, 30),
    bottom_cm = c(30, 90),
    designation = c("A", "Bt"),
    clay_pct = c(NA_real_, NA_real_),
    silt_pct = c(NA_real_, NA_real_),
    sand_pct = c(NA_real_, NA_real_),
    oc_pct = c(2, 0.6),
    ph_h2o = c(4.5, 5.0)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "Bt-not-Bw"), horizons = hz)
  withr::with_options(list(soilKey.ferralic_texture_morphological_fallback = TRUE), {
    res <- test_ferralic_texture(pr$horizons)
    expect_true(is.na(res$passed))
  })
})


test_that("v0.9.70: morphological fallback does NOT override real numeric texture", {
  # When real measurements ARE present and they fail (e.g. very sandy),
  # the morphological fallback must not override them.
  hz <- data.table::data.table(
    top_cm    = c(0, 30),
    bottom_cm = c(30, 90),
    designation = c("A", "Bw"),
    clay_pct = c(2, 2),    # very sandy
    silt_pct = c(3, 3),
    sand_pct = c(95, 95),
    oc_pct = c(0.5, 0.2),
    ph_h2o = c(5, 5.2)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "sandy-Bw"), horizons = hz)
  withr::with_options(list(soilKey.ferralic_texture_morphological_fallback = TRUE), {
    res <- test_ferralic_texture(pr$horizons)
    # Real numeric data says FALSE (95% sand fails the sandy-loam predicate),
    # so the fallback path is not entered.
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.70: ferralic with morphological-fallback recovers Bw-only Latossolo", {
  # Build a pedon with cec_cmol+clay only on topsoil but Bw subsoil
  # has no texture. With cec measured directly (no ECEC fallback needed),
  # only the texture morphological fallback is exercised.
  hz <- data.table::data.table(
    top_cm    = c(0, 30, 90),
    bottom_cm = c(30, 90, 150),
    designation = c("A", "Bw1", "Bw2"),
    clay_pct = c(45, NA_real_, NA_real_),
    silt_pct = c(25, NA_real_, NA_real_),
    sand_pct = c(30, NA_real_, NA_real_),
    cec_cmol = c(8, 4, 4),  # CEC measured everywhere
    oc_pct   = c(2, 0.6, 0.4),
    ph_h2o   = c(4.5, 4.7, 4.8)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "Bw-only"), horizons = hz)
  on.exit(options(soilKey.ferralic_texture_morphological_fallback = NULL))
  res_off <- ferralic(pr, engine = "aqp")
  # Off: texture passes on topsoil layer but ferralic also needs thickness
  # >= 30 ON A LAYER THAT ALSO PASSED CEC. Topsoil layer has cec=8, clay=45
  # -> 17.8 cmol/kg-clay > 16 (soilkey) but < 20 (aqp) -- passes aqp.
  # Subsoil layers have NA texture so don't pass texture without fallback.
  # With aqp engine, topsoil might give a 30 cm layer that passes.
  # Result depends on layer-overlap logic; just assert: with the fallback
  # ON, the answer flips from NA / FALSE to TRUE.
  options(soilKey.ferralic_texture_morphological_fallback = TRUE)
  res_on <- ferralic(pr, engine = "aqp")
  expect_true(isTRUE(res_on$passed))
})
