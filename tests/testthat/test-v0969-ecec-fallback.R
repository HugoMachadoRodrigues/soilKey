# =============================================================================
# Tests for v0.9.69 -- test_cec_per_clay() ECEC fallback (opt-in).
#
# BDsolos RJ diagnostic showed 66/115 (57.4%) of reference Latossolos
# fail B_latossolico because cec_cmol (Valor T) is NA throughout the
# profile -- but Ca/Mg/K/Na/Al cmol are recorded. v0.9.69 lets the
# test fall back to ECEC = sum(Ca, Mg, K, Na, Al) when opted in via
# options(soilKey.ferralic_ecec_fallback = TRUE).
# =============================================================================

.no_cec_pedon <- function(ca = 0.5, mg = 0.4, k = 0.05, na_v = 0.02, al = 1.5,
                            clay = 60) {
  hz <- data.table::data.table(
    top_cm    = c(0, 30, 100),
    bottom_cm = c(30, 100, 200),
    designation = c("A", "Bw1", "Bw2"),
    coarse_fragments_pct = c(2, 3, 4),
    clay_pct = c(clay, clay, clay - 2),
    silt_pct = c(20, 15, 17),
    sand_pct = c(20 - (clay - 60), 25 - (clay - 60), 25 - (clay - 60)),
    cec_cmol = NA_real_,
    ca_cmol  = c(ca, ca, ca),
    mg_cmol  = c(mg, mg, mg),
    k_cmol   = c(k, k, k),
    na_cmol  = c(na_v, na_v, na_v),
    al_cmol  = c(al, al, al),
    oc_pct   = c(2.0, 0.6, 0.4),
    ph_h2o   = c(4.5, 4.7, 4.8),
    structure_grade = c("moderate", "moderate", "weak"),
    structure_type  = c("granular", "subangular blocky", "subangular blocky"),
    structure_size  = c("small", "small", "medium")
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "RJ-no-cec", lat = -22, lon = -43, country = "BR"),
    horizons = hz
  )
}


test_that("v0.9.69: ECEC fallback OFF (default) -- test_cec_per_clay returns NA when cec_cmol missing", {
  pr <- .no_cec_pedon()
  res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 20)
  expect_true(is.na(res$passed))
  expect_true("cec_cmol" %in% res$missing)
})


test_that("v0.9.69: ECEC fallback ON -- test_cec_per_clay PASSES on a Latossolo-like ECEC profile", {
  # ca=0.5, mg=0.4, k=0.05, na=0.02, al=1.5 -> ECEC = 2.47 cmol_c
  # clay=60% -> ECEC/clay = 4.1 cmol_c/kg-clay -> well below 20
  pr <- .no_cec_pedon()
  withr::with_options(list(soilKey.ferralic_ecec_fallback = TRUE), {
    res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 20)
    expect_true(isTRUE(res$passed))
    # Evidence should mark cec_source = "ecec_fallback"
    sources <- vapply(res$details, function(d) d$cec_source %||% NA_character_, character(1))
    expect_true(all(sources[!is.na(sources)] == "ecec_fallback"))
  })
})


test_that("v0.9.69: ECEC fallback respects the threshold -- high ECEC fails", {
  # ca=10, mg=10, k=1, na=0.5, al=10 -> ECEC = 31.5 cmol_c
  # clay=60% -> ECEC/clay = 52.5 -> > 20
  pr <- .no_cec_pedon(ca = 10, mg = 10, k = 1, na_v = 0.5, al = 10)
  withr::with_options(list(soilKey.ferralic_ecec_fallback = TRUE), {
    res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 20)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.69: ECEC fallback does not override a real cec_cmol value", {
  hz <- data.table::data.table(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    clay_pct = c(60, 60),
    cec_cmol = c(5, 5),  # Valor T present, low
    ca_cmol = c(0.5, 0.5), mg_cmol = c(0.4, 0.4),
    k_cmol = c(0.05, 0.05), na_cmol = c(0.02, 0.02),
    al_cmol = c(1.5, 1.5),
    oc_pct = c(2.0, 0.5), ph_h2o = c(4.5, 4.7),
    silt_pct = c(20, 15), sand_pct = c(20, 25)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "real-cec"), horizons = hz)
  withr::with_options(list(soilKey.ferralic_ecec_fallback = TRUE), {
    res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 20)
    expect_true(isTRUE(res$passed))
    sources <- vapply(res$details, function(d) d$cec_source %||% NA_character_, character(1))
    # Should use the real cec_cmol, not ecec_fallback
    expect_true(all(sources[!is.na(sources)] == "cec_cmol"))
  })
})


test_that("v0.9.69: ferralic with ECEC fallback recovers borderline Latossolo without cec_cmol", {
  pr <- .no_cec_pedon()
  res_no_fallback <- ferralic(pr, engine = "aqp")
  expect_true(is.na(res_no_fallback$passed))

  withr::with_options(list(soilKey.ferralic_ecec_fallback = TRUE), {
    res_with_fallback <- ferralic(pr, engine = "aqp")
    expect_true(isTRUE(res_with_fallback$passed))
  })
})


test_that("v0.9.69: B_latossolico with ECEC fallback also recovers", {
  pr <- .no_cec_pedon()
  res_no_fallback <- B_latossolico(pr, engine = "aqp")
  expect_true(is.na(res_no_fallback$passed) || isFALSE(res_no_fallback$passed))

  withr::with_options(list(soilKey.ferralic_ecec_fallback = TRUE), {
    res_with_fallback <- B_latossolico(pr, engine = "aqp")
    expect_true(isTRUE(res_with_fallback$passed))
  })
})
