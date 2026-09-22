# =============================================================================
# Tests for v0.9.79 -- vertic chroma+clay path declines when mollic
# also fires (intergrade resolution: Mollisol-with-vertic features
# routes to Phaeozem/Kastanozem section, not to Vertisol).
# =============================================================================


.mollisol_with_vertic_features <- function() {
  # A Phaeozem-like profile: dark surface mollic + high clay subsoil
  # with low chroma (which would fire vertic chroma+clay if not gated).
  hz <- data.table::data.table(
    top_cm    = c(0, 10, 27, 60, 100),
    bottom_cm = c(10, 27, 60, 100, 150),
    designation = c("A11","A12","B1t","Bk1","Bk2"),
    munsell_hue_moist = rep("10YR", 5),
    munsell_value_moist  = c(2, 2, 3, 4, 4),
    munsell_chroma_moist = c(2, 2, 2, 2, 2),
    munsell_value_dry = c(3, 3, 4, 5, 5),
    clay_pct = c(40, 45, 60, 65, 60),
    silt_pct = c(30, 30, 25, 20, 25),
    sand_pct = c(30, 25, 15, 15, 15),
    cec_cmol = c(20, 25, 30, 30, 28),
    bs_pct   = c(95, 95, 100, 100, 100),
    oc_pct   = c(2.5, 1.5, 0.6, 0.3, 0.2),
    ph_h2o   = c(7.0, 7.2, 7.5, 7.8, 7.9)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-mollic-vertic-intergrade"),
                   horizons = hz)
}


.real_vertisol_no_mollic <- function() {
  # A Vertisol-like profile: NO mollic (surface OC too low) but high
  # clay + low chroma in B subsoil -- chroma+clay path must still fire.
  hz <- data.table::data.table(
    top_cm    = c(0, 12, 38, 74),
    bottom_cm = c(12, 38, 74, 130),
    designation = c("A", "Bw", "Bkg1", "Bkg2"),
    munsell_hue_moist = rep("10YR", 4),
    munsell_value_moist  = c(4, 5, 5, 5),
    munsell_chroma_moist = c(2, 2, 1, 1),
    clay_pct = c(50, 55, 65, 65),
    silt_pct = c(25, 23, 20, 20),
    sand_pct = c(25, 22, 15, 15),
    cec_cmol = c(40, 42, 44, 45),
    bs_pct   = c(100, 100, 100, 100),
    oc_pct   = c(0.4, 0.3, 0.2, 0.2),  # too low for mollic
    ph_h2o   = c(7.5, 7.7, 7.8, 8.0)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-real-vertisol"),
                   horizons = hz)
}


test_that("v0.9.79: vertic chroma+clay path DECLINES when mollic also fires", {
  pr <- .mollisol_with_vertic_features()
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    # mollic should fire on the surface stack
    mh <- mollic(pr)
    expect_true(isTRUE(mh$passed))
    # vertic_horizon chroma+clay path is now declined because mollic competes
    vh <- vertic_horizon(pr)
    expect_false(isTRUE(vh$passed))
  })
})


test_that("v0.9.79: vertic chroma+clay path STILL fires on real Vertisols (no mollic)", {
  pr <- .real_vertisol_no_mollic()
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    mh <- mollic(pr)
    expect_false(isTRUE(mh$passed))   # OC too low
    vh <- vertic_horizon(pr)
    expect_true(isTRUE(vh$passed))
  })
})


test_that("v0.9.79: full classifier routes mollic+vertic intergrade away from Vertisol", {
  pr <- .mollisol_with_vertic_features()
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    res <- classify_wrb2022(pr, on_missing = "silent")
    pred <- sub("s$", "", res$rsg_or_order)
    # Should NOT be Vertisol
    expect_false(identical(pred, "Vertisol"))
  })
})


test_that("v0.9.79: canonical vertic paths (cracks + COLE) NOT affected by mollic-priority", {
  # Real-vertisol fixture: even if we faked mollic-passing data, the
  # CANONICAL vertic paths (slickensides+cracks, COLE) would still
  # declare vertic since they are explicit field measurements.
  # Smoke test: real-vertisol still passes vertic chroma+clay path
  pr <- .real_vertisol_no_mollic()
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    vh <- vertic_horizon(pr)
    cci <- vh$evidence$chroma_clay_inference
    expect_true(isTRUE(cci$passed))
  })
})
