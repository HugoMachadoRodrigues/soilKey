# =============================================================================
# Tests for v0.9.66 -- leptic_features(engine = "aqp") thin-topsoil rule
# tightening. v0.9.65 over-fired on LUCAS topsoil-only data (29/30 pedons
# collapsed onto Leptosols). v0.9.66 requires positive evidence of rock
# contact: R-designation, coarse_fragments_pct >= 30, or a deeper R/Cr
# horizon. The opt-in option soilKey.leptic_assume_rock_below restores
# the loose behaviour for users with strong external priors.
# =============================================================================

.lucas_topsoil_pedon <- function(designation = "Ap", cfvo = NA_real_,
                                    bottom_cm = 20, with_subsoil = FALSE) {
  hz <- data.table::data.table(
    top_cm               = 0,
    bottom_cm            = bottom_cm,
    designation          = designation,
    coarse_fragments_pct = cfvo,
    oc_pct               = 1.5,
    ph_h2o               = 6.0,
    clay_pct             = 25, silt_pct = 35, sand_pct = 40
  )
  if (isTRUE(with_subsoil)) {
    sub <- data.table::data.table(
      top_cm = bottom_cm, bottom_cm = bottom_cm + 10,
      designation = "B", oc_pct = 0.7, clay_pct = 28, silt_pct = 32, sand_pct = 40
    )
    hz <- data.table::rbindlist(list(hz, sub), fill = TRUE)
  }
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = "test-lucas", lat = 50, lon = 10, country = "FR"),
    horizons = hz
  )
}


test_that("v0.9.66: LUCAS-like topsoil-only pedon does NOT pass leptic_features (engine=aqp)", {
  pr <- .lucas_topsoil_pedon(designation = "Ap", cfvo = NA_real_)
  res <- leptic_features(pr, engine = "aqp")
  # passed should be FALSE -- no R-designation, no high cfvo, no R below
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.66: LUCAS-like pedon with subsoil also does NOT pass leptic", {
  pr <- .lucas_topsoil_pedon(designation = "Ap", cfvo = NA_real_,
                                with_subsoil = TRUE)
  res <- leptic_features(pr, engine = "aqp")
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.66: pedon with R-designation in topsoil DOES pass leptic (engine=aqp)", {
  # An "AR" or "Cr" designation indicates the user knows rock is
  # near the surface; the thin-topsoil path fires.
  pr <- .lucas_topsoil_pedon(designation = "AR", cfvo = NA_real_)
  res <- leptic_features(pr, engine = "aqp")
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.66: pedon with cfvo >= 30 AND bottom <= 25cm DOES pass leptic", {
  pr <- .lucas_topsoil_pedon(designation = "Ap", cfvo = 45)
  res <- leptic_features(pr, engine = "aqp")
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.66: opt-in option restores the loose v0.9.65 behaviour", {
  pr <- .lucas_topsoil_pedon(designation = "Ap", cfvo = NA_real_)
  withr::with_options(list(soilKey.leptic_assume_rock_below = TRUE), {
    res <- leptic_features(pr, engine = "aqp")
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.66: traditional R/Cr-designation path still works (engine=aqp)", {
  hz <- data.table::data.table(
    top_cm    = c(0, 18),
    bottom_cm = c(18, 30),
    designation = c("A", "R"),
    coarse_fragments_pct = c(10, 90),
    oc_pct = c(2.0, 0.1),
    clay_pct = c(20, 10), silt_pct = c(40, 5), sand_pct = c(40, 85)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(
    site = list(id = "test-leptic-real", lat = 45, lon = 5, country = "FR"),
    horizons = hz
  )
  res <- leptic_features(pr, engine = "aqp")
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.66: soilkey engine (default) is unaffected by the v0.9.66 tightening", {
  # The thin-topsoil path is aqp-only. Strict soilkey engine still
  # requires R-designation OR cfvo >= 90.
  pr <- .lucas_topsoil_pedon(designation = "Ap", cfvo = NA_real_)
  res_soilkey <- leptic_features(pr, engine = "soilkey")
  expect_false(isTRUE(res_soilkey$passed))

  # And on a real Leptosol fixture, soilkey engine still fires
  hz <- data.table::data.table(
    top_cm    = c(0, 15),
    bottom_cm = c(15, 25),
    designation = c("A", "R"),
    coarse_fragments_pct = c(10, 95),
    oc_pct = c(2.0, 0.1),
    clay_pct = c(20, 10), silt_pct = c(40, 5), sand_pct = c(40, 85)
  )
  hz <- ensure_horizon_schema(hz)
  pr2 <- PedonRecord$new(
    site = list(id = "real-lepto", lat = 45, lon = 5, country = "FR"),
    horizons = hz
  )
  res2 <- leptic_features(pr2, engine = "soilkey")
  expect_true(isTRUE(res2$passed))
})


test_that("v0.9.66: evidence$thin_topsoil records which rule fired", {
  pr <- .lucas_topsoil_pedon(designation = "AR", cfvo = NA_real_)
  res <- leptic_features(pr, engine = "aqp")
  # Inspect the evidence
  ev <- res$evidence$thin_topsoil$shallow_topsoil_with_rock$details
  expect_true(isTRUE(ev$rock_R_designation))
  expect_false(isTRUE(ev$rock_high_cfvo))
  expect_false(isTRUE(ev$assume_rock_option))
})
