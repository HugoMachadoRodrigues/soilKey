# Tests for v0.9.111 "an argic horizon is never a Regosol": the Luvisol
# graceful-default fallback in luvisol(). A confirmed argic horizon with
# high-activity clay (CEC >= 24) but no Al-saturation measurement defaults to
# Luvisol instead of dropping to the Regosol catch-all -- guarded so a measured
# Alisol/Luvisol is never overridden and the argic must sit on a B master
# horizon (not a stratified Fluvisol C layer).

# A 3-horizon argic profile: clean clay increase into a high-activity Bt.
# al_sat / base cations control whether the eutric/alic split is determinable.
.b2_argic_pedon <- function(al_sat = NA_real_, ca = NA_real_, mg = NA_real_,
                            k = NA_real_, na = NA_real_, al_cmol = NA_real_,
                            bt_designation = c("Bt1", "Bt2")) {
  h <- data.frame(
    designation = c("A", bt_designation[1], bt_designation[2]),
    top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
    clay_pct = c(15, 38, 40), silt_pct = c(20, 17, 15),
    sand_pct = c(65, 45, 45), cec_cmol = c(8, 16, 16),
    ph_h2o = c(5.5, 5.6, 5.7),
    clay_films_amount = c(NA, "common", "common"),
    al_sat_pct = c(NA, al_sat, al_sat),
    ca_cmol = c(NA, ca, ca), mg_cmol = c(NA, mg, mg),
    k_cmol = c(NA, k, k), na_cmol = c(NA, na, na),
    al_cmol = c(NA, al_cmol, al_cmol),
    stringsAsFactors = FALSE)
  soilKey::PedonRecord$new(site = list(id = "b2"), horizons = h)
}

test_that("a high-activity argic with no Al-sat defaults to Luvisol, not Regosol", {
  p  <- .b2_argic_pedon()                     # al_sat + all bases NA
  lv <- soilKey:::luvisol(p)
  expect_true(isTRUE(soilKey:::argic(p)$passed))
  expect_true(isTRUE(lv$passed))              # promoted
  expect_gt(length(lv$layers), 0L)            # non-empty layers (load-bearing)
  expect_true(any(grepl("al_sat", lv$missing)))   # al_sat still flagged
  expect_true(!is.null(lv$evidence$al_sat_low$details$al_sat_low_default))
  res <- classify_wrb2022(p, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Luvisols")  # was "Regosols" pre-v0.9.111
  expect_true(any(grepl("al_sat", res$missing_data)))  # assumption surfaced
})

test_that("a measured Alisol (al_sat >= 50) is not overridden by the default", {
  p <- .b2_argic_pedon(al_sat = 60, ca = 1, mg = 1, k = 0.2, na = 0.1,
                       al_cmol = 6)
  expect_true(isTRUE(soilKey:::alisol(p)$passed))
  # Luvisol must be FALSE (measured high Al), NOT NA and NOT promoted-TRUE
  expect_false(isTRUE(soilKey:::luvisol(p)$passed))
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order,
               "Alisols")
})

test_that("a measured Luvisol (al_sat < 50) passes the canonical path, not the default", {
  p  <- .b2_argic_pedon(al_sat = 20, ca = 4, mg = 2, k = 0.3, na = 0.1,
                        al_cmol = 1)
  lv <- soilKey:::luvisol(p)
  expect_true(isTRUE(lv$passed))
  # canonical pass -> the default note must NOT be present
  expect_null(lv$evidence$al_sat_low$details$al_sat_low_default %||% NULL)
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order,
               "Luvisols")
})

test_that("Alisol abstains (NA) when Al-sat is unmeasured, ceding to the promoted Luvisol", {
  # Guards the key-ordering reasoning: Alisol (tested before Luvisol) must
  # return NA (skip), not FALSE, so the engine continues to the Luvisol gate.
  p <- .b2_argic_pedon()
  expect_true(is.na(soilKey:::alisol(p)$passed))
})

test_that("the default does NOT fire on a stratified clay increase in a C layer", {
  # Mirrors the make_fluvisol_canonical pattern: argic's clay-increase test
  # fires on a sedimentary jump between C layers; that is a Fluvisol, not a
  # default Luvisol. The B-horizon guard keeps it out of the Luvisol gate.
  p <- .b2_argic_pedon(bt_designation = c("C1", "C2"))   # argic layer is a C
  expect_false(isTRUE(soilKey:::luvisol(p)$passed))      # NA or FALSE, not TRUE
})

test_that("canonical fixtures with measured chemistry are byte-identical", {
  # The fallback fires only on is.na(al_sat); every argic-derived fixture
  # carries measured or computable al_sat/BS, so none flips.
  expect_equal(classify_wrb2022(make_luvisol_canonical())$rsg_or_order, "Luvisols")
  expect_equal(classify_wrb2022(make_alisol_canonical())$rsg_or_order,  "Alisols")
  expect_equal(classify_wrb2022(make_acrisol_canonical())$rsg_or_order, "Acrisols")
  expect_equal(classify_wrb2022(make_lixisol_canonical())$rsg_or_order, "Lixisols")
  expect_equal(classify_wrb2022(make_fluvisol_canonical())$rsg_or_order, "Fluvisols")
  # the SiBCS argic fixtures' WRB landings (previously unasserted) are pinned
  expect_equal(classify_wrb2022(make_argissolo_canonical())$rsg_or_order, "Acrisols")
  expect_equal(classify_wrb2022(make_luvissolo_canonical())$rsg_or_order, "Luvisols")
})
