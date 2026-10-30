# Tests for v0.9.111 "an argic horizon is never a Regosol". v0.9.111 made a
# high-activity argic horizon with no Al-saturation data a Luvisol by default,
# on a B master horizon only. Since v0.9.310 the Luvisols are what the WRB
# 2022 key says (Ch 4, p. 122): "Other soils having an argic horizon starting
# <= 100 cm from the mineral soil surface", so an argic horizon within 100 cm
# reaches the Luvisols at the latest, whatever the Al data; the Alisol test
# reads the pH (WRB 2022 Annex 2, Table 9.3) when the cations are not given.

# A 3-horizon argic profile: clean clay increase into a high-activity Bt.
# al_sat / base cations control whether the Alisol test is decided by them.
.b2_argic_pedon <- function(al_sat = NA_real_, ca = NA_real_, mg = NA_real_,
                            k = NA_real_, na = NA_real_, al_cmol = NA_real_,
                            bt_designation = c("Bt1", "Bt2"),
                            ph = c(5.5, 5.6, 5.7), films = "common") {
  h <- data.frame(
    designation = c("A", bt_designation[1], bt_designation[2]),
    top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
    clay_pct = c(15, 38, 40), silt_pct = c(20, 17, 15),
    sand_pct = c(65, 45, 45), cec_cmol = c(8, 16, 16),
    ph_h2o = ph,
    clay_films_amount = c(NA, films, films),
    al_sat_pct = c(NA, al_sat, al_sat),
    ca_cmol = c(NA, ca, ca), mg_cmol = c(NA, mg, mg),
    k_cmol = c(NA, k, k), na_cmol = c(NA, na, na),
    al_cmol = c(NA, al_cmol, al_cmol),
    stringsAsFactors = FALSE)
  soilKey::PedonRecord$new(site = list(id = "b2"), horizons = h)
}

test_that("a high-activity argic with no Al data is a Luvisol, not a Regosol", {
  skip_on_cran()
  p  <- .b2_argic_pedon()                     # al_sat + all bases NA
  expect_true(isTRUE(soilKey:::argic(p)$passed))
  expect_true(isTRUE(soilKey:::luvisol(p)$passed))
  # pH 5.6-5.7 at 50-100 cm is above the 5.4 of Table 9.3 for any carbon
  # content: the bases exceed the Al, so no Alisol
  expect_false(isTRUE(soilKey:::alisol(p)$passed))
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order, "Luvisols")
})

test_that("a measured Alisol (Al > bases) keys out before the Luvisols", {
  skip_on_cran()
  p <- .b2_argic_pedon(al_sat = 60, ca = 1, mg = 1, k = 0.2, na = 0.1,
                       al_cmol = 6)
  expect_true(isTRUE(soilKey:::alisol(p)$passed))
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order,
               "Alisols")
})

test_that("a measured Luvisol (Al <= bases) is a Luvisol", {
  skip_on_cran()
  p  <- .b2_argic_pedon(al_sat = 20, ca = 4, mg = 2, k = 0.3, na = 0.1,
                        al_cmol = 1)
  expect_false(isTRUE(soilKey:::alisol(p)$passed))
  expect_true(isTRUE(soilKey:::luvisol(p)$passed))
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order,
               "Luvisols")
})

test_that("with no cations the Alisol test reads the pH, or abstains", {
  skip_on_cran()
  # pH 4.2-4.3 is below 4.6, the lowest pH of Table 9.3: Al > bases
  p <- .b2_argic_pedon(ph = c(4.5, 4.2, 4.3))
  expect_true(isTRUE(soilKey:::alisol(p)$passed))
  # pH 5.0 with no organic carbon: between 4.6 and 5.4, undecided
  q <- .b2_argic_pedon(ph = c(5.0, 5.0, 5.0))
  expect_true(is.na(soilKey:::alisol(q)$passed))
  expect_equal(classify_wrb2022(q, on_missing = "silent")$rsg_or_order, "Luvisols")
})

test_that("a clay increase between strata of different deposits is no argic horizon", {
  skip_on_cran()
  # C layers of one material with a clay increase meet the argic horizon
  # (WRB 2022 Ch 3.1.3 does not ask for a B); numbered as deposits of their
  # own (C, 2C) they are separated by a lithic discontinuity (criterion 2.a.i);
  # without clay coatings (2.b.ii) nothing else makes the 2C argic
  p <- .b2_argic_pedon(bt_designation = c("C1", "C2"))
  expect_true(isTRUE(soilKey:::argic(p)$passed))
  q <- .b2_argic_pedon(bt_designation = c("2C1", "2C2"), films = NA_character_)
  expect_false(isTRUE(soilKey:::argic(q)$passed))
})

test_that("canonical fixtures with measured chemistry keep their RSGs", {
  skip_on_cran()
  expect_equal(classify_wrb2022(make_luvisol_canonical())$rsg_or_order, "Luvisols")
  expect_equal(classify_wrb2022(make_alisol_canonical())$rsg_or_order,  "Alisols")
  expect_equal(classify_wrb2022(make_acrisol_canonical())$rsg_or_order, "Acrisols")
  expect_equal(classify_wrb2022(make_lixisol_canonical())$rsg_or_order, "Lixisols")
  expect_equal(classify_wrb2022(make_fluvisol_canonical())$rsg_or_order, "Fluvisols")
  # v0.9.310: the Argissolo's exchangeable Al equals its bases (BS x CEC) in
  # the Bt at 50-100 cm, so it is no Acrisol (Ch 4, p. 119)
  expect_equal(classify_wrb2022(make_argissolo_canonical())$rsg_or_order, "Lixisols")
  expect_equal(classify_wrb2022(make_luvissolo_canonical())$rsg_or_order, "Luvisols")
})
