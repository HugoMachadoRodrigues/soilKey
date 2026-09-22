# v0.9.129 -- WRB 2022 base-status qualifiers redefined from base saturation
# to exchangeable Al vs exchangeable bases, strict (no base-saturation
# fallback).
#
# CANONICAL BASIS (verified against the official WRB 2022 4th-edition PDF,
# IUSS Working Group WRB 2022, section "Definitions of qualifiers"):
#   Dystric (p.130-131): one or more layers within 20-100 cm with exchangeable
#     Al > exchangeable (Ca+Mg+K+Na) in HALF OR MORE of their combined thickness.
#   Eutric  (p.131-132): same window with exchangeable bases >= exchangeable Al
#     in the MAJOR PART.
# Base saturation (>=/< 50%) was the WRB 2014 rule and is NOT the WRB 2022
# criterion; these tests guard against a regression back to it.

mk <- function(df) {
  PedonRecord$new(horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("Dystric = Al > bases in >= half; Eutric = bases >= Al in major part", {
  al <- mk(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                      al_sat_pct = c(70, 75, 80)))
  expect_true(qual_dystric(al)$passed)
  expect_false(isTRUE(qual_eutric(al)$passed))

  ba <- mk(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                      al_cmol = c(0.5, 0.4, 0.3), ca_cmol = c(8, 9, 10),
                      mg_cmol = c(2, 2, 2)))
  expect_true(qual_eutric(ba)$passed)
  expect_false(isTRUE(qual_dystric(ba)$passed))
})

test_that("Hyperdystric needs Al > 4x bases (al_sat > 80) in the major part", {
  # al_sat 70-80: Al-dominated throughout but NOT > 4x bases -> not Hyperdystric
  weak <- mk(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                        al_sat_pct = c(70, 75, 80)))
  expect_true(qual_dystric(weak)$passed)
  expect_false(isTRUE(qual_hyperdystric(weak)$passed))
  # al_sat 85-90 throughout -> Hyperdystric
  strong <- mk(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                          al_sat_pct = c(85, 90, 88)))
  expect_true(qual_hyperdystric(strong)$passed)
})

test_that("Hypereutric needs bases >= 4x Al (al_sat <= 20) in the major part", {
  he <- mk(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                      al_sat_pct = c(15, 10, 12)))
  expect_true(qual_hypereutric(he)$passed)
  # base-dominated but only mildly (al_sat 40) -> Eutric, not Hypereutric
  mild <- mk(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                        al_sat_pct = c(40, 35, 38)))
  expect_true(qual_eutric(mild)$passed)
  expect_false(isTRUE(qual_hypereutric(mild)$passed))
})

test_that("strict: base saturation alone (no Al data) yields NA, not a fallback", {
  bs_only <- mk(data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
                           bs_pct = c(20, 30, 40)))
  expect_true(is.na(qual_dystric(bs_only)$passed))
  expect_true(is.na(qual_eutric(bs_only)$passed))
  expect_true(is.na(qual_hyperdystric(bs_only)$passed))
})

test_that("Epi/Endo variants restrict the Al criterion to the upper/lower part", {
  # Al-dominated upper (20-50), base-dominated lower (50-100)
  p <- mk(data.frame(top_cm = c(0, 20, 55), bottom_cm = c(20, 55, 100),
                     al_sat_pct = c(70, 75, 10)))
  expect_true(qual_epidystric(p)$passed)
  expect_false(isTRUE(qual_endodystric(p)$passed))
  expect_true(qual_endoeutric(p)$passed)
  expect_false(isTRUE(qual_epieutric(p)$passed))
})

test_that("organic layers use the WRB Histosol pH branch", {
  # peat: pH 4.0 -> dystric-side; pH 6.0 -> eutric-side
  acid <- mk(data.frame(top_cm = c(0, 30), bottom_cm = c(30, 80),
                        oc_pct = c(30, 30), ph_h2o = c(4.0, 4.2)))
  expect_true(qual_dystric(acid)$passed)
  base <- mk(data.frame(top_cm = c(0, 30), bottom_cm = c(30, 80),
                        oc_pct = c(30, 30), ph_h2o = c(6.0, 6.2)))
  expect_true(qual_eutric(base)$passed)
})

test_that("the variable-charge Ferralsol is Eutric under WRB 2022 (showcase)", {
  # low base saturation (24%) but bases > exchangeable Al on the effective
  # exchange -> Eutric, NOT Dystric. The point of the 2014->2022 change.
  fr <- make_ferralsol_canonical()
  expect_true(qual_eutric(fr)$passed)
  expect_false(isTRUE(qual_dystric(fr)$passed))
})

test_that("WRB 2022 discriminator: base-dominated but low-base-saturation -> Eutric", {
  # Self-contained anti-regression guard (independent of any fixture).
  # A variable-charge subsoil: exchangeable bases (Ca+Mg+K+Na = 3.1 cmol_c/kg)
  # clearly exceed exchangeable Al (0.3), so WRB 2022 keys EUTRIC. Yet the CEC
  # at pH 7 is high (pH-dependent charge), so base saturation is well below 50%
  # -- the WRB *2014* rule would have called this Dystric. This test fails if
  # anyone reintroduces a base-saturation criterion.  (WRB 2022 p.131-132.)
  vc <- mk(data.frame(
    top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100),
    al_cmol = c(0.3, 0.3, 0.2),
    ca_cmol = c(2.0, 1.8, 1.6), mg_cmol = c(0.8, 0.7, 0.6),
    k_cmol  = c(0.2, 0.2, 0.2), na_cmol = c(0.1, 0.1, 0.1),
    cec_cmol = c(12, 11, 10),   # high pH-7 CEC -> base saturation ~ 26% (< 50)
    bs_pct   = c(26, 25, 24)
  ))
  expect_true(qual_eutric(vc)$passed)          # WRB 2022: bases >> Al -> Eutric
  expect_false(isTRUE(qual_dystric(vc)$passed)) # a bs<50 rule would say Dystric
})
