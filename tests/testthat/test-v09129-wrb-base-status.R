# v0.9.129 -- WRB 2022 base-status qualifiers redefined from base saturation
# to exchangeable Al vs exchangeable bases (WRB 2022 Ch 5, p131-133), strict
# (no base-saturation fallback).

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
