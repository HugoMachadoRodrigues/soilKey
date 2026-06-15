# v0.9.135 -- the fluvic-material proxies (test_fluvic_stratification) tightened
# so they detect GENUINE sedimentary stratification, not pedogenic trends.

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))

test_that("a monotone A->Bt clay increase is NOT stratification (fluvic FALSE)", {
  # a normal Argissolo: monotone clay, regular OC -> not fluvic (the old proxy
  # wrongly flagged the A->Bt clay jump as "stratification").
  mono <- mkh(data.frame(top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 120),
                         clay_pct = c(15, 30, 45), oc_pct = c(1, 0.5, 0.3)))
  expect_false(isTRUE(test_fluvic_stratification(mono)$passed))
  # erratic clay alone is necessary but, under the current AND, not sufficient
  # (the OC pattern must also be irregular) -- so this still does not fire.
  strat_only <- mkh(data.frame(top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 120),
                          clay_pct = c(15, 40, 18), oc_pct = c(1, 0.9, 0.8)))
  expect_false(isTRUE(test_fluvic_stratification(strat_only)$passed))
})

test_that("an E->Bs OC increase (podzolization) is NOT counted as irregular-OC", {
  # spodic: low E -> higher Bs OC is pedogenic, not fluvic
  spod <- mkh(data.frame(top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 90),
                         designation = c("E", "Bs", "BC"),
                         clay_pct = c(8, 8, 7), oc_pct = c(0.4, 1.6, 0.3)))
  expect_false(isTRUE(test_fluvic_stratification(spod)$passed))
})

test_that("a genuine erratic OC reversal into a non-spodic layer + texture is fluvic", {
  fl <- mkh(data.frame(top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 120),
                       designation = c("A", "C", "Ab"),
                       clay_pct = c(15, 40, 18),   # reversal
                       oc_pct = c(1.0, 0.4, 1.2))) # erratic into a buried A
  expect_true(isTRUE(test_fluvic_stratification(fl)$passed))
})
