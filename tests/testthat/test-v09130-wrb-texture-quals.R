# v0.9.130 -- WRB 2022 texture-qualifier audit (Fix D slice 2).
# Clayic was a confirmed threshold bug: WRB 2022 Ch 5 defines it as the
# texture classes clay / sandy clay / silty clay (~ clay >= 40%), not the
# v0.9 proxy of clay >= 60%.

mk <- function(df) {
  PedonRecord$new(horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("Clayic recognises the clay / sandy clay / silty clay texture classes", {
  # clay class (clay 45) over >= 30 cm -> Clayic (was FALSE under clay >= 60)
  clay <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                        clay_pct = c(45, 48), sand_pct = c(20, 18),
                        silt_pct = c(35, 34)))
  expect_true(qual_clayic(clay)$passed)

  # sandy clay (clay 38, sand 50) -> Clayic
  sandy <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                         clay_pct = c(38, 38), sand_pct = c(50, 50),
                         silt_pct = c(12, 12)))
  expect_true(qual_clayic(sandy)$passed)

  # silty clay (clay 42, silt 45) -> Clayic
  silty <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                         clay_pct = c(42, 42), sand_pct = c(13, 13),
                         silt_pct = c(45, 45)))
  expect_true(qual_clayic(silty)$passed)
})

test_that("Clayic does NOT fire on clay loam (clay < 35-40, not a clay class)", {
  cl <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                      clay_pct = c(32, 32), sand_pct = c(30, 30),
                      silt_pct = c(38, 38)))
  expect_false(isTRUE(qual_clayic(cl)$passed))
})

test_that("Clayic still needs a >= 30 cm combined thickness within 100 cm", {
  thin <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 40),
                        clay_pct = c(20, 50), sand_pct = c(40, 20),
                        silt_pct = c(40, 30)))  # only 20 cm of clay
  expect_false(isTRUE(qual_clayic(thin)$passed))
})
