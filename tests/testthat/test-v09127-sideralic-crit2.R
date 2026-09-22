# v0.9.127 -- sideralic properties criterion 2 (cambic criterion 3 evidence)
# WRB 2022 Ch 3.2.13 crit 2 = "evidence of soil formation as defined in
# criterion 3 of the cambic horizon" (Ch 3.1.5).

mk <- function(df) PedonRecord$new(horizons = df)

test_that(".munsell_hue_units places hues on the 2.5-unit red->yellow scale", {
  u <- .munsell_hue_units(c("2.5R", "10R", "2.5YR", "5YR", "7.5YR",
                              "10YR", "2.5Y", "10Y"))
  expect_equal(u, c(0, 7.5, 10, 12.5, 15, 17.5, 20, 27.5))
  # redder = lower, yellower = higher
  expect_lt(.munsell_hue_units("5YR"), .munsell_hue_units("10YR"))
  # neutral / unparseable / NA -> NA
  expect_true(all(is.na(.munsell_hue_units(c("N", "foo", NA)))))
  # whitespace + lowercase tolerated
  expect_equal(.munsell_hue_units(" 7.5yr "), 15)
})

test_that("cambic crit 3.a.iii fires on a >= 4% absolute clay increase", {
  p <- mk(data.frame(top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 80),
                     designation = c("A", "Bw", "C"),
                     clay_pct = c(10, 22, 16)))
  r <- test_cambic_soil_formation(p$horizons, candidate_layers = 2L)
  expect_true(r$passed)
  expect_true(2L %in% r$layers)
})

test_that("cambic crit 3.d fires on Fe-ox/Fe-dith + reddish chroma", {
  p <- mk(data.frame(top_cm = c(0, 15), bottom_cm = c(15, 60),
                     designation = c("A", "Bw"),
                     munsell_hue_moist = c("10YR", "5YR"),
                     munsell_chroma_moist = c(2, 6),
                     fe_dcb_pct = c(NA, 2.0), fe_ox_pct = c(NA, 0.5)))
  r <- test_cambic_soil_formation(p$horizons, candidate_layers = 2L)
  expect_true(r$passed)
})

test_that("cambic crit 3 respects a lithic discontinuity (2-prefix designation)", {
  # Bw clay 30 over a 2C clay 16 -- discontinuity blocks the 3.a clay compare
  p <- mk(data.frame(top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 80),
                     designation = c("A", "Bw", "2C"),
                     clay_pct = c(28, 30, 16)))
  r <- test_cambic_soil_formation(p$horizons, candidate_layers = 2L)
  # no 3.a/3.c vs 2C; 3.b vs A clay-only has no colour -> nothing evaluable
  expect_true(is.na(r$passed))
})

test_that("cambic crit 3 returns NA when no adjacency data is assessable", {
  p <- mk(data.frame(top_cm = c(0, 15), bottom_cm = c(15, 60),
                     designation = c(NA, NA), clay_pct = c(NA, 30)))
  r <- test_cambic_soil_formation(p$horizons, candidate_layers = 2L)
  expect_true(is.na(r$passed))
})

test_that("sideralic requires BOTH low CEC and soil-formation evidence", {
  # Bw: clay 30, CEC 6 -> CEC/clay = 20 < 24 (crit1); clay 30 vs C 20 (crit2)
  p <- mk(data.frame(top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 90),
                     designation = c("A", "Bw", "C"),
                     clay_pct = c(12, 30, 20), cec_cmol = c(8, 6, 5),
                     munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
                     munsell_value_moist = c(3, 4, 5),
                     munsell_chroma_moist = c(2, 5, 3)))
  s <- sideralic_properties(p)
  expect_true(s$passed)
  expect_true(2L %in% s$layers)
})

test_that("sideralic is NA when crit1 holds but crit2 cannot be assessed", {
  p <- mk(data.frame(top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 90),
                     designation = c(NA, NA, NA),
                     clay_pct = c(NA, 30, NA), cec_cmol = c(NA, 6, NA)))
  s <- sideralic_properties(p)
  expect_true(is.na(s$passed))
})

test_that("sideralic is FALSE when crit1 holds but soil formation is absent", {
  # uniform profile: low CEC/clay but NO contrast with neighbours
  p <- mk(data.frame(top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 90),
                     designation = c("A", "Bw", "C"),
                     clay_pct = c(30, 30, 30), cec_cmol = c(6, 6, 6),
                     munsell_hue_moist = c("10YR", "10YR", "10YR"),
                     munsell_value_moist = c(4, 4, 4),
                     munsell_chroma_moist = c(3, 3, 3),
                     caco3_pct = c(0, 0, 0)))
  s <- sideralic_properties(p)
  expect_false(isTRUE(s$passed))
  expect_false(is.na(s$passed))
})
