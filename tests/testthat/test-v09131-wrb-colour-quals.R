# v0.9.131 -- WRB 2022 colour-qualifier audit (Fix D slice 3): Chromic, Rhodic,
# Xanthic completed against the verbatim PDF (Ch 5, p130/145/151).

mk <- function(df) {
  PedonRecord$new(horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("Rhodic: redder than 5YR, value < 4, soil formation, >= 30 cm", {
  # 2.5YR, value 3, with an AB->BA chroma increase (cambic crit 3), >= 30 cm
  p <- mk(data.frame(top_cm = c(0, 20, 55), bottom_cm = c(20, 55, 95),
                     munsell_hue_moist = c("2.5YR", "2.5YR", "2.5YR"),
                     munsell_value_moist = c(3, 3, 3), munsell_value_dry = c(4, 4, 4),
                     munsell_chroma_moist = c(3, 4, 6), clay_pct = c(20, 25, 30)))
  expect_true(qual_rhodic(p)$passed)
})

test_that("Rhodic fails when the dry value is > 1 unit above the moist value", {
  p <- mk(data.frame(top_cm = c(0, 20, 55), bottom_cm = c(20, 55, 95),
                     munsell_hue_moist = c("2.5YR", "2.5YR", "2.5YR"),
                     munsell_value_moist = c(3, 3, 3), munsell_value_dry = c(6, 6, 6),
                     munsell_chroma_moist = c(3, 4, 6), clay_pct = c(20, 25, 30)))
  expect_false(isTRUE(qual_rhodic(p)$passed))
})

test_that("Chromic: redder than 7.5YR, chroma > 4, and NOT Rhodic", {
  # 5YR (redder than 7.5YR, not redder than 5YR), value 5 (not Rhodic), chroma 6
  p <- mk(data.frame(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
                     munsell_hue_moist = c("7.5YR", "5YR", "5YR"),
                     munsell_chroma_moist = c(3, 6, 6),
                     munsell_value_moist = c(4, 5, 5), clay_pct = c(20, 30, 30)))
  expect_false(isTRUE(qual_rhodic(p)$passed))
  expect_true(qual_chromic(p)$passed)
})

test_that("Chromic does NOT co-occur with Rhodic (mutual exclusion)", {
  fr <- make_ferralsol_canonical()
  expect_true(qual_rhodic(fr)$passed)
  expect_false(isTRUE(qual_chromic(fr)$passed))
})

test_that("Xanthic: ferralic + hue 7.5YR-or-yellower, value >= 4, chroma >= 5", {
  # build a yellow ferralic-like profile; relies on ferralic() passing
  fr <- make_ferralsol_canonical()
  h  <- fr$horizons
  h$munsell_hue_moist    <- rep("10YR", nrow(h))
  h$munsell_value_moist  <- rep(5, nrow(h))
  h$munsell_chroma_moist <- rep(6, nrow(h))
  yellow <- PedonRecord$new(horizons = h)
  # ferralic must still pass on this clayey low-activity profile
  if (isTRUE(ferralic(yellow)$passed)) {
    expect_true(qual_xanthic(yellow)$passed)
  } else {
    skip("ferralic() did not pass on the recoloured fixture")
  }
  # the (red) canonical Ferralsol is NOT Xanthic
  expect_false(isTRUE(qual_xanthic(make_ferralsol_canonical())$passed))
})
