# v0.9.320: in the Ferrosol test, a B2 horizon whose vertic properties are
# not recorded counts as without them (Hugo, 2026-10-31); only recorded
# vertic properties keep a soil out.

test_that("unrecorded vertic properties do not keep a Ferrosol out", {
  skip_on_cran()
  p <- make_asc3_example("ferrosols")
  p$horizons$slickensides <- NA_character_
  r <- soilKey:::ferrosol_asc(p)
  expect_true(r$passed)
  expect_identical(r$evidence$vertic_b2_cm, 0)
  expect_identical(r$evidence$vertic_unrecorded_b2_cm, 135)
  expect_identical(r$missing, character(0))
  k <- classify_asc3(p, on_missing = "silent")
  expect_identical(k$rsg_or_order, "Ferrosol")
  expect_false("orders:FE" %in% k$provisional_because)
})

test_that("recorded vertic properties still keep it out, and free iron still decides", {
  skip_on_cran()
  # slickensides recorded in 85 cm of the B2, the rest unrecorded
  p <- make_asc3_example("ferrosols")
  p$horizons$slickensides <- c(NA, "common", "common", NA)
  r <- soilKey:::ferrosol_asc(p)
  expect_false(r$passed)
  expect_identical(r$evidence$vertic_b2_cm, 85)
  expect_identical(r$evidence$vertic_unrecorded_b2_cm, 50)
  # under 0.3 m recorded: not kept out
  p$horizons$slickensides <- c(NA, NA, NA, "few")
  p$horizons$bottom_cm[4] <- 120
  expect_true(soilKey:::ferrosol_asc(p)$passed)
  # free iron not measured: undetermined, and the reason is the iron
  p <- make_asc3_example("ferrosols")
  p$horizons$slickensides <- NA_character_
  p$horizons$fe_dcb_pct <- NA_real_
  r <- soilKey:::ferrosol_asc(p)
  expect_true(is.na(r$passed))
  expect_true("fe_dcb_pct" %in% r$missing)
  expect_false("slickensides" %in% r$missing)
  # 5 % or less in the major part of the B2: not a Ferrosol
  p <- make_asc3_example("ferrosols")
  p$horizons$slickensides <- NA_character_
  p$horizons$fe_dcb_pct <- c(7, 4, 4, 9)
  expect_false(soilKey:::ferrosol_asc(p)$passed)
})
