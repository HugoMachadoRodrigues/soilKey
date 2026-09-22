# v0.9.143 -- BDsolos coordinates: the CSV records the hemisphere as a full
# Portuguese word ("Sul" / "Oeste"), not the single letter (S/W/O) the prior
# .bdsolos_dms_to_decimal matched -- so the S/W sign was never applied and every
# Brazilian coordinate was mirrored into the N/E hemisphere. The deterministic
# key ignores coordinates (so classification is unchanged), but SoilGrids /
# spatial priors queried the wrong location.

test_that("v0.9.143: Sul / Oeste hemispheres yield negative decimal degrees", {
  skip_on_cran()
  expect_equal(round(soilKey:::.bdsolos_dms_to_decimal(21, 31, 9.98, "Sul"), 3), -21.519)
  expect_equal(round(soilKey:::.bdsolos_dms_to_decimal(41, 46, 45, "Oeste"), 3), -41.779)
})

test_that("v0.9.143: Norte / Leste hemispheres stay positive", {
  skip_on_cran()
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(5, 0, 0, "Norte"), 5)
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(60, 0, 0, "Leste"), 60)
})

test_that("v0.9.143: the single-letter forms still work (S / W / O)", {
  skip_on_cran()
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(10, 0, 0, "S"), -10)
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(10, 0, 0, "W"), -10)
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(10, 0, 0, "O"), -10)
})

test_that("v0.9.143: a missing / empty hemisphere leaves the magnitude unsigned", {
  skip_on_cran()
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(10, 0, 0, NA), 10)
  expect_equal(soilKey:::.bdsolos_dms_to_decimal(10, 0, 0, ""), 10)
})
