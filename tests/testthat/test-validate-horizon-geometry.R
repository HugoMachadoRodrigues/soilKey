# Tests for v0.9.116 validate_horizon_geometry(): the pure depth-geometry
# checker used by the Pro app's Pedon builder.

test_that("a well-formed profile is valid with no issues", {
  h <- data.frame(top_cm = c(0, 20, 55), bottom_cm = c(20, 55, 90),
                  designation = c("A", "AB", "Bt"))
  r <- validate_horizon_geometry(h)
  expect_true(r$valid)
  expect_length(r$errors, 0L)
  expect_length(r$warnings, 0L)
  expect_named(r, c("valid", "errors", "warnings", "details"))
})

test_that("inverted / zero-thickness depths are errors", {
  expect_false(validate_horizon_geometry(
    data.frame(top_cm = c(0, 40), bottom_cm = c(20, 30)))$valid)        # inverted
  zt <- validate_horizon_geometry(
    data.frame(top_cm = c(0, 20), bottom_cm = c(20, 20)))               # zero thickness
  expect_false(zt$valid)
  expect_equal(zt$details$inverted, 2L)
})

test_that("negative and missing depths are errors", {
  expect_false(validate_horizon_geometry(
    data.frame(top_cm = c(-5, 20), bottom_cm = c(20, 40)))$valid)
  na <- validate_horizon_geometry(
    data.frame(top_cm = c(0, NA), bottom_cm = c(20, 40)))
  expect_false(na$valid)
  expect_equal(na$details$missing_depth, 2L)
})

test_that("overlapping horizons are an error", {
  r <- validate_horizon_geometry(
    data.frame(top_cm = c(0, 15), bottom_cm = c(20, 40)))
  expect_false(r$valid)
  expect_true(any(grepl("overlap", r$errors)))
})

test_that("gaps, surface offset, ordering and duplicates are warnings (still valid)", {
  gap <- validate_horizon_geometry(
    data.frame(top_cm = c(0, 30), bottom_cm = c(20, 50)))
  expect_true(gap$valid); expect_true(any(grepl("Gap", gap$warnings)))

  surf <- validate_horizon_geometry(
    data.frame(top_cm = c(10, 30), bottom_cm = c(30, 50)))
  expect_true(surf$valid); expect_equal(surf$details$surface_gap, 10)

  unord <- validate_horizon_geometry(
    data.frame(top_cm = c(20, 0), bottom_cm = c(40, 20)))
  expect_true(unord$valid); expect_true(isTRUE(unord$details$non_monotonic))

  dup <- validate_horizon_geometry(
    data.frame(top_cm = c(0, 20), bottom_cm = c(20, 40),
               designation = c("Bt", "Bt")))
  expect_true(dup$valid); expect_equal(dup$details$duplicate_designation, "Bt")
})

test_that("structural problems are handled gracefully", {
  expect_false(validate_horizon_geometry(NULL)$valid)
  expect_false(validate_horizon_geometry(data.frame())$valid)
  expect_false(validate_horizon_geometry(
    data.frame(x = 1, y = 2))$valid)   # missing top_cm/bottom_cm
})
