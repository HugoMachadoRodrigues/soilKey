test_that("PedonRecord constructs with empty inputs", {
  pr <- PedonRecord$new()
  expect_s3_class(pr, "PedonRecord")
  expect_true(data.table::is.data.table(pr$horizons))
  expect_equal(nrow(pr$horizons), 0L)
  expect_true(data.table::is.data.table(pr$provenance))
  expect_equal(nrow(pr$provenance), 0L)
})

test_that("PedonRecord enforces the canonical horizon schema", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = c(0, 30), bottom_cm = c(30, 100))
  )
  spec_cols <- names(soilKey:::horizon_column_spec())
  expect_true(all(spec_cols %in% names(pr$horizons)))
  expect_true(is.numeric(pr$horizons$clay_pct))
  expect_true(is.character(pr$horizons$designation))
})

test_that("validate() catches inverted depths", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = c(30, 0), bottom_cm = c(0, 30))
  )
  res <- pr$validate(verbose = FALSE)
  expect_false(res$valid)
  expect_true(any(grepl("top_cm >= bottom_cm", res$errors)))
})

test_that("validate() catches bad texture sums", {
  pr <- PedonRecord$new(
    horizons = data.frame(
      top_cm = 0, bottom_cm = 30,
      clay_pct = 30, silt_pct = 30, sand_pct = 30   # sums to 90
    )
  )
  res <- pr$validate(verbose = FALSE)
  expect_false(res$valid)
  expect_true(any(grepl("clay\\+silt\\+sand", res$errors)))
})

test_that("validate() catches implausible pH", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30, ph_h2o = 15)
  )
  res <- pr$validate(verbose = FALSE)
  expect_false(res$valid)
  expect_true(any(grepl("ph_h2o", res$errors)))
})

test_that("validate() warns when sum of bases > CEC", {
  pr <- PedonRecord$new(
    horizons = data.frame(
      top_cm = 0, bottom_cm = 30,
      cec_cmol = 5, ca_cmol = 3, mg_cmol = 2, k_cmol = 1, na_cmol = 0.5
    )
  )
  res <- pr$validate(verbose = FALSE)
  expect_true(any(grepl("Sum of bases > CEC", res$warnings)))
})

test_that("validate() catches implausible Munsell value", {
  pr <- PedonRecord$new(
    horizons = data.frame(
      top_cm = 0, bottom_cm = 30, munsell_value_moist = 11
    )
  )
  res <- pr$validate(verbose = FALSE)
  expect_false(res$valid)
})

test_that("validate() throws under strict = TRUE", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 30, bottom_cm = 0)
  )
  expect_error(pr$validate(strict = TRUE, verbose = FALSE),
                "validation failed")
})

test_that("add_measurement updates horizon and provenance", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  pr$add_measurement(1, "clay_pct", 35, "measured")
  expect_equal(pr$horizons$clay_pct[1], 35)
  expect_equal(nrow(pr$provenance), 1L)
  expect_equal(pr$provenance$source, "measured")
  expect_equal(pr$provenance$attribute, "clay_pct")
})

test_that("add_measurement respects authority order", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  pr$add_measurement(1, "clay_pct", 35, "measured")
  pr$add_measurement(1, "clay_pct", 50, "extracted_vlm")
  # Lower-authority extracted_vlm must NOT overwrite measured.
  expect_equal(pr$horizons$clay_pct[1], 35)
})

test_that("add_measurement allows overwrite when forced", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  pr$add_measurement(1, "clay_pct", 35, "measured")
  pr$add_measurement(1, "clay_pct", 50, "extracted_vlm",
                       overwrite = TRUE)
  expect_equal(pr$horizons$clay_pct[1], 50)
})

test_that("add_measurement rejects invalid source", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  expect_error(pr$add_measurement(1, "clay_pct", 35, "wishful_thinking"),
                "source must be one of")
})

test_that("add_measurement rejects unknown attribute", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  expect_error(pr$add_measurement(1, "soil_color_chakra", 35, "measured"),
                "not in horizon schema")
})

test_that("add_measurement rejects out-of-range horizon index", {
  pr <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  expect_error(pr$add_measurement(99, "clay_pct", 35, "measured"),
                "out of range")
})

test_that("Ferralsol fixture validates", {
  pr <- make_ferralsol_canonical()
  res <- pr$validate(verbose = FALSE)
  expect_true(res$valid, info = paste(res$errors, collapse = "; "))
})

test_that("Luvisol fixture validates", {
  pr <- make_luvisol_canonical()
  res <- pr$validate(verbose = FALSE)
  expect_true(res$valid, info = paste(res$errors, collapse = "; "))
})

test_that("Chernozem fixture validates", {
  pr <- make_chernozem_canonical()
  res <- pr$validate(verbose = FALSE)
  expect_true(res$valid, info = paste(res$errors, collapse = "; "))
})

test_that("summary() reports horizon count and depth range", {
  pr <- make_ferralsol_canonical()
  s <- pr$summary()
  expect_equal(s$n_horizons, 5L)
  expect_equal(s$depth_range[1], 0)
  expect_equal(s$depth_range[2], 200)
})

test_that("make_empty_horizons returns the canonical schema", {
  e <- make_empty_horizons(3)
  expect_equal(nrow(e), 3L)
  spec <- soilKey:::horizon_column_spec()
  expect_equal(sort(names(e)), sort(names(spec)))
})
