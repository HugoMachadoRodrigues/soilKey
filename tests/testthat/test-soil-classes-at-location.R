# Tests for soil_classes_at_location() -- the user-facing spatial
# classification aid. We exercise the input-validation, the
# no-source-warning path, and the WRB->SiBCS translation. Real raster
# fetching is skipped via skip_if_no_proj() in the spatial test
# infrastructure.


test_that("soil_classes_at_location() rejects bad coordinates", {
  expect_error(soil_classes_at_location(NA_real_, -43, verbose = FALSE),
                 "must be numeric")
  expect_error(soil_classes_at_location(91, 0, verbose = FALSE),
                 "out of WGS-84 range")
  expect_error(soil_classes_at_location(0, 200, verbose = FALSE),
                 "out of WGS-84 range")
})


test_that("soil_classes_at_location() returns empty result with a warning when no source given", {
  skip_if_not_installed("terra")
  res <- soil_classes_at_location(lat = -22.7, lon = -43.7,
                                    verbose = FALSE)
  expect_type(res, "list")
  expect_named(res, c("distribution", "typical_attributes", "site"),
                 ignore.order = TRUE)
  expect_equal(nrow(res$distribution), 0L)
  expect_equal(res$site$lat, -22.7)
  expect_equal(res$site$lon, -43.7)
})


test_that("WRB -> SiBCS translation collapses RSGs to ordens", {
  # Internal helper accessor (use ::: to reach unexported function).
  trans <- soilKey:::.wrb_to_sibcs_distribution
  dist <- data.table::data.table(
    rsg_code    = c("FR", "AC", "VR", "PZ", "RG"),
    probability = c(0.4, 0.3, 0.1, 0.1, 0.1)
  )
  out <- trans(dist)
  # FR -> L (Latossolos); AC -> P (Argissolos); VR -> V (Vertissolos);
  # PZ -> E (Espodossolos); RG -> R (Neossolos).
  expect_setequal(out$rsg_code, c("L", "P", "V", "E", "R"))
  expect_equal(round(sum(out$probability), 6), 1.0)
})


test_that("typical_attribute_table populates a row per requested code", {
  attrs <- soilKey:::.typical_attribute_table("wrb2022",
                                              c("FR", "AC", "VR"))
  expect_s3_class(attrs, "data.table")
  expect_true(all(c("FR", "AC", "VR") %in% attrs$rsg_code))
})


test_that("typical_attribute_table is empty when no codes are passed", {
  attrs <- soilKey:::.typical_attribute_table("wrb2022", character(0))
  expect_equal(nrow(attrs), 0L)
})
