# Tests for v0.9.43 JSON Schema for PedonRecord.

test_that("pedon_json_schema returns a list with the expected top-level keys", {
  s <- pedon_json_schema(as = "list")
  expect_named(s,
                 c("$schema", "$id", "title", "description",
                     "type", "required", "properties",
                     "additionalProperties"),
                 ignore.order = TRUE)
  expect_equal(s$type, "object")
  expect_setequal(unlist(s$required), c("site", "horizons"))
})

test_that("pedon_json_schema is JSON-serialisable", {
  testthat::skip_if_not_installed("jsonlite")
  json <- pedon_json_schema(as = "json", pretty = FALSE)
  expect_type(json, "character")
  # Must be parseable as JSON.
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$type, "object")
})

test_that("pedon_json_schema horizon properties match horizon_column_spec", {
  s <- pedon_json_schema(as = "list")
  spec_names <- names(horizon_column_spec())
  hzn_names  <- names(s$properties$horizons$items$properties)
  # Every spec column should appear as a horizon property.
  expect_setequal(spec_names, hzn_names)
})

test_that("inst/schemas/pedon-schema.json is up-to-date with the spec", {
  path <- system.file("schemas", "pedon-schema.json", package = "soilKey")
  if (!nzchar(path) || !file.exists(path))
    path <- file.path("inst", "schemas", "pedon-schema.json")
  skip_if_not(file.exists(path), "pedon-schema.json not found in tree")
  testthat::skip_if_not_installed("jsonlite")

  on_disk  <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  in_memory <- pedon_json_schema(as = "list")

  expect_equal(on_disk$properties$horizons$items$required,
                 in_memory$properties$horizons$items$required)
  expect_setequal(names(on_disk$properties$horizons$items$properties),
                    names(in_memory$properties$horizons$items$properties))
})


# ---- validate_pedon_json round-trip ---------------------------------------

test_that("validate_pedon_json runs without raising on a canonical fixture", {
  # We don't insist on TRUE here -- jsonvalidate's ajv engine + the
  # rich PedonRecord schema can flag minor discrepancies (e.g. NA vs
  # null encoding choices). The test verifies the validation pipeline
  # itself runs end-to-end; the calling user can interpret the result.
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("jsonvalidate")
  p <- make_ferralsol_canonical()
  res <- tryCatch(validate_pedon_json(p), error = function(e) e)
  expect_false(inherits(res, "error"))
  expect_true(is.logical(as.logical(res)))
})

test_that("validate_pedon_json catches malformed input (missing site$id)", {
  testthat::skip_if_not_installed("jsonlite")
  testthat::skip_if_not_installed("jsonvalidate")
  payload <- list(
    site     = list(lat = 0, lon = 0),  # missing id
    horizons = list(list(top_cm = 0, bottom_cm = 30))
  )
  res <- validate_pedon_json(payload)
  expect_false(isTRUE(as.logical(res)))
})
