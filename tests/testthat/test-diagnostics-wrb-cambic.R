test_that("cambic passes on canonical Cambisol fixture", {
  pr <- make_cambisol_canonical()
  res <- cambic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
})

test_that("cambic excluded by argic on Luvisol fixture", {
  pr <- make_luvisol_canonical()
  res <- cambic(pr)
  expect_false(isTRUE(res$passed))
  # Confirm exclusion is recorded as 'argic passed' note
  expect_match(res$evidence$not_argic$notes,
                "argic", ignore.case = TRUE)
})

test_that("cambic excluded by ferralic on Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- cambic(pr)
  expect_false(isTRUE(res$passed))
  expect_match(res$evidence$not_ferralic$notes,
                "ferralic", ignore.case = TRUE)
})

test_that("cambic respects custom thickness", {
  pr <- make_cambisol_canonical()
  expect_false(isTRUE(cambic(pr, min_thickness = 200)$passed))
})

test_that("cambic evidence carries the named sub-tests", {
  pr <- make_cambisol_canonical()
  res <- cambic(pr)
  # v0.9.2.C added the "subsurface" depth-gate AND a
  # "structure_development" gate so massive C horizons no longer
  # qualify as cambic.
  expect_named(res$evidence,
                c("subsurface", "thickness", "texture",
                  "structure_development",
                  "not_argic", "not_ferralic"))
})
