test_that("cambic passes on canonical Cambisol fixture", {
  pr <- make_cambisol_canonical()
  res <- cambic(pr)
  expect_s3_class(res, "DiagnosticResult")
  expect_true(isTRUE(res$passed))
})

test_that("cambic sets aside the layers of the argic horizon on the Luvisol fixture", {
  # v0.9.314: WRB 2022 Ch 3.1.5 criterion 4 is read layer by layer ("does not
  # form part of ... an argic ... horizon"): the Bt1 and Bt2 are set aside, the
  # E above them is not part of the argic horizon.
  pr <- make_luvisol_canonical()
  res <- cambic(pr)
  expect_false(any(c(3L, 4L) %in% res$layers))
  expect_match(res$evidence$not_part_of_other_horizon$notes,
                "argic", ignore.case = TRUE)
})

test_that("cambic excluded by ferralic on Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- cambic(pr)
  expect_false(isTRUE(res$passed))
  expect_match(res$evidence$not_part_of_other_horizon$notes,
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
  # qualify as cambic. v0.9.314: criterion 4 layer by layer, and the
  # thickness of the adjoining layers last.
  expect_named(res$evidence,
                c("subsurface", "texture", "structure_development",
                  "not_part_of_other_horizon", "thickness"))
})
