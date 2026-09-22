test_that("evaluate_rsg_tests handles 'not_implemented_v01' marker", {
  pr <- make_ferralsol_canonical()
  res <- evaluate_rsg_tests(pr, list(not_implemented_v01 = "histic_horizon"))
  expect_true(is.na(res$passed))
  expect_true("diagnostic_histic_horizon" %in% res$missing)
  expect_match(res$notes, "scheduled")
})

test_that("evaluate_rsg_tests handles 'default: true'", {
  pr <- make_ferralsol_canonical()
  res <- evaluate_rsg_tests(pr, list(default = TRUE))
  expect_true(isTRUE(res$passed))
})

test_that("evaluate_rsg_tests handles all_of with one passing test", {
  pr <- make_ferralsol_canonical()
  res <- evaluate_rsg_tests(pr, list(
    all_of = list(list(ferralic = list()))
  ))
  expect_true(isTRUE(res$passed))
})

test_that("evaluate_rsg_tests handles all_of with one failing test", {
  pr <- make_luvisol_canonical()
  res <- evaluate_rsg_tests(pr, list(
    all_of = list(list(ferralic = list()))
  ))
  expect_false(isTRUE(res$passed))
})

test_that("evaluate_rsg_tests handles any_of where one passes", {
  pr <- make_ferralsol_canonical()
  res <- evaluate_rsg_tests(pr, list(
    any_of = list(
      list(argic    = list()),
      list(ferralic = list())
    )
  ))
  expect_true(isTRUE(res$passed))
})

test_that("evaluate_rsg_tests handles any_of where all fail", {
  pr <- make_luvisol_canonical()
  res <- evaluate_rsg_tests(pr, list(
    any_of = list(
      list(ferralic = list()),
      list(mollic   = list())
    )
  ))
  expect_false(isTRUE(res$passed))
})

test_that("evaluate_rsg_tests propagates NA from unrecognised diagnostic", {
  pr <- make_ferralsol_canonical()
  res <- evaluate_rsg_tests(pr, list(
    all_of = list(list(unknown_diagnostic_xyz = list()))
  ))
  expect_true(is.na(res$passed))
})

test_that("evaluate_rsg_tests handles malformed test block", {
  pr <- make_ferralsol_canonical()
  res <- evaluate_rsg_tests(pr, list())   # no combinator at all
  expect_true(is.na(res$passed))
  expect_match(res$notes, "Malformed")
})

test_that("test parameters from YAML pass through to diagnostic functions", {
  pr <- make_ferralsol_canonical()

  # Tighten ferralic threshold via the rule engine
  res_default <- evaluate_rsg_tests(pr, list(
    all_of = list(list(ferralic = list()))
  ))
  expect_true(isTRUE(res_default$passed))

  res_strict <- evaluate_rsg_tests(pr, list(
    all_of = list(list(ferralic = list(max_cec = 5)))
  ))
  expect_false(isTRUE(res_strict$passed))
})
