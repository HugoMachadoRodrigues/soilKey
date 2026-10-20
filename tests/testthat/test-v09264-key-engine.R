# v0.9.264: the rule engine reads nested all_of / any_of / none_of blocks, so a
# key can say "A and (B or C) and not D" in YAML; the lower levels of a key are
# read from the folders its registry entry names; the validator reports blocks
# that mix combinators and, for systems added after the registry, predicates
# without the system's suffix.

# Building blocks on the Ferralsol fixture: entisol_usda() always passes,
# gelisol_usda() fails, and an unknown diagnostic is indeterminate (NA).
.T <- list(entisol_usda = list())
.F <- list(gelisol_usda = list())
.N <- list(no_such_diagnostic_v09264 = list())

test_that("none_of passes only when no test passes, three-valued", {
  skip_on_cran()
  p  <- make_ferralsol_canonical()
  ev <- function(...) evaluate_rsg_tests(p, list(none_of = list(...)))$passed
  expect_true(ev(.F))
  expect_false(ev(.T))
  expect_false(ev(.T, .N))
  expect_true(is.na(ev(.F, .N)))
})

test_that("blocks nest inside all_of / any_of / none_of", {
  skip_on_cran()
  p  <- make_ferralsol_canonical()
  ev <- function(tests) evaluate_rsg_tests(p, tests)$passed
  expect_true(ev(list(all_of = list(.T, list(any_of = list(.F, .T))))))
  expect_false(ev(list(all_of = list(.T, list(none_of = list(.T))))))
  expect_true(ev(list(all_of = list(.T, list(none_of = list(.F))))))
  expect_true(is.na(ev(list(any_of = list(.F, list(all_of = list(.T, .N)))))))
  expect_true(is.na(ev(list(all_of = list(.T, list(any_of = list(.F, .N)))))))
  # the nested block is reported as a test of its own, with its sub-results
  res <- evaluate_rsg_tests(p, list(all_of = list(.T,
                                    list(any_of = list(.F, .N)))))
  nested <- res$evidence[[2L]]
  expect_identical(nested$test_name, "any_of")
  expect_length(nested$evidence, 2L)
  expect_true("diagnostic_no_such_diagnostic_v09264" %in% res$missing)
})

test_that("the predicate collector skips nested combinators", {
  rules <- list(x = list(tests = list(all_of = list(
    list(pred_a = list()),
    list(any_of = list(list(pred_b = list()),
                       list(none_of = list(list(pred_c = list())))))))))
  expect_identical(sort(unique(soilKey:::.collect_rule_predicates(rules))),
                   c("pred_a", "pred_b", "pred_c"))
})

test_that("the validator reports blocks that mix combinators", {
  rules <- list(orders = list(
    list(code = "AA", tests = list(all_of = list(list(p = list())),
                                   any_of = list(list(q = list())))),
    list(code = "BB", tests = list(default = TRUE))))
  mixed <- soilKey:::.collect_mixed_blocks(rules)
  expect_length(mixed, 1L)
  expect_match(mixed, "AA")
  expect_match(mixed, "all_of \\+ any_of")
})

test_that("the validator reports combinators written as a mapping", {
  rules <- list(orders = list(
    list(code = "AA", tests = list(all_of = list(
      list(any_of = list(p = list(), q = list())))))))
  bad <- soilKey:::.collect_malformed_blocks(rules)
  expect_length(bad, 1L)
  expect_match(bad, "any_of is not a list")
})

test_that("the validator reports unquoted, missing and repeated codes", {
  sys <- list(top_level = "orders", levels = c("orders", "groups"))
  rules <- list(
    orders = list(list(code = "A", name = "A soils", tests = list()),
                  list(code = FALSE, name = "N soils", tests = list()),
                  list(code = "A", name = "A again", tests = list())),
    groups = c(list(A = list(list(code = "AA", name = "x", tests = list()))),
               list(A = list(list(code = "AB", name = "y", tests = list()))),
               list(`FALSE` = list(list(code = 12, name = "z",
                                        tests = list())))))
  probs <- soilKey:::.collect_code_problems(rules, sys)
  expect_true(any(grepl("orders, entry 2: code and name must be quoted", probs)))
  expect_true(any(grepl("orders: code\\(s\\) listed twice: A", probs)))
  expect_true(any(grepl("groups: parent code\\(s\\) listed twice: A", probs)))
  expect_true(any(grepl("parent key\\(s\\) FALSE look unquoted", probs)))
  expect_true(any(grepl("groups/FALSE, entry 1: code and name", probs)))
})

test_that("the three systems' rules are clean under the new checks", {
  skip_on_cran()
  for (sys in soilkey_systems()$id) {
    res <- soilKey:::.validate_rules(sys)
    expect_identical(res$mixed, character(0), info = sys)
    expect_identical(res$unsuffixed, character(0), info = sys)
    expect_identical(res$malformed, character(0), info = sys)
    expect_identical(res$codes, character(0), info = sys)
  }
})

test_that("a system added after the registry must suffix its predicates", {
  skip_on_cran()
  real <- soilKey:::.system_get
  local_mocked_bindings(
    .system_get = function(id) { e <- real(id); e$legacy <- FALSE; e },
    .package = "soilKey")
  res <- soilKey:::.validate_rules("usda")
  expect_gt(length(res$unsuffixed), 0L)
  expect_false(any(endsWith(res$unsuffixed, "_usda")))
})
