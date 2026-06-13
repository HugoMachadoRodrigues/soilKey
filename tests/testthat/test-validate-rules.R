# v0.9.118: every predicate referenced in the YAML rule base must exist as a
# function. Catches a typo'd predicate name in a rule (which would otherwise
# degrade to a silent NA at classification time).

test_that("every rule-base predicate exists, for all three systems", {
  for (sys in c("wrb2022", "usda", "sibcs5")) {
    res <- soilKey:::.validate_rules(sys)
    expect_gt(res$n, 0L)
    expect_equal(res$missing, character(0),
                 info = sprintf("%s rules reference missing predicate(s): %s",
                                sys, paste(res$missing, collapse = ", ")))
  }
})

test_that(".collect_rule_predicates pulls names from all_of / any_of at depth", {
  rules <- list(
    a = list(tests = list(all_of = list(list(pred_one = list()),
                                        list(pred_two = list(x = 1))))),
    b = list(nested = list(tests = list(any_of = list(list(pred_three = list())))))
  )
  preds <- sort(unique(soilKey:::.collect_rule_predicates(rules)))
  expect_equal(preds, c("pred_one", "pred_three", "pred_two"))
})
