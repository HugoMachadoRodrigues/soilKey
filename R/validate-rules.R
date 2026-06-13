# =============================================================================
# v0.9.118 -- .validate_rules(): a proactive integrity check that every
# predicate referenced by a system's YAML rule base actually exists as a
# function in the package namespace. The rule engine already degrades
# gracefully at runtime (a missing predicate yields passed = NA with a note;
# see evaluate_test_spec in rule-engine.R), but a typo'd predicate name in a
# rule would then fail *silently*. This validator catches such malformed rules
# at test time across all three systems.
# =============================================================================

# Recursively collect every predicate name referenced in a rules object: the
# name of each spec inside an all_of / any_of / none_of test list, at any depth.
.collect_rule_predicates <- function(x, acc = character(0)) {
  if (!is.list(x)) return(acc)
  for (combinator in c("all_of", "any_of", "none_of")) {
    block <- x[[combinator]]
    if (!is.null(block) && is.list(block)) {
      for (spec in block) {
        nm <- names(spec)[1L]
        if (!is.null(nm) && nzchar(nm)) acc <- c(acc, nm)
      }
    }
  }
  for (el in x) acc <- .collect_rule_predicates(el, acc)
  acc
}

# Check that every predicate referenced by a system's rules exists as a
# function in the soilKey namespace. Returns a list(system, n, missing).
.validate_rules <- function(system = c("wrb2022", "usda", "sibcs5")) {
  system <- match.arg(system)
  rules  <- load_rules(system)
  preds  <- unique(.collect_rule_predicates(rules))
  preds  <- sort(preds[nzchar(preds)])
  ok <- vapply(preds, function(p)
    exists(p, envir = asNamespace("soilKey"), mode = "function", inherits = FALSE),
    logical(1))
  list(system = system, n = length(preds), missing = preds[!ok])
}
