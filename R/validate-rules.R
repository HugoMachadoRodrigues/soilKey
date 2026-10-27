# =============================================================================
# v0.9.118 -- .validate_rules(): a proactive integrity check that every
# predicate referenced by a system's YAML rule base actually exists as a
# function in the package namespace. The rule engine already degrades
# gracefully at runtime (a missing predicate yields passed = NA with a note;
# see evaluate_test_spec in rule-engine.R), but a typo'd predicate name in a
# rule would then fail *silently*. This validator catches such malformed rules
# at test time across all three systems.
# =============================================================================

# The test combinators of a rule block (see evaluate_rsg_tests); since
# v0.9.264 they may also appear nested, as an entry of another block.
.RULE_COMBINATORS <- c("all_of", "any_of", "none_of")

# Recursively collect every predicate name referenced in a rules object: the
# name of each spec inside an all_of / any_of / none_of test list, at any depth.
# A nested combinator is not a predicate; its own entries are collected.
.collect_rule_predicates <- function(x, acc = character(0)) {
  if (!is.list(x)) return(acc)
  for (combinator in .RULE_COMBINATORS) {
    block <- x[[combinator]]
    if (!is.null(block) && is.list(block)) {
      for (spec in block) {
        nm <- names(spec)[1L]
        if (!is.null(nm) && nzchar(nm) && !nm %in% .RULE_COMBINATORS)
          acc <- c(acc, nm)
      }
    }
  }
  for (el in x) acc <- .collect_rule_predicates(el, acc)
  acc
}

# Blocks that name more than one of all_of / any_of / none_of / default /
# not_implemented_v01. The engine reads only one of them (any_of first), so
# such a block silently ignores part of what it says; the validator reports it.
.collect_mixed_blocks <- function(x, path = "", acc = character(0)) {
  if (!is.list(x)) return(acc)
  kinds <- intersect(names(x), c(.RULE_COMBINATORS, "default",
                                 "not_implemented_v01"))
  if (length(kinds) > 1L) {
    acc <- c(acc, sprintf("%s (%s)", path, paste(kinds, collapse = " + ")))
  }
  nms <- names(x) %||% rep("", length(x))
  for (i in seq_along(x)) {
    lab <- if (nzchar(nms[i])) nms[i]
           else if (is.list(x[[i]]) && !is.null(x[[i]]$code)) x[[i]]$code
           else as.character(i)
    acc <- .collect_mixed_blocks(x[[i]], paste0(path, "/", lab), acc)
  }
  acc
}

# Combinator blocks whose value is not a YAML sequence (an unnamed list). A
# block written as a mapping (`any_of: {a: {}, b: {}}`) is read as one
# malformed test, stays undecided whatever the profile, and hides its
# predicates from the checks above.
.collect_malformed_blocks <- function(x, path = "", acc = character(0)) {
  if (!is.list(x)) return(acc)
  for (cmb in .RULE_COMBINATORS) {
    b <- x[[cmb]]
    if (!is.null(b) && (!is.list(b) || !is.null(names(b)))) {
      acc <- c(acc, sprintf("%s (%s is not a list)", path, cmb))
    }
  }
  nms <- names(x) %||% rep("", length(x))
  for (i in seq_along(x)) {
    lab <- if (nzchar(nms[i])) nms[i]
           else if (is.list(x[[i]]) && !is.null(x[[i]]$code)) x[[i]]$code
           else as.character(i)
    acc <- .collect_malformed_blocks(x[[i]], paste0(path, "/", lab), acc)
  }
  acc
}

# Class entries whose code or name is not a single non-empty string, codes
# listed twice in one list, and parent codes listed twice in one level (two
# level files for the same parent: only the first would ever be read). YAML
# reads an unquoted `N`, `Y`, `yes` or `no` as a logical and a bare number as
# a number, so national codes must be quoted.
.collect_code_problems <- function(rules, sys) {
  probs <- character(0)
  ok <- function(v) is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
  lists <- list()
  lists[[sys$top_level]] <- rules[[sys$top_level]]
  for (lvl in sys$levels[-1L]) {
    m <- rules[[lvl]]
    keys <- names(m) %||% character(0)
    twice <- unique(keys[duplicated(keys)])
    if (length(twice) > 0L) {
      probs <- c(probs, sprintf("%s: parent code(s) listed twice: %s",
                                lvl, paste(twice, collapse = ", ")))
    }
    odd <- keys[keys %in% c("TRUE", "FALSE") | !nzchar(keys)]
    if (length(odd) > 0L) {
      probs <- c(probs, sprintf("%s: parent key(s) %s look unquoted",
                                lvl, paste(odd, collapse = ", ")))
    }
    for (i in seq_along(m)) lists[[paste0(lvl, "/", keys[i])]] <- m[[i]]
  }
  for (lab in names(lists)) {
    codes <- character(0)
    for (i in seq_along(lists[[lab]])) {
      t <- lists[[lab]][[i]]
      if (!is.list(t) || !ok(t$code) || !ok(t$name)) {
        probs <- c(probs, sprintf("%s, entry %d: code and name must be quoted strings",
                                  lab, i))
      } else {
        codes <- c(codes, t$code)
      }
    }
    twice <- unique(codes[duplicated(codes)])
    if (length(twice) > 0L) {
      probs <- c(probs, sprintf("%s: code(s) listed twice: %s",
                                lab, paste(twice, collapse = ", ")))
    }
  }
  # v0.9.291: a "<level>_from" entry names a class of the top level, and the
  # list it stands for is there and empty (else the key would never read it)
  top_codes <- vapply(rules[[sys$top_level]] %||% list(),
                      function(t) as.character(t$code %||% NA_character_), "")
  for (lvl in sys$levels[-1L]) {
    fb <- rules[[paste0(lvl, "_from")]]
    for (k in names(fb) %||% character(0)) {
      spec <- fb[[k]]
      cls <- if (is.list(spec)) spec$class else spec
      if (!ok(cls) || !(cls %in% top_codes)) {
        probs <- c(probs, sprintf("%s_from/%s: names no class of the %s list",
                                  lvl, k, sys$top_level))
      }
      if (is.list(spec) && !is.null(spec$site) && (!is.list(spec$site) || is.null(names(spec$site)))) {
        probs <- c(probs, sprintf("%s_from/%s: site must be a mapping", lvl, k))
      }
      if (is.null(rules[[lvl]][[k]]) || length(rules[[lvl]][[k]]) > 0L) {
        probs <- c(probs, sprintf("%s_from/%s: the %s list of %s must be present and empty",
                                  lvl, k, lvl, k))
      }
    }
  }
  probs
}

# Check a system's rules: every predicate exists as a function in the soilKey
# namespace; no block mixes combinators; and, for a system added after the
# registry (not legacy), every predicate carries the system's suffix
# (<name>_<namespace>), so that its diagnostics never collide with another
# system's (a Canadian Chernozemic gate is not WRB's chernozem()); no
# combinator is written as a mapping; and every class has a quoted code and
# name, unique within its list.
# Returns a list(system, n, missing, mixed, unsuffixed, malformed, codes).
.validate_rules <- function(system = "wrb2022") {
  system <- .system_resolve(system)
  sys    <- .system_get(system)
  rules  <- load_rules(system)
  preds  <- unique(.collect_rule_predicates(rules))
  preds  <- sort(preds[nzchar(preds)])
  ok <- vapply(preds, function(p)
    exists(p, envir = asNamespace("soilKey"), mode = "function", inherits = FALSE),
    logical(1))
  suffix <- paste0("_", sys$namespace %||% system)
  unsuffixed <- if (sys$legacy) character(0)
                else preds[!endsWith(preds, suffix)]
  list(system = system, n = length(preds), missing = preds[!ok],
       mixed = .collect_mixed_blocks(rules), unsuffixed = unsuffixed,
       malformed = .collect_malformed_blocks(rules),
       codes = .collect_code_problems(rules, sys))
}
