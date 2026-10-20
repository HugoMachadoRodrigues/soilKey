# =============================================================================
# v0.9.265 -- the generic classifier of ordered keys.
#
# Most national systems (Canada, Australia, New Zealand, Japan, Hungary,
# China) are keys of the USDA kind: at each level the first class whose tests
# pass is assigned, and the next level is the list of classes under it. Their
# classifiers are thin wrappers around classify_hierarchical(), which walks
# the levels named in the system's registry entry.
#
# Two rules differ from the three older keys, on purpose:
#   * a class is assigned only when its tests pass (a `default: true` entry
#     passes by definition). When nothing passes, the level stays unassigned
#     and the name stops at the level above, instead of falling back to the
#     last class of the list (run_taxa_list() returns it anyway; the old keys
#     demote it at some levels only).
#   * a class that could not be decided for lack of data, tested before the
#     one assigned, makes the assignment provisional: the result lists it in
#     provisional_because, as the Australian key does with its confidence
#     levels.
# WRB, SiBCS and USDA keep their own classifiers.
# =============================================================================


# Run one level: the first class whose tests pass, its trace, and the classes
# left undecided before it (all of them when nothing passed), counted by
# position as the key is read. Unlike run_taxa_list(), no class is returned
# when none passes. A class without a code stops with an error naming it
# (.validate_rules() reports the same at test time).
.run_level <- function(pedon, taxa, level) {
  trace <- list()
  undecided <- character(0)
  for (i in seq_along(taxa)) {
    taxon <- taxa[[i]]
    code <- as.character(taxon$code %||% NA_character_)
    if (length(code) != 1L || is.na(code) || !nzchar(code)) {
      rlang::abort(sprintf("Class %d of the %s list has no code.", i, level))
    }
    result <- evaluate_rsg_tests(pedon, taxon$tests)
    trace[[code]] <- list(
      code     = code,
      name     = taxon$name,
      passed   = result$passed,
      evidence = result$evidence,
      missing  = result$missing %||% character(0),
      notes    = result$notes
    )
    if (isTRUE(result$passed)) {
      taxon$code <- code
      return(list(assigned = taxon, trace = trace, undecided = undecided))
    }
    if (is.na(result$passed)) undecided <- c(undecided, code)
  }
  list(assigned = NULL, trace = trace, undecided = undecided)
}


#' Classify a pedon with an ordered (first-match) key
#'
#' The engine behind the classifiers of ordered national keys: it walks the
#' levels of the system's registry entry from the top, assigns at each level
#' the first class whose tests pass, and descends into the classes listed
#' under it.
#'
#' @param system Registry id or alias of the system.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{\link{load_rules}}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}}: \code{name} is the deepest
#'   class assigned (\code{NA} when no class of the top level passed),
#'   \code{levels} the class assigned at each level, \code{trace} one phase
#'   per level tested, and \code{provisional_because} the undecided classes
#'   tested before the assigned ones.
#' @noRd
classify_hierarchical <- function(system, pedon, rules = NULL,
                                  on_missing = c("warn", "silent", "error"),
                                  gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  id  <- .system_resolve(system)
  sys <- .system_get(id)
  .classify_require_horizons(pedon, sys$classifier)
  rules <- rules %||% load_rules(id)
  pedon <- .classify_apply_gapfill(pedon, gapfill)

  top <- sys$levels[1L]
  if (length(rules[[top]]) == 0L) {
    rlang::abort(sprintf("Rule set has no '%s' list", top))
  }

  trace <- list()
  chosen <- list()
  provisional <- character(0)
  stopped <- character(0)
  parent <- NULL
  for (lvl in sys$levels) {
    taxa <- if (is.null(parent)) rules[[lvl]] else rules[[lvl]][[parent$code]]
    if (length(taxa) == 0L) break
    r <- .run_level(pedon, taxa, lvl)
    trace[[lvl]] <- r$trace
    if (length(r$undecided) > 0L) {
      provisional <- c(provisional, paste0(lvl, ":", r$undecided))
    }
    if (is.null(r$assigned)) {
      # nothing passed below the top and nothing was left undecided: the
      # name stops at the level above, and the user should know why
      if (!is.null(parent) && length(r$undecided) == 0L) {
        stopped <- sprintf(
          "No class of the %s list under %s passed, so the name stops at %s.",
          lvl, parent$code, parent$name)
      }
      break
    }
    chosen[[lvl]] <- r$assigned
    parent <- r$assigned
  }

  levels <- data.frame(
    level = as.character(names(chosen)),
    code  = vapply(chosen, function(x) x$code, character(1), USE.NAMES = FALSE),
    name  = vapply(chosen, function(x) as.character(x$name %||% NA_character_),
                   character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  top_class <- chosen[[top]]
  name <- if (nrow(levels) > 0L) levels$name[nrow(levels)] else NA_character_

  top_trace    <- trace[[top]]
  ambiguities  <- find_ambiguities(top_trace,
                                   current = top_class$code %||% NA_character_)
  grade        <- compute_evidence_grade(pedon, top_trace)
  missing_data <- collect_missing_attributes(do.call(c, unname(trace)))

  warnings <- character(0)
  if (is.na(grade)) warnings <- c(warnings, .NO_PROPERTIES_WARNING)
  if (is.null(top_class)) {
    warnings <- c(warnings, sprintf(
      "No class of the %s key passed, so the profile is not classified; %s",
      top, "see $trace and $missing_data."))
  }
  warnings <- c(warnings, stopped)
  if (length(provisional) > 0L) {
    warnings <- c(warnings, sprintf(
      "Provisional: %d class(es) tested before the assigned one could not be decided (%s).",
      length(provisional), paste(provisional, collapse = ", ")))
  }
  if (length(missing_data) > 0L) {
    msg <- sprintf(
      "%d distinct attribute(s) missing across the key trace -- see $missing_data",
      length(missing_data)
    )
    if      (on_missing == "warn")  warnings <- c(warnings, msg)
    else if (on_missing == "error") rlang::abort(msg)
  }

  ClassificationResult$new(
    system         = sys$display$result %||% sys$display$en,
    name           = name,
    rsg_or_order   = top_class$name %||% NA_character_,
    qualifiers     = list(),
    trace          = trace,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = NULL,
    warnings       = warnings,
    system_id      = id,
    levels         = levels,
    provisional_because = provisional
  )
}
