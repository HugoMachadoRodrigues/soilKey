# ================================================================
# Rule engine
#
# The rule engine consumes the YAML key (e.g.
# inst/rules/wrb2022/key.yaml), iterates over RSGs in canonical order,
# and for each RSG evaluates its tests by resolving named diagnostic
# functions in the soilKey namespace.
#
# Three test combinators are recognised:
#   - all_of: every test must pass
#   - any_of: at least one test must pass
#   - default: catch-all (used for the last RSG, e.g. Regosols)
#
# A pseudo-combinator `not_implemented_v01:` marks RSGs whose
# diagnostics are scheduled for a later release. The engine returns NA
# for those tests and continues to the next RSG, which keeps the trace
# transparent without falsely failing.
#
# Three-valued logic: any test that returns NA propagates: with all_of,
# the result is NA if no test failed but at least one was NA; with
# any_of, the result is NA if no test passed but at least one was NA.
# ================================================================


#' Load a soilKey rule set (YAML)
#'
#' @param system One of \code{"wrb2022"} (full WRB 2022 key, v0.2 wires
#'        16/32 RSGs), \code{"usda"} (USDA Soil Taxonomy, v0.2 scaffold
#'        with one delegating diagnostic), or \code{"sibcs5"} (SiBCS
#'        5th edition, v0.2 scaffold with one delegating diagnostic).
#' @param package Package owning the rule files (default \code{"soilKey"}).
#' @return A parsed YAML list with elements \code{version},
#'         \code{source}, and a system-specific taxa list
#'         (\code{rsgs}, \code{orders}, or \code{ordens}).
#' @export
load_rules <- function(system = c("wrb2022", "usda", "sibcs5"),
                         package = "soilKey") {
  system <- match.arg(system)
  path <- system.file("rules", system, "key.yaml", package = package)
  if (path == "" || !file.exists(path)) {
    rlang::abort(sprintf(
      "Rule file not found for system '%s' (looked in package '%s')",
      system, package
    ))
  }
  rules <- yaml::read_yaml(path)

  # v0.7.3 / v0.8.3: merge split sub-level YAMLs from per-level
  # subdirectories. Each file contributes its named block (matching
  # the directory name with hyphens replaced by underscores) into the
  # merged rules object returned to the caller. Allows per-ordem
  # files for the SiBCS 3o-4o categorical levels and the USDA
  # Suborders / Great Groups / Subgroups (Path C, v0.8+).
  # Backward-compatible: WRB without subdirs keeps current behavior.
  #
  # Mapping subdir -> rules block:
  #   grandes-grupos/ (SiBCS) -> rules$grandes_grupos
  #   subgrupos/      (SiBCS) -> rules$subgrupos
  #   subordens/      (SiBCS) -> rules$subordens (currently in key.yaml,
  #                              future-proofing)
  #   suborders/      (USDA)  -> rules$suborders
  #   great-groups/   (USDA)  -> rules$great_groups
  #   subgroups/      (USDA)  -> rules$subgroups
  for (subdir_name in c("grandes-grupos", "subgrupos", "subordens",
                          "suborders", "great-groups", "subgroups")) {
    sub_path <- system.file("rules", system, subdir_name, package = package)
    if (!nzchar(sub_path) || !dir.exists(sub_path)) next
    files <- list.files(sub_path, pattern = "\\.yaml$", full.names = TRUE)
    if (length(files) == 0L) next
    yaml_key <- gsub("-", "_", subdir_name)
    merged <- list()
    for (f in files) {
      content <- yaml::read_yaml(f)
      block <- content[[yaml_key]]
      if (!is.null(block)) merged <- c(merged, block)
    }
    rules[[yaml_key]] <- c(rules[[yaml_key]] %||% list(), merged)
  }

  rules
}


#' Run a taxonomic key (system-agnostic engine)
#'
#' Iterates over the taxa list at \code{rules[[level_key]]} in
#' canonical order; the first taxon whose tests pass is assigned.
#' \code{evaluate_rsg_tests} is reused as the per-taxon evaluator
#' regardless of system -- the test combinator semantics
#' (\code{all_of} / \code{any_of} / \code{default} /
#' \code{not_implemented_v01}) are the same in all three systems.
#'
#' Used at the TOP level (RSG / Order / Ordem). For nested categorical
#' levels (subordens, grandes grupos, subgrupos, familias) iterate the
#' flat taxa list directly via \code{\link{run_taxa_list}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules A parsed rule set (output of \code{\link{load_rules}}).
#' @param level_key Name of the taxa list inside \code{rules}: typically
#'        \code{"rsgs"} (WRB), \code{"orders"} (USDA), or
#'        \code{"ordens"} (SiBCS).
#' @return A list with \code{assigned} (the YAML entry of the assigned
#'         taxon) and \code{trace} (one entry per taxon tested).
#' @export
run_taxonomic_key <- function(pedon, rules, level_key) {
  taxa <- rules[[level_key]]
  if (is.null(taxa) || length(taxa) == 0L) {
    rlang::abort(sprintf("Rule set has no '%s' list", level_key))
  }
  run_taxa_list(pedon, taxa)
}


#' Iterate a flat taxa list and evaluate tests in canonical order
#'
#' Internal iterator extracted from \code{\link{run_taxonomic_key}} so
#' nested categorical levels (subordens, grandes grupos, subgrupos,
#' familias) can be iterated directly, without going through the
#' \code{rules[[level_key]]} indirection that only makes sense at the
#' top level.
#'
#' Behavioural note: when \code{taxa} is empty or \code{NULL}, returns
#' \code{list(assigned = NULL, trace = list())} -- a sub-level lookup
#' with no canonical entries is non-fatal. The top-level
#' \code{\link{run_taxonomic_key}} keeps the stricter "missing list is
#' an error" semantics by guarding before calling this helper.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param taxa A list of taxon entries; each entry must have
#'        \code{code}, \code{name}, and \code{tests} fields, where
#'        \code{tests} is a block parseable by
#'        \code{\link{evaluate_rsg_tests}}.
#' @return A list with \code{assigned} (the entry of the assigned taxon,
#'         or NULL when \code{taxa} was empty) and \code{trace}.
#' @export
run_taxa_list <- function(pedon, taxa) {
  if (is.null(taxa) || length(taxa) == 0L) {
    return(list(assigned = NULL, trace = list()))
  }

  trace <- list()
  for (taxon in taxa) {
    result <- evaluate_rsg_tests(pedon, taxon$tests)
    trace[[taxon$code]] <- list(
      code     = taxon$code,
      name     = taxon$name,
      passed   = result$passed,
      evidence = result$evidence,
      missing  = result$missing %||% character(0),
      notes    = result$notes
    )
    if (isTRUE(result$passed)) {
      return(list(assigned = taxon, trace = trace))
    }
  }

  list(
    assigned = taxa[[length(taxa)]],
    trace    = trace
  )
}


#' Evaluate the test block of a single RSG
#'
#' Given a parsed \code{tests} block from a YAML key entry, evaluates
#' the appropriate combinator and returns a list with \code{passed},
#' \code{evidence}, \code{missing}, and (optionally) \code{notes}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param tests A \code{tests} block from the YAML.
#' @return A list summarising the test outcome.
#' @export
evaluate_rsg_tests <- function(pedon, tests) {

  # v0.1 stub: an RSG whose diagnostics are not yet implemented.
  if (!is.null(tests$not_implemented_v01)) {
    return(list(
      passed   = NA,
      evidence = list(),
      missing  = paste0("diagnostic_", tests$not_implemented_v01),
      notes    = sprintf(
        "RSG diagnostic '%s' is scheduled for a future release (v0.1 stub)",
        tests$not_implemented_v01
      )
    ))
  }

  # Catch-all default.
  if (isTRUE(tests$default)) {
    return(list(
      passed   = TRUE,
      evidence = list(),
      missing  = character(0),
      notes    = "default (catch-all)"
    ))
  }

  combinator <- if (!is.null(tests$any_of)) "any_of"
                 else if (!is.null(tests$all_of)) "all_of"
                 else NULL

  if (is.null(combinator)) {
    return(list(
      passed   = NA,
      evidence = list(),
      missing  = character(0),
      notes    = "Malformed test block: no any_of / all_of / default"
    ))
  }

  test_list <- tests[[combinator]]
  results   <- lapply(test_list, function(spec) run_single_test(pedon, spec))

  passed_vec <- vapply(results, function(r) isTRUE(r$passed), logical(1))
  na_vec     <- vapply(results, function(r) is.na(r$passed),  logical(1))

  passed <- if (combinator == "any_of") {
    if (any(passed_vec)) TRUE
    else if (any(na_vec)) NA
    else FALSE
  } else { # all_of
    if (all(passed_vec)) TRUE
    else if (any(na_vec) && all(passed_vec | na_vec)) NA
    else FALSE
  }

  missing_collected <- unique(unlist(lapply(results, `[[`, "missing")))
  if (is.null(missing_collected)) missing_collected <- character(0)

  list(
    passed   = passed,
    evidence = results,
    missing  = missing_collected
  )
}


#' Resolve and run a single named diagnostic test
#'
#' Looks up \code{names(test_spec)[1]} as a function exported by
#' soilKey, calls it with the pedon and the parameters listed in
#' \code{test_spec[[1]]}, and normalises the return value.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param test_spec A one-entry named list parsed from YAML, e.g.
#'        \code{list(ferralic = list(min_thickness = 30))}.
#' @return A list with \code{test_name}, \code{passed}, \code{layers},
#'         \code{missing}, plus optional \code{evidence}, \code{reference},
#'         \code{notes}.
#' @keywords internal
run_single_test <- function(pedon, test_spec) {

  if (!is.list(test_spec) || length(test_spec) == 0L ||
      is.null(names(test_spec))) {
    return(list(
      test_name = "<malformed>",
      passed    = NA,
      layers    = integer(0),
      missing   = character(0),
      notes     = "Test spec lacks a named diagnostic"
    ))
  }

  test_name <- names(test_spec)[1L]
  test_args <- test_spec[[1L]]
  if (is.null(test_args) ||
      (is.list(test_args) && length(test_args) == 0L)) {
    test_args <- list()
  }

  fn <- tryCatch(
    get(test_name, envir = asNamespace("soilKey"),
        mode = "function", inherits = FALSE),
    error = function(e) NULL
  )

  if (is.null(fn)) {
    return(list(
      test_name = test_name,
      passed    = NA,
      layers    = integer(0),
      missing   = paste0("diagnostic_", test_name),
      notes     = sprintf(
        "Diagnostic '%s' is not yet implemented in soilKey", test_name
      )
    ))
  }

  call_args <- c(list(pedon = pedon), test_args)

  result <- tryCatch(
    do.call(fn, call_args),
    error = function(e) {
      list(
        test_name = test_name,
        passed    = NA,
        layers    = integer(0),
        missing   = character(0),
        notes     = sprintf("Error running '%s': %s",
                              test_name, conditionMessage(e))
      )
    }
  )

  # Normalise R6 DiagnosticResult and plain-list returns to a flat dict.
  if (inherits(result, "DiagnosticResult") || inherits(result, "R6")) {
    list(
      test_name = test_name,
      passed    = result$passed,
      layers    = result$layers   %||% integer(0),
      missing   = result$missing  %||% character(0),
      evidence  = result$evidence,
      reference = result$reference,
      notes     = result$notes
    )
  } else if (is.list(result) && "passed" %in% names(result)) {
    result$test_name <- test_name
    if (is.null(result$missing)) result$missing <- character(0)
    if (is.null(result$layers))  result$layers  <- integer(0)
    result
  } else {
    list(
      test_name = test_name,
      passed    = NA,
      layers    = integer(0),
      missing   = character(0),
      notes     = "Unrecognised return type from diagnostic"
    )
  }
}
