# ================================================================
# WRB 2022 key
#
# Top-level entry points:
#   - run_wrb_key(): consumes a PedonRecord and returns the list of
#                    (assigned RSG, full trace).
#   - classify_wrb2022(): builds the user-facing ClassificationResult
#                          on top of run_wrb_key, including provenance-
#                          aware evidence grade, ambiguities, missing-
#                          data hints, and (eventually) spatial-prior
#                          sanity check.
#
# In v0.1, the key.yaml has only one fully implemented RSG path:
# Ferralsols (via the ferralic diagnostic). The remaining 30 RSGs are
# stubbed with `not_implemented_v01` markers; their tests return NA and
# the engine continues to the next RSG, ultimately falling through to
# the Regosols catch-all.
# ================================================================


#' Run the WRB 2022 key over a pedon
#'
#' Iterates over the RSGs in canonical key order; the first RSG whose
#' tests pass is assigned. RSGs whose tests return NA (stubbed
#' diagnostics or insufficient data) are skipped and recorded in the
#' trace.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set; if NULL, reads
#'        \code{inst/rules/wrb2022/key.yaml}.
#' @return A list with \code{assigned} (the YAML entry for the assigned
#'         RSG) and \code{trace} (one entry per RSG tested, in order).
#' @export
run_wrb_key <- function(pedon, rules = NULL) {
  rules <- rules %||% load_rules("wrb2022")
  run_taxonomic_key(pedon, rules, level_key = "rsgs")
}


#' Classify a pedon under WRB 2022
#'
#' High-level classification entry point. Pre-computes the implemented
#' diagnostic horizons (argic, ferralic, mollic) for transparent
#' reporting, runs the key, and assembles a
#' \code{\link{ClassificationResult}} with the trace, ambiguities,
#' missing-data hints, evidence grade, and (in future) prior sanity
#' check.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param prior Optional spatial prior -- a \code{data.table} with
#'        columns \code{rsg_code} and \code{probability}, typically the
#'        return value of \code{\link{spatial_prior}}. If supplied, the
#'        result records a \code{prior_check} entry from
#'        \code{\link{prior_consistency_check}}; an inconsistent prior
#'        also emits a warning. The deterministic key is NEVER
#'        overridden by the prior.
#' @param prior_threshold Probability below which the prior triggers
#'        an "inconsistent" warning (default 0.01).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}. Behaviour when the trace reports missing
#'        attributes.
#' @param rules Optional pre-loaded rule set.
#' @return A \code{\link{ClassificationResult}}.
#' @export
classify_wrb2022 <- function(pedon,
                               prior           = NULL,
                               prior_threshold = 0.01,
                               on_missing      = c("warn", "silent", "error"),
                               rules           = NULL) {
  on_missing <- match.arg(on_missing)
  rules      <- rules %||% load_rules("wrb2022")

  # Pre-compute the implemented diagnostics so we can report which ones
  # passed even when the corresponding RSG test path is not yet wired up.
  diags <- list(
    argic    = tryCatch(argic(pedon),    error = function(e) NULL),
    ferralic = tryCatch(ferralic(pedon), error = function(e) NULL),
    mollic   = tryCatch(mollic(pedon),   error = function(e) NULL)
  )

  key_result <- run_wrb_key(pedon, rules)
  rsg        <- key_result$assigned

  rsg_codes  <- vapply(rules$rsgs, function(r) r$code, character(1))
  is_default <- identical(rsg$code, tail(rsg_codes, 1L))

  name <- compute_v01_classification_name(rsg, diags, is_default = is_default)

  ambiguities  <- find_ambiguities(key_result$trace, current = rsg$code,
                                     diags = diags)
  grade        <- compute_evidence_grade(pedon, key_result$trace)
  missing_data <- collect_missing_attributes(key_result$trace)

  warnings <- character(0)
  if (is_default) {
    passed_diag_names <- names(diags)[vapply(diags, function(d) {
      !is.null(d) && isTRUE(d$passed)
    }, logical(1))]
    if (length(passed_diag_names) > 0L) {
      warnings <- c(warnings, sprintf(
        paste0("Profile keyed to default catch-all (%s). However, the ",
                "following implemented diagnostics PASSED: %s. The RSGs ",
                "that depend on these diagnostics (Acrisols, Lixisols, ",
                "Alisols, Luvisols, Retisols for argic; Chernozems, ",
                "Kastanozems, Phaeozems, Umbrisols for mollic) are scheduled ",
                "for v0.2. The diagnostic functions can be called directly ",
                "for full evidence."),
        rsg$name, paste(passed_diag_names, collapse = ", ")
      ))
    }
  }

  if (length(missing_data) > 0L) {
    msg <- sprintf(
      "%d distinct attribute(s) missing across the key trace -- see $missing_data",
      length(missing_data)
    )
    if (on_missing == "warn") {
      warnings <- c(warnings, msg)
    } else if (on_missing == "error") {
      rlang::abort(msg)
    }
  }

  prior_check <- NULL
  if (!is.null(prior)) {
    prior_check <- prior_consistency_check(
      rsg_code  = rsg$code,
      prior     = prior,
      threshold = prior_threshold
    )
    if (isFALSE(prior_check$consistent)) {
      warnings <- c(warnings, prior_check$note)
    }
  }

  # v0.9: Resolve principal qualifiers for the assigned RSG.
  # v0.9.3.A: also resolve supplementary qualifiers (parenthesised
  # tags per WRB 2022 Ch 6 -- e.g. "Rhodic Ferralsol (Clayic, Humic,
  # Dystric)").
  qual_result <- tryCatch(
    resolve_wrb_qualifiers(pedon, rsg$code, rules),
    error = function(e) list(principal = character(0),
                              supplementary = character(0),
                              trace = list())
  )
  full_name <- if (length(qual_result$principal) > 0L) {
    format_wrb_name(rsg$name,
                     principal     = qual_result$principal,
                     supplementary = qual_result$supplementary %||% character(0))
  } else {
    name
  }

  ClassificationResult$new(
    system         = "WRB 2022",
    name           = full_name,
    rsg_or_order   = rsg$name,
    qualifiers     = list(principal     = qual_result$principal,
                            supplementary = qual_result$supplementary %||% character(0),
                            trace         = qual_result$trace,
                            trace_supplementary = qual_result$trace_supplementary %||% list()),
    trace          = key_result$trace,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = prior_check,
    warnings       = warnings
  )
}


# --------------------------------------------------------- helpers ----

#' Compose the v0.1 classification name with disambiguation hints
#'
#' When the catch-all (Regosols) is assigned but an implemented
#' diagnostic still passed, the name reflects that the true RSG is
#' identifiable in principle but not yet wired to the key.
#'
#' @keywords internal
compute_v01_classification_name <- function(rsg, diags, is_default) {

  if (!is_default) {
    return(rsg$name)
  }

  if (!is.null(diags$ferralic) && isTRUE(diags$ferralic$passed)) {
    return(paste0(
      "Ferralsol (catch-all path; ferralic diagnostic passed -- ",
      "promote when full Ferralsol path is wired in v0.2)"
    ))
  }
  if (!is.null(diags$argic) && isTRUE(diags$argic$passed)) {
    return(paste0(
      "Argic-RSG, undisambiguated ",
      "(Acrisol/Lixisol/Alisol/Luvisol/Retisol -- v0.2 scope)"
    ))
  }
  if (!is.null(diags$mollic) && isTRUE(diags$mollic$passed)) {
    return(paste0(
      "Mollic-RSG, undisambiguated ",
      "(Chernozem/Kastanozem/Phaeozem -- v0.2 scope)"
    ))
  }

  rsg$name
}


#' Compute the provenance-aware evidence grade
#'
#' v0.1 rule: A if every recorded provenance is \code{"measured"}, B if
#' any \code{"predicted_spectra"}, C if any \code{"inferred_prior"}, D
#' if any \code{"extracted_vlm"} or \code{"user_assumed"}. If no
#' provenance is recorded, defaults to A (assume measured).
#'
#' @keywords internal
compute_evidence_grade <- function(pedon, trace) {
  prov <- pedon$provenance
  if (is.null(prov) || nrow(prov) == 0L) {
    return("A")
  }
  sources <- unique(prov$source)
  if ("user_assumed"      %in% sources) return("D")
  if ("extracted_vlm"     %in% sources) return("D")
  if ("inferred_prior"    %in% sources) return("C")
  if ("predicted_spectra" %in% sources) return("B")
  "A"
}


#' Collect ambiguous RSG candidates from the trace
#'
#' v0.1 rule: an entry is ambiguous if its result is NA and at least one
#' attribute (not just a stubbed diagnostic) was reported missing.
#'
#' @keywords internal
find_ambiguities <- function(trace, current, diags = NULL) {
  ambiguities <- list()
  for (entry in trace) {
    if (identical(entry$code, current)) next
    if (is.na(entry$passed)) {
      missing <- entry$missing %||% character(0)
      attr_missing <- missing[!grepl("^diagnostic_", missing)]
      if (length(attr_missing) > 0L) {
        ambiguities[[length(ambiguities) + 1L]] <- list(
          rsg_code = entry$code,
          rsg_name = entry$name,
          reason   = sprintf(
            "Indeterminate -- missing %d attribute(s): %s",
            length(attr_missing),
            paste(unique(attr_missing), collapse = ", ")
          )
        )
      }
    }
  }
  ambiguities
}


#' Collect distinct missing soil attributes from the trace
#'
#' Filters out the \code{diagnostic_X} markers that record stubbed RSG
#' tests; the user can already see those in the trace and the
#' classification name.
#'
#' @keywords internal
collect_missing_attributes <- function(trace) {
  all_missing <- unique(unlist(lapply(trace,
                                       function(e) e$missing %||% character(0))))
  if (is.null(all_missing)) return(character(0))
  all_missing[!grepl("^diagnostic_", all_missing)]
}
