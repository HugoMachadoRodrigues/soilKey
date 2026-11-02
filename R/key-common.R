# =============================================================================
# v0.9.264 -- helpers shared by every key: the evidence grade, the
# no-property check and its warning, the ambiguities and the missing
# attributes collected from a key trace. They lived in key-wrb2022.R although
# all three keys use them; they move here unchanged so that new systems use
# them from a neutral place.
# =============================================================================


#' Compute the provenance-aware evidence grade
#'
#' Returns the weakest grade present across the pedon's provenance
#' ledger: A if every recorded provenance is \code{"measured"}, B if
#' any \code{"predicted_spectra"}, C if any \code{"inferred_prior"}, D
#' if any \code{"extracted_vlm"}, E if any \code{"user_assumed"}. If no
#' provenance is recorded, defaults to A (assume measured).
#'
#' v0.9.213: \code{NA} when no horizon carries a soil property, only
#' depths, designations and boundaries. Such a profile ran through every
#' key and ended at its catch-all (Regosols, Neossolos, Entisols) by
#' elimination, with nothing verified, and was graded A because a pedon
#' with no provenance is read as measured. The keys add
#' \code{.NO_PROPERTIES_WARNING} to the result's warnings.
#'
#' Grade E was split out from D in v0.9.99 so that a wholly assumed
#' value is distinguishable from a VLM-extracted one; see
#' \code{\link{compute_per_attribute_evidence_grade}} for the
#' cell-by-cell breakdown.
#'
#' @noRd
#' @param pedon A \code{\link{PedonRecord}}.
compute_evidence_grade <- function(pedon, trace) {
  if (!.pedon_has_properties(pedon)) return(NA_character_)
  prov <- pedon$provenance
  if (is.null(prov) || nrow(prov) == 0L) {
    return("A")
  }
  sources <- unique(prov$source)
  if ("user_assumed"      %in% sources) return("E")
  if ("extracted_vlm"     %in% sources) return("D")
  if ("inferred_prior"    %in% sources) return("C")
  if ("predicted_spectra" %in% sources) return("B")
  "A"
}


# Columns that place and name a horizon rather than describe it.
.HORIZON_FRAME_COLS <- c("top_cm", "bottom_cm", "designation",
                         "boundary_distinctness", "boundary_topography")

#' Does any horizon carry a soil property?
#'
#' TRUE when any horizon has a value outside \code{.HORIZON_FRAME_COLS}: a
#' laboratory value, a Munsell colour, structure, consistence, any field
#' observation.
#' @noRd
.pedon_has_properties <- function(pedon) {
  h <- pedon$horizons
  if (is.null(h) || nrow(h) == 0L) return(FALSE)
  for (cn in setdiff(names(h), .HORIZON_FRAME_COLS)) {
    x <- h[[cn]]
    x <- x[!is.na(x)]
    if (is.character(x)) x <- x[nzchar(trimws(x))]
    if (length(x) > 0L) return(TRUE)
  }
  FALSE
}

.NO_PROPERTIES_WARNING <- paste(
  "No horizon carries a soil property, only depths and designations:",
  "the class was reached by elimination, with nothing in it verified,",
  "so it has no evidence grade. Add measured or described properties",
  "(texture, colour, CEC, base saturation, ...) to classify the profile.")


#' Collect ambiguous RSG candidates from the trace
#'
#' v0.1 rule: an entry is ambiguous if its result is NA and at least one
#' attribute (not just a stubbed diagnostic) was reported missing.
#'
#' @noRd
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
#' @noRd
collect_missing_attributes <- function(trace) {
  all_missing <- unique(unlist(lapply(trace,
                                       function(e) e$missing %||% character(0))))
  if (is.null(all_missing)) return(character(0))
  all_missing[!grepl("^diagnostic_", all_missing)]
}


# =============================================================================
# v0.9.334 -- an additive level: several classes at once after the last level
# of a key, the generic form of the Hungarian subtype properties
# (.hscs_add_properties(), which keeps its own code). The Russian subtypes
# will be the first users: every feature of the division's list that passes
# is named, in list order (Hugo's decision Q16, 2026-11-02).
# =============================================================================

#' Add an additive level to a classification result
#'
#' After the class assigned at level `after` (the type), every entry of
#' `features` that passes is named, in list order, after the class name:
#' "<type><name_sep><feature 1><list_sep><feature 2>". When every entry is
#' FALSE the class is the typical one (`typical`); when some are NA the
#' level is provisional, the undecided entries listed in
#' `provisional_because` ("<level>:<id>") and in a warning, the passing ones
#' still named, and with none passing the name is the class's alone. Entries
#' whose id is in `already` (written in the type's formula) are not evaluated
#' and not named again. Nothing changes when the class at `after` is not
#' assigned. The result passed in is not modified (an R6 result is cloned),
#' and applying the level again replaces it.
#'
#' @param res A \code{ClassificationResult} (or a list with the same fields).
#' @param pedon The pedon, passed to the tests.
#' @param features The list in its order: each entry a list with \code{id},
#'   \code{name}, optionally \code{name_en}, and either \code{test}, a
#'   function of \code{(pedon, context)} returning TRUE, FALSE, NA or a list
#'   with \code{passed}, \code{evidence}, \code{missing} and \code{notes}, or
#'   \code{passed}, the verdict already known. Ids must be unique.
#' @param level The name of the level added (rows of \code{res$levels}, the
#'   trace phase, the \code{qualifiers} entry).
#' @param after The level whose class the entries qualify.
#' @param already Ids not evaluated: features the class already names.
#' @param typical,typical_en The name of the typical class, and its gloss.
#'   \code{typical = NULL} (v0.9.340): the system has no typical class, so
#'   when nothing is named no row is added and the name is the class's alone.
#' @param context Passed to the tests with \code{type} (the code of the
#'   class) and \code{result}.
#' @param name_sep,list_sep Separators: between the class name and the
#'   features, and between features.
#' @return A copy of `res`, with one row per feature named (or one for the
#'   typical class, code NA) added to \code{levels}, the full name in
#'   \code{name}, the names in \code{qualifiers[[level]]}, one trace phase
#'   \code{level} with every entry (\code{status} "named", "not named",
#'   "undecided", or "in the formula" for the entries of `already`, which
#'   hold by the type: \code{passed} TRUE, \code{skipped} TRUE), and
#'   \code{provisional_because}, \code{warnings} and \code{missing_data}
#'   updated.
#' @noRd
.add_additive_level <- function(res, pedon, features, level = "subtypes", after = "types",
                                already = character(0), typical = "[typical]",
                                typical_en = typical, context = list(),
                                name_sep = " ", list_sep = ", ") {
  if (inherits(res, "R6")) res <- res$clone(deep = TRUE)
  lv <- res$levels
  if (!is.data.frame(lv) || !nrow(lv)) return(res)
  at <- which(lv$level == after)
  if (!length(at) || is.na(lv$code[at[1L]])) return(res)
  ids_all <- vapply(features, function(f) as.character(f[["id"]] %||% NA_character_)[1L], "")
  if (anyNA(ids_all) || any(!nzchar(ids_all)))
    rlang::abort(sprintf("Entry %d of the %s list has no id.", which(is.na(ids_all) | !nzchar(ids_all))[1L], level))
  if (anyDuplicated(ids_all))
    rlang::abort(sprintf("The %s list repeats the id(s) %s.", level,
                         paste(unique(ids_all[duplicated(ids_all)]), collapse = ", ")))
  # applying the level again replaces it
  lv <- lv[lv$level != level, , drop = FALSE]
  rownames(lv) <- NULL
  prefix <- paste0(level, ":")
  res$provisional_because <- res$provisional_because[!startsWith(res$provisional_because, prefix)]
  warn_prefix <- sprintf("Provisional %s:", level)
  res$warnings <- res$warnings[!startsWith(res$warnings, warn_prefix)]
  type_code <- lv$code[at[1L]]
  type_name <- as.character(lv$name[at[1L]])
  ctx <- c(list(type = type_code, result = res), context)
  trace <- list()
  ids <- character(0); labels <- character(0); glosses <- character(0)
  und <- character(0); missing <- character(0)
  for (k in seq_along(features)) {
    f <- features[[k]]
    id <- ids_all[k]
    nm <- as.character(f[["name"]] %||% id)[1L]
    if (id %in% already) {
      trace[[id]] <- list(code = id, name = nm, passed = TRUE, status = "in the formula",
                          skipped = TRUE, evidence = NULL, missing = character(0),
                          notes = "written in the type's formula: not named again")
      next
    }
    notes <- character(0)
    r <- if (is.function(f[["test"]])) {
      tryCatch(withCallingHandlers(f[["test"]](pedon, ctx), warning = function(w) {
        notes <<- c(notes, paste("warning:", conditionMessage(w)))
        invokeRestart("muffleWarning")
      }), error = function(e) list(passed = NA, notes = paste("error:", conditionMessage(e))))
    } else f[["passed"]]
    if (!is.list(r)) r <- list(passed = r)
    p <- suppressWarnings(as.logical(r[["passed"]]))
    if (length(p) != 1L) {
      notes <- c(notes, sprintf("the test gave %d values, read as undecided", length(p)))
      p <- NA
    }
    miss <- as.character(unlist(r[["missing"]]))
    trace[[id]] <- list(code = id, name = nm, passed = p,
                        status = if (isTRUE(p)) "named" else if (isFALSE(p)) "not named" else "undecided",
                        evidence = r[["evidence"]],
                        missing = if (is.na(p)) miss else character(0),
                        notes = c(r[["notes"]], notes))
    if (isTRUE(p)) {
      ids <- c(ids, id); labels <- c(labels, nm)
      glosses <- c(glosses, as.character(f[["name_en"]] %||% NA_character_)[1L])
    } else if (is.na(p)) {
      und <- c(und, id); missing <- c(missing, miss)
    }
  }
  # v0.9.340: typical = NULL, a system with no typical class (the
  # Referentiel's qualifiers): nothing is added when nothing is named
  is_typical <- !length(ids) && !length(und) && !is.null(typical)
  add_code <- if (length(ids)) ids else if (is_typical) NA_character_ else character(0)
  add_name <- if (length(ids)) labels else if (is_typical) typical else character(0)
  add_en <- if (length(ids)) glosses else if (is_typical) typical_en else character(0)
  if (length(add_code)) {
    add <- lv[rep(NA_integer_, length(add_code)), , drop = FALSE]
    add$level <- level
    add$code <- add_code
    add$name <- add_name
    if (!is.null(lv$name_en) || any(!is.na(add_en))) {
      if (is.null(lv$name_en)) lv$name_en <- NA_character_
      add$name_en <- add_en
    }
    rownames(add) <- NULL
    lv <- rbind(lv, add)
  }
  res$levels <- lv
  # the full name: the class, then what is named
  res$name <- if (length(add_name)) paste0(type_name, name_sep, paste(add_name, collapse = list_sep))
              else type_name
  q <- res$qualifiers %||% list()
  q[[level]] <- add_name
  res$qualifiers <- q
  tr <- res$trace %||% list()
  tr[[level]] <- trace
  res$trace <- tr
  if (length(und)) {
    res$provisional_because <- c(res$provisional_because, paste0(prefix, und))
    res$warnings <- c(res$warnings, sprintf(
      "%s %d listed feature(s) could not be decided (%s).", warn_prefix,
      length(und), paste(und, collapse = ", ")))
  }
  missing <- missing[!grepl("^diagnostic_", missing)]
  if (length(missing)) res$missing_data <- unique(c(res$missing_data, missing))
  res
}
