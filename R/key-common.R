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
