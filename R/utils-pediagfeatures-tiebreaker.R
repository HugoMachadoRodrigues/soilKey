# =============================================================================
# v0.9.21 -- pediagfeatures tie-breaker (NASIS surveyor diagnostics)
#
# The NASIS sqlite `pediagfeatures` table records which diagnostic
# horizons / properties the field surveyor identified directly. With
# 13 501 "Argillic horizon", 6 860 "Mollic epipedon", 4 970 "Cambic
# horizon", 829 "Spodic horizon", 519 "Slickensides", 494 "Andic soil
# properties" entries, this is the most authoritative source for
# diagnostic identification short of re-running the field survey.
#
# `load_kssl_pedons_with_nasis()` already populates
# `pedon$site$nasis_diagnostic_features` as a character vector of
# featkind values. v0.9.21 wires that into the Order-level USDA gates
# AS A TIE-BREAKER ONLY: when the canonical lab + morphology gate
# returns `passed = NA` (insufficient data), the field-survey tag
# turns it `TRUE`. When the canonical gate returns `TRUE` or `FALSE`,
# the tag is recorded as evidence but does NOT override -- preserving
# the deterministic-key-on-data invariant.
#
# Each gate that consults the tie-breaker also drops a string into
# its evidence list so the trace shows whether the Order assignment
# came from canonical chemistry, morphology, or surveyor diagnostic.
# =============================================================================


#' Has the field surveyor identified this diagnostic in NASIS?
#'
#' Looks at \code{pedon$site$nasis_diagnostic_features} for a
#' \code{featkind} value matching \code{pattern} (case-insensitive
#' regex). Returns \code{FALSE} when the slot is missing entirely
#' (e.g. lab-only loaders, non-KSSL pedons).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param pattern Regex pattern matched case-insensitively against
#'        each featkind string.
#' @return Logical scalar.
#' @keywords internal
.has_nasis_feature <- function(pedon, pattern) {
  feats <- pedon$site$nasis_diagnostic_features
  if (is.null(feats) || length(feats) == 0L) return(FALSE)
  any(!is.na(feats) & grepl(pattern, feats, ignore.case = TRUE))
}


#' Apply the NASIS surveyor tie-breaker to a DiagnosticResult
#'
#' When \code{result$passed} is \code{NA} (insufficient data) AND the
#' surveyor identified the matching diagnostic in NASIS, flips the
#' result to \code{TRUE} with a provenance note. \code{TRUE} or
#' \code{FALSE} canonical results are NOT overridden -- the function
#' returns the input unchanged in those cases.
#'
#' @param result A \code{\link{DiagnosticResult}} (from a canonical
#'        gate like \code{mollic_epipedon_usda()}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param pattern Regex pattern matched against
#'        \code{pediagfeatures.featkind}.
#' @param feature_label Short label for the provenance note.
#' @return The (possibly modified) DiagnosticResult.
#' @keywords internal
.apply_nasis_tiebreaker <- function(result, pedon, pattern,
                                       feature_label) {
  if (!is.na(result$passed)) return(result)
  if (!.has_nasis_feature(pedon, pattern)) return(result)
  # NA + surveyor confirmation -> TRUE.
  result$passed <- TRUE
  result$layers <- if (length(result$layers) == 0L)
                       seq_len(nrow(pedon$horizons))
                     else result$layers
  if (is.null(result$evidence)) result$evidence <- list()
  result$evidence$nasis_tiebreaker <- list(
    triggered    = TRUE,
    feature      = feature_label,
    source       = paste0("v0.9.21: canonical gate returned NA; ",
                            "NASIS pediagfeatures.featkind = '",
                            feature_label, "' confirms diagnostic")
  )
  result
}
