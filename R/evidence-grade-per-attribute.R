# =============================================================================
# v0.9.99 -- per-attribute evidence grade.
#
# The global compute_evidence_grade() collapses a whole pedon to a single
# A/B/C/D/E letter. For the photo-only pipeline (and for the provenance-
# weighted uncertainty MC in v0.9.100) we need the grade resolved per
# (horizon, attribute) cell, so that a profile mixing measured texture,
# spectra-predicted CEC and SoilGrids-inferred pH can be reasoned about
# attribute by attribute.
# =============================================================================


# Map a provenance source code to an evidence-grade letter.
# A = measured, B = predicted_spectra, C = inferred_prior,
# D = extracted_vlm, E = user_assumed.
.source_to_grade <- function(source) {
  map <- c(measured          = "A",
           predicted_spectra = "B",
           inferred_prior    = "C",
           extracted_vlm     = "D",
           user_assumed      = "E")
  out <- unname(map[source])
  out[is.na(out)] <- "E"
  out
}


#' Per-attribute provenance-aware evidence grade
#'
#' Resolves the evidence grade of every \code{(horizon, attribute)} cell
#' that carries a provenance entry. Where a cell has more than one entry
#' (a value re-sourced over the profile's lifetime) the most authoritative
#' source wins, mirroring \code{\link{PedonRecord}}'s own authority order.
#'
#' Grades: \code{A} measured, \code{B} predicted from spectra, \code{C}
#' inferred from a spatial prior, \code{D} extracted by a vision-language
#' model, \code{E} user-assumed.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{data.table} with columns \code{horizon_idx},
#'         \code{attribute} and \code{grade}, sorted by horizon then
#'         attribute. A pedon with no provenance entries yields a
#'         zero-row table.
#' @seealso \code{\link{classify_from_photos}}, the global
#'          evidence grade reported on every \code{\link{ClassificationResult}}.
#' @examples
#' p <- make_ferralsol_canonical()
#' compute_per_attribute_evidence_grade(p)   # all-measured -> all grade A
#' @export
compute_per_attribute_evidence_grade <- function(pedon) {
  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord")
  }
  empty <- data.table::data.table(
    horizon_idx = integer(0),
    attribute   = character(0),
    grade       = character(0)
  )
  prov <- pedon$provenance
  if (is.null(prov) || nrow(prov) == 0L) return(empty)

  prov <- as.data.frame(prov, stringsAsFactors = FALSE)
  auth <- provenance_authority(prov$source)
  auth[is.na(auth)] <- 0L
  key  <- paste(prov$horizon_idx, prov$attribute, sep = "\r")

  rows <- lapply(split(seq_len(nrow(prov)), key), function(idx) {
    win <- idx[which.max(auth[idx])]
    data.frame(
      horizon_idx = as.integer(prov$horizon_idx[win]),
      attribute   = as.character(prov$attribute[win]),
      grade       = .source_to_grade(prov$source[win]),
      stringsAsFactors = FALSE
    )
  })
  out <- data.table::rbindlist(rows)
  data.table::setorder(out, horizon_idx, attribute)
  out[]
}
