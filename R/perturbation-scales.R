# =============================================================================
# v0.9.100 -- provenance-weighted perturbation scales.
#
# classification_robustness() (v0.9.42) perturbs every cell by the same fixed
# fraction. That treats a lab-measured clay value and a VLM-guessed one as
# equally trustworthy, which they are not. These helpers scale the Monte-Carlo
# noise by each cell's evidence grade: an A-grade measurement wobbles by a few
# percent, an E-grade assumption by a third of its value.
# =============================================================================


#' Monte-Carlo perturbation scale for an evidence grade
#'
#' Returns the noise magnitudes used by \code{\link{classify_with_uncertainty}}
#' for a cell of the given evidence grade. A measurement (grade A) is
#' perturbed only slightly; a user-assumed value (grade E) is perturbed
#' heavily, reflecting how little is actually known about it.
#'
#' @param grade One of \code{"A"} (measured), \code{"B"} (spectra-predicted),
#'        \code{"C"} (prior-inferred), \code{"D"} (VLM-extracted) or
#'        \code{"E"} (user-assumed).
#' @return A list with three elements: \code{pct} (the half-width of the
#'        multiplicative perturbation, applied to most numeric attributes),
#'        \code{ph_abs} (the half-width of the additive perturbation applied
#'        to pH columns) and \code{munsell_abs} (the additive half-width for
#'        Munsell value / chroma columns).
#' @examples
#' get_perturbation_scale("A")$pct   # 0.03 -- measured values barely move
#' get_perturbation_scale("E")$pct   # 0.30 -- assumptions move a lot
#' @export
get_perturbation_scale <- function(grade = c("A", "B", "C", "D", "E")) {
  grade <- match.arg(grade)
  list(
    pct         = c(A = 0.03, B = 0.07, C = 0.10, D = 0.17, E = 0.30)[[grade]],
    ph_abs      = c(A = 0.10, B = 0.20, C = 0.30, D = 0.50, E = 1.00)[[grade]],
    munsell_abs = c(A = 0.30, B = 0.50, C = 0.70, D = 1.00, E = 2.00)[[grade]]
  )
}

# Resolve the scale for a grade, honouring a caller-supplied override list.
.resolve_scale <- function(grade, scales = NULL) {
  if (!is.null(scales) && !is.null(scales[[grade]])) return(scales[[grade]])
  get_perturbation_scale(grade)
}

# Build a fast (horizon_idx, attribute) -> grade lookup from a pedon's
# provenance ledger. Cells with no provenance entry are absent from the
# lookup and treated as grade A (measured) by the perturbation code.
.build_grade_lookup <- function(pedon) {
  gt <- compute_per_attribute_evidence_grade(pedon)
  if (nrow(gt) == 0L) return(list())
  stats::setNames(as.list(as.character(gt$grade)),
                  paste(gt$horizon_idx, gt$attribute, sep = "\r"))
}

# Horizon columns that carry an additive (not multiplicative) perturbation.
.PH_COLS      <- c("ph_h2o", "ph_kcl", "ph_cacl2")
.MUNSELL_COLS <- c("munsell_value_moist", "munsell_chroma_moist",
                   "munsell_value_dry",   "munsell_chroma_dry")

# Perturb a pedon, scaling each numeric cell's noise by its evidence grade.
# `grade_lookup` is the table from .build_grade_lookup(); `exclude_cols`
# names columns held fixed at their baseline value (used for the
# leave-one-out sensitivity pass). Geometry columns are never perturbed.
.perturb_pedon_provenance <- function(pedon, grade_lookup,
                                      scales = NULL, exclude_cols = character(0)) {
  h <- data.table::copy(pedon$horizons)
  skip <- c("top_cm", "bottom_cm", exclude_cols)
  for (col in names(h)) {
    if (col %in% skip) next
    vals <- h[[col]]
    if (!is.numeric(vals)) next
    for (i in seq_along(vals)) {
      v <- vals[i]
      if (is.na(v)) next
      grade <- grade_lookup[[paste(i, col, sep = "\r")]]
      if (is.null(grade)) grade <- "A"
      sc <- .resolve_scale(grade, scales)
      noise <- if (col %in% .PH_COLS) {
                 stats::runif(1L, -sc$ph_abs, sc$ph_abs)
               } else if (col %in% .MUNSELL_COLS) {
                 stats::runif(1L, -sc$munsell_abs, sc$munsell_abs)
               } else {
                 v * stats::runif(1L, -sc$pct, sc$pct)
               }
      h[[col]][i] <- max(0, v + noise)
    }
  }
  PedonRecord$new(site = pedon$site, horizons = h)
}

# Numeric horizon columns worth perturbing: at least one non-NA value,
# excluding the geometry columns.
.perturbable_columns <- function(pedon) {
  h <- pedon$horizons
  cols <- setdiff(names(h), c("top_cm", "bottom_cm"))
  cols[vapply(cols, function(cn) {
    is.numeric(h[[cn]]) && any(!is.na(h[[cn]]))
  }, logical(1L))]
}
