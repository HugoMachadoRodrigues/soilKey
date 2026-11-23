# =============================================================
# USDA Soil Taxonomy 13ed -- Alfisols helpers (Cap 5, pp 73-115)
# =============================================================
#
# Alfisols are soils with an argillic, kandic, or natric horizon (or a
# fragipan with thick clay films) that are not Ultisols or Mollisols; the
# order key itself has no base-saturation test (KST 13th ed., p. 70).
# =============================================================


#' Alfisol Order qualifier
#' Pass when argillic OR kandic horizon present + BS >= 35\% in some part.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
alfisol_qualifying_usda <- function(pedon) {
  ar <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  if (!isTRUE(ar$passed)) {
    return(DiagnosticResult$new(
      name = "alfisol_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no argillic or kandic"),
      missing = ar$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 5"
    ))
  }
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 200)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  high_bs <- any(!is.na(bs) & bs >= 35)
  passed <- isTRUE(high_bs)
  DiagnosticResult$new(
    name = "alfisol_qualifying_usda", passed = passed,
    layers = ar$layers,
    evidence = list(argic_or_kandic = ar, high_bs = high_bs),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 5"
  )
}


#' Aqualf Suborder qualifier (aquic conditions in argillic Alfisol).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aqualf_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aqualf_qualifying_usda"
  res
}


#' Glossic Subgroup helper (Glossaqualfs, Glossocryalfs, Glossudalfs)
#' Pass when interfingering of albic materials into argillic horizon
#' is detected. v0.8 proxy: albic + argillic + lateral chroma <= 2
#' on argillic boundary.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
glossic_subgroup_usda <- function(pedon) {
  al <- albic(pedon)
  arg <- argillic_within_usda(pedon, max_top_cm = 200)
  passed <- isTRUE(al$passed) && isTRUE(arg$passed)
  if (passed) {
    h <- pedon$horizons
    al_top <- min(h$top_cm[al$layers], na.rm = TRUE)
    arg_top <- min(h$top_cm[arg$layers], na.rm = TRUE)
    passed <- al_top <= arg_top
  }
  DiagnosticResult$new(
    name = "glossic_subgroup_usda", passed = passed,
    layers = c(al$layers, arg$layers),
    evidence = list(albic = al, argillic = arg),
    missing = unique(c(al$missing %||% character(0),
                          arg$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 5"
  )
}


#' Ferric Subgroup helper (Ferrudalfs)
#'
#' v0.9.420 (audit item G3): the Ferrudalfs of the KST (13th ed., p. 86), a
#' glossic horizon and, in the argillic or kandic horizon, discrete nodules
#' 2.5 to 30 cm across, enriched with iron, with exteriors redder or of
#' higher chroma than the interiors. Read as \code{glossic_subgroup_usda()}
#' and \code{ferric_nodules_pct} above 0 in the argillic or kandic horizon;
#' the nodules are a field observation, absent when not recorded. Until
#' v0.9.419 it passed on Fe-DCB of 4\% or more in any B horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
ferric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  gl <- glossic_subgroup_usda(pedon)
  ar <- argillic_usda(pedon)
  ka <- kandic_horizon_usda(pedon)
  L <- unique(c(if (isTRUE(ar$passed)) ar$layers, if (isTRUE(ka$passed)) ka$layers))
  nod <- h$ferric_nodules_pct %||% rep(NA_real_, nrow(h))
  has_nod <- length(L) > 0L && any(!is.na(nod[L]) & nod[L] > 0)
  nodules <- if (has_nod) TRUE else if (length(L)) FALSE
             else if (isFALSE(.usda_or3(ar$passed, ka$passed))) FALSE else NA
  passed <- .and3(gl$passed, nodules)
  DiagnosticResult$new(
    name = "ferric_subgroup_usda", passed = passed,
    layers = if (isTRUE(passed)) L[!is.na(nod[L]) & nod[L] > 0] else integer(0),
    evidence = list(glossic = gl, iron_nodules = nodules),
    missing = if (is.na(passed)) unique(c(gl$missing, ar$missing, ka$missing)) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 5, Ferrudalfs (p. 86)"
  )
}


#' Ultic Subgroup helper: argillic or kandic (any BS).
#'
#' v0.9.420 (audit item G4): with \code{alfisol = TRUE}, the Ultic subgroups
#' of the Alfisols (e.g. Ultic Paleustalfs, KST 13th ed., p. 109): an
#' argillic horizon (the kandic horizon when there is no argillic one) with a
#' base saturation (by sum of cations; NH4OAc stands in where it is not
#' recorded, audit reading L18) of less than 75\% throughout. The default
#' keeps the earlier reading (argillic or kandic horizon), which the
#' Spodosol, Mollisol and Andisol subgroups use.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param alfisol The reading of the Alfisol subgroups.
#' @noRd
ultic_subgroup_usda <- function(pedon, alfisol = FALSE) {
  if (!isTRUE(alfisol)) {
    res <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
    res$name <- "ultic_subgroup_usda"
    return(res)
  }
  hz <- argillic_usda(pedon)
  if (!isTRUE(hz$passed)) {
    ka <- kandic_horizon_usda(pedon)
    if (isTRUE(ka$passed)) hz <- ka
    else hz$passed <- .usda_or3(hz$passed, ka$passed)
  }
  b <- .bs_sum_cations_usda(pedon)
  passed <- if (!isTRUE(hz$passed)) hz$passed else {
    bs <- b$bs[hz$layers]
    if (any(!is.na(bs) & .kst_round(bs) >= 75)) FALSE else if (anyNA(bs)) NA else TRUE
  }
  DiagnosticResult$new(
    name = "ultic_subgroup_usda", passed = passed,
    layers = if (isTRUE(passed)) hz$layers else integer(0),
    evidence = list(horizon = hz$name, base_saturation = if (isTRUE(hz$passed)) b$bs[hz$layers],
                    source = if (isTRUE(hz$passed)) unique(b$source[hz$layers])),
    missing = if (is.na(passed)) unique(c(hz$missing, "bs_pct")) else character(0),
    reference = "Soil Survey Staff (2022), KST 13th ed., Ch. 5, Ultic subgroups of the Alfisols"
  )
}
