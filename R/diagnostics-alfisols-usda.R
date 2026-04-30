# =============================================================
# USDA Soil Taxonomy 13ed -- Alfisols helpers (Cap 5, pp 73-115)
# =============================================================
#
# Alfisols are soils with an argillic, kandic, or natric horizon
# AND base saturation >= 35\% (sum-of-cations) in some part. The
# main distinction from Ultisols is the higher base saturation.
# =============================================================


#' Alfisol Order qualifier
#' Pass when argillic OR kandic horizon present + BS >= 35% in some part.
#' @export
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
#' @export
aqualf_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aqualf_qualifying_usda"
  res
}


#' Glossic Subgroup helper (Glossaqualfs, Glossocryalfs, Glossudalfs)
#' Pass when interfingering of albic materials into argillic horizon
#' is detected. v0.8 proxy: albic + argillic + lateral chroma <= 2
#' on argillic boundary.
#' @export
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
#' Pass when iron-rich (fe_dcb_pct >= 4%) horizon present in B.
#' @export
ferric_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$designation) & grepl("^B", h$designation))
  fe <- h$fe_dcb_pct[cand]
  miss <- if (all(is.na(fe))) "fe_dcb_pct" else character(0)
  passing <- cand[!is.na(fe) & fe >= 4]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "ferric_subgroup_usda", passed = passed, layers = passing,
    evidence = list(threshold = 4),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 5"
  )
}
