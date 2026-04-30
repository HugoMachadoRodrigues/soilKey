# =============================================================
# USDA Soil Taxonomy 13ed -- Mollisols helpers (Cap 12, pp 247-294)
# =============================================================
#
# Mollisols have a mollic epipedon AND base saturation >= 50%.
# 8 Suborders: Albolls, Aquolls, Rendolls, Gelolls, Cryolls,
#              Udolls, Ustolls, Xerolls.
# =============================================================


#' Mollisol Order qualifier (USDA, KST 13ed, Ch 12)
#' Pass when mollic_epipedon AND BS (NH4OAc) >= 50% in upper 100 cm.
#' @export
mollisol_qualifying_usda <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  if (!isTRUE(mo$passed)) {
    return(DiagnosticResult$new(
      name = "mollisol_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no mollic epipedon"),
      missing = mo$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 12"
    ))
  }
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  high_bs <- !all(is.na(bs)) && all(is.na(bs) | bs >= 50)
  passed <- isTRUE(high_bs)
  DiagnosticResult$new(
    name = "mollisol_qualifying_usda", passed = passed,
    layers = mo$layers,
    evidence = list(mollic = mo, high_bs = high_bs),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 12"
  )
}


#' Albolls qualifier: mollic + albic + argillic.
#' @export
alboll_qualifying_usda <- function(pedon) {
  al <- albic(pedon)
  arg <- argillic_within_usda(pedon, max_top_cm = 200)
  passed <- isTRUE(al$passed) && isTRUE(arg$passed)
  DiagnosticResult$new(
    name = "alboll_qualifying_usda", passed = passed,
    layers = c(al$layers, arg$layers),
    evidence = list(albic = al, argillic = arg),
    missing = unique(c(al$missing %||% character(0),
                          arg$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 12"
  )
}


#' Aquolls qualifier (aquic conditions in mollic).
#' @export
aquoll_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquoll_qualifying_usda"
  res
}


#' Rendolls qualifier: shallow soil over carbonate parent material.
#' Pass when CaCO3 >= 40\% in subsurface AND profile depth < 100 cm
#' to a contact.
#' @export
rendoll_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  cas <- h$caco3_pct[cand]
  miss <- if (all(is.na(cas))) "caco3_pct" else character(0)
  has_carbonate <- any(!is.na(cas) & cas >= 40)
  has_lithic <- isTRUE(lithic_contact_usda(pedon, max_top_cm = 100)$passed)
  passed <- has_carbonate && has_lithic
  DiagnosticResult$new(
    name = "rendoll_qualifying_usda", passed = passed,
    layers = cand[!is.na(cas) & cas >= 40],
    evidence = list(has_carbonate = has_carbonate,
                      has_lithic = has_lithic),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 12"
  )
}


#' Vermic Subgroup helper (Vermudolls / Vermustolls)
#' Pass when worm_holes_pct >= 50\% in some horizon (KST 13ed worm
#' burrow criterion).
#' @export
vermic_subgroup_usda <- function(pedon) {
  h <- pedon$horizons
  wh <- h$worm_holes_pct
  miss <- if (all(is.na(wh))) "worm_holes_pct" else character(0)
  passing <- which(!is.na(wh) & wh >= 50)
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "vermic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(threshold_pct = 50),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 12"
  )
}


#' Argic Mollisol Suborder helper -- delegates argillic_within_usda.
#' @export
argic_mollisol_usda <- function(pedon) {
  res <- argillic_within_usda(pedon, max_top_cm = 200)
  res$name <- "argic_mollisol_usda"
  res
}
