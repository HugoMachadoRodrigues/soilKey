# =============================================================
# USDA Soil Taxonomy 13ed -- Ultisols helpers (Cap 15, pp 321-342)
# =============================================================
#
# Ultisols are soils with an argillic, kandic, or kandilic horizon
# AND base saturation < 35\% in some part. 5 Suborders by SMR/aquic.
# =============================================================


#' Ultisol Order qualifier (USDA, KST 13ed, Ch 2)
#' Pass when argillic OR kandic horizon present + BS < 35\% in some
#' part of the upper 200 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
ultisol_qualifying_usda <- function(pedon) {
  ar <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  if (!isTRUE(ar$passed)) {
    return(DiagnosticResult$new(
      name = "ultisol_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no argillic or kandic"),
      missing = ar$missing,
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
    ))
  }
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 200)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  low_bs <- any(!is.na(bs) & bs < 35)
  passed <- isTRUE(low_bs)
  DiagnosticResult$new(
    name = "ultisol_qualifying_usda", passed = passed,
    layers = ar$layers,
    evidence = list(argic_or_kandic = ar, low_bs = low_bs),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
  )
}


#' Aquult Suborder qualifier
#' Pass when aquic_conditions within 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
aquult_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquult_qualifying_usda"
  res
}


#' Humult Suborder qualifier (Ultisols with thick humus accumulation)
#' Pass when 0.9\% OC weighted average in 0-15 cm AND/OR
#' organic carbon mass >= 12 kg/m2 in 0-100 cm (proxy via humic_oxisol_usda
#' with lower threshold).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
humult_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  oc_mass <- 0
  for (i in cand) {
    oc <- h$oc_pct[i]
    bd <- h$bulk_density_g_cm3[i] %||% 1.2
    if (is.na(bd)) bd <- 1.2
    if (is.na(oc)) next
    top <- max(h$top_cm[i], 0)
    bot <- min(h$bottom_cm[i], 100)
    dz <- pmax(bot - top, 0)
    oc_mass <- oc_mass + (oc / 100) * bd * dz * 10
  }
  passed <- oc_mass >= 12
  DiagnosticResult$new(
    name = "humult_qualifying_usda", passed = passed,
    layers = cand,
    evidence = list(oc_mass_kg_m2 = oc_mass, threshold = 12),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
  )
}


#' Albic-over-argillic qualifying (Albaquults)
#' Pass when albic horizon overlies an argillic horizon directly.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
albaquult_qualifying_usda <- function(pedon) {
  al <- albic(pedon)
  arg <- argillic_within_usda(pedon, max_top_cm = 100)
  passed <- isTRUE(al$passed) && isTRUE(arg$passed)
  if (passed) {
    h <- pedon$horizons
    al_top <- min(h$top_cm[al$layers], na.rm = TRUE)
    arg_top <- min(h$top_cm[arg$layers], na.rm = TRUE)
    passed <- al_top < arg_top
  }
  DiagnosticResult$new(
    name = "albaquult_qualifying_usda", passed = passed,
    layers = c(al$layers, arg$layers),
    evidence = list(albic = al, argillic = arg),
    missing = unique(c(al$missing %||% character(0),
                          arg$missing %||% character(0))),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
  )
}


#' Pale qualifying helper (Paleudults / Paleustults / Palexerults /
#' Palehumults / Paleaquults)
#'
#' Pass when an argillic horizon has either:
#' \itemize{
#'   \item clay >= 35\% in upper 30 cm of argillic; OR
#'   \item lithologic discontinuity NOT followed by argic; OR
#'   \item argillic that does NOT decrease in clay >= 20\% relative
#'         from its maximum.
#' }
#' v0.8 proxy: clay_pct >= 35\% in upper argillic.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
pale_qualifying_usda <- function(pedon) {
  res <- paleargid_qualifying_usda(pedon)
  res$name <- "pale_qualifying_usda"
  res
}


#' Kanhapl qualifying helper (Kanhapludults / Kanhaplustults / etc.)
#' Pass when kandic horizon present BUT NOT meeting Pale criteria
#' (i.e. younger / less developed kandic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
kanhapl_qualifying_usda <- function(pedon) {
  ka <- kandic_horizon_usda(pedon)
  pa <- pale_qualifying_usda(pedon)
  passed <- isTRUE(ka$passed) && !isTRUE(pa$passed)
  DiagnosticResult$new(
    name = "kanhapl_qualifying_usda", passed = passed,
    layers = ka$layers,
    evidence = list(kandic = ka, pale = pa),
    missing = ka$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
  )
}


#' Plinth qualifying helper (Plinth*ults)
#' Pass when plinthite >= 5\% in 50\%+ of layers within 150 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @export
plinth_subgroup_usda <- function(pedon, max_top_cm = 150) {
  res <- plinthic_subgroup_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "plinth_subgroup_usda"
  res
}


#' Albic Subgroup helper (Albaquultic / Albaquic)
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
albic_subgroup_usda <- function(pedon) {
  res <- albic_horizon_usda(pedon)
  res$name <- "albic_subgroup_usda"
  res
}
