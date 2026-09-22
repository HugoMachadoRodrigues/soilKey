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
#' @noRd
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
#' @noRd
aquult_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquult_qualifying_usda"
  res
}


#' Humult Suborder qualifier (Ultisols with thick humus accumulation)
#'
#' Passes when either criterion of KST 13ed key HB holds: (1) >= 0.9\% organic
#' carbon (weighted average) in the upper 15 cm of the argillic or kandic
#' horizon; or (2) >= 12 kg/m2 organic carbon between the mineral soil surface
#' and 100 cm. Criterion 1's 15 cm window is anchored at the illuvial onset
#' (the shallowest diagnostic layer whose clay exceeds the horizon directly
#' above it) so a transitional B with no clay increase cannot inflate it.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humult_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  # KST 13ed (Humults) qualifies via EITHER criterion:
  #   (1) >= 0.9% weighted-average OC in the upper 15 cm of the argillic/kandic
  #       horizon; OR
  #   (2) >= 12 kg/m2 OC between the surface and 100 cm.

  # --- Criterion 2: OC mass in 0-100 cm ---
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
  crit2 <- oc_mass >= 12

  # --- Criterion 1: 0.9% weighted-avg OC in upper 15 cm of argillic/kandic ---
  # The 15 cm window starts at the ILLUVIAL ONSET -- the shallowest diagnostic
  # layer whose clay exceeds the horizon immediately above it -- not at the
  # diagnostic's reported top. This avoids inflating the window with a
  # transitional B that has no clay increase, which argic()'s "min-above"
  # heuristic can include relative to a sandy A (e.g. A clay 6.7 -> E 16.6 ->
  # B 15.8 -> Bt 20.5: argic includes the B, but the true onset is the Bt).
  dx <- argillic_within_usda(pedon)
  dx_layers <- if (isTRUE(dx$passed)) dx$layers else integer(0)
  if (length(dx_layers) == 0L) {
    kd <- kandic_horizon_usda(pedon)
    dx_layers <- if (isTRUE(kd$passed)) kd$layers else integer(0)
  }
  oc_top15 <- NA_real_
  if (length(dx_layers) > 0L) {
    dx_layers <- sort(dx_layers)
    onset <- NA_integer_
    for (i in dx_layers) {
      if (i <= 1L) { onset <- i; break }
      ca <- h$clay_pct[i]; cb <- h$clay_pct[i - 1L]
      if (!is.na(ca) && !is.na(cb) && ca > cb) { onset <- i; break }
    }
    if (is.na(onset)) onset <- dx_layers[1L]
    z0 <- h$top_cm[onset]; z1 <- z0 + 15
    num <- 0; den <- 0
    for (i in seq_len(nrow(h))) {
      oc <- h$oc_pct[i]
      if (is.na(oc) || is.na(h$top_cm[i]) || is.na(h$bottom_cm[i])) next
      a <- max(h$top_cm[i], z0); b <- min(h$bottom_cm[i], z1)
      dz <- max(b - a, 0); if (dz <= 0) next
      num <- num + oc * dz; den <- den + dz
    }
    if (den > 0) oc_top15 <- num / den
  }
  crit1 <- !is.na(oc_top15) && oc_top15 >= 0.9

  passed <- isTRUE(crit1) || isTRUE(crit2)
  DiagnosticResult$new(
    name = "humult_qualifying_usda", passed = passed,
    layers = if (isTRUE(crit1)) dx_layers else cand,
    evidence = list(oc_mass_kg_m2 = oc_mass, mass_threshold = 12,
                    oc_pct_top15_dx = oc_top15, oc_pct_threshold = 0.9,
                    via_crit1 = isTRUE(crit1), via_crit2 = isTRUE(crit2)),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 15"
  )
}


#' Albic-over-argillic qualifying (Albaquults)
#' Pass when albic horizon overlies an argillic horizon directly.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#' @noRd
pale_qualifying_usda <- function(pedon) {
  res <- paleargid_qualifying_usda(pedon)
  res$name <- "pale_qualifying_usda"
  res
}


#' Kanhapl qualifying helper (Kanhapludults / Kanhaplustults / etc.)
#' Pass when kandic horizon present BUT NOT meeting Pale criteria
#' (i.e. younger / less developed kandic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
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
#' @noRd
plinth_subgroup_usda <- function(pedon, max_top_cm = 150) {
  res <- plinthic_subgroup_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "plinth_subgroup_usda"
  res
}


#' Albic Subgroup helper (Albaquultic / Albaquic)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
albic_subgroup_usda <- function(pedon) {
  res <- albic_horizon_usda(pedon)
  res$name <- "albic_subgroup_usda"
  res
}
