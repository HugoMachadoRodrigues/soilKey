# =============================================================
# USDA Soil Taxonomy 13ed -- Inceptisols helpers (Cap 11, pp 207-246)
# =============================================================
#
# Inceptisols are weakly developed soils with a cambic horizon (or
# cambic + a few other mild diagnostic features). 6 Suborders.
# =============================================================


#' Inceptisol Order qualifier
#' Pass when a cambic horizon is present (no argillic, no spodic,
#' no mollic, etc. -- enforced by prior order exclusion).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
inceptisol_qualifying_usda <- function(pedon) {
  cb <- cambic(pedon)
  res <- DiagnosticResult$new(
    name = "inceptisol_qualifying_usda", passed = isTRUE(cb$passed),
    layers = cb$layers,
    evidence = list(cambic = cb),
    missing = cb$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11, p 207"
  )
  res
}


#' Aquept Suborder qualifier
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aquept_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquept_qualifying_usda"
  res
}


#' Halic helper for Halaquepts
#' Pass when EC >= 8 dS/m within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
halaquept_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  ec <- h$ec_dS_m[cand]
  miss <- if (all(is.na(ec))) "ec_dS_m" else character(0)
  passing <- cand[!is.na(ec) & ec >= 8]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "halaquept_qualifying_usda", passed = passed, layers = passing,
    evidence = list(threshold_dS_m = 8),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}


#' Densiaquept qualifying (densic contact within 100 cm)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
densiaquept_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100 &
                  !is.na(h$rupture_resistance) &
                  tolower(h$rupture_resistance) %in%
                    c("very firm", "extremely firm"))
  passed <- length(cand) > 0L
  DiagnosticResult$new(
    name = "densiaquept_qualifying_usda", passed = passed, layers = cand,
    evidence = list(note = "v0.8 proxy: very firm/extremely firm rupture-resistance"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 39"
  )
}


#' Eutric Inceptisol Suborder helper (Eutrudepts)
#' Pass when BS (NH4OAc) >= 60\% in some part of upper 75 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_bs Numeric threshold or option (see Details).
#' @noRd
eutric_inceptisol_usda <- function(pedon, min_bs = 60) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 75)
  bs <- h$bs_pct[cand]
  miss <- if (all(is.na(bs))) "bs_pct" else character(0)
  passed <- any(!is.na(bs) & bs >= min_bs)
  DiagnosticResult$new(
    name = "eutric_inceptisol_usda", passed = passed,
    layers = cand[!is.na(bs) & bs >= min_bs],
    evidence = list(threshold = min_bs),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}


#' Humic Inceptisol Suborder helper (Hum*)
#' Pass when umbric or mollic epipedon present + thick (>= 25 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
humic_inceptisol_usda <- function(pedon) {
  mo <- mollic_epipedon_usda(pedon)
  um <- umbric_epipedon_usda(pedon)
  passed <- isTRUE(mo$passed) || isTRUE(um$passed)
  DiagnosticResult$new(
    name = "humic_inceptisol_usda", passed = passed,
    layers = unique(c(mo$layers, um$layers)),
    evidence = list(mollic = mo, umbric = um),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11"
  )
}


#' Humic colour-value intergrade (KST 13ed) — Hum* Inceptisol subgroups
#'
#' The "Humic" subgroup differentia for Udept / Xerept / Ustept great groups
#' (verbatim, e.g. KST-13 Ch. 11 Humic Eutrudepts): a colour value, moist, of
#' \code{max_value_moist} (3) or less AND a colour value, dry, of
#' \code{max_value_dry} (5) or less (crushed and smoothed sample) throughout the
#' upper \code{depth_cm} (18) cm of the mineral soil. This is the dark-coloured
#' intergrade that does NOT reach an umbric / mollic epipedon (so the order has
#' already keyed without one) — distinct from \code{humic_inceptisol_usda}
#' (the epipedon-based suborder helper). Conservative: requires BOTH the moist
#' and dry value recorded for every layer overlapping the window (a missing dry
#' value cannot confirm the criterion), so it never over-fires on a dark A alone.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_value_moist,max_value_dry Colour-value ceilings (default 3 / 5).
#' @param depth_cm Top-of-soil window in cm (default 18).
#' @return A \code{\link{DiagnosticResult}}.
#' @noRd
humic_colour_usda <- function(pedon, max_value_moist = 3, max_value_dry = 5,
                              depth_cm = 18) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < depth_cm)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "humic_colour_usda", passed = FALSE, layers = integer(0),
      evidence = list(reason = "no layer within the upper window"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11 (Humic)"
    ))
  }
  vm <- h$munsell_value_moist[cand]
  vd <- h$munsell_value_dry[cand]
  miss <- character(0)
  if (any(is.na(vm))) miss <- c(miss, "munsell_value_moist")
  if (any(is.na(vd))) miss <- c(miss, "munsell_value_dry")
  # "throughout the upper 18 cm": EVERY overlapping mineral layer must be dark
  # in BOTH moist and dry value. Missing -> not confirmed (conservative).
  vm_ok <- !is.na(vm) & vm <= max_value_moist
  vd_ok <- !is.na(vd) & vd <= max_value_dry
  passed <- all(vm_ok) && all(vd_ok)
  DiagnosticResult$new(
    name = "humic_colour_usda",
    passed = passed,
    layers = if (passed) cand else integer(0),
    evidence = list(window_cm = depth_cm, n_layers = length(cand),
                    value_moist = vm, value_dry = vd),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 11 (Humic subgroups)"
  )
}
