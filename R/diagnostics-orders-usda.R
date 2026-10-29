# ============================================================================
# USDA Soil Taxonomy 13th Ed. (2022) -- Order-level diagnostics (Cap 4
# Order Key, pp 65-72). 12 orders in canonical key order:
#   A. Gelisols    -- gelic conditions / permafrost
#   B. Histosols   -- organic materials >= 40 cm
#   C. Spodosols   -- spodic horizon
#   D. Andisols    -- andic soil properties >= 60% of thickness
#   E. Oxisols     -- oxic horizon + kandic + 40% clay (already wired
#                       via oxic_usda() in diagnostics-horizons-usda.R)
#   F. Vertisols   -- slickensides + cracks open/close
#   G. Aridisols   -- aridic moisture regime + ochric/anthropic
#   H. Ultisols    -- argillic/kandic + base saturation < 35%
#   I. Mollisols   -- mollic epipedon + base saturation >= 50%
#   J. Alfisols    -- argillic/kandic/natric horizon (BS >= 35%)
#   K. Inceptisols -- cambic / various subsurface diagnostics
#   L. Entisols    -- catch-all
#
# Each order's gate must enforce: (1) the order's diagnostic horizon /
# property; (2) the exclusion of all orders that come earlier in the
# key. This ensures the chave is mutually exclusive.
# ============================================================================


# Helper: estimate base saturation in argillic/kandic-like layers.
# v0.8 simplification: looks at horizon idx where designation matches
# Bt or Bk and returns mean bs_pct (NA-safe).
.argillic_bs_mean <- function(pedon) {
  h <- pedon$horizons
  arg <- argic(pedon, lithic_discontinuity = FALSE)
  layers <- arg$layers %||% integer(0)
  if (length(layers) == 0L) return(NA_real_)
  vals <- h$bs_pct[layers]
  if (all(is.na(vals))) return(NA_real_)
  mean(vals, na.rm = TRUE)
}


# v0.9.17: graceful fallback for the Ultisol BS-low criterion when
# bs_pct is missing but proxy evidence is conclusive. KST 13ed Ch 15
# (p 321) defines Ultisols as having BS < 35 % on a sum-of-cations
# basis in the argillic. The FEBR archive (and many legacy tropical
# pedon descriptions) reports al_sat_pct or pH but not BS. Rather
# than punish those profiles by falling through to Inceptisols, we
# infer BS-low from:
#   - al_sat_pct >= 50 in any argillic layer (high Al saturation
#     mathematically forces BS < 50, and BS < 35 in nearly all
#     tropical soils with this profile),
#   - pH_h2o < 5.0 in any argillic layer (the empirical threshold
#     below which BS exceeds 35 in fewer than 5 % of tropical
#     B horizons, per Embrapa / IUSS calibration tables).
# Both fall-backs are conservative: they only fire when the direct
# measurement is missing.
.bs_low_inferred <- function(pedon, bs_threshold = 35) {
  h <- pedon$horizons
  arg <- argic(pedon, lithic_discontinuity = FALSE)
  layers <- arg$layers %||% integer(0)
  if (length(layers) == 0L)
    return(list(bs_low = FALSE, source = "no_argic"))
  bs_vals <- h$bs_pct[layers]
  if (any(!is.na(bs_vals)))
    return(list(bs_low = isTRUE(mean(bs_vals, na.rm = TRUE) < bs_threshold),
                  source = "measured"))
  al_sat <- h$al_sat_pct[layers]
  if (any(!is.na(al_sat) & al_sat >= 50))
    return(list(bs_low = TRUE, source = "al_sat_ge_50"))
  ph <- h$ph_h2o[layers]
  if (any(!is.na(ph) & ph < 5.0))
    return(list(bs_low = TRUE, source = "ph_below_5"))
  list(bs_low = FALSE, source = "no_evidence")
}


# ---- A. Gelisols (Cap 9, p 189) -------------------------------------------

#' Gelisols (USDA Cap 9): gelic conditions / permafrost.
#'
#' Order-level gate: cryic_conditions diagnostic from WRB delegated +
#' optional permafrost_temp_C if available.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
gelisol_usda <- function(pedon) {
  h <- pedon$horizons
  cr <- cryic_conditions(pedon)
  cold <- if ("permafrost_temp_C" %in% names(h))
            any(!is.na(h$permafrost_temp_C) & h$permafrost_temp_C < 0)
          else FALSE
  passed <- isTRUE(cr$passed) || isTRUE(cold)
  DiagnosticResult$new(
    name = "gelisol_usda", passed = passed,
    layers = cr$layers, evidence = list(cryic = cr, cold_temp = cold),
    missing = cr$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 9 Gelisols (p 189)"
  )
}


# ---- B. Histosols (Cap 10, p 199) -----------------------------------------

#' Histosols (USDA Cap 10): organic materials >= 40 cm in 0-100.
#' Refined v0.8.4 -- now uses histosol_qualifying_usda (40 cm
#' threshold) instead of WRB histic_horizon (10 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
histosol_usda <- function(pedon) {
  hi <- histosol_qualifying_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- Histic / Folistic / Hemic / Sapric
  # / Fibric surveyor identification flips an NA histosol_qualifying
  # to TRUE.
  hi <- .apply_nasis_tiebreaker(hi, pedon,
                                 pattern       = "^Histic epipedon$|^Folistic epipedon$|^Hemic|^Sapric|^Fibric|^Limnic|^Coprogenous",
                                 feature_label = "Histic / Folistic / Hemic / Sapric / Fibric materials")
  ge <- gelisol_usda(pedon)
  passed <- isTRUE(hi$passed) && !isTRUE(ge$passed)
  DiagnosticResult$new(
    name = "histosol_usda", passed = passed,
    layers = hi$layers, evidence = list(histosol_qualifying = hi,
                                          gelisol_excluded = ge),
    missing = hi$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 10 Histosols (p 199)"
  )
}


# ---- C. Spodosols (Cap 14, p 311) -----------------------------------------

#' Spodosols (USDA Cap 14): spodic horizon (illuvial Al/Fe/OC).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
spodosol_usda <- function(pedon) {
  sp <- spodic(pedon)
  # v0.9.21: NASIS tie-breaker -- "Spodic horizon" or "Spodic
  # materials" surveyor identification flips an NA spodic to TRUE.
  sp <- .apply_nasis_tiebreaker(sp, pedon,
                                 pattern       = "^Spodic horizon$|^Spodic materials$|^Ortstein$|^Placic horizon$",
                                 feature_label = "Spodic horizon / materials")
  ex <- isTRUE(gelisol_usda(pedon)$passed) || isTRUE(histosol_usda(pedon)$passed)
  passed <- isTRUE(sp$passed) && !ex
  DiagnosticResult$new(
    name = "spodosol_usda", passed = passed,
    layers = sp$layers, evidence = list(spodic = sp, prior_order = ex),
    missing = sp$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 14 Spodosols (p 311)"
  )
}


# ---- D. Andisols (Cap 6, p 117) -------------------------------------------

#' Andisols (USDA Cap 6): andic soil properties >= 60\% of thickness.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
andisol_usda <- function(pedon) {
  # Refined v0.8.6: uses andisol_qualifying_usda which enforces the
  # 60% / 60 cm rule (KST 13ed, Ch 6 p 117) instead of just any
  # andic-property layer.
  aq <- andisol_qualifying_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- "Andic soil properties" or "Volcanic
  # glass" surveyor identification flips an NA andisol_qualifying to
  # TRUE.
  aq <- .apply_nasis_tiebreaker(aq, pedon,
                                 pattern       = "^Andic soil properties$|^Vitric|^Volcanic glass$",
                                 feature_label = "Andic soil properties / Volcanic glass")
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed)
  passed <- isTRUE(aq$passed) && !ex
  DiagnosticResult$new(
    name = "andisol_usda", passed = passed,
    layers = aq$layers,
    evidence = list(andisol_qualifying = aq, prior_order = ex),
    missing = aq$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 6 Andisols (p 117)"
  )
}


# ---- E. Oxisols already wired via oxic_usda() ------------------------------

#' Oxisols (USDA, KST 13ed Ch 8, key to soil orders, item E, pp. 68-69)
#'
#' v0.9.309: the text of the key. "Other soils that have either: 1. an oxic
#' horizon within 150 cm of the mineral soil surface and no kandic horizon
#' within that depth; or 2. 40 percent or more (by weight) clay in the
#' fine-earth fraction between the mineral soil surface and a depth of 18 cm
#' (after mixing) and a kandic horizon that has the weatherable-mineral
#' properties of an oxic horizon and has its upper boundary within 100 cm of
#' the mineral soil surface." Since v0.9.17 soilKey had used instead "no
#' argillic horizon starting above the oxic", which turned clayey Oxisols with
#' a kandic horizon into Mollisols, Ultisols or Alfisols (ISRIC's KE009, a
#' Rhodic Eutrustox, came out a Typic Paleustoll). soilKey does not record
#' weatherable minerals, and its oxic horizon does not test them either; the
#' kandic horizon of item 2 is read the same way.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
oxisol_usda <- function(pedon) {
  h <- pedon$horizons
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed) ||
          isTRUE(andisol_usda(pedon)$passed)
  ox <- oxic_usda(pedon)
  ka <- kandic_horizon_usda(pedon)
  ms <- .q_mineral_surface_cm(h); if (is.na(ms)) ms <- 0
  ox_top <- if (isTRUE(ox$passed) && length(ox$layers)) min(h$top_cm[ox$layers], na.rm = TRUE) else NA_real_
  ka_top <- if (isTRUE(ka$passed) && length(ka$layers)) min(h$top_cm[ka$layers], na.rm = TRUE) else NA_real_
  # The two items, for a kandic horizon present (starting at `kt`) or absent;
  # an undecided kandic horizon decides nothing when both answers agree.
  ox150 <- if (isTRUE(ox$passed)) !is.na(ox_top) && ox_top <= ms + 150 else if (is.na(ox$passed)) NA else FALSE
  clay18 <- .q_weighted_mean(h$clay_pct, h$top_cm, h$bottom_cm, ms, ms + 18)
  covered <- sum(pmax(0, pmin(h$bottom_cm, ms + 18) - pmax(h$top_cm, ms))[!is.na(h$clay_pct)], na.rm = TRUE)
  clay_ok <- if (is.na(clay18) || covered < 18 - 1e-6) NA else clay18 >= 40
  and3 <- function(a, b) if (isFALSE(a) || isFALSE(b)) FALSE else if (isTRUE(a) && isTRUE(b)) TRUE else NA
  or3 <- function(a, b) if (isTRUE(a) || isTRUE(b)) TRUE else if (isFALSE(a) && isFALSE(b)) FALSE else NA
  items <- function(has_kandic, kt) {
    r1 <- and3(ox150, if (has_kandic) !(kt <= ms + 150) else TRUE)   # oxic, no kandic within 150 cm
    r2 <- and3(clay_ok, if (has_kandic) kt <= ms + 100 else FALSE)   # clayey surface, kandic within 100 cm
    list(r1 = r1, r2 = r2, any = or3(r1, r2))
  }
  if (isTRUE(ka$passed)) {
    it <- items(TRUE, ka_top); route <- it$any
  } else if (isFALSE(ka$passed)) {
    it <- items(FALSE, NA); route <- it$any
  } else {
    # undecided: a kandic horizon might start at any of the undecided
    # boundaries, or be absent; decide when every case gives the same answer
    kts <- ka$evidence$candidate_tops_cm %||% numeric(0)
    cases <- c(list(items(FALSE, NA)), lapply(kts, function(kt) items(TRUE, kt)))
    anys <- vapply(cases, function(x) if (is.na(x$any)) NA else x$any, NA)
    route <- if (!length(kts)) NA else if (all(anys %in% TRUE)) TRUE
             else if (all(anys %in% FALSE)) FALSE else NA
    it <- list(r1 = NA, r2 = NA)
  }
  r1 <- it$r1; r2 <- it$r2
  passed <- if (ex) FALSE else route
  DiagnosticResult$new(
    name = "oxisol_usda", passed = passed,
    layers = if (isTRUE(r1)) ox$layers else if (isTRUE(r2)) ka$layers else integer(0),
    evidence = list(oxic = ox, kandic = ka, prior_order = ex,
                    item1_oxic_no_kandic = r1, item2_clayey_surface_kandic = r2,
                    clay_0_18_pct = clay18),
    missing = if (is.na(passed)) unique(c(ox$missing, ka$missing, if (is.na(clay_ok)) "clay_pct")) else character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 8, key to soil orders, E (pp. 68-69)"
  )
}


# ---- F. Vertisols (Cap 16, p 343) ------------------------------------------

#' Vertisols (USDA Cap 16): slickensides + cracks.
#' Delegates to vertic_horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vertisol_usda <- function(pedon) {
  # v0.9.311: the USDA reading (slickensides and cracks, or the KST 13 LE
  # path); vertic_horizon() itself now follows WRB 2022 Ch 3.1.40.
  vh <- vertic_horizon(pedon, reading = "usda")
  # v0.9.21: NASIS tie-breaker -- Slickensides surveyor
  # identification flips an NA vertic_horizon to TRUE (when no COLE
  # or cracks measurement is available).
  vh <- .apply_nasis_tiebreaker(vh, pedon,
                                 pattern       = "^Slickensides$|^Vertic|^Gilgai$",
                                 feature_label = "Slickensides / Vertic features / Gilgai")
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed) ||
          isTRUE(andisol_usda(pedon)$passed) ||
          isTRUE(oxisol_usda(pedon)$passed)
  passed <- isTRUE(vh$passed) && !ex
  DiagnosticResult$new(
    name = "vertisol_usda", passed = passed,
    layers = vh$layers, evidence = list(vertic = vh, prior_order = ex),
    missing = vh$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 16 Vertisols (p 343)"
  )
}


# ---- G. Aridisols (Cap 7, p 137) -------------------------------------------

#' Aridisols (USDA Cap 7): aridic moisture regime + ochric/anthropic +
#' subsurface diagnostic. v0.8 simplification: detected via aridity
#' proxies (low EC OR salic OR caracter combinations) + non-mollic
#' surface + low OC (no organic accumulation).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aridisol_usda <- function(pedon) {
  # Refined v0.8.9: uses aridisol_qualifying_usda which enforces
  # aridic SMR + a diagnostic subsurface horizon (KST 13ed Ch 7
  # p 137). Fallback to aridity proxies when SMR not provided.
  aq <- aridisol_qualifying_usda(pedon)
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed) ||
          isTRUE(andisol_usda(pedon)$passed) ||
          isTRUE(oxisol_usda(pedon)$passed) ||
          isTRUE(vertisol_usda(pedon)$passed)
  passed <- isTRUE(aq$passed) && !ex
  DiagnosticResult$new(
    name = "aridisol_usda", passed = passed,
    layers = aq$layers,
    evidence = list(aridisol_qualifying = aq, prior_order = ex),
    missing = aq$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 7 Aridisols (p 137)"
  )
}


# ---- H. Ultisols (Cap 15, p 321) -------------------------------------------

#' Ultisols (USDA Cap 15): argillic/kandic horizon + base saturation < 35\%.
#'
#' v0.9.17 graceful BS handling: when \code{bs_pct} is missing in the
#' argillic layers, the diagnostic falls back to two equivalent
#' indirect criteria before failing:
#' \itemize{
#'   \item \code{al_sat_pct >= 50} (high Al saturation mathematically
#'         forces BS < 50, and BS < 35 in essentially all tropical
#'         soils with this profile);
#'   \item \code{ph_h2o < 5.0} (the empirical threshold below which BS
#'         exceeds 35 in fewer than 5 % of tropical B horizons).
#' }
#' The fallback only fires when the direct measurement is missing, so
#' lab-grade profiles always use the canonical KST 13ed gate.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
ultisol_usda <- function(pedon) {
  ar <- argic(pedon, lithic_discontinuity = FALSE)
  # v0.9.21: NASIS tie-breaker for argic when canonical gate is NA.
  ar <- .apply_nasis_tiebreaker(ar, pedon,
                                 pattern       = "^Argillic horizon$|^Kandic horizon$",
                                 feature_label = "Argillic / Kandic horizon")
  bs <- .argillic_bs_mean(pedon)
  bs_inf <- .bs_low_inferred(pedon, bs_threshold = 35)
  bs_low <- isTRUE(bs_inf$bs_low)
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed)
  ))
  passed <- isTRUE(ar$passed) && isTRUE(bs_low) && !ex
  DiagnosticResult$new(
    name = "ultisol_usda", passed = passed,
    layers = ar$layers,
    evidence = list(argic = ar, bs_mean = bs, bs_low = bs_low,
                     bs_low_source = bs_inf$source,
                     prior_order = ex),
    missing = ar$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 15 Ultisols (p 321)"
  )
}


# ---- I. Mollisols (Cap 12, p 247) ------------------------------------------

#' Mollisols (USDA Cap 12): mollic epipedon + base saturation >= 50\%.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
mollisol_usda <- function(pedon) {
  # v0.9.10: switched the mollic gate from the WRB `mollic()` (a v0.2
  # scaffold leftover) to the USDA-native `mollic_epipedon_usda()`,
  # which carries the artefact-rich exclusion in v0.9.10 and the
  # full KST 13ed Ch. 3 thickness / colour / OC / BS / structure
  # contract. This is the fix that prevents Technosol fixtures
  # (artefacts >= 20 % in the surface A) from being routed to
  # Hapludolls.
  m <- mollic_epipedon_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- when the canonical lab + morphology
  # gate returns NA (insufficient data), the surveyor's
  # pediagfeatures.featkind = "Mollic epipedon" identification flips
  # the result to TRUE. Does NOT override TRUE / FALSE.
  m <- .apply_nasis_tiebreaker(m, pedon,
                                pattern       = "^Mollic epipedon$",
                                feature_label = "Mollic epipedon")
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed),
    isTRUE(ultisol_usda(pedon)$passed)
  ))
  passed <- isTRUE(m$passed) && !ex
  DiagnosticResult$new(
    name = "mollisol_usda", passed = passed,
    layers = m$layers, evidence = list(mollic = m, prior_order = ex),
    missing = m$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 12 Mollisols (p 247)"
  )
}


# ---- J. Alfisols (Cap 5, p 73) ---------------------------------------------

#' Alfisols (USDA Cap 5): argillic/kandic/natric horizon + base saturation
#' >= 35\% at the implicit reference depth.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
alfisol_usda <- function(pedon) {
  ar <- argillic_usda(pedon)
  # v0.9.21: NASIS tie-breaker -- argillic / kandic / natric horizon
  # surveyor identification flips an NA argillic_usda to TRUE.
  ar <- .apply_nasis_tiebreaker(ar, pedon,
                                 pattern       = "^Argillic horizon$|^Kandic horizon$|^Natric horizon$",
                                 feature_label = "Argillic / Kandic / Natric horizon")
  bs <- .argillic_bs_mean(pedon)
  bs_ok <- is.na(bs) || bs >= 35
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed),
    isTRUE(ultisol_usda(pedon)$passed),
    isTRUE(mollisol_usda(pedon)$passed)
  ))
  passed <- isTRUE(ar$passed) && isTRUE(bs_ok) && !ex
  DiagnosticResult$new(
    name = "alfisol_usda", passed = passed,
    layers = ar$layers,
    evidence = list(argillic = ar, bs_mean = bs, bs_ok = bs_ok,
                     prior_order = ex),
    missing = ar$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 5 Alfisols (p 73)"
  )
}


# ---- K. Inceptisols (Cap 11, p 207) ----------------------------------------

#' Inceptisols (USDA Cap 11): cambic horizon (or several alternative
#' subsurface diagnostics: folistic/histic/mollic with thin sub, salic,
#' sodium-affected sub).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
inceptisol_usda <- function(pedon) {
  cb <- cambic(pedon)
  # v0.9.21: NASIS tie-breaker -- cambic horizon surveyor
  # identification flips an NA cambic to TRUE.
  cb <- .apply_nasis_tiebreaker(cb, pedon,
                                 pattern       = "^Cambic horizon$",
                                 feature_label = "Cambic horizon")
  ex <- any(c(
    isTRUE(gelisol_usda(pedon)$passed),
    isTRUE(histosol_usda(pedon)$passed),
    isTRUE(spodosol_usda(pedon)$passed),
    isTRUE(andisol_usda(pedon)$passed),
    isTRUE(oxisol_usda(pedon)$passed),
    isTRUE(vertisol_usda(pedon)$passed),
    isTRUE(aridisol_usda(pedon)$passed),
    isTRUE(ultisol_usda(pedon)$passed),
    isTRUE(mollisol_usda(pedon)$passed),
    isTRUE(alfisol_usda(pedon)$passed)
  ))
  passed <- isTRUE(cb$passed) && !ex
  DiagnosticResult$new(
    name = "inceptisol_usda", passed = passed,
    layers = cb$layers, evidence = list(cambic = cb, prior_order = ex),
    missing = cb$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 11 Inceptisols (p 207)"
  )
}


# ---- L. Entisols (Cap 8, p 165) - catch-all -----------------------------

#' Entisols (USDA Cap 8): catch-all for soils that don't match any
#' other Order. Always passes.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
entisol_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "entisol_usda", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE),
    missing = character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 8 Entisols (p 165) -- catch-all"
  )
}
