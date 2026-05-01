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
  arg <- argic(pedon)
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
  arg <- argic(pedon)
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
#' @export
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
#' @export
histosol_usda <- function(pedon) {
  hi <- histosol_qualifying_usda(pedon)
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
#' @export
spodosol_usda <- function(pedon) {
  sp <- spodic(pedon)
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
#' @export
andisol_usda <- function(pedon) {
  # Refined v0.8.6: uses andisol_qualifying_usda which enforces the
  # 60% / 60 cm rule (KST 13ed, Ch 6 p 117) instead of just any
  # andic-property layer.
  aq <- andisol_qualifying_usda(pedon)
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

#' Oxisol (USDA Cap 13): oxic horizon, excluding profiles with an
#' argillic horizon overlying the oxic.
#'
#' v0.9.17 fix: KST 13ed Ch 13 (p 295) excludes from Oxisols any
#' profile whose argillic horizon's upper boundary lies within 100 cm
#' of the surface AND whose argillic base lies within 30 cm of the
#' upper boundary of the oxic. Operationally we use the simpler and
#' more defensible "argillic above oxic" check: if argillic exists
#' and starts strictly shallower than the oxic, the profile is NOT
#' an Oxisol (route to Ultisols / Alfisols instead). The previous
#' v0.8 implementation lacked this exclusion and was responsible for
#' misclassifying 144 Embrapa FEBR Ultisols as Oxisols in the
#' v0.9.16 benchmark.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
oxisol_usda <- function(pedon) {
  ox <- oxic_usda(pedon)
  ex <- isTRUE(gelisol_usda(pedon)$passed) ||
          isTRUE(histosol_usda(pedon)$passed) ||
          isTRUE(spodosol_usda(pedon)$passed) ||
          isTRUE(andisol_usda(pedon)$passed)
  passed <- isTRUE(ox$passed) && !ex

  # v0.9.17 argillic-above-oxic exclusion.
  argillic_above <- FALSE
  if (passed) {
    ar <- argillic_usda(pedon)
    if (isTRUE(ar$passed) && length(ar$layers) > 0L &&
          length(ox$layers) > 0L) {
      h <- pedon$horizons
      argillic_top <- min(h$top_cm[ar$layers], na.rm = TRUE)
      oxic_top     <- min(h$top_cm[ox$layers], na.rm = TRUE)
      if (!is.na(argillic_top) && !is.na(oxic_top) &&
            argillic_top < oxic_top) {
        argillic_above <- TRUE
        passed <- FALSE
      }
    }
  }

  DiagnosticResult$new(
    name = "oxisol_usda", passed = passed,
    layers = ox$layers,
    evidence = list(oxic = ox, prior_order = ex,
                      argillic_above_oxic = argillic_above),
    missing = ox$missing,
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 13 Oxisols (p 295)"
  )
}


# ---- F. Vertisols (Cap 16, p 343) ------------------------------------------

#' Vertisols (USDA Cap 16): slickensides + cracks.
#' Delegates to vertic_horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
vertisol_usda <- function(pedon) {
  vh <- vertic_horizon(pedon)
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
#' @export
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
#' @export
ultisol_usda <- function(pedon) {
  ar <- argic(pedon)
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
#' @export
mollisol_usda <- function(pedon) {
  # v0.9.10: switched the mollic gate from the WRB `mollic()` (a v0.2
  # scaffold leftover) to the USDA-native `mollic_epipedon_usda()`,
  # which carries the artefact-rich exclusion in v0.9.10 and the
  # full KST 13ed Ch. 3 thickness / colour / OC / BS / structure
  # contract. This is the fix that prevents Technosol fixtures
  # (artefacts >= 20 % in the surface A) from being routed to
  # Hapludolls.
  m <- mollic_epipedon_usda(pedon)
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
#' @export
alfisol_usda <- function(pedon) {
  ar <- argillic_usda(pedon)
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
#' @export
inceptisol_usda <- function(pedon) {
  cb <- cambic(pedon)
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
#' @export
entisol_usda <- function(pedon) {
  DiagnosticResult$new(
    name = "entisol_usda", passed = TRUE,
    layers = seq_len(nrow(pedon$horizons)),
    evidence = list(catch_all = TRUE),
    missing = character(0),
    reference = "USDA Soil Survey Staff (2022), KST 13th ed., Ch 8 Entisols (p 165) -- catch-all"
  )
}
