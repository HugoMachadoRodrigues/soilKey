# =============================================================================
# v0.9.33 -- WRB 2022 qualifier closure (Ch 5 + Ch 6)
#
# Closes the 7 qualifiers identified by the v0.9.33 audit as referenced in
# qualifiers.yaml but lacking a backing function. After this release, 139/139
# unique qualifier names across the 32 RSGs have a backing qual_* function
# (100 % structural coverage).
#
# Each qualifier defined here cross-references the WRB 2022 Ch 5 (page-precise
# definition) and the RSG(s) where it is referenced as principal in Ch 4.
#
#   Endocalcic   (cam) -- depth-conditional calcic
#   Endogleyic   (eng) -- depth-conditional gleyic
#   Endostagnic  (ens) -- depth-conditional stagnic
#   Floatic      (fc)  -- specific gravity < 1 (organic floats on water)
#   Ombric       (om)  -- rain-fed Histosol (no extraneous water input)
#   Rheic        (rh)  -- water-fed Histosol (groundwater / surface water)
#   Toxic        (tx)  -- toxic levels of organic / inorganic constituents
# =============================================================================


# Helper: layer indices where the horizon FALLS WITHIN [min_top, max_top] cm
# of the mineral soil surface. The "Endo-" prefix in WRB 2022 is depth-
# conditional: the diagnostic must appear between 50 and 100 cm of the
# mineral surface (not within the upper 50). Used by Endocalcic /
# Endogleyic / Endostagnic.
.in_lower_subsoil <- function(pedon, min_top_cm = 50, max_top_cm = 100) {
  h <- pedon$horizons
  which(!is.na(h$top_cm) & h$top_cm >= min_top_cm & h$top_cm <= max_top_cm)
}


# Helper: thin presence qualifier in a depth band [min_top, max_top].
.q_endo_presence <- function(name, base_diag, pedon,
                                min_top_cm = 50, max_top_cm = 100) {
  passed <- isTRUE(base_diag$passed) &&
              length(intersect(base_diag$layers,
                                  .in_lower_subsoil(pedon,
                                                      min_top_cm,
                                                      max_top_cm))) > 0L
  layers <- if (passed) base_diag$layers else integer(0)
  DiagnosticResult$new(
    name = name, passed = passed, layers = layers,
    evidence = list(base = base_diag,
                      min_top_cm = min_top_cm,
                      max_top_cm = max_top_cm),
    missing = base_diag$missing %||% character(0),
    reference = "WRB (2022) Ch 5"
  )
}


# ---- Endo-* qualifiers (subsoil-conditional, 50-100 cm) -------------------

#' Endocalcic qualifier (cam): calcic horizon between 50 and 100 cm.
#'
#' WRB 2022 Ch 5 (depth-conditional supplementary form of Calcic).
#' Referenced in Chernozems Ch 4. The diagnostic is the same as
#' Calcic; the difference is the depth band -- Endocalcic requires
#' the calcic horizon to start at >= 50 cm (deep, subsoil) rather
#' than within the upper 50 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
qual_endocalcic <- function(pedon)
  .q_endo_presence("Endocalcic", calcic(pedon), pedon, 50, 100)

#' Endogleyic qualifier (eng): gleyic conditions between 50 and 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
qual_endogleyic <- function(pedon)
  .q_endo_presence("Endogleyic", gleyic_properties(pedon), pedon, 50, 100)

#' Endostagnic qualifier (ens): stagnic conditions between 50 and 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
qual_endostagnic <- function(pedon)
  .q_endo_presence("Endostagnic", stagnic_properties(pedon), pedon, 50, 100)


# ---- Histosol-specific qualifiers -----------------------------------------

#' Floatic qualifier (fc): Histosol that floats on water.
#'
#' WRB 2022 Ch 5 / Ch 4 Histosols (p 96): organic material with very low
#' bulk density (< 0.1 g/cm3 dry, OR < 0.4 g/cm3 in fully saturated state)
#' that floats on water. Practical proxy: oc_pct >= 12 (Histic threshold)
#' AND bulk_density_g_cm3 <= 0.4 in any layer of the upper 100 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
qual_floatic <- function(pedon) {
  h <- pedon$horizons
  cand <- .in_upper(pedon, 100)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Floatic", passed = FALSE,
                                  layers = integer(0),
                                  evidence = list(reason = "no candidate layers"),
                                  missing = character(0),
                                  reference = "WRB (2022) Ch 5"))

  oc <- h$oc_pct[cand]
  bd <- h$bulk_density_g_cm3[cand]
  ok <- !is.na(oc) & oc >= 12 & !is.na(bd) & bd <= 0.4
  passing <- cand[ok]
  passed <- length(passing) > 0L

  miss <- character(0)
  if (all(is.na(oc))) miss <- c(miss, "oc_pct")
  if (all(is.na(bd))) miss <- c(miss, "bulk_density_g_cm3")

  DiagnosticResult$new(
    name = "Floatic", passed = passed, layers = passing,
    evidence = list(threshold_oc = 12, threshold_bd = 0.4,
                      candidate_layers = cand),
    missing = miss,
    reference = "WRB (2022) Ch 4 Histosols (p 96), Ch 5"
  )
}


#' Toxic qualifier (tx): toxic concentration of organic or inorganic constituents.
#'
#' WRB 2022 Ch 5 / Ch 4 Histosols + Cryosols + Technosols (variable pages):
#' substances at concentrations toxic to plant roots. Practical proxy:
#' very low pH (<= 3.5, sulfuric / hyperacidic) OR very high electrical
#' conductivity (>= 16 dS/m, equivalent to Salic) OR specific contamination
#' fields (heavy metals, hydrocarbons) which the soilKey schema does not
#' yet model. v0.9.33 v0 implementation uses pH <= 3.5 OR EC >= 16 dS/m.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
qual_toxic <- function(pedon) {
  h <- pedon$horizons
  cand <- .in_upper(pedon, 100)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Toxic", passed = FALSE,
                                  layers = integer(0),
                                  evidence = list(reason = "no candidate layers"),
                                  missing = character(0),
                                  reference = "WRB (2022) Ch 5"))

  ph  <- h$ph_h2o[cand]
  ec  <- h$ec_dS_m[cand]
  ok_ph <- !is.na(ph) & ph <= 3.5
  ok_ec <- !is.na(ec) & ec >= 16
  passing <- cand[ok_ph | ok_ec]
  passed <- length(passing) > 0L

  miss <- character(0)
  if (all(is.na(ph))) miss <- c(miss, "ph_h2o")
  if (all(is.na(ec))) miss <- c(miss, "ec_dS_m")

  DiagnosticResult$new(
    name = "Toxic", passed = passed, layers = passing,
    evidence = list(threshold_ph_max = 3.5, threshold_ec_min = 16,
                      candidate_layers = cand),
    missing = miss,
    reference = "WRB (2022) Ch 4 Histosols / Cryosols / Technosols, Ch 5"
  )
}


#' Ombric qualifier (om): rain-fed Histosol.
#'
#' WRB 2022 Ch 5 / Ch 4 Histosols (p 96): organic material formed under
#' the influence of rainwater only (NO surface or groundwater input).
#' Distinguished from Rheic by its low pH (rainwater is naturally
#' acidic and unbuffered) and low base saturation. Practical proxy:
#' Histosol Order with weighted-mean pH_h2o <= 4.5 in the upper 100 cm
#' AND no carbonates (calcaric / calcium-rich evidence absent).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
qual_ombric <- function(pedon) {
  h <- pedon$horizons
  cand <- .in_upper(pedon, 100)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Ombric", passed = FALSE,
                                  layers = integer(0),
                                  evidence = list(reason = "no candidate layers"),
                                  missing = character(0),
                                  reference = "WRB (2022) Ch 5"))

  ph    <- h$ph_h2o[cand]
  caco3 <- h$caco3_pct[cand] %||% rep(NA_real_, length(cand))
  oc    <- h$oc_pct[cand]

  # Histic-ish layers (oc >= 12) with very low pH and no carbonates
  histic <- !is.na(oc) & oc >= 12
  acidic <- !is.na(ph) & ph <= 4.5
  no_carb <- is.na(caco3) | caco3 < 1
  ok <- histic & acidic & no_carb
  passing <- cand[ok]
  passed <- length(passing) > 0L

  miss <- character(0)
  if (all(is.na(ph))) miss <- c(miss, "ph_h2o")
  if (all(is.na(oc))) miss <- c(miss, "oc_pct")

  DiagnosticResult$new(
    name = "Ombric", passed = passed, layers = passing,
    evidence = list(threshold_ph_max = 4.5,
                      threshold_oc_min = 12,
                      candidate_layers = cand),
    missing = miss,
    reference = "WRB (2022) Ch 4 Histosols (p 96), Ch 5"
  )
}


#' Rheic qualifier (rh): water-fed Histosol.
#'
#' WRB 2022 Ch 5 / Ch 4 Histosols (p 96): organic material formed under
#' the influence of surface or groundwater (the opposite of Ombric).
#' Distinguished by HIGHER pH and base saturation than Ombric (because
#' the input water carries dissolved bases). Practical proxy:
#' Histosol Order with pH_h2o > 4.5 (above the Ombric ceiling) in the
#' upper 100 cm OR carbonates / calcium-rich evidence present.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @export
qual_rheic <- function(pedon) {
  h <- pedon$horizons
  cand <- .in_upper(pedon, 100)
  if (length(cand) == 0L)
    return(DiagnosticResult$new(name = "Rheic", passed = FALSE,
                                  layers = integer(0),
                                  evidence = list(reason = "no candidate layers"),
                                  missing = character(0),
                                  reference = "WRB (2022) Ch 5"))

  ph    <- h$ph_h2o[cand]
  caco3 <- h$caco3_pct[cand] %||% rep(NA_real_, length(cand))
  oc    <- h$oc_pct[cand]

  histic <- !is.na(oc) & oc >= 12
  base_evidence <- (!is.na(ph) & ph > 4.5) | (!is.na(caco3) & caco3 >= 1)
  ok <- histic & base_evidence
  passing <- cand[ok]
  passed <- length(passing) > 0L

  miss <- character(0)
  if (all(is.na(ph))) miss <- c(miss, "ph_h2o")
  if (all(is.na(oc))) miss <- c(miss, "oc_pct")

  DiagnosticResult$new(
    name = "Rheic", passed = passed, layers = passing,
    evidence = list(threshold_ph_min = 4.5,
                      threshold_oc_min = 12,
                      candidate_layers = cand),
    missing = miss,
    reference = "WRB (2022) Ch 4 Histosols (p 96), Ch 5"
  )
}
