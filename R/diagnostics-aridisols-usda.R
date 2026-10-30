# =============================================================
# USDA Soil Taxonomy 13ed -- Aridisols helpers (Cap 7, pp 137-164)
# =============================================================
#
# Aridisols form in arid (or seasonally arid) climates and have one
# or more diagnostic subsurface horizons or characteristics
# (argillic, natric, calcic, gypsic, salic, duripan, etc.).
# 7 Suborders distinguished by the dominant subsurface feature.
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 7.
# =============================================================


#' Aridisol Order qualifier (USDA, KST 13ed, Ch 2)
#' Pass when the soil has aridic SMR AND any one of: argillic, natric,
#' kandic, calcic, petrocalcic, gypsic, petrogypsic, salic, duripan,
#' cambic, sulfuric horizon. Also requires no other prior order match.
#'
#' The aridic regime is read only from \code{site$soil_moisture_regime}.
#' When the site does not record a regime the test is undecided (NA) and
#' the key goes on to the next order; it is never inferred from the
#' horizons (v0.9.324).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
aridisol_qualifying_usda <- function(pedon) {
  smr <- smr_aridic_usda(pedon)
  # Key to Soil Orders, item G.1.a (KST 13 p 69): "An aridic soil moisture
  # regime". KST 13 p 48 defines it by the moisture control section in
  # normal years: "Dry in all parts for more than half of the cumulative
  # days per year" above 5 C at 50 cm, and moist for "less than 90
  # consecutive days" above 8 C. It is a property of the climate of the
  # soil, estimated "from climatic data" (p 47, the Newhall model), not
  # of the horizons. So a site that records no regime leaves the test
  # undecided (NA). Up to v0.9.314 a missing regime was read as aridic
  # whenever the surface (top <= 5 cm) organic carbon was below 1 % or
  # not recorded: a proxy the text does not give, which sent humid
  # Brazilian Latosols without a recorded regime to the Aridisols.
  aridic <- if (length(smr$missing) > 0L) NA else isTRUE(smr$passed)
  diag_present <-
    isTRUE(argillic_within_usda(pedon, max_top_cm = 200)$passed) ||
    isTRUE(.natric_horizon_impl(pedon)$passed) ||   # v0.9.310: USDA reading
    isTRUE(kandic_horizon_usda(pedon)$passed) ||
    isTRUE(calcic_horizon_usda(pedon, max_top_cm = 200)$passed) ||
    isTRUE(gypsic_horizon_usda(pedon, max_top_cm = 200)$passed) ||
    isTRUE(salic_horizon_usda(pedon, max_top_cm = 200)$passed) ||
    isTRUE(duripan_usda(pedon, max_top_cm = 200)$passed) ||
    isTRUE(duric_horizon(pedon)$passed) ||  # broader cementation
    isTRUE(.cambic_impl(pedon)$passed) ||   # v0.9.310: USDA reading
    isTRUE(sulfuric_horizon_usda(pedon, max_top_cm = 200)$passed)
  # TRUE / FALSE / NA: a recorded non-aridic regime, or no qualifying
  # horizon, fails; an unrecorded regime with a qualifying horizon is NA.
  passed <- .and3v(aridic, diag_present)
  DiagnosticResult$new(
    name = "aridisol_qualifying_usda", passed = passed,
    layers = integer(0),
    evidence = list(aridic = smr, diag_present = diag_present,
                      smr_pass = aridic),
    missing = smr$missing,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 7, p 137"
  )
}


#' Petrocalcic Subgroup helper (Aridisols Petrocalcids)
#' Cemented calcic horizon with cementation_class >= "strongly".
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
petrocalcic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  cem <- h$cementation_class[cand]
  cas <- h$caco3_pct[cand]
  miss <- character(0)
  if (all(is.na(cem))) miss <- c(miss, "cementation_class")
  if (all(is.na(cas))) miss <- c(miss, "caco3_pct")
  passing <- cand[!is.na(cem) & tolower(cem) %in%
                                  c("strongly", "indurated") &
                      !is.na(cas) & cas >= 15]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "petrocalcic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 47"
  )
}


#' Petrogypsic Subgroup helper -- delegate to petrogypsic_horizon_usda
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
petrogypsic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  res <- petrogypsic_horizon_usda(pedon, max_top_cm = max_top_cm)
  res$name <- "petrogypsic_subgroup_usda"
  res
}


#' Sodic Subgroup helper -- delegate to natric_horizon (USDA)
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
sodic_subgroup_usda <- function(pedon) {
  res <- natric_horizon_usda(pedon)
  res$name <- "sodic_subgroup_usda"
  res
}


#' Petronodic Subgroup helper (Aridisols)
#' Pass when 5\%+ rock fragments cemented by carbonates within 100 cm.
#' v0.8 proxy: caco3_pct >= 15 AND coarse_fragments_pct >= 5.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Numeric threshold or option (see Details).
#' @noRd
petronodic_subgroup_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  cas <- h$caco3_pct[cand]
  cf  <- h$coarse_fragments_pct[cand]
  passing <- cand[!is.na(cas) & cas >= 15 &
                      !is.na(cf) & cf >= 5]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "petronodic_subgroup_usda", passed = passed, layers = passing,
    evidence = list(max_top_cm = max_top_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 7"
  )
}


#' Argic Aridisol helper -- argillic-or-kandic in Argids/Cryids/etc.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
argic_aridisol_usda <- function(pedon) {
  res <- argillic_or_kandic_usda(pedon, max_top_cm = 200)
  res$name <- "argic_aridisol_usda"
  res
}


#' Paleargid qualifying helper
#' Pass when argillic horizon has continuous clay films AND
#' clay >> 35\% in upper 10 cm (proxy for old, well-developed argillic).
#' v0.8 proxy: argillic + clay_pct >= 35 in upper 30 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
paleargid_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  arg <- argillic_within_usda(pedon, max_top_cm = 100)
  if (!isTRUE(arg$passed) || length(arg$layers) == 0L) {
    return(DiagnosticResult$new(
      name = "paleargid_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no argillic"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 7"
    ))
  }
  upper_arg <- arg$layers[order(h$top_cm[arg$layers])][1]
  cl <- h$clay_pct[upper_arg]
  passed <- !is.na(cl) && cl >= 35
  DiagnosticResult$new(
    name = "paleargid_qualifying_usda", passed = passed,
    layers = if (passed) upper_arg else integer(0),
    evidence = list(clay_upper_argillic_pct = cl),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 7"
  )
}


#' Vertic Aridisols helper -- delegates to vertic_subgroup_usda
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
vertic_aridisol_usda <- function(pedon) {
  res <- vertic_subgroup_usda(pedon)
  res$name <- "vertic_aridisol_usda"
  res
}
