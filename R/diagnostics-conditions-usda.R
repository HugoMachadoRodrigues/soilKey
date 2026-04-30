# =============================================================
# USDA Soil Taxonomy 13th edition (2022)
# Diagnostic Soil Characteristics
# Chapter 3, pp 34-50 (selected characteristics for Suborder/GG/SG keys)
# =============================================================
#
# This file implements diagnostic characteristics (non-horizon
# features) used in Order/Suborder/Great Group/Subgroup keys:
#
#  Aquic conditions       Saturation + reduction (redoximorphic features)
#  Anhydrous conditions   Cold-desert dry conditions in permafrost soils
#  Cryoturbation          Frost-churning (mixed/distorted horizons)
#  Glacic layer           >=30 cm of >=75% visible ice
#  Permafrost             Soil material < 0 C for 2+ consecutive years
#
# Reference: Soil Survey Staff (2022), KST 13ed, Ch. 3.
# =============================================================


# ---- Aquic Conditions (KST 13ed, Ch 3, pp 41-44) ---------------------

#' Aquic conditions (USDA Soil Taxonomy, 13th edition)
#'
#' "Soils with aquic conditions are those that currently undergo
#' continuous or periodic saturation and reduction. The presence of
#' these conditions is indicated by redoximorphic features, except
#' in Histosols and Histels."  -- KST 13ed, Ch 3, p 41.
#'
#' Three types of saturation are defined:
#' \itemize{
#'   \item \strong{Endosaturation}: saturated in all layers from the
#'         upper boundary of saturation to >=200 cm.
#'   \item \strong{Episaturation}: saturated in one or more layers
#'         within 200 cm with unsaturated layer(s) below.
#'   \item \strong{Anthric saturation}: cultivated/flood-irrigated.
#' }
#'
#' Implementation (v0.8.x):
#' \itemize{
#'   \item Saturation is inferred from the presence of redoximorphic
#'         features (\code{redoximorphic_features_pct >= 5}) and/or a
#'         glei horizon (designation containing 'g').
#'   \item Reduction is inferred when chroma <= 2 in the matrix.
#'   \item Artificial drainage is treated as positive aquic when
#'         \code{site$artificially_drained == TRUE} (deferred -- not
#'         in current schema).
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum depth at which saturation must occur
#'        (default 100 -- typical for Suborder keys; 200 for some).
#' @param min_redox_pct Threshold for redoximorphic features
#'        (default 5 percent).
#' @param max_chroma Maximum chroma indicating reduction
#'        (default 2).
#' @return A \code{\link{DiagnosticResult}} with
#'         \code{evidence$saturation_type} = "endo" / "epi" / NA.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 41-44.
#' @export
aquic_conditions_usda <- function(pedon,
                                      max_top_cm    = 100,
                                      min_redox_pct = 5,
                                      max_chroma    = 2) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "aquic_conditions_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 41-44"
    ))
  }
  cand <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  passing <- integer(0); miss <- character(0); details <- list()
  for (i in cand) {
    rdx <- h$redoximorphic_features_pct[i]
    chr <- h$munsell_chroma_moist[i]
    des <- h$designation[i]
    redox_ok <- !is.na(rdx) && rdx >= min_redox_pct
    chroma_ok <- !is.na(chr) && chr <= max_chroma
    glei_des <- !is.na(des) && grepl("g", des)
    layer_pass <- isTRUE(redox_ok) || isTRUE(chroma_ok && glei_des) ||
                    isTRUE(redox_ok && chroma_ok)
    if (is.na(rdx)) miss <- c(miss, "redoximorphic_features_pct")
    if (is.na(chr)) miss <- c(miss, "munsell_chroma_moist")
    details[[as.character(i)]] <- list(idx = i,
                                          redox = redox_ok,
                                          chroma = chroma_ok,
                                          glei = glei_des,
                                          passed = layer_pass)
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- length(passing) > 0L
  # Saturation type heuristic: episaturation if upper passing layer
  # is shallower than 50 cm AND a non-passing layer occurs below it.
  sat_type <- NA_character_
  if (passed) {
    tops_pass <- h$top_cm[passing]
    if (any(tops_pass < 50)) {
      below <- setdiff(cand, passing)
      below_deeper <- below[!is.na(h$top_cm[below]) &
                                h$top_cm[below] > min(tops_pass)]
      sat_type <- if (length(below_deeper) > 0L) "episaturation"
                  else "endosaturation"
    } else {
      sat_type <- "endosaturation"
    }
  }
  DiagnosticResult$new(
    name = "aquic_conditions_usda", passed = passed,
    layers = passing,
    evidence = list(layer_details = details,
                      saturation_type = sat_type,
                      max_top_cm = max_top_cm),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, pp 41-44"
  )
}


# ---- Anhydrous Conditions (KST 13ed, Ch 3, p 33) --------------------

#' Anhydrous conditions (USDA Soil Taxonomy, 13th edition)
#'
#' "Anhydrous conditions refer to the moisture condition of soils in
#' very cold deserts and other areas with permafrost (often dry
#' permafrost). These soils typically have low precipitation
#' (usually less than 50 mm water equivalent per year) and a moisture
#' content of less than 3 percent by weight."  -- KST 13ed, Ch 3,
#' p 33.
#'
#' Required characteristics:
#' \itemize{
#'   \item Mean annual soil temperature <= 0 C; AND
#'   \item Layer 10-70 cm with soil temperature < 5 C throughout the year; AND
#'   \item No ice-impregnated permafrost in that layer; AND
#'   \item One of:
#'     \itemize{
#'       \item Dry (>= 1500 kPa) in 1/2+ of soil for 1/2+ of time
#'             above 0 C; OR
#'       \item Rupture-resistance class loose to slightly hard
#'             throughout when temp <= 0 C (except where pedogenically
#'             cemented).
#'     }
#' }
#'
#' Implementation (v0.8.x): Uses \code{permafrost_temp_C} from schema
#' to flag layers below freezing; checks rupture_resistance for
#' "loose" / "soft" / "slightly hard" in the 10-70 cm layer.
#' Precipitation criterion is deferred to v0.9 (climatic data).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 33.
#' @export
anhydrous_conditions_usda <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "anhydrous_conditions_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 33"
    ))
  }
  # Layer of interest: 10-70 cm
  loi <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) &
                  h$bottom_cm > 10 & h$top_cm < 70)
  if (length(loi) == 0L) {
    return(DiagnosticResult$new(
      name = "anhydrous_conditions_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no 10-70 cm layer"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 33"
    ))
  }
  miss <- character(0)
  pf_temps <- h$permafrost_temp_C[loi]
  rr <- h$rupture_resistance[loi]
  # Need temp < 5 C throughout AND rupture loose-to-slightly-hard
  # (or dry condition).
  if (all(is.na(pf_temps))) miss <- c(miss, "permafrost_temp_C")
  temps_ok <- length(pf_temps) > 0L &&
                all(!is.na(pf_temps) & pf_temps < 5)
  loose_classes <- c("loose", "soft", "slightly hard")
  rr_ok <- !all(is.na(rr)) &&
             all(is.na(rr) | tolower(rr) %in% loose_classes)
  passed <- isTRUE(temps_ok) && isTRUE(rr_ok)
  DiagnosticResult$new(
    name = "anhydrous_conditions_usda", passed = passed,
    layers = if (passed) loi else integer(0),
    evidence = list(layer_of_interest = loi,
                      permafrost_temps_C = pf_temps,
                      rupture_resistance = rr,
                      temps_below_5 = temps_ok,
                      rupture_loose = rr_ok),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 33"
  )
}


# ---- Cryoturbation (KST 13ed, Ch 3, p 43-44) -------------------------

#' Cryoturbation (USDA Soil Taxonomy, 13th edition)
#'
#' "Cryoturbation (frost churning) is the mixing of the soil matrix
#' within the pedon that results in irregular or broken horizons,
#' involutions, accumulation of organic matter on the permafrost
#' table, oriented rock fragments, and silt caps on rock fragments."
#' -- KST 13ed, Ch 3, p 43.
#'
#' Diagnostic for the Turbels suborder of Gelisols.
#'
#' Implementation (v0.8.x): Uses heuristics from horizon designations
#' and morphology data:
#' \itemize{
#'   \item Designation contains 'jj' (cryoturbation symbol) per
#'         KST notation;
#'   \item OR boundary_topography in \{"irregular", "broken",
#'         "involuted"\};
#'   \item OR coarse_fragments_pct varying non-monotonically with
#'         depth (proxy for "oriented rock fragments");
#'   \item OR designation contains 'f' (frozen) AND irregular
#'         boundary_distinctness.
#' }
#'
#' Refinement to incorporate explicit \code{cryoturbation_evidence}
#' column is deferred to v0.9.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 43.
#' @export
cryoturbation_usda <- function(pedon) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "cryoturbation_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 43"
    ))
  }
  jj_layers <- which(!is.na(h$designation) &
                       grepl("jj|@", h$designation))
  irreg_layers <- which(!is.na(h$boundary_topography) &
                          tolower(h$boundary_topography) %in%
                            c("irregular", "broken", "involuted",
                              "discontinuous"))
  passing <- unique(c(jj_layers, irreg_layers))
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "cryoturbation_usda", passed = passed,
    layers = passing,
    evidence = list(jj_designation_layers = jj_layers,
                      irregular_boundary_layers = irreg_layers,
                      note = "v0.8: heuristic via designation 'jj' or irregular topography"),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 43"
  )
}


# ---- Glacic Layer (KST 13ed, Ch 3, p 45) ----------------------------

#' Glacic layer (USDA Soil Taxonomy, 13th edition)
#'
#' "A glacic layer is massive ice or ground ice in the form of ice
#' lenses or wedges. The layer is 30 cm or more thick and contains 75
#' percent or more visible ice."  -- KST 13ed, Ch 3, p 45.
#'
#' Diagnostic for the Glacistels great group of Histels and the
#' Glacic subgroup modifier in Gelisols.
#'
#' Implementation (v0.8.x): Detected via designation containing
#' 'ff' (massive ice) per KST notation, with thickness >= 30 cm.
#' Refinement to use an \code{ice_pct} schema column is deferred
#' to v0.9.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (default 100 cm; subgroup-level
#'        depth bound).
#' @param min_thickness_cm Minimum thickness (default 30 cm).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45.
#' @export
glacic_layer_usda <- function(pedon,
                                  max_top_cm = 100,
                                  min_thickness_cm = 30) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "glacic_layer_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45"
    ))
  }
  ice_layers <- which(!is.na(h$designation) &
                          grepl("ff|^Wf|ice", h$designation,
                                  ignore.case = TRUE) &
                          !is.na(h$top_cm) &
                          h$top_cm <= max_top_cm)
  if (length(ice_layers) == 0L) {
    return(DiagnosticResult$new(
      name = "glacic_layer_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no ice-designated layer within depth"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45"
    ))
  }
  thk <- h$bottom_cm[ice_layers] - h$top_cm[ice_layers]
  passing <- ice_layers[!is.na(thk) & thk >= min_thickness_cm]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "glacic_layer_usda", passed = passed,
    layers = passing,
    evidence = list(ice_layers = ice_layers,
                      thicknesses_cm = thk,
                      max_top_cm = max_top_cm,
                      min_thickness_cm = min_thickness_cm),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 45"
  )
}


# ---- Permafrost (KST 13ed, Ch 3, p 47) ------------------------------

#' Permafrost (USDA Soil Taxonomy, 13th edition)
#'
#' "Permafrost is defined as a thermal condition in which a material
#' (including soil material) remains below 0 C for 2 or more years in
#' succession."  -- KST 13ed, Ch 3, p 47.
#'
#' Permafrost is the defining characteristic of the Gelisols order
#' (within 100 cm of the soil surface) and qualifies many subgroups
#' across Histosols (Histels), Inceptisols, and others.
#'
#' Implementation: Uses \code{permafrost_temp_C} from schema. A
#' layer qualifies as permafrost when its \code{permafrost_temp_C}
#' is <= 0 C. The function checks whether any qualifying layer
#' occurs within \code{max_top_cm} of the surface.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum depth where permafrost must occur
#'        (default 100 cm -- Gelisols criterion at Order level).
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), KST 13ed, Ch. 3, p 47.
#' @export
permafrost_within_usda <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "permafrost_within_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "empty horizons"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 47"
    ))
  }
  miss <- character(0); details <- list(); passing <- integer(0)
  cand <- which(!is.na(h$top_cm) & h$top_cm <= max_top_cm)
  pf_in_layer <- !is.na(h$permafrost_temp_C[cand]) &
                   h$permafrost_temp_C[cand] <= 0
  pf_in_des <- !is.na(h$designation[cand]) &
                  grepl("ff|^Wf", h$designation[cand])
  passing <- cand[pf_in_layer | pf_in_des]
  if (all(is.na(h$permafrost_temp_C[cand])) && length(passing) == 0L) {
    miss <- c(miss, "permafrost_temp_C")
  }
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "permafrost_within_usda", passed = passed,
    layers = passing,
    evidence = list(candidate_layers = cand,
                      pf_in_layer = pf_in_layer,
                      pf_in_designation = pf_in_des,
                      max_top_cm = max_top_cm),
    missing = unique(miss),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 3, p 47"
  )
}
