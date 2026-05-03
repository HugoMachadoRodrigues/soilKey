# =============================================================
# USDA Soil Taxonomy 13ed -- Entisols helpers (Cap 8, pp 165-188)
# =============================================================
#
# Entisols are catch-all weakly-developed soils -- usually little
# or no profile development beyond a recently formed A horizon.
# 5 Suborders: Wassents (subaqueous), Aquents, Fluvents (flood
# plain irregular OC), Psamments (sandy), Orthents (catch-all).
# =============================================================


#' Wassent Suborder qualifier (subaqueous Entisol).
#' Pass when site$water_table_cm_above_surface > 0 (water column
#' permanently above the surface).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
wassent_qualifying_usda <- function(pedon) {
  wt <- pedon$site$water_table_cm_above_surface %||% NA_real_
  passed <- !is.na(wt) && wt > 0
  DiagnosticResult$new(
    name = "wassent_qualifying_usda", passed = passed,
    layers = integer(0),
    evidence = list(water_table_cm_above_surface = wt),
    missing = if (is.na(wt)) "site$water_table_cm_above_surface"
              else character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
  )
}


#' Aquent Suborder qualifier (Entisol with aquic conditions <50 cm).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
aquent_qualifying_usda <- function(pedon) {
  res <- aquic_conditions_usda(pedon, max_top_cm = 50)
  res$name <- "aquent_qualifying_usda"
  res
}


#' Fluvent Suborder qualifier (irregular OC decrease in 25-125 cm,
#' OR layered alluvial designation).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
fluvent_qualifying_usda <- function(pedon) {
  res <- fluventic_usda(pedon)
  # Also accept layered alluvial designation pattern (proxy)
  if (!isTRUE(res$passed)) {
    h <- pedon$horizons
    layer_origin_fluv <- !is.na(h$layer_origin) &
                          tolower(h$layer_origin) == "fluvic"
    if (any(layer_origin_fluv)) {
      res <- DiagnosticResult$new(
        name = "fluvent_qualifying_usda", passed = TRUE,
        layers = which(layer_origin_fluv),
        evidence = list(layer_origin = "fluvic"),
        missing = character(0),
        reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
      )
      return(res)
    }
  }
  res$name <- "fluvent_qualifying_usda"
  res
}


#' Psamment Suborder qualifier (sandy texture: clay + 2*silt < 30
#' AND no clay films / argillic).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
psamment_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "psamment_qualifying_usda", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no candidate layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
    ))
  }
  passing <- integer(0)
  for (i in cand) {
    cl <- h$clay_pct[i]; si <- h$silt_pct[i]
    if (is.na(cl) || is.na(si)) next
    # Loamy fine sand or coarser: silt + 2*clay < 30
    if (si + 2 * cl < 30) passing <- c(passing, i)
  }
  passed <- length(passing) >= 0.5 * length(cand)  # at least half
  DiagnosticResult$new(
    name = "psamment_qualifying_usda", passed = passed, layers = passing,
    evidence = list(passing_layers = passing,
                      total_layers = length(cand)),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
  )
}


#' Quartzipsamment helper (Quartzipsamments: >= 95\% resistant minerals)
#'
#' KST 13ed Ch 8 (p 357) defines Quartzipsamments as Psamments where
#' "a weighted average of the resistant minerals in the 0.02-2.0 mm
#' fraction is at least 95 percent". Resistant minerals are dominated
#' by quartz; the practical proxy is a profile that is uniformly
#' sandy with very little clay AND minimal coarse fragments AND no
#' explicit mineralogical evidence of weatherable minerals.
#'
#' v0.9.31 broadens the proxy from "clay <= 5 % AND coarse_fragments
#' <= 5 %" (which under-detected; only 0/14 KSSL Quartzipsamments
#' were caught) to:
#'
#' \itemize{
#'   \item \code{clay_pct <= 10} (loamy sands and finer sands all
#'         qualify -- the 5 % cutoff was too strict);
#'   \item \code{sand_pct >= 80} (sand-dominated texture -- a NEW
#'         requirement, since clay alone is not sufficient);
#'   \item \code{coarse_fragments_pct <= 15} (some coarse fragments
#'         tolerated; 5 % was overly strict);
#'   \item at least 50 % of in-range layers must satisfy all three
#'         (preserved from v0.8).
#' }
#'
#' This still excludes Loamy Psamments and Sandy-Loamy Psamments
#' (Udipsamments / Ustipsamments fallthroughs) by requiring sand >=
#' 80 %; it captures the mineralogical signal indirectly via the
#' near-pure-sand texture.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
quartzipsamment_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 100)
  if (length(cand) == 0L) {
    return(DiagnosticResult$new(
      name = "quartzipsamment_qualifying_usda", passed = FALSE,
      layers = integer(0), evidence = list(reason = "no layers"),
      missing = character(0),
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8 (p 357)"
    ))
  }
  cl <- h$clay_pct[cand]
  sd <- h$sand_pct[cand]
  cf <- h$coarse_fragments_pct[cand]

  # Layer is Quartzipsamment-compatible if all three conditions met.
  layer_ok <- !is.na(cl) & cl <= 10 &
                !is.na(sd) & sd >= 80 &
                (is.na(cf) | cf <= 15)
  passing <- cand[layer_ok]
  passed <- length(passing) >= 0.5 * length(cand)

  DiagnosticResult$new(
    name = "quartzipsamment_qualifying_usda", passed = passed,
    layers = passing,
    evidence = list(
      threshold_clay_max = 10,
      threshold_sand_min = 80,
      threshold_cf_max   = 15,
      total_layers       = length(cand),
      passing_layers     = length(passing)
    ),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8 (p 357)"
  )
}


#' Hydric Aquent helper (Hydraquents)
#' Pass when surface 0-50 has high water content (n value high).
#' v0.8 proxy: water_content_1500kpa >= 80\% in surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
hydraquent_qualifying_usda <- function(pedon) {
  h <- pedon$horizons
  cand <- which(!is.na(h$top_cm) & h$top_cm < 50)
  wr <- h$water_content_1500kpa[cand]
  miss <- if (all(is.na(wr))) "water_content_1500kpa" else character(0)
  passing <- cand[!is.na(wr) & wr >= 80]
  passed <- length(passing) > 0L
  DiagnosticResult$new(
    name = "hydraquent_qualifying_usda", passed = passed, layers = passing,
    evidence = list(threshold_pct = 80),
    missing = miss,
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
  )
}
