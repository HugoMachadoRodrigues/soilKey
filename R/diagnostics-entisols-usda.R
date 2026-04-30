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


#' Quartzipsamment helper (Quartzipsamments: >= 95\% silica)
#' v0.8 proxy: clay <= 5\% AND coarse_fragments_pct <= 5\%.
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
      reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
    ))
  }
  cl <- h$clay_pct[cand]
  cf <- h$coarse_fragments_pct[cand]
  passing <- cand[!is.na(cl) & cl <= 5 &
                      (is.na(cf) | cf <= 5)]
  passed <- length(passing) >= 0.5 * length(cand)
  DiagnosticResult$new(
    name = "quartzipsamment_qualifying_usda", passed = passed,
    layers = passing,
    evidence = list(threshold_clay = 5),
    missing = character(0),
    reference = "Soil Survey Staff (2022), KST 13ed, Ch. 8"
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
