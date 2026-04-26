# ================================================================
# WRB 2022 -- Diagnostic properties
#
# Properties differ from horizons in that they characterise a portion
# of the profile (typically depth-bounded) rather than a discrete
# horizon. WRB 2022 Chapter 3 lists gleyic properties, stagnic
# properties, vertic properties, andic properties, and several others.
#
# v0.2 implements gleyic_properties and vertic_properties; the
# remainder (stagnic, andic, anthric, retic, ...) are scheduled for
# v0.3 and the SoilGrids prior integration in v0.5.
# ================================================================


#' Gleyic properties (WRB 2022)
#'
#' Tests whether the profile shows gleyic properties -- evidence of
#' prolonged saturation by groundwater -- within the upper 50 cm.
#' Gleyic properties are diagnostic for Gleysols and qualify many other
#' RSGs (Endogleyic, Epigleyic qualifiers).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) of a candidate layer
#'        (default 50, per WRB 2022).
#' @param min_redox_pct Minimum \code{redoximorphic_features_pct}
#'        (default 5).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-test: \code{\link{test_gleyic_features}} -- requires explicit
#' \code{redoximorphic_features_pct} >= 5\% within the upper 50 cm.
#'
#' v0.2 deliberately does NOT use the Munsell-based shortcut (chroma <=
#' 2 + value >= 4) as a primary criterion: that pattern fits albic /
#' bleached horizons of Podzols just as well as truly reduced gleyic
#' horizons. v0.3 will add reductimorphic / oxidimorphic feature
#' discrimination once we model field-described mottle properties.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Gleyic properties.
#' @export
gleyic_properties <- function(pedon, max_top_cm = 50, min_redox_pct = 5) {
  h <- pedon$horizons

  tests <- list()
  tests$gleyic_features <- test_gleyic_features(h,
                                                   max_top_cm    = max_top_cm,
                                                   min_redox_pct = min_redox_pct)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "gleyic_properties",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Gleyic properties"
  )
}


#' Vertic properties (WRB 2022)
#'
#' Tests whether any horizon shows vertic properties -- shrink-swell
#' clay behaviour evidenced by slickensides, wedge-shaped peds, and
#' deep cracks. Diagnostic for Vertisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_clay Minimum clay percent (default 30, per WRB 2022).
#' @param slickenside_levels Vector of \code{slickensides} values
#'        accepted as evidence (default \code{c("common", "many",
#'        "continuous")}).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests:
#' \itemize{
#'   \item \code{\link{test_clay_above}} -- clay >= 30\%
#'   \item \code{\link{test_slickensides_present}} -- slickensides at
#'         or above the "common" level
#' }
#'
#' v0.2 limitations: WRB also accepts deep cracks (>= 1 cm wide
#' extending from the surface to >= 50 cm depth, when soil is dry) and
#' wedge-shaped peds as alternative evidence; v0.2 only uses the
#' clay + slickensides combination. The "after mixing of upper 18 cm"
#' clause from WRB is also not yet implemented. Both scheduled for v0.3.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Vertic properties.
#' @export
vertic_properties <- function(pedon,
                                min_clay = 30,
                                slickenside_levels = c("common", "many",
                                                         "continuous")) {
  h <- pedon$horizons

  tests <- list()
  tests$clay         <- test_clay_above(h, min_pct = min_clay)
  tests$slickensides <- test_slickensides_present(h,
                                                     levels           = slickenside_levels,
                                                     candidate_layers = tests$clay$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "vertic_properties",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Vertic properties"
  )
}
