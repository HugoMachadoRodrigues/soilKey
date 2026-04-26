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


#' Leptic features (WRB 2022)
#'
#' Tests whether continuous rock or rock-like material occurs within
#' \code{max_depth} cm of the surface. Diagnostic of Leptosols.
#'
#' v0.3 implementation infers continuous rock from horizon designation
#' (any layer with designation matching \code{"^R"} or
#' \code{"^Cr"} -- standard pedological codes for hard / continuous
#' rock and weathered rock-like substrate respectively).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth Maximum depth (cm) at which continuous rock must
#'        appear (default 25).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Leptosols.
#' @export
leptic_features <- function(pedon, max_depth = 25) {
  h <- pedon$horizons

  tests <- list()
  tests$rock_designation <- test_designation_pattern(h, pattern = "^R$|^Cr|^R[a-z]")
  tests$shallow          <- test_top_at_or_above(h,
                                                    max_top_cm       = max_depth,
                                                    candidate_layers = tests$rock_designation$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "leptic_features",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Leptosols"
  )
}


#' Andic properties (WRB 2022)
#'
#' Tests for the andic property complex -- volcanic-ash-derived
#' allophanic / imogolitic / Al-humus material with low bulk density,
#' high active Al + Fe. Diagnostic of Andosols.
#'
#' v0.3 simplified criteria:
#' \itemize{
#'   \item (Al_ox + 0.5 * Fe_ox) >= 2.0\%
#'   \item bulk_density <= 0.9 g/cm^3
#' }
#' Both must hold on the same layer.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_alfe Minimum (Al_ox + 0.5*Fe_ox) percent (default 2.0).
#' @param max_bd Maximum bulk density g/cm^3 (default 0.9).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' v0.3 limitations: WRB 2022 also accepts phosphate retention
#' >= 70\%, glass content >= 30\% in the sand fraction, or
#' specific Si-oxalate alternatives. None of these are in the
#' canonical schema; v0.4 will extend.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Andic
#'   properties.
#' @export
andic_properties <- function(pedon, min_alfe = 2.0, max_bd = 0.9) {
  h <- pedon$horizons

  tests <- list()
  tests$alfe_oxalate <- test_andic_alfe(h, min_pct = min_alfe)
  tests$low_bd       <- test_bulk_density_below(h,
                                                   max_g_cm3        = max_bd,
                                                   candidate_layers = tests$alfe_oxalate$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "andic_properties",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Andic properties"
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
