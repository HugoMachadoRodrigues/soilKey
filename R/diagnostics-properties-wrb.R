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


#' Planic features (WRB 2022)
#'
#' Tests whether the profile shows an abrupt textural change between
#' adjacent horizons (clay-doubling within 7.5 cm vertical distance,
#' typically at the E/Bt boundary). Diagnostic of Planosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_ratio Minimum clay ratio (default 2.0).
#' @param require_abrupt_boundary If TRUE (default), the upper horizon
#'        must have \code{boundary_distinctness} matching "abrupt".
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Planosols.
#' @export
planic_features <- function(pedon, min_ratio = 2.0,
                              require_abrupt_boundary = TRUE) {
  h <- pedon$horizons
  tests <- list()
  tests$abrupt <- test_abrupt_textural_change(h,
                                                 min_ratio              = min_ratio,
                                                 require_abrupt_boundary = require_abrupt_boundary)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "planic_features",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Planosols"
  )
}


#' Stagnic properties (WRB 2022)
#'
#' Tests for redoximorphic features driven by perched water. Distinct
#' from gleyic (groundwater): stagnic features appear in upper layers
#' AND redox decreases substantially with depth (the perched layer
#' sits above a slowly permeable subsoil that itself is not
#' saturated).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) of candidate shallow
#'        layers (default 100).
#' @param min_redox_pct Minimum redox feature percent in the shallow
#'        layer (default 5).
#' @param decay_factor Required factor of redox decrease with depth
#'        (default 3, i.e., deeper redox < shallow / 3).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3, Stagnic
#'   properties.
#' @export
stagnic_properties <- function(pedon, max_top_cm = 100,
                                  min_redox_pct = 5, decay_factor = 3) {
  h <- pedon$horizons
  tests <- list()
  tests$stagnic_pattern <- test_stagnic_pattern(h,
                                                   max_top_cm    = max_top_cm,
                                                   min_redox_pct = min_redox_pct,
                                                   decay_factor  = decay_factor)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "stagnic_properties",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Stagnic properties"
  )
}


#' Retic properties (WRB 2022)
#'
#' Tests whether any horizon designation indicates retic features
#' (glossic tongues of bleached material penetrating into a clay-
#' enriched horizon). v0.3 detects these via designation pattern
#' matching \code{"glossic|retic|albeluvic"} (case-insensitive).
#' Diagnostic of Retisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param pattern Regex (default
#'        \code{"glossic|retic|albeluvic"}).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Retisols.
#' @export
retic_properties <- function(pedon, pattern = "glossic|retic|albeluvic") {
  h <- pedon$horizons
  tests <- list()
  tests$retic_designation <- test_designation_pattern(h, pattern = pattern)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "retic_properties",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Retisols"
  )
}


#' Cryic conditions (WRB 2022)
#'
#' Tests whether continuous frozen / permafrost material occurs within
#' the upper 100 cm. v0.3 detects via designation pattern: any layer
#' with designation containing the suffix \code{"f"} (frozen) within
#' the top 100 cm, or the explicit pattern \code{"^Cf"} / \code{"perma"}.
#' Diagnostic of Cryosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) (default 100).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Cryosols.
#' @export
cryic_conditions <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons
  tests <- list()
  tests$frozen_designation <- test_designation_pattern(
    h, pattern = "^[A-Z][a-z]*f($|[0-9])|^Cf|perma"
  )
  tests$within_depth <- test_top_at_or_above(
    h, max_top_cm = max_top_cm,
    candidate_layers = tests$frozen_designation$layers
  )

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "cryic_conditions",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Cryosols"
  )
}


#' Anthric horizons (WRB 2022)
#'
#' Tests for any of five anthropogenic surface horizons recognised by
#' WRB 2022: hortic, irragric, plaggic, pretic, terric.
#' v0.3 detects via designation pattern matching any of those names.
#' Diagnostic of Anthrosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Anthrosols.
#' @export
anthric_horizons <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$anthric_designation <- test_designation_pattern(
    h, pattern = "hortic|irragric|plaggic|pretic|terric"
  )

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "anthric_horizons",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Anthrosols"
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
