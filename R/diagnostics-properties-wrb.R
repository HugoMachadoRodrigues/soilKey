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
#' @param stagnic_decay_factor Numeric threshold or option (see Details).
#' @export
gleyic_properties <- function(pedon, max_top_cm = 50, min_redox_pct = 5,
                                 stagnic_decay_factor = 3) {
  h <- pedon$horizons

  tests <- list()
  tests$gleyic_features <- test_gleyic_features(h,
                                                   max_top_cm    = max_top_cm,
                                                   min_redox_pct = min_redox_pct)
  # Refined v0.3 criterion: gleyic vs stagnic discrimination.
  # Gleyic implies groundwater saturation, so redox features should
  # CONTINUE with depth (no substantial decay). If redox decays with
  # depth by stagnic_decay_factor, the profile is more consistent
  # with stagnic / perched-water regime and gleyic_properties must
  # NOT fire (otherwise Stagnosols would never key correctly given
  # GL @ #9 < ST @ #16 in the canonical WRB order).
  tests$stagnic_pattern <- test_stagnic_pattern(h,
                                                   max_top_cm    = max_top_cm,
                                                   min_redox_pct = min_redox_pct,
                                                   decay_factor  = stagnic_decay_factor)

  features_ok <- isTRUE(tests$gleyic_features$passed)
  stagnic_pat <- isTRUE(tests$stagnic_pattern$passed)
  any_na      <- any(vapply(tests, function(t) is.na(t$passed), logical(1)))

  passed <- if (features_ok && !stagnic_pat) TRUE
            else if (any_na && !features_ok) NA
            else FALSE

  layers <- if (isTRUE(passed)) tests$gleyic_features$layers else integer(0)
  missing <- unique(unlist(lapply(tests, function(t) t$missing %||% character(0))))
  if (is.null(missing)) missing <- character(0)

  DiagnosticResult$new(
    name      = "gleyic_properties",
    passed    = passed,
    layers    = layers,
    evidence  = tests,
    missing   = missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Gleyic properties"
  )
}


#' Leptic features (WRB 2022)
#'
#' Tests whether continuous rock or rock-like material occurs within
#' \code{max_depth} cm of the surface. Two alternative paths qualify
#' per WRB 2022:
#' \enumerate{
#'   \item \strong{Designation}: a layer at depth <= \code{max_depth}
#'         with designation matching \code{"^R"} or \code{"^Cr"}
#'         (continuous rock or weathered rock-like substrate).
#'   \item \strong{Coarse fragments}: a layer at depth <= \code{max_depth}
#'         with coarse_fragments_pct >= \code{min_coarse_pct} (default
#'         90\% by volume), interpreted as rock-dominated even when not
#'         R / Cr-designated.
#' }
#' Either path qualifies.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_depth Maximum depth (cm) at which continuous rock or
#'        rock-dominated material must appear (default 25).
#' @param min_coarse_pct Minimum coarse-fragment percent for the
#'        coarse-fragments path (default 90).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Leptosols.
#' @export
leptic_features <- function(pedon, max_depth = 25, min_coarse_pct = 90) {
  h <- pedon$horizons

  designation <- test_designation_pattern(h, pattern = "^R$|^Cr|^R[a-z]")
  paths <- list()
  paths$designation <- list(
    rock_designation = designation,
    shallow          = test_top_at_or_above(
                          h, max_top_cm = max_depth,
                          candidate_layers = designation$layers)
  )
  paths$coarse_fragments <- list(
    coarse_at_surface = test_coarse_fragments_above(
                          h, min_pct    = min_coarse_pct,
                          max_top_cm = max_depth)
  )

  agg <- aggregate_alternatives(paths)

  DiagnosticResult$new(
    name      = "leptic_features",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = paths,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Leptosols"
  )
}


#' Andic properties (WRB 2022)
#'
#' Tests for the andic property complex -- volcanic-ash-derived
#' allophanic / imogolitic / Al-humus material. Diagnostic of
#' Andosols. Two alternative qualifying paths per WRB 2022 Ch 3.2:
#' \enumerate{
#'   \item \strong{Al-Fe oxalate + low BD}:
#'         (Al_ox + 0.5*Fe_ox) >= \code{min_alfe} (default 2.0\%) AND
#'         bulk_density <= \code{max_bd} (default 0.9 g/cm^3) on the
#'         same layer.
#'   \item \strong{Phosphate retention}: phosphate_retention_pct
#'         >= \code{min_p_retention} (default 70\%).
#' }
#' Either path qualifies. The volcanic-glass criterion is the
#' separate \code{\link{vitric_properties}} diagnostic; Andosols key
#' on (andic OR vitric) at the RSG-gate level (\code{\link{andosol}}).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_alfe Minimum (Al_ox + 0.5*Fe_ox) percent for the Al-Fe
#'        path (default 2.0).
#' @param max_bd Maximum bulk density g/cm^3 for the Al-Fe path
#'        (default 0.9).
#' @param min_p_retention Minimum phosphate retention \% for the P
#'        path (default 70).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3, Andic
#'   properties.
#' @export
andic_properties <- function(pedon,
                                min_alfe         = 2.0,
                                max_bd           = 0.9,
                                min_p_retention  = 70) {
  h <- pedon$horizons

  alfe_test <- test_andic_alfe(h, min_pct = min_alfe)

  paths <- list()
  paths$alfe_lowbd <- list(
    alfe_oxalate = alfe_test,
    low_bd       = test_bulk_density_below(
                      h, max_g_cm3         = max_bd,
                         candidate_layers  = alfe_test$layers)
  )
  paths$phosphate_retention <- list(
    p_retention = test_phosphate_retention_above(h, min_pct = min_p_retention)
  )

  agg <- aggregate_alternatives(paths)

  DiagnosticResult$new(
    name      = "andic_properties",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = paths,
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
#' the upper \code{max_top_cm}. Two alternative paths qualify per WRB
#' 2022:
#' \enumerate{
#'   \item \strong{Permafrost temperature}: a layer at top_cm <=
#'         \code{max_top_cm} (default 100) with
#'         \code{permafrost_temp_C <= max_temp_C} (default 0 C).
#'   \item \strong{Designation pattern}: a layer at top_cm <=
#'         \code{max_top_cm} with designation containing suffix
#'         \code{"f"} (frozen) or matching \code{"^Cf"} / \code{"perma"}.
#'         Used as a fallback when the temperature field is not in the
#'         pedon (typical of legacy survey data).
#' }
#' Either path qualifies. Diagnostic of Cryosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) (default 100).
#' @param max_temp_C Maximum mean annual permafrost-zone temperature
#'        (deg C) for the temperature path (default 0).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Cryosols.
#' @export
cryic_conditions <- function(pedon, max_top_cm = 100, max_temp_C = 0) {
  h <- pedon$horizons

  paths <- list()
  paths$permafrost_temp <- list(
    permafrost = test_permafrost_temp_below(h, max_temp_C = max_temp_C,
                                                max_top_cm = max_top_cm)
  )
  desig <- test_designation_pattern(
    h, pattern = "^[A-Z][a-z]*f($|[0-9])|^Cf|perma"
  )
  paths$designation <- list(
    frozen_designation = desig,
    within_depth       = test_top_at_or_above(
                            h, max_top_cm = max_top_cm,
                            candidate_layers = desig$layers)
  )

  agg <- aggregate_alternatives(paths)

  DiagnosticResult$new(
    name      = "cryic_conditions",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = paths,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Cryosols"
  )
}


#' Anthric horizons (WRB 2022)
#'
#' Tests for any of five anthropogenic surface horizons recognised by
#' WRB 2022 (hortic, irragric, plaggic, pretic, terric). Diagnostic
#' of Anthrosols. Two alternative paths qualify:
#' \enumerate{
#'   \item \strong{Designation}: any layer's designation contains one
#'         of \code{hortic|irragric|plaggic|pretic|terric}.
#'   \item \strong{Property-based}: a surface layer (top_cm <= 5)
#'         at least \code{min_thickness_cm} cm thick (default 20)
#'         with elevated dark colour (Munsell value moist <=
#'         \code{max_munsell_value}, default 4) AND elevated
#'         plant-available P (\code{p_mehlich3_mg_kg} >=
#'         \code{min_p_mg_kg}, default 50).
#' }
#' Either path qualifies.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness_cm Minimum thickness for the property-based
#'        path (default 20).
#' @param min_p_mg_kg Minimum plant-available P (Mehlich 3, mg/kg)
#'        for the property-based path (default 50).
#' @param max_munsell_value Maximum Munsell value moist for the
#'        property-based path (default 4).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Anthrosols.
#' @export
anthric_horizons <- function(pedon,
                                min_thickness_cm  = 20,
                                min_p_mg_kg       = 50,
                                max_munsell_value = 4) {
  h <- pedon$horizons

  paths <- list()
  paths$designation <- list(
    anthric_designation = test_designation_pattern(
      h, pattern = "hortic|irragric|plaggic|pretic|terric"
    )
  )
  paths$property_based <- list(
    anthric_props = test_anthric_horizon_properties(
                      h, min_thickness_cm   = min_thickness_cm,
                         min_p_mg_kg        = min_p_mg_kg,
                         max_munsell_value  = max_munsell_value)
  )

  agg <- aggregate_alternatives(paths)

  DiagnosticResult$new(
    name      = "anthric_horizons",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = paths,
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
#' @param min_thickness Minimum thickness (cm) of the vertic layer
#'        (default 25 per WRB 2022 Ch 3.2.x).
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
#'   \item \code{\link{test_minimum_thickness}} -- combined vertic layer
#'         thickness >= 25 cm (v0.3.1 added per WRB 2022)
#' }
#'
#' v0.3.1: thickness gate added. Limitations remaining: WRB also accepts
#' deep cracks (>= 1 cm wide extending from the surface to >= 50 cm
#' depth, when soil is dry) and wedge-shaped peds as alternative
#' evidence; this implementation requires clay + slickensides. The
#' "after mixing of upper 18 cm" clause from WRB is still deferred.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3.2 -- Vertic
#'   properties.
#' @export
vertic_properties <- function(pedon,
                                min_clay        = 30,
                                min_thickness   = 25,
                                slickenside_levels = c("common", "many",
                                                         "continuous")) {
  h <- pedon$horizons

  tests <- list()
  tests$clay         <- test_clay_above(h, min_pct = min_clay)
  tests$slickensides <- test_slickensides_present(h,
                                                     levels           = slickenside_levels,
                                                     candidate_layers = tests$clay$layers)
  # Layers that pass BOTH clay and slickensides feed the thickness gate.
  shared <- intersect(tests$clay$layers, tests$slickensides$layers)
  tests$thickness    <- test_minimum_thickness(h,
                                                 min_cm           = min_thickness,
                                                 candidate_layers = shared)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "vertic_properties",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2, Vertic properties"
  )
}
