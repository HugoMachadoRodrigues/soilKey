# ================================================================
# WRB 2022 -- Diagnostic materials
#
# Materials are profile-level features that key on the soil's parent
# substrate, depositional history, or constituent composition rather
# than on a discrete horizon. WRB 2022 Chapter 3 lists organic
# material, fluvic material, calcaric / gypsiric material, sulfidic
# material, tephric material, mineral material, artefact-rich
# material, etc.
#
# v0.3 implements four materials whose underlying RSGs are otherwise
# unaddressable: histic (Histosols), fluvic (Fluvisols), arenic
# (Arenosols), and artefact-rich technic (Technosols).
# ================================================================


#' Histic horizon (WRB 2022)
#'
#' A surface horizon (or near-surface, after drainage) of organic
#' material >= 10 cm thick; diagnostic of Histosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm) of contiguous organic
#'        material from the surface (default 10).
#' @param min_oc Minimum organic carbon \% (default 12, WRB 2022;
#'        equivalent to \code{>= 20\%} organic matter).
#' @param surface_top_cm Maximum top depth (cm) for a layer to be
#'        considered "surface-related" (default 0; the histic horizon
#'        must reach the surface, possibly after drainage).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests:
#' \itemize{
#'   \item \code{\link{test_oc_above}} -- OC \% >= 12
#'   \item \code{\link{test_top_at_or_above}} -- top_cm <= 0
#'   \item \code{\link{test_minimum_thickness}} -- thickness >= 10 cm
#' }
#'
#' v0.3 limitations: WRB 2022 also accepts a 40 cm cumulative
#' organic-material thickness within the upper 80 cm (relevant for
#' folic / mossy Histosols on slopes); v0.4 will add the cumulative
#' variant. The "after drainage" qualifier (recently-drained organic
#' soils) is also deferred.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Histic horizon
#'   and organic material.
#' @export
histic_horizon <- function(pedon,
                              min_thickness  = 10,
                              min_oc         = 12,
                              surface_top_cm = 0) {
  h <- pedon$horizons

  tests <- list()
  tests$organic_carbon <- test_oc_above(h, min_pct = min_oc)
  tests$at_surface     <- test_top_at_or_above(h,
                                                  max_top_cm        = surface_top_cm,
                                                  candidate_layers  = tests$organic_carbon$layers)
  tests$thickness      <- test_minimum_thickness(h,
                                                    min_cm            = min_thickness,
                                                    candidate_layers  = tests$at_surface$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "histic_horizon",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Histic horizon"
  )
}


#' Arenic texture (WRB 2022)
#'
#' Tests whether the upper 100 cm is uniformly coarser than sandy
#' loam (i.e., \code{silt + 2 * clay < 30} in every layer).
#' Diagnostic of Arenosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) of layers to be tested
#'        (default 100, per WRB 2022).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-test: \code{\link{test_coarse_texture_throughout}}.
#'
#' v0.3 limitations: WRB 2022 Arenosol also requires that no other
#' diagnostic horizon (argic, ferralic, etc.) is present, but those
#' exclusions happen at the key level via canonical RSG order.
#'
#' @references IUSS Working Group WRB (2022), Chapter 5, Arenosols.
#' @export
arenic_texture <- function(pedon, max_top_cm = 100) {
  h <- pedon$horizons

  tests <- list()
  tests$coarse_throughout <- test_coarse_texture_throughout(h,
                                                              max_top_cm = max_top_cm)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "arenic_texture",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Arenosols"
  )
}


#' Technic features (WRB 2022)
#'
#' Tests whether the profile contains >= 20\% by volume of artefacts
#' (any human-made or human-altered material) within the upper 100 cm.
#' Diagnostic of Technosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_pct Minimum artefact percent (default 20).
#' @param max_top_cm Maximum top depth (cm) (default 100).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-test: \code{\link{test_artefacts_concentration}}.
#'
#' v0.3 limitations: WRB 2022 also accepts a continuous geomembrane
#' within 100 cm OR technic hard material (concrete, asphalt, mine
#' spoil) covering >= 95\% within 5 cm of the surface as alternative
#' qualifying conditions. v0.4 will add these alternate paths.
#'
#' @references IUSS Working Group WRB (2022), Chapter 5, Technosols.
#' @export
technic_features <- function(pedon, min_pct = 20, max_top_cm = 100) {
  h <- pedon$horizons

  tests <- list()
  tests$artefacts <- test_artefacts_concentration(h,
                                                    min_pct    = min_pct,
                                                    max_top_cm = max_top_cm)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "technic_features",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Technosols"
  )
}


#' Fluvic material (WRB 2022)
#'
#' Tests whether the profile shows fluvic material features: alternating
#' textures across consecutive horizons within the upper 100 cm AND an
#' irregular (non-monotone) organic carbon pattern with depth.
#' Diagnostic of Fluvisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) considered (default 100).
#' @param min_clay_swing Minimum absolute clay-percent change between
#'        consecutive layers required to count as alternation
#'        (default 8 percentage points).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-test: \code{\link{test_fluvic_stratification}}.
#'
#' v0.3 limitations: WRB 2022 fluvic material also requires age
#' (typically <100 years for sediment freshness), which v0.3 does not
#' check (no temporal fields in the schema). The stratification proxy
#' is conservative -- truly heterogeneous floodplain profiles with
#' dramatic texture swings will pass; subtle alluvial sequences may
#' miss. v0.4 will refine.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Fluvic material.
#' @export
fluvic_material <- function(pedon, max_top_cm = 100, min_clay_swing = 8) {
  h <- pedon$horizons

  tests <- list()
  tests$stratification <- test_fluvic_stratification(h,
                                                       max_top_cm     = max_top_cm,
                                                       min_clay_swing = min_clay_swing)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "fluvic_material",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Fluvic material"
  )
}
