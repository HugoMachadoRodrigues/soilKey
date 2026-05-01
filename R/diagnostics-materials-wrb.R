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
#' A surface (or near-surface, after drainage) horizon of organic
#' material; diagnostic of Histosols. Two alternative qualifying
#' paths per WRB 2022:
#' \itemize{
#'   \item \strong{Contiguous}: a single layer of organic material
#'         (OC \% >= \code{min_oc}) reaching the surface and at
#'         least \code{min_thickness} cm thick (default 10 cm).
#'   \item \strong{Cumulative}: organic material totalling
#'         \code{cumulative_min_cm} cm (default 40) within the upper
#'         \code{cumulative_max_depth_cm} (default 80). Relevant for
#'         folic / mossy Histosols on slopes.
#' }
#' Either path qualifies. The "after drainage" qualifier (recently
#' drained organic soils) is treated as implicit since the same OC
#' and thickness criteria apply.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm) for the contiguous
#'        path (default 10).
#' @param min_oc Minimum organic carbon \% (default 12, WRB 2022;
#'        equivalent to \code{>= 20\%} organic matter).
#' @param surface_top_cm Maximum top depth (cm) for a layer to be
#'        considered "surface-related" in the contiguous path
#'        (default 0).
#' @param cumulative_min_cm Minimum cumulative thickness (cm) for the
#'        cumulative path (default 40).
#' @param cumulative_max_depth_cm Depth window (cm) for the cumulative
#'        path (default 80).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Histic horizon
#'   and organic material.
#' @export
histic_horizon <- function(pedon,
                              min_thickness            = 10,
                              min_oc                   = 12,
                              surface_top_cm           = 0,
                              cumulative_min_cm        = 40,
                              cumulative_max_depth_cm  = 80) {
  h <- pedon$horizons

  paths <- list()
  paths$contiguous <- list(
    organic_carbon = test_oc_above(h, min_pct = min_oc),
    at_surface     = test_top_at_or_above(h, max_top_cm = surface_top_cm,
                                            candidate_layers =
                                              test_oc_above(h, min_pct = min_oc)$layers),
    thickness      = test_minimum_thickness(
                        h, min_cm = min_thickness,
                        candidate_layers =
                          test_top_at_or_above(h,
                            max_top_cm = surface_top_cm,
                            candidate_layers =
                              test_oc_above(h, min_pct = min_oc)$layers)$layers)
  )
  paths$cumulative <- list(
    cumulative_oc = test_oc_cumulative_thickness(
                      h, min_oc = min_oc,
                      min_thickness_cm = cumulative_min_cm,
                      max_depth_cm     = cumulative_max_depth_cm)
  )

  agg <- aggregate_alternatives(paths)

  DiagnosticResult$new(
    name      = "histic_horizon",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = paths,
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
#' Tests for any of three WRB 2022 alternative qualifying conditions
#' for Technosols:
#' \enumerate{
#'   \item Artefacts >= \code{artefacts_min_pct} (default 20\%) by
#'         volume within the upper \code{max_top_cm} (default 100 cm).
#'   \item A continuous geomembrane (\code{geomembrane_present == TRUE})
#'         within the upper 100 cm.
#'   \item Technic hard material (concrete, asphalt, mine spoil) with
#'         \code{technic_hardmaterial_pct >= hardmaterial_min_pct}
#'         (default 95\%) at the surface (top_cm <=
#'         \code{hardmaterial_max_top_cm}, default 5).
#' }
#' Either path qualifies.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param artefacts_min_pct Minimum artefact percent (default 20).
#' @param max_top_cm Maximum top depth (cm) for the artefact and
#'        geomembrane paths (default 100).
#' @param hardmaterial_min_pct Minimum hard-material coverage (\%)
#'        for the technic-hard-material path (default 95).
#' @param hardmaterial_max_top_cm Surface depth window (cm) for the
#'        technic-hard-material path (default 5).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Technosols.
#' @export
technic_features <- function(pedon,
                                artefacts_min_pct       = 20,
                                max_top_cm              = 100,
                                hardmaterial_min_pct    = 95,
                                hardmaterial_max_top_cm = 5) {
  h <- pedon$horizons

  paths <- list()
  paths$artefacts <- list(
    artefacts = test_artefacts_concentration(
                  h, min_pct = artefacts_min_pct, max_top_cm = max_top_cm)
  )
  paths$geomembrane <- list(
    geomembrane = test_geomembrane_within_depth(h, max_top_cm = max_top_cm)
  )
  paths$hardmaterial <- list(
    hardmaterial = test_technic_hardmaterial_at_surface(
                     h, min_pct    = hardmaterial_min_pct,
                        max_top_cm = hardmaterial_max_top_cm)
  )

  agg <- aggregate_alternatives(paths)

  DiagnosticResult$new(
    name      = "technic_features",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = paths,
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
