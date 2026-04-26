# ================================================================
# WRB 2022 -- Diagnostic horizons
#
# v0.1 implements three horizons: argic, ferralic, mollic.
#
# Each function:
#   - takes a PedonRecord
#   - calls a sequence of sub-tests from utils-diagnostic-tests.R
#   - aggregates the results
#   - returns a DiagnosticResult
#
# Sub-tests are exported so users can inspect them in isolation. The
# diagnostic functions are exported for direct use and for the rule
# engine to discover by name.
# ================================================================


#' Argic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the argic horizon criteria per Chapter
#' 3 of the WRB 2022 (4th edition). Argic is a subsurface horizon with
#' distinctly higher clay content than the overlying horizon, qualified by
#' three depth-conditional clay-increase rules; it must also have texture
#' of sandy loam or finer, satisfy a minimum thickness, and not exhibit
#' albeluvic glossic features (which would direct the profile to the
#' Retisol path).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 7.5).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called (each a list with \code{passed}, \code{layers},
#' \code{missing}, \code{details}, \code{notes}):
#' \itemize{
#'   \item \code{\link{test_clay_increase_argic}} -- the three-pronged
#'         WRB 2022 clay-increase rule.
#'   \item \code{\link{test_minimum_thickness}} -- thickness >= 7.5 cm
#'         (configurable via \code{min_thickness}).
#'   \item \code{\link{test_texture_argic}} -- texture of sandy loam or
#'         finer (\code{silt + 2 * clay >= 30}).
#'   \item \code{test_not_albeluvic} -- excludes profiles with glossic
#'         tongues (Retisol path).
#' }
#'
#' v0.1 limitations: clay-increase distance (<= 30 cm vertical, or <= 15
#' cm with abrupt textural change) is not yet enforced; that is scheduled
#' for v0.2 and depends on horizon boundary descriptions.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3 -- Argic horizon.
#'
#' @export
argic <- function(pedon, min_thickness = 7.5) {
  h <- pedon$horizons

  tests <- list()
  tests$clay_increase <- test_clay_increase_argic(h)

  candidate_layers <- tests$clay_increase$layers

  tests$thickness     <- test_minimum_thickness(h,
                                                  min_cm           = min_thickness,
                                                  candidate_layers = candidate_layers)
  tests$texture       <- test_texture_argic(h,
                                              candidate_layers = candidate_layers)
  tests$not_albeluvic <- test_not_albeluvic(h)

  agg <- aggregate_subtests(
    tests,
    layer_tests = c("clay_increase", "thickness", "texture"),
    exclusions  = "not_albeluvic"
  )

  DiagnosticResult$new(
    name      = "argic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Argic horizon",
    notes     = if (length(candidate_layers) == 0L) {
                   "No layer satisfied the clay-increase precondition"
                 } else NA_character_
  )
}


#' Ferralic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the ferralic horizon criteria. The
#' ferralic horizon is a subsurface horizon resulting from long and
#' intense weathering, characterized by very low cation exchange capacity
#' per unit clay -- the canonical "low-activity clay" signal that defines
#' the Ferralsol RSG.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 30).
#' @param max_cec Maximum CEC (1M NH4OAc, pH 7) per kg clay (default 16).
#' @param max_ecec Maximum effective CEC per kg clay (default 12).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called:
#' \itemize{
#'   \item \code{\link{test_ferralic_texture}} -- texture sandy loam or
#'         finer.
#'   \item \code{\link{test_cec_per_clay}} -- CEC / clay <= 16
#'         cmol_c/kg clay.
#'   \item \code{\link{test_ecec_per_clay}} -- ECEC / clay <= 12
#'         cmol_c/kg clay; ECEC computed from sum of exchangeable bases
#'         plus Al if not measured directly.
#'   \item \code{\link{test_ferralic_thickness}} -- thickness >= 30 cm.
#' }
#'
#' v0.1 limitations: weatherable-mineral test (<= 10\% by volume in silt
#' and sand fractions), water-dispersible-clay test, and stratification /
#' rock-structure exclusions are deferred to v0.2 -- they require
#' mineralogical data not present in the canonical horizon schema. In
#' practice, profiles that meet the CEC/ECEC and texture criteria are
#' overwhelmingly true ferralic horizons; the deferred tests are
#' refinements rather than gates.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3 -- Ferralic horizon.
#'
#' @export
ferralic <- function(pedon,
                       min_thickness = 30,
                       max_cec       = 16,
                       max_ecec      = 12) {
  h <- pedon$horizons

  tests <- list()
  tests$texture       <- test_ferralic_texture(h)
  tests$cec_per_clay  <- test_cec_per_clay(h,
                                             max_cmol_per_kg_clay = max_cec)
  tests$ecec_per_clay <- test_ecec_per_clay(h,
                                              max_cmol_per_kg_clay = max_ecec)
  tests$thickness     <- test_ferralic_thickness(h, min_cm = min_thickness)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "ferralic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Ferralic horizon"
  )
}


#' Mollic horizon (WRB 2022)
#'
#' Tests whether any near-surface horizon meets the mollic horizon
#' criteria. The mollic horizon is the diagnostic surface horizon of
#' Chernozems, Phaeozems, Kastanozems, and several other RSGs; it
#' indicates a thick, dark, base-rich, organic-matter-enriched topsoil
#' formed under steppe or comparable vegetation.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 20).
#' @param min_oc Minimum SOC \% (default 0.6).
#' @param min_bs Minimum base saturation \% (default 50).
#' @param surface_top_cm Maximum top depth (cm) for a horizon to be
#'        considered "surface-related" (default 5). v0.1 uses this as a
#'        proxy for the WRB rule that mollic must form continuously from
#'        the soil surface (after mixing of upper 20 cm if required).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called:
#' \itemize{
#'   \item \code{\link{test_mollic_color}} -- moist value <= 3, moist
#'         chroma <= 3, dry value <= 5.
#'   \item \code{\link{test_mollic_organic_carbon}} -- SOC >= 0.6\%.
#'   \item \code{\link{test_mollic_base_saturation}} -- BS (NH4OAc, pH 7)
#'         >= 50\%.
#'   \item \code{\link{test_mollic_thickness}} -- horizon thickness >= 20 cm.
#'   \item \code{\link{test_mollic_structure}} -- not simultaneously
#'         massive AND very hard when dry.
#' }
#'
#' v0.1 limitations: cumulative thickness across contiguous mollic-
#' qualifying horizons is not yet supported -- this matters for profiles
#' where mollic criteria are met by an A1+A2 sequence but no single
#' horizon is >= 20 cm thick. Mixing of upper 20 cm before the test (per
#' WRB) is also deferred to v0.2.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3 -- Mollic horizon.
#'
#' @export
mollic <- function(pedon,
                     min_thickness  = 20,
                     min_oc         = 0.6,
                     min_bs         = 50,
                     surface_top_cm = 5) {
  h <- pedon$horizons

  candidate_layers <- which(!is.na(h$top_cm) & h$top_cm <= surface_top_cm)

  tests <- list()
  tests$color           <- test_mollic_color(h,
                                                candidate_layers = candidate_layers)
  tests$organic_carbon  <- test_mollic_organic_carbon(h,
                                                        min_pct = min_oc,
                                                        candidate_layers = candidate_layers)
  tests$base_saturation <- test_mollic_base_saturation(h,
                                                         min_pct = min_bs,
                                                         candidate_layers = candidate_layers)
  tests$thickness       <- test_mollic_thickness(h,
                                                    min_cm = min_thickness,
                                                    candidate_layers = candidate_layers)
  tests$structure       <- test_mollic_structure(h,
                                                    candidate_layers = candidate_layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "mollic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Mollic horizon",
    notes     = if (length(candidate_layers) == 0L) {
                   "No surface-related candidate layers (top_cm <= 5)"
                 } else NA_character_
  )
}
