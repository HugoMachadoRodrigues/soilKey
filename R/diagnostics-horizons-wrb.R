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


#' Calcic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the calcic horizon criteria. The
#' calcic horizon is a horizon of secondary carbonate accumulation,
#' diagnostic for Calcisols and qualifying many other RSGs.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 15).
#' @param min_caco3_pct Minimum CaCO3 percent in fine earth (default 15).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called:
#' \itemize{
#'   \item \code{\link{test_caco3_concentration}} -- CaCO3 >= 15\%.
#'   \item \code{\link{test_minimum_thickness}} -- thickness >= 15 cm.
#' }
#'
#' v0.2 limitations: the WRB criterion of "5\% absolute or relative more
#' CaCO3 than the underlying horizon" is not enforced; this captures
#' true calcic horizons but may also mark uniformly carbonate-rich
#' substrates that are not pedologically calcic. Cementation
#' (petrocalcic) is not yet detected. Both refinements are scheduled
#' for v0.3.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3 -- Calcic horizon.
#'
#' @export
calcic <- function(pedon, min_thickness = 15, min_caco3_pct = 15) {
  h <- pedon$horizons

  tests <- list()
  tests$caco3     <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  tests$thickness <- test_minimum_thickness(h,
                                              min_cm           = min_thickness,
                                              candidate_layers = tests$caco3$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "calcic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Calcic horizon"
  )
}


#' Gypsic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the gypsic horizon criteria. The
#' gypsic horizon is a horizon of secondary gypsum accumulation,
#' diagnostic for Gypsisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 15).
#' @param min_gypsum_pct Minimum gypsum percent in fine earth (default 5).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called:
#' \itemize{
#'   \item \code{\link{test_caso4_concentration}} -- gypsum >= 5\%.
#'   \item \code{\link{test_minimum_thickness}} -- thickness >= 15 cm.
#' }
#'
#' v0.2 limitations: the WRB rule that gypsum content must exceed the
#' underlying horizon by 1\% (absolute) is not enforced. Petrogypsic
#' (cemented) horizons are not yet detected. Both deferred to v0.3.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3 -- Gypsic horizon.
#'
#' @export
gypsic <- function(pedon, min_thickness = 15, min_gypsum_pct = 5) {
  h <- pedon$horizons

  tests <- list()
  tests$gypsum    <- test_caso4_concentration(h, min_pct = min_gypsum_pct)
  tests$thickness <- test_minimum_thickness(h,
                                              min_cm           = min_thickness,
                                              candidate_layers = tests$gypsum$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "gypsic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Gypsic horizon"
  )
}


#' Cambic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the cambic horizon criteria. The
#' cambic horizon is a subsurface horizon with evidence of pedological
#' alteration that does not meet the criteria for any stronger
#' diagnostic horizon. It is the diagnostic of Cambisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 15).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' v0.2 implementation tests three conditions:
#' \itemize{
#'   \item thickness >= 15 cm (\code{\link{test_minimum_thickness}})
#'   \item texture sandy loam or finer (\code{\link{test_texture_argic}})
#'   \item NOT \code{\link{argic}} AND NOT \code{\link{ferralic}}
#' }
#'
#' v0.2 limitations: WRB 2022 also excludes profiles with spodic,
#' calcic, gypsic, plinthic, vertic, and several other diagnostic
#' horizons. Those exclusions, plus the WRB criteria of "evidence of
#' alteration" (color/structure differences from parent material,
#' carbonate removal), are scheduled for v0.3.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Cambic horizon.
#' @export
cambic <- function(pedon, min_thickness = 15) {
  h <- pedon$horizons

  arg_res <- argic(pedon)
  fer_res <- ferralic(pedon)

  tests <- list()
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness)
  tests$texture   <- test_texture_argic(h,
                                          candidate_layers = tests$thickness$layers)
  tests$not_argic    <- list(
    passed  = !isTRUE(arg_res$passed),
    layers  = if (isTRUE(arg_res$passed)) integer(0) else seq_len(nrow(h)),
    missing = arg_res$missing %||% character(0),
    details = list(argic_passed = arg_res$passed),
    notes   = if (isTRUE(arg_res$passed))
                "Excluded -- profile has an argic horizon" else NA_character_
  )
  tests$not_ferralic <- list(
    passed  = !isTRUE(fer_res$passed),
    layers  = if (isTRUE(fer_res$passed)) integer(0) else seq_len(nrow(h)),
    missing = fer_res$missing %||% character(0),
    details = list(ferralic_passed = fer_res$passed),
    notes   = if (isTRUE(fer_res$passed))
                "Excluded -- profile has a ferralic horizon" else NA_character_
  )

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "cambic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Cambic horizon"
  )
}


#' Plinthic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the plinthic horizon criteria.
#' Plinthite is Fe-rich material that hardens irreversibly on repeated
#' wetting and drying; the plinthic horizon is the diagnostic of
#' Plinthosols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 15).
#' @param min_plinthite_pct Minimum volume \% plinthite (default 15).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests:
#' \itemize{
#'   \item \code{\link{test_plinthite_concentration}} -- plinthite
#'         volume \% >= 15
#'   \item \code{\link{test_minimum_thickness}} -- thickness >= 15 cm
#' }
#'
#' v0.2 limitations: WRB 2022 also accepts profiles with >= 40\% red
#' Fe-rich mottles as alternative criterion -- not yet wired. The
#' "irreversibly hardens" criterion is conceptual and requires field
#' observation; v0.2 takes \code{plinthite_pct} as already representing
#' true plinthite (as opposed to soft mottles).
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Plinthic horizon.
#' @export
plinthic <- function(pedon, min_thickness = 15, min_plinthite_pct = 15) {
  h <- pedon$horizons

  tests <- list()
  tests$plinthite <- test_plinthite_concentration(h,
                                                    min_pct = min_plinthite_pct)
  tests$thickness <- test_minimum_thickness(h,
                                              min_cm           = min_thickness,
                                              candidate_layers = tests$plinthite$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "plinthic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Plinthic horizon"
  )
}


#' Spodic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the spodic horizon criteria. The
#' spodic horizon is an illuvial horizon with active Al + Fe oxalate-
#' extractable material plus organic matter; diagnostic of Podzols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 2.5).
#' @param min_alfe Minimum (Al_ox + 0.5 * Fe_ox) percent (default 0.5).
#' @param max_ph Maximum ph_h2o (default 5.9).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests:
#' \itemize{
#'   \item \code{\link{test_spodic_aluminum_iron}} -- (Al_ox +
#'         0.5*Fe_ox) >= 0.5\%
#'   \item \code{\link{test_ph_below}} -- ph_h2o <= 5.9
#'   \item \code{\link{test_minimum_thickness}} -- thickness >= 2.5 cm
#' }
#'
#' v0.2 limitations: the WRB color criterion (hue 5YR or yellower with
#' chroma <= 5, or specific dark colors) is not enforced. The
#' (Al_ox + Fe_ox)/clay >= 0.05 alternative ratio test is not yet wired.
#' Both deferred to v0.3.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Spodic horizon.
#' @export
spodic <- function(pedon,
                     min_thickness = 2.5,
                     min_alfe      = 0.5,
                     max_ph        = 5.9) {
  h <- pedon$horizons

  tests <- list()
  tests$alfe_oxalate <- test_spodic_aluminum_iron(h, min_pct = min_alfe)
  tests$ph           <- test_ph_below(h, max_ph = max_ph,
                                        candidate_layers = tests$alfe_oxalate$layers)
  tests$thickness    <- test_minimum_thickness(h,
                                                  min_cm           = min_thickness,
                                                  candidate_layers = tests$alfe_oxalate$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "spodic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Spodic horizon"
  )
}


#' Umbric horizon (WRB 2022)
#'
#' Tests for the umbric horizon -- a thick, dark, organic-rich surface
#' horizon like mollic, but with low base saturation (< 50\%).
#' Diagnostic of Umbrisols.
#'
#' Implementation reuses every mollic sub-test except the BS test,
#' which is inverted via \code{\link{test_bs_below}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm; default 20).
#' @param min_oc Minimum SOC \% (default 0.6).
#' @param max_bs Maximum base saturation \% (default 50; profile must
#'        be BELOW this).
#' @param surface_top_cm Maximum top_cm for surface-related layers
#'        (default 5).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Umbric horizon.
#' @export
umbric_horizon <- function(pedon,
                              min_thickness  = 20,
                              min_oc         = 0.6,
                              max_bs         = 50,
                              surface_top_cm = 5) {
  h <- pedon$horizons
  candidate_layers <- which(!is.na(h$top_cm) & h$top_cm <= surface_top_cm)

  tests <- list()
  tests$color           <- test_mollic_color(h, candidate_layers = candidate_layers)
  tests$organic_carbon  <- test_mollic_organic_carbon(h, min_pct = min_oc,
                                                        candidate_layers = candidate_layers)
  tests$bs_low          <- test_bs_below(h, max_pct = max_bs,
                                            candidate_layers = candidate_layers)
  tests$thickness       <- test_mollic_thickness(h, min_cm = min_thickness,
                                                    candidate_layers = candidate_layers)
  tests$structure       <- test_mollic_structure(h,
                                                    candidate_layers = candidate_layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "umbric_horizon",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Umbric horizon",
    notes     = if (length(candidate_layers) == 0L) {
                   "No surface-related candidate layers"
                 } else NA_character_
  )
}


#' Duric horizon (WRB 2022)
#'
#' Tests for >= 15\% volume of duripan nodules (Si-cemented) within a
#' horizon at least 15 cm thick. Diagnostic of Durisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm; default 15).
#' @param min_duripan_pct Minimum duripan volume \% (default 15).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' v0.3 limitations: petroduric (cemented continuous duripan)
#' detection is not yet implemented and will be added in v0.4.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Duric horizon.
#' @export
duric_horizon <- function(pedon, min_thickness = 15, min_duripan_pct = 15) {
  h <- pedon$horizons

  tests <- list()
  tests$duripan   <- test_duripan_concentration(h, min_pct = min_duripan_pct)
  tests$thickness <- test_minimum_thickness(h,
                                              min_cm           = min_thickness,
                                              candidate_layers = tests$duripan$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "duric_horizon",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Duric horizon"
  )
}


#' Natric horizon (WRB 2022)
#'
#' Tests for the natric horizon: an argic horizon with diagnostic
#' sodium accumulation (ESP >= 15\%) within at least one argic layer.
#' Diagnostic of Solonetz.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_esp Minimum ESP \% (default 15).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3, Natric horizon.
#' @export
natric_horizon <- function(pedon, min_esp = 15) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed)) {
    return(DiagnosticResult$new(
      name      = "natric_horizon",
      passed    = if (is.na(arg$passed)) NA else FALSE,
      layers    = integer(0),
      evidence  = list(argic = arg),
      missing   = arg$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 3, Natric horizon",
      notes     = "Profile lacks an argic horizon -- natric inapplicable"
    ))
  }

  h <- pedon$horizons
  layers <- arg$layers

  tests <- list(argic = arg)
  tests$esp_high <- test_esp_above(h, min_pct = min_esp,
                                      candidate_layers = layers)

  layer_lists <- list(arg$layers, tests$esp_high$layers)
  layers_passing <- Reduce(intersect, layer_lists)
  missing <- unique(unlist(lapply(tests, function(t) t$missing %||% character(0))))
  if (is.null(missing)) missing <- character(0)
  any_test_na <- any(vapply(tests, function(t) is.na(t$passed), logical(1)))
  passed <- if (length(layers_passing) > 0L) TRUE
            else if (any_test_na && length(missing) > 0L) NA
            else FALSE

  DiagnosticResult$new(
    name      = "natric_horizon",
    passed    = passed,
    layers    = layers_passing,
    evidence  = tests,
    missing   = missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Natric horizon"
  )
}


#' Nitic horizon (WRB 2022)
#'
#' Tests for the nitic horizon: a clay-rich (>= 30\%), Fe-rich
#' subsurface horizon at least 30 cm thick. Diagnostic of Nitisols.
#'
#' v0.3 simplification: WRB 2022 also requires polyhedral / nutty
#' structure with shiny ped surfaces and a gradual clay decrease with
#' depth (no clay maximum at the top of the B). v0.4 will add the
#' structural / depth-pattern tests.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_clay Minimum clay \% (default 30).
#' @param min_fe_dcb Minimum DCB-extractable Fe \% (default 4).
#' @param min_thickness Minimum thickness in cm (default 30).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3, Nitic horizon.
#' @export
nitic_horizon <- function(pedon, min_clay = 30, min_fe_dcb = 4,
                            min_thickness = 30) {
  h <- pedon$horizons
  tests <- list()
  tests$clay      <- test_clay_above(h, min_pct = min_clay)
  tests$fe_dcb    <- test_fe_dcb_above(h, min_pct = min_fe_dcb,
                                          candidate_layers = tests$clay$layers)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$fe_dcb$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "nitic_horizon",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Nitic horizon"
  )
}


#' Salic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the salic horizon criteria. The
#' salic horizon is a horizon of soluble-salt accumulation, diagnostic
#' for Solonchaks.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 15).
#' @param min_ec_dS_m Minimum electrical conductivity (dS/m) at 25C
#'        (default 15).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called:
#' \itemize{
#'   \item \code{\link{test_ec_concentration}} -- EC >= 15 dS/m.
#'   \item \code{\link{test_minimum_thickness}} -- thickness >= 15 cm.
#' }
#'
#' v0.2 limitations: the alternate WRB criterion (EC >= 8 dS/m if pH of
#' the saturated paste >= 8.5) is not implemented. Defer to v0.3.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3 -- Salic horizon.
#'
#' @export
salic <- function(pedon, min_thickness = 15, min_ec_dS_m = 15) {
  h <- pedon$horizons

  tests <- list()
  tests$ec        <- test_ec_concentration(h, min_dS_m = min_ec_dS_m)
  tests$thickness <- test_minimum_thickness(h,
                                              min_cm           = min_thickness,
                                              candidate_layers = tests$ec$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "salic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Salic horizon"
  )
}
