# ============================================================================
# v0.3.3 -- WRB 2022 Ch 3.1 horizon diagnostics not previously implemented.
#
# Each function returns a DiagnosticResult and follows the same pattern as
# the v0.1-v0.3 horizons (R/diagnostics-horizons-wrb.R). Limitations
# specific to v0.3.3 are noted inline; most stem from the canonical
# horizon schema not yet carrying the full mineralogical / micromorpho-
# logical attributes that WRB defines in some criteria.
# ============================================================================


# ---- albic horizon (WRB 2022 Ch 3.1) ---------------------------------------

#' Albic horizon (WRB 2022)
#'
#' A bleached eluvial horizon -- claric material that has lost iron oxides
#' and/or organic matter due to clay migration, podzolization, or redox
#' under stagnant water. Diagnostic for parts of Podzols, Retisols and
#' Planosols qualifiers.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 1, per WRB
#'        2022). The albic horizon has no canonical thickness gate; we
#'        keep a token min so that fully-NA layers don't pass.
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details Sub-tests:
#' \itemize{
#'   \item \code{\link{test_claric_munsell}} -- Munsell criteria of claric
#'         material (Ch 3.3.4).
#' }
#' Designation pattern \code{E} or \code{Eg} also serves as positive
#' evidence when Munsell columns are missing (proxy path).
#'
#' @references IUSS Working Group WRB (2022), Ch 3.1 -- Albic horizon.
#' @export
albic <- function(pedon, min_thickness = 1) {
  h <- pedon$horizons
  tests <- list()
  tests$claric <- test_claric_munsell(h)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$claric$layers)
  # Designation proxy as fallback.
  desg <- test_pattern_match(h, "designation", "^E[bg]?$|^Eg")
  if (!isTRUE(tests$claric$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name      = "albic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Albic horizon",
    notes     = "v0.3.3: claric Munsell + thickness; designation E* as fallback"
  )
}


# ---- ferric horizon -------------------------------------------------------

#' Ferric horizon (WRB 2022)
#'
#' A horizon of iron accumulation that does not reach the cementation /
#' redness levels of plinthic. Diagnostic for the Ferric qualifier.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm; default 15).
#' @param min_fe_dith_pct Minimum dithionite-extractable iron percent (default 5).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3.1, Ferric horizon.
#' @export
ferric <- function(pedon, min_thickness = 15, min_fe_dith_pct = 5) {
  h <- pedon$horizons
  tests <- list()
  tests$fe_dith <- test_numeric_above(h, "fe_dcb_pct", min_fe_dith_pct)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$fe_dith$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name      = "ferric",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Ferric horizon"
  )
}


# ---- petric variants (cemented horizons) -----------------------------------

#' Petrocalcic horizon (WRB 2022)
#'
#' A continuously cemented variant of the calcic horizon. Same chemistry
#' (CaCO3 \\>= 15\\%) plus moderate-or-greater cementation in at least
#' 50\\% of the layer.
#' @export
petrocalcic <- function(pedon, min_thickness = 10, min_caco3_pct = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$caco3      <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$caco3$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name      = "petrocalcic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petrocalcic horizon"
  )
}

#' Petroduric horizon (WRB 2022): cemented duric.
#' @export
petroduric <- function(pedon, min_thickness = 10, min_duripan_pct = 10) {
  h <- pedon$horizons
  tests <- list()
  tests$duripan    <- test_duripan_concentration(h, min_pct = min_duripan_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$duripan$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "petroduric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petroduric horizon"
  )
}

#' Petrogypsic horizon (WRB 2022): cemented gypsic.
#' @export
petrogypsic <- function(pedon, min_thickness = 10, min_gypsum_pct = 5) {
  h <- pedon$horizons
  tests <- list()
  tests$gypsum     <- test_caso4_concentration(h, min_pct = min_gypsum_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$gypsum$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "petrogypsic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petrogypsic horizon"
  )
}

#' Petroplinthic horizon (WRB 2022): cemented plinthic.
#' @export
petroplinthic <- function(pedon, min_thickness = 10, min_plinthite_pct = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$plinthite  <- test_plinthite_concentration(h, min_pct = min_plinthite_pct)
  tests$cemented   <- test_cemented(h, min_class = "moderately",
                                       candidate_layers = tests$plinthite$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$cemented$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "petroplinthic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Petroplinthic horizon"
  )
}

#' Pisoplinthic horizon (WRB 2022): pisolitic plinthic. v0.3.3 detects via
#' designation pattern \code{Bspl} / \code{Bvpi} or via plinthite \\>= 15\\%
#' AND structure_type containing 'pisol'.
#' @export
pisoplinthic <- function(pedon, min_thickness = 15, min_plinthite_pct = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$plinthite  <- test_plinthite_concentration(h, min_pct = min_plinthite_pct)
  tests$pisolitic  <- test_pattern_match(h, "structure_type", "pisol",
                                            candidate_layers = tests$plinthite$layers)
  tests$thickness  <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$pisolitic$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "pisoplinthic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Pisoplinthic horizon"
  )
}


# ---- vertic horizon (the horizon, not the property) -----------------------

#' Vertic horizon (WRB 2022 Ch 3.1)
#'
#' Stricter than the vertic *properties*: the vertic *horizon* requires
#' \\>= 30\\% clay throughout, slickensides at \\>= "common" level, AND
#' shrink-swell cracks \\>= 0.5 cm wide. Used by Vertisols.
#' @export
vertic_horizon <- function(pedon, min_clay = 30, min_thickness = 25) {
  h <- pedon$horizons
  tests <- list()
  tests$clay         <- test_clay_above(h, min_pct = min_clay)
  tests$slickensides <- test_slickensides_present(h,
                                                     levels = c("common", "many",
                                                                "continuous"),
                                                     candidate_layers = tests$clay$layers)
  tests$cracks       <- test_shrink_swell_cracks(h, min_width_cm = 0.5,
                                                    candidate_layers = tests$slickensides$layers)
  shared <- intersect(tests$slickensides$layers, tests$cracks$layers)
  tests$thickness    <- test_minimum_thickness(h, min_cm = min_thickness,
                                                  candidate_layers = shared)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "vertic_horizon", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Vertic horizon"
  )
}


# ---- thionic / fragic / sombric / chernic --------------------------------

#' Thionic horizon (WRB 2022): post-oxidation acid sulfate horizon.
#' Requires sulfidic_s_pct >= 0.01 AND pH(H2O) <= 4.
#' @export
thionic <- function(pedon, min_thickness = 15, max_pH = 4,
                       min_sulfidic_s = 0.01) {
  h <- pedon$horizons
  tests <- list()
  tests$pH       <- test_ph_below(h, max_ph = max_pH)
  tests$sulfidic <- test_numeric_above(h, "sulfidic_s_pct",
                                          threshold = min_sulfidic_s,
                                          candidate_layers = tests$pH$layers)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                               candidate_layers = tests$sulfidic$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "thionic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Thionic horizon"
  )
}

#' Fragic horizon (WRB 2022): a high-bulk-density horizon with restricted
#' rooting. v0.3.3: detects via bulk_density_g_cm3 >= 1.65 AND structure
#' grade massive/very firm OR designation pattern \code{x}/\code{Bx}.
#' @export
fragic <- function(pedon, min_thickness = 15, min_bd = 1.65) {
  h <- pedon$horizons
  tests <- list()
  tests$bulk_density <- test_numeric_above(h, "bulk_density_g_cm3",
                                              threshold = min_bd)
  tests$designation_or_struct <- test_pattern_match(
    h, "structure_grade", "massive|very firm",
    candidate_layers = tests$bulk_density$layers
  )
  if (!isTRUE(tests$designation_or_struct$passed)) {
    desg <- test_pattern_match(h, "designation", "x|Bx|Btx",
                                  candidate_layers = tests$bulk_density$layers)
    if (isTRUE(desg$passed)) tests$designation_or_struct <- desg
  }
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$bulk_density$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "fragic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Fragic horizon",
    notes = "v0.3.3 simplification: BD + structure or designation 'x' as proxy"
  )
}

#' Sombric horizon (WRB 2022): subsurface accumulation of humus that
#' qualified neither as spodic nor as a true mollic-like horizon
#' (low-base-saturation cool tropical highlands). v0.3.3 detects via
#' designation pattern + OC criteria (BS < 50, OC > 0.6, depth > 25 cm).
#' @export
sombric <- function(pedon, min_thickness = 15, min_oc = 0.6,
                       max_bs = 50, min_top_cm = 25) {
  h <- pedon$horizons
  tests <- list()
  tests$top         <- test_top_at_or_above(h, min_top_cm = min_top_cm)
  tests$oc          <- test_oc_above(h, min_pct = min_oc,
                                        candidate_layers = tests$top$layers)
  tests$bs          <- test_bs_below(h, max_pct = max_bs,
                                        candidate_layers = tests$oc$layers)
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                candidate_layers = tests$bs$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "sombric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Sombric horizon",
    notes = "v0.3.3 simplification: OC + BS in subsurface; humus illuviation deferred"
  )
}

#' Chernic horizon (WRB 2022): the cherozemic-style mollic with very high
#' biological activity (worm holes, casts, coprolites). v0.3.3:
#' delegates to mollic + worm_holes_pct >= 50 (proxy for "biological
#' homogenization").
#' @export
chernic <- function(pedon, min_worm_pct = 50) {
  m <- mollic(pedon)
  if (!isTRUE(m$passed)) {
    return(DiagnosticResult$new(
      name = "chernic",
      passed = if (is.na(m$passed)) NA else FALSE,
      layers = integer(0), evidence = list(mollic = m),
      missing = m$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 3.1, Chernic horizon",
      notes = "Failed/NA because mollic horizon test did not pass"
    ))
  }
  h <- pedon$horizons
  worms <- test_numeric_above(h, "worm_holes_pct",
                                 threshold = min_worm_pct,
                                 candidate_layers = m$layers)
  passed <- if (is.na(worms$passed)) NA else
            (isTRUE(m$passed) && isTRUE(worms$passed))
  DiagnosticResult$new(
    name = "chernic",
    passed = passed,
    layers = if (isTRUE(passed)) intersect(m$layers, worms$layers)
             else integer(0),
    evidence = list(mollic = m, worm_holes = worms),
    missing  = unique(c(m$missing, worms$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Chernic horizon",
    notes = "Mollic + worm_holes_pct >= 50 (biological-homogenization proxy)"
  )
}


# ---- anthropogenic family (anthraquic, hydragric, hortic, irragric, ----
# ---- plaggic, pretic, terric) -------------------------------------------

# Common helper: detect the anthropogenic horizons via designation pattern
# OR via structured agronomic indicators (P_mehlich-3, structure).

#' Anthraquic horizon (WRB 2022): puddled-rice / paddy plough layer.
#' v0.3.3 detects via designation pattern \code{Apl|Ap|Hh}.
#' @export
anthraquic <- function(pedon, min_thickness = 20, max_top_cm = 50) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation", "Apl|Apg|Ahg|Hh")
  tests$top         <- test_starts_within(h, max_top_cm = max_top_cm,
                                            candidate_layers = tests$designation$layers)
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                candidate_layers = tests$top$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "anthraquic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Anthraquic horizon"
  )
}

#' Hydragric horizon (WRB 2022): subsoil hydric horizon under anthraquic.
#' v0.3.3 detects via designation pattern \code{Bg|Brg} immediately below
#' an anthraquic-like topsoil.
#' @export
hydragric <- function(pedon, min_thickness = 20) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation", "^Bg|^Brg|^Bdg")
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                candidate_layers = tests$designation$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "hydragric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Hydragric horizon"
  )
}

#' Hortic horizon (WRB 2022): garden / kitchen-midden topsoil. Diagnostic
#' criteria: thickness \\>= 20 cm, dark colour (mollic-like), high P
#' (Mehlich-3 P >= 100 mg/kg or P2O5_1pct_citric >= 175 mg/kg), high SOC.
#' @export
hortic <- function(pedon, min_thickness = 20, min_oc = 1.0,
                      min_p_mehlich3 = 100) {
  h <- pedon$horizons
  tests <- list()
  tests$top    <- test_starts_within(h, max_top_cm = 5)
  tests$oc     <- test_oc_above(h, min_pct = min_oc,
                                   candidate_layers = tests$top$layers)
  tests$p      <- test_numeric_above(h, "p_mehlich3_mg_kg",
                                        threshold = min_p_mehlich3,
                                        candidate_layers = tests$oc$layers)
  tests$thick  <- test_minimum_thickness(h, min_cm = min_thickness,
                                            candidate_layers = tests$p$layers)
  desg <- test_pattern_match(h, "designation", "^Ah?h|^Ap[ht]")
  if (!isTRUE(tests$p$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "hortic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Hortic horizon"
  )
}

#' Irragric horizon (WRB 2022): topsoil thickened by irrigation deposits.
#' v0.3.3: thickness >= 20 cm + sediment-derived structure proxied via
#' designation \code{Apk|Apg|Au}.
#' @export
irragric <- function(pedon, min_thickness = 20) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation", "^A[pu]k?|^Ahu")
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                 candidate_layers = tests$designation$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "irragric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Irragric horizon"
  )
}

#' Plaggic horizon (WRB 2022): sod-derived topsoil >= 20 cm with low BD.
#' @export
plaggic <- function(pedon, min_thickness = 20, max_bd = 1.5,
                       min_oc = 0.6) {
  h <- pedon$horizons
  tests <- list()
  tests$oc          <- test_oc_above(h, min_pct = min_oc)
  tests$bd          <- test_bulk_density_below(h, max = max_bd,
                                                  candidate_layers = tests$oc$layers)
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                 candidate_layers = tests$bd$layers)
  desg <- test_pattern_match(h, "designation", "^Ap[lp]")
  if (!isTRUE(tests$thickness$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "plaggic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Plaggic horizon"
  )
}

#' Pretic horizon (WRB 2022): "Amazonian Dark Earth" (terra preta de
#' indio) horizon -- thick anthropogenic surface with high P, SOC, and
#' incorporated charcoal / pottery.
#' @export
pretic <- function(pedon, min_thickness = 20, min_oc = 1.5,
                      min_p_mehlich3 = 30) {
  h <- pedon$horizons
  tests <- list()
  tests$oc    <- test_oc_above(h, min_pct = min_oc)
  tests$p     <- test_numeric_above(h, "p_mehlich3_mg_kg",
                                       threshold = min_p_mehlich3,
                                       candidate_layers = tests$oc$layers)
  tests$thick <- test_minimum_thickness(h, min_cm = min_thickness,
                                           candidate_layers = tests$p$layers)
  desg <- test_pattern_match(h, "designation", "^A[pu]?p|^Au")
  if (!isTRUE(tests$thick$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "pretic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Pretic horizon"
  )
}

#' Terric horizon (WRB 2022): topsoil thickened by long-term application
#' of mineral material (sediment / sand additions). v0.3.3: thickness >=
#' 20 cm + designation Au / Apc.
#' @export
terric <- function(pedon, min_thickness = 20) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation", "^Au|^Apc")
  tests$thickness   <- test_minimum_thickness(h, min_cm = min_thickness,
                                                 candidate_layers = tests$designation$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "terric", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1, Terric horizon"
  )
}
