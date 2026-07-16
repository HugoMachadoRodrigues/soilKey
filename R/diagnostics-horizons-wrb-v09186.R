# =============================================================================
# WRB 2022 diagnostic horizons completed to 40/40 (v0.9.186).
#
# A gap audit against the official WRB 2022 Chapter 3.1 list (40 horizons; see
# inst/extdata/canonical/wrb2022_diagnostics.csv) found three horizons not yet
# exposed as diagnostic functions: Cohesic (3.1.7), Cryic (3.1.8) and Folic
# (3.1.12). This file adds them, faithful to the manual's diagnostic criteria,
# so coverage_report("wrb_horizons") reports a genuine, auditable 40/40.
# =============================================================================

#' Cohesic horizon (WRB 2022, Chapter 3.1.7)
#'
#' A subsurface horizon with a massive or weak subangular blocky structure,
#' poor in organic matter and iron oxides, with a kaolinite-dominated,
#' low-activity clay fraction, and hard when dry. It is the diagnostic basis of
#' the \emph{Cohesic} qualifier and corresponds to the \emph{caracter coeso} of
#' the Brazilian SiBCS (typical of the coastal "Tabuleiros" Latossolos and
#' Argissolos).
#'
#' Diagnostic criteria (all required): mineral material with (1) < 0.5\% soil
#' organic carbon; (2) >= 15\% clay; (3) a CEC (by 1 M NH4OAc, pH 7) of
#' < 24 cmol_c per kg clay; (4) a massive or weak subangular blocky structure;
#' (5) not cemented; (6) a rupture-resistance class, when dry, of at least hard;
#' and (7) a thickness of >= 10 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_oc Maximum soil organic carbon (\%), default 0.5.
#' @param min_clay Minimum clay (\%), default 15.
#' @param max_cec_per_clay Maximum CEC in cmol_c per kg clay, default 24.
#' @param min_thickness Minimum thickness in cm, default 10.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.1.7, Cohesic horizon.
#' @export
cohesic <- function(pedon, max_oc = 0.5, min_clay = 15,
                    max_cec_per_clay = 24, min_thickness = 10) {
  h <- pedon$horizons
  n <- nrow(h)
  tests <- list()

  # (1) < 0.5% soil organic carbon (a low-humus subsurface horizon).
  tests$low_oc <- .subtest_result(
    passed  = if (all(is.na(h$oc_pct))) NA
              else if (any(!is.na(h$oc_pct) & h$oc_pct < max_oc)) TRUE else FALSE,
    layers  = which(!is.na(h$oc_pct) & h$oc_pct < max_oc),
    missing = if (all(is.na(h$oc_pct))) "oc_pct" else character(0),
    details = list(max_oc = max_oc))

  # (2) >= 15% clay ; (3) CEC (NH4OAc) < 24 cmolc/kg clay (low-activity clay).
  tests$clay <- test_clay_above(h, min_pct = min_clay)
  tests$cec_per_clay <- test_cec_per_clay(h, max_cmol_per_kg_clay = max_cec_per_clay)

  # (4) massive OR weak subangular blocky structure.
  gr <- tolower(as.character(h$structure_grade))
  ty <- tolower(as.character(h$structure_type))
  massive  <- grepl("massive|structureless", ty) | grepl("massive|structureless", gr)
  weak_sab <- grepl("weak", gr) & grepl("subangular", ty)
  struct_ok <- massive | weak_sab
  tests$structure <- .subtest_result(
    passed  = if (all(is.na(gr) & is.na(ty))) NA
              else if (any(struct_ok, na.rm = TRUE)) TRUE else FALSE,
    layers  = which(isTRUE_vec(struct_ok)),
    missing = if (all(is.na(gr) & is.na(ty))) "structure_type" else character(0),
    details = list(rule = "massive or weak subangular blocky"))

  # (5) not cemented.
  cc <- tolower(trimws(as.character(h$cementation_class)))
  not_cemented <- is.na(cc) | cc %in% c("", "none", "non-cemented", "noncemented",
                                        "not cemented", "uncemented")
  tests$not_cemented <- .subtest_result(
    passed  = if (any(not_cemented)) TRUE else FALSE,
    layers  = which(not_cemented),
    missing = character(0),
    details = list(rule = "not cemented"))

  # (6) rupture-resistance, when dry, at least hard.
  rr <- tolower(trimws(as.character(h$rupture_resistance)))
  hard_dry <- rr %in% c("hard", "very hard", "extremely hard", "rigid")
  tests$hard <- .subtest_result(
    passed  = if (all(is.na(rr))) NA
              else if (any(hard_dry, na.rm = TRUE)) TRUE else FALSE,
    layers  = which(isTRUE_vec(hard_dry)),
    missing = if (all(is.na(rr))) "rupture_resistance" else character(0),
    details = list(rule = "rupture-resistance (dry) >= hard"))

  # (7) thickness >= 10 cm, over the layers meeting the point criteria above.
  cand <- Reduce(intersect, list(tests$low_oc$layers, tests$clay$layers,
                                 tests$cec_per_clay$layers, tests$structure$layers,
                                 tests$not_cemented$layers, tests$hard$layers))
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                            candidate_layers = cand)

  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "cohesic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.7, Cohesic horizon",
    notes = "v0.9.186: kaolinitic, hard-setting subsurface horizon; SiBCS caracter coeso.")
}


#' Folic horizon (WRB 2022, Chapter 3.1.12)
#'
#' A surface horizon of \strong{well-aerated} organic material, i.e. organic
#' material that is \emph{not} water-saturated for long periods. It is the
#' aerobic twin of the \code{\link{histic_horizon}}: both consist of organic
#' material, but the histic horizon is water-saturated for >= 30 consecutive
#' days in most years while the folic horizon is saturated for < 30 days and is
#' not drained.
#'
#' Diagnostic criteria (all required): organic material that (1) is saturated
#' with water for < 30 consecutive days in most years and is not drained; and
#' (2) has a thickness of >= 10 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_oc Minimum soil organic carbon (\%) for organic material,
#'        default 12 (matching \code{\link{histic_horizon}}).
#' @param max_saturation_days Maximum consecutive water-saturation days that
#'        still count as well-aerated, default 30.
#' @param min_thickness Minimum thickness in cm, default 10.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.1.12, Folic horizon.
#' @export
folic <- function(pedon, min_oc = 12, max_saturation_days = 30,
                  min_thickness = 10) {
  h <- pedon$horizons
  tests <- list()

  # (organic material) -- same OC proxy as the histic horizon.
  tests$organic <- test_oc_above(h, min_pct = min_oc)

  # (1) well-aerated: saturated < 30 consecutive days in most years, not drained.
  # A drained layer is excluded (it is well-aerated only by artificial drainage);
  # `layer_origin`/drainage status is not carried in the schema, so this uses the
  # water-saturation window and treats a missing value as unconfirmed (NA).
  wsd <- suppressWarnings(as.numeric(h$water_saturation_days))
  aerated <- !is.na(wsd) & wsd < max_saturation_days
  tests$aerated <- .subtest_result(
    passed  = if (all(is.na(wsd))) NA
              else if (any(aerated)) TRUE else FALSE,
    layers  = which(aerated),
    missing = if (all(is.na(wsd))) "water_saturation_days" else character(0),
    details = list(max_saturation_days = max_saturation_days))

  # (2) thickness >= 10 cm over the organic, well-aerated layers.
  cand <- intersect(tests$organic$layers, tests$aerated$layers)
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                            candidate_layers = cand)

  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "folic", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.12, Folic horizon",
    notes = "v0.9.186: aerobic twin of the histic horizon (< 30 days saturation).")
}


#' Cryic horizon (WRB 2022, Chapter 3.1.8)
#'
#' A perennially frozen horizon in mineral or organic material (permafrost).
#' The permafrost logic is shared with \code{\link{cryic_conditions}}; this
#' exposes it under the canonical WRB 2022 horizon name so that
#' \code{coverage_report("wrb_horizons")} reports it, and returns a
#' \code{DiagnosticResult} named \code{"cryic"}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Passed to \code{\link{cryic_conditions}} (e.g. \code{max_top_cm},
#'        \code{max_temp_C}).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.1.8, Cryic horizon.
#' @export
cryic_horizon <- function(pedon, ...) {
  r <- cryic_conditions(pedon, ...)
  DiagnosticResult$new(
    name = "cryic", passed = r$passed, layers = r$layers,
    evidence = r$evidence, missing = r$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.8, Cryic horizon")
}


# Small vectorised helper: TRUE only where x is TRUE (NA -> FALSE), for which().
isTRUE_vec <- function(x) !is.na(x) & x
