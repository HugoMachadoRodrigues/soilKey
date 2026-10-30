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
#' @param system One of \code{"wrb2022"} (default) or \code{"usda"}.
#'        Selects the clay-increase threshold set: WRB uses
#'        6/1.4/20 pp/ratio/pp; KST 13ed uses 3/1.2/8 (looser).
#'        See \code{test_clay_increase_argic} for the table.
#' @param engine v0.9.63+. One of \code{"soilkey"} (the hand-coded
#'        path, default for back-compat) or \code{"aqp"} (canonical
#'        NRCS dispatch via \code{aqp::getArgillicBounds}). When
#'        \code{NULL} (the new default) the function reads
#'        \code{getOption("soilKey.diagnostic_engine", "soilkey")}
#'        so a global \code{options(soilKey.diagnostic_engine = "aqp")}
#'        flips every \code{argic()} call without modifying call
#'        sites. See \code{\link{argic_aqp}}.
#' @param require_t v0.9.63+. Forwarded to \code{aqp::getArgillicBounds}
#'        when \code{engine = "aqp"}: \code{TRUE} requires a "t"
#'        suffix in the horizon designation (the strict KST 13ed
#'        text); \code{FALSE} accepts argic by clay-increase alone
#'        (more permissive on data-sparse profiles). \code{NULL}
#'        (default) auto-picks: \code{TRUE} for \code{system =
#'        "usda"}, \code{FALSE} for \code{system = "wrb2022"}.
#'        Ignored when \code{engine = "soilkey"}.
#' @param lithic_discontinuity v0.9.220. Apply WRB 2022 criterion 2.a.i:
#'        a clay increase over a coarser layer of another material (Ap over
#'        2Bt, read from the designations' material numbers) counts only
#'        with clay films of "common" or more (illuvial clay, criterion 2.b).
#'        \code{NULL} (default): \code{TRUE} for \code{system = "wrb2022"}.
#'        The SiBCS B textural, whose item (j) accepts such an increase by
#'        its textural ratio, and the USDA keys pass \code{FALSE}.
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called (each a list with \code{passed}, \code{layers},
#' \code{missing}, \code{details}, \code{notes}):
#' \itemize{
#'   \item \code{test_clay_increase_argic} -- the three-pronged
#'         WRB 2022 clay-increase rule.
#'   \item \code{test_minimum_thickness} -- thickness >= 7.5 cm
#'         (configurable via \code{min_thickness}).
#'   \item \code{test_texture_argic} -- texture of sandy loam or
#'         finer (\code{silt + 2 * clay >= 30}).
#' }
#' Until v0.9.219 a glossic, albeluvic or retic designation anywhere
#' voided the argic horizon (the WRB 2014 Albeluvisol logic); in WRB 2022
#' retic properties belong to an argic or natric horizon.
#'
#' v0.9.310, for \code{system = "wrb2022"} (WRB 2022 Ch 3.1.3, p. 35-36):
#' \itemize{
#'   \item criterion 1: the layers have \eqn{\ge} 8\% clay;
#'   \item criterion 2.a.i (v0.9.317): the coarser-textured layer is not
#'         separated from the layer by a lithic discontinuity, read from the
#'         material numbers of the designations and from the data of each
#'         pair of layers in between (Ch 3.2.7, p. 76-77: the ratio sand to
#'         silt, criterion 2.a.iv; coarse fragments, criterion 6);
#'   \item criterion 2.a.ii (v0.9.317): no clay increase counts below a plough
#'         layer (Ap) that directly overlies the argic horizon;
#'   \item criterion 2.a.iii: a layer is compared with a coarser-textured layer
#'         above it only across a transitional horizon of \eqn{\le} 15 cm;
#'         layers already accepted and layers whose designation has the
#'         suffix t are part of the argic horizon, not transitional;
#'   \item criterion 2.b.ii: clay films of "common" or more (clay coatings on
#'         \eqn{\ge} 15\% of the aggregate surfaces) make a layer argic
#'         without a clay increase;
#'   \item a subsurface horizon: the layer at the mineral soil surface and
#'         the A horizons are not part of it.
#' }
#' The USDA argillic and order keys and the SiBCS B textural keep the earlier
#' reading.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3 -- Argic horizon.
#'
#' @export
argic <- function(pedon, min_thickness = 7.5,
                    system = c("wrb2022", "usda"),
                    engine = NULL,
                    require_t = NULL,
                    lithic_discontinuity = NULL) {
  system <- match.arg(system)
  # v0.9.310: the WRB argic reads the WRB 2022 text of Ch 3.1.3 (criteria 1,
  # 2.a.iii and 2.b.ii; a subsurface horizon), see .argic_impl(). The USDA
  # argillic, the USDA orders and the SiBCS B textural call .argic_impl()
  # without it, so that those keys keep their reading.
  .argic_impl(pedon, min_thickness = min_thickness, system = system,
              engine = engine, require_t = require_t,
              lithic_discontinuity = lithic_discontinuity,
              wrb2022_text = system == "wrb2022")
}

#' The argic test. \code{wrb2022_text = TRUE} adds the readings of the WRB 2022
#' text made in v0.9.310 (see the comments below); \code{argic()} sets it for
#' \code{system = "wrb2022"}. The callers of the other systems use the default.
#' @noRd
.argic_impl <- function(pedon, min_thickness = 7.5,
                        system = c("wrb2022", "usda"),
                        engine = NULL,
                        require_t = NULL,
                        lithic_discontinuity = NULL,
                        wrb2022_text = FALSE) {
  system <- match.arg(system)
  # v0.9.220: WRB 2022 criterion 2.a.i (see below) for the WRB argic only; the
  # SiBCS B textural (item j) and the USDA keys call this with FALSE.
  if (is.null(lithic_discontinuity)) lithic_discontinuity <- system == "wrb2022"
  # v0.9.63: engine resolution order:
  #   1. explicit `engine` argument
  #   2. R option soilKey.diagnostic_engine (e.g. "aqp")
  #   3. default "soilkey" (back-compat)
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))
  # v0.9.63: optional dispatch to aqp::getArgillicBounds() via
  # `argic_aqp()`. Default `engine = "soilkey"` preserves the
  # historical hand-coded behavior (WRB 6/1.4/20 thresholds) for
  # back-compatibility. `engine = "aqp"` uses the canonical NRCS
  # KST 13ed tiered thresholds (clay <15%: +3pp; 15-40%: 1.2x;
  # >=40%: +8pp) -- recommended for benchmarks against KSSL+NASIS
  # / FEBR / BDsolos because the v0.9.62 RJ A/B showed it is
  # ~15 pp stricter on argic, which better aligns with the SiBCS
  # Latossolo / Argissolo / Cambissolo boundary.
  if (engine == "aqp") {
    rt <- require_t %||% (system == "usda")
    res <- argic_aqp(pedon, require_t = rt)
    res$reference <- sprintf("%s [engine=aqp, system=%s]",
                                 res$reference, system)
    # v0.9.90: apply the designation-inference fallback when the
    # canonical aqp test failed AND engine="aqp" implies the user
    # accepts data-quality-aware morphology paths.
    if (!isTRUE(res$passed)) {
      designation_inference_opt <- getOption(
        "soilKey.argic_designation_inference", NULL)
      use_inference <- if (!is.null(designation_inference_opt)) {
        isTRUE(designation_inference_opt)
      } else {
        TRUE
      }
      if (use_inference) {
        h <- pedon$horizons
        desg_chr <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
        films    <- as.character(h$clay_films_amount %||% rep(NA_character_, nrow(h)))
        has_bt    <- !is.na(desg_chr) & grepl("^Bt", desg_chr)
        has_films <- !is.na(films)    & nzchar(trimws(films))
        deep      <- !is.na(h$top_cm) & h$top_cm > 25
        inferred_layers <- which(has_bt & has_films & deep)
        if (length(inferred_layers) > 0L) {
          ev <- res$evidence %||% list()
          ev$designation_inference <- list(
            passed = TRUE, layers = inferred_layers,
            details = list(source = "engine_aqp_bt_with_films",
                              note   = paste0("v0.9.90: aqp argic test failed but ",
                                               "Bt designation + clay films + ",
                                               "subsoil depth accepted as argic ",
                                               "morphology"))
          )
          res$evidence <- ev
          res$passed <- TRUE
          res$layers <- inferred_layers
        }
      }
    }
    return(res)
  }
  h <- pedon$horizons

  # v0.9.310: "An argic horizon ... is a subsurface horizon" (WRB 2022 Ch 3.1.3,
  # p. 35). The layer at the mineral soil surface and the A horizons (Ah2, Ap2)
  # are not part of it (Hugo's reading, 2026-10-29).
  not_sub <- if (wrb2022_text) .wrb_not_subsurface(h) else rep(FALSE, nrow(h))
  # v0.9.310: criterion 2.a.iii, a transitional horizon of <= 15 cm at most
  # between the coarser-textured layer and the argic horizon (p. 36).
  max_transition <- if (wrb2022_text) 15 else NULL

  tests <- list()
  # v0.9.26: per-system clay-increase thresholds. WRB 2022 (default)
  # uses 6/1.4/20; KST 13ed uses 3/1.2/8 (looser).
  # v0.9.317: criteria 2.a.i and 2.a.ii as WRB 2022 words them (p. 36): a
  # lithic discontinuity is also read from the data (Ch 3.2.7), and a plough
  # layer directly above leaves the clay increase without effect.
  tests$clay_increase <- test_clay_increase_argic(h, system = system,
                                                  lithic_aware = lithic_discontinuity,
                                                  max_transition_cm = max_transition,
                                                  exclude = not_sub,
                                                  lithic_data = wrb2022_text,
                                                  plough_layer = wrb2022_text)
  # v0.9.220: WRB 2022 Ch 3.1.3, criterion 2.a.i. A clay increase across a
  # lithic discontinuity (Ap over 2Bt) is not an argic horizon unless "illuvial
  # clay is evidenced" (criterion 2.b; WRB: "if the soil shows a lithic
  # discontinuity directly over the argic horizon ... the illuvial nature must
  # be clearly established"), read here as clay films ("cerosidade") of
  # "common" or more (2.b.ii: coatings on >= 15% of the aggregate surfaces).
  if (lithic_discontinuity) {
    across <- setdiff(test_clay_increase_argic(h, system = system,
                                               max_transition_cm = max_transition,
                                               exclude = not_sub,
                                               plough_layer = wrb2022_text)$layers,
                      tests$clay_increase$layers)
    if (length(across)) {
      films <- .clay_films_class(h$clay_films_amount %||% rep(NA_character_, nrow(h)))
      kept <- across[!is.na(films[across]) & films[across] >= 3L]
      tests$clay_increase$across_lithic_discontinuity <-
        list(layers = across, kept_by_clay_films = kept)
      if (length(kept)) {
        tests$clay_increase$layers <- sort(union(tests$clay_increase$layers, kept))
        tests$clay_increase$passed <- TRUE
      }
    }
  }

  candidate_layers <- tests$clay_increase$layers
  layer_tests <- c("clay_increase", "thickness", "texture")

  if (wrb2022_text) {
    # v0.9.310: criterion 2.b.ii, without a clay increase: "clay coatings
    # covering >= 15% of the surfaces of soil aggregates, coarse fragments
    # and/or biopore walls" (p. 36), read as clay films of "common" or more in
    # a subsurface layer (patchy cutans are "few" and do not count). Clay films
    # that are not recorded leave the criterion unmet, not undecided: the other
    # forms of 2.b (clay bridges, thin sections, fine clay) are not recorded
    # either.
    films  <- .clay_films_class(h$clay_films_amount %||% rep(NA_character_, nrow(h)))
    coated <- which(!is.na(films) & films >= 3L & !not_sub)
    tests$illuvial_clay <- .subtest_result(
      passed = length(coated) > 0L, layers = coated,
      notes  = "WRB 2022 Ch 3.1.3 criterion 2.b.ii: clay films common or more")
    candidate_layers <- sort(union(candidate_layers, coated))
    tests$criterion_2 <- .subtest_result(
      passed  = if (length(candidate_layers)) TRUE else tests$clay_increase$passed,
      layers  = candidate_layers,
      missing = tests$clay_increase$missing)
    # v0.9.310: criterion 1, ">= 8% clay" (p. 36).
    tests$clay_min <- test_clay_above(h, min_pct = 8,
                                      candidate_layers = candidate_layers)
    layer_tests <- c("criterion_2", "thickness", "texture", "clay_min")
  }

  tests$thickness     <- test_minimum_thickness(h,
                                                  min_cm           = min_thickness,
                                                  candidate_layers = candidate_layers)
  tests$texture       <- test_texture_argic(h,
                                              candidate_layers = candidate_layers)
  # v0.9.220: no albeluvic exclusion. Until v0.9.219 a "glossic", "albeluvic"
  # or "retic" designation anywhere voided the argic horizon (the WRB 2014
  # Albeluvisol logic); in WRB 2022 retic properties belong to an argic or
  # natric horizon (Ch 3.2.11, criterion 1) and Retisols have one (Ch 4).

  agg <- aggregate_subtests(tests, layer_tests = layer_tests)

  # v0.9.90 -- Designation-inference fallback (opt-in, auto-on under
  # engine="aqp"). BDsolos / SOTERLAC profiles routinely report only 2
  # depth samples (a topsoil A and a deep B at, say, 50-150 cm) so the
  # numeric clay-increase test undercounts the actual A->B clay gradient.
  # Surveyors who already labelled the deep B as "Bt*" and recorded
  # ANY clay films qualifier are signalling argic by morphology even
  # when the 2-point clay difference falls below the canonical
  # thresholds. Accept those layers as argic when:
  #   * canonical clay-increase failed at this layer, AND
  #   * designation matches "Bt" prefix, AND
  #   * clay_films_amount has a non-empty qualifier, AND
  #   * top_cm > 25 (subsoil context, not topsoil).
  designation_inference_opt <- getOption(
    "soilKey.argic_designation_inference", NULL)
  designation_inference <- if (!is.null(designation_inference_opt)) {
    isTRUE(designation_inference_opt)
  } else {
    identical(getOption("soilKey.diagnostic_engine", "soilkey"), "aqp")
  }
  inferred_layers <- integer(0)
  if (designation_inference && !isTRUE(agg$passed)) {
    desg_chr <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
    films    <- as.character(h$clay_films_amount %||% rep(NA_character_, nrow(h)))
    has_bt    <- !is.na(desg_chr) & grepl("^Bt", desg_chr)
    has_films <- !is.na(films)    & nzchar(trimws(films))
    deep      <- !is.na(h$top_cm) & h$top_cm > 25
    inferred_layers <- which(has_bt & has_films & deep)
    if (length(inferred_layers) > 0L) {
      tests$designation_inference <- list(
        passed = TRUE, layers = inferred_layers,
        details = list(source = "engine_aqp_bt_with_films",
                          note   = paste0("v0.9.90: clay-increase test failed but ",
                                           "Bt designation + clay films + subsoil ",
                                           "depth accepted as argic morphology"))
      )
    }
  }
  if (length(inferred_layers) > 0L) {
    final_layers <- union(agg$layers, inferred_layers)
    final_passed <- TRUE
  } else {
    final_layers <- agg$layers
    final_passed <- agg$passed
  }

  # v0.9.187 -- designation-driven morphological fallback (opt-in). A "Bt"
  # horizon records field-observed clay illuviation; accept it as argic when the
  # quantitative clay-increase data is absent (common in legacy 2-sample
  # profiles), at a downgraded morphological grade. Unlike the films-based path
  # above it does not require a recorded clay_films qualifier.
  mi <- .apply_morph_inference("argic", h, final_passed, final_layers, tests)
  final_passed <- mi$passed; final_layers <- mi$layers; tests <- mi$evidence
  # v0.9.310: nor do the designation readings above make a surface layer or an
  # A horizon argic (a subsurface horizon, p. 35).
  if (wrb2022_text && any(not_sub[final_layers])) {
    final_layers <- setdiff(final_layers, which(not_sub))
    if (isTRUE(final_passed) && !length(final_layers)) final_passed <- FALSE
  }

  DiagnosticResult$new(
    name      = "argic",
    passed    = final_passed,
    layers    = final_layers,
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
#' @param max_cec CEC (1M NH4OAc, pH 7) per kg clay that a ferralic
#'        horizon must stay below (default \code{NULL} = 16 in soilkey
#'        engine, 20 in aqp engine; see \code{engine}).
#' @param cec_inclusive \code{FALSE} (default): the CEC must be below
#'        \code{max_cec}, as WRB 2022 writes it ("< 16 cmolc kg-1 clay").
#'        \code{TRUE} accepts a value equal to the limit, as USDA's oxic
#'        horizon does ("16 cmol(+) or less"); \code{\link{oxic_usda}} uses
#'        it.
#' @param engine One of \code{"soilkey"} (default; strict 16
#'        cmol_c/kg-clay threshold per WRB 2022) or \code{"aqp"}
#'        (relaxed 20 cmol_c/kg-clay -- a regional tolerance that
#'        accommodates Brazilian / SOTERLAC Latossolos data, where
#'        Embrapa-style Mehlich/Ca+Mg+K+Al sum often reads ~17-20
#'        on profiles that the canonical NRCS / WRB definition would
#'        accept as ferralic). \code{NULL} reads
#'        \code{getOption("soilKey.diagnostic_engine")}. The numeric
#'        threshold can also be overridden directly via
#'        \code{options(soilKey.ferralic_max_cec = ...)}.
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called:
#' \itemize{
#'   \item \code{test_ferralic_texture} -- texture sandy loam or
#'         finer.
#'   \item \code{test_cec_per_clay} -- CEC / clay < 16 (or < 20
#'         under \code{engine = "aqp"}) cmol_c/kg clay. Until v0.9.212 a
#'         value equal to the limit passed; WRB 2022 Ch 3.1.10 criterion 3
#'         is "< 16 cmolc kg-1 clay".
#'   \item \code{test_ferralic_thickness} -- thickness >= 30 cm.
#' }
#'
#' v0.3.1 alignment with WRB 2022 Ch 3.1.10 (p. 44): the older
#' "ECEC <= 12 cmol_c/kg clay" gate was removed because it is not in the
#' canonical text -- only CEC (1M NH4OAc, pH 7) < 16 is required.
#'
#' v0.9.67 regional tolerance: BDsolos RJ benchmark (n=722 perfis)
#' showed 88/115 Latossolos failing the strict 16-cmol gate because
#' Embrapa lab methodology often reads CEC at 17-20 on profiles that
#' are unambiguously Latossolos by every other criterion. The
#' \code{engine = "aqp"} threshold of 20 closes that gap without
#' redefining the WRB threshold itself; users targeting strict
#' WRB 2022 fidelity should keep \code{engine = "soilkey"}.
#'
#' The weatherable-mineral test (<= 10\% by volume),
#' water-dispersible-clay test, and stratification / rock-structure
#' exclusions remain deferred (they need mineralogical data outside
#' the canonical horizon schema) and are refinements rather than gates.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3.1.10 -- Ferralic horizon (p. 44).
#'
#' @export
ferralic <- function(pedon,
                       min_thickness = 30,
                       max_cec       = NULL,
                       engine        = NULL,
                       cec_inclusive = FALSE) {
  .ferralic_impl(pedon, min_thickness = min_thickness, max_cec = max_cec,
                 engine = engine, cec_inclusive = cec_inclusive,
                 wrb2022_text = TRUE)
}

#' The ferralic test; \code{wrb2022_text = FALSE} keeps the thickness asked of
#' each layer alone, the reading before v0.9.310, for USDA's oxic horizon
#' (oxic_usda) and the cambic horizon of the USDA keys.
#' @noRd
.ferralic_impl <- function(pedon,
                           min_thickness = 30,
                           max_cec       = NULL,
                           engine        = NULL,
                           cec_inclusive = FALSE,
                           wrb2022_text  = FALSE) {
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))
  if (is.null(max_cec))
    max_cec <- getOption("soilKey.ferralic_max_cec",
                            default = if (engine == "aqp") 20 else 16)

  h <- pedon$horizons
  # v0.9.188 -- region governs the ambiguous Bw/Bo designation: it is the
  # latossolic (ferralic) B in the tropics but the cambic B in temperate usage.
  tropical <- .pedon_is_tropical(pedon)

  tests <- list()
  tests$texture       <- test_ferralic_texture(h, tropical = tropical)
  tests$cec_per_clay  <- test_cec_per_clay(h,
                                             max_cmol_per_kg_clay = max_cec,
                                             inclusive = isTRUE(cec_inclusive))
  # v0.9.310: criterion 6, "a thickness of >= 30 cm" (WRB 2022 Ch 3.1.10,
  # p. 44), is that of the horizon: the adjoining layers that meet the texture
  # and CEC criteria are taken together (Ch 2.1, rule 7, p. 23: the criteria
  # "except thickness" are met in every subhorizon). It was asked of each layer
  # alone. The ferralic is "a subsurface horizon" (p. 44), so the layer at the
  # mineral soil surface and the A horizons are not taken into it (as for the
  # argic and nitic horizons); a thin A of low-activity clay no longer starts
  # the horizon at the surface. USDA's oxic horizon (oxic_usda) keeps the
  # per-layer test: read as a horizon there, it moved profiles of the FEBR and
  # ISIS USDA panels from the Ultisols and Alfisols to the Oxisols, as many
  # wrongly as rightly, and is left to a review of the USDA keys.
  tests$thickness <- if (wrb2022_text)
    test_minimum_thickness(
      h, min_cm = min_thickness,
      candidate_layers = setdiff(intersect(tests$texture$layers,
                                           tests$cec_per_clay$layers),
                                 which(.wrb_not_subsurface(h))),
      adjoining = TRUE)
  else test_ferralic_thickness(h, min_cm = min_thickness)

  agg <- aggregate_subtests(tests)

  # v0.9.187 -- designation-driven morphological fallback (opt-in). A "Bw"/"Bo"
  # horizon is the field pedologist's call of an in-situ, strongly-weathered
  # (latossolic/ferralic) B; accept it when the quantitative CEC-per-clay /
  # texture data is missing or borderline, at a downgraded morphological grade.
  # v0.9.188: region-gated -- Bw/Bo indicates ferralic only in a tropical
  # regime (see .pedon_is_tropical); a temperate Bw is cambic, handled by
  # cambic().
  ev  <- c(tests, list(engine = engine, max_cec_used = max_cec))
  mi  <- .apply_morph_inference("ferralic", h, agg$passed, agg$layers, ev,
                                min_top = 25, tropical = tropical)

  notes_str <- paste0(
    "v0.3.1: ECEC/clay <= 12 test removed; not part of WRB 2022 ferralic. ",
    "v0.9.67 engine=", engine, " threshold = ", max_cec, " cmol_c/kg clay."
  )

  DiagnosticResult$new(
    name      = "ferralic",
    passed    = mi$passed,
    layers    = mi$layers,
    evidence  = mi$evidence,
    missing   = agg$missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 3.1.10,",
                       "Ferralic horizon (p. 44)",
                       if (engine == "aqp") "[engine=aqp regional tolerance]" else ""),
    notes     = notes_str
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
#' Since v0.9.314 the criteria are read as WRB 2022 Ch 2.1 (p. 23) sets them for
#' a horizon of several subhorizons: every criterion except the thickness in
#' every layer, unless the text says otherwise. Criteria 1 (structure;
#' structure not recorded does not block it), 2 (>= 0.6\% SOC) and 3 (colour,
#' items a-c; a plough layer takes the colour of the subhorizon below it) are
#' read in each layer, and the horizon is the run of such layers from the
#' mineral soil surface. Criterion 5, the base saturation, is the weighted
#' average over the run (by pH where it is not recorded, Annex 2 Table 9.2),
#' and criterion 6 its thickness: >= 20 cm (6b), or since v0.9.321 >= 10 cm
#' directly over continuous rock, technic hard material or a cryic,
#' petrocalcic, petroduric, petrogypsic or petroplinthic horizon (6a, p. 53;
#' the umbric horizon names cryic, petroduric and petroplinthic, p. 70). A
#' moist value with no dry value recorded reads the dry value as the moist
#' value + 1; a layer with no colour
#' recorded at all and >= 1.5\% SOC is read as dark (Hugo, 2026-10-29). A dry
#' colour alone leaves criterion 3 undecided unless its value is above 5. The
#' colour subtest records where each layer's reading came from.
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
  ref <- "IUSS Working Group WRB (2022), Chapter 3, Mollic horizon"
  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "mollic", passed = NA, layers = integer(0),
      evidence = list(), missing = character(0), reference = ref))
  }
  # v0.9.314: criteria 1-3 in every layer (WRB 2022 Ch 2.1, p. 23); the
  # horizon is the run of such layers from the mineral soil surface
  ls  <- .wrb_mollic_layer_status(h, umbric = FALSE, min_oc = min_oc)
  run <- .wrb_mollic_run(h, ls$status, ls$missing, surface_top_cm = surface_top_cm)
  L <- run$layers
  thk <- function(x) sum(pmax(h$bottom_cm[x] - h$top_cm[x], 0), na.rm = TRUE)

  tests <- .wrb_mollic_layer_subtests(h, ls, run)
  # 5. "a base saturation (by 1 M NH4OAc, pH 7) of >= 50% on a weighted
  # average" (p. 53); where BS is not recorded, pH by Annex 2 Table 9.2
  bs <- if (length(L)) .wrb_bs_weighted_above(h, L, min_bs = min_bs)
        else list(passed = if (length(run$maybe)) NA else FALSE, missing = character(0))
  tests$base_saturation <- .subtest_result(
    passed  = bs$passed,
    layers  = if (isTRUE(bs$passed)) L else integer(0),
    missing = bs$missing %||% character(0),
    details = list(weighted_average = bs$average, bounds = bs$bounds,
                   layers = bs$details))
  # 6. a. >= 10 cm "if directly overlying continuous rock, technic hard
  # material or a cryic, petrocalcic, petroduric, petrogypsic or
  # petroplinthic horizon"; b. >= 20 cm (p. 53); the layers of the run taken
  # together, undecided where layers not yet decided could make it up.
  # v0.9.321: item a is read (.wrb_mollic_criterion6()); before, only b.
  six <- .wrb_mollic_criterion6(pedon, run, min_thickness = min_thickness)
  tests$thickness <- .subtest_result(
    passed  = six$passed,
    layers  = six$layers,
    missing = if (is.na(six$passed))
                unique(c(run$missing, "designation", "cementation_class"))
              else character(0),
    details = list(cumulative_cm = thk(L), threshold = min_thickness, layers = L,
                   criterion_6a = six$by_6a, layer_below = six$layer_below,
                   undecided_below = six$undecided_below))

  # the order of the sub-tests before v0.9.314
  tests <- tests[c("color", "organic_carbon", "base_saturation", "thickness", "structure")]
  agg <- aggregate_subtests(tests)
  # a criterion that fails decides, whatever is undecided (three-valued AND)
  if (any(vapply(tests, function(t) isFALSE(t$passed), NA))) agg$passed <- FALSE
  DiagnosticResult$new(
    name      = "mollic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = ref,
    notes     = if (!length(run$maybe)) "No run of mollic layers from the mineral soil surface"
                else NA_character_
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
#'   \item \code{test_caco3_concentration} -- CaCO3 >= 15\%.
#'   \item \code{test_minimum_thickness} -- thickness >= 15 cm.
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
#' @section v0.9.314:
#' The WRB calcic horizon follows WRB 2022 Ch 3.1.4 (p. 37-38) criterion by
#' criterion: criterion 2 is protocalcic properties (secondary carbonates on
#' \eqn{\ge} 5\% of the exposed area, \code{secondary_carbonates_pct}) or
#' \eqn{\ge} 5\% more CaCO3 than an underlying layer with no lithic
#' discontinuity between; a layer of a petrocalcic horizon is not calcic; the
#' 15 cm are those of the adjoining layers. A layer whose criterion 2 cannot be
#' decided (secondary carbonates not recorded) is undecided, not calcic. The
#' USDA calcic horizon and the SiBCS horizonte calcico keep the earlier
#' reading.
#'
#' @export
calcic <- function(pedon, min_thickness = 15, min_caco3_pct = 15) {
  # v0.9.314: WRB 2022 Ch 3.1.4 (p. 37-38), see .calcic_wrb2022(). The USDA
  # calcic horizon and the SiBCS horizonte calcico call .calcic_impl().
  .calcic_wrb2022(pedon, min_thickness = min_thickness,
                  min_caco3_pct = min_caco3_pct)
}

#' The calcic test before v0.9.314, read by the USDA calcic horizon and the
#' SiBCS horizonte calcico: a layer with >= 15\% CaCO3 is dropped only when
#' its enrichment and its secondary carbonates are both disproven.
#' @noRd
.calcic_impl <- function(pedon, min_thickness = 15, min_caco3_pct = 15) {
  h <- pedon$horizons

  tests <- list()
  tests$caco3     <- test_caco3_concentration(h, min_pct = min_caco3_pct)
  # v0.9.142: WRB 2022 (3.1.4 crit 2) / USDA KST require, beyond the absolute
  # >= 15%, ONE of: (2a) protocalcic properties / >= 5% by-volume identifiable
  # secondary carbonates, OR (2b) a +5% (absolute) CaCO3 enrichment over an
  # underlying layer. v0.9.139 measured (KSSL n=34,755) that enforcing the
  # caco3-only (2b) path in this shared core drops 10 genuine protocalcic
  # Aridisols -- so the morphological (2a) path is essential. Now that the
  # secondary_carbonates_pct field exists, the enrichment is enforced
  # REFINE-WHEN-PRESENT at the criterion level: a >= 15% layer is dropped ONLY
  # when its CaCO3 fails the +5% test AND secondary carbonates are RECORDED and
  # < 5% (both 2a and 2b disproven). When secondary_carbonates_pct is absent
  # (all current KSSL/FEBR/fixtures) criterion 2a is indeterminate, so nothing is
  # dropped -> byte-identical. (SiBCS horizonte_calcico keeps its stricter +50.)
  if (length(tests$caco3$layers) > 0L) {
    enr <- test_caco3_enrichment(h, candidate_layers = tests$caco3$layers)
    sc  <- h[["secondary_carbonates_pct"]]
    keep <- vapply(tests$caco3$layers, function(i) {
      if (i %in% enr$layers) return(TRUE)                 # 2b: CaCO3 enrichment
      scv <- if (!is.null(sc)) sc[i] else NA_real_
      if (is.na(scv)) return(TRUE)                        # 2a indeterminate -> keep
      scv >= 5                                            # 2a: by-volume secondary carbonates
    }, logical(1))
    enriched_layers <- tests$caco3$layers[keep]
    tests$enrichment <- list(passed = length(enriched_layers) > 0L,
                             layers = enriched_layers)
  } else {
    enriched_layers <- tests$caco3$layers
  }
  tests$thickness <- test_minimum_thickness(h,
                                              min_cm           = min_thickness,
                                              candidate_layers = enriched_layers)

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
#'   \item \code{test_caso4_concentration} -- gypsum >= 5\%.
#'   \item \code{test_minimum_thickness} -- thickness >= 15 cm.
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
#'   \item thickness >= 15 cm (\code{test_minimum_thickness})
#'   \item texture sandy loam or finer (\code{test_texture_argic})
#'   \item NOT \code{\link{argic}} AND NOT \code{\link{ferralic}}
#' }
#'
#' v0.2 limitations: WRB 2022 also excludes profiles with spodic,
#' calcic, gypsic, plinthic, vertic, and several other diagnostic
#' horizons. Those exclusions, plus the WRB criteria of "evidence of
#' alteration" (color/structure differences from parent material,
#' carbonate removal), are scheduled for v0.3.
#'
#' @section v0.9.314:
#' The WRB cambic horizon follows WRB 2022 Ch 3.1.5 (p. 39-40): criterion 4
#' is read layer by layer, so a layer that forms part of an argic, ferralic,
#' calcic, mollic or other listed horizon (or of a layer with andic
#' properties) is set aside, not the whole profile; the subsurface layers are
#' those below the layer at the mineral soil surface that are not A horizons;
#' the 15 cm are those of the adjoining layers; a layer whose texture or
#' structure is not recorded leaves the result undecided. Criterion 3 is not
#' tested. The USDA keys keep the earlier reading.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3, Cambic horizon.
#' @param min_top_cm Minimum top depth (cm) for a horizon to be
#'        considered cambic-eligible (default 5). Anchors the
#'        candidate set to subsurface layers. Since v0.9.314 the WRB
#'        reading asks it only when it is given.
#' @param engine v0.9.63+. One of \code{"soilkey"} (hand-coded path,
#'        default for back-compat) or \code{"aqp"} (canonical NRCS
#'        dispatch via \code{aqp::getCambicBounds}). When \code{NULL}
#'        (the new default) the function reads
#'        \code{getOption("soilKey.diagnostic_engine", "soilkey")},
#'        so a global \code{options(soilKey.diagnostic_engine = "aqp")}
#'        flips every \code{cambic()} call without modifying call
#'        sites. The aqp engine fired 40.6% of BDsolos RJ perfis vs
#'        soilkey 0% in the v0.9.62 A/B benchmark -- and lifted the
#'        v0.9.50 LUCAS WRB benchmark from 0% to 60% (Cambisols recall
#'        100%) when combined with subsoil-fill SoilGrids. See
#'        \code{\link{cambic_aqp}}.
#' @export
cambic <- function(pedon, min_thickness = 15, min_top_cm = 5,
                     engine = NULL) {
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))
  if (engine == "aqp")
    return(.cambic_impl(pedon, engine = "aqp"))
  # v0.9.314: WRB 2022 Ch 3.1.5 (p. 39-40), see .cambic_wrb2022(): the
  # subsurface layers, criterion 4 layer by layer, the 15 cm of the horizon.
  # min_top_cm is asked only when given. The USDA keys call .cambic_impl().
  .cambic_wrb2022(pedon, min_thickness = min_thickness,
                  min_top_cm = if (missing(min_top_cm)) NULL else min_top_cm)
}

#' The cambic test. It sets aside a profile with an argic or ferralic horizon;
#' \code{wrb2022_text = FALSE} reads those two as before v0.9.310, for the USDA
#' keys that use this cambic horizon (Inceptisols, Aridisols, Andisols). Since
#' v0.9.314 the WRB cambic() reads .cambic_wrb2022() and calls this only for
#' \code{engine = "aqp"}.
#' @noRd
.cambic_impl <- function(pedon, min_thickness = 15, min_top_cm = 5,
                         engine = NULL, wrb2022_text = FALSE) {
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))
  # v0.9.63: optional dispatch to aqp::getCambicBounds() via
  # `cambic_aqp()`. The v0.9.62 RJ A/B showed soilkey's cambic
  # passes 0% of BDsolos perfis vs aqp's 40.6% -- a critical gap
  # that explains the v0.9.50 LUCAS WRB 0% baseline (Cambisols
  # never fire). For users who want the canonical KST 13ed cambic
  # behavior, set `engine = "aqp"`. Default kept "soilkey" for
  # back-compat with existing classify_*() flows.
  if (engine == "aqp") {
    res <- cambic_aqp(pedon)
    res$reference <- paste(res$reference, "[engine=aqp]")
    return(res)
  }
  h <- pedon$horizons

  arg_res <- if (wrb2022_text) argic(pedon) else .argic_impl(pedon)
  fer_res <- if (wrb2022_text) ferralic(pedon) else .ferralic_impl(pedon)

  tests <- list()
  # v0.9.2.C: cambic is by definition a SUBSURFACE horizon. The bare
  # v0.x test fired on any horizon that met thickness + texture, which
  # included surface A horizons, leading to false-positive Brunic-of-
  # Mollic-A across many fixtures. Anchor the candidate set to layers
  # that start at or below `min_top_cm` (default 5 cm).
  subsurface <- which(!is.na(h$top_cm) & h$top_cm >= min_top_cm)
  tests$subsurface <- .subtest_result(
    passed  = if (length(subsurface) > 0L) TRUE
              else if (any(is.na(h$top_cm))) NA else FALSE,
    layers  = subsurface,
    missing = if (any(is.na(h$top_cm))) "top_cm" else character(0),
    details = list(min_top_cm = min_top_cm)
  )
  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = subsurface)
  tests$texture   <- test_texture_argic(h,
                                          candidate_layers = tests$thickness$layers)
  # v0.9.2.C: cambic requires evidence of soil formation, NOT just
  # texture. A C horizon (massive structure, single-grain or
  # structureless) is parent material, not an altered B; without
  # structural development the layer is ineligible.
  cand_str <- tests$texture$layers
  if (length(cand_str) > 0L) {
    grade <- h$structure_grade[cand_str]
    type  <- h$structure_type[cand_str]
    has_dev <- !is.na(grade) & grade %in% c("weak", "moderate", "strong") &
                 (is.na(type) | !grepl("^massive$|^single grain$",
                                          type, ignore.case = TRUE))

    # v0.9.19: when structure_grade is missing across all candidate
    # layers, fall back to designation-based evidence. KST 13ed Ch 3
    # (cambic horizon, p 13) accepts a B-horizon designation pattern
    # (Bw weathered, Bg gleyed, Bk calcic, Bv vertic-like, Bj
    # jarositic, Bz salt-accumulating, Bx fragic-like) as morphological
    # evidence of soil formation in lieu of structure data, since the
    # surveyor's "B*" suffix already records the alteration. Only
    # fires when structure data is genuinely absent (lab-grade
    # profiles still gate on measured structure).
    if (all(is.na(grade))) {
      desg <- h$designation[cand_str]
      bdev_pattern <- !is.na(desg) &
                       grepl("^B[wgkjvzx]|^B[wgkjvzx][0-9]", desg)
      has_dev_morph <- bdev_pattern
      tests$structure_development <- .subtest_result(
        passed  = if (any(has_dev_morph)) TRUE
                  else if (all(is.na(desg))) NA else FALSE,
        layers  = cand_str[has_dev_morph],
        missing = "structure_grade",
        details = list(source = "designation_morphology_inference",
                        designations = desg, note = paste0(
                          "v0.9.19: structure_grade missing; ",
                          "Bw/Bg/Bk/Bj/Bv/Bz/Bx designations accepted ",
                          "as KST 13ed Ch 3 morphological evidence."))
      )
    } else {
      tests$structure_development <- .subtest_result(
        passed  = if (any(has_dev)) TRUE
                  else if (all(is.na(grade))) NA else FALSE,
        layers  = cand_str[has_dev],
        missing = if (all(is.na(grade))) "structure_grade" else character(0),
        details = list(grade = grade, type = type)
      )
    }
  } else {
    tests$structure_development <- .subtest_result(
      passed = FALSE, layers = integer(0),
      missing = character(0), details = list()
    )
  }
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
#' Tests whether any horizon meets the plinthic horizon criteria (WRB 2022
#' Ch 3.1.30, p. 62), the diagnostic of Plinthosols with the pisoplinthic
#' and petroplinthic horizons. The depth limit belongs to the Plinthosols
#' key (\code{plinthosol()}), not to the horizon.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 15).
#' @param min_plinthite_pct Minimum volume \% plinthite (default 15).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Criteria read (v0.9.312):
#' \itemize{
#'   \item 1: \code{plinthite_pct} \\>= 15 (the oximorphic features inside
#'         former aggregates of criterion 1, as \code{plinthite_pct} records
#'         them); a layer where it is not recorded is undecided;
#'   \item 2: Fe-dithionite \\>= 2.5\% (2.a) when \code{fe_dcb_pct} is
#'         measured; below it the layer is undecided, since 2.b (10\% Fe in
#'         the oximorphic features) and 2.c (hardening after drying and
#'         wetting) are not recorded; not measured, the recorded plinthite
#'         is taken for 2.c;
#'   \item 3: Feox/Fedith < 0.1 when both are measured;
#'   \item 4: not part of a petroplinthic or pisoplinthic horizon;
#'   \item 5: \\>= 15 cm thick; adjoining sublayers (Btv1 over Btv2) count
#'         together.
#' }
#' The result is \code{NA} when undecided layers could make the horizon.
#'
#' @section v0.9.72 designation morphological inference (opt-in):
#' Field-described Brazilian Plintossolos profiles (e.g.\ the Embrapa
#' Redape curated dataset) routinely encode plinthite via the
#' designation suffix \code{f} in the master letter sequence (e.g.\
#' \code{Btf}, \code{2Btf}, \code{Cf}) -- the curator's direct
#' assertion that plinthite is present -- without recording
#' \code{plinthite_pct} as a numeric volume percent.
#'
#' With \code{options(soilKey.plinthic_designation_inference = TRUE)} the
#' function accepts a layer as plinthic when:
#' \enumerate{
#'   \item the canonical \code{plinthite_pct} test is \code{NA} for
#'         that layer, AND
#'   \item the designation matches \code{[A-Z]+[A-Za-z]*f[0-9]?}
#'         (a \code{f} master-letter modifier in any sub-position).
#' }
#'
#' Default is \code{FALSE} (canonical behaviour preserved).
#'
#' @references IUSS Working Group WRB (2022), Chapter 3.1.30, Plinthic
#'   horizon.
#' @export
plinthic <- function(pedon, min_thickness = 15, min_plinthite_pct = 15) {
  h <- pedon$horizons
  n <- nrow(h)

  tests <- list()
  tests$plinthite <- test_plinthite_concentration(h,
                                                    min_pct = min_plinthite_pct)
  ok <- .subtest_by_layer(tests$plinthite, n)
  # criteria 2 and 3 (p. 62), where the Fe is measured
  fe_d  <- suppressWarnings(as.numeric(h$fe_dcb_pct %||% rep(NA_real_, n)))
  fe_ox <- suppressWarnings(as.numeric(h$fe_ox_pct %||% rep(NA_real_, n)))
  low_fe <- !is.na(fe_d) & fe_d < 2.5
  ratio_high <- !is.na(fe_d) & !is.na(fe_ox) & fe_d > 0 & fe_ox / fe_d >= 0.1
  ok[ok %in% TRUE & low_fe] <- NA
  ok[ratio_high] <- FALSE
  # criterion 4: not part of a petroplinthic or pisoplinthic horizon
  part <- union(.petroplinthic_run(h)$layers_true, .pisoplinthic_run(h)$layers_true)
  ok[part] <- FALSE
  run <- .adjoining_runs(h, ok, min_cm = min_thickness)
  tests$thickness <- list(passed = run$passed, layers = run$layers,
                          min_cm = min_thickness, adjoining = TRUE)
  tests$fe_criteria <- list(low_fe_dith = which(low_fe), feox_fedith_high = which(ratio_high),
                            part_of_petro_or_pisoplinthic = part)
  agg <- list(passed = run$passed,
              layers = if (isTRUE(run$passed)) run$layers else integer(0),
              missing = if (isTRUE(run$passed)) character(0)
                        else unique(c(tests$plinthite$missing,
                                      if (any(low_fe)) "fe_dcb_pct")))
  tests$undecided_layers <- if (is.na(run$passed)) run$layers else integer(0)

  # v0.9.72 -- designation-suffix morphological inference
  designation_inference_enabled <- isTRUE(
    getOption("soilKey.plinthic_designation_inference", default = FALSE)) ||
    .morph_inference_enabled()   # v0.9.187: also under the global morph mode
  inferred_layers <- integer(0)
  inference_path  <- list(passed = NA, layers = integer(0), source = "off")
  if (designation_inference_enabled && !isTRUE(agg$passed)) {
    desig <- if (!is.null(h$designation)) as.character(h$designation)
              else rep(NA_character_, nrow(h))
    # 'f' master-letter modifier: matches uppercase + (zero or more
    # lowercase modifier letters or sequence digits) + 'f' anywhere
    # in that chunk. Catches Btf, 2Btf, Cf, Btf1, Apf, Cfn, Btff,
    # B1f. Standalone-modifier letters like t, w, s come BEFORE f
    # in the canonical Brazilian / FAO sequence.
    has_f_suffix <- !is.na(desig) & grepl("[A-Z][a-z0-9]*f", desig)
    inferred_layers <- which(has_f_suffix)
    if (length(inferred_layers) > 0L) {
      # Apply thickness check on inferred layers
      thk <- pmax(h$bottom_cm[inferred_layers] - h$top_cm[inferred_layers], 0,
                    na.rm = TRUE)
      if (sum(thk, na.rm = TRUE) >= min_thickness) {
        inference_path <- list(
          passed = TRUE, layers = inferred_layers,
          source = "designation_f_suffix",
          details = list(matched_designations = desig[has_f_suffix],
                            total_thickness_cm   = sum(thk, na.rm = TRUE),
                            min_thickness        = min_thickness)
        )
      } else {
        inference_path <- list(
          passed = FALSE, layers = inferred_layers,
          source = "designation_f_suffix_thin",
          details = list(matched_designations = desig[has_f_suffix],
                            total_thickness_cm   = sum(thk, na.rm = TRUE),
                            min_thickness        = min_thickness)
        )
        inferred_layers <- integer(0)
      }
    }
  }
  tests$designation_inference <- inference_path

  final_passed <- if (isTRUE(agg$passed)) TRUE
                    else if (length(inferred_layers) > 0L) TRUE
                    else agg$passed  # NA or FALSE
  final_layers <- if (isTRUE(agg$passed)) agg$layers
                    else if (length(inferred_layers) > 0L) inferred_layers
                    else integer(0)

  DiagnosticResult$new(
    name      = "plinthic",
    passed    = final_passed,
    layers    = final_layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 3.1.30, Plinthic horizon (p. 62)",
                       if (length(inferred_layers) > 0L)
                         "[v0.9.72 designation-suffix inference]" else "")
  )
}


#' Spodic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the spodic horizon criteria. The
#' spodic horizon is an illuvial horizon with active Al + Fe oxalate-
#' extractable material plus organic matter; diagnostic of Podzols.
#'
#' @section v0.9.84 engine="aqp" relaxation:
#' KSSL+NASIS Spodosols routinely use generic "B1" / "B2" / "Bw"
#' designations rather than the specific Bh / Bs / Bhs that the
#' v0.9.19 morphological-inference path requires. Of 14 KSSL+NASIS
#' Podzol references, only 1 / 14 passes spodic via the v0.9.19
#' path; 7 / 14 have BOTH an E-designated albic-eligible horizon
#' above AND an OC peak in a B horizon below (the canonical Podzol
#' illuviation signature) but use generic B / Bw designations and so
#' fail strict morph.
#'
#' When \code{engine = "aqp"} (read from
#' \code{getOption("soilKey.diagnostic_engine", "soilkey")} when
#' \code{engine} is \code{NULL}) AND Al / Fe oxalate is unmeasured
#' AND the v0.9.19 strict path did not fire, accept any
#' \code{B*} designation below an \code{E*}-designated horizon when:
#' \itemize{
#'   \item \code{ph_h2o <= max_ph} in the B horizon, AND
#'   \item \code{oc_pct >= min_oc_in_b} in the B horizon, AND
#'   \item OC in the B is greater than the maximum OC in any
#'         horizon above (the translocation signature).
#' }
#' Default \code{engine} is \code{"soilkey"} -- canonical behaviour
#' bit-for-bit preserved.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 2.5).
#' @param min_alfe Minimum (Al_ox + 0.5 * Fe_ox) percent (default 0.5).
#' @param max_ph Maximum ph_h2o (default 5.9).
#' @param min_oc_in_b Minimum OC \% in the candidate Bh / Bs layer
#'        for the v0.9.19 morphological inference path when Al / Fe
#'        oxalate are missing (default 0.5).
#' @param engine One of \code{"soilkey"} (default; strict v0.9.19
#'        morphological path requires Bh / Bs / Bhs designation +
#'        albic E above) or \code{"aqp"} (relaxed v0.9.84 path: any
#'        B* below E* with OC translocation peak). When
#'        \code{NULL}, reads \code{getOption("soilKey.diagnostic_engine")}.
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests:
#' \itemize{
#'   \item \code{test_spodic_aluminum_iron} -- (Al_ox +
#'         0.5*Fe_ox) >= 0.5\%
#'   \item \code{test_ph_below} -- ph_h2o <= 5.9
#'   \item \code{test_minimum_thickness} -- thickness >= 2.5 cm
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
                     max_ph        = 5.9,
                     min_oc_in_b   = 0.5,
                     engine        = NULL) {
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  h <- pedon$horizons

  tests <- list()
  tests$alfe_oxalate <- test_spodic_aluminum_iron(h, min_pct = min_alfe)
  tests$ph           <- test_ph_below(h, max_ph = max_ph,
                                        candidate_layers = tests$alfe_oxalate$layers)
  # v0.3.4: require either a B(h|s|hs) designation OR an albic-or-claric
  # horizon directly overlying. This disambiguates spodic from andic
  # (which has the same Al/Fe + low pH chemistry but no eluvial-illuvial
  # designation pattern). Without this gate the spodic test triggers on
  # any acidic Al/Fe-rich Bw and confuses Andosol vs Podzol.
  desg <- test_pattern_match(h, "designation", "^B[hsr](h)?[s]?$|^Bhs|^Bsh|^Bs$|^Bh$")
  has_albic_above <- integer(0)
  if (length(tests$alfe_oxalate$layers) > 0L) {
    al <- albic(pedon)
    if (isTRUE(al$passed)) {
      for (i in tests$alfe_oxalate$layers) {
        above_layers <- which(seq_len(nrow(h)) < i)
        if (any(above_layers %in% al$layers)) {
          has_albic_above <- c(has_albic_above, i)
        }
      }
    }
  }
  tests$illuvial_signature <- .subtest_result(
    passed = if (length(union(desg$layers, has_albic_above)) > 0L) TRUE
             else if (all(is.na(h$designation))) NA
             else FALSE,
    layers = union(desg$layers, has_albic_above),
    missing = desg$missing,
    details = list(designation_match = desg$layers,
                     albic_above_match = has_albic_above)
  )

  # v0.9.19: morphological inference path. KST 13ed Ch 3 (spodic
  # horizon, p 23) accepts (Al + 0.5*Fe)_ox >= 0.5 as ONE path; it
  # also accepts spodic morphology with characteristic illuvial
  # accumulation (Bs / Bh designation), eluvial depletion (albic
  # E above), low pH, and elevated OC in B. When al_ox / fe_ox
  # are missing across all candidate layers we fall back to that
  # morphological path: Bh/Bs designation + albic E above the Bh/Bs
  # + pH <= max_ph + OC in the Bh/Bs >= min_oc_in_b. The fallback
  # only fires when the direct measurement is missing.
  if (is.na(tests$alfe_oxalate$passed) ||
        (isFALSE(tests$alfe_oxalate$passed) &&
           length(tests$alfe_oxalate$layers) == 0L)) {
    al_ox_measured <- any(!is.na(h$al_ox_pct))
    fe_ox_measured <- any(!is.na(h$fe_ox_pct))
    if (!al_ox_measured && !fe_ox_measured) {
      al_check <- albic(pedon)
      if (length(desg$layers) > 0L && isTRUE(al_check$passed)) {
        morph_layers <- integer(0)
        for (i in desg$layers) {
          above <- which(seq_len(nrow(h)) < i)
          if (!any(above %in% al_check$layers)) next
          ph_ok <- !is.na(h$ph_h2o[i]) && h$ph_h2o[i] <= max_ph
          oc_ok <- !is.na(h$oc_pct[i]) && h$oc_pct[i] >= min_oc_in_b
          if (ph_ok && oc_ok) morph_layers <- c(morph_layers, i)
        }
        if (length(morph_layers) > 0L) {
          tests$alfe_oxalate <- list(
            passed  = TRUE,
            layers  = morph_layers,
            missing = c("al_ox_pct", "fe_ox_pct"),
            details = list(source = "morphological_inference",
                            note   = paste0("v0.9.19: Al/Fe oxalate missing; ",
                                             "spodic accepted via Bh/Bs designation + ",
                                             "albic E above + pH <= ", max_ph,
                                             " + OC in B >= ", min_oc_in_b)),
            notes   = NA_character_
          )
          tests$ph <- test_ph_below(h, max_ph = max_ph,
                                       candidate_layers = morph_layers)
          tests$illuvial_signature <- .subtest_result(
            passed = TRUE, layers = morph_layers,
            details = list(source = "morphological_inference"))
        }
      }
    }
  }

  # v0.9.84: engine-aware relaxation. KSSL+NASIS Spodosols routinely
  # use generic "B1" / "B2" / "Bw" designations rather than the
  # specific Bh / Bs / Bhs that the v0.9.19 morphological path
  # requires. Of 14 KSSL+NASIS Podzol references, only 1 passes
  # spodic via the v0.9.19 path; 7 / 14 have BOTH an E-designated
  # albic-eligible horizon above AND an OC peak in a B horizon
  # below (the canonical Podzol illuviation signature) but use
  # generic B / Bw designations and so fail the strict morph path.
  #
  # When engine = "aqp" AND Al/Fe oxalate is unmeasured AND the
  # v0.9.19 strict morph path did not fire, accept any B* designation
  # below an E*-designated horizon when:
  #   * pH <= max_ph in the B horizon, AND
  #   * OC in the B >= min_oc_in_b, AND
  #   * OC in the B > max(OC) in any horizon above (the
  #     translocation signature).
  #
  # This is gated by engine = "aqp" so the canonical soilkey path
  # is preserved bit-for-bit when the option is unset.
  engine <- match.arg(engine, c("soilkey", "aqp"))
  v0984_relax_fired <- FALSE
  if (engine == "aqp" &&
        (is.na(tests$alfe_oxalate$passed) ||
           (isFALSE(tests$alfe_oxalate$passed) &&
              length(tests$alfe_oxalate$layers) == 0L)) &&
        !any(!is.na(h$al_ox_pct)) && !any(!is.na(h$fe_ox_pct))) {
    desg_chr <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
    is_E   <- !is.na(desg_chr) & grepl("^E", desg_chr)
    is_B   <- !is.na(desg_chr) & grepl("^B", desg_chr)
    relax_layers <- integer(0)
    for (j in which(is_B)) {
      above <- seq_len(j - 1L)
      if (!any(is_E[above])) next
      oc_in_b <- h$oc_pct[j]
      if (is.na(oc_in_b) || oc_in_b < min_oc_in_b) next
      oc_above <- h$oc_pct[above]
      if (all(is.na(oc_above))) next
      max_oc_above <- max(oc_above, na.rm = TRUE)
      if (!(oc_in_b > max_oc_above)) next
      # pH check: prefer measured pH <= max_ph; when pH is NA at the
      # candidate Bh, accept the path only if the OC translocation is
      # strong enough to be unambiguous (oc_in_b >= 1.5 * max_oc_above).
      # This catches the KSSL+NASIS Spodosols whose Bh chemistry is
      # documented but whose pH was never measured at the illuvial
      # horizon (5 of 7 OC-peak Podzols on the v0.9.84 audit).
      ph_b <- h$ph_h2o[j]
      ph_ok <- if (!is.na(ph_b)) {
        ph_b <= max_ph
      } else {
        oc_in_b >= 1.5 * max_oc_above
      }
      if (!isTRUE(ph_ok)) next
      relax_layers <- c(relax_layers, j)
    }
    if (length(relax_layers) > 0L) {
      v0984_relax_fired <- TRUE
      tests$alfe_oxalate <- list(
        passed  = TRUE,
        layers  = relax_layers,
        missing = c("al_ox_pct", "fe_ox_pct"),
        details = list(source = "engine_aqp_oc_translocation",
                          note   = paste0("v0.9.84: engine=aqp relaxed -- ",
                                           "any B* designation under E* with ",
                                           "OC peak (B > max(above)) + ",
                                           "(pH <= ", max_ph, " OR ",
                                           "OC ratio >= 1.5x) + ",
                                           "OC in B >= ", min_oc_in_b)),
        notes   = NA_character_
      )
      # When v0.9.84 fires we ALREADY checked the pH-or-OC-ratio gate
      # in the loop above, so synthesise a passing pH subtest result
      # rather than re-running test_ph_below(): the latter returns NA
      # when ph_h2o is unmeasured at the candidate layer, which would
      # block the aggregate even though the v0.9.84 OC-ratio path
      # already accepted the absence of pH.
      tests$ph <- .subtest_result(
        passed = TRUE, layers = relax_layers,
        details = list(source = "engine_aqp_oc_translocation_or_ph"))
      tests$illuvial_signature <- .subtest_result(
        passed = TRUE, layers = relax_layers,
        details = list(source = "engine_aqp_oc_translocation"))
    }
  }

  tests$thickness    <- test_minimum_thickness(h,
                                                  min_cm           = min_thickness,
                                                  candidate_layers = intersect(
                                                    tests$alfe_oxalate$layers,
                                                    tests$illuvial_signature$layers
                                                  ))

  agg <- aggregate_subtests(tests)

  # v0.9.187 -- designation fallback (opt-in): a Bh/Bs records field-observed
  # illuvial humus/sesquioxides; accept it when Al/Fe-oxalate AND the albic /
  # pH / OC corroboration the strict v0.9.19 path needs are all missing.
  mi <- .apply_morph_inference("spodic", h, agg$passed, agg$layers, tests)

  DiagnosticResult$new(
    name      = "spodic",
    passed    = mi$passed,
    layers    = mi$layers,
    evidence  = mi$evidence,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3, Spodic horizon",
    notes     = "v0.9.19: + morphological inference path when Al/Fe oxalate missing"
  )
}


#' Umbric horizon (WRB 2022)
#'
#' Tests for the umbric horizon -- a thick, dark, organic-rich surface
#' horizon like mollic, but with low base saturation (< 50\%).
#' Diagnostic of Umbrisols.
#'
#' Implementation reuses the mollic sub-tests on the contiguous stack of
#' dark layers from the mineral soil surface (organic layers on top are
#' not part of it), and tests the weighted average base saturation of the
#' stack against \code{max_bs} (WRB 2022 3.1.39, pp. 69-70).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm; default 20).
#' @param min_oc Minimum SOC \% (default 0.6).
#' @param max_bs Maximum base saturation \% (default 50; profile must
#'        be BELOW this).
#' @param surface_top_cm Maximum depth (cm) below the mineral soil
#'        surface at which the stack may start (default 5).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3.1.39, Umbric
#'   horizon (pp. 69-70).
#' @export
umbric_horizon <- function(pedon,
                              min_thickness  = 20,
                              min_oc         = 0.6,
                              max_bs         = 50,
                              surface_top_cm = 5) {
  h <- pedon$horizons
  ref <- "IUSS Working Group WRB (2022), Chapter 3.1.39, Umbric horizon (pp. 69-70)"

  # v0.9.309: WRB 2022 3.1.39 (p. 69), "a surface horizon consisting of
  # mineral material": the contiguous stack of dark layers from the mineral
  # soil surface, as mollic() has used since v0.9.78, instead of each layer
  # starting <= 5 cm on its own. Organic layers on top are skipped (Ch 2.1
  # rule 3, p. 23).
  # v0.9.314: criteria 1-3 in every layer (WRB 2022 Ch 2.1, p. 23); the
  # horizon is the run of such layers from the mineral soil surface
  ls    <- .wrb_mollic_layer_status(h, umbric = TRUE, min_oc = min_oc)
  stack <- .wrb_mollic_run(h, ls$status, ls$missing, surface_top_cm = surface_top_cm)
  L <- stack$layers
  thk <- function(x) sum(pmax(h$bottom_cm[x] - h$top_cm[x], 0), na.rm = TRUE)

  tests <- .wrb_mollic_layer_subtests(h, ls, stack)
  # 5. "a base saturation (by 1 M NH4OAc, pH 7) of < 50% on a weighted
  # average" (p. 70); where BS is not recorded, pH (Annex 2, Table 9.2).
  bs <- if (length(L)) .wrb_bs_weighted_below(h, L, max_bs = max_bs)
        else list(passed = NA, missing = character(0))
  tests$bs_low <- .subtest_result(
    passed  = if (length(L)) bs$passed else if (length(stack$maybe)) NA else FALSE,
    layers  = if (isTRUE(bs$passed)) L else integer(0),
    missing = bs$missing %||% character(0),
    details = list(weighted_average = bs$average, bounds = bs$bounds,
                   layers = bs$details))
  # 6. a. >= 10 cm "if directly overlying continuous rock, technic hard
  # material or a cryic, petroduric or petroplinthic horizon"; b. >= 20 cm
  # (p. 70); undecided where layers not yet decided could make it up.
  # v0.9.321: item a is read (.wrb_mollic_criterion6()); before, only b.
  six <- .wrb_mollic_criterion6(pedon, stack, min_thickness = min_thickness,
                                umbric = TRUE)
  tests$thickness <- .subtest_result(
    passed  = six$passed,
    layers  = six$layers,
    missing = if (is.na(six$passed))
                unique(c(stack$missing, "designation", "cementation_class"))
              else character(0),
    details = list(cumulative_cm = thk(L), threshold = min_thickness,
                   criterion_6a = six$by_6a, layer_below = six$layer_below,
                   undecided_below = six$undecided_below))

  agg <- aggregate_subtests(tests)
  # a criterion that fails decides, whatever is undecided (three-valued AND)
  if (any(vapply(tests, function(t) isFALSE(t$passed), NA))) agg$passed <- FALSE

  DiagnosticResult$new(
    name      = "umbric_horizon",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = ref,
    notes     = if (length(stack$maybe) == 0L) {
                   "No run of umbric layers from the mineral soil surface"
                 } else NA_character_
  )
}


#' Duric horizon (WRB 2022)
#'
#' Tests for >= 10\% volume of duripan nodules (Si-cemented) within a
#' horizon at least 10 cm thick. Diagnostic of Durisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm; default 10 per WRB 2022).
#' @param min_duripan_pct Minimum duripan volume \% (default 10 per WRB
#'        2022).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' v0.3.1: thresholds aligned with WRB 2022 Ch 3.1.7 (10\%, 10 cm) --
#' previous v0.3 used 15\%/15 cm. Petroduric (cemented continuous
#' duripan) detection still deferred and will be added in v0.4.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3.1.7 -- Duric
#' horizon (p. 41).
#' @export
duric_horizon <- function(pedon, min_thickness = 10, min_duripan_pct = 10) {
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
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.7, Duric horizon (p. 41)"
  )
}


#' Natric horizon (WRB 2022)
#'
#' Tests for the natric horizon: an argic horizon with diagnostic
#' sodium accumulation. Diagnostic of Solonetz.
#'
#' v0.9.310: WRB 2022 Ch 3.1.21 (p. 53-54) criterion 3 (columnar or
#' prismatic structure, or blocky with tongues of the overlying layer) and
#' criterion 4 (ESP \eqn{\ge} 15 throughout the upper 40 cm, or Mg + Na >
#' Ca + exchange acidity there with ESP \eqn{\ge} 15 within 50 cm). Until
#' then an ESP \eqn{\ge} 15 in any argic layer sufficed; the USDA natric
#' helpers keep that reading.
#'
#' @section v0.9.76 designation + ESP-only inference (opt-in):
#' Field-described Solonetz profiles in NCSS / KSSL data routinely
#' reach the natric ESP threshold (computed from
#' \code{na_cmol / cec_cmol}) without satisfying the strict
#' \code{argic()} clay-increase test, because surveyors record
#' \code{Btk}-suffix designations (carbonates dominate the horizon
#' designation choice) rather than \code{Btn}/\code{Bn} or
#' \code{clay_pct} is missing.
#'
#' With \code{options(soilKey.natric_designation_inference = TRUE)} the
#' function accepts a layer as natric when the canonical argic test
#' returns NA or FALSE AND \emph{either}:
#' \enumerate{
#'   \item the designation matches \code{[A-Z][a-z0-9]*n} (an \code{n}
#'         master-letter modifier in the horizon name -- e.g.\
#'         \code{Btn}, \code{Btnz}, \code{Bn}, the curator's direct
#'         assertion that natric features are present), OR
#'   \item ESP >= \code{min_esp} on a B-prefixed subsoil layer
#'         (\code{top_cm > 20}) AND the layer's pH(H2O) >= 7
#'         (alkaline -- typical of true natric, excludes acidic Bt
#'         horizons that happen to read high Na from sea-spray).
#' }
#'
#' Default is \code{FALSE} (canonical behaviour preserved).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_esp Minimum ESP \% (default 15).
#' @param min_pH_h2o Minimum pH(H2O) for the ESP-only path
#'        (default 7.0; alkaline gate to exclude false-positive
#'        acidic Bt horizons).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3, Natric horizon.
#' @export
natric_horizon <- function(pedon, min_esp = 15, min_pH_h2o = 7.0) {
  # v0.9.310: the WRB natric horizon follows WRB 2022 Ch 3.1.21 (criteria 3
  # and 4, see .natric_wrb2022()). The USDA keys, which delegate their natric
  # horizon here, call .natric_horizon_impl() and keep the earlier reading.
  .natric_horizon_impl(pedon, min_esp = min_esp, min_pH_h2o = min_pH_h2o,
                       wrb2022_text = TRUE)
}

#' The natric test; \code{wrb2022_text = FALSE} is the reading before v0.9.310
#' (argic + ESP >= 15 in an argic layer), kept for the USDA keys.
#' @noRd
.natric_horizon_impl <- function(pedon, min_esp = 15, min_pH_h2o = 7.0,
                                 wrb2022_text = FALSE) {
  arg <- if (wrb2022_text) argic(pedon) else .argic_impl(pedon)
  designation_inference_enabled <- isTRUE(
    getOption("soilKey.natric_designation_inference", default = FALSE)) ||
    .morph_inference_enabled()   # v0.9.187: also under the global morph mode

  if (wrb2022_text && isTRUE(arg$passed)) {
    res <- .natric_wrb2022(pedon, arg, min_esp = min_esp)
    if (isTRUE(res$passed) || !designation_inference_enabled) return(res)
  }

  # ---- canonical path: argic + ESP >= min_esp on argic layers ----
  if (!wrb2022_text && isTRUE(arg$passed)) {
    h <- pedon$horizons
    layers <- arg$layers
    tests <- list(argic = arg)
    tests$esp_high <- test_esp_above(h, min_pct = min_esp,
                                        candidate_layers = layers)
    layers_passing <- Reduce(intersect, list(arg$layers, tests$esp_high$layers))
    missing <- unique(unlist(lapply(tests, function(t) t$missing %||% character(0))))
    if (is.null(missing)) missing <- character(0)
    any_test_na <- any(vapply(tests, function(t) is.na(t$passed), logical(1)))
    passed <- if (length(layers_passing) > 0L) TRUE
              else if (any_test_na && length(missing) > 0L) NA
              else FALSE
    if (isTRUE(passed) || !designation_inference_enabled) {
      return(DiagnosticResult$new(
        name      = "natric_horizon",
        passed    = passed,
        layers    = layers_passing,
        evidence  = tests,
        missing   = missing,
        reference = "IUSS Working Group WRB (2022), Chapter 3, Natric horizon"
      ))
    }
  }

  # ---- v0.9.76 opt-in inference path -------------------------------
  if (!designation_inference_enabled) {
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
  desig <- if (!is.null(h$designation)) as.character(h$designation)
            else rep(NA_character_, nrow(h))
  esp <- vapply(seq_len(nrow(h)),
                  function(i) compute_esp(h$na_cmol[i], h$cec_cmol[i]),
                  numeric(1))
  ph  <- if (!is.null(h$ph_h2o)) h$ph_h2o else rep(NA_real_, nrow(h))
  topcm <- if (!is.null(h$top_cm)) h$top_cm else rep(NA_real_, nrow(h))

  # Path 1: 'n' master-letter modifier in designation
  has_n_suffix <- !is.na(desig) & grepl("[A-Z][a-z0-9]*n($|[a-z]|[0-9])", desig)
  # Path 2: high ESP in alkaline subsoil B horizon
  is_subsoil_B <- !is.na(desig) & grepl("^[0-9]?B", desig) &
                    !is.na(topcm) & topcm > 20
  high_esp <- !is.na(esp) & esp >= min_esp
  alkaline <- !is.na(ph) & ph >= min_pH_h2o
  esp_alkaline_path <- is_subsoil_B & high_esp & alkaline

  inferred_layers <- which(has_n_suffix | esp_alkaline_path)

  inference_evidence <- list(
    n_suffix_match     = which(has_n_suffix),
    esp_alkaline_match = which(esp_alkaline_path),
    esp_per_layer      = esp,
    ph_per_layer       = ph,
    threshold_esp      = min_esp,
    threshold_pH       = min_pH_h2o
  )

  passed_final <- length(inferred_layers) > 0L
  DiagnosticResult$new(
    name      = "natric_horizon",
    passed    = passed_final,
    layers    = inferred_layers,
    evidence  = list(argic = arg, designation_inference = inference_evidence),
    missing   = character(0),
    reference = paste("IUSS Working Group WRB (2022), Chapter 3, Natric horizon",
                       "[v0.9.76 designation + ESP inference]")
  )
}

#' The WRB 2022 natric horizon on a profile with an argic horizon (v0.9.310).
#'
#' Criteria 1, 2 and 5 of the natric horizon (WRB 2022 Ch 3.1.21, p. 53-54)
#' are those of the argic horizon (criteria 1, 2 and 4), so the natric horizon
#' is sought among the argic layers: adjoining argic layers from a start down to
#' an end (the argic layers above or below it are an argic horizon; argic
#' criterion 3.a). The uppermost start with an end that passes is the natric
#' horizon, down to the deepest such end. Each is tested on:
#' \itemize{
#'   \item criterion 3: "a columnar or prismatic structure in some part of the
#'         horizon", or an angular or subangular blocky structure with
#'         "penetrations of an overlying coarser-textured layer ... extending
#'         >= 2.5 cm into the natric horizon", read from the lower boundary of
#'         the layer above: tongued or irregular (pockets deeper than wide)
#'         meets it, smooth does not, anything else is undecided. A structure
#'         soilKey does not recognise (a code) is undecided. When no structure
#'         is recorded in the whole horizon the criterion is not tested
#'         (Hugo's reading N8).
#'   \item criterion 4: (a) an ESP >= 15 "throughout the entire natric horizon
#'         or its upper 40 cm, whichever is thinner"; or (b) "more exchangeable
#'         Mg plus Na than Ca plus exchange acidity (buffered at pH 8.2)" over
#'         the same depth and an ESP >= 15 in a subhorizon starting <= 50 cm
#'         below its upper limit. The ESP is .natric_esp() (Hugo's reading N10).
#'         The exchange acidity is not a soilKey attribute: it is at least the
#'         exchangeable Al, and nil at a pH (H2O) of 8.2 or more; otherwise
#'         4.b is undecided where Mg + Na exceed Ca.
#' }
#' @noRd
.natric_wrb2022 <- function(pedon, arg, min_esp = 15) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "IUSS Working Group WRB (2022), Chapter 3.1.21, Natric horizon (p. 53-54)"
  col <- function(v, na = NA_real_) h[[v]] %||% rep(na, n)
  esp <- .natric_esp(h)
  st  <- .field_term(col("structure_type", NA_character_))
  columnar <- !is.na(st) & grepl("column|colun|prism", st)
  blocky   <- !is.na(st) & !columnar & grepl("block|bloco|angular", st)
  # a structure recorded that is neither (massive, granular, platy, ...)
  other <- !is.na(st) & !columnar & !blocky &
    grepl(paste0("massive|maci|granul|platy|lamin|single|simples|grao|",
                 "structureless|sem estrutura|crumb|grumo|polyhedr|polied|",
                 "nutty|wedge|cuneif|lenticular"), st)
  topo <- .field_term(col("boundary_topography", NA_character_))
  ph <- col("ph_h2o"); ca <- col("ca_cmol"); mg <- col("mg_cmol")
  na <- col("na_cmol"); al <- col("al_cmol")
  mg_na <- vapply(seq_len(n), function(i) {
    if (is.na(mg[i]) || is.na(na[i]) || is.na(ca[i])) return(NA)
    if (mg[i] + na[i] <= ca[i] + (if (is.na(al[i])) 0 else al[i])) return(FALSE)
    if (!is.na(ph[i]) && ph[i] >= 8.2) TRUE else NA
  }, logical(1))

  # one candidate horizon: the adjoining argic layers `lay`
  test_run <- function(lay) {
    top <- h$top_cm[lay[1]]
    bot <- max(h$bottom_cm[lay])
    above <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - top) < 1e-6)
    tongues <- if (!length(above) || all(is.na(topo[above]))) NA
               else if (any(grepl("tongu|irregular", topo[above]))) TRUE
               else if (any(grepl("smooth|plana", topo[above]))) FALSE else NA
    tested <- !all(is.na(st[lay]))
    may_be_columnar <- any(!columnar[lay] & !blocky[lay] & !other[lay])
    c3 <- if (any(columnar[lay])) TRUE
          else if (!tested) TRUE
          else if (isTRUE(tongues) && any(blocky[lay])) TRUE
          else if (may_be_columnar || (any(blocky[lay]) && is.na(tongues))) NA
          else FALSE
    win  <- lay[h$top_cm[lay] < min(top + 40, bot)]
    e    <- esp[win]
    c4a  <- if (any(e < min_esp, na.rm = TRUE)) FALSE else if (!anyNA(e)) TRUE else NA
    deep <- lay[h$top_cm[lay] - top <= 50]
    c4bii <- if (any(esp[deep] >= min_esp, na.rm = TRUE)) TRUE
             else if (anyNA(esp[deep])) NA else FALSE
    c4b <- .and3(.and3(mg_na[win]), c4bii)
    c4  <- if (isTRUE(c4a) || isTRUE(c4b)) TRUE
           else if (isFALSE(c4a) && isFALSE(c4b)) FALSE else NA
    list(start = lay[1], top_cm = top, layers = lay, structure_tested = tested,
         c3 = c3, c4a = c4a, c4b = c4b, passed = .and3(c3, c4))
  }
  starts <- list()
  for (r in .nitic_runs(h, arg$layers)) {
    L <- r$layers
    for (k in seq_along(L)) {
      ends <- lapply(seq.int(k, length(L)), function(m) test_run(L[k:m]))
      ok <- vapply(ends, function(x) x$passed, logical(1))
      # the deepest end that passes; else the whole run, which records why not
      best <- if (any(ok %in% TRUE)) ends[[max(which(ok %in% TRUE))]] else ends[[length(ends)]]
      best$passed <- if (any(ok %in% TRUE)) TRUE else if (anyNA(ok)) NA else FALSE
      if (is.na(best$passed)) best <- modifyList(ends[[which(is.na(ok))[1]]], list(passed = NA))
      starts[[length(starts) + 1L]] <- best
    }
  }
  pass <- vapply(starts, function(s) s$passed, logical(1))
  passed <- if (any(pass %in% TRUE)) TRUE else if (anyNA(pass)) NA else FALSE
  layers <- if (isTRUE(passed)) starts[[which(pass %in% TRUE)[1]]]$layers else integer(0)
  miss <- character(0)
  if (is.na(passed)) {
    if (any(vapply(starts, function(s) is.na(s$c3), logical(1))))
      miss <- c(miss, "structure_type", "boundary_topography")
    if (any(vapply(starts, function(s) is.na(s$c4a) && !isTRUE(s$c4b), logical(1))))
      miss <- c(miss, "na_cmol", "cec_cmol", "esp_pct", "ca_cmol", "mg_cmol")
  }
  DiagnosticResult$new(
    name      = "natric_horizon",
    passed    = passed,
    layers    = layers,
    evidence  = list(argic = arg, starts = starts, esp_pct = esp),
    missing   = miss,
    reference = ref
  )
}

#' Solonetz RSG gate (WRB 2022): "a natric horizon starting <= 100 cm from the
#' mineral soil surface" (Ch 4, p. 100). v0.9.310; until then the key asked
#' for a natric horizon at any depth.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
solonetz <- function(pedon) {
  nat <- natric_horizon(pedon)
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  starts <- nat$evidence$starts
  if (length(starts)) {
    near <- vapply(starts, function(s) s$top_cm - ms <= 100, logical(1))
    pass <- vapply(starts, function(s) s$passed, logical(1))
    passed <- if (any(pass[near] %in% TRUE)) TRUE
              else if (anyNA(pass[near])) NA else FALSE
    layers <- if (isTRUE(passed)) starts[[which(near & pass %in% TRUE)[1]]]$layers
              else integer(0)
  } else {
    passed <- nat$passed
    layers <- nat$layers %||% integer(0)
    if (isTRUE(passed) && min(h$top_cm[layers], na.rm = TRUE) - ms > 100) {
      passed <- FALSE; layers <- integer(0)
    }
  }
  DiagnosticResult$new(
    name = "solonetz", passed = passed, layers = layers,
    evidence = list(natric_horizon = nat, mineral_surface_cm = ms),
    missing = if (is.na(passed)) nat$missing %||% character(0) else character(0),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Solonetz (p. 100)")
}


#' Nitic horizon (WRB 2022)
#'
#' Tests for a nitic horizon, WRB 2022 Chapter 3.1.22, criterion by
#' criterion on each layer:
#' \enumerate{
#'   \item \eqn{\ge} 30\% clay (\code{min_clay});
#'   \item moderate to strong angular or subangular blocky structure
#'         breaking into polyhedral or flat-edged second-level structure, or
#'         polyhedral structure, with pressure faces (shiny surfaces) on
#'         \eqn{\ge} 25\% of the aggregate surfaces. Read from
#'         \code{structure_type}, \code{structure_grade},
#'         \code{structure_type_secondary} and \code{pressure_faces},
#'         "common" or more (since v0.9.317; until then from clay films,
#'         which WRB 2022 keeps apart from pressure faces);
#'   \item \eqn{\ge} 4\% Fe-dith (\code{fe_dcb_pct}), \eqn{\ge} 0.2\%
#'         Fe-ox (\code{fe_ox_pct}) and Fe-ox / Fe-dith \eqn{\ge} 0.05;
#'   \item not part of a plinthic horizon (\code{\link{plinthic}});
#'   \item the layers that meet 1-4 are \eqn{\ge} 30 cm thick together, as
#'         one horizon (\code{min_thickness}).
#' }
#' A criterion is \code{FALSE} when the data contradict it and \code{NA}
#' when the data it needs are missing: a profile without Fe-ox or structure
#' records gets \code{NA}, not a nitic horizon. With
#' \code{options(soilKey.morphological_inference = TRUE)} missing structure
#' and Fe data are read from a Bt designation with a CEC of 8-36 cmolc/kg
#' clay and no albic E above (the v0.9.18 reading of legacy profiles),
#' recorded in the evidence.
#'
#' Until v0.9.219 the structure and shiny-surface tests never vetoed,
#' Fe-ox was not read, a plinthic horizon was not excluded, any ferralic
#' horizon in the profile excluded a nitic one (WRB 2022 does not:
#' Nitisols key out before Ferralsols), the Fe inference above ran by
#' default, and a clay drop of more than 8 percentage points within 50 cm
#' vetoed it. The clay rule of the Nitisol key is in \code{\link{nitisol}}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_clay Minimum clay \% (default 30).
#' @param min_fe_dcb Minimum dithionite Fe \% (default 4).
#' @param min_thickness Minimum thickness in cm (default 30).
#' @param min_fe_ox Minimum oxalate Fe \% (default 0.2).
#' @param min_feox_fedith Minimum Fe-ox / Fe-dith ratio (default 0.05).
#' @param max_clay_drop_pct,max_decrease_depth Unused since v0.9.220, kept
#'        so that existing calls still run.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.1.22, Nitic horizon.
#' @export
nitic_horizon <- function(pedon, min_clay = 30, min_fe_dcb = 4,
                            min_thickness   = 30,
                            min_fe_ox       = 0.2,
                            min_feox_fedith = 0.05,
                            max_clay_drop_pct  = NULL,
                            max_decrease_depth = NULL) {
  h <- pedon$horizons
  n <- nrow(h)
  ref <- "IUSS Working Group WRB (2022), Chapter 3.1.22, Nitic horizon"
  col <- function(v, na) h[[v]] %||% rep(na, n)
  clay <- col("clay_pct", NA_real_)
  st   <- tolower(as.character(col("structure_type", NA_character_)))
  gr   <- tolower(as.character(col("structure_grade", NA_character_)))
  fed  <- col("fe_dcb_pct", NA_real_)
  fox  <- col("fe_ox_pct", NA_real_)
  st2  <- tolower(as.character(col("structure_type_secondary", NA_character_)))
  pf   <- .clay_films_class(col("pressure_faces", NA_character_))

  # 1. >= 30% clay
  c1 <- ifelse(is.na(clay), NA, clay >= min_clay)
  # 2. v0.9.317, as WRB 2022 Ch 3.1.22 words it (p. 55): "a. moderate to strong
  #    angular or subangular blocky structure, breaking into polyhedral or
  #    flat-edged second-level structure with pressure faces (shiny surfaces)
  #    at >= 25% of the surfaces of the soil aggregates of the second-level
  #    structure; or b. polyhedral structure with pressure faces (shiny
  #    surfaces) at >= 25% of the surfaces". The pressure faces are their own
  #    attribute, common or more read as >= 25%; clay coatings, which the text
  #    keeps apart ("In addition, clay coatings may be found"), no longer count.
  #    Nutty structure is read as polyhedral, as before.
  poly_re <- "polyhedr|poli[e\u00e9]dr|nutty|nucif"
  poly    <- !is.na(st) & grepl(poly_re, st)
  blocky  <- ifelse(is.na(st), NA, grepl("block|bloco|angular", st))
  poly2   <- ifelse(is.na(st2), NA, grepl(paste0(poly_re, "|flat.edged"), st2))
  grade_ok <- ifelse(is.na(gr), NA,
                ifelse(grepl("moderate|strong|moderad|forte", gr), TRUE,
                  ifelse(grepl("weak|fraca|massive|maci|single|simples|structureless|sem estrutura",
                               gr), FALSE, NA)))
  shiny <- ifelse(is.na(pf), NA, pf >= 3L)
  c2 <- vapply(seq_len(n), function(i) {
    b <- if (poly[i]) .and3(shiny[i]) else if (is.na(st[i])) NA else FALSE
    a <- .and3(blocky[i], grade_ok[i], poly2[i], shiny[i])
    if (isTRUE(a) || isTRUE(b)) TRUE else if (isFALSE(a) && isFALSE(b)) FALSE else NA
  }, logical(1))
  # 3. Fe-dith >= 4%, Fe-ox >= 0.2%, Fe-ox / Fe-dith >= 0.05
  c3 <- vapply(seq_len(n), function(i) .and3(
    if (is.na(fed[i])) NA else fed[i] >= min_fe_dcb,
    if (is.na(fox[i])) NA else fox[i] >= min_fe_ox,
    if (is.na(fed[i]) || is.na(fox[i]) || fed[i] <= 0) NA
    else fox[i] / fed[i] >= min_feox_fedith), logical(1))
  # 4. not part of a plinthic horizon (one soilKey cannot establish does not
  #    exclude)
  pl <- tryCatch(plinthic(pedon), error = function(e) NULL)
  c4 <- rep(TRUE, n)
  if (!is.null(pl) && isTRUE(pl$passed)) c4[pl$layers] <- FALSE

  # opt-in reading of legacy profiles: missing structure and Fe data from a
  # Bt designation with a CEC of 8-36 cmolc/kg clay and no albic E above
  inferred <- integer(0)
  if (.morph_inference_enabled()) {
    desg <- as.character(col("designation", NA_character_))
    cec  <- col("cec_cmol", NA_real_)
    cpc  <- ifelse(is.na(cec) | is.na(clay) | clay <= 0, NA, cec * 100 / clay)
    has_albic_e <- any(!is.na(desg) & grepl("^[0-9]*E[bg]?$|^[0-9]*E[0-9]", desg))
    can <- which(!is.na(desg) & grepl("^[0-9]*B[a-z]*t", desg) &
                   !is.na(cpc) & cpc >= 8 & cpc <= 36)
    if (!has_albic_e) for (i in can) {
      if (is.na(c2[i]) || is.na(c3[i])) inferred <- c(inferred, i)
      if (is.na(c2[i])) c2[i] <- TRUE
      if (is.na(c3[i])) c3[i] <- TRUE
    }
  }

  # v0.9.310: "A nitic horizon ... is a clay-rich subsurface horizon" (WRB 2022
  # Ch 3.1.22, p. 55): not the layer at the mineral soil surface nor an A
  # horizon (Hugo's reading, as for the argic horizon).
  subsurface <- !.wrb_not_subsurface(h)
  status <- vapply(seq_len(n), function(i)
    .and3(c1[i], c2[i], c3[i], c4[i], subsurface[i]), logical(1))
  # 5. >= 30 cm, as one horizon (contiguous layers)
  runs <- .nitic_runs(h, which(status %in% TRUE))
  may  <- .nitic_runs(h, which(status %in% TRUE | is.na(status)))
  long <- Filter(function(r) r$thickness >= min_thickness, runs)
  passed <- if (length(long)) TRUE
            else if (any(vapply(may, function(r) r$thickness >= min_thickness,
                                logical(1)))) NA
            else FALSE
  layers <- sort(unlist(lapply(long, `[[`, "layers")))
  cand <- which(!(status %in% FALSE))
  miss <- c(if (anyNA(c1[cand])) "clay_pct",
            if (anyNA(c2[cand])) c("structure_type", "structure_grade",
                                   "structure_type_secondary", "pressure_faces"),
            if (anyNA(c3[cand])) c("fe_dcb_pct", "fe_ox_pct"))
  DiagnosticResult$new(
    name      = "nitic_horizon",
    passed    = passed,
    layers    = layers %||% integer(0),
    evidence  = list(
      criteria = data.frame(layer = seq_len(n), clay = c1, structure_shiny = c2,
                            fe = c3, not_plinthic = c4, subsurface = subsurface,
                            status = status),
      plinthic = pl,
      morphological_inference = if (length(inferred))
        list(layers = inferred, provenance = "morphological_designation",
             note = paste("structure and Fe criteria read from a Bt designation,",
                          "CEC 8-36 cmolc/kg clay and no albic E (v0.9.18)"))),
    missing   = if (isTRUE(passed)) character(0) else unique(miss),
    reference = ref
  )
}

# Contiguous runs (by depth) of the given layers, with their thickness.
.nitic_runs <- function(h, idx) {
  idx <- idx[!is.na(h$top_cm[idx]) & !is.na(h$bottom_cm[idx])]
  if (!length(idx)) return(list())
  idx <- idx[order(h$top_cm[idx])]
  runs <- list(); cur <- idx[1]
  for (i in idx[-1]) {
    if (abs(h$top_cm[i] - h$bottom_cm[cur[length(cur)]]) < 1e-6) cur <- c(cur, i)
    else { runs[[length(runs) + 1L]] <- cur; cur <- i }
  }
  runs[[length(runs) + 1L]] <- cur
  lapply(runs, function(r) list(layers = r,
    thickness = sum(h$bottom_cm[r] - h$top_cm[r])))
}


#' Nitisol RSG gate (WRB 2022)
#'
#' The Nitisol entry of the WRB 2022 key (Chapter 4): "1. a nitic horizon
#' starting \eqn{\le} 100 cm from the mineral soil surface; and 2. from the
#' mineral soil surface to the nitic horizon, a clay content that is at least
#' half of the weighted average clay content of the nitic horizon; and 3. no
#' vertic horizon starting above or at the upper limit of the nitic horizon."
#' Criterion 2 is read layer by layer; a vertic horizon soilKey cannot
#' establish does not exclude.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 4, Nitisols.
#' @export
nitisol <- function(pedon) {
  h <- pedon$horizons
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Nitisols"
  nh <- nitic_horizon(pedon)
  if (!isTRUE(nh$passed))
    return(DiagnosticResult$new(name = "nitisol", passed = nh$passed,
      layers = integer(0), evidence = list(nitic_horizon = nh),
      missing = nh$missing %||% character(0), reference = ref))
  top <- min(h$top_cm[nh$layers])
  ms  <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  c1 <- top - ms <= 100
  w  <- h$bottom_cm[nh$layers] - h$top_cm[nh$layers]
  avg <- sum(h$clay_pct[nh$layers] * w) / sum(w)
  above <- which(!is.na(h$top_cm) & h$top_cm >= ms & h$top_cm < top)
  c2 <- if (!length(above)) TRUE
        else .and3(ifelse(is.na(h$clay_pct[above]), NA,
                          h$clay_pct[above] >= avg / 2))
  vt <- tryCatch(vertic_horizon(pedon), error = function(e) NULL)
  c3 <- !(isTRUE(vt$passed) && any(h$top_cm[vt$layers] <= top, na.rm = TRUE))
  passed <- .and3(c1, c2, c3)
  DiagnosticResult$new(
    name = "nitisol", passed = passed,
    layers = if (isTRUE(passed)) nh$layers else integer(0),
    evidence = list(nitic_horizon = nh, nitic_top_cm = top,
                    mineral_surface_cm = ms, starts_within_100 = c1,
                    nitic_clay_mean = avg, clay_above_half = c2,
                    no_vertic_above = c3, vertic_horizon = vt),
    missing = if (is.na(passed)) "clay_pct" else character(0),
    reference = ref)
}


#' Salic horizon (WRB 2022)
#'
#' Tests whether any horizon meets the salic horizon criteria. The
#' salic horizon is a horizon of soluble-salt accumulation, diagnostic
#' for Solonchaks.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness in cm (default 15).
#' @param min_ec_dS_m Primary EC threshold (default 15 dS/m at 25C).
#' @param alkaline_min_ec_dS_m Alkaline-path EC threshold (default 8
#'        dS/m, used when pH(H2O) \\>= \code{alkaline_min_pH}).
#' @param alkaline_min_pH Required pH(H2O) for alkaline path
#'        (default 8.5).
#' @param min_product Primary path product (EC * thickness in
#'        dS/m * cm) threshold (default 450 per WRB 2022).
#' @param alkaline_min_product Alkaline-path product threshold
#'        (default 240).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-tests called:
#' \itemize{
#'   \item \code{test_ec_concentration} -- EC \\>= 15 dS/m
#'         (primary) OR (EC \\>= 8 dS/m AND pH(H2O) \\>= 8.5)
#'         (alkaline).
#'   \item \code{test_minimum_thickness} -- thickness \\>= 15 cm.
#'   \item \code{test_salic_product} -- EC * thickness product
#'         \\>= 450 (primary) or \\>= 240 (alkaline) per qualifying
#'         layer.
#' }
#'
#' v0.3.1: alkaline-path and product test added (WRB 2022 Ch 3.1.20,
#' p. 49). Earlier versions only enforced the primary EC + thickness
#' gate.
#'
#' @references IUSS Working Group WRB (2022). \emph{World Reference Base
#' for Soil Resources}, 4th edition. International Union of Soil Sciences,
#' Vienna. Chapter 3.1.20 -- Salic horizon (p. 49).
#'
#' @export
salic <- function(pedon,
                    min_thickness        = 15,
                    min_ec_dS_m          = 15,
                    alkaline_min_ec_dS_m = 8,
                    alkaline_min_pH      = 8.5,
                    min_product          = 450,
                    alkaline_min_product = 240) {
  h <- pedon$horizons

  tests <- list()
  tests$ec        <- test_ec_concentration(h,
                                              min_dS_m          = min_ec_dS_m,
                                              alkaline_min_dS_m = alkaline_min_ec_dS_m,
                                              alkaline_min_pH   = alkaline_min_pH)
  tests$thickness <- test_minimum_thickness(h,
                                              min_cm           = min_thickness,
                                              candidate_layers = tests$ec$layers)
  tests$product   <- test_salic_product(h,
                                          min_product          = min_product,
                                          alkaline_min_product = alkaline_min_product,
                                          ec_path_lookup       = tests$ec$details,
                                          candidate_layers     = tests$thickness$layers)

  agg <- aggregate_subtests(tests)

  DiagnosticResult$new(
    name      = "salic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.20, Salic horizon (p. 49)"
  )
}
