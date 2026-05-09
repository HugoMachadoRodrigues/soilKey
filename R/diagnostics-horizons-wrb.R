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
#'        See \code{\link{test_clay_increase_argic}} for the table.
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
argic <- function(pedon, min_thickness = 7.5,
                    system = c("wrb2022", "usda"),
                    engine = NULL,
                    require_t = NULL) {
  system <- match.arg(system)
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

  tests <- list()
  # v0.9.26: per-system clay-increase thresholds. WRB 2022 (default)
  # uses 6/1.4/20; KST 13ed uses 3/1.2/8 (looser).
  tests$clay_increase <- test_clay_increase_argic(h, system = system)

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
#' @param max_cec Maximum CEC (1M NH4OAc, pH 7) per kg clay
#'        (default \code{NULL} = 16 in soilkey engine, 20 in aqp
#'        engine; see \code{engine}).
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
#'   \item \code{\link{test_ferralic_texture}} -- texture sandy loam or
#'         finer.
#'   \item \code{\link{test_cec_per_clay}} -- CEC / clay <= 16 (or 20
#'         under \code{engine = "aqp"}) cmol_c/kg clay.
#'   \item \code{\link{test_ferralic_thickness}} -- thickness >= 30 cm.
#' }
#'
#' v0.3.1 alignment with WRB 2022 Ch 3.1.10 (p. 44): the older
#' "ECEC <= 12 cmol_c/kg clay" gate was removed because it is not in the
#' canonical text -- only CEC (1M NH4OAc, pH 7) <= 16 is required.
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
                       engine        = NULL) {
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))
  if (is.null(max_cec))
    max_cec <- getOption("soilKey.ferralic_max_cec",
                            default = if (engine == "aqp") 20 else 16)

  h <- pedon$horizons

  tests <- list()
  tests$texture       <- test_ferralic_texture(h)
  tests$cec_per_clay  <- test_cec_per_clay(h,
                                             max_cmol_per_kg_clay = max_cec)
  tests$thickness     <- test_ferralic_thickness(h, min_cm = min_thickness)

  agg <- aggregate_subtests(tests)

  notes_str <- paste0(
    "v0.3.1: ECEC/clay <= 12 test removed; not part of WRB 2022 ferralic. ",
    "v0.9.67 engine=", engine, " threshold = ", max_cec, " cmol_c/kg clay."
  )

  DiagnosticResult$new(
    name      = "ferralic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = c(tests, list(engine = engine, max_cec_used = max_cec)),
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

  # v0.9.78 -- mollic candidate-layer logic.
  #
  # WRB 2022 Ch 3: the mollic horizon is the diagnostic SURFACE horizon
  # AND must be at least 20 cm thick. Previously soilKey gated the
  # candidate set with `top_cm <= surface_top_cm` (default 5 cm), which
  # excluded contiguous A12 / AB layers that DO belong to the mollic
  # horizon as a single morphological unit. The KE W3_0289 Phaeozem
  # has A11 (0-10) + A12 (10-27): 27 cm of mollic-passing material, but
  # only A11 entered the candidate pool, so thickness 10 < 20 -> fail.
  #
  # The fix: candidate_layers is the CONTIGUOUS stack of layers from
  # the surface where each layer passes the mollic color test. This
  # matches WRB's "contiguous from surface" definition.

  if (nrow(h) == 0L) {
    return(DiagnosticResult$new(
      name = "mollic", passed = NA, layers = integer(0),
      evidence = list(), missing = character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 3, Mollic horizon"
    ))
  }

  # v0.9.78: when the surface layer has insufficient evidence
  # (NA Munsell value/chroma AND NA OC AND NA BS), we CANNOT
  # determine whether the surface is mollic. Return NA rather
  # than firing the contiguous-stack inference (which permissively
  # default-passes such layers via OC-inference and would yield
  # spurious TRUE on profiles whose surface morphology is unknown).
  surf_idx <- which(!is.na(h$top_cm) & h$top_cm <= surface_top_cm)
  if (length(surf_idx) > 0L) {
    si <- surf_idx[1]
    surf_all_na <- is.na(h$munsell_value_moist[si]) &&
                      is.na(h$munsell_chroma_moist[si]) &&
                      is.na(h$oc_pct[si]) &&
                      is.na(h$bs_pct %||% rep(NA_real_, nrow(h)))[si]
    if (isTRUE(surf_all_na)) {
      return(DiagnosticResult$new(
        name = "mollic",
        passed = NA,
        layers = integer(0),
        evidence = list(surface_attributes_all_na = TRUE),
        missing = c("munsell_value_moist", "munsell_chroma_moist",
                     "oc_pct", "bs_pct"),
        reference = "IUSS Working Group WRB (2022), Chapter 3, Mollic horizon"
      ))
    }
  }

  # Order by top_cm to compute the contiguous stack
  ord <- order(h$top_cm, na.last = TRUE)
  # Quick mollic-color check on every layer
  cl_all <- seq_len(nrow(h))
  color_check <- test_mollic_color(h, candidate_layers = cl_all)
  passing_color <- color_check$layers
  # Find the surface-anchored contiguous stack: starts with a layer
  # whose top_cm <= surface_top_cm, then extends downward while each
  # next layer (a) starts where the previous ends and (b) passes color.
  candidate_layers <- integer(0)
  for (i in ord) {
    if (length(candidate_layers) == 0L) {
      # First layer must be surface-anchored AND pass color
      if (!is.na(h$top_cm[i]) && h$top_cm[i] <= surface_top_cm &&
            i %in% passing_color) {
        candidate_layers <- c(candidate_layers, i)
      } else if (!is.na(h$top_cm[i]) && h$top_cm[i] <= surface_top_cm) {
        # surface layer fails color -> can't be mollic
        break
      }
    } else {
      # Subsequent layer must contiguous AND pass color
      last <- candidate_layers[length(candidate_layers)]
      gap <- h$top_cm[i] - h$bottom_cm[last]
      if (!is.na(gap) && abs(gap) <= 1 && i %in% passing_color) {
        candidate_layers <- c(candidate_layers, i)
      } else {
        break
      }
    }
  }

  tests <- list()
  tests$color           <- color_check
  tests$organic_carbon  <- test_mollic_organic_carbon(h,
                                                        min_pct = min_oc,
                                                        candidate_layers = candidate_layers)
  tests$base_saturation <- test_mollic_base_saturation(h,
                                                         min_pct = min_bs,
                                                         candidate_layers = candidate_layers)
  # v0.9.78: cumulative thickness check on the contiguous stack
  # (test_minimum_thickness checks each layer individually against
  # min_cm, but mollic requires the SUMMED thickness of the
  # contiguous stack to reach 20 cm -- a single A11 layer of 10 cm
  # plus A12 of 17 cm = 27 cm cumulative, valid mollic).
  if (length(candidate_layers) > 0L) {
    thk <- sum(pmax(h$bottom_cm[candidate_layers] -
                          h$top_cm[candidate_layers], 0, na.rm = TRUE),
                 na.rm = TRUE)
    tests$thickness <- list(
      passed  = thk >= min_thickness,
      layers  = if (thk >= min_thickness) candidate_layers else integer(0),
      missing = character(0),
      details = list(cumulative_cm = thk, threshold = min_thickness,
                       layers       = candidate_layers)
    )
  } else {
    # No surface-anchored stack found. Distinguish between (a) NA
    # input (surface Munsell/OC/BS missing -> NA) vs (b) genuine
    # FAIL (surface fails colour with measured values -> FALSE).
    surface_idx <- which(!is.na(h$top_cm) & h$top_cm <= surface_top_cm)
    has_na_surface <- length(surface_idx) > 0L && any(
      is.na(h$munsell_value_moist[surface_idx]) |
        is.na(h$munsell_chroma_moist[surface_idx])
    )
    tests$thickness <- list(
      passed = if (has_na_surface) NA else FALSE,
      layers = integer(0),
      missing = if (has_na_surface)
                  c("munsell_value_moist", "munsell_chroma_moist")
                else c("top_cm", "bottom_cm"),
      details = list(cumulative_cm = 0, threshold = min_thickness)
    )
  }
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
                   "No surface-anchored contiguous mollic stack found"
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
#' @param min_top_cm Minimum top depth (cm) for a horizon to be
#'        considered cambic-eligible (default 5). Anchors the
#'        candidate set to subsurface layers.
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

  arg_res <- argic(pedon)
  fer_res <- ferralic(pedon)

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

  # v0.9.72 -- designation-suffix morphological inference
  designation_inference_enabled <- isTRUE(
    getOption("soilKey.plinthic_designation_inference", default = FALSE))
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
    reference = paste("IUSS Working Group WRB (2022), Chapter 3, Plinthic horizon",
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

  DiagnosticResult$new(
    name      = "spodic",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
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
#' sodium accumulation (ESP >= 15\%) within at least one argic layer.
#' Diagnostic of Solonetz.
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
  arg <- argic(pedon)
  designation_inference_enabled <- isTRUE(
    getOption("soilKey.natric_designation_inference", default = FALSE))

  # ---- canonical path: argic + ESP >= min_esp on argic layers ----
  if (isTRUE(arg$passed)) {
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


#' Nitic horizon (WRB 2022)
#'
#' Tests for the nitic horizon: a clay-rich (>= 30\%), Fe-rich (DCB
#' Fe >= 4\%) subsurface horizon at least 30 cm thick. Diagnostic of
#' Nitisols. WRB 2022 additionally requires polyhedral / nutty
#' structure with shiny ped surfaces and a gradual (non-abrupt) clay
#' decrease with depth.
#'
#' Required (AND-combined) sub-tests:
#' \itemize{
#'   \item Profile does not have a ferralic horizon (Ferralsol path
#'         is canonical for the clay-rich + low-CEC corner).
#'   \item clay \% >= \code{min_clay}.
#'   \item fe_dcb_pct >= \code{min_fe_dcb}.
#'   \item thickness >= \code{min_thickness}.
#' }
#'
#' Supplementary (soft-AND) sub-tests -- evaluated when evidence is
#' present in the pedon, evaluate to NA (not a fail) when missing:
#' \itemize{
#'   \item structure_type matches polyhedral / nutty / (sub)angular
#'         blocky.
#'   \item slickensides / shiny ped surfaces present (proxy for
#'         WRB's "shiny ped surfaces").
#'   \item clay does not decrease abruptly between adjacent layers
#'         within 50 cm of the surface (gradual-decrease pattern;
#'         drop > 8 percentage points fails).
#' }
#' Supplementary tests fail (return passed = FALSE) only when evidence
#' actively contradicts the criterion; missing evidence is permissive.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_clay Minimum clay \% (default 30).
#' @param min_fe_dcb Minimum DCB-extractable Fe \% (default 4).
#' @param min_thickness Minimum thickness in cm (default 30).
#' @param max_clay_drop_pct Maximum clay drop (percentage points)
#'        between adjacent layers within \code{max_decrease_depth}
#'        before failing the gradual-decrease test (default 8).
#' @param max_decrease_depth Depth window (cm) for the gradual-decrease
#'        check (default 50).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3, Nitic horizon.
#' @export
nitic_horizon <- function(pedon, min_clay = 30, min_fe_dcb = 4,
                            min_thickness        = 30,
                            max_clay_drop_pct    = 8,
                            max_decrease_depth   = 50) {
  h <- pedon$horizons

  fer <- ferralic(pedon)
  if (isTRUE(fer$passed)) {
    return(DiagnosticResult$new(
      name      = "nitic_horizon",
      passed    = FALSE,
      layers    = integer(0),
      evidence  = list(ferralic = fer),
      missing   = fer$missing %||% character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 3, Nitic horizon",
      notes     = "Excluded -- profile has a ferralic horizon (Ferralsol path)"
    ))
  }

  tests <- list()
  tests$not_ferralic <- list(
    passed  = !isTRUE(fer$passed),
    layers  = if (isTRUE(fer$passed)) integer(0) else seq_len(nrow(h)),
    missing = fer$missing %||% character(0),
    details = list(ferralic_passed = fer$passed),
    notes   = NA_character_
  )
  tests$clay   <- test_clay_above(h, min_pct = min_clay)
  tests$fe_dcb <- test_fe_dcb_above(h, min_pct = min_fe_dcb,
                                       candidate_layers = tests$clay$layers)

  # v0.9.18: when fe_dcb_pct is missing across all clay-qualifying
  # layers, infer plausible Fe-rich behaviour from (a) Bt designation
  # + (b) CEC/clay activity in the 8-36 cmol/kg-clay range +
  # (c) NO albic E horizon above the Bt (Nitisol diagnostic morphology
  # does not include an albic E -- profiles with an E above are
  # canonically Acrisols / Lixisols / Alisols / Luvisols / Retisols).
  # The inference does NOT fire when fe_dcb is measured -- the
  # canonical gate stays in charge for lab-grade profiles. Closes the
  # FEBR Nitosols 0/14 gap diagnosed in the v0.9.17 benchmark
  # without regressing the canonical Acrisol / Lixisol / Alisol
  # fixtures (which all have an E horizon).
  if (is.na(tests$fe_dcb$passed) || isFALSE(tests$fe_dcb$passed)) {
    fe_layers <- tests$clay$layers
    have_fe_data <- any(!is.na(h$fe_dcb_pct[fe_layers]))
    has_albic_E <- any(!is.na(h$designation) &
                          grepl("^E[bg]?$|^E[0-9]", h$designation))
    if (length(fe_layers) > 0L && !have_fe_data && !has_albic_E) {
      desg <- h$designation[fe_layers]
      bt_pattern <- !is.na(desg) & grepl("^B[a-z]*t", desg)
      cec_per_clay_in_range <- vapply(fe_layers, function(j) {
        if (is.na(h$cec_cmol[j]) || is.na(h$clay_pct[j]) ||
              h$clay_pct[j] <= 0) return(FALSE)
        cpc <- h$cec_cmol[j] * 100 / h$clay_pct[j]
        !is.na(cpc) && cpc >= 8 && cpc <= 36
      }, logical(1))
      inferred <- fe_layers[bt_pattern & cec_per_clay_in_range]
      if (length(inferred) > 0L) {
        tests$fe_dcb <- list(
          passed  = TRUE,
          layers  = inferred,
          missing = "fe_dcb_pct",
          details = list(source = "inferred_from_Bt_and_cec_per_clay_and_no_albic_E",
                          note   = paste0("v0.9.18: fe_dcb_pct missing; ",
                                           "Fe-rich behaviour inferred from ",
                                           "Bt designation + CEC/clay in ",
                                           "[8, 36] + no albic E")),
          notes   = NA_character_
        )
      }
    }
  }

  tests$thickness <- test_minimum_thickness(h, min_cm = min_thickness,
                                              candidate_layers = tests$fe_dcb$layers)

  # Supplementary structure / morphology / pattern tests. These are
  # AND-combined only when evidence is conclusive (passed is TRUE or
  # FALSE). Missing evidence (passed is NA) is treated as permissive.
  struct <- test_polyhedral_or_nutty_structure(
              h, candidate_layers = tests$thickness$layers)
  shiny  <- test_shiny_ped_surfaces(
              h, candidate_layers = tests$thickness$layers)
  clay_dec <- test_clay_decreases_with_depth(
                h, candidate_layers = tests$thickness$layers,
                   max_drop_pct     = max_clay_drop_pct,
                   max_depth_cm     = max_decrease_depth)
  if (isFALSE(struct$passed))   tests$structure        <- struct
  if (isFALSE(shiny$passed))    tests$shiny_peds       <- shiny
  if (isFALSE(clay_dec$passed)) tests$clay_decrease    <- clay_dec
  # Always record the structural evidence that DID pass / NA in
  # evidence so the trace is full.
  tests$evidence_structure     <- struct
  tests$evidence_shiny_peds    <- shiny
  tests$evidence_clay_decrease <- clay_dec

  agg <- aggregate_subtests(
    tests,
    layer_tests = c("not_ferralic", "clay", "fe_dcb", "thickness")
  )
  # v0.9.18: only hard-fail on a conclusively FALSE clay-decrease
  # pattern. test_polyhedral_or_nutty_structure() and
  # test_shiny_ped_surfaces() now return NA when evidence is missing
  # or inconclusive (rather than FALSE), so they are evidence-only
  # and never veto. The clay-decrease test still vetoes when there
  # IS measured clay data showing a > 8 pp drop -- that's
  # mineralogically incompatible with a nitic horizon.
  if (isFALSE(clay_dec$passed)) {
    agg$passed <- FALSE
    agg$layers <- integer(0)
  }

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
#'   \item \code{\link{test_ec_concentration}} -- EC \\>= 15 dS/m
#'         (primary) OR (EC \\>= 8 dS/m AND pH(H2O) \\>= 8.5)
#'         (alkaline).
#'   \item \code{\link{test_minimum_thickness}} -- thickness \\>= 15 cm.
#'   \item \code{\link{test_salic_product}} -- EC * thickness product
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
