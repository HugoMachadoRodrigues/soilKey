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
#' @section v0.9.72 designation morphological inference (opt-in):
#' Field-described Brazilian Gleissolos profiles (e.g.\ the Embrapa
#' Redape curated dataset) routinely encode gleyic properties via the
#' designation suffix \code{g} (e.g.\ \code{Cg}, \code{Cg1}, \code{Cgn},
#' \code{Apg}) plus low-chroma Munsell colours (chroma \\<= 2), without
#' recording \code{redoximorphic_features_pct} as a numeric percent.
#' The strict canonical test then returns \code{NA} on every horizon
#' and Gleissolos cascade to other Orders.
#'
#' With \code{options(soilKey.gleyic_designation_inference = TRUE)} the
#' function accepts a layer as gleyic when:
#' \enumerate{
#'   \item the canonical \code{redoximorphic_features_pct} test is
#'         \code{NA} for that layer, AND
#'   \item the designation matches \code{[A-Z]+g[0-9a-z]?} (a horizon
#'         name with a \code{g} suffix in the master letter sequence,
#'         e.g.\ \code{Cg}, \code{Bg2}, \code{Apg}, \code{Cgn}), AND
#'   \item the layer has \code{munsell_chroma_moist <= 2} (low-chroma
#'         reduced colour) when Munsell is recorded; if Munsell is
#'         missing on the layer the suffix alone is sufficient
#'         (designation suffix is the most direct signal of pedologist
#'         field judgment).
#' }
#'
#' This is conservative: the suffix \code{g} is a master-letter
#' modifier in the FAO/Embrapa horizon nomenclature that explicitly
#' means "gleyic-affected" -- the curator already made the call.
#' Default is \code{FALSE} (canonical behaviour preserved).
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
#' v0.9.72 adds the designation-suffix path (opt-in).
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

  # v0.9.72 -- designation-suffix morphological inference
  designation_inference_enabled <- isTRUE(
    getOption("soilKey.gleyic_designation_inference", default = FALSE))
  inferred_layers <- integer(0)
  inference_path  <- list(passed = NA, layers = integer(0), source = "off")
  if (designation_inference_enabled && !features_ok) {
    desig <- if (!is.null(h$designation)) as.character(h$designation)
              else rep(NA_character_, nrow(h))
    chroma <- if (!is.null(h$munsell_chroma_moist)) h$munsell_chroma_moist
              else rep(NA_real_, nrow(h))
    topcm  <- if (!is.null(h$top_cm)) h$top_cm else rep(NA_real_, nrow(h))
    # Match a "g" master-letter modifier: uppercase, then optional
    # lowercase modifiers OR sequence digits (e.g. C1, C2), then 'g'.
    # Catches Cg, Cg1, Cgn, Apg, 2Cgnz, 3Cgjz, 11C1g, Cgnz1, Cnz1g.
    has_g_suffix <- !is.na(desig) & grepl("[A-Z][a-z0-9]*g", desig)
    in_window    <- !is.na(topcm) & topcm <= max_top_cm
    chroma_ok    <- is.na(chroma) | chroma <= 2
    inferred_mask <- has_g_suffix & in_window & chroma_ok
    inferred_layers <- which(inferred_mask)
    inference_path <- list(
      passed = length(inferred_layers) > 0L,
      layers = inferred_layers,
      source = "designation_g_suffix",
      details = list(matched_designations = desig[inferred_mask],
                       max_top_cm           = max_top_cm)
    )
  }
  tests$designation_inference <- inference_path

  passed <- if (features_ok && !stagnic_pat) TRUE
            else if (length(inferred_layers) > 0L && !stagnic_pat) TRUE
            else if (any_na && !features_ok) NA
            else FALSE

  layers <- if (features_ok && !stagnic_pat)
              tests$gleyic_features$layers
            else if (length(inferred_layers) > 0L && !stagnic_pat)
              inferred_layers
            else integer(0)
  missing <- unique(unlist(lapply(tests[c("gleyic_features", "stagnic_pattern")],
                                    function(t) t$missing %||% character(0))))
  if (is.null(missing)) missing <- character(0)

  DiagnosticResult$new(
    name      = "gleyic_properties",
    passed    = passed,
    layers    = layers,
    evidence  = tests,
    missing   = missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 3, Gleyic properties",
                       if (length(inferred_layers) > 0L)
                         "[v0.9.72 designation-suffix inference]" else "")
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
#'        coarse-fragments path (default 90 in soilkey engine, 50
#'        in aqp engine; \code{NULL} picks a default per engine).
#' @param engine One of \code{"soilkey"} (default; strict 90\\%
#'        cfvo threshold) or \code{"aqp"} (LUCAS-friendly relaxed
#'        50\\% cfvo path \emph{plus} a thin-topsoil-with-rock path
#'        requiring positive evidence of rock contact -- v0.9.66
#'        tightening). The thin-topsoil path fires only when a
#'        horizon ending within \code{max_depth} also satisfies
#'        \emph{at least one} of: (a) designation contains "R"
#'        (e.g.\ AR, BR, Cr, R, Rk), (b)
#'        \code{coarse_fragments_pct >= 30} (gravelly), or
#'        (c) a deeper horizon is R/Cr-designated. Users with a
#'        strong external prior (e.g.\ a parent-material survey
#'        that documents rock < 25 cm but did not record it in
#'        the horizon table) can opt back into the original
#'        v0.9.65 loose behaviour with
#'        \code{options(soilKey.leptic_assume_rock_below = TRUE)}.
#'        \code{NULL} (the default) reads
#'        \code{getOption("soilKey.diagnostic_engine")}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Leptosols.
#' @export
leptic_features <- function(pedon, max_depth = 25, min_coarse_pct = NULL,
                              engine = NULL) {
  # v0.9.65: engine-aware threshold relaxation. When the global option
  # `soilKey.diagnostic_engine = "aqp"` is set (the v0.9.63 opt-in for
  # canonical NRCS dispatch), we relax `min_coarse_pct` from the WRB
  # canonical 90% to 50% AND we accept any horizon with bottom_cm
  # < max_depth + thickness < 25 cm as a Leptic candidate. This fixes
  # the v0.9.64 LUCAS over-Cambisols artifact: LUCAS topsoil-only data
  # has neither R/Cr designation nor cfvo >= 90, so the strict leptic
  # path never fires, and LUCAS Leptosols cascade to Cambisols.
  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))
  if (is.null(min_coarse_pct))
    min_coarse_pct <- if (engine == "aqp") 50 else 90

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
  # v0.9.65 engine="aqp" path: shallow topsoil with positive evidence
  # of rock contact (v0.9.66 tightening). The original v0.9.65
  # implementation accepted ANY horizon ending <= 25 cm as leptic,
  # which over-fired on LUCAS topsoil-only data (29/30 pedons collapsed
  # onto Leptosols regardless of true class). v0.9.66 adds a gate:
  # the shallow horizon must show positive evidence of rock,
  # operationalised as
  #   (a) designation contains R (e.g., AR, BR, Cr, R, Rk), OR
  #   (b) coarse_fragments_pct >= 30 (gravelly / very gravelly), OR
  #   (c) a deeper horizon designated R / Cr is present in the profile.
  # If the user has strong external priors (a parent material survey
  # that documents rock < 25 cm), they can opt back into the loose
  # behaviour with options(soilKey.leptic_assume_rock_below = TRUE).
  if (engine == "aqp") {
    assume_rock <- isTRUE(getOption("soilKey.leptic_assume_rock_below",
                                      default = FALSE))
    cfvo <- if (!is.null(h$coarse_fragments_pct)) h$coarse_fragments_pct
            else rep(NA_real_, nrow(h))
    desig <- if (!is.null(h$designation)) as.character(h$designation)
              else rep(NA_character_, nrow(h))
    has_R_in_designation <- !is.na(desig) & grepl("R", desig)
    has_high_cfvo        <- !is.na(cfvo) & cfvo >= 30
    has_R_below          <- any(!is.na(desig) &
                                  grepl("^R$|^Cr|^R[a-z]", desig))
    rock_evidence_per_row <- has_R_in_designation | has_high_cfvo |
                                rep(has_R_below, nrow(h))
    if (isTRUE(assume_rock)) rock_evidence_per_row[] <- TRUE
    shallow_layers <- which(
      !is.na(h$bottom_cm) & h$bottom_cm <= max_depth &
        rock_evidence_per_row
    )
    paths$thin_topsoil <- list(
      shallow_topsoil_with_rock = list(
        passed = length(shallow_layers) > 0L,
        layers = shallow_layers,
        details = list(max_depth = max_depth,
                          rock_R_designation = any(has_R_in_designation),
                          rock_high_cfvo     = any(has_high_cfvo),
                          rock_R_below       = has_R_below,
                          assume_rock_option = assume_rock)
      )
    )
  }

  agg <- aggregate_alternatives(paths)

  DiagnosticResult$new(
    name      = "leptic_features",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = c(paths, list(engine = engine)),
    missing   = agg$missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 5,",
                        "Leptosols",
                        if (engine == "aqp") "[engine=aqp relaxed]" else "")
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
#' @section v0.9.80 OC + BD proxy (opt-in):
#' Field-described volcanic-ash soils (e.g.\ AfSP, KSSL/NASIS, SOTER)
#' routinely lack oxalate Al/Fe and phosphate retention measurements,
#' so the canonical paths return \code{NA} and Andosols cascade to
#' other RSGs. The genetic signature is still detectable from coarser
#' data: very high SOC (>= 4-5\%) plus low bulk density
#' (<= 0.9 g/cm^3) typical of allophanic / Al-humus complexation.
#'
#' With \code{options(soilKey.andic_oc_bd_proxy = TRUE)} the function
#' adds a third path that fires when both canonical paths fail and the
#' surface horizon shows \code{oc_pct >= min_oc_proxy} AND
#' \code{bulk_density_g_cm3 <= max_bd_proxy} (or OC alone >= 5\% when
#' BD is missing). Default is \code{FALSE} (canonical behaviour
#' preserved).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_alfe Minimum (Al_ox + 0.5*Fe_ox) percent for the Al-Fe
#'        path (default 2.0).
#' @param max_bd Maximum bulk density g/cm^3 for the Al-Fe path
#'        (default 0.9).
#' @param min_p_retention Minimum phosphate retention \% for the P
#'        path (default 70).
#' @param min_oc_proxy Minimum SOC \% for the v0.9.80 OC+BD proxy
#'        path (default 4.0). Only consulted when the proxy is
#'        enabled via \code{options(soilKey.andic_oc_bd_proxy = TRUE)}.
#' @param max_bd_proxy Maximum bulk density g/cm^3 for the v0.9.80
#'        OC+BD proxy path (default 0.9). Only consulted when the
#'        proxy is enabled.
#' @section v0.9.85 proxy contiguous-layer extension (opt-in):
#' When \code{options(soilKey.andic_oc_bd_proxy_extend = TRUE)}
#' (only meaningful with \code{soilKey.andic_oc_bd_proxy = TRUE}),
#' iteratively extend the proxy layers to include contiguous deeper
#' layers whose \code{oc_pct >= min_oc_proxy / 2} AND whose
#' \code{bulk_density_g_cm3} is missing OR
#' \code{<= max_bd_proxy + 0.15}. The extension stops at the first
#' horizon failing either constraint, so a ferralic / argic subsoil
#' cannot accidentally inflate the andic thickness. Default is
#' \code{FALSE} -- canonical proxy behaviour preserved.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3, Andic
#'   properties.
#' @export
andic_properties <- function(pedon,
                                min_alfe         = 2.0,
                                max_bd           = 0.9,
                                min_p_retention  = 70,
                                min_oc_proxy     = 4.0,
                                max_bd_proxy     = 0.9) {
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

  # v0.9.80 -- High-OC + low-BD proxy (opt-in).
  # Field-described volcanic-ash soils (KSSL/AfSP/SOTER) routinely have
  # oxalate Al/Fe and phosphate retention NOT measured. The signature
  # of andic-property genesis IS still detectable from coarser data:
  # very high SOC (>= 5%) on the surface horizon AND low bulk density
  # (<= 0.9 g/cm3) -- the same low-BD threshold the canonical Al-Fe
  # path uses. This proxy is conservative: 5% OC requires either a
  # melanic/hyperhumic surface OR a thick organic accumulation typical
  # of allophanic / Al-humus complexation in volcanic ash.
  proxy_enabled <- isTRUE(getOption("soilKey.andic_oc_bd_proxy",
                                       default = FALSE))
  proxy_path <- list(passed = NA, layers = integer(0), source = "off")
  if (proxy_enabled && !isTRUE(alfe_test$passed) &&
        !isTRUE(paths$phosphate_retention$p_retention$passed)) {
    oc <- if (!is.null(h$oc_pct)) h$oc_pct else rep(NA_real_, nrow(h))
    bd <- if (!is.null(h$bulk_density_g_cm3)) h$bulk_density_g_cm3
          else rep(NA_real_, nrow(h))
    # Two sub-paths within the proxy:
    #   (a) OC >= min_oc_proxy AND BD <= max_bd_proxy (both measured)
    #   (b) OC >= min_oc_proxy + 1 AND BD missing (high OC alone, when
    #       BD wasn't measured; the high OC requires Al-humus complex
    #       formation typical of volcanic ash genesis).
    high_oc <- !is.na(oc) & oc >= min_oc_proxy
    very_high_oc <- !is.na(oc) & oc >= (min_oc_proxy + 1)
    low_bd <- !is.na(bd) & bd <= max_bd_proxy
    bd_missing <- is.na(bd)
    inferred <- which((high_oc & low_bd) |
                          (very_high_oc & bd_missing))
    if (length(inferred) > 0L) {
      # v0.9.85 -- proxy contiguous-layer extension (opt-in).
      # When the v0.9.80 proxy fires on a surface horizon, AfSP /
      # SOTER Andosol references like KE SOTER_182/4-75 (Ah 0-25 cm
      # OC=4.7 BD=0.8 -> proxy fires; AB 25-50 cm OC=2.7 BD=1.0 ->
      # below v0.9.80 thresholds) lose the AB layer from the andic
      # thickness even though the AB clearly belongs to the same
      # andic-affected mantle.
      #
      # When `soilKey.andic_oc_bd_proxy_extend = TRUE` (opt-in,
      # default FALSE -- only meaningful with the v0.9.80 proxy
      # enabled), iteratively extend `inferred` to include
      # contiguous deeper layers whose OC stays >= min_oc_proxy / 2
      # AND whose BD is either missing OR <= max_bd_proxy + 0.15
      # (additive slack -- BD = 1.0 still counts when the surface
      # threshold is 0.9, but BD = 1.3 [a typical mineral subsoil]
      # does not). The extension stops at the first horizon failing
      # either constraint, so a ferralic / argic subsoil cannot
      # accidentally inflate the andic thickness.
      extend_enabled <- isTRUE(getOption("soilKey.andic_oc_bd_proxy_extend",
                                           default = FALSE))
      extended_layers <- integer(0)
      if (extend_enabled) {
        oc_min_extend <- min_oc_proxy / 2
        bd_max_extend <- max_bd_proxy + 0.15
        # Extend below the lowest currently-firing layer through
        # contiguous deeper horizons that still satisfy the looser
        # extension thresholds.
        stop_at <- max(inferred)
        for (j in seq(stop_at + 1L, nrow(h))) {
          if (j > nrow(h)) break
          oc_ok <- !is.na(oc[j]) && oc[j] >= oc_min_extend
          bd_ok <- is.na(bd[j]) || (!is.na(bd[j]) &&
                                       bd[j] <= bd_max_extend)
          if (!isTRUE(oc_ok) || !isTRUE(bd_ok)) break
          extended_layers <- c(extended_layers, j)
        }
      }
      proxy_path <- list(
        passed = TRUE,
        layers = c(inferred, extended_layers),
        source = if (length(extended_layers) > 0L)
                   "high_oc_low_bd_extended" else "high_oc_low_bd",
        details = list(min_oc = min_oc_proxy, max_bd = max_bd_proxy,
                          matched_layers = inferred,
                          extended_layers = extended_layers,
                          extend_enabled  = extend_enabled)
      )
    }
  }
  paths$oc_bd_proxy <- proxy_path

  agg <- aggregate_alternatives(paths[c("alfe_lowbd", "phosphate_retention")])
  inferred_passed <- isTRUE(proxy_path$passed)
  passed <- isTRUE(agg$passed) || isTRUE(inferred_passed)
  layers <- if (isTRUE(agg$passed)) agg$layers
            else if (isTRUE(inferred_passed)) proxy_path$layers
            else integer(0)

  DiagnosticResult$new(
    name      = "andic_properties",
    passed    = passed,
    layers    = layers,
    evidence  = paths,
    missing   = agg$missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 3, Andic properties",
                       if (inferred_passed) "[v0.9.80 OC+BD proxy]" else "")
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
