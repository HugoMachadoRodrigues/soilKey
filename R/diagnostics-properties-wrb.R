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
#' Tests whether the profile shows gleyic properties (WRB 2022 Ch 3.2.6,
#' p. 75) in a layer starting within the upper \code{max_top_cm} (default
#' 50). Gleyic properties qualify the Gleysols and many other RSGs (Gleyic,
#' Endogleyic, Epigleyic qualifiers).
#'
#' Since v0.9.312 the layer reading follows the text and Hugo's reading of
#' it for field descriptions that do not say where the mottles sit: a layer
#' has gleyic properties when its matrix has a reductimorphic colour, moist
#' (criterion 1: hue N, 10Y, GY, G, BG, B or PB, or hue 2.5Y or 5Y with a
#' chroma of 2 or less; with more than 5\% oximorphic features these are read
#' as the gleyic features of criterion 2 over a reduced matrix); when it has
#' more than 5\% oximorphic features and lies directly on a layer with such a
#' matrix (criterion 3); or when it has them and the site records
#' groundwater within 150 cm that is not perched
#' (\code{site$groundwater_kind}, \code{site$groundwater_depth_cm}). A
#' mottled layer with none of these is \code{NA}, unless the site records no
#' groundwater, a perched water table or a deeper one, or a claric layer
#' lies directly above it: its features are then stagnic. Continuous rock
#' has no gleyic properties. Until v0.9.311 the features also had to
#' continue with depth (a "decay" test that is not in WRB 2022 and failed
#' the textbook oximorphic-over-reductimorphic sequence).
#'
#' @section v0.9.72 designation morphological inference (opt-in):
#' Field-described Brazilian Gleissolos profiles (e.g.\ the Embrapa
#' Redape curated dataset) routinely encode gleyic properties via the
#' designation suffix \code{g} (e.g.\ \code{Cg}, \code{Cg1}, \code{Cgn},
#' \code{Apg}) plus low-chroma Munsell colours (chroma \\<= 2), without
#' recording \code{redoximorphic_features_pct} as a numeric percent.
#'
#' With \code{options(soilKey.gleyic_designation_inference = TRUE)} the
#' function accepts a layer as gleyic when the reading above does not pass,
#' the designation matches \code{[A-Z][a-z0-9]*g} and
#' \code{munsell_chroma_moist <= 2} (or no Munsell colour). Default is
#' \code{FALSE}.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) of a candidate layer
#'        (default 50).
#' @param min_redox_pct The oximorphic features of criterion 2 are more
#'        than this \% of the layer (default 5).
#' @param stagnic_decay_factor Not used since v0.9.312 (the decay test was
#'        removed); kept so that existing calls still work.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.2.6, Gleyic
#'   properties.
#' @export
gleyic_properties <- function(pedon, max_top_cm = 50, min_redox_pct = 5,
                                 stagnic_decay_factor = 3) {
  h <- pedon$horizons
  z <- .wrb_redox_layers(pedon, min_redox_pct = min_redox_pct)
  in_window <- !is.na(h$top_cm) & h$top_cm <= max_top_cm
  layers <- which(in_window & z$gleyic %in% TRUE)
  undecided <- which(in_window & is.na(z$gleyic))
  passed <- if (length(layers)) TRUE else if (length(undecided)) NA else FALSE
  missing <- .redox_missing(z, undecided)
  tests <- list(redox_layers = z, undecided_layers = undecided)

  # v0.9.72 -- designation-suffix morphological inference (opt-in)
  inferred_layers <- integer(0)
  if (isTRUE(getOption("soilKey.gleyic_designation_inference", default = FALSE)) &&
      !isTRUE(passed)) {
    desig  <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
    chroma <- h$munsell_chroma_moist %||% rep(NA_real_, nrow(h))
    inferred_layers <- which(!is.na(desig) & grepl("[A-Z][a-z0-9]*g", desig) &
                               in_window & (is.na(chroma) | chroma <= 2))
    tests$designation_inference <- list(
      passed = length(inferred_layers) > 0L, layers = inferred_layers,
      source = "designation_g_suffix",
      details = list(matched_designations = desig[inferred_layers],
                     max_top_cm = max_top_cm))
    if (length(inferred_layers)) { passed <- TRUE; layers <- inferred_layers }
  }

  # v0.9.187 -- designation fallback (opt-in): a bare Bg/Cg records field-
  # observed gleying; accept it when the redox data is missing.
  mi <- .apply_morph_inference("gleyic", h, passed, layers, tests)
  passed <- mi$passed; layers <- mi$layers; tests <- mi$evidence

  DiagnosticResult$new(
    name      = "gleyic_properties",
    passed    = passed,
    layers    = layers,
    evidence  = tests,
    missing   = if (isTRUE(passed)) character(0) else missing,
    reference = paste("IUSS Working Group WRB (2022), Chapter 3.2.6, Gleyic properties (p. 75)",
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

  # v0.9.220: continuous rock is R, RCr or R/Cr (.WRB_ROCK_DESIGNATION); Cr,
  # weathered or soft bedrock, made Leptosols of soils over saprolite.
  designation <- test_designation_pattern(h, pattern = .WRB_ROCK_DESIGNATION)
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
#' @section v0.9.313:
#' The answer is read layer by layer and is \code{NA} when a value it
#' needs is not recorded (until v0.9.312 an undecided profile returned
#' \code{FALSE}); \code{evidence$layer_status} holds the answer per layer.
#' A layer fails when Al_ox + 0.5*Fe_ox < \code{min_alfe} or the bulk
#' density > \code{max_bd} is measured (phosphate retention, an optional
#' path, can still pass it). Where bulk density is not measured,
#' Al_ox + 0.5*Fe_ox >= \code{min_alfe} together with a pH in NaF
#' (\code{ph_naf}) >= 9.5 in a carbonate-free layer passes, read from the
#' field test of WRB 2022 Ch 3.2.3; this is a proxy, named in
#' \code{notes}, \code{reference} and \code{evidence$naf_field_test}.
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

  # v0.9.313: the answer per layer (.andic_layer_status()), so that a value
  # not recorded stays undecided. Until v0.9.312 `isTRUE(agg) || isTRUE(proxy)`
  # turned every undecided profile into FALSE. Where bulk density is not
  # measured, Alox + 1/2 Feox >= 2% with a pH NaF >= 9.5 is read as andic
  # (WRB 2022 Ch 3.2.3, p 73, field identification; Hugo, N2): a proxy, named
  # in `notes`, `reference` and `evidence$naf_field_test`.
  st <- .andic_layer_status(h, min_alfe = min_alfe, max_bd = max_bd,
                            min_p_retention = min_p_retention)
  paths$naf_field_test <- list(
    passed = length(st$naf) > 0L, layers = st$naf,
    details = list(rule = "bulk density not measured; Alox + 1/2 Feox >= 2 and pH NaF >= 9.5, carbonate-free",
                   proxy = TRUE))
  canonical <- if (any(st$status %in% TRUE)) TRUE
               else if (all(st$status %in% FALSE)) FALSE
               else NA
  inferred_passed <- !isTRUE(canonical) && isTRUE(proxy_path$passed)
  passed <- if (isTRUE(canonical) || inferred_passed) TRUE else canonical
  layers <- if (isTRUE(canonical)) which(st$status %in% TRUE)
            else if (inferred_passed) proxy_path$layers
            else integer(0)
  naf_used <- isTRUE(canonical) && length(st$naf) > 0L
  evidence <- c(paths, list(layer_status = st$status))

  DiagnosticResult$new(
    name      = "andic_properties",
    passed    = passed,
    layers    = layers,
    evidence  = evidence,
    missing   = if (is.na(passed)) st$missing else character(0),
    reference = paste("IUSS Working Group WRB (2022), Chapter 3, Andic properties",
                       if (inferred_passed) "[v0.9.80 OC+BD proxy]"
                       else if (naf_used) "[pH NaF field test where bulk density is not measured]"
                       else ""),
    notes     = if (naf_used)
                  sprintf(paste("Andic properties in layer(s) %s read from Alox + 1/2 Feox",
                                "and the NaF field test (pH NaF >= 9.5), bulk density not",
                                "measured: a proxy (WRB 2022 Ch 3.2.3, field identification)."),
                          paste(st$naf, collapse = ", "))
                else NA_character_
  )
}


# Andic properties per layer (v0.9.313) as TRUE / FALSE / NA, on the paths
# andic_properties() reads:
#   (a) Alox + 1/2 Feox >= min_alfe and bulk density <= max_bd (WRB 2022 Ch
#       3.2.3, p 73, criteria 1 and 2);
#   (b) phosphate retention >= min_p_retention, the optional path soilKey has
#       kept since v0.3.4 (Hugo: not a required criterion). It can pass a
#       layer; where it is not measured the layer is decided by (a) and (c);
#   (c) where bulk density is not measured, Alox + 1/2 Feox >= min_alfe with
#       pH NaF >= min_ph_naf in a carbonate-free layer (the field test of Ch
#       3.2.3, which "indicates allophane and/or organo-aluminium complexes in
#       carbonate-free soils"; Hugo, N2). A pH NaF below 9.5 does not rule the
#       layer out (the text: the test fails in layers very rich in organic
#       matter), so it stays undecided.
# A layer fails when (a) fails on a measured value: Alox + 1/2 Feox < 2%, or
# bulk density > 0.9.
.andic_layer_status <- function(h, min_alfe = 2.0, max_bd = 0.9,
                                min_p_retention = 70, min_ph_naf = 9.5) {
  n <- nrow(h)
  col <- function(x) h[[x]] %||% rep(NA_real_, n)
  al <- col("al_ox_pct"); fe <- col("fe_ox_pct")
  bd <- col("bulk_density_g_cm3"); pr <- col("phosphate_retention_pct")
  naf <- col("ph_naf"); caco3 <- col("caco3_pct")
  alfe <- al + fe / 2
  alfe_ok <- ifelse(!is.na(alfe), alfe >= min_alfe,
                    ifelse(!is.na(al) & al >= min_alfe, TRUE, NA))
  a <- alfe_ok & (bd <= max_bd)
  p <- !is.na(pr) & pr >= min_p_retention
  calcareous <- !is.na(caco3) & caco3 > 0
  f <- is.na(bd) & alfe_ok %in% TRUE & !is.na(naf) & naf >= min_ph_naf &
       !calcareous
  status <- ifelse(a %in% TRUE | p | f, TRUE, ifelse(a %in% FALSE, FALSE, NA))
  und <- which(is.na(status))
  missing <- unique(c(
    if (any(is.na(al[und]))) "al_ox_pct",
    if (any(is.na(fe[und]) & !(alfe_ok[und] %in% TRUE))) "fe_ox_pct",
    if (any(is.na(bd[und]))) "bulk_density_g_cm3",
    if (any(is.na(bd[und]) & is.na(naf[und]))) "ph_naf",
    if (any(is.na(pr[und]))) "phosphate_retention_pct"))
  list(status = status,
       alfe_lowbd = which(a %in% TRUE),
       p_retention = which(p),
       naf = which(f & !(a %in% TRUE) & !p),
       missing = missing %||% character(0))
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
#' Tests for stagnic properties (WRB 2022 Ch 3.2.14, p. 81) in a layer
#' starting within the upper \code{max_top_cm} (default 100).
#'
#' Since v0.9.312 the layer reading is that of \code{\link{gleyic_properties}}
#' (Hugo's reading for descriptions that do not say where the mottles sit):
#' a layer with more than 5\% oximorphic features that are not gleyic
#' (criteria 1-3: the matrix has no reductimorphic colour, the layer does
#' not lie directly on a layer with such a matrix, and the site records no
#' groundwater within 150 cm that is not perched, or a claric layer lies
#' directly above it); a layer with claric colours in 95\% or more of
#' its area above an abrupt textural difference or a layer of 1.5 kg/dm3 or
#' more (criterion 4), or above a layer with stagnic oximorphic features
#' (criterion 5). Mottles with neither colour nor water evidence leave the
#' layer \code{NA}. Until v0.9.311 a layer with 5\% redox features passed when
#' the features decayed with depth, which is not in WRB 2022.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_top_cm Maximum top depth (cm) of candidate layers
#'        (default 100).
#' @param min_redox_pct The oximorphic features are more than this \% of the
#'        layer (default 5).
#' @param decay_factor Not used since v0.9.312 (the decay test was removed);
#'        kept so that existing calls still work.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.2.14, Stagnic
#'   properties.
#' @export
stagnic_properties <- function(pedon, max_top_cm = 100,
                                  min_redox_pct = 5, decay_factor = 3) {
  h <- pedon$horizons
  z <- .wrb_redox_layers(pedon, min_redox_pct = min_redox_pct)
  in_window <- !is.na(h$top_cm) & h$top_cm <= max_top_cm
  layers <- which(in_window & z$stagnic %in% TRUE)
  undecided <- which(in_window & is.na(z$stagnic))
  passed <- if (length(layers)) TRUE else if (length(undecided)) NA else FALSE
  DiagnosticResult$new(
    name      = "stagnic_properties",
    passed    = passed,
    layers    = layers,
    evidence  = list(redox_layers = z, undecided_layers = undecided),
    missing   = if (isTRUE(passed)) character(0) else .redox_missing(z, undecided),
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.14, Stagnic properties (p. 81)"
  )
}


#' Retic properties (WRB 2022)
#'
#' WRB 2022 Chapter 3.2.11: claric material interfingering into an argic or
#' natric horizon from its upper limit. soilKey reads criteria 1 and 6 from
#' the data, the finer-textured parts belonging to an argic or natric horizon
#' (\code{\link{argic}}, \code{\link{natric_horizon}}) and the interfingering
#' recorded at its upper limit, and stands in for criteria 2-5, 7 and 8
#' (claric coarser parts, colour and clay contrasts, width, 10-90\% of the
#' sections, not in a plough layer), which no column holds, by what the
#' describer recorded on the uppermost argic or natric layer or the layer
#' directly above it: a designation matching \code{pattern}; an FAO
#' designation of a layer with distinct parts of an E and a B horizon
#' (\code{E/B}, \code{2E/Bt}, \code{B/E}, since v0.9.312); or (since v0.9.312)
#' an E or claric layer whose lower boundary on the argic or natric horizon
#' is irregular or broken (\code{boundary_topography}), the uneven limit that
#' 3.2.1 names as an example of retic properties.
#'
#' \code{FALSE} without an argic or natric horizon, or when designations or
#' boundaries are recorded and none shows the interfingering; \code{NA} when
#' the argic or natric horizon cannot be established or there are neither
#' designations nor boundary topographies. Until v0.9.219 the designation
#' alone decided, anywhere in the profile.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param pattern Regex (default \code{"glossic|retic|albeluvic"}).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 3.2.11, Retic
#'   properties.
#' @export
retic_properties <- function(pedon, pattern = "glossic|retic|albeluvic") {
  h <- pedon$horizons
  ref <- "IUSS Working Group WRB (2022), Chapter 3.2.11, Retic properties (p. 79)"
  arg <- argic(pedon)
  nat <- natric_horizon(pedon)
  hz  <- sort(union(if (isTRUE(arg$passed)) arg$layers,
                    if (isTRUE(nat$passed)) nat$layers))
  desg <- as.character(h$designation %||% rep(NA_character_, nrow(h)))
  topo <- as.character(h$boundary_topography %||% rep(NA_character_, nrow(h)))
  # v0.9.312: E/B and B/E (FAO), a layer with distinct parts of an E and a B
  # horizon, the claric material in the finer-textured argic or natric parts
  # (3.2.11 criteria 1-2).
  inter <- !is.na(desg) &
    grepl("^[0-9]*E[a-z0-9]*/[0-9]*B|^[0-9]*B[a-z0-9]*/[0-9]*E", desg)
  flag <- (!is.na(desg) & grepl(pattern, desg, ignore.case = TRUE)) | inter
  recorded <- !all(is.na(desg)) || !all(is.na(topo))
  if (!length(hz)) {
    passed <- if (is.na(arg$passed) || is.na(nat$passed)) {
      if (any(flag) || !recorded) NA else FALSE
    } else FALSE
    return(DiagnosticResult$new(
      name = "retic_properties", passed = passed, layers = integer(0),
      evidence = list(argic = arg, natric = nat, designation_flags = which(flag)),
      missing = if (is.na(passed)) unique(c(arg$missing, nat$missing)) else character(0),
      reference = ref))
  }
  top <- hz[which.min(h$top_cm[hz])]
  above <- which(!is.na(h$bottom_cm) & abs(h$bottom_cm - h$top_cm[top]) < 1e-6)
  at_boundary <- intersect(which(flag), c(top, above))
  # v0.9.312: the irregular or broken lower boundary of an E or claric layer
  # on the argic or natric horizon: coarser material reaching into it from
  # its upper limit (3.2.11 criterion 6; 3.2.1, p. 72: retic properties are
  # an example of an uneven limit).
  clr <- .subtest_by_layer(test_claric_munsell(h), nrow(h))
  tongue <- above[!is.na(topo[above]) &
                    grepl("irregular|broken", topo[above], ignore.case = TRUE) &
                    ((!is.na(desg[above]) & grepl("^[0-9]*E", desg[above])) |
                       clr[above] %in% TRUE)]
  hit <- sort(union(at_boundary, tongue))
  passed <- if (length(hit)) TRUE else if (!recorded) NA else FALSE
  DiagnosticResult$new(
    name      = "retic_properties",
    passed    = passed,
    layers    = if (isTRUE(passed)) sort(union(hit, top)) else integer(0),
    evidence  = list(argic_or_natric = hz, upper_limit_layer = top,
                     designation_flags = which(flag), irregular_boundary = tongue,
                     argic = arg, natric = nat),
    missing   = if (is.na(passed)) c("designation", "boundary_topography") else character(0),
    reference = ref
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
#' Either path qualifies. Diagnostic of Cryosols. When neither passes and
#' the site has a soil temperature regime warmer than gelic (cryic, frigid,
#' mesic, thermic, hyperthermic or an iso form;
#' \code{site$soil_temperature_regime}), the answer is no: those regimes
#' have no permafrost (KST 13, Ch. 3, p. 50).
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
  # No permafrost under a soil temperature regime warmer than gelic (see
  # .str_rules_out_permafrost); a measured temperature or a frozen
  # designation that passes is kept.
  evidence <- paths
  if (!isTRUE(agg$passed) && .str_rules_out_permafrost(pedon$site)) {
    agg$passed  <- FALSE
    agg$missing <- character(0)
    evidence$temperature_regime <- list(
      regime = as.character(pedon$site$soil_temperature_regime)[1L],
      rules_out_permafrost = TRUE)
  }

  DiagnosticResult$new(
    name      = "cryic_conditions",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = evidence,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Cryosols"
  )
}


#' Cryosols (WRB 2022 Ch 4, p 98)
#'
#' (1) A cryic horizon starting <= 100 cm, or (2) a cryic horizon starting
#' <= 200 cm and evidence of cryogenic alteration in some layer within
#' 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
cryosol <- function(pedon) {
  # v0.9.313: WRB 2022 Ch 4, Cryosols (p 98). Item 1 is cryic_conditions()
  # (cryic horizon starting <= 100 cm), the old gate. Item 2: "a cryic
  # horizon starting <= 200 cm from the soil surface and evidence of
  # cryogenic alteration (cryoturbation, frost heave, cryogenic sorting,
  # thermal cracking, ice segregation, patterned ground, etc.) in some layer
  # within 100 cm". Cryogenic alteration is read as the Gelic qualifier reads
  # it (cryoturbation_usda(): jj or @ designations, broken or involuted
  # boundaries), so that Gelic and this item stay complementary. Both items
  # keep the v0.9.307 rule: no permafrost under a temperature regime warmer
  # than gelic.
  h <- pedon$horizons
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Cryosols (p. 98)"
  c100 <- cryic_conditions(pedon)
  if (isTRUE(c100$passed))
    return(DiagnosticResult$new(
      name = "cryosol", passed = TRUE, layers = c100$layers,
      evidence = list(item1 = c100), missing = character(0), reference = ref))
  c200 <- cryic_conditions(pedon, max_top_cm = 200)
  top <- h$top_cm
  # Where no soil temperature is recorded, horizons described without the
  # frozen suffix, in a profile that uses it deeper down, count as not frozen
  # (the reading of qual_gelic()).
  frozen_des <- c200$evidence$designation$frozen_designation$layers %||% integer(0)
  settle <- function(x, depth) {
    if (!is.na(x)) return(x)
    up <- which(!is.na(top) & top <= depth)
    if (length(frozen_des) && all(!is.na(h$designation[up]))) FALSE else NA
  }
  item1 <- settle(c100$passed, 100)
  cryic200 <- settle(c200$passed, 200)
  ct <- cryoturbation_usda(pedon)
  alt <- ct$layers[!is.na(top[ct$layers]) & top[ct$layers] < 100]
  up <- which(!is.na(top) & top < 100)
  seen <- any(!is.na(h$designation[up]) | !is.na(h$boundary_topography[up]))
  altered <- if (length(alt)) TRUE else if (seen) FALSE else NA
  item2 <- if (isFALSE(cryic200) || isFALSE(altered)) FALSE
           else if (isTRUE(cryic200) && isTRUE(altered)) TRUE
           else NA
  passed <- if (isTRUE(item2)) TRUE
            else if (isFALSE(item1) && isFALSE(item2)) FALSE
            else NA
  missing <- character(0)
  if (is.na(passed)) {
    missing <- c(if (is.na(item1)) c100$missing, if (is.na(cryic200)) c200$missing)
    if (is.na(altered)) missing <- c(missing, "designation", "boundary_topography")
  }
  DiagnosticResult$new(
    name      = "cryosol",
    passed    = passed,
    layers    = if (isTRUE(passed)) sort(union(c200$layers, alt)) else integer(0),
    evidence  = list(item1 = item1, cryic_100cm = c100,
                     item2 = list(passed = item2, cryic_200cm = c200,
                                  cryogenic_alteration_layers = alt)),
    missing   = unique(missing),
    reference = ref
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
#'   \item \code{test_clay_above} -- clay >= 30\%
#'   \item \code{test_slickensides_present} -- slickensides at
#'         or above the "common" level
#'   \item \code{test_minimum_thickness} -- combined vertic layer
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
