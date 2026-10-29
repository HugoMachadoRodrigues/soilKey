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
#' A horizon of organic material (WRB 2022 Ch 3.1.15): organic material
#' (\code{oc_pct >= min_oc}, default 20, Ch 3.3.13) that is at least
#' \code{min_thickness} cm thick (default 10). The thickness is that of the
#' adjoining organic layers that run from the surface, taken together. A
#' layer whose organic carbon is not recorded may be organic material and
#' leaves the answer undecided (\code{NA}) where it could change it. The
#' water-saturation criterion (saturated for 30 or more consecutive days, or
#' drained) is not read.
#'
#' Since v0.9.313 this is the horizon only: the Histosols are keyed by
#' \code{histosol()}, which reads the Chapter 4 key as worded. Until then
#' this function was also the Histosols gate, with organic material read at
#' 12\% organic carbon and a second path of 40 cm of organic material within
#' the upper 80 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_thickness Minimum thickness (cm), default 10.
#' @param min_oc Minimum soil organic carbon (\%) of organic material,
#'        default 20 (WRB 2022 Ch 3.3.13).
#' @param surface_top_cm Maximum top depth (cm) of the uppermost layer of
#'        the horizon (default 0, the soil surface).
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @references IUSS Working Group WRB (2022), Chapter 3.1.15, Histic
#'   horizon, and Chapter 3.3.13, Organic material.
#' @export
histic_horizon <- function(pedon,
                           min_thickness  = 10,
                           min_oc         = 20,
                           surface_top_cm = 0) {
  # v0.9.313: WRB 2022 Ch 3.3.13 (p 91): organic material "has >= 20% soil
  # organic carbon" (the default was 12). Ch 3.1.15 (p 48): a histic horizon
  # consists of organic material and has a thickness of >= 10 cm; the
  # thickness of a horizon is that of its subhorizons together (Ch 2.1,
  # rule 7, p 23), so the adjoining organic layers from the surface are
  # summed (the old code read one layer). The 40-cm-within-80-cm path left
  # with the Histosols key (histosol()). Saturation (criterion 1) is not read.
  h <- pedon$horizons
  oc <- h$oc_pct
  status <- ifelse(is.na(oc), NA, oc >= min_oc)
  ok <- !is.na(h$top_cm) & !is.na(h$bottom_cm)
  ord <- order(h$top_cm)
  ord <- ord[ok[ord]]
  # The run of adjoining layers from the surface whose status is in `keep`.
  surface_run <- function(keep) {
    ly <- integer(0); bot <- NA_real_
    for (i in ord) {
      if (!keep[i]) break
      if (!length(ly)) {
        if (h$top_cm[i] > surface_top_cm) break
      } else if (h$top_cm[i] > bot + 1e-6) break
      ly <- c(ly, i); bot <- max(bot, h$bottom_cm[i], na.rm = TRUE)
    }
    list(layers = ly,
         thickness = if (length(ly)) bot - min(h$top_cm[ly]) else 0)
  }
  yes <- surface_run(status %in% TRUE)        # organic material
  may <- surface_run(!(status %in% FALSE))    # organic or not recorded
  passed <- if (yes$thickness >= min_thickness) TRUE
            else if (may$thickness < min_thickness) FALSE
            else NA

  DiagnosticResult$new(
    name      = "histic_horizon",
    passed    = passed,
    layers    = if (isTRUE(passed)) yes$layers else integer(0),
    evidence  = list(organic_layers = which(status %in% TRUE),
                     surface_run = yes$layers,
                     thickness_cm = yes$thickness,
                     possible_thickness_cm = may$thickness,
                     min_oc = min_oc, min_thickness = min_thickness),
    missing   = if (is.na(passed)) "oc_pct" else character(0),
    reference = "IUSS Working Group WRB (2022), Chapter 3.1.15, Histic horizon"
  )
}


#' Histosols (WRB 2022 Ch 4, p 95)
#'
#' The Histosols key as worded: (1) organic material starting <= 40 cm with
#' a combined thickness within 100 cm of >= 40 cm (if < 75\% moss fibres;
#' >= 60 cm otherwise, not read: soilKey has no moss-fibre attribute);
#' (2) organic material from the surface, >= 10 cm thick,
#' directly over ice, continuous rock or technic hard material; or (3) a
#' layer of coarse fragments with organic material in its interstices (the
#' Mawic case).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
histosol <- function(pedon) {
  # v0.9.313: WRB 2022 Ch 4, Histosols (p 95), in place of histic_horizon()
  # as the gate (it passed any surface layer >= 10 cm with >= 12% OC, or
  # 40 cm of it within 80 cm). Organic material per layer is
  # .q_organic_layers(): >= 20% SOC where measured (Ch 3.3.13, p 91), an O or
  # H horizon where not.
  h <- pedon$horizons
  ref <- "IUSS Working Group WRB (2022), Chapter 4, Histosols (p. 95)"
  top <- h$top_cm; bot <- h$bottom_cm
  ok <- !is.na(top) & !is.na(bot)
  if (!any(ok))
    return(DiagnosticResult$new(name = "histosol", passed = NA,
      layers = integer(0), evidence = list(),
      missing = c("top_cm", "bottom_cm"), reference = ref))
  surface <- min(top[ok])
  # Continuous rock, technic hard material and ice are not organic material.
  rock <- continuous_rock(pedon)
  thm <- .q_technic_hard(pedon)
  ice <- vapply(seq_len(nrow(h)), function(i) .q_ice_layer(h, i), NA)
  hard <- vapply(seq_len(nrow(h)), function(i) .q_hard_layer(pedon, i, rock, thm), NA)
  org <- .q_organic_layers(pedon)
  org[ice %in% TRUE | hard %in% TRUE] <- FALSE

  # Item 1: "organic material starting <= 40 cm from the soil surface and
  # having within 100 cm of the soil surface a combined thickness of a.
  # >= 40 cm if < 75% ... consists of moss fibres; or b. >= 60 cm". All the
  # organic material within 100 cm is added up, clipped at 100 cm. soilKey
  # records no moss-fibre volume, so item 1a (40 cm) is read: the 60 cm of
  # item 1b binds only moss peat with >= 75% moss fibres (Bryic), which no
  # attribute can show (a reading left to Hugo).
  w <- ifelse(ok, pmax(0, pmin(bot, surface + 100) - pmax(top, surface)), 0)
  yes <- ok & org %in% TRUE
  may <- ok & !(org %in% FALSE)
  start_yes <- any(yes & top - surface <= 40)
  start_may <- any(may & top - surface <= 40)
  thk_yes <- sum(w[yes]); thk_may <- sum(w[may])
  item1 <- if (start_yes && thk_yes >= 40) TRUE
           else if (!start_may || thk_may < 40) FALSE
           else NA

  # Items 2 and 3 read the organic material from the surface: the adjoining
  # layers from the surface that are (or, not recorded, may be) organic, and
  # every depth at which that column may end (the layer below it then is not
  # organic material).
  ord <- order(top); ord <- ord[ok[ord]]
  run <- integer(0)
  for (i in ord) {
    if (org[i] %in% FALSE) break
    if (!length(run) && top[i] > surface + 1e-6) break
    if (length(run) && top[i] > bot[run[length(run)]] + 1e-6) break
    run <- c(run, i)
  }
  n_yes <- sum(cumprod(org[run] %in% TRUE))   # leading layers known organic
  ends <- if (length(run)) seq(max(1L, n_yes), length(run)) else integer(0)
  end_thick <- vapply(ends, function(k) bot[run[k]] - surface, 0)
  end_below <- vapply(ends, function(k) .q_layer_at(h, bot[run[k]]), 0L)
  end_over <- vapply(end_below, function(b) {
    if (is.na(b)) return(NA)
    if (isTRUE(ice[b]) || isTRUE(hard[b])) TRUE
    else if (isFALSE(ice[b]) && isFALSE(hard[b])) FALSE else NA
  }, NA)
  known <- ends == n_yes & n_yes >= 1L        # the column of known organic layers

  # Item 2: "organic material starting at the soil surface, having a
  # thickness of >= 10 cm and directly overlying ice, continuous rock or
  # technic hard material". Ice, rock and technic hard material are read as
  # the Muusic and Rockic qualifiers read them.
  item2 <- if (any(known & end_thick >= 10 & end_over %in% TRUE)) TRUE
           else if (any(end_thick >= 10 & !(end_over %in% FALSE))) NA
           else FALSE

  # Item 3: "a layer of coarse fragments that, together with overlying
  # organic material, if present, starts at the soil surface and has a
  # thickness of a. >= 10 cm if overlying continuous rock or technic hard
  # material; or b. >= 40 cm; and the major part of the interstices between
  # the coarse fragments is filled with organic material". As the Mawic
  # qualifier: soilKey does not record the filling of the interstices, so
  # the item is FALSE where the data rule it out (no column of the
  # thickness, < 50% coarse fragments throughout it) and NA otherwise.
  cf <- h$coarse_fragments_pct[run]
  stony <- any(is.na(cf) | cf >= 50)
  end_hard <- vapply(end_below, function(b) if (is.na(b)) NA else hard[b], NA)
  thick3 <- any(end_thick >= 40 | (end_thick >= 10 & !(end_hard %in% FALSE)))
  item3 <- if (length(run) && stony && thick3) NA else FALSE

  items <- c(item1 = item1, item2 = item2, item3 = item3)
  passed <- if (any(items %in% TRUE)) TRUE
            else if (anyNA(items)) NA else FALSE
  layers <- if (isTRUE(item1)) which(yes)
            else if (isTRUE(item2)) run[seq_len(n_yes)] else integer(0)
  missing <- character(0)
  if (is.na(passed)) {
    if (any(ok & is.na(org))) missing <- c(missing, "oc_pct")
    if (is.na(item2)) missing <- c(missing, "designation", "ice_pct")
    if (is.na(item3))
      missing <- c(missing, "interstices between coarse fragments filled with organic material (% of their volume; not in the schema)")
  }
  DiagnosticResult$new(
    name      = "histosol",
    passed    = passed,
    layers    = layers,
    evidence  = list(items = as.list(items),
                     organic_thickness_100cm = thk_yes,
                     possible_organic_thickness_100cm = thk_may,
                     organic_from_surface = run[seq_len(n_yes)],
                     possible_organic_from_surface = run),
    missing   = unique(missing),
    reference = ref
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
#' @param engine One of \code{"soilkey"} (default; strict WRB sand
#'        threshold via \code{test_coarse_texture_throughout})
#'        or \code{"aqp"} (LUCAS-friendly fallback: passes when sand
#'        >= 70\\% across the upper \code{max_top_cm}). \code{NULL}
#'        reads \code{getOption("soilKey.diagnostic_engine")}.
#' @return A \code{\link{DiagnosticResult}}.
#'
#' @details
#' Sub-test: \code{test_coarse_texture_throughout}.
#'
#' v0.3 limitations: WRB 2022 Arenosol also requires that no other
#' diagnostic horizon (argic, ferralic, etc.) is present, but those
#' exclusions happen at the key level via canonical RSG order.
#'
#' @references IUSS Working Group WRB (2022), Chapter 5, Arenosols.
#' @export
arenic_texture <- function(pedon, max_top_cm = 100, engine = NULL) {
  h <- pedon$horizons

  if (is.null(engine))
    engine <- getOption("soilKey.diagnostic_engine", "soilkey")
  engine <- match.arg(engine, c("soilkey", "aqp"))

  tests <- list()
  tests$coarse_throughout <- test_coarse_texture_throughout(
    h, max_top_cm = max_top_cm)

  # v0.9.65 engine="aqp" relaxation: also accept "sand >= 70% in
  # upper 100 cm" as an Arenosol marker, even if silt+2*clay >= 30.
  # This catches LUCAS Arenosols whose topsoil is just above the
  # strict silt+2*clay < 30 cut-off but is unmistakably sandy.
  if (engine == "aqp") {
    sand <- h$sand_pct
    upper <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
    sand_70 <- !is.na(sand[upper]) & sand[upper] >= 70
    tests$sandy_relaxed_aqp <- list(
      passed = any(sand_70, na.rm = TRUE),
      layers = upper[which(sand_70)],
      details = list(threshold_sand_pct = 70)
    )
  }

  agg <- aggregate_alternatives(list(tests))
  # aggregate_alternatives expects a list of lists; passes if any
  # sub-test passes. Restructure: each tests$* is a single subtest.
  passed <- any(vapply(tests, function(t) isTRUE(t$passed), logical(1L)))
  layers <- unique(unlist(lapply(tests, function(t) t$layers)))

  DiagnosticResult$new(
    name      = "arenic_texture",
    passed    = passed,
    layers    = layers,
    evidence  = c(tests, list(engine = engine)),
    missing   = unique(unlist(lapply(tests,
                                          function(t) t$missing %||% character(0)))),
    reference = paste("IUSS Working Group WRB (2022), Chapter 5,",
                        "Arenosols",
                        if (engine == "aqp") "[engine=aqp relaxed]" else "")
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
#' Sub-test: \code{test_fluvic_stratification}.
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
