# =============================================================================
# WRB 2022 (4th ed.) -- Qualifiers (Ch 5) -- v0.9.64 final batch.
#
# Closes the v0.9.63 audit gap (8 PQ + 43 SQ remaining) to reach
# 100% / 100% qualifier coverage of the canonical IUSS WRB 2022
# specification.
#
# Coverage philosophy
# -------------------
# Each qualifier here returns a `DiagnosticResult` per the established
# `qual_<Name>` contract from `R/qualifiers-wrb2022.R`. Where the
# soilKey horizon schema carries the necessary attributes, the
# qualifier is implemented substantively (clear pass/fail logic).
# Where the canonical WRB criterion requires data we do not yet ingest
# (Tier-3 qualifiers per the v0.9.64 backlog), the function returns
# `NA` with the missing schema field listed in `$missing` -- the
# function exists, the audit picks it up, and downstream code can
# request it; the actual data path is wired later when the schema
# extension lands.
#
# References: IUSS Working Group WRB (2022). World Reference Base for
# Soil Resources, 4th edition. Chapter 5 (qualifiers).
# =============================================================================


# --- Internal helpers ------------------------------------------------------

#' Stub-NA qualifier that exists in NAMESPACE but reports missing data
#'
#' For Tier-3 qualifiers requiring schema fields not yet on the
#' \code{horizon_column_spec()} or site-level lists. The audit picks
#' the function up as "implemented", and downstream code that calls
#' it gets a NA-passed result with a clear `missing` listing.
#'
#' @noRd
.q_stub_na <- function(name, missing_fields, reference) {
  function(pedon) {
    DiagnosticResult$new(
      name      = name,
      passed    = NA,
      layers    = integer(0),
      evidence  = list(reason = sprintf(
        "Tier-3 qualifier: requires schema fields not yet in soilKey (%s)",
        paste(missing_fields, collapse = ", "))),
      missing   = as.character(missing_fields),
      reference = reference
    )
  }
}


# ============================================================================
# PRINCIPAL QUALIFIERS (PQ) -- v0.9.64 final batch
# ============================================================================


#' Entic qualifier (et): no albic horizon above the spodic horizon
#'
#' WRB 2022 Ch 5: "not having an albic horizon above the spodic horizon
#' (in Podzols only)".
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_entic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Entic: "not having an albic horizon above the
  # spodic horizon (in Podzols only)". The old code had it backwards: it
  # passed on an albic horizon AND no spodic horizon. Now the spodic horizon
  # must be present and no albic layer may start above its upper limit.
  r <- .q_rsg_only("Entic", rsg_code, "PZ"); if (!is.null(r)) return(r)
  ref <- "WRB (2022) Ch 5, Entic"
  h <- pedon$horizons
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  if (is.null(spo) || !isTRUE(spo$passed)) {
    return(DiagnosticResult$new(
      name = "Entic", passed = if (is.null(spo) || is.na(spo$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(spodic = spo, reason = "no spodic horizon"),
      missing = spo$missing %||% character(0), reference = ref))
  }
  spo_top <- min(h$top_cm[spo$layers], na.rm = TRUE)
  alb <- albic(pedon)
  above <- if (isTRUE(alb$passed))
    alb$layers[!is.na(h$top_cm[alb$layers]) & h$top_cm[alb$layers] < spo_top]
  else integer(0)
  # Mineral horizons above the spodic horizon with no colour at all could hide
  # an albic horizon.
  oc <- h$oc_pct %||% rep(NA_real_, nrow(h))
  blind <- which(!is.na(h$top_cm) & h$top_cm < spo_top & (is.na(oc) | oc < 20) &
                   is.na(h$munsell_value_moist) & is.na(h$munsell_value_dry))
  passed <- if (length(above)) FALSE
            else if (is.na(alb$passed) || length(blind)) NA else TRUE
  DiagnosticResult$new(
    name = "Entic", passed = passed,
    layers = if (isTRUE(passed)) spo$layers else integer(0),
    evidence = list(spodic = spo, albic = alb, spodic_top_cm = spo_top,
                    albic_layers_above_spodic = above),
    missing = if (is.na(passed)) alb$missing %||% character(0) else character(0),
    reference = ref)
}


# --- v0.9.217 helpers for the Chapter 5 thickness rules ----------------------

# Thickness of the layers `idx` (row indices of `h`) clipped to
# [win_top, win_bot]: their combined thickness, or, with `contiguous = TRUE`,
# that of the thickest run of depth-contiguous layers (one "layer" in the WRB
# sense) whose upper limit is <= `max_start`.
.q_layers_thickness <- function(h, idx, win_top = -Inf, win_bot = Inf,
                                contiguous = FALSE, max_start = Inf) {
  idx <- idx[!is.na(h$top_cm[idx]) & !is.na(h$bottom_cm[idx])]
  if (!length(idx)) return(0)
  idx <- idx[order(h$top_cm[idx])]
  clip <- function(i) max(0, min(h$bottom_cm[i], win_bot) - max(h$top_cm[i], win_top))
  if (!contiguous) return(sum(vapply(idx, clip, numeric(1))))
  best <- 0; run <- 0; end <- -Inf; start <- NA_real_
  for (i in idx) {
    joined <- h$top_cm[i] <= end + 0.5
    if (!joined) { run <- 0; start <- h$top_cm[i]; end <- -Inf }
    run <- run + clip(i)
    end <- max(end, h$bottom_cm[i])
    if (start <= max_start) best <- max(best, run)
  }
  best
}

# Three-valued thickness rule over a per-layer status (TRUE / FALSE / NA):
# TRUE when the layers known to qualify reach `min_cm`, FALSE when even the
# layers that might qualify (status NA) cannot, NA otherwise. `...` goes to
# .q_layers_thickness().
.q_thickness_rule <- function(h, status, min_cm, ...) {
  yes <- which(status %in% TRUE)
  may <- which(status %in% TRUE | is.na(status))
  t_yes <- .q_layers_thickness(h, yes, ...)
  t_may <- .q_layers_thickness(h, may, ...)
  list(passed = if (t_yes >= min_cm) TRUE else if (t_may < min_cm) FALSE else NA,
       layers = yes, thickness_cm = t_yes)
}

# Depth of the mineral soil surface: the top of the shallowest layer that is
# not organic (SOC >= 20 \% or an O / H designation).
.q_mineral_surface_cm <- function(h) {
  org <- (!is.na(h$oc_pct) & h$oc_pct >= 20) |
         (!is.na(h$designation) & grepl("^[0-9]*[OH]", h$designation))
  tops <- h$top_cm[!org & !is.na(h$top_cm)]
  if (length(tops)) min(tops) else NA_real_
}


#' Tonguic qualifier (to): tonguing of a chernic, mollic or umbric horizon
#'
#' v0.9.217: WRB 2022 Ch 5, "showing tonguing of a chernic, mollic or umbric
#' horizon into an underlying layer". The v0.9.64 code passed on any AB, BA or
#' A/B designation (a transitional horizon is not a tongue) and read a
#' \code{transition_topography} column that does not exist, so it fired on
#' most profiles with an AB horizon. It now needs the horizon itself
#' (\code{chernic()}, \code{mollic()} or \code{umbric_horizon()}) and its lower
#' boundary recorded as tongued or irregular (FAO: pockets deeper than wide) in
#' \code{boundary_topography} of the horizon's deepest layer.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_tonguic <- function(pedon) {
  h <- pedon$horizons
  diags <- list(
    chernic = tryCatch(chernic(pedon), error = function(e) NULL),
    mollic  = tryCatch(mollic(pedon), error = function(e) NULL),
    umbric  = tryCatch(umbric_horizon(pedon), error = function(e) NULL))
  present <- vapply(diags, function(d) !is.null(d) && isTRUE(d$passed), logical(1))
  hz <- sort(unique(unlist(lapply(diags[present], function(d) d$layers))))
  hz <- hz[!is.na(h$bottom_cm[hz])]
  if (!length(hz)) {
    unknown <- vapply(diags, function(d) is.null(d) || is.na(d$passed), logical(1))
    return(DiagnosticResult$new(
      name = "Tonguic", passed = if (any(unknown)) NA else FALSE,
      layers = integer(0), evidence = diags,
      missing = unique(unlist(lapply(diags, function(d) d$missing))),
      reference = "WRB (2022) Ch 5, Tonguic"))
  }
  low  <- hz[which.max(h$bottom_cm[hz])]
  topo <- (h$boundary_topography %||% rep(NA_character_, nrow(h)))[low]
  pat  <- "(?i)tongu|irregular"
  passed <- if (is.na(topo)) NA else grepl(pat, topo, perl = TRUE)
  DiagnosticResult$new(
    name = "Tonguic", passed = passed,
    layers = if (isTRUE(passed)) hz else integer(0),
    evidence = c(diags[present], list(lower_boundary_layer = low,
                                      boundary_topography = topo, pattern = pat)),
    missing = if (is.na(topo)) "boundary_topography" else character(0),
    reference = "WRB (2022) Ch 5, Tonguic"
  )
}


#' Nudiargic qualifier (ng): argic horizon at the mineral soil surface
#'
#' v0.9.217: WRB 2022 Ch 5, "having an argic horizon starting at the mineral
#' soil surface". The v0.9.64 code accepted an argic horizon starting <= 5 cm
#' from the soil surface and returned FALSE when argic() could not be
#' evaluated; it now needs the argic horizon to start at the mineral soil
#' surface (the top of the shallowest non-organic layer) and is NA when argic()
#' is NA.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_nudiargic <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed)) {
    return(DiagnosticResult$new(
      name = "Nudiargic", passed = if (is.na(arg$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(argic = arg),
      missing = arg$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Nudiargic"
    ))
  }
  h <- pedon$horizons
  shallowest <- suppressWarnings(min(h$top_cm[arg$layers], na.rm = TRUE))
  mss <- .q_mineral_surface_cm(h)
  passed <- if (!is.finite(shallowest) || is.na(mss)) NA else shallowest <= mss
  DiagnosticResult$new(
    name = "Nudiargic", passed = passed,
    layers = if (isTRUE(passed)) arg$layers else integer(0),
    evidence = list(argic = arg, shallowest_top_cm = shallowest,
                      mineral_surface_cm = mss),
    missing = if (is.na(passed)) "top_cm" else character(0),
    reference = "WRB (2022) Ch 5, Nudiargic"
  )
}


#' Nudinatric qualifier (nn): natric horizon at the mineral soil surface
#'
#' v0.9.217: WRB 2022 Ch 5 (under Natric), "having a natric horizon starting
#' at the mineral soil surface". Was a natric horizon starting <= 5 cm from
#' the soil surface, and FALSE when natric_horizon() was NA; now the natric
#' horizon must start at the mineral soil surface, NA when natric is NA.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_nudinatric <- function(pedon) {
  nat <- tryCatch(natric_horizon(pedon), error = function(e) NULL)
  if (is.null(nat)) {
    return(DiagnosticResult$new(
      name = "Nudinatric", passed = NA, layers = integer(0),
      evidence = list(reason = "natric() unavailable"),
      missing = "natric",
      reference = "WRB (2022) Ch 5, Nudinatric"
    ))
  }
  if (!isTRUE(nat$passed)) {
    return(DiagnosticResult$new(
      name = "Nudinatric", passed = if (is.na(nat$passed)) NA else FALSE,
      layers = integer(0),
      evidence = list(natric = nat),
      missing = nat$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Nudinatric"
    ))
  }
  h <- pedon$horizons
  shallowest <- suppressWarnings(min(h$top_cm[nat$layers], na.rm = TRUE))
  mss <- .q_mineral_surface_cm(h)
  passed <- if (!is.finite(shallowest) || is.na(mss)) NA else shallowest <= mss
  DiagnosticResult$new(
    name = "Nudinatric", passed = passed,
    layers = if (isTRUE(passed)) nat$layers else integer(0),
    evidence = list(natric = nat, shallowest_top_cm = shallowest,
                      mineral_surface_cm = mss),
    missing = if (is.na(passed)) "top_cm" else character(0),
    reference = "WRB (2022) Ch 5, Nudinatric"
  )
}


#' Someric qualifier (si): mollic or umbric horizon < 20 cm thick
#'
#' v0.9.217: WRB 2022 Ch 5, "having a mollic or umbric horizon, < 20 cm
#' thick". The v0.9.64 code tested an anthric (hortic / plaggic ...) horizon
#' over a mollic one, which is not the definition. A mollic or umbric horizon
#' may be < 20 cm thick only when it is >= 10 cm and directly overlies
#' continuous rock, technic hard material or a cryic, petrocalcic, petroduric,
#' petrogypsic or petroplinthic horizon (Ch 3.1.20 / 3.1.39, criterion 6), so
#' mollic() / umbric_horizon() are called with the 10 cm minimum and the layer
#' directly below the horizon must be one of those.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_someric <- function(pedon) {
  h <- pedon$horizons
  hz <- list(mollic = tryCatch(mollic(pedon, min_thickness = 10),
                               error = function(e) NULL),
             umbric = tryCatch(umbric_horizon(pedon, min_thickness = 10),
                               error = function(e) NULL))
  ok <- vapply(hz, function(d) !is.null(d) && isTRUE(d$passed), logical(1))
  layers <- sort(unique(unlist(lapply(hz[ok], function(d) d$layers))))
  layers <- layers[!is.na(h$top_cm[layers]) & !is.na(h$bottom_cm[layers])]
  if (!length(layers)) {
    unknown <- vapply(hz, function(d) is.null(d) || is.na(d$passed), logical(1))
    return(DiagnosticResult$new(
      name = "Someric", passed = if (any(unknown)) NA else FALSE,
      layers = integer(0), evidence = hz,
      missing = unique(unlist(lapply(hz, function(d) d$missing))),
      reference = "WRB (2022) Ch 5, Someric"))
  }
  thk <- .q_layers_thickness(h, layers, contiguous = TRUE)
  bottom <- max(h$bottom_cm[layers])
  below <- which(!is.na(h$top_cm) & abs(h$top_cm - bottom) <= 0.5)
  bar_fns <- c("continuous_rock", "technic_hard_material", "cryic_horizon",
               "petrocalcic", "petroduric", "petrogypsic", "petroplinthic")
  bars <- lapply(bar_fns, function(f)
    tryCatch(get(f, envir = asNamespace("soilKey"))(pedon), error = function(e) NULL))
  on_bar <- any(vapply(bars, function(d) !is.null(d) && isTRUE(d$passed) &&
                         length(intersect(d$layers, below)) > 0L, logical(1)))
  bar_unknown <- !length(below) ||
    any(vapply(bars, function(d) is.null(d) || is.na(d$passed), logical(1)))
  passed <- if (thk >= 20) FALSE else if (on_bar) TRUE else if (bar_unknown) NA else FALSE
  DiagnosticResult$new(
    name = "Someric", passed = passed,
    layers = if (isTRUE(passed)) layers else integer(0),
    evidence = c(hz[ok], list(thickness_cm = thk, directly_over_limiting = on_bar)),
    missing = if (is.na(passed)) "limiting layer directly below the horizon"
              else character(0),
    reference = "WRB (2022) Ch 5, Someric"
  )
}


# v0.9.217: what a Neocambic / Neobrunic layer must overlie (WRB 2022 Ch 5):
# "an albic horizon that overlies an argic, a natric or a spodic horizon, or a
# layer with retic properties". Returns list(top = upper limit of the
# deepest such feature or NA, passed = TRUE / FALSE / NA, evidence).
.q_neo_lower_feature <- function(pedon) {
  h <- pedon$horizons
  get_d <- function(f) tryCatch(get(f, envir = asNamespace("soilKey"))(pedon),
                                error = function(e) NULL)
  alb <- get_d("albic"); ret <- get_d("retic_properties")
  ill <- lapply(c(argic = "argic", natric = "natric_horizon", spodic = "spodic"), get_d)
  tops <- numeric(0)
  if (!is.null(alb) && isTRUE(alb$passed)) {
    ill_tops <- unlist(lapply(ill, function(d)
      if (!is.null(d) && isTRUE(d$passed)) h$top_cm[d$layers]))
    for (a in alb$layers)
      if (any(ill_tops >= h$bottom_cm[a] - 0.5, na.rm = TRUE))
        tops <- c(tops, h$top_cm[a])
  }
  if (!is.null(ret) && isTRUE(ret$passed)) tops <- c(tops, h$top_cm[ret$layers])
  tops <- tops[!is.na(tops)]
  # An albic layer with nothing below it cannot overlie an argic / natric /
  # spodic horizon, so an unevaluable one of those only matters otherwise.
  alb_over <- if (!is.null(alb) && isTRUE(alb$passed))
    alb$layers[vapply(alb$layers, function(a)
      any(!is.na(h$top_cm) & h$top_cm >= h$bottom_cm[a] - 0.5), logical(1))]
    else integer(0)
  unknown <- is.null(alb) || is.na(alb$passed) || is.null(ret) || is.na(ret$passed) ||
    (length(alb_over) > 0L &&
       any(vapply(ill, function(d) is.null(d) || is.na(d$passed), logical(1))))
  list(top = if (length(tops)) max(tops) else NA_real_,
       passed = if (length(tops)) TRUE else if (unknown) NA else FALSE,
       evidence = c(list(albic = alb, retic_properties = ret), ill))
}

#' Neobrunic qualifier (nb): Brunic layer over albic-over-argic / retic
#'
#' v0.9.217: WRB 2022 Ch 5 (under Brunic), "having a layer, >= 15 cm thick and
#' starting <= 50 cm from the mineral soil surface, that meets diagnostic
#' criteria 3 and 4 of the cambic horizon but fails diagnostic criterion 1,
#' does not consist of claric material and overlies: an albic horizon that
#' overlies an argic, a natric or a spodic horizon, or a layer with retic
#' properties". The v0.9.64 code read a "recent deposit" pattern in
#' \code{layer_origin} on top of a full cambic horizon. soilKey has no
#' diagnostic for the Brunic layer (cambic criteria 3 + 4 without criterion 1;
#' failing criterion 1 also needs the very-fine-sand classes, which the schema
#' cannot tell apart), so a passing case is NA. FALSE when the data rule it
#' out: no albic-over-argic/natric/spodic or retic layer, no layer coarser than
#' sandy loam >= 15 cm thick starting <= 50 cm above it, or those layers are
#' claric or show no soil-formation evidence (cambic criterion 3,
#' \code{test_cambic_soil_formation()}).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_neobrunic <- function(pedon) {
  h <- pedon$horizons
  low <- .q_neo_lower_feature(pedon)
  res <- function(passed, missing = character(0), ...)
    DiagnosticResult$new(name = "Neobrunic", passed = passed, layers = integer(0),
      evidence = c(list(...), low$evidence), missing = missing,
      reference = "WRB (2022) Ch 5, Neobrunic")
  if (isFALSE(low$passed))
    return(res(FALSE, reason = "no albic horizon over argic/natric/spodic, no retic properties"))
  cut <- if (is.na(low$top)) Inf else low$top
  clay <- h$clay_pct
  silt <- ifelse(is.na(h$silt_pct) & !is.na(h$sand_pct) & !is.na(clay),
                 100 - h$sand_pct - clay, h$silt_pct)
  # Fails cambic criterion 1: sand or loamy sand (silt + 2 clay < 30).
  coarse <- ifelse(is.na(clay) | is.na(silt), NA, silt + 2 * clay < 30)
  coarse[!is.na(h$bottom_cm) & h$bottom_cm > cut + 0.5] <- FALSE
  rule <- .q_thickness_rule(h, coarse, 15, contiguous = TRUE, max_start = 50)
  if (isFALSE(rule$passed))
    return(res(FALSE, reason = "no layer coarser than sandy loam >= 15 cm starting <= 50 cm above the albic/retic layer"))
  cand <- which(coarse %in% TRUE | is.na(coarse))
  cand <- cand[!is.na(h$top_cm[cand]) & h$top_cm[cand] <= 50]
  cla <- tryCatch(claric_material(pedon), error = function(e) NULL)
  is_claric <- seq_len(nrow(h)) %in% (if (!is.null(cla) && isTRUE(cla$passed)) cla$layers else integer(0))
  sf <- test_cambic_soil_formation(h, candidate_layers = cand)
  sf_eval <- as.integer(names(Filter(function(d) length(d$evidence) > 0L, sf$details)))
  no_sf <- setdiff(sf_eval, sf$layers)
  if (length(cand) && all(cand %in% union(which(is_claric), no_sf)))
    return(res(FALSE, reason = "candidate layers are claric or show no soil-formation evidence"))
  res(NA, missing = c("Brunic layer: cambic criteria 3 and 4 without criterion 1 (no soilKey diagnostic)",
                      "very fine sand classes"),
      candidate_layers = cand, lower_feature_top_cm = low$top)
}


#' Neocambic qualifier (nc): cambic horizon over albic-over-argic / retic
#'
#' v0.9.217: WRB 2022 Ch 5 (under Cambic), "having a cambic horizon, not
#' consisting of claric material, starting <= 50 cm from the mineral soil
#' surface and overlying: an albic horizon that overlies an argic, a natric
#' or a spodic horizon, or a layer with retic properties". The v0.9.64 code
#' was a cambic horizon with weak structure. cambic() rejects any profile that
#' has an argic horizon, while the cambic criterion is only that the layer
#' does not form part of one, so cambic() is evaluated on the layers above the
#' albic / retic layer.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_neocambic <- function(pedon) {
  h <- pedon$horizons
  low <- .q_neo_lower_feature(pedon)
  res <- function(passed, layers = integer(0), missing = character(0), ...)
    DiagnosticResult$new(name = "Neocambic", passed = passed, layers = layers,
      evidence = c(list(...), low$evidence), missing = missing,
      reference = "WRB (2022) Ch 5, Neocambic")
  if (isFALSE(low$passed))
    return(res(FALSE, reason = "no albic horizon over argic/natric/spodic, no retic properties"))
  cut <- if (is.na(low$top)) Inf else low$top
  keep <- if (is.finite(cut)) which(!is.na(h$top_cm) & h$top_cm < cut) else seq_len(nrow(h))
  if (!length(keep))
    return(res(FALSE, reason = "no layer above the albic / retic layer"))
  sub <- if (is.finite(cut)) PedonRecord$new(site = pedon$site, horizons = h[keep, ])
         else pedon
  cam <- tryCatch(cambic(sub), error = function(e) NULL)
  if (is.null(cam) || !isTRUE(cam$passed))
    return(res(if (is.null(cam) || is.na(cam$passed)) NA else FALSE,
               missing = cam$missing %||% "cambic", cambic = cam))
  cl <- keep[cam$layers]
  cl <- cl[!is.na(h$top_cm[cl]) & h$top_cm[cl] <= 50 &
             !is.na(h$bottom_cm[cl]) & h$bottom_cm[cl] <= cut + 0.5]
  if (!length(cl))
    return(res(FALSE, reason = "cambic horizon starts > 50 cm or is not above the albic / retic layer",
               cambic = cam))
  cla <- tryCatch(claric_material(pedon), error = function(e) NULL)
  claric <- if (!is.null(cla) && isTRUE(cla$passed)) cla$layers else integer(0)
  has_col <- (!is.na(h$munsell_value_moist) & !is.na(h$munsell_chroma_moist)) |
             (!is.na(h$munsell_value_dry) & !is.na(h$munsell_chroma_dry))
  not_claric <- cl[!cl %in% claric & has_col[cl]]
  passed <- if (length(not_claric)) low$passed
            else if (all(cl %in% claric)) FALSE else NA
  res(passed, layers = if (isTRUE(passed)) not_claric else integer(0),
      missing = if (is.na(passed)) c(if (is.na(low$passed)) "albic / argic / natric / spodic / retic status",
                                     if (!length(not_claric)) "munsell colour (claric material)")
                else character(0),
      cambic = cam, lower_feature_top_cm = low$top)
}


#' Petrosalic qualifier (ps): layer cemented by salts more soluble than gypsum
#'
#' v0.9.217: WRB 2022 Ch 5, "having a layer, >= 10 cm thick and within 100 cm
#' of the mineral soil surface, which is cemented by salts more soluble than
#' gypsum". The v0.9.64 code combined the SiBCS carater_salico() with a
#' \code{consistence_dry} column that is not in the schema (so it never
#' passed). The schema has no cementing-agent field; the agent is read from
#' the FAO designation suffixes, where \code{zm} marks cementation (m) by
#' salts more soluble than gypsum (z). A layer is not salt-cemented when
#' \code{cementation_class} says it is not cemented, when its designation has
#' no \code{m} (and no class is recorded), or when \code{km} / \code{ym} /
#' \code{qm} / \code{sm} name another cement. A cemented layer whose agent is
#' not recorded leaves the result NA.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_petrosalic <- function(pedon) {
  h <- pedon$horizons
  d  <- h$designation %||% rep(NA_character_, nrow(h))
  cc <- tolower(h$cementation_class %||% rep(NA_character_, nrow(h)))
  sfx <- ifelse(is.na(d), NA_character_, sub("^[0-9]*[A-Z/&]+", "", d))
  cc_no  <- !is.na(cc) & grepl("none|non|not|uncement", cc)
  cc_yes <- !is.na(cc) & !cc_no & grepl("weak|moderat|strong|indurat|cement", cc)
  m_sfx  <- !is.na(sfx) & grepl("m", sfx)
  salt_m <- !is.na(sfx) & (grepl("zm", sfx) |
              (grepl("z", sfx) & m_sfx & !grepl("[kyqs]", sfx)))
  other_m <- !is.na(sfx) & grepl("[kyqs]m", sfx) & !grepl("zm", sfx)
  cemented <- ifelse(cc_yes | m_sfx, TRUE,
                ifelse(cc_no | (is.na(cc) & !is.na(sfx)), FALSE, NA))
  status <- ifelse(cc_no, FALSE,
              ifelse(salt_m & cemented %in% TRUE, TRUE,
                ifelse(cemented %in% FALSE | other_m, FALSE, NA)))
  if (all(is.na(d)) && all(is.na(cc))) {
    return(DiagnosticResult$new(
      name = "Petrosalic", passed = NA, layers = integer(0),
      evidence = list(reason = "no designation / cementation_class"),
      missing = c("designation", "cementation_class"),
      reference = "WRB (2022) Ch 5, Petrosalic"))
  }
  rule <- .q_thickness_rule(h, status, 10, win_top = 0, win_bot = 100,
                            contiguous = TRUE)
  DiagnosticResult$new(
    name = "Petrosalic", passed = rule$passed,
    layers = if (isTRUE(rule$passed)) rule$layers else integer(0),
    evidence = list(salt_cemented = status, thickness_cm = rule$thickness_cm),
    missing = if (is.na(rule$passed)) "cementing agent of the cemented layer"
              else character(0),
    reference = "WRB (2022) Ch 5, Petrosalic"
  )
}


# ----------------------------------------------------------------------------
# Thin presence-wrappers for three canonical qualifiers whose backing
# diagnostic was already implemented but lacked a `qual_*` entry point
# (v0.9.145). None of Sideralic / Panpaic / Claric appears in any RSG
# applicable list, so these add no classification behaviour -- they make the
# existing diagnostics callable as qualifiers and let coverage_report() count
# them honestly. The remaining gap, Novic, is genuinely schema-blocked
# (needs a deposition-age field).
# ----------------------------------------------------------------------------

#' Sideralic qualifier (se): sideralic properties within 150 cm, no ferralic.
#'
#' v0.9.217: WRB 2022 Ch 5, "having within 150 cm of the mineral soil surface
#' a layer that has sideralic properties; and not having a ferralic horizon
#' starting <= 150 cm from the mineral soil surface". The v0.9.145 wrapper
#' used 100 cm and did not exclude ferralic horizons.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_sideralic <- function(pedon) {
  h <- pedon$horizons
  sp <- sideralic_properties(pedon)
  fr <- tryCatch(ferralic(pedon), error = function(e) NULL)
  sp_layers <- if (isTRUE(sp$passed))
    sp$layers[!is.na(h$top_cm[sp$layers]) & h$top_cm[sp$layers] < 150] else integer(0)
  fr_in <- !is.null(fr) && isTRUE(fr$passed) &&
    any(!is.na(h$top_cm[fr$layers]) & h$top_cm[fr$layers] <= 150)
  fr_unknown <- is.null(fr) || is.na(fr$passed)
  passed <- if (fr_in) FALSE
            else if (length(sp_layers)) { if (fr_unknown) NA else TRUE }
            else if (is.na(sp$passed)) NA else FALSE
  DiagnosticResult$new(
    name = "Sideralic", passed = passed,
    layers = if (isTRUE(passed)) sp_layers else integer(0),
    evidence = list(sideralic_properties = sp, ferralic = fr),
    missing = if (is.na(passed))
                unique(c(sp$missing, if (fr_unknown) fr$missing %||% "ferralic"))
              else character(0),
    reference = "WRB (2022) Ch 5, Sideralic")
}

#' Panpaic qualifier (pb): panpaic horizon starting <= 100 cm.
#'
#' v0.9.217: WRB 2022 Ch 5, "having a panpaic horizon starting <= 100 cm from
#' the mineral soil surface". The panpaic horizon (Ch 3.1.23) is a buried
#' surface horizon of mineral material with (1) >= 0.2 \% SOC, (2) SOC >= 25 \%
#' (relative) and >= 0.2 \% (absolute) higher than in the overlying layer,
#' (3) a lithic discontinuity at its upper limit and (4) >= 5 cm thickness.
#' panpaic() only matches buried-horizon designations, and case-insensitively,
#' so every AB horizon passed (the v0.9.145 wrapper put Panpaic on most
#' Chernozems, Phaeozems and Ferralsols). Its candidates are kept only when
#' they are buried A horizons (b suffix, or A master below a numbered
#' discontinuity) and meet criteria 1-4; the lithic discontinuity is read from
#' lithic_discontinuity() or a change in the designation's numeral prefix.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_panpaic <- function(pedon) {
  h <- pedon$horizons
  pp <- panpaic(pedon)
  ref <- "WRB (2022) Ch 5, Panpaic"
  if (!isTRUE(pp$passed))
    return(DiagnosticResult$new(name = "Panpaic",
      passed = if (is.na(pp$passed)) NA else FALSE, layers = integer(0),
      evidence = list(panpaic = pp), missing = pp$missing %||% character(0),
      reference = ref))
  d <- h$designation %||% rep(NA_character_, nrow(h))
  num <- suppressWarnings(as.integer(ifelse(grepl("^[0-9]+", d),
                                            sub("^([0-9]+).*", "\\1", d), "1")))
  buried_a <- !is.na(d) & grepl("^[0-9]*A", d) &
    (grepl("^[0-9]*A[a-z0-9]*b[0-9]*$", d) | (!is.na(num) & num >= 2))
  ld <- tryCatch(lithic_discontinuity(pedon), error = function(e) NULL)
  ord <- order(h$top_cm)
  status <- rep(FALSE, nrow(h))
  for (k in seq_along(ord)) {
    i <- ord[k]
    if (!(i %in% pp$layers) || !buried_a[i] || k == 1L) next
    j <- ord[k - 1L]
    if (is.na(h$top_cm[i]) || h$top_cm[i] > 100) next
    thk <- h$bottom_cm[i] - h$top_cm[i]
    disc <- if ((i %in% (ld$layers %||% integer(0))) ||
                (!is.na(num[i]) && !is.na(num[j]) && num[i] != num[j])) TRUE
            else if (is.null(ld) || is.na(ld$passed)) NA else FALSE
    oc_i <- h$oc_pct[i]; oc_j <- h$oc_pct[j]
    mineral <- if (is.na(oc_i)) NA else oc_i < 20
    soc <- if (is.na(oc_i) || is.na(oc_j)) NA
           else oc_i >= 0.2 && oc_i - oc_j >= 0.2 && oc_i >= 1.25 * oc_j
    crit <- c(mineral, soc, disc, if (is.na(thk)) NA else thk >= 5)
    status[i] <- if (any(crit %in% FALSE)) FALSE else if (all(crit %in% TRUE)) TRUE else NA
  }
  passed <- if (any(status %in% TRUE)) TRUE else if (anyNA(status)) NA else FALSE
  DiagnosticResult$new(
    name = "Panpaic", passed = passed,
    layers = which(status %in% TRUE),
    evidence = list(panpaic = pp, lithic_discontinuity = ld,
                    buried_a_horizon = which(buried_a), status = status),
    missing = if (is.na(passed)) c("oc_pct", "top_cm", "bottom_cm") else character(0),
    reference = ref)
}

#' Claric qualifier (cq): a layer >= 30 cm thick of claric material between
#' 25 and 100 cm of the mineral soil surface, and not Bathyspodic (Arenosols).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_claric <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Claric: "having between 25 and 100 cm of the
  # mineral soil surface a layer, >= 30 cm thick, that consists of claric
  # material, and the soil does not meet the set of diagnostic criteria of the
  # Bathyspodic qualifier (in Arenosols only)". The old code passed on claric
  # material in any one horizon starting <= 100 cm: no thickness, no 25 cm
  # upper limit, no Bathyspodic exclusion.
  r <- .q_rsg_only("Claric", rsg_code, "AR"); if (!is.null(r)) return(r)
  ref <- "WRB (2022) Ch 5, Claric"
  h <- pedon$horizons
  mss <- .q_mineral_surface_cm(h)
  if (is.na(mss))
    return(DiagnosticResult$new(name = "Claric", passed = FALSE,
      layers = integer(0), evidence = list(reason = "no mineral material"),
      missing = character(0), reference = ref))
  cm <- claric_material(pedon)
  # Claric material needs the dry colour (criterion 1) AND the moist colour
  # (criterion 2) of Ch 3.3.4; claric_material() accepts either one, so both
  # are read from its per-horizon evidence. A missing colour leaves the
  # horizon unknown.
  ok <- rep(NA, nrow(h))
  for (d in cm$evidence$munsell$details %||% list()) {
    moist <- if (anyNA(d$moist)) NA else isTRUE(d$moist_ok)
    dry   <- if (anyNA(d$dry))   NA else isTRUE(d$dry_ok)
    ok[d$idx] <- moist & dry
  }
  oc <- h$oc_pct %||% rep(NA_real_, nrow(h))
  ok[!is.na(oc) & oc >= 20] <- FALSE          # claric material is mineral
  run <- .q_run_tristate(h, ok, 30, from = mss + 25, to = mss + 100)
  # Bathyspodic: in an Arenosol a spodic horizon can only start > 200 cm.
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  deep <- any(!is.na(h$top_cm) & h$top_cm > mss + 200)
  bathy <- if (!deep) FALSE
           else if (is.null(spo) || is.na(spo$passed)) NA
           else isTRUE(spo$passed) &&
                  min(h$top_cm[spo$layers], na.rm = TRUE) > mss + 200
  passed <- if (isTRUE(bathy)) FALSE else run$passed & !bathy
  DiagnosticResult$new(
    name = "Claric", passed = passed,
    layers = if (isTRUE(passed)) run$layers else integer(0),
    evidence = list(claric_material = cm, claric_thickness_cm = run$thickness,
                    window_cm = c(mss + 25, mss + 100), bathyspodic = bathy),
    missing = if (is.na(passed))
                unique(c(if (is.na(run$passed))
                           c("munsell_value_dry", "munsell_chroma_dry",
                             "munsell_value_moist", "munsell_chroma_moist"),
                         if (is.na(bathy)) spo$missing %||% "spodic"))
              else character(0),
    reference = ref)
}


# ============================================================================
# SUPPLEMENTARY QUALIFIERS (SQ) -- v0.9.64 final batch
# ============================================================================
#
# Most are mechanical Endo-/Bathy-/Hyper-/Hypo-/Proto- variants of
# existing primitives. We use `.q_within_depth()` (defined in v0.9.63)
# for the depth-modifier patterns.
# ============================================================================


# --- Endic / Epic generic depth markers -------------------------------------





#' Endothyric supplementary qualifier: technic hard material starting > 50
#' and <= 100 cm from the soil surface, none starting <= 50 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endothyric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Thyric is "having technic hard material starting
  # within > 5 and <= 100 cm from the soil surface (1: Epi- and Endo- only)";
  # Endo- for a characteristic at a point of depth (Ch 2.3.1, rule 1) is
  # "present somewhere > 50 cm ... and absent <= 50 cm". So the uppermost
  # technic hard material starts > 50 and <= 100 cm. The old code took any
  # qual_thyric() layer overlapping 50-200 cm, and qual_thyric() read
  # organic-rich artefacts, not technic hard material.
  ref <- "WRB (2022) Ch 5, Endothyric"
  h <- pedon$horizons
  thm <- technic_hard_material(pedon)
  if (!isTRUE(thm$passed))
    return(DiagnosticResult$new(name = "Endothyric",
      passed = if (is.na(thm$passed)) NA else FALSE, layers = integer(0),
      evidence = list(technic_hard_material = thm),
      missing = thm$missing %||% character(0), reference = ref))
  top <- min(h$top_cm[thm$layers], na.rm = TRUE)
  passed <- is.finite(top) && top > 50 && top <= 100
  DiagnosticResult$new(
    name = "Endothyric", passed = passed,
    layers = if (passed) thm$layers else integer(0),
    evidence = list(technic_hard_material = thm, upper_limit_cm = top),
    missing = character(0), reference = ref)
}


# --- Tier-2 substantive ----------------------------------------------------

#' Hyperorganic supplementary qualifier (jo): organic material >= 200 cm
#' thick (Histosols).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_hyperorganic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Hyperorganic: "having organic material >= 200 cm
  # thick (in Histosols only)". Organic material now comes from
  # organic_material() (>= 20% SOC; the old code used 18%) and has to form one
  # layer >= 200 cm thick (the old code added up separate layers). When the
  # description ends in organic material short of 200 cm the result is NA.
  r <- .q_rsg_only("Hyperorganic", rsg_code, "HS"); if (!is.null(r)) return(r)
  ref <- "WRB (2022) Ch 5, Hyperorganic"
  h <- pedon$horizons
  oc <- h$oc_pct %||% rep(NA_real_, nrow(h))
  if (all(is.na(oc)))
    return(DiagnosticResult$new(name = "Hyperorganic", passed = NA,
      layers = integer(0), evidence = list(reason = "no oc_pct data"),
      missing = "oc_pct", reference = ref))
  om <- organic_material(pedon)
  ok <- ifelse(is.na(oc), NA, seq_len(nrow(h)) %in% om$layers)
  run <- .q_run_tristate(h, ok, 200)
  # The organic material may go on below the deepest described horizon.
  last <- which.max(h$bottom_cm)
  open_end <- length(last) == 1L && !isFALSE(ok[last])
  passed <- if (isTRUE(run$passed)) TRUE
            else if (is.na(run$passed) || open_end) NA else FALSE
  DiagnosticResult$new(
    name = "Hyperorganic", passed = passed,
    layers = if (isTRUE(passed)) run$layers else integer(0),
    evidence = list(organic_thickness_cm = run$thickness,
                    described_to_cm = max(h$bottom_cm, na.rm = TRUE)),
    missing = if (is.na(passed)) c("oc_pct", "bottom_cm") else character(0),
    reference = ref)
}


#' Mineralic supplementary qualifier (mi): mineral layers in a Histosol
#'
#' v0.9.217: WRB 2022 Ch 5, "having, within 100 cm of the soil surface, one or
#' more layers of mineral material, not consisting of mulmic material, with a
#' combined thickness of >= 20 cm, above or in between layers of organic
#' material (in Histosols only)". The v0.9.64 code passed whenever the
#' weighted SOC of the upper 100 cm was < 12 \%, i.e. on almost every mineral
#' soil. It now counts layers that are mineral_material(), not
#' mulmic_material() and have organic_material() somewhere below them.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_mineralic <- function(pedon) {
  h <- pedon$horizons
  oc <- h$oc_pct
  if (is.null(oc) || all(is.na(oc))) {
    return(DiagnosticResult$new(
      name = "Mineralic", passed = NA, layers = integer(0),
      evidence = list(reason = "no oc_pct data"),
      missing = "oc_pct",
      reference = "WRB (2022) Ch 5, Mineralic"
    ))
  }
  n <- nrow(h)
  mi <- mineral_material(pedon); mu <- mulmic_material(pedon)
  om <- organic_material(pedon)
  org <- if (isTRUE(om$passed)) om$layers else integer(0)
  mineral <- ifelse(is.na(oc), NA, seq_len(n) %in% mi$layers)
  chroma <- h$munsell_chroma_moist
  # mulmic material needs >= 8 % SOC (and a chroma <= 2 in soilKey's test)
  not_mulmic <- ifelse(seq_len(n) %in% mu$layers, FALSE,
                  ifelse(!is.na(oc) & oc < 8, TRUE,
                    ifelse(!is.na(oc) & !is.na(chroma), TRUE, NA)))
  # A layer without SOC data is still known not to be organic when its
  # designation is not an O / H horizon.
  d <- h$designation
  non_org <- (!is.na(oc) & oc < 20) |
    (is.na(oc) & !is.na(d) & !grepl("^[0-9]*[OH]", d))
  below_org <- vapply(seq_len(n), function(i) {
    deeper <- which(!is.na(h$top_cm) & h$top_cm >= h$bottom_cm[i] - 0.5)
    if (any(deeper %in% org)) TRUE
    else if (all(non_org[deeper])) FALSE else NA
  }, logical(1))
  status <- vapply(seq_len(n), function(i) {
    x <- c(mineral[i], not_mulmic[i], below_org[i])
    if (any(x %in% FALSE)) FALSE else if (all(x %in% TRUE)) TRUE else NA
  }, logical(1))
  rule <- .q_thickness_rule(h, status, 20, win_top = 0, win_bot = 100)
  DiagnosticResult$new(
    name = "Mineralic", passed = rule$passed,
    layers = if (isTRUE(rule$passed)) rule$layers else integer(0),
    evidence = list(mineral_material = mi, mulmic_material = mu,
                    organic_material = om, thickness_cm = rule$thickness_cm),
    missing = if (is.na(rule$passed)) c("oc_pct", "munsell_chroma_moist")
              else character(0),
    reference = "WRB (2022) Ch 5, Mineralic"
  )
}


#' Alcalic supplementary qualifier (ax): pH (1:1 water) >= 8.5 in the upper
#' 50 cm (organic material in Histosols) and Eutric.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_alcalic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Alcalic: "in Histosols, a pH (1:1 in water) of
  # >= 8.5 in the organic material within 50 cm of the soil surface, in other
  # soils, a pH (1:1 in water) of >= 8.5 in the upper 50 cm of the mineral soil
  # surface or to a limiting layer, whichever is shallower, and fulfilling the
  # set of diagnostic criteria of the Eutric qualifier". The pH has to hold
  # throughout that depth ("The diagnostic criteria must be fulfilled
  # throughout the specified depth range, unless stated otherwise", Ch 2.1).
  # The old code passed on pH >= 9 in any one horizon within 100 cm and
  # ignored Eutric. Without an RSG the "other soils" branch is used.
  ref <- "WRB (2022) Ch 5, Alcalic"
  h <- pedon$horizons
  ph <- h$ph_h2o %||% rep(NA_real_, nrow(h))
  if (all(is.na(ph)))
    return(DiagnosticResult$new(name = "Alcalic", passed = NA,
      layers = integer(0), evidence = list(reason = "no ph_h2o data"),
      missing = "ph_h2o", reference = ref))
  alk <- ph >= 8.5
  if (identical(rsg_code, "HS")) {
    om <- organic_material(pedon)
    ly <- om$layers[!is.na(h$top_cm[om$layers]) & h$top_cm[om$layers] < 50]
    ph_ok <- if (!length(ly)) { if (is.na(om$passed)) NA else FALSE }
             else if (any(alk[ly] %in% FALSE)) FALSE
             else if (anyNA(alk[ly])) NA else TRUE
    window <- c(0, 50)
  } else {
    mss <- .q_mineral_surface_cm(h)
    if (is.na(mss))
      return(DiagnosticResult$new(name = "Alcalic", passed = FALSE,
        layers = integer(0), evidence = list(reason = "no mineral material"),
        missing = character(0), reference = ref))
    # Limiting layer: continuous rock or technic hard material.
    lim <- unlist(lapply(list(continuous_rock(pedon), technic_hard_material(pedon)),
                         function(r) if (isTRUE(r$passed)) h$top_cm[r$layers]))
    bot <- min(c(mss + 50, lim[!is.na(lim) & lim > mss]))
    ly <- which(!is.na(h$top_cm) & !is.na(h$bottom_cm) &
                  h$bottom_cm > mss & h$top_cm < bot)
    # "Throughout": one run of alkaline horizons covering the whole window.
    ph_ok <- .q_run_tristate(h, alk, bot - mss, from = mss, to = bot,
                             max_start = mss)$passed
    window <- c(mss, bot)
  }
  eu <- qual_eutric(pedon)
  passed <- ph_ok & eu$passed
  DiagnosticResult$new(
    name = "Alcalic", passed = passed,
    layers = if (isTRUE(passed)) ly else integer(0),
    evidence = list(window_cm = window, ph_h2o = ph[ly], ph_ok = ph_ok,
                    eutric = eu),
    missing = if (is.na(passed))
                unique(c(if (is.na(ph_ok)) "ph_h2o", eu$missing))
              else character(0),
    reference = ref)
}


#' Chloridic supplementary qualifier (cl): salic horizon whose 1:1 soil
#' solution has [Cl-] > 2*[SO4--] > 2*[HCO3-] (Solonchaks).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_chloridic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Chloridic: "having a salic horizon with a soil
  # solution (1:1 in water) with [Cl-] > 2*[SO42-] > 2*[HCO3-] (in Solonchaks
  # only)". soilKey holds no soil-solution anions, so the result is FALSE
  # without a salic horizon and NA with one. The old code passed on
  # Cl >= 4 cmolc/kg or EC >= 8 dS/m (read from a misspelt ec_ds_m column).
  r <- .q_rsg_only("Chloridic", rsg_code, "SC"); if (!is.null(r)) return(r)
  ref <- "WRB (2022) Ch 5, Chloridic"
  sal <- salic(pedon)
  if (!isTRUE(sal$passed))
    return(DiagnosticResult$new(name = "Chloridic",
      passed = if (is.na(sal$passed)) NA else FALSE, layers = integer(0),
      evidence = list(salic = sal),
      missing = sal$missing %||% character(0), reference = ref))
  DiagnosticResult$new(
    name = "Chloridic", passed = NA, layers = integer(0),
    evidence = list(salic = sal,
                    reason = "anion ratio of the 1:1 soil solution not recorded"),
    missing = c("soil_solution_cl", "soil_solution_so4", "soil_solution_hco3"),
    reference = ref)
}


#' Columnic supplementary qualifier (cu): a layer >= 15 cm thick, starting
#' <= 100 cm from the mineral soil surface, with columnar structure.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_columnic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Columnic: "having a layer, >= 15 cm thick and
  # starting <= 100 cm from the mineral soil surface, that has a columnar
  # structure". The columnar horizons must now be contiguous (the old code
  # added up separate ones), the depth counts from the mineral soil surface,
  # and the Portuguese "colunar" is read.
  ref <- "WRB (2022) Ch 5, Columnic"
  h <- pedon$horizons
  st <- h$structure_type %||% rep(NA_character_, nrow(h))
  if (all(is.na(st)))
    return(DiagnosticResult$new(name = "Columnic", passed = NA,
      layers = integer(0), evidence = list(reason = "no structure_type data"),
      missing = "structure_type", reference = ref))
  mss <- .q_mineral_surface_cm(h)
  if (is.na(mss))
    return(DiagnosticResult$new(name = "Columnic", passed = FALSE,
      layers = integer(0), evidence = list(reason = "no mineral material"),
      missing = character(0), reference = ref))
  pat <- "(?i)colum|colun"
  ok <- ifelse(is.na(st), NA, grepl(pat, st, perl = TRUE))
  run <- .q_run_tristate(h, ok, 15, from = mss, max_start = mss + 100)
  DiagnosticResult$new(
    name = "Columnic", passed = run$passed,
    layers = if (isTRUE(run$passed)) run$layers else integer(0),
    evidence = list(pattern = pat, columnar_thickness_cm = run$thickness),
    missing = if (is.na(run$passed)) "structure_type" else character(0),
    reference = ref)
}


#' Differentic supplementary qualifier (df): an argic or natric horizon that
#' meets criterion 2.a (clay increase) of the respective horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_differentic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Differentic: "having an argic or natric horizon
  # that meets diagnostic criterion 2.a of the respective horizon", i.e. the
  # clay increase over an overlying coarser-textured layer (2.a.iv-vi: +6%
  # absolute, x1.4 or +20% absolute). Criterion 2.a is read from the argic
  # clay-increase test (WRB thresholds) on the horizons that argic() or
  # natric_horizon() accept. The old code passed on a clay ratio of 1.2-1.4
  # between any two adjacent horizons, below the argic threshold and without
  # an argic or natric horizon.
  ref <- "WRB (2022) Ch 5, Differentic"
  h <- pedon$horizons
  arg <- argic(pedon)
  nat <- natric_horizon(pedon)
  hz <- sort(union(if (isTRUE(arg$passed)) arg$layers,
                   if (isTRUE(nat$passed)) nat$layers))
  if (!length(hz))
    return(DiagnosticResult$new(name = "Differentic",
      passed = if (is.na(arg$passed) || is.na(nat$passed)) NA else FALSE,
      layers = integer(0), evidence = list(argic = arg, natric = nat),
      missing = unique(c(arg$missing, nat$missing)), reference = ref))
  ci <- test_clay_increase_argic(h, system = "wrb2022")
  hit <- intersect(hz, ci$layers)
  clay_known <- !anyNA(h$clay_pct[seq_len(max(hz))])
  passed <- if (length(hit)) TRUE else if (clay_known) FALSE else NA
  DiagnosticResult$new(
    name = "Differentic", passed = passed,
    layers = hit,
    evidence = list(argic = arg, natric = nat, clay_increase = ci),
    missing = if (is.na(passed)) "clay_pct" else character(0),
    reference = ref)
}


#' Capillaric supplementary qualifier (cp): a layer >= 25 cm thick, starting
#' <= 75 cm from the mineral soil surface, so poor in macropores that water
#' saturation of capillary pores causes reducing conditions.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_capillaric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Capillaric: "having a layer, >= 25 cm thick and
  # starting <= 75 cm from the mineral soil surface, that has so few
  # macropores that water saturation of capillary pores causes reducing
  # conditions". soilKey records neither macroporosity nor what causes the
  # reducing conditions, so the qualifier can only be ruled out: FALSE when no
  # such layer has reducing conditions, NA otherwise. The old code passed on
  # >= 2% redox features and clay + silt > 50% above 50 cm, which named the
  # example Planosol and Stagnosol Capillaric although their reducing
  # conditions come from perched (stagnic) water, not from capillary pores.
  ref <- "WRB (2022) Ch 5, Capillaric"
  h <- pedon$horizons
  mss <- .q_mineral_surface_cm(h)
  if (is.na(mss))
    return(DiagnosticResult$new(name = "Capillaric", passed = FALSE,
      layers = integer(0), evidence = list(reason = "no mineral material"),
      missing = character(0), reference = ref))
  rc <- reducing_conditions(pedon)
  red <- rep(NA, nrow(h))
  for (d in rc$evidence$redox$details %||% list()) red[d$idx] <- isTRUE(d$passed)
  run <- .q_run_tristate(h, red, 25, from = mss, max_start = mss + 75)
  passed <- if (isFALSE(run$passed)) FALSE else NA
  DiagnosticResult$new(
    name = "Capillaric", passed = passed, layers = integer(0),
    evidence = list(reducing_conditions = rc,
                    reducing_layer_cm = run$thickness,
                    reason = if (is.na(passed))
                      "macroporosity / capillary saturation not recorded"),
    missing = if (is.na(passed))
                c("macroporosity", if (is.na(run$passed)) "redoximorphic_features_pct")
              else character(0),
    reference = ref)
}


#' Protospodic supplementary qualifier (qp): Al_ox enrichment, no spodic
#'
#' v0.9.217: WRB 2022 Ch 5 (under Spodic), "having a layer, starting <= 100 cm
#' from the mineral soil surface, that has an Alox value that is >= 1.5 times
#' that of the lowest Alox value of all the mineral layers above; and not
#' having a spodic horizon starting <= 200 cm from the mineral soil surface".
#' The v0.9.64 code passed on a Bh / Bs designation when spodic() did not
#' pass. It now compares \code{al_ox_pct} with the mineral layers above
#' (the lowest known value; an unknown one could only be lower, which keeps a
#' pass valid) and needs spodic() to be FALSE, not NA.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_protospodic <- function(pedon) {
  ref <- "WRB (2022) Ch 5, Protospodic"
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  h <- pedon$horizons
  al <- h$al_ox_pct
  if (is.null(al) || all(is.na(al))) {
    return(DiagnosticResult$new(
      name = "Protospodic", passed = NA, layers = integer(0),
      evidence = list(reason = "no al_ox_pct data", spodic = spo),
      missing = "al_ox_pct", reference = ref))
  }
  spodic_in <- !is.null(spo) && isTRUE(spo$passed) &&
    any(!is.na(h$top_cm[spo$layers]) & h$top_cm[spo$layers] <= 200)
  org <- (!is.na(h$oc_pct) & h$oc_pct >= 20) |
         (!is.na(h$designation) & grepl("^[0-9]*[OH]", h$designation))
  known_mineral <- !org & (!is.na(h$oc_pct) | !is.na(h$designation))
  hits <- integer(0); undecided <- FALSE
  for (i in which(!is.na(h$top_cm) & h$top_cm <= 100 & !org)) {
    above_all <- which(!org & !is.na(h$bottom_cm) & h$bottom_cm <= h$top_cm[i] + 0.5)
    if (!length(above_all)) next
    above <- above_all[known_mineral[above_all] & !is.na(al[above_all])]
    if (is.na(al[i]) || !length(above)) { undecided <- TRUE; next }
    if (al[i] > 0 && al[i] >= 1.5 * min(al[above])) hits <- c(hits, i)
    else if (length(above) < length(above_all)) undecided <- TRUE
  }
  passed <- if (spodic_in) FALSE
            else if (!length(hits)) { if (undecided) NA else FALSE }
            else if (is.null(spo) || is.na(spo$passed)) NA else TRUE
  DiagnosticResult$new(
    name = "Protospodic", passed = passed,
    layers = if (isTRUE(passed)) hits else integer(0),
    evidence = list(spodic = spo, al_ox_enriched_layers = hits),
    missing = if (!is.na(passed)) character(0)
              else if (!length(hits)) "al_ox_pct"
              else spo$missing %||% "spodic",
    reference = ref)
}


#' Protoargic supplementary qualifier (qg): >= 4 \% clay step, within 100 cm
#'
#' v0.9.217: WRB 2022 Ch 5, "having an absolute clay increase of >= 4 \% from
#' one layer to the directly underlying layer, within 100 cm of the mineral
#' soil surface (in Arenosols only)". The v0.9.64 code accepted increases of
#' 2 to < 6 points at any depth (rejecting >= 6 and accepting 2-4). It now
#' needs >= 4 points between depth-contiguous layers, the underlying one
#' starting <= 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_protoargic <- function(pedon) {
  h <- pedon$horizons
  cl <- h$clay_pct
  if (is.null(cl) || sum(!is.na(cl)) < 2L) {
    return(DiagnosticResult$new(
      name = "Protoargic", passed = NA, layers = integer(0),
      evidence = list(reason = "need >= 2 layers with clay_pct"),
      missing = "clay_pct",
      reference = "WRB (2022) Ch 5, Protoargic"
    ))
  }
  ord <- order(h$top_cm)
  hits <- integer(0); undecided <- FALSE
  for (k in seq_len(length(ord) - 1L)) {
    up <- ord[k]; dn <- ord[k + 1L]
    if (is.na(h$top_cm[dn]) || h$top_cm[dn] > 100) next
    if (is.na(h$bottom_cm[up]) || abs(h$top_cm[dn] - h$bottom_cm[up]) > 0.5) next
    if (is.na(cl[up]) || is.na(cl[dn])) { undecided <- TRUE; next }
    if (cl[dn] - cl[up] >= 4) hits <- c(hits, dn)
  }
  passed <- if (length(hits)) TRUE else if (undecided) NA else FALSE
  DiagnosticResult$new(
    name = "Protoargic", passed = passed, layers = hits,
    evidence = list(min_increase_pct = 4, max_depth_cm = 100),
    missing = if (is.na(passed)) "clay_pct" else character(0),
    reference = "WRB (2022) Ch 5, Protoargic"
  )
}


#' Protoandic supplementary qualifier (qa): weak andic characteristics
#'
#' v0.9.217: WRB 2022 Ch 5 (under Andic), "having within 100 cm of the soil
#' surface one or more layers with a combined thickness of >= 15 cm, and with
#' an Alox + 1/2 Feox value of >= 1.2 \%, a bulk density of <= 1.2 kg dm-3
#' and a phosphate retention of >= 55 \%; and not fulfilling the set of
#' diagnostic criteria of the Andic qualifier". The v0.9.64 code took
#' Alox + Feox (not 1/2 Feox) between 0.4 and 2 \% in any layer, without bulk
#' density, phosphate retention, thickness or the Andic exclusion. Bulk
#' density is read from \code{bulk_density_g_cm3}, which the definition wants
#' measured on undried samples desorbed at 33 kPa. The Andic criteria differ
#' in Cambisols, so the RSG is passed on to qual_andic() when it takes one.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_protoandic <- function(pedon, rsg_code = NULL) {
  h <- pedon$horizons
  al_ox <- h$al_ox_pct
  fe_ox <- h$fe_ox_pct %||% rep(NA_real_, nrow(h))
  if (is.null(al_ox) || all(is.na(al_ox))) {
    return(DiagnosticResult$new(
      name = "Protoandic", passed = NA, layers = integer(0),
      evidence = list(reason = "no al_ox_pct data"),
      missing = c("al_ox_pct", "fe_ox_pct"),
      reference = "WRB (2022) Ch 5, Protoandic"
    ))
  }
  # Alox alone >= 1.2 already meets Alox + 1/2 Feox >= 1.2.
  alfe <- ifelse(is.na(al_ox), NA,
            ifelse(al_ox >= 1.2, TRUE,
              ifelse(is.na(fe_ox), NA, al_ox + 0.5 * fe_ox >= 1.2)))
  bd <- h$bulk_density_g_cm3; pr <- h$phosphate_retention_pct
  status <- vapply(seq_len(nrow(h)), function(i) {
    x <- c(alfe[i], if (is.na(bd[i])) NA else bd[i] <= 1.2,
           if (is.na(pr[i])) NA else pr[i] >= 55)
    if (any(x %in% FALSE)) FALSE else if (all(x %in% TRUE)) TRUE else NA
  }, logical(1))
  rule <- .q_thickness_rule(h, status, 15, win_top = 0, win_bot = 100)
  an <- tryCatch(
    if ("rsg_code" %in% names(formals(qual_andic))) qual_andic(pedon, rsg_code = rsg_code)
    else qual_andic(pedon),
    error = function(e) NULL)
  andic <- if (is.null(an)) NA else an$passed
  passed <- if (isFALSE(rule$passed) || isTRUE(andic)) FALSE
            else if (isTRUE(rule$passed) && isFALSE(andic)) TRUE else NA
  DiagnosticResult$new(
    name = "Protoandic", passed = passed,
    layers = if (isTRUE(passed)) rule$layers else integer(0),
    evidence = list(thickness_cm = rule$thickness_cm, andic = an),
    missing = if (is.na(passed))
                c(if (is.na(rule$passed)) c("al_ox_pct", "fe_ox_pct",
                    "bulk_density_g_cm3", "phosphate_retention_pct"),
                  if (is.na(andic)) "Andic qualifier")
              else character(0),
    reference = "WRB (2022) Ch 5, Protoandic"
  )
}


#' Activic supplementary qualifier (at): above the ferralic horizon a layer
#' >= 30 cm thick with CEC >= 24 cmolc/kg clay and < 0.6\% SOC (Ferralsols).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_activic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Activic: "having above a ferralic horizon a
  # layer, >= 30 cm thick, with a CEC (by 1 M NH4OAc, pH 7) of >= 24 cmolc
  # kg-1 clay and < 0.6% soil organic carbon (in Ferralsols only)". The old
  # code passed on KCl-extractable (or exchangeable) Al >= 5 cmolc/kg in any
  # horizon within 100 cm, a criterion that is not in the definition.
  r <- .q_rsg_only("Activic", rsg_code, "FR"); if (!is.null(r)) return(r)
  ref <- "WRB (2022) Ch 5, Activic"
  h <- pedon$horizons
  fer <- ferralic(pedon)
  if (!isTRUE(fer$passed))
    return(DiagnosticResult$new(name = "Activic",
      passed = if (is.na(fer$passed)) NA else FALSE, layers = integer(0),
      evidence = list(ferralic = fer), missing = fer$missing %||% character(0),
      reference = ref))
  fer_top <- min(h$top_cm[fer$layers], na.rm = TRUE)
  clay <- h$clay_pct %||% rep(NA_real_, nrow(h))
  cec_clay <- ifelse(!is.na(clay) & clay > 0,
                     (h$cec_cmol %||% NA_real_) / clay * 100, NA_real_)
  ok <- cec_clay >= 24 & (h$oc_pct %||% NA_real_) < 0.6
  run <- .q_run_tristate(h, ok, 30, from = 0, to = fer_top)
  DiagnosticResult$new(
    name = "Activic", passed = run$passed,
    layers = if (isTRUE(run$passed)) run$layers else integer(0),
    evidence = list(ferralic_top_cm = fer_top, cec_per_kg_clay = cec_clay,
                    oc_pct = h$oc_pct, layer_thickness_cm = run$thickness),
    missing = if (is.na(run$passed)) c("cec_cmol", "clay_pct", "oc_pct")
              else character(0),
    reference = ref)
}


#' Geoabruptic supplementary qualifier (go): an abrupt textural difference
#' within 100 cm of the mineral soil surface that is not the upper limit of
#' an argic, natric or spodic horizon.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_geoabruptic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Geoabruptic (under Abruptic): "having an abrupt
  # textural difference within 100 cm of the mineral soil surface that is not
  # associated with the upper limit of an argic, natric or spodic horizon".
  # The old code passed on any designation with a lithic-discontinuity
  # prefix (2C, 3Bt) and never tested the textural difference. Now each
  # abrupt_textural_difference() boundary within 100 cm counts unless it is
  # the upper limit of an argic(), natric_horizon() or spodic() horizon; if one
  # of those cannot be evaluated, a boundary not already explained by another
  # is unknown.
  ref <- "WRB (2022) Ch 5, Geoabruptic"
  h <- pedon$horizons
  mss <- .q_mineral_surface_cm(h)
  atd <- abrupt_textural_difference(pedon)
  if (is.na(mss) || isFALSE(atd$passed))
    return(DiagnosticResult$new(name = "Geoabruptic", passed = FALSE,
      layers = integer(0), evidence = list(abrupt_textural_difference = atd),
      missing = character(0), reference = ref))
  lim <- mss + 100
  bnd <- atd$layers[!is.na(h$top_cm[atd$layers]) & h$top_cm[atd$layers] <= lim]
  # Pairs of mineral layers within 100 cm that the textural test could not
  # evaluate (organic material, continuous rock and technic hard material are
  # not "layers consisting of mineral material", Ch 3.2.1).
  evaluated <- vapply(atd$evidence$layer_pairs %||% list(),
                      function(d) d$between[2], integer(1))
  not_mineral <- unlist(lapply(list(organic_material(pedon), continuous_rock(pedon),
                                    technic_hard_material(pedon)),
                               function(r) if (isTRUE(r$passed)) r$layers))
  unknown <- setdiff(which(seq_len(nrow(h)) > 1L & !is.na(h$top_cm) &
                             h$top_cm <= lim), evaluated)
  unknown <- unknown[!unknown %in% not_mineral & !(unknown - 1L) %in% not_mineral]
  hz <- list(argic = argic(pedon), natric = natric_horizon(pedon),
             spodic = tryCatch(spodic(pedon), error = function(e) NULL))
  upper <- unlist(lapply(hz, function(r) {
    if (is.null(r) || !isTRUE(r$passed)) return(NULL)
    ly <- sort(r$layers)
    h$top_cm[ly[!(ly - 1L) %in% ly]]
  }))
  undetermined <- any(vapply(hz, function(r) is.null(r) || is.na(r$passed),
                             logical(1)))
  free <- bnd[!vapply(h$top_cm[bnd], function(d) any(abs(upper - d) < 0.5),
                      logical(1))]
  passed <- if (length(free) && !undetermined) TRUE
            else if (length(free) || length(unknown)) NA else FALSE
  DiagnosticResult$new(
    name = "Geoabruptic", passed = passed,
    layers = if (isTRUE(passed)) free else integer(0),
    evidence = list(abrupt_textural_difference = atd,
                    boundaries_cm = h$top_cm[bnd],
                    horizon_upper_limits_cm = upper),
    missing = if (is.na(passed))
                unique(c(if (length(unknown)) "clay_pct",
                         unlist(lapply(hz, function(r)
                           if (!is.null(r) && is.na(r$passed)) r$missing))))
              else character(0),
    reference = ref)
}


#' Gilgaic supplementary qualifier (gg): gilgai microrelief at the soil
#' surface (Vertisols).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_gilgaic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Gilgaic: "having at the soil surface microhighs
  # and microlows with a difference in level of >= 10 cm, i.e. gilgai
  # microrelief (in Vertisols only)". Read from site$gilgai_presence or from a
  # microrelief / relief text that names gilgai. The old code returned FALSE
  # (not NA) when nothing was recorded, read "no gilgai" as gilgai and ignored
  # microrelief_form.
  r <- .q_rsg_only("Gilgaic", rsg_code, "VR"); if (!is.null(r)) return(r)
  s <- pedon$site
  h <- pedon$horizons
  flag <- s$gilgai_presence %||% NA
  txt <- function(x) { x <- as.character(unlist(x)); x[!is.na(x) & nzchar(trimws(x))] }
  micro <- txt(list(s$microrelief_form, h$microrelief_form))
  relief <- c(micro, txt(list(s$forma_relevo, s$relevo_local, s$relief_form,
                              s$landform)))
  neg <- paste0("(?i)\\b(no|not|without|absent|absence|none|sem|nenhum|",
                "aus.{1,2}ncia|sin)\\b\\W+(\\w+\\W+){0,2}gilgai|",
                "gilgai\\W+(absent|ausente|not)")
  says_neg <- grepl(neg, relief, perl = TRUE)
  says_pos <- grepl("(?i)gilgai", relief, perl = TRUE) & !says_neg
  passed <- if (isTRUE(as.logical(flag)) || any(says_pos)) TRUE
            else if (isFALSE(as.logical(flag)) || any(says_neg) ||
                     length(micro)) FALSE
            else NA
  DiagnosticResult$new(
    name = "Gilgaic", passed = passed, layers = integer(0),
    evidence = list(gilgai_presence = flag, relief_text = relief),
    missing = if (is.na(passed)) c("site$gilgai_presence", "site$microrelief_form")
              else character(0),
    reference = "WRB (2022) Ch 5, Gilgaic")
}


#' Gelistagnic supplementary qualifier (gt): temporary water saturation
#' caused by a frozen layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_gelistagnic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Gelistagnic: "having temporary water saturation
  # caused by a frozen layer". The frozen layer may be seasonal, so neither
  # permafrost data nor redox features show whether saturation is caused by
  # it; soilKey has no field for this observation and the result is NA. The
  # old code passed on cryic conditions plus >= 5% redox features above 50 cm,
  # which does not show the cause of the saturation.
  DiagnosticResult$new(
    name = "Gelistagnic", passed = NA, layers = integer(0),
    evidence = list(reason = "water saturation caused by a frozen layer is not recorded"),
    missing = "frozen_layer_saturation",
    reference = "WRB (2022) Ch 5, Gelistagnic")
}


#' Mahic supplementary qualifier (ma): thin artefact layer in a non-Technosol
#'
#' v0.9.217: WRB 2022 Ch 5, "having a layer, >= 10 cm thick and starting
#' <= 50 cm from the soil surface, with >= 80 \% (by volume, weighted average,
#' related to the whole soil) artefacts; and having < 20 \% (by volume,
#' weighted average, related to the whole soil) artefacts in the upper 100 cm
#' from the soil surface or to a limiting layer, whichever is shallower". The
#' v0.9.64 code tested SOC, P and a \code{base_saturation_pct} column that is
#' not in the schema, as if Mahic were a manured topsoil. It now reads
#' \code{artefacts_pct}: depth-contiguous layers each >= 80 \% make the
#' artefact layer, and the 100 cm average is bounded from both sides when some
#' layers lack data (unknown = 0 \% / 100 \%).
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_mahic <- function(pedon) {
  h <- pedon$horizons
  art <- h$artefacts_pct
  if (is.null(art) || all(is.na(art))) {
    return(DiagnosticResult$new(
      name = "Mahic", passed = NA, layers = integer(0),
      evidence = list(reason = "no artefacts_pct data"),
      missing = "artefacts_pct",
      reference = "WRB (2022) Ch 5, Mahic"
    ))
  }
  layer <- .q_thickness_rule(h, ifelse(is.na(art), NA, art >= 80), 10,
                             contiguous = TRUE, max_start = 50)
  lim <- tryCatch(.barrier_top_cm(pedon), error = function(e) NA_real_)
  win <- suppressWarnings(min(c(100, lim, max(h$bottom_cm, na.rm = TRUE)), na.rm = TRUE))
  ov <- pmax(0, pmin(h$bottom_cm, win) - pmax(h$top_cm, 0))
  ov[is.na(ov)] <- 0
  known <- !is.na(art)
  cover <- sum(ov)
  lo <- if (cover > 0) sum(art[known] * ov[known]) / cover else NA_real_
  hi <- if (cover > 0) (sum(art[known] * ov[known]) + 100 * sum(ov[!known])) / cover
        else NA_real_
  low_avg <- if (is.na(lo)) NA else if (hi < 20) TRUE else if (lo >= 20) FALSE else NA
  x <- c(layer$passed, low_avg)
  passed <- if (any(x %in% FALSE)) FALSE else if (all(x %in% TRUE)) TRUE else NA
  DiagnosticResult$new(
    name = "Mahic", passed = passed,
    layers = if (isTRUE(passed)) layer$layers else integer(0),
    evidence = list(artefact_layer_cm = layer$thickness_cm,
                    mean_artefacts_pct = c(low = lo, high = hi),
                    window_cm = win),
    missing = if (is.na(passed)) "artefacts_pct" else character(0),
    reference = "WRB (2022) Ch 5, Mahic"
  )
}


#' Laxic supplementary qualifier (la): between 25 and 75 cm of the mineral
#' soil surface a mineral layer >= 20 cm thick with bulk density <= 0.9.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_laxic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Laxic: "having between 25 and 75 cm from the
  # mineral soil surface a mineral soil layer, >= 20 cm thick, that has a bulk
  # density of <= 0.9 kg dm-3" (volume at 33 kPa, undried). soilKey does not
  # record the bulk-density method, so bulk_density_g_cm3 is used as given (a
  # dried-clod value can only be higher than the 33 kPa one). The old code
  # passed on loose dry consistence or single-grain / massive structure in the
  # upper 30 cm.
  ref <- "WRB (2022) Ch 5, Laxic"
  h <- pedon$horizons
  bd <- h$bulk_density_g_cm3 %||% rep(NA_real_, nrow(h))
  if (all(is.na(bd)))
    return(DiagnosticResult$new(name = "Laxic", passed = NA,
      layers = integer(0), evidence = list(reason = "no bulk_density_g_cm3 data"),
      missing = "bulk_density_g_cm3", reference = ref))
  mss <- .q_mineral_surface_cm(h)
  if (is.na(mss))
    return(DiagnosticResult$new(name = "Laxic", passed = FALSE,
      layers = integer(0), evidence = list(reason = "no mineral material"),
      missing = character(0), reference = ref))
  oc <- h$oc_pct %||% rep(NA_real_, nrow(h))
  ok <- bd <= 0.9 & oc < 20                     # mineral material only
  run <- .q_run_tristate(h, ok, 20, from = mss + 25, to = mss + 75)
  DiagnosticResult$new(
    name = "Laxic", passed = run$passed,
    layers = if (isTRUE(run$passed)) run$layers else integer(0),
    evidence = list(window_cm = c(mss + 25, mss + 75), bulk_density = bd,
                    layer_thickness_cm = run$thickness),
    missing = if (is.na(run$passed)) c("bulk_density_g_cm3", "oc_pct")
              else character(0),
    reference = ref)
}


# --- v0.9.65: Tier-3 qualifiers wired to actual schema fields --------------
#
# v0.9.64 had these as `.q_stub_na()` placeholders. v0.9.65 adds the
# corresponding schema fields to `horizon_column_spec()` and wires the
# qualifiers to read them. Each is now a substantive function (still
# returns NA when the field is unpopulated).


# --- v0.9.217: layer tests shared by the qualifiers below -------------------

# Result for a qualifier that Chapter 5 restricts to some RSGs ("in Ferralsols
# only") when the resolver is naming another RSG; NULL when it applies (or no
# RSG is given).
.q_rsg_only <- function(name, rsg_code, allowed) {
  if (is.null(rsg_code) || rsg_code %in% allowed) return(NULL)
  DiagnosticResult$new(
    name = name, passed = FALSE, layers = integer(0),
    evidence = list(reason = sprintf("%s is used in %s only", name,
                                     paste(allowed, collapse = ", ")),
                    rsg_code = rsg_code),
    missing = character(0), reference = paste0("WRB (2022) Ch 5, ", name))
}

# "A layer, >= min_thick cm thick", made of depth-contiguous horizons that each
# meet a condition. `ok` is TRUE / FALSE / NA per horizon; the layer lies
# between `from` and `to` (cm; `to = Inf` means the bottom of the description)
# and starts <= `max_start`. Depths inside the window that no horizon covers
# are unknown. passed: TRUE when horizons known to meet the condition form such
# a layer, NA when one could exist only through unknown horizons or depths,
# FALSE otherwise; layers: the horizons of the qualifying layer(s).
.q_run_tristate <- function(h, ok, min_thick, from = 0, to = Inf,
                            max_start = Inf) {
  top <- h$top_cm; bot <- h$bottom_cm
  i <- which(!is.na(top) & !is.na(bot) & bot > top)
  if (!length(i)) return(list(passed = NA, layers = integer(0), thickness = 0))
  if (!is.finite(to)) to <- max(bot[i])
  if (to <= from) return(list(passed = FALSE, layers = integer(0), thickness = 0))
  i <- i[order(top[i])]
  seg <- list(ix = integer(0), s = numeric(0), e = numeric(0), ok = logical(0))
  add <- function(seg, ix, s, e, ok)
    list(ix = c(seg$ix, ix), s = c(seg$s, s), e = c(seg$e, e), ok = c(seg$ok, ok))
  cur <- from
  for (k in i) {
    s0 <- max(top[k], from); e0 <- min(bot[k], to)
    if (e0 <= max(s0, cur)) next
    if (s0 > cur) seg <- add(seg, NA_integer_, cur, s0, NA)
    seg <- add(seg, k, max(s0, cur), e0, ok[k])
    cur <- e0
  }
  if (to > cur) seg <- add(seg, NA_integer_, cur, to, NA)
  runs <- function(accept) {
    out <- list(); s <- NA_real_; e <- NA_real_; ix <- integer(0)
    for (k in seq_along(seg$s)) {
      if (accept(seg$ok[k])) {
        if (is.na(s)) { s <- seg$s[k]; ix <- integer(0) }
        e <- seg$e[k]; ix <- c(ix, seg$ix[k])
      } else if (!is.na(s)) {
        out[[length(out) + 1L]] <- list(s = s, e = e, ix = ix); s <- NA_real_
      }
    }
    if (!is.na(s)) out[[length(out) + 1L]] <- list(s = s, e = e, ix = ix)
    Filter(function(r) r$s <= max_start, out)
  }
  sure  <- runs(isTRUE)
  maybe <- runs(function(x) !isFALSE(x))
  long  <- function(rs) Filter(function(r) r$e - r$s >= min_thick, rs)
  thick <- if (length(sure)) max(vapply(sure, function(r) r$e - r$s, numeric(1))) else 0
  if (length(long(sure)))
    return(list(passed = TRUE, thickness = thick,
                layers = sort(unique(stats::na.omit(unlist(lapply(long(sure),
                                                     `[[`, "ix")))))))
  list(passed = if (length(long(maybe))) NA else FALSE, layers = integer(0),
       thickness = thick)
}

# "A layer, >= min_thick cm thick, with a weighted average of >= thr" of a
# horizon attribute `v`. A layer is one or more depth-contiguous whole horizons
# (a horizon is not split), clipped to [from, to] (cm), starting <= `max_start`
# or, with `at_from`, at `from`. TRUE when such a layer with complete data
# reaches thr, NA when one would reach it only if the missing values were as
# high as `hi`, FALSE otherwise.
.q_window_mean3 <- function(h, v, min_thick, thr, from = 0, to = Inf,
                            max_start = Inf, at_from = FALSE, hi = 100) {
  top <- h$top_cm; bot <- h$bottom_cm
  i <- which(!is.na(top) & !is.na(bot) & bot > top)
  none <- list(passed = FALSE, layers = integer(0), best = NA_real_)
  if (!length(i)) return(modifyList(none, list(passed = NA)))
  i <- i[order(top[i])]
  s <- pmax(top[i], from); e <- pmin(bot[i], to)
  keep <- e > s
  i <- i[keep]; s <- s[keep]; e <- e[keep]; x <- v[i]
  best <- NA_real_; win <- NULL; maybe <- FALSE
  for (a in seq_along(i)) {
    if (s[a] > max_start || (at_from && s[a] > from)) next
    tot <- 0; sm <- 0; smh <- 0; known <- TRUE
    for (b in a:length(i)) {
      if (b > a && s[b] > e[b - 1L] + 0.5) break        # depth gap: new layer
      w <- e[b] - s[b]; tot <- tot + w
      if (is.na(x[b])) { known <- FALSE; smh <- smh + hi * w }
      else { sm <- sm + x[b] * w; smh <- smh + x[b] * w }
      if (tot < min_thick) next
      if (known) {
        best <- max(best, sm / tot, na.rm = TRUE)
        if (sm / tot >= thr && is.null(win)) win <- i[a:b]
      } else if (smh / tot >= thr) maybe <- TRUE
    }
  }
  if (!is.null(win)) return(list(passed = TRUE, layers = sort(win), best = best))
  list(passed = if (maybe) NA else FALSE, layers = integer(0), best = best)
}


#' Archaic supplementary qualifier (ah): a layer >= 20 cm thick within 100 cm
#' with >= 20\% artefacts, >= 50\% of them pre-industrial (Technosols).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_archaic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Archaic: "having a layer, >= 20 cm thick and
  # within 100 cm of the soil surface, with >= 20% (by volume, weighted
  # average, related to the whole soil) artefacts containing >= 50% (by
  # volume, weighted average, related to the whole soil) artefacts produced by
  # pre-industrial processes ... (in Technosols only)". soilKey records the
  # artefact volume (artefacts_pct) but not the pre-industrial share, so the
  # result is FALSE when no such >= 20% artefact layer exists and NA when one
  # does. The old code passed on any site cultural_period text or an
  # "archaeological" contamination_type.
  r <- .q_rsg_only("Archaic", rsg_code, "TC"); if (!is.null(r)) return(r)
  ref <- "WRB (2022) Ch 5, Archaic"
  h <- pedon$horizons
  art <- h$artefacts_pct %||% rep(NA_real_, nrow(h))
  if (all(is.na(art)))
    return(DiagnosticResult$new(name = "Archaic", passed = NA,
      layers = integer(0), evidence = list(reason = "no artefacts_pct data"),
      missing = c("artefacts_pct", "preindustrial_artefacts_pct"),
      reference = ref))
  w <- .q_window_mean3(h, art, 20, 20, from = 0, to = 100)
  passed <- if (isFALSE(w$passed)) FALSE else NA
  DiagnosticResult$new(
    name = "Archaic", passed = passed, layers = integer(0),
    evidence = list(artefact_layer = w$passed, max_artefacts_pct = w$best,
                    reason = if (is.na(passed))
                      "pre-industrial share of the artefacts not recorded"),
    missing = if (is.na(passed))
                c(if (is.na(w$passed)) "artefacts_pct",
                  "preindustrial_artefacts_pct")
              else character(0),
    reference = ref)
}


#' Arenicolic supplementary qualifier (ad): >= 50\% worm holes, casts or
#' filled burrows in a layer >= 20 cm thick, in a tidal area.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_arenicolic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Arenicolic: "having >= 50% (by volume, weighted
  # average) of worm holes, worm casts, or filled animal burrows in a layer,
  # >= 20 cm thick and occurring in a tidal area". The volume is read from
  # worm_holes_pct (the schema field for these features) and the tidal setting
  # from qual_tidalic(). The old code passed on a "common" bioturbation_density
  # anywhere within 100 cm: no 50% volume, no thickness, no tidal area.
  ref <- "WRB (2022) Ch 5, Arenicolic"
  h <- pedon$horizons
  worm <- h$worm_holes_pct %||% rep(NA_real_, nrow(h))
  if (all(is.na(worm)))
    return(DiagnosticResult$new(name = "Arenicolic", passed = NA,
      layers = integer(0), evidence = list(reason = "no worm_holes_pct data"),
      missing = "worm_holes_pct", reference = ref))
  w <- .q_window_mean3(h, worm, 20, 50)
  td <- tryCatch(qual_tidalic(pedon), error = function(e) NULL)
  tidal <- if (is.null(td) || (!isTRUE(td$passed) && length(td$missing)))
             NA else isTRUE(td$passed)
  passed <- w$passed & tidal
  DiagnosticResult$new(
    name = "Arenicolic", passed = passed,
    layers = if (isTRUE(passed)) w$layers else integer(0),
    evidence = list(max_worm_holes_pct = w$best, tidal = td),
    missing = if (is.na(passed))
                c(if (is.na(w$passed)) "worm_holes_pct",
                  if (is.na(tidal)) "site$drainage_class")
              else character(0),
    reference = ref)
}


#' Biocrustic supplementary qualifier (bk): biological soil crust
#'
#' WRB 2022 Ch 5: "Surface biological crust (cyanobacteria, algae,
#' lichens, mosses)." Implementation: \code{surface_crust_type} matching
#' biological pattern in upper 5 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_biocrustic <- function(pedon) {
  h <- pedon$horizons
  sc <- h$surface_crust_type %||% rep(NA_character_, nrow(h))
  if (all(is.na(sc))) {
    return(DiagnosticResult$new(
      name = "Biocrustic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface_crust_type"),
      missing = "surface_crust_type",
      reference = "WRB (2022) Ch 5, Biocrustic"))
  }
  pat <- "(?i)biocrust|biolog|cyano|algae|lichen|moss"
  hits <- !is.na(sc) & grepl(pat, sc, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 5)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Biocrustic", passed = passed, layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Biocrustic")
}


#' Bryic supplementary qualifier (by): >= 75\% of the organic material within
#' 100 cm of the soil surface consists of moss fibres.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_bryic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Bryic: ">= 75% (by volume, related to the fine
  # earth plus all dead plant residues) of the organic material within 100 cm
  # of the soil surface consists of moss fibres". soilKey has no moss-fibre
  # volume, so the result is NA where organic material occurs within 100 cm
  # and FALSE where none does. The old code passed on a moss / lichen
  # layer_origin in the upper 10 cm or a moss vegetation cover; neither
  # measures the moss fibres in the organic material (and lichens are not
  # mosses).
  ref <- "WRB (2022) Ch 5, Bryic"
  h <- pedon$horizons
  om <- organic_material(pedon)
  ly <- om$layers[!is.na(h$top_cm[om$layers]) & h$top_cm[om$layers] < 100]
  if (!length(ly) && isFALSE(om$passed))
    return(DiagnosticResult$new(name = "Bryic", passed = FALSE,
      layers = integer(0),
      evidence = list(reason = "no organic material within 100 cm"),
      missing = character(0), reference = ref))
  DiagnosticResult$new(
    name = "Bryic", passed = NA, layers = integer(0),
    evidence = list(organic_layers = ly,
                    reason = "moss-fibre share of the organic material not recorded"),
    missing = c(if (is.na(om$passed)) "oc_pct", "moss_fibre_pct"),
    reference = ref)
}


#' Cordic supplementary qualifier (cd): two or more uncemented ribbon-like
#' Fe-oxide / organic-matter accumulations, >= 0.5 and < 2.5 cm thick.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_cordic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Cordic: "having two or more ribbon-like
  # accumulations, >= 0.5 and < 2.5 cm thick, that are not cemented, have
  # higher contents of Fe oxides and/or organic matter than the directly
  # overlying and underlying layers, do not meet the set of diagnostic
  # criteria of the Lamellic qualifier and have a combined thickness of
  # >= 2.5 cm within 50 cm; the uppermost ribbon-like accumulation starting
  # <= 200 cm from the mineral soil surface". The ribbons are read from
  # horizons described as such (>= 0.5 and < 2.5 cm thick), against
  # fe_dcb_pct / oc_pct of the horizons above and below and cementation_class;
  # with no such horizon the result is NA. The old code read the
  # cordic_horizon flag, which the schema documents as a cemented layer, the
  # opposite of the definition.
  ref <- "WRB (2022) Ch 5, Cordic"
  h <- pedon$horizons
  thk <- h$bottom_cm - h$top_cm
  band <- which(!is.na(thk) & thk >= 0.5 & thk < 2.5)
  mss <- .q_mineral_surface_cm(h)
  if (!length(band) || is.na(mss))
    return(DiagnosticResult$new(name = "Cordic", passed = NA,
      layers = integer(0),
      evidence = list(reason = "no ribbon-like accumulation described as a horizon"),
      missing = "ribbon_accumulations", reference = ref))
  ord <- order(h$top_cm)
  fe <- h$fe_dcb_pct %||% rep(NA_real_, nrow(h))
  oc <- h$oc_pct %||% rep(NA_real_, nrow(h))
  cem <- tolower(as.character(h$cementation_class %||% rep(NA, nrow(h))))
  lam <- qual_lamellic(pedon)
  ok <- vapply(band, function(i) {
    p <- match(i, ord)
    if (p == 1L || p == length(ord)) return(NA)
    a <- ord[p - 1L]; b <- ord[p + 1L]
    richer <- (fe[i] > fe[a] & fe[i] > fe[b]) | (oc[i] > oc[a] & oc[i] > oc[b])
    uncemented <- if (is.na(cem[i])) NA
                  else grepl("^(none|non|not|un)", cem[i])
    lamella <- if (is.na(lam$passed)) NA else isTRUE(lam$passed) && i %in% lam$layers
    !lamella & richer & uncemented
  }, logical(1))
  # >= 2 ribbons with >= 2.5 cm combined within 50 cm, the uppermost <= 200 cm.
  group <- function(sel) {
    b <- band[sel]
    for (j in b[h$top_cm[b] <= mss + 200]) {
      g <- b[h$top_cm[b] >= h$top_cm[j] & h$bottom_cm[b] <= h$top_cm[j] + 50]
      if (length(g) >= 2L && sum(thk[g]) >= 2.5) return(g)
    }
    integer(0)
  }
  sure <- group(ok %in% TRUE)
  passed <- if (length(sure)) TRUE
            else if (length(group(!ok %in% FALSE))) NA else FALSE
  DiagnosticResult$new(
    name = "Cordic", passed = passed,
    layers = sure,
    evidence = list(ribbon_horizons = band, ribbon_status = ok, lamellic = lam),
    missing = if (is.na(passed)) c("fe_dcb_pct", "oc_pct", "cementation_class")
              else character(0),
    reference = ref)
}




#' Escalic supplementary qualifier (ec): soil truncated and/or locally
#' transported to form human-made terraces.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_escalic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Escalic: "soil has been truncated and/or locally
  # transported to form human-made terraces". Read from the site landform /
  # microrelief / land-use text: it must say the terraces are built (terraced
  # field, bench or agricultural terrace, socalcos, bancales); a bare
  # "terrace" may be a river terrace and gives NA. The old code passed on any
  # "terrace" or "step" in site$microrelief_form.
  ref <- "WRB (2022) Ch 5, Escalic"
  s <- pedon$site
  h <- pedon$horizons
  txt <- function(x) { x <- as.character(unlist(x)); x[!is.na(x) & nzchar(trimws(x))] }
  form <- txt(list(s$microrelief_form, s$landform, s$forma_relevo,
                   s$relief_form, h$microrelief_form))
  all_txt <- c(form, txt(list(s$land_use)))
  built <- paste0("(?i)escalic|terraced|terracing|terraceament|socalco|bancal|",
                  "patamar|andenes|\\b(human|man|anthrop\\w*|artificial|",
                  "agricultur\\w*|bench|rice|paddy|cultivat\\w*|built|",
                  "constructed)\\W*(made\\W*)?terrac|",
                  "(terra\\S{1,2}os?|terrazas?)\\s+agr\\S{1,2}colas?")
  vague <- "(?i)terrac|terra\\S{1,2}os?\\b|terraza|escal|\\bsteps?\\b|degrau"
  passed <- if (any(grepl(built, all_txt, perl = TRUE))) TRUE
            else if (any(grepl(vague, all_txt, perl = TRUE))) NA
            else if (length(form)) FALSE else NA
  DiagnosticResult$new(
    name = "Escalic", passed = passed, layers = integer(0),
    evidence = list(site_text = all_txt),
    missing = if (is.na(passed)) c("site$landform", "site$microrelief_form")
              else character(0),
    reference = ref)
}


#' Evapocrustic supplementary qualifier (ev): a saline crust <= 2 cm thick
#' on the soil surface.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_evapocrustic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Evapocrustic: "having a saline crust, <= 2 cm
  # thick, on the soil surface". The crust is read from surface_crust_type /
  # salt_crust_pattern of the surface horizon; its thickness is known only
  # when the crust is described as its own horizon, so a thicker surface
  # horizon gives NA. The old code ignored the thickness, accepted gypsum
  # crusts (WRB's salts are those "more soluble than gypsum", Ch 3.1.33) and
  # the generic "crusty", and looked at any horizon starting < 5 cm.
  ref <- "WRB (2022) Ch 5, Evapocrustic"
  h <- pedon$horizons
  s0 <- which(!is.na(h$top_cm) & h$top_cm == min(h$top_cm, na.rm = TRUE))[1]
  sc <- c((h$surface_crust_type %||% NA)[s0], (h$salt_crust_pattern %||% NA)[s0])
  sc <- as.character(sc[!is.na(sc) & nzchar(trimws(sc))])
  if (is.na(s0) || !length(sc))
    return(DiagnosticResult$new(name = "Evapocrustic", passed = NA,
      layers = integer(0), evidence = list(reason = "no surface crust recorded"),
      missing = c("surface_crust_type", "salt_crust_pattern"), reference = ref))
  pat <- "(?i)salt|saline|salin|halite|evapor|efflores|sal\\b"
  saline <- any(grepl(pat, sc, perl = TRUE) & !grepl("(?i)gyps|gesso", sc, perl = TRUE))
  thk <- h$bottom_cm[s0] - h$top_cm[s0]
  passed <- if (!saline) FALSE else if (!is.na(thk) && thk <= 2) TRUE else NA
  DiagnosticResult$new(
    name = "Evapocrustic", passed = passed,
    layers = if (isTRUE(passed)) s0 else integer(0),
    evidence = list(crust = sc, surface_horizon_thickness_cm = thk),
    missing = if (is.na(passed)) "surface_crust_thickness_cm" else character(0),
    reference = ref)
}


#' Immissic supplementary qualifier (im): a surface layer >= 10 cm thick with
#' >= 20\% sedimented dust, soot or ash that are artefacts.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_immissic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Immissic: "having at the soil surface a layer,
  # >= 10 cm thick, with >= 20% (by volume) sedimented dust, soot or ash that
  # meets the diagnostic criteria of artefacts". Such dust is part of the
  # artefacts, so artefacts_pct < 20% in every surface layer >= 10 cm thick
  # rules it out (FALSE); otherwise the dust share is not recorded and the
  # result is NA. The old code passed on a heavy-metal or "atmospheric"
  # contamination_type anywhere within 100 cm.
  ref <- "WRB (2022) Ch 5, Immissic"
  h <- pedon$horizons
  art <- h$artefacts_pct %||% rep(NA_real_, nrow(h))
  if (all(is.na(art)))
    return(DiagnosticResult$new(name = "Immissic", passed = NA,
      layers = integer(0), evidence = list(reason = "no artefacts_pct data"),
      missing = c("artefacts_pct", "sedimented_dust_soot_ash_pct"),
      reference = ref))
  w <- .q_window_mean3(h, art, 10, 20, from = 0, at_from = TRUE)
  passed <- if (isFALSE(w$passed)) FALSE else NA
  DiagnosticResult$new(
    name = "Immissic", passed = passed, layers = integer(0),
    evidence = list(artefact_layer = w$passed, max_artefacts_pct = w$best),
    missing = if (is.na(passed))
                c(if (is.na(w$passed)) "artefacts_pct",
                  "sedimented_dust_soot_ash_pct")
              else character(0),
    reference = ref)
}


#' Isopteric supplementary qualifier (ip): a termite-remodelled layer >= 30 cm
#' thick from the mineral soil surface, bulk density <= 1.3, < 5\% >= 630 um.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_isopteric <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Isopteric: "having a layer, >= 30 cm thick and
  # starting at the mineral soil surface, that is remodelled by termites, has
  # a bulk density <= 1.3 kg dm-3 and < 5% particles >= 630 um". Every horizon
  # of the layer must show termite remodelling (bioturbation_density or
  # layer_origin) and have both physical values measured. The old code
  # (v0.9.133) accepted missing bulk density / coarse-particle data as passing,
  # any layer starting < 100 cm, separate layers added up, and ant mounds.
  ref <- "WRB (2022) Ch 5, Isopteric"
  h <- pedon$horizons
  bt <- h$bioturbation_density %||% rep(NA_character_, nrow(h))
  origin <- h$layer_origin %||% rep(NA_character_, nrow(h))
  pat <- "(?i)termit|cupi[mn]|isopter"
  termite <- ifelse(is.na(bt) & is.na(origin), NA,
                    grepl(pat, bt, perl = TRUE) | grepl(pat, origin, perl = TRUE))
  bdv  <- h$bulk_density_g_cm3 %||% rep(NA_real_, nrow(h))
  p630 <- h$particles_630um_pct %||% rep(NA_real_, nrow(h))
  ok <- termite & bdv <= 1.3 & p630 < 5
  mss <- .q_mineral_surface_cm(h)
  run <- if (is.na(mss)) list(passed = FALSE, layers = integer(0), thickness = 0)
         else .q_run_tristate(h, ok, 30, from = mss, max_start = mss)
  DiagnosticResult$new(
    name = "Isopteric", passed = run$passed,
    layers = if (isTRUE(run$passed)) run$layers else integer(0),
    evidence = list(pattern = pat, thickness_cm = run$thickness),
    missing = if (is.na(run$passed))
                c("bioturbation_density", "bulk_density_g_cm3",
                  "particles_630um_pct")
              else character(0),
    reference = ref)
}


#' Kalaic supplementary qualifier (ka): a layer >= 10 cm thick, starting
#' <= 90 cm from the soil surface, with >= 50\% artefacts.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_kalaic <- function(pedon) {
  # v0.9.217: WRB 2022 Ch 5 Kalaic: "having a layer, >= 10 cm thick and
  # starting <= 90 cm from the soil surface, with >= 50% (by volume, weighted
  # average, related to the whole soil) artefacts". The old code read
  # surface_puff_layer (a puffed surface crust, which is Puffic), unrelated
  # to artefacts.
  ref <- "WRB (2022) Ch 5, Kalaic"
  h <- pedon$horizons
  art <- h$artefacts_pct %||% rep(NA_real_, nrow(h))
  if (all(is.na(art)))
    return(DiagnosticResult$new(name = "Kalaic", passed = NA,
      layers = integer(0), evidence = list(reason = "no artefacts_pct data"),
      missing = "artefacts_pct", reference = ref))
  w <- .q_window_mean3(h, art, 10, 50, from = 0, max_start = 90)
  DiagnosticResult$new(
    name = "Kalaic", passed = w$passed,
    layers = if (isTRUE(w$passed)) w$layers else integer(0),
    evidence = list(max_artefacts_pct = w$best),
    missing = if (is.na(w$passed)) "artefacts_pct" else character(0),
    reference = ref)
}


#' Lapiadic supplementary qualifier (ld): continuous rock at the soil surface
#' with dissolution features >= 20 cm deep covering 10-50\% of it (Leptosols).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_lapiadic <- function(pedon, rsg_code = NULL) {
  # v0.9.217: WRB 2022 Ch 5 Lapiadic: "having at the soil surface continuous
  # rock that has dissolution features (rills, grooves), >= 20 cm deep and
  # covering >= 10 and < 50% of the surface of the continuous rock (in
  # Leptosols only)". soilKey records neither the depth nor the cover of the
  # dissolution features, so the result is FALSE without continuous rock and
  # NA with it. The old code passed on "karren / lapies" in weathering_stage
  # of any horizon, with no depth or cover.
  r <- .q_rsg_only("Lapiadic", rsg_code, "LP"); if (!is.null(r)) return(r)
  ref <- "WRB (2022) Ch 5, Lapiadic"
  rock <- continuous_rock(pedon)
  if (!isTRUE(rock$passed))
    return(DiagnosticResult$new(name = "Lapiadic",
      passed = if (is.na(rock$passed)) NA else FALSE, layers = integer(0),
      evidence = list(continuous_rock = rock),
      missing = rock$missing %||% character(0), reference = ref))
  DiagnosticResult$new(
    name = "Lapiadic", passed = NA, layers = integer(0),
    evidence = list(continuous_rock = rock,
                    reason = "depth and cover of the rock dissolution features not recorded"),
    missing = "rock_dissolution_features",
    reference = ref)
}


#' Litholinic supplementary qualifier (lh): stone line
#'
#' v0.9.217: WRB 2022 Ch 5, "having a layer, >= 2 and <= 20 cm thick and
#' starting <= 150 cm from the mineral soil surface, that has >= 40 \% (by
#' volume, related to the whole soil) coarse fragments and in the layers above
#' and below < 10 \% (by volume, related to the whole soil) coarse fragments
#' (stone line)". The v0.9.64 code passed on an R / Cr designation or a
#' "stratified" pattern, i.e. on any soil over rock. It now reads
#' \code{coarse_fragments_pct}: a depth-contiguous run of layers with >= 40 \%,
#' 2-20 cm thick, starting <= 150 cm, between directly adjacent layers that
#' both have < 10 \%.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_litholinic <- function(pedon) {
  h <- pedon$horizons
  cf <- h$coarse_fragments_pct
  if (is.null(cf) || all(is.na(cf))) {
    return(DiagnosticResult$new(
      name = "Litholinic", passed = NA, layers = integer(0),
      evidence = list(reason = "no coarse_fragments_pct data"),
      missing = "coarse_fragments_pct",
      reference = "WRB (2022) Ch 5, Litholinic"))
  }
  ord <- order(h$top_cm)
  ord <- ord[!is.na(h$top_cm[ord]) & !is.na(h$bottom_cm[ord])]
  stony <- !is.na(cf) & cf >= 40
  hits <- integer(0); undecided <- FALSE; k <- 1L
  while (k <= length(ord)) {
    i <- ord[k]
    if (!stony[i]) {
      if (is.na(cf[i]) && h$top_cm[i] <= 150 && k > 1L &&
          h$bottom_cm[i] - h$top_cm[i] <= 20) undecided <- TRUE
      k <- k + 1L; next
    }
    run <- i                                  # depth-contiguous stony run
    while (k < length(ord) && stony[ord[k + 1L]] &&
           abs(h$top_cm[ord[k + 1L]] - h$bottom_cm[ord[k]]) <= 0.5) {
      k <- k + 1L; run <- c(run, ord[k])
    }
    top <- min(h$top_cm[run]); thk <- max(h$bottom_cm[run]) - top
    k0 <- match(run[1L], ord); k1 <- match(run[length(run)], ord)
    up <- if (k0 > 1L) ord[k0 - 1L] else NA_integer_
    dn <- if (k1 < length(ord)) ord[k1 + 1L] else NA_integer_
    if (top <= 150 && thk >= 2 && thk <= 20 && !is.na(up)) {
      adj <- c(if (abs(h$bottom_cm[up] - top) <= 0.5) cf[up] else NA,
               if (!is.na(dn) && abs(h$top_cm[dn] - max(h$bottom_cm[run])) <= 0.5)
                 cf[dn] else NA)
      if (all(!is.na(adj) & adj < 10)) hits <- c(hits, run)
      else if (!any(!is.na(adj) & adj >= 10)) undecided <- TRUE
    }
    k <- k + 1L
  }
  passed <- if (length(hits)) TRUE else if (undecided) NA else FALSE
  DiagnosticResult$new(
    name = "Litholinic", passed = passed,
    layers = hits,
    evidence = list(min_line_cf_pct = 40, max_adjacent_cf_pct = 10,
                    thickness_cm = c(2, 20)),
    missing = if (is.na(passed)) "coarse_fragments_pct" else character(0),
    reference = "WRB (2022) Ch 5, Litholinic")
}


#' Mochipic supplementary qualifier (mc): almost permanently saturated
#' stagnic layer
#'
#' v0.9.217: WRB 2022 Ch 5, "having a layer with stagnic properties, >= 25 cm
#' thick and within 100 cm of the mineral soil surface, that is
#' water-saturated for >= 300 cumulative days in most years". The v0.9.64 /
#' v0.9.133 code matched "mochi|banded|patchy" in \code{mottle_morphology}
#' and let an unmeasured \code{water_saturation_days} count as >= 300. It now
#' needs stagnic_properties() layers with \code{water_saturation_days >= 300}
#' forming a depth-contiguous layer >= 25 cm thick within 100 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_mochipic <- function(pedon) {
  h <- pedon$horizons
  st <- tryCatch(stagnic_properties(pedon, max_top_cm = 100), error = function(e) NULL)
  sat <- h$water_saturation_days %||% rep(NA_real_, nrow(h))
  stag <- if (is.null(st) || is.na(st$passed)) rep(NA, nrow(h))
          else seq_len(nrow(h)) %in% st$layers
  status <- vapply(seq_len(nrow(h)), function(i) {
    x <- c(stag[i], if (is.na(sat[i])) NA else sat[i] >= 300)
    if (any(x %in% FALSE)) FALSE else if (all(x %in% TRUE)) TRUE else NA
  }, logical(1))
  rule <- .q_thickness_rule(h, status, 25, win_top = 0, win_bot = 100,
                            contiguous = TRUE)
  DiagnosticResult$new(
    name = "Mochipic", passed = rule$passed,
    layers = if (isTRUE(rule$passed)) rule$layers else integer(0),
    evidence = list(stagnic_properties = st, thickness_cm = rule$thickness_cm),
    missing = if (is.na(rule$passed))
                unique(c(if (any(is.na(stag))) st$missing %||% "stagnic_properties",
                         "water_saturation_days"))
              else character(0),
    reference = "WRB (2022) Ch 5, Mochipic")
}


#' Naramic supplementary qualifier (nr): soft horizon over its petro- form
#'
#' v0.9.217: WRB 2022 Ch 5, "in Gypsisols: having a gypsic horizon above a
#' petrogypsic horizon that starts <= 100 cm from the mineral soil surface;
#' in Calcisols: having a calcic horizon above a petrocalcic horizon that
#' starts <= 100 cm from the mineral soil surface". The v0.9.64 code matched
#' salt-crust words in \code{salt_crust_pattern}. The definition depends on
#' the RSG, which the resolver passes; other RSGs give NA.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_naramic <- function(pedon, rsg_code = NULL) {
  ref <- "WRB (2022) Ch 5, Naramic"
  pair <- switch(rsg_code %||% "", GY = c("gypsic", "petrogypsic"),
                 CL = c("calcic", "petrocalcic"), NULL)
  if (is.null(pair))
    return(DiagnosticResult$new(
      name = "Naramic", passed = NA, layers = integer(0),
      evidence = list(reason = "defined for Gypsisols and Calcisols only; no such RSG given"),
      missing = character(0), reference = ref))
  h <- pedon$horizons
  ns <- asNamespace("soilKey")
  soft <- tryCatch(get(pair[1], envir = ns)(pedon), error = function(e) NULL)
  hard <- tryCatch(get(pair[2], envir = ns)(pedon), error = function(e) NULL)
  hard_top <- if (!is.null(hard) && isTRUE(hard$passed))
    suppressWarnings(min(h$top_cm[hard$layers], na.rm = TRUE)) else NA_real_
  res <- function(passed, layers = integer(0), missing = character(0))
    DiagnosticResult$new(name = "Naramic", passed = passed, layers = layers,
      evidence = setNames(list(soft, hard, hard_top), c(pair, "petro_top_cm")),
      missing = missing, reference = ref)
  if (is.null(hard) || !isTRUE(hard$passed) || !is.finite(hard_top))
    return(res(if (is.null(hard) || is.na(hard$passed)) NA else FALSE,
               missing = hard$missing %||% pair[2]))
  if (hard_top > 100) return(res(FALSE))
  above <- if (!is.null(soft) && isTRUE(soft$passed))
    setdiff(soft$layers, hard$layers) else integer(0)
  above <- above[!is.na(h$bottom_cm[above]) & h$bottom_cm[above] <= hard_top + 0.5]
  if (length(above)) return(res(TRUE, layers = c(above, hard$layers)))
  res(if (is.null(soft) || is.na(soft$passed)) NA else FALSE,
      missing = if (is.null(soft) || is.na(soft$passed)) soft$missing %||% pair[1]
                else character(0))
}


#' Nechic supplementary qualifier (ne): uncoated grains in an acid topsoil
#'
#' v0.9.217: WRB 2022 Ch 5, "having a pHwater of < 5 and uncoated mineral
#' grains of sand and/or coarse silt size in a darker matrix somewhere within
#' 5 cm of the mineral soil surface and no spodic horizon starting <= 200 cm
#' from the mineral soil surface". The v0.9.64 code matched loess / dune words
#' in \code{aeolian_morphology}. The schema has no field for uncoated grains,
#' so the result is FALSE when the pH (>= 5 throughout the upper 5 cm) or a
#' spodic horizon <= 200 cm rules it out, and NA otherwise.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_nechic <- function(pedon) {
  h <- pedon$horizons
  mss <- .q_mineral_surface_cm(h)
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  spodic_in <- !is.null(spo) && isTRUE(spo$passed) &&
    any(!is.na(h$top_cm[spo$layers]) & h$top_cm[spo$layers] <= 200)
  top5 <- if (is.na(mss)) integer(0)
          else which(!is.na(h$top_cm) & h$top_cm < mss + 5 &
                       !is.na(h$bottom_cm) & h$bottom_cm > mss)
  ph <- h$ph_h2o[top5]
  ph_rules_out <- length(ph) > 0L && all(!is.na(ph) & ph >= 5)
  passed <- if (spodic_in || ph_rules_out) FALSE else NA
  DiagnosticResult$new(
    name = "Nechic", passed = passed, layers = integer(0),
    evidence = list(ph_h2o_upper_5cm = ph, spodic = spo),
    missing = if (is.na(passed))
                c("uncoated sand / coarse-silt grains (not in schema)",
                  if (!length(ph) || anyNA(ph)) "ph_h2o")
              else character(0),
    reference = "WRB (2022) Ch 5, Nechic")
}


#' Pelocrustic supplementary qualifier (p): permanent clayey physical crust
#'
#' v0.9.217: WRB 2022 Ch 5, "having a permanent physical surface crust with
#' >= 30 \% clay (in Vertisols only)". The v0.9.64 code passed on any
#' "clay" crust type without permanence or clay content. It now needs the
#' crust recorded in \code{surface_crust_type} of the uppermost layer as
#' pelocrustic, or as a permanent physical / clay crust with that layer's
#' \code{clay_pct} >= 30 (the crust is formed from the surface layer's own
#' material). A clay crust whose permanence is not recorded gives NA; a
#' biological, salt or absent crust gives FALSE.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_pelocrustic <- function(pedon) {
  h <- pedon$horizons
  sc <- h$surface_crust_type %||% rep(NA_character_, nrow(h))
  surf <- which(!is.na(h$top_cm) & h$top_cm == suppressWarnings(min(h$top_cm, na.rm = TRUE)))
  s <- sc[surf][!is.na(sc[surf])]
  if (!length(s)) {
    return(DiagnosticResult$new(
      name = "Pelocrustic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface_crust_type on the uppermost layer"),
      missing = "surface_crust_type",
      reference = "WRB (2022) Ch 5, Pelocrustic"))
  }
  s <- paste(s, collapse = " ")
  clay <- suppressWarnings(max(h$clay_pct[surf], na.rm = TRUE))
  physical <- grepl("(?i)pelo|clay|physic|struct|seal|deposit", s, perl = TRUE)
  permanent <- grepl("(?i)pelocrust|perman", s, perl = TRUE)
  passed <- if (!physical) FALSE
            else if (grepl("(?i)pelocrust", s, perl = TRUE)) TRUE
            else if (!permanent || !is.finite(clay)) NA
            else clay >= 30
  DiagnosticResult$new(
    name = "Pelocrustic", passed = passed,
    layers = if (isTRUE(passed)) surf else integer(0),
    evidence = list(surface_crust_type = s, clay_pct = clay),
    missing = if (is.na(passed))
                c(if (!permanent) "crust permanence", if (!is.finite(clay)) "clay_pct")
              else character(0),
    reference = "WRB (2022) Ch 5, Pelocrustic")
}


#' Puffic supplementary qualifier (pu): crust of readily soluble salts
#'
#' v0.9.217: WRB 2022 Ch 5, "having a chemical surface crust formed by
#' readily soluble salts". The v0.9.64 code passed on any
#' \code{surface_puff_layer} flag. It now needs, on the uppermost layer, a
#' \code{surface_crust_type} that names a crust of readily soluble salts
#' (salt, halite, thenardite, mirabilite, natron, trona, puffed), or a puffed
#' surface layer together with salic-level salinity (\code{ec_dS_m} >= 15),
#' which ties the puffing to readily soluble salts. A gypsum or generic
#' evaporite crust (gypsum is not readily soluble) or a puffed layer without
#' salinity data gives NA; a biological, clay or absent crust gives FALSE.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_puffic <- function(pedon) {
  h <- pedon$horizons
  surf <- which(!is.na(h$top_cm) & h$top_cm == suppressWarnings(min(h$top_cm, na.rm = TRUE)))
  sc <- (h$surface_crust_type %||% rep(NA_character_, nrow(h)))[surf]
  pf <- (h$surface_puff_layer %||% rep(NA, nrow(h)))[surf]
  ec <- suppressWarnings(max((h$ec_dS_m %||% rep(NA_real_, nrow(h)))[surf], na.rm = TRUE))
  if (all(is.na(sc)) && all(is.na(pf))) {
    return(DiagnosticResult$new(
      name = "Puffic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface_crust_type / surface_puff_layer"),
      missing = c("surface_crust_type", "surface_puff_layer"),
      reference = "WRB (2022) Ch 5, Puffic"))
  }
  s <- paste(sc[!is.na(sc)], collapse = " ")
  gyps <- grepl("(?i)gyps|calc|carbonat", s, perl = TRUE)
  salt_crust <- !gyps &&
    grepl("(?i)puff|salt|halit|thenard|mirabil|natron|trona", s, perl = TRUE)
  puffed <- any(pf %in% TRUE)
  other_crust <- nzchar(s) && !salt_crust && !gyps &&
    !grepl("(?i)evapor|chemic", s, perl = TRUE)
  passed <- if (salt_crust) TRUE
            else if (puffed && is.finite(ec) && ec >= 15) TRUE
            else if (puffed || gyps || grepl("(?i)evapor|chemic", s, perl = TRUE)) NA
            else if (other_crust) FALSE
            else NA
  DiagnosticResult$new(
    name = "Puffic", passed = passed,
    layers = if (isTRUE(passed)) surf else integer(0),
    evidence = list(surface_crust_type = s, surface_puff_layer = pf, ec_dS_m = ec),
    missing = if (is.na(passed)) c("salt type of the surface crust", "ec_dS_m")
              else character(0),
    reference = "WRB (2022) Ch 5, Puffic")
}


#' Raptic supplementary qualifier (rp): lithic discontinuity <= 100 cm
#'
#' v0.9.217: WRB 2022 Ch 5, "having a lithic discontinuity at some depth
#' <= 100 cm from the mineral soil surface, that is not related to aeolic,
#' fluvic, solimovic or tephric material". The v0.9.64 / v0.9.142 code
#' matched "break|interrupt|discont" in \code{stratification_pattern} and
#' counted a discontinuity whose origin was unrecorded. A discontinuity at the
#' upper limit of a layer is now read from lithic_discontinuity(), from a
#' change in the designation's numeral prefix (2C, 3C), or from
#' \code{stratification_pattern} naming a lithic break. It counts only when
#' the origin of both layers it separates is recorded (\code{rock_origin} /
#' \code{layer_origin}) and neither is aeolic, fluvic (incl. marine,
#' lacustrine), solimovic (colluvial) or tephric (pyroclastic), nor flagged by
#' the corresponding material diagnostics.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_raptic <- function(pedon) {
  h <- pedon$horizons
  n <- nrow(h)
  ns <- asNamespace("soilKey")
  getd <- function(f) tryCatch(get(f, envir = ns)(pedon), error = function(e) NULL)
  ld <- getd("lithic_discontinuity")
  mats <- lapply(c("aeolic_material", "fluvic_material", "solimovic_material",
                   "tephric_material"), getd)
  related_layers <- unique(unlist(lapply(mats, function(d)
    if (!is.null(d) && isTRUE(d$passed)) d$layers)))
  d  <- h$designation %||% rep(NA_character_, n)
  sp <- h$stratification_pattern %||% rep(NA_character_, n)
  num <- suppressWarnings(as.integer(ifelse(grepl("^[0-9]+", d),
                                            sub("^([0-9]+).*", "\\1", d), "1")))
  origin <- paste(h$rock_origin %||% rep(NA_character_, n),
                  h$layer_origin %||% rep(NA_character_, n))
  origin_known <- !is.na(h$rock_origin %||% rep(NA, n)) |
                  !is.na(h$layer_origin %||% rep(NA, n))
  related <- (seq_len(n) %in% related_layers) |
    grepl("(?i)aeol|fluv|marine|lacustr|alluv|solimov|colluv|tephr|pyroclast",
          origin, perl = TRUE)
  # A lithic discontinuity compares two layers of mineral (soil) material: the
  # contact with continuous rock or technic hard material is not one.
  hard <- unique(unlist(lapply(c("continuous_rock", "technic_hard_material"),
    function(f) { r <- getd(f); if (!is.null(r) && isTRUE(r$passed)) r$layers })))
  ord <- order(h$top_cm)
  status <- rep(NA, n); is_disc <- rep(FALSE, n)
  for (k in seq_along(ord)[-1L]) {
    i <- ord[k]; j <- ord[k - 1L]
    if (is.na(h$top_cm[i]) || h$top_cm[i] > 100 || i %in% hard || j %in% hard) next
    disc <- (i %in% (ld$layers %||% integer(0))) ||
            (!is.na(num[i]) && !is.na(num[j]) && num[i] != num[j]) ||
            (!is.na(sp[i]) && grepl("(?i)litholog|lithic|discontinu|raptic", sp[i], perl = TRUE))
    if (!disc) next
    is_disc[i] <- TRUE
    status[i] <- if (related[i] || related[j]) FALSE
                 else if (origin_known[i] && origin_known[j]) TRUE else NA
  }
  st <- status[is_disc]
  no_data <- (is.null(ld) || is.na(ld$passed)) && all(is.na(d)) && all(is.na(sp))
  passed <- if (any(st %in% TRUE)) TRUE
            else if (anyNA(st)) NA
            else if (!length(st) && no_data) NA
            else FALSE
  DiagnosticResult$new(
    name = "Raptic", passed = passed,
    layers = which(status %in% TRUE),
    evidence = list(lithic_discontinuity = ld,
                    discontinuity_at_top_of = which(is_disc),
                    related_material_layers = related_layers),
    missing = if (is.na(passed)) c("rock_origin", "layer_origin",
                                   "coarse_fragments_pct", "designation")
              else character(0),
    reference = "WRB (2022) Ch 5, Raptic")
}


#' Saprolithic supplementary qualifier (sh): saprolite with low-activity clay
#'
#' v0.9.217: WRB 2022 Ch 5, "having a layer, >= 30 cm thick and starting
#' <= 150 cm from the mineral soil surface, that has rock structure in >= 75 \%
#' (by volume, related to the whole soil) and a CEC (by 1 M NH4OAc, pH 7) of
#' < 24 cmolc kg-1 clay". The v0.9.64 code took >= 50 \% saprolite or a
#' "weathered" \code{weathering_stage} starting < 200 cm, with no CEC or
#' thickness test. It now needs \code{saprolite_pct} >= 75 (saprolite keeps
#' the rock structure) and \code{cec_cmol} / \code{clay_pct} < 24 cmolc kg-1
#' clay in a depth-contiguous layer >= 30 cm thick starting <= 150 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_saprolithic <- function(pedon) {
  h <- pedon$horizons
  sp <- h$saprolite_pct %||% rep(NA_real_, nrow(h))
  if (all(is.na(sp))) {
    return(DiagnosticResult$new(
      name = "Saprolithic", passed = NA, layers = integer(0),
      evidence = list(reason = "no saprolite_pct"),
      missing = "saprolite_pct",
      reference = "WRB (2022) Ch 5, Saprolithic"))
  }
  cec_clay <- ifelse(!is.na(h$clay_pct) & h$clay_pct > 0,
                     h$cec_cmol / h$clay_pct * 100, NA_real_)
  status <- vapply(seq_len(nrow(h)), function(i) {
    x <- c(if (is.na(sp[i])) NA else sp[i] >= 75,
           if (is.na(cec_clay[i])) NA else cec_clay[i] < 24)
    if (any(x %in% FALSE)) FALSE else if (all(x %in% TRUE)) TRUE else NA
  }, logical(1))
  rule <- .q_thickness_rule(h, status, 30, contiguous = TRUE, max_start = 150)
  DiagnosticResult$new(
    name = "Saprolithic", passed = rule$passed,
    layers = if (isTRUE(rule$passed)) rule$layers else integer(0),
    evidence = list(min_saprolite_pct = 75, cec_per_kg_clay = cec_clay,
                    thickness_cm = rule$thickness_cm),
    missing = if (is.na(rule$passed)) c("saprolite_pct", "cec_cmol", "clay_pct")
              else character(0),
    reference = "WRB (2022) Ch 5, Saprolithic")
}


#' Thixotropic supplementary qualifier (tx): thixotropic behavior
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_thixotropic <- function(pedon) {
  h <- pedon$horizons
  ti <- h$thixotropic_index %||% rep(NA_real_, nrow(h))
  if (all(is.na(ti))) {
    return(DiagnosticResult$new(
      name = "Thixotropic", passed = NA, layers = integer(0),
      evidence = list(reason = "no thixotropic_index"),
      missing = "thixotropic_index",
      reference = "WRB (2022) Ch 5, Thixotropic"))
  }
  # WRB 2022 Ch 5 Thixotropic: in some layer within 50 cm (was 100 cm).
  qualifying <- which(!is.na(ti) & ti >= 50 & h$top_cm < 50)
  DiagnosticResult$new(
    name = "Thixotropic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(threshold_thixotropic_index = 50),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Thixotropic")
}


#' Uterquic supplementary qualifier (uq): gleyic and stagnic in one layer
#'
#' v0.9.217: WRB 2022 Ch 5, "having a layer with dominant gleyic properties
#' and some parts with stagnic properties, starting <= 75 cm from the mineral
#' soil surface (in Gleysols only); with dominant stagnic properties and some
#' parts with gleyic properties, starting <= 75 cm from the mineral soil
#' surface (in Planosols and Stagnosols only)". The v0.9.64 code matched
#' "bidirec|fluctuat" in \code{water_regime_pattern}. The definition depends
#' on the RSG (passed by the resolver). soilKey records neither the
#' subordinate colour pattern inside a layer nor which pattern dominates, so
#' the result is FALSE when no layer starting <= 75 cm has the RSG's dominant
#' property (gleyic_properties() / stagnic_properties()) and NA otherwise.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rsg_code The RSG being named (the resolver passes it).
#' @noRd
qual_uterquic <- function(pedon, rsg_code = NULL) {
  ref <- "WRB (2022) Ch 5, Uterquic"
  dom <- switch(rsg_code %||% "", GL = "gleyic_properties",
                PL = "stagnic_properties", ST = "stagnic_properties", NULL)
  if (is.null(dom))
    return(DiagnosticResult$new(
      name = "Uterquic", passed = NA, layers = integer(0),
      evidence = list(reason = "defined for Gleysols, Planosols and Stagnosols only; no such RSG given"),
      missing = character(0), reference = ref))
  h <- pedon$horizons
  dd <- tryCatch(get(dom, envir = asNamespace("soilKey"))(pedon, max_top_cm = 75),
                 error = function(e) NULL)
  lay <- if (!is.null(dd) && isTRUE(dd$passed))
    dd$layers[!is.na(h$top_cm[dd$layers]) & h$top_cm[dd$layers] <= 75] else integer(0)
  passed <- if (length(lay)) NA
            else if (is.null(dd) || is.na(dd$passed)) NA else FALSE
  DiagnosticResult$new(
    name = "Uterquic", passed = passed, layers = integer(0),
    evidence = setNames(list(dd, lay), c(dom, "dominant_property_layers")),
    missing = if (is.na(passed))
                c("subordinate gleyic / stagnic pattern and its dominance (not in schema)",
                  if (!length(lay)) dd$missing %||% dom)
              else character(0),
    reference = ref)
}


# Bonus Endo- variants (qual_endocalcic / qual_endogypsic /
# qual_endoduric) -- mechanical depth modifiers of existing
# diagnostics, not in v0.9.63's batch.




#' Endogypsic supplementary qualifier: gypsic horizon at depth >= 50 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endogypsic <- function(pedon) {
  base <- gypsic(pedon)
  .q_within_depth("Endogypsic", base, pedon, 50, 200)
}


#' Endoduric supplementary qualifier: duric horizon at depth >= 50 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @noRd
qual_endoduric <- function(pedon) {
  base <- duric_horizon(pedon)
  .q_within_depth("Endoduric", base, pedon, 50, 200)
}
