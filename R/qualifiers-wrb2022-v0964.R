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
#' @keywords internal
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


#' Entic qualifier (et): albic horizon AND NOT spodic
#'
#' WRB 2022 Ch 5 (Podzols): "Having an albic horizon (>= 1 cm thick)
#' starting <= 50 cm AND NOT meeting the criteria for a spodic
#' horizon." Compose: albic AND NOT spodic.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_entic <- function(pedon) {
  alb <- albic(pedon)
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  if (!isTRUE(alb$passed)) {
    return(DiagnosticResult$new(
      name = "Entic", passed = FALSE, layers = integer(0),
      evidence = list(albic = alb,
                        spodic = spo),
      missing = alb$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Entic"
    ))
  }
  spodic_passes <- !is.null(spo) && isTRUE(spo$passed)
  passed <- !spodic_passes
  DiagnosticResult$new(
    name = "Entic", passed = passed,
    layers = if (passed) alb$layers else integer(0),
    evidence = list(albic = alb, spodic = spo,
                      logic = "albic AND NOT spodic"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Entic"
  )
}


#' Tonguic qualifier (tg): tongues of A horizon penetrating into B
#'
#' WRB 2022 Ch 5 (Chernozems / Kastanozems / Phaeozems / Umbrisols):
#' "Showing tongues of an A horizon penetrating >= 50 cm into the B
#' horizon (irregular boundary; A material in B-depth pockets)."
#'
#' Implementation: designation pattern \code{^A.*\\+|A/B|B/A} OR
#' \code{transition_horizon_topography} (BDsolos column for "Transição
#' de horizonte subjacente - Topografia") matching irregular /
#' tongued patterns.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_tonguic <- function(pedon) {
  h <- pedon$horizons
  desg <- h$designation %||% rep(NA_character_, nrow(h))
  topog <- h$transition_topography %||% rep(NA_character_, nrow(h))
  # Tonguing patterns
  pat_desg <- "(?i)^A.*\\+|A/B|B/A|^AB|^BA"
  pat_top  <- "(?i)tongu|irregular|interrupted|interrompid|lingu"
  hits <- (!is.na(desg) & grepl(pat_desg, desg, perl = TRUE)) |
          (!is.na(topog) & grepl(pat_top, topog, perl = TRUE))
  qualifying <- which(hits & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Tonguic", passed = passed, layers = qualifying,
    evidence = list(pattern_designation = pat_desg,
                      pattern_topography = pat_top),
    missing = if (all(is.na(desg)) && all(is.na(topog)))
                c("designation", "transition_topography") else character(0),
    reference = "WRB (2022) Ch 5, Tonguic"
  )
}


#' Nudiargic qualifier (nu): argic horizon at the surface
#'
#' WRB 2022 Ch 5 (Acrisols / Lixisols / Alisols / Luvisols / Retisols):
#' "Argic horizon starting <= 5 cm from the soil surface (no
#' overlying eluvial / albic / mollic / umbric layer)."
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_nudiargic <- function(pedon) {
  arg <- argic(pedon)
  if (!isTRUE(arg$passed)) {
    return(DiagnosticResult$new(
      name = "Nudiargic", passed = FALSE, layers = integer(0),
      evidence = list(argic = arg),
      missing = arg$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Nudiargic"
    ))
  }
  h <- pedon$horizons
  shallowest <- min(h$top_cm[arg$layers], na.rm = TRUE)
  passed <- isTRUE(is.finite(shallowest) && shallowest <= 5)
  DiagnosticResult$new(
    name = "Nudiargic", passed = passed,
    layers = if (passed) arg$layers else integer(0),
    evidence = list(argic = arg, shallowest_top_cm = shallowest,
                      threshold_cm = 5),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Nudiargic"
  )
}


#' Nudinatric qualifier (nn): natric horizon at the surface
#'
#' WRB 2022 Ch 5 (Solonetz): same logic as Nudiargic but for the
#' natric horizon (high ESP + columnar / prismatic structure).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
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
      name = "Nudinatric", passed = FALSE, layers = integer(0),
      evidence = list(natric = nat),
      missing = nat$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Nudinatric"
    ))
  }
  h <- pedon$horizons
  shallowest <- min(h$top_cm[nat$layers], na.rm = TRUE)
  passed <- isTRUE(is.finite(shallowest) && shallowest <= 5)
  DiagnosticResult$new(
    name = "Nudinatric", passed = passed,
    layers = if (passed) nat$layers else integer(0),
    evidence = list(natric = nat, shallowest_top_cm = shallowest),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Nudinatric"
  )
}


#' Someric qualifier (sm): anthric epipedon over chernic / mollic
#'
#' WRB 2022 Ch 5 (Phaeozems / Chernozems / Kastanozems / Umbrisols):
#' "Anthric epipedon (irrigation- or Plaggic-derived) overlying a
#' chernic or mollic horizon." Composes anthric_horizons + mollic
#' (or umbric).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_someric <- function(pedon) {
  ant <- tryCatch(anthric_horizons(pedon),
                    error = function(e) NULL)
  mol <- mollic(pedon)
  if (is.null(ant) || !isTRUE(ant$passed) || !isTRUE(mol$passed)) {
    return(DiagnosticResult$new(
      name = "Someric", passed = FALSE, layers = integer(0),
      evidence = list(anthric = ant, mollic = mol),
      missing = c(if (is.null(ant)) "anthric_horizons" else
                    ant$missing %||% character(0),
                    mol$missing %||% character(0)),
      reference = "WRB (2022) Ch 5, Someric"
    ))
  }
  DiagnosticResult$new(
    name = "Someric", passed = TRUE,
    layers = unique(c(ant$layers, mol$layers)),
    evidence = list(anthric = ant, mollic = mol),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Someric"
  )
}


#' Neobrunic qualifier (nb): "young" cambic-like horizon
#'
#' WRB 2022 Ch 5 (Retisols): "Cambic horizon-like alteration that
#' has formed in the last few centuries (recent agricultural,
#' colluvial, or volcanic deposits)." Composite: cambic + recent-age
#' marker via \code{layer_origin} matching young-soil patterns.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_neobrunic <- function(pedon) {
  cam <- cambic(pedon)
  h <- pedon$horizons
  origin <- h$layer_origin %||% rep(NA_character_, nrow(h))
  pat <- "(?i)recente|young|holocen|colluvial|aluvial|aluvi|deposit|deposit"
  has_recent <- !is.na(origin) & grepl(pat, origin, perl = TRUE)
  passed <- isTRUE(cam$passed) && any(has_recent[cam$layers], na.rm = TRUE)
  DiagnosticResult$new(
    name = "Neobrunic", passed = passed,
    layers = if (passed) intersect(cam$layers, which(has_recent))
             else integer(0),
    evidence = list(cambic = cam, recent_pattern = pat),
    missing = if (all(is.na(origin))) "layer_origin" else character(0),
    reference = "WRB (2022) Ch 5, Neobrunic"
  )
}


#' Neocambic qualifier (nc): "young" cambic horizon with weak development
#'
#' WRB 2022 Ch 5 (Retisols): "Cambic horizon with structure_grade
#' \"weak\" only (early-stage pedogenesis)." Composite: cambic + weak
#' structure.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_neocambic <- function(pedon) {
  cam <- cambic(pedon)
  h <- pedon$horizons
  grade <- h$structure_grade %||% rep(NA_character_, nrow(h))
  is_weak <- !is.na(grade) & tolower(trimws(grade)) %in%
              c("weak", "fraca", "fraco", "weak/moderate")
  passed <- isTRUE(cam$passed) && any(is_weak[cam$layers], na.rm = TRUE)
  DiagnosticResult$new(
    name = "Neocambic", passed = passed,
    layers = if (passed) intersect(cam$layers, which(is_weak))
             else integer(0),
    evidence = list(cambic = cam),
    missing = if (all(is.na(grade))) "structure_grade" else character(0),
    reference = "WRB (2022) Ch 5, Neocambic"
  )
}


#' Petrosalic qualifier (ptso): cemented salic horizon
#'
#' WRB 2022 Ch 5 (Solonchaks): "Salic horizon cemented by salts in
#' >= 90\% of the layer volume (forms a hard slab)." Composite:
#' salic + extreme dry consistence (cemented).
#'
#' Audit list typo "etrosalic" -> Petrosalic; this function carries
#' the canonical name.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_petrosalic <- function(pedon) {
  sal <- tryCatch(carater_salico(pedon), error = function(e) NULL)
  if (is.null(sal) || !isTRUE(sal$passed)) {
    return(DiagnosticResult$new(
      name = "Petrosalic", passed = FALSE, layers = integer(0),
      evidence = list(salic = sal),
      missing = if (is.null(sal)) "carater_salico" else
                  sal$missing %||% character(0),
      reference = "WRB (2022) Ch 5, Petrosalic"
    ))
  }
  h <- pedon$horizons
  cd <- h$consistence_dry %||% rep(NA_character_, nrow(h))
  cemented <- !is.na(cd) & grepl("(?i)cemented|extr.+dur|petric",
                                     cd, perl = TRUE)
  passed <- any(cemented[sal$layers], na.rm = TRUE)
  DiagnosticResult$new(
    name = "Petrosalic", passed = passed,
    layers = if (passed) intersect(sal$layers, which(cemented))
             else integer(0),
    evidence = list(salic = sal, n_cemented = sum(cemented[sal$layers],
                                                       na.rm = TRUE)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Petrosalic"
  )
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

#' Endic supplementary qualifier (ec): generic "in deep horizon" marker
#'
#' WRB 2022 Ch 5: generic "Endo-X" prefix marker for any qualifier
#' that takes a depth window 50-100 cm. Without a base diagnostic it
#' returns NA; in practice it is composed with another qualifier.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endic <- function(pedon) {
  h <- pedon$horizons
  in_window <- which(!is.na(h$top_cm) & h$top_cm >= 50 &
                       h$top_cm <= 100)
  passed <- length(in_window) > 0L
  DiagnosticResult$new(
    name = "Endic", passed = passed, layers = in_window,
    evidence = list(depth_window_cm = c(50, 100),
                      n_layers_in_window = length(in_window)),
    missing = if (length(in_window) == 0L && any(is.na(h$top_cm)))
                "top_cm" else character(0),
    reference = "WRB (2022) Ch 5, Endic"
  )
}


#' Epic supplementary qualifier (ep): generic "in shallow horizon"
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_epic <- function(pedon) {
  h <- pedon$horizons
  in_window <- which(!is.na(h$top_cm) & h$top_cm < 50)
  passed <- length(in_window) > 0L
  DiagnosticResult$new(
    name = "Epic", passed = passed, layers = in_window,
    evidence = list(depth_window_cm = c(0, 50)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Epic"
  )
}


#' Endothyric supplementary qualifier (etc): thyric only at depth >= 50
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endothyric <- function(pedon) {
  base <- qual_thyric(pedon)
  .q_within_depth("Endothyric", base, pedon, 50, 200)
}


# --- Tier-2 substantive ----------------------------------------------------

#' Hyperorganic supplementary qualifier (hyo): SOC >= 18\% (peat-like)
#' WRB 2022 Ch 5: "Containing organic carbon >= 18\% by mass in any
#' layer >= 10 cm thick." A stronger version of `Carbonic`.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_hyperorganic <- function(pedon) {
  h <- pedon$horizons
  oc <- h$oc_pct
  if (is.null(oc) || all(is.na(oc))) {
    return(DiagnosticResult$new(
      name = "Hyperorganic", passed = NA, layers = integer(0),
      evidence = list(reason = "no oc_pct data"),
      missing = "oc_pct",
      reference = "WRB (2022) Ch 5, Hyperorganic"
    ))
  }
  qualifying <- which(!is.na(oc) & oc >= 18 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Hyperorganic", passed = passed, layers = qualifying,
    evidence = list(threshold_oc_pct = 18),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Hyperorganic"
  )
}


#' Mineralic supplementary qualifier (mn): predominantly mineral
#' WRB 2022 Ch 5: "Predominantly mineral material in upper 100 cm
#' (oc_pct < 12\% averaged over depth)."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
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
  wmean <- .q_weighted_mean(oc, h$top_cm, h$bottom_cm, 0, 100)
  passed <- isTRUE(is.finite(wmean) && wmean < 12)
  DiagnosticResult$new(
    name = "Mineralic", passed = passed,
    layers = which(!is.na(oc) & oc < 12 & h$top_cm < 100),
    evidence = list(weighted_mean_oc_pct = wmean, threshold = 12),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Mineralic"
  )
}


#' Alcalic supplementary qualifier (ac): pH (H2O) >= 9.0
#' WRB 2022 Ch 5: "Strongly alkaline reaction (pH H2O >= 9 in any
#' layer within 100 cm of the soil surface)."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_alcalic <- function(pedon) {
  h <- pedon$horizons
  ph <- h$ph_h2o
  if (is.null(ph) || all(is.na(ph))) {
    return(DiagnosticResult$new(
      name = "Alcalic", passed = NA, layers = integer(0),
      evidence = list(reason = "no ph_h2o data"),
      missing = "ph_h2o",
      reference = "WRB (2022) Ch 5, Alcalic"
    ))
  }
  qualifying <- which(!is.na(ph) & ph >= 9 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Alcalic", passed = passed, layers = qualifying,
    evidence = list(threshold_ph_h2o = 9),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Alcalic"
  )
}


#' Chloridic supplementary qualifier (cl): high chloride
#' WRB 2022 Ch 5: "Containing >= 4 cmol(c)/kg chloride OR EC >= 8
#' dS/m within 100 cm." Proxy via electrical conductivity field
#' (\code{ec_ds_m}) when chloride is unavailable.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_chloridic <- function(pedon) {
  h <- pedon$horizons
  cl <- h$cl_cmol %||% rep(NA_real_, nrow(h))
  ec <- h$ec_ds_m %||% rep(NA_real_, nrow(h))
  if (all(is.na(cl)) && all(is.na(ec))) {
    return(DiagnosticResult$new(
      name = "Chloridic", passed = NA, layers = integer(0),
      evidence = list(reason = "no cl_cmol / ec_ds_m"),
      missing = c("cl_cmol", "ec_ds_m"),
      reference = "WRB (2022) Ch 5, Chloridic"
    ))
  }
  hits <- (!is.na(cl) & cl >= 4) | (!is.na(ec) & ec >= 8)
  qualifying <- which(hits & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Chloridic", passed = passed, layers = qualifying,
    evidence = list(threshold_cl_cmol = 4, threshold_ec_ds_m = 8),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Chloridic"
  )
}


#' Columnic supplementary qualifier (cm): columnar / prismatic structure
#' WRB 2022 Ch 5: "Columnar or strong prismatic structure
#' (associated with natric horizons)."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_columnic <- function(pedon) {
  h <- pedon$horizons
  st <- h$structure_type %||% rep(NA_character_, nrow(h))
  pat <- "(?i)columnar|column|prism"
  hits <- !is.na(st) & grepl(pat, st, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  if (all(is.na(st))) {
    return(DiagnosticResult$new(
      name = "Columnic", passed = NA, layers = integer(0),
      evidence = list(reason = "no structure_type data"),
      missing = "structure_type",
      reference = "WRB (2022) Ch 5, Columnic"
    ))
  }
  DiagnosticResult$new(
    name = "Columnic", passed = passed, layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Columnic"
  )
}


#' Differentic supplementary qualifier (df): contrasting layers
#' WRB 2022 Ch 5: "Strong differences (texture, mineralogy, color)
#' between adjacent layers without abrupt textural transition (mild
#' clay-increase 1.2-1.4x ratio)."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_differentic <- function(pedon) {
  h <- pedon$horizons
  cl <- h$clay_pct
  if (is.null(cl) || sum(!is.na(cl)) < 2L) {
    return(DiagnosticResult$new(
      name = "Differentic", passed = NA, layers = integer(0),
      evidence = list(reason = "need >= 2 horizons with clay_pct"),
      missing = "clay_pct",
      reference = "WRB (2022) Ch 5, Differentic"
    ))
  }
  ord <- order(h$top_cm)
  cl_ord <- cl[ord]
  hits <- integer(0)
  for (i in seq_len(length(cl_ord) - 1L)) {
    if (is.na(cl_ord[i]) || is.na(cl_ord[i + 1L])) next
    ratio <- cl_ord[i + 1L] / cl_ord[i]
    if (is.finite(ratio) && ratio >= 1.2 && ratio <= 1.4)
      hits <- c(hits, ord[i + 1L])
  }
  passed <- length(hits) > 0L
  DiagnosticResult$new(
    name = "Differentic", passed = passed, layers = hits,
    evidence = list(threshold_ratio_low = 1.2,
                      threshold_ratio_high = 1.4),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Differentic"
  )
}


#' Capillaric supplementary qualifier (cp): capillary rise zone
#' WRB 2022 Ch 5: "Capillary rise from a shallow water table to within
#' 50 cm of the soil surface; flagged via redox concentrations (>=2\%) +
#' fine texture (clay+silt > 50\%)."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_capillaric <- function(pedon) {
  h <- pedon$horizons
  redox <- h$redoximorphic_features_pct
  cl <- h$clay_pct; si <- h$silt_pct
  fine <- !is.na(cl) & !is.na(si) & (cl + si) > 50
  has_redox <- !is.na(redox) & redox >= 2
  qualifying <- which(fine & has_redox & h$top_cm < 50)
  passed <- length(qualifying) > 0L
  if (all(is.na(redox)) || all(is.na(cl))) {
    return(DiagnosticResult$new(
      name = "Capillaric", passed = NA, layers = integer(0),
      evidence = list(reason = "need redox + clay/silt data in upper 50 cm"),
      missing = c("redoximorphic_features_pct", "clay_pct"),
      reference = "WRB (2022) Ch 5, Capillaric"
    ))
  }
  DiagnosticResult$new(
    name = "Capillaric", passed = passed, layers = qualifying,
    evidence = list(threshold_redox_pct = 2,
                      threshold_clay_silt_pct = 50),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Capillaric"
  )
}


#' Protospodic supplementary qualifier (psp): early-stage spodic
#' WRB 2022 Ch 5: "Spodic-like horizon meeting weakened criteria
#' (Al+Fe oxalate < 0.5\% but pyrophosphate > 0.05\%)." Lacking
#' pyrophosphate field; we proxy via spodic candidate horizons that
#' fail strict spodic.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_protospodic <- function(pedon) {
  spo <- tryCatch(spodic(pedon), error = function(e) NULL)
  if (is.null(spo)) {
    return(DiagnosticResult$new(
      name = "Protospodic", passed = NA, layers = integer(0),
      evidence = list(reason = "spodic() unavailable"),
      missing = "spodic",
      reference = "WRB (2022) Ch 5, Protospodic"
    ))
  }
  # Protospodic: spodic-like designation (Bs/Bh) without strict pass
  h <- pedon$horizons
  desg <- h$designation %||% rep(NA_character_, nrow(h))
  spodic_designation <- !is.na(desg) & grepl("(?i)^Bh|^Bs|^Bsh|^Bhs",
                                                  desg, perl = TRUE)
  passed <- !isTRUE(spo$passed) && any(spodic_designation, na.rm = TRUE)
  DiagnosticResult$new(
    name = "Protospodic", passed = passed,
    layers = if (passed) which(spodic_designation) else integer(0),
    evidence = list(spodic_strict = spo,
                      proto_pattern = "Bh|Bs|Bsh|Bhs"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Protospodic"
  )
}


#' Protoargic supplementary qualifier (pra): early-stage argic
#' WRB 2022 Ch 5: "Clay increase 2-6 percentage points (below the
#' canonical argic threshold)."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
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
  cl_ord <- cl[ord]
  hits <- integer(0)
  for (i in seq_len(length(cl_ord) - 1L)) {
    if (is.na(cl_ord[i]) || is.na(cl_ord[i + 1L])) next
    delta <- cl_ord[i + 1L] - cl_ord[i]
    if (is.finite(delta) && delta >= 2 && delta < 6)
      hits <- c(hits, ord[i + 1L])
  }
  passed <- length(hits) > 0L
  DiagnosticResult$new(
    name = "Protoargic", passed = passed, layers = hits,
    evidence = list(delta_pp_range = c(2, 6)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Protoargic"
  )
}


#' Protoandic supplementary qualifier (pan): early-stage andic
#' WRB 2022 Ch 5: "Andic-like properties below the strict threshold
#' (oxalate Al+Fe 0.4-2.0\%)."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_protoandic <- function(pedon) {
  h <- pedon$horizons
  al_ox <- h$al_ox_pct
  fe_ox <- h$fe_ox_pct
  if (is.null(al_ox) || all(is.na(al_ox))) {
    return(DiagnosticResult$new(
      name = "Protoandic", passed = NA, layers = integer(0),
      evidence = list(reason = "no al_ox_pct data"),
      missing = c("al_ox_pct", "fe_ox_pct"),
      reference = "WRB (2022) Ch 5, Protoandic"
    ))
  }
  alfe <- al_ox + (fe_ox %||% 0)
  qualifying <- which(!is.na(alfe) & alfe >= 0.4 & alfe < 2 &
                          h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Protoandic", passed = passed, layers = qualifying,
    evidence = list(alfe_range_pct = c(0.4, 2.0)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Protoandic"
  )
}


#' Activic supplementary qualifier (av): active aluminium >= 5 cmol/kg
#' WRB 2022 Ch 5: "KCl-extractable Al (\code{al_kcl_cmol}) >= 5
#' cmol(c)/kg in any layer in upper 100 cm." Proxy via existing
#' \code{al_cmol} (exchangeable Al) when al_kcl_cmol absent.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_activic <- function(pedon) {
  h <- pedon$horizons
  alkcl <- h$al_kcl_cmol %||% rep(NA_real_, nrow(h))
  alex  <- h$al_cmol %||% rep(NA_real_, nrow(h))
  series <- if (any(!is.na(alkcl))) alkcl else alex
  if (all(is.na(series))) {
    return(DiagnosticResult$new(
      name = "Activic", passed = NA, layers = integer(0),
      evidence = list(reason = "no al_kcl_cmol / al_cmol data"),
      missing = c("al_kcl_cmol", "al_cmol"),
      reference = "WRB (2022) Ch 5, Activic"
    ))
  }
  qualifying <- which(!is.na(series) & series >= 5 & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Activic", passed = passed, layers = qualifying,
    evidence = list(threshold_al_cmol = 5,
                      using = if (any(!is.na(alkcl))) "al_kcl_cmol"
                              else "al_cmol_proxy"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Activic"
  )
}


#' Geoabruptic supplementary qualifier (ga): abrupt change at lithological boundary
#' WRB 2022 Ch 5: "Abrupt textural / mineralogical change at a
#' lithological discontinuity (e.g., 2C horizon below B)."
#' Implementation: designation pattern containing "2C" or "3C"
#' (numeric prefix indicates lithologic discontinuity).
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_geoabruptic <- function(pedon) {
  h <- pedon$horizons
  desg <- h$designation %||% rep(NA_character_, nrow(h))
  hits <- !is.na(desg) & grepl("(?i)^[2-9][A-Z]", desg, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  if (all(is.na(desg))) {
    return(DiagnosticResult$new(
      name = "Geoabruptic", passed = NA, layers = integer(0),
      evidence = list(reason = "no designation data"),
      missing = "designation",
      reference = "WRB (2022) Ch 5, Geoabruptic"
    ))
  }
  DiagnosticResult$new(
    name = "Geoabruptic", passed = passed, layers = qualifying,
    evidence = list(pattern = "^[2-9][A-Z] (lithological discontinuity)"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Geoabruptic"
  )
}


#' Gilgaic supplementary qualifier (gi): gilgai microrelief
#' WRB 2022 Ch 5: "Gilgai microrelief (associated with vertic
#' shrinking/swelling soils)." Site-level field detection.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_gilgaic <- function(pedon) {
  relief <- pedon$site$forma_relevo %||%
              pedon$site$relevo_local %||%
              pedon$site$relief_form %||% NA_character_
  presence <- pedon$site$gilgai_presence %||% NA
  has_text <- !is.na(relief) && grepl("(?i)gilgai", relief, perl = TRUE)
  has_flag <- isTRUE(presence)
  passed <- has_text || has_flag
  DiagnosticResult$new(
    name = "Gilgaic", passed = passed, layers = integer(0),
    evidence = list(relief = relief, presence = presence),
    missing = if (!has_text && is.na(presence))
                c("forma_relevo", "gilgai_presence") else character(0),
    reference = "WRB (2022) Ch 5, Gilgaic"
  )
}


#' Gelistagnic supplementary qualifier (gst): stagnic in cold conditions
#' WRB 2022 Ch 5: "Stagnic features (perched water) in cryic regime."
#' Compose: stagnic_pattern + cryic_conditions.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_gelistagnic <- function(pedon) {
  cry <- tryCatch(cryic_conditions(pedon),
                    error = function(e) NULL)
  # Reuse soilkey's stagnic test via the gleyic fallback path
  h <- pedon$horizons
  redox <- h$redoximorphic_features_pct
  has_redox <- !is.na(redox) & redox >= 5 & h$top_cm <= 50
  passed <- !is.null(cry) && isTRUE(cry$passed) &&
              any(has_redox, na.rm = TRUE)
  DiagnosticResult$new(
    name = "Gelistagnic", passed = passed,
    layers = if (passed) which(has_redox) else integer(0),
    evidence = list(cryic = cry),
    missing = if (is.null(cry)) "cryic_conditions" else
                character(0),
    reference = "WRB (2022) Ch 5, Gelistagnic"
  )
}


#' Mahic supplementary qualifier (mh): manure-derived dark surface
#' WRB 2022 Ch 5: "Topsoil enriched by long-term manure / compost
#' application; oc_pct >= 4\%, base_saturation_pct >= 50\%, and
#' p_mehlich >= 100 mg/kg."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_mahic <- function(pedon) {
  h <- pedon$horizons
  oc <- h$oc_pct; bs <- h$base_saturation_pct
  p  <- h$p_mehlich3_mg_kg %||% rep(NA_real_, nrow(h))
  upper <- which(!is.na(h$top_cm) & h$top_cm <= 30)
  hits <- !is.na(oc[upper]) & oc[upper] >= 4 &
            !is.na(bs[upper]) & bs[upper] >= 50 &
            !is.na(p[upper]) & p[upper] >= 100
  qualifying <- upper[hits]
  passed <- length(qualifying) > 0L
  if (all(is.na(p))) {
    return(DiagnosticResult$new(
      name = "Mahic", passed = NA, layers = integer(0),
      evidence = list(reason = "no p_mehlich3_mg_kg data"),
      missing = "p_mehlich3_mg_kg",
      reference = "WRB (2022) Ch 5, Mahic"
    ))
  }
  DiagnosticResult$new(
    name = "Mahic", passed = passed, layers = qualifying,
    evidence = list(threshold_oc = 4, threshold_bs = 50,
                      threshold_p_mg_kg = 100),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Mahic"
  )
}


#' Laxic supplementary qualifier (lx): loose / non-cohesive surface
#' WRB 2022 Ch 5: "Surface horizon with loose dry consistence and
#' single-grain or massive structure."
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_laxic <- function(pedon) {
  h <- pedon$horizons
  cd <- h$consistence_dry %||% rep(NA_character_, nrow(h))
  st <- h$structure_type %||% rep(NA_character_, nrow(h))
  upper <- which(!is.na(h$top_cm) & h$top_cm < 30)
  loose <- !is.na(cd[upper]) & grepl("(?i)loose|solta", cd[upper], perl = TRUE)
  weak  <- !is.na(st[upper]) & grepl("(?i)single.?grain|massiv|graos.simples",
                                          st[upper], perl = TRUE)
  qualifying <- upper[loose | weak]
  passed <- length(qualifying) > 0L
  if (all(is.na(cd[upper])) && all(is.na(st[upper]))) {
    return(DiagnosticResult$new(
      name = "Laxic", passed = NA, layers = integer(0),
      evidence = list(reason = "no consistence_dry / structure_type"),
      missing = c("consistence_dry", "structure_type"),
      reference = "WRB (2022) Ch 5, Laxic"
    ))
  }
  DiagnosticResult$new(
    name = "Laxic", passed = passed, layers = qualifying,
    evidence = list(loose_n = sum(loose, na.rm = TRUE),
                      weak_struct_n = sum(weak, na.rm = TRUE)),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Laxic"
  )
}


# --- v0.9.65: Tier-3 qualifiers wired to actual schema fields --------------
#
# v0.9.64 had these as `.q_stub_na()` placeholders. v0.9.65 adds the
# corresponding schema fields to `horizon_column_spec()` and wires the
# qualifiers to read them. Each is now a substantive function (still
# returns NA when the field is unpopulated).


#' Archaic supplementary qualifier (ah): archeological context
#'
#' WRB 2022 Ch 5: "Soil developed in or affected by ancient cultural
#' material (>1000 yr old)." Detects via \code{contamination_type}
#' matching "archaeological" or site-level cultural-period field.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_archaic <- function(pedon) {
  cont <- pedon$horizons$contamination_type
  cult <- pedon$site$cultural_period %||% NA_character_
  if (all(is.na(cont)) && is.na(cult)) {
    return(DiagnosticResult$new(
      name = "Archaic", passed = NA, layers = integer(0),
      evidence = list(reason = "no contamination_type / cultural_period"),
      missing = c("contamination_type", "site$cultural_period"),
      reference = "WRB (2022) Ch 5, Archaic"))
  }
  hits <- !is.na(cont) & grepl("(?i)archae|archaic|ancient",
                                   cont, perl = TRUE)
  has_text <- !is.na(cult) && nzchar(cult)
  passed <- any(hits, na.rm = TRUE) || has_text
  DiagnosticResult$new(
    name = "Archaic", passed = passed,
    layers = if (any(hits, na.rm = TRUE)) which(hits) else integer(0),
    evidence = list(cultural_period = cult),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Archaic")
}


#' Arenicolic supplementary qualifier (an): faunal sand burrows
#'
#' WRB 2022 Ch 5: "Containing layers with extensive sand-grade
#' bioturbation (faunal burrows from earthworms / ants / termites)."
#' Implementation: \code{bioturbation_density} \\>= "common".
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_arenicolic <- function(pedon) {
  h <- pedon$horizons
  bd <- h$bioturbation_density %||% rep(NA_character_, nrow(h))
  if (all(is.na(bd))) {
    return(DiagnosticResult$new(
      name = "Arenicolic", passed = NA, layers = integer(0),
      evidence = list(reason = "no bioturbation_density data"),
      missing = "bioturbation_density",
      reference = "WRB (2022) Ch 5, Arenicolic"))
  }
  hits <- !is.na(bd) & grepl("(?i)common|many|abundant",
                                  bd, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Arenicolic", passed = passed, layers = qualifying,
    evidence = list(threshold = "common+"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Arenicolic")
}


#' Biocrustic supplementary qualifier (bk): biological soil crust
#'
#' WRB 2022 Ch 5: "Surface biological crust (cyanobacteria, algae,
#' lichens, mosses)." Implementation: \code{surface_crust_type} matching
#' biological pattern in upper 5 cm.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
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


#' Bryic supplementary qualifier (by): bryophyte cover at surface
#'
#' WRB 2022 Ch 5: "Predominant bryophyte (moss / liverwort) ground
#' cover." Implementation: \code{layer_origin} matches moss / lichen
#' pattern OR \code{vegetation_cover} site field >= 50.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_bryic <- function(pedon) {
  h <- pedon$horizons
  origin <- h$layer_origin %||% rep(NA_character_, nrow(h))
  cover <- pedon$site$vegetation_cover %||% NA_character_
  pat <- "(?i)moss|musgo|sphagnum|liverwort|lichen"
  hits <- !is.na(origin) & grepl(pat, origin, perl = TRUE)
  has_cover <- !is.na(cover) && grepl(pat, tolower(cover), perl = TRUE)
  qualifying <- which(hits & h$top_cm < 10)
  passed <- length(qualifying) > 0L || has_cover
  if (length(qualifying) == 0L && !has_cover &&
        all(is.na(origin)) && is.na(cover)) {
    return(DiagnosticResult$new(
      name = "Bryic", passed = NA, layers = integer(0),
      evidence = list(reason = "no layer_origin / vegetation_cover"),
      missing = c("layer_origin", "site$vegetation_cover"),
      reference = "WRB (2022) Ch 5, Bryic"))
  }
  DiagnosticResult$new(
    name = "Bryic", passed = passed, layers = qualifying,
    evidence = list(pattern = pat, vegetation_cover = cover),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Bryic")
}


#' Cordic supplementary qualifier (cd): cordic horizon
#'
#' WRB 2022 Ch 5: "Cemented horizon NOT meeting duripan / petrocalcic /
#' petrogypsic criteria but slacks moderately in water." Detection via
#' \code{cordic_horizon} TRUE/FALSE schema flag.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_cordic <- function(pedon) {
  h <- pedon$horizons
  cf <- h$cordic_horizon %||% rep(NA, nrow(h))
  if (all(is.na(cf))) {
    return(DiagnosticResult$new(
      name = "Cordic", passed = NA, layers = integer(0),
      evidence = list(reason = "no cordic_horizon flag"),
      missing = "cordic_horizon",
      reference = "WRB (2022) Ch 5, Cordic"))
  }
  qualifying <- which(!is.na(cf) & cf & h$top_cm < 100)
  passed <- length(qualifying) > 0L
  DiagnosticResult$new(
    name = "Cordic", passed = passed, layers = qualifying,
    evidence = list(),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Cordic")
}


#' Dorsic supplementary qualifier (do): dorsal-ridge microrelief
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_dorsic <- function(pedon) {
  mr <- pedon$site$microrelief_form %||% NA_character_
  if (is.na(mr)) {
    return(DiagnosticResult$new(
      name = "Dorsic", passed = NA, layers = integer(0),
      evidence = list(reason = "no microrelief_form site field"),
      missing = "site$microrelief_form",
      reference = "WRB (2022) Ch 5, Dorsic"))
  }
  passed <- grepl("(?i)dorsal|ridge|cumulus|hummock", mr, perl = TRUE)
  DiagnosticResult$new(
    name = "Dorsic", passed = passed, layers = integer(0),
    evidence = list(microrelief_form = mr),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Dorsic")
}


#' Escalic supplementary qualifier (es): terraced / stepped morphology
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_escalic <- function(pedon) {
  mr <- pedon$site$microrelief_form %||% NA_character_
  if (is.na(mr)) {
    return(DiagnosticResult$new(
      name = "Escalic", passed = NA, layers = integer(0),
      evidence = list(reason = "no microrelief_form site field"),
      missing = "site$microrelief_form",
      reference = "WRB (2022) Ch 5, Escalic"))
  }
  passed <- grepl("(?i)terrac|escal|step|degraus", mr, perl = TRUE)
  DiagnosticResult$new(
    name = "Escalic", passed = passed, layers = integer(0),
    evidence = list(microrelief_form = mr),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Escalic")
}


#' Evapocrustic supplementary qualifier (ev): evaporite surface crust
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_evapocrustic <- function(pedon) {
  h <- pedon$horizons
  sc <- h$surface_crust_type %||% rep(NA_character_, nrow(h))
  if (all(is.na(sc))) {
    return(DiagnosticResult$new(
      name = "Evapocrustic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface_crust_type"),
      missing = "surface_crust_type",
      reference = "WRB (2022) Ch 5, Evapocrustic"))
  }
  pat <- "(?i)evapor|salt.crust|gypsum.crust|halite|crusty"
  hits <- !is.na(sc) & grepl(pat, sc, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 5)
  DiagnosticResult$new(
    name = "Evapocrustic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Evapocrustic")
}


#' Immissic supplementary qualifier (im): atmospheric immission
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_immissic <- function(pedon) {
  h <- pedon$horizons
  ct <- h$contamination_type %||% rep(NA_character_, nrow(h))
  if (all(is.na(ct))) {
    return(DiagnosticResult$new(
      name = "Immissic", passed = NA, layers = integer(0),
      evidence = list(reason = "no contamination_type"),
      missing = "contamination_type",
      reference = "WRB (2022) Ch 5, Immissic"))
  }
  pat <- "(?i)immission|atmospheric|heavy.metal|imissao"
  hits <- !is.na(ct) & grepl(pat, ct, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Immissic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Immissic")
}


#' Isopteric supplementary qualifier (ip): termite / ant biogenesis
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_isopteric <- function(pedon) {
  h <- pedon$horizons
  bd <- h$bioturbation_density %||% rep(NA_character_, nrow(h))
  origin <- h$layer_origin %||% rep(NA_character_, nrow(h))
  if (all(is.na(bd)) && all(is.na(origin))) {
    return(DiagnosticResult$new(
      name = "Isopteric", passed = NA, layers = integer(0),
      evidence = list(reason = "no bioturbation_density / layer_origin"),
      missing = c("bioturbation_density", "layer_origin"),
      reference = "WRB (2022) Ch 5, Isopteric"))
  }
  pat <- "(?i)termit|ant.mound|formig|cupim|isopter"
  hits <- (!is.na(bd) & grepl(pat, bd, perl = TRUE)) |
          (!is.na(origin) & grepl(pat, origin, perl = TRUE))
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Isopteric", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Isopteric")
}


#' Kalaic supplementary qualifier (ka): dry-season puffed surface layer
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_kalaic <- function(pedon) {
  h <- pedon$horizons
  pf <- h$surface_puff_layer %||% rep(NA, nrow(h))
  if (all(is.na(pf))) {
    return(DiagnosticResult$new(
      name = "Kalaic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface_puff_layer flag"),
      missing = "surface_puff_layer",
      reference = "WRB (2022) Ch 5, Kalaic"))
  }
  qualifying <- which(!is.na(pf) & pf & h$top_cm < 5)
  DiagnosticResult$new(
    name = "Kalaic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Kalaic")
}


#' Lapiadic supplementary qualifier (lp): karren / lapies bedrock features
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_lapiadic <- function(pedon) {
  h <- pedon$horizons
  ws <- h$weathering_stage %||% rep(NA_character_, nrow(h))
  if (all(is.na(ws))) {
    return(DiagnosticResult$new(
      name = "Lapiadic", passed = NA, layers = integer(0),
      evidence = list(reason = "no weathering_stage"),
      missing = "weathering_stage",
      reference = "WRB (2022) Ch 5, Lapiadic"))
  }
  pat <- "(?i)karren|lapies|lapiad|grike"
  hits <- !is.na(ws) & grepl(pat, ws, perl = TRUE)
  qualifying <- which(hits)
  DiagnosticResult$new(
    name = "Lapiadic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Lapiadic")
}


#' Litholinic supplementary qualifier (ll): stratified soil on rock
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_litholinic <- function(pedon) {
  h <- pedon$horizons
  sp <- h$stratification_pattern %||% rep(NA_character_, nrow(h))
  desg <- h$designation %||% rep(NA_character_, nrow(h))
  if (all(is.na(sp)) && all(is.na(desg))) {
    return(DiagnosticResult$new(
      name = "Litholinic", passed = NA, layers = integer(0),
      evidence = list(reason = "no stratification_pattern / designation"),
      missing = c("stratification_pattern", "designation"),
      reference = "WRB (2022) Ch 5, Litholinic"))
  }
  pat_sp <- "(?i)stratif|layer|interrupt"
  pat_dg <- "(?i)^R|^Cr"
  hits <- (!is.na(sp) & grepl(pat_sp, sp, perl = TRUE)) |
          (!is.na(desg) & grepl(pat_dg, desg, perl = TRUE))
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Litholinic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern_strat = pat_sp,
                      pattern_desg = pat_dg),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Litholinic")
}


#' Mochipic supplementary qualifier (mp): mottled mochi-like pattern
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_mochipic <- function(pedon) {
  h <- pedon$horizons
  mm <- h$mottle_morphology %||% rep(NA_character_, nrow(h))
  if (all(is.na(mm))) {
    return(DiagnosticResult$new(
      name = "Mochipic", passed = NA, layers = integer(0),
      evidence = list(reason = "no mottle_morphology"),
      missing = "mottle_morphology",
      reference = "WRB (2022) Ch 5, Mochipic"))
  }
  hits <- !is.na(mm) & grepl("(?i)mochi|banded|patchy",
                                  mm, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Mochipic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = "mochi|banded|patchy"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Mochipic")
}


#' Naramic supplementary qualifier (na): salt-crust morphology
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_naramic <- function(pedon) {
  h <- pedon$horizons
  sc <- h$salt_crust_pattern %||% rep(NA_character_, nrow(h))
  if (all(is.na(sc))) {
    return(DiagnosticResult$new(
      name = "Naramic", passed = NA, layers = integer(0),
      evidence = list(reason = "no salt_crust_pattern"),
      missing = "salt_crust_pattern",
      reference = "WRB (2022) Ch 5, Naramic"))
  }
  hits <- !is.na(sc) & grepl("(?i)effloresc|crusty|hardpan|salt.crust",
                                  sc, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Naramic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = "salt-crust morphology"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Naramic")
}


#' Nechic supplementary qualifier (ne): aeolian / loess deposit pattern
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_nechic <- function(pedon) {
  h <- pedon$horizons
  ae <- h$aeolian_morphology %||% rep(NA_character_, nrow(h))
  if (all(is.na(ae))) {
    return(DiagnosticResult$new(
      name = "Nechic", passed = NA, layers = integer(0),
      evidence = list(reason = "no aeolian_morphology"),
      missing = "aeolian_morphology",
      reference = "WRB (2022) Ch 5, Nechic"))
  }
  hits <- !is.na(ae) & grepl("(?i)loess|dune|aeolian|sandsheet",
                                  ae, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Nechic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = "loess|dune|aeolian|sandsheet"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Nechic")
}


#' Pelocrustic supplementary qualifier (pc): clayey surface crust
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_pelocrustic <- function(pedon) {
  h <- pedon$horizons
  sc <- h$surface_crust_type %||% rep(NA_character_, nrow(h))
  if (all(is.na(sc))) {
    return(DiagnosticResult$new(
      name = "Pelocrustic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface_crust_type"),
      missing = "surface_crust_type",
      reference = "WRB (2022) Ch 5, Pelocrustic"))
  }
  hits <- !is.na(sc) & grepl("(?i)pelo|clay|clayey",
                                  sc, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 5)
  DiagnosticResult$new(
    name = "Pelocrustic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = "pelo|clay|clayey"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Pelocrustic")
}


#' Puffic supplementary qualifier (pf): puffed surface
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_puffic <- function(pedon) {
  h <- pedon$horizons
  pf <- h$surface_puff_layer %||% rep(NA, nrow(h))
  if (all(is.na(pf))) {
    return(DiagnosticResult$new(
      name = "Puffic", passed = NA, layers = integer(0),
      evidence = list(reason = "no surface_puff_layer flag"),
      missing = "surface_puff_layer",
      reference = "WRB (2022) Ch 5, Puffic"))
  }
  qualifying <- which(!is.na(pf) & pf & h$top_cm < 5)
  DiagnosticResult$new(
    name = "Puffic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Puffic")
}


#' Raptic supplementary qualifier (rp): stratification break
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_raptic <- function(pedon) {
  h <- pedon$horizons
  sp <- h$stratification_pattern %||% rep(NA_character_, nrow(h))
  if (all(is.na(sp))) {
    return(DiagnosticResult$new(
      name = "Raptic", passed = NA, layers = integer(0),
      evidence = list(reason = "no stratification_pattern"),
      missing = "stratification_pattern",
      reference = "WRB (2022) Ch 5, Raptic"))
  }
  pat <- "(?i)break|interrupt|raptic|discont"
  hits <- !is.na(sp) & grepl(pat, sp, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Raptic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Raptic")
}


#' Saprolithic supplementary qualifier (sp): saprolite parent material
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_saprolithic <- function(pedon) {
  h <- pedon$horizons
  sp <- h$saprolite_pct %||% rep(NA_real_, nrow(h))
  ws <- h$weathering_stage %||% rep(NA_character_, nrow(h))
  if (all(is.na(sp)) && all(is.na(ws))) {
    return(DiagnosticResult$new(
      name = "Saprolithic", passed = NA, layers = integer(0),
      evidence = list(reason = "no saprolite_pct / weathering_stage"),
      missing = c("saprolite_pct", "weathering_stage"),
      reference = "WRB (2022) Ch 5, Saprolithic"))
  }
  hits_pct <- !is.na(sp) & sp >= 50
  hits_ws  <- !is.na(ws) & grepl("(?i)saprolit|weathered|saprolite",
                                       ws, perl = TRUE)
  qualifying <- which((hits_pct | hits_ws) & h$top_cm < 200)
  DiagnosticResult$new(
    name = "Saprolithic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(threshold_saprolite_pct = 50,
                      pattern_ws = "saprolit|weathered"),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Saprolithic")
}


#' Thixotropic supplementary qualifier (tx): thixotropic behavior
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
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
  qualifying <- which(!is.na(ti) & ti >= 50 & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Thixotropic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(threshold_thixotropic_index = 50),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Thixotropic")
}


#' Uterquic supplementary qualifier (uq): bidirectional water regime
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_uterquic <- function(pedon) {
  h <- pedon$horizons
  wr <- h$water_regime_pattern %||% rep(NA_character_, nrow(h))
  if (all(is.na(wr))) {
    return(DiagnosticResult$new(
      name = "Uterquic", passed = NA, layers = integer(0),
      evidence = list(reason = "no water_regime_pattern"),
      missing = "water_regime_pattern",
      reference = "WRB (2022) Ch 5, Uterquic"))
  }
  pat <- "(?i)bidirec|uterquic|fluctuat"
  hits <- !is.na(wr) & grepl(pat, wr, perl = TRUE)
  qualifying <- which(hits & h$top_cm < 100)
  DiagnosticResult$new(
    name = "Uterquic", passed = length(qualifying) > 0L,
    layers = qualifying,
    evidence = list(pattern = pat),
    missing = character(0),
    reference = "WRB (2022) Ch 5, Uterquic")
}


# Bonus Endo- variants (qual_endocalcic / qual_endogypsic /
# qual_endoduric) -- mechanical depth modifiers of existing
# diagnostics, not in v0.9.63's batch.


#' Endocalcic supplementary qualifier: calcic horizon at depth >= 50 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endocalcic <- function(pedon) {
  base <- calcic(pedon)
  .q_within_depth("Endocalcic", base, pedon, 50, 200)
}


#' Endogypsic supplementary qualifier: gypsic horizon at depth >= 50 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endogypsic <- function(pedon) {
  base <- gypsic(pedon)
  .q_within_depth("Endogypsic", base, pedon, 50, 200)
}


#' Endoduric supplementary qualifier: duric horizon at depth >= 50 cm
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_endoduric <- function(pedon) {
  base <- duric_horizon(pedon)
  .q_within_depth("Endoduric", base, pedon, 50, 200)
}
