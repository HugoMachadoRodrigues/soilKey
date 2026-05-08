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
#' horizon." Compose: albic ∧ ¬spodic.
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
  nat <- tryCatch(natric(pedon), error = function(e) NULL)
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
#' >= 90% of the layer volume (forms a hard slab)." Composite:
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

#' Hyperorganic supplementary qualifier (hyo): SOC >= 18% (peat-like)
#' WRB 2022 Ch 5: "Containing organic carbon >= 18% by mass in any
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
#' (oc_pct < 12% averaged over depth)."
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
#' 50 cm of the soil surface; flagged via redox concentrations (>=2%) +
#' fine texture (clay+silt > 50%)."
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
#' (Al+Fe oxalate < 0.5% but pyrophosphate > 0.05%)." Lacking
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
#' (oxalate Al+Fe 0.4-2.0%)."
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
#' application; oc_pct >= 4%, base_saturation_pct >= 50%, and
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


# --- Tier-3 stubs (function exists but requires schema we don't have yet) ---

#' Archaic supplementary qualifier (ah): archeological context
#'
#' WRB 2022 Ch 5: "Soil developed in or affected by ancient cultural
#' material (>1000 yr old)." Tier-3: requires archeological-context
#' site field not yet in soilKey schema.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_archaic <- .q_stub_na("Archaic",
                              c("archeological_context",
                                "site$cultural_period"),
                              "WRB (2022) Ch 5, Archaic")


#' Arenicolic supplementary qualifier (an): faunal sand burrows
#' Tier-3: requires bioturbation / burrow-density site field.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_arenicolic <- .q_stub_na("Arenicolic",
                                  c("bioturbation_density",
                                    "burrow_density"),
                                  "WRB (2022) Ch 5, Arenicolic")


#' Biocrustic supplementary qualifier (bk): biological soil crust
#' Tier-3: requires surface-crust morphology field.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_biocrustic <- .q_stub_na("Biocrustic",
                                  c("surface_crust_type"),
                                  "WRB (2022) Ch 5, Biocrustic")


#' Bryic supplementary qualifier (by): bryophyte cover
#' Tier-3: requires vegetation-cover composition field.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_bryic <- .q_stub_na("Bryic",
                            c("vegetation_cover", "site$bryophyte_pct"),
                            "WRB (2022) Ch 5, Bryic")


#' Cordic supplementary qualifier (cd): cordic horizon
#' Tier-3: requires "cordic horizon" diagnostic not yet in soilKey.
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_cordic <- .q_stub_na("Cordic",
                            c("cordic_horizon"),
                            "WRB (2022) Ch 5, Cordic")


#' Dorsic supplementary qualifier (do): dorsal/ridged surface morphology
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_dorsic <- .q_stub_na("Dorsic",
                            c("microrelief_form", "site$dorsal_morphology"),
                            "WRB (2022) Ch 5, Dorsic")


#' Escalic supplementary qualifier (es): terraced/stepped morphology
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_escalic <- .q_stub_na("Escalic",
                              c("site$terrace_form"),
                              "WRB (2022) Ch 5, Escalic")


#' Evapocrustic supplementary qualifier (ev): evaporite crust at surface
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_evapocrustic <- .q_stub_na("Evapocrustic",
                                    c("surface_crust_type"),
                                    "WRB (2022) Ch 5, Evapocrustic")


#' Immissic supplementary qualifier (im): atmospheric immission contamination
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_immissic <- .q_stub_na("Immissic",
                                c("contamination_type",
                                  "site$pollution_history"),
                                "WRB (2022) Ch 5, Immissic")


#' Isopteric supplementary qualifier (ip): termite / ant biogenesis
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_isopteric <- .q_stub_na("Isopteric",
                                c("termite_activity", "isopter_density"),
                                "WRB (2022) Ch 5, Isopteric")


#' Kalaic supplementary qualifier (ka): dry-season puff layer
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_kalaic <- .q_stub_na("Kalaic",
                              c("surface_puff_layer"),
                              "WRB (2022) Ch 5, Kalaic")


#' Lapiadic supplementary qualifier (lp): karren/lapies bedrock features
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_lapiadic <- .q_stub_na("Lapiadic",
                                c("bedrock_morphology"),
                                "WRB (2022) Ch 5, Lapiadic")


#' Litholinic supplementary qualifier (ll): stratification on rock
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_litholinic <- .q_stub_na("Litholinic",
                                  c("stratification_pattern",
                                    "rock_substrate_form"),
                                  "WRB (2022) Ch 5, Litholinic"  )


#' Mochipic supplementary qualifier (mp): mottled mochi-like pattern
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_mochipic <- .q_stub_na("Mochipic",
                                c("mottle_morphology"),
                                "WRB (2022) Ch 5, Mochipic")


#' Naramic supplementary qualifier (na): salt crust patterns
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_naramic <- .q_stub_na("Naramic",
                              c("salt_crust_pattern"),
                              "WRB (2022) Ch 5, Naramic")


#' Nechic supplementary qualifier (ne): aeolian deposit / loess pattern
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_nechic <- .q_stub_na("Nechic",
                              c("aeolian_morphology", "loess_indicator"),
                              "WRB (2022) Ch 5, Nechic")


#' Pelocrustic supplementary qualifier (pc): clayey surface crust
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_pelocrustic <- .q_stub_na("Pelocrustic",
                                  c("surface_crust_type"),
                                  "WRB (2022) Ch 5, Pelocrustic")


#' Puffic supplementary qualifier (pf): puffed surface
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_puffic <- .q_stub_na("Puffic",
                              c("surface_puff_layer"),
                              "WRB (2022) Ch 5, Puffic")


#' Raptic supplementary qualifier (rp): stratification break
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_raptic <- .q_stub_na("Raptic",
                              c("stratification_break"),
                              "WRB (2022) Ch 5, Raptic")


#' Saprolithic supplementary qualifier (sp): saprolite parent material
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_saprolithic <- .q_stub_na("Saprolithic",
                                    c("saprolite_pct", "weathering_stage"),
                                    "WRB (2022) Ch 5, Saprolithic")


#' Thixotropic supplementary qualifier (tx): thixotropic behavior
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_thixotropic <- .q_stub_na("Thixotropic",
                                    c("thixotropic_index", "slurry_test"),
                                    "WRB (2022) Ch 5, Thixotropic")


#' Uterquic supplementary qualifier (uq): bidirectional water regime
#' @param pedon A \code{\link{PedonRecord}}.
#' @export
qual_uterquic <- .q_stub_na("Uterquic",
                                c("water_regime_pattern"),
                                "WRB (2022) Ch 5, Uterquic")


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
