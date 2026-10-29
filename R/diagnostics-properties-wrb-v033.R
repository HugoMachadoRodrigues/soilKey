# ============================================================================
# v0.3.3 -- WRB 2022 Ch 3.2 diagnostic properties not previously
# implemented:
#   abrupt_textural_difference, albeluvic_glossae, continuous_rock,
#   lithic_discontinuity, protocalcic_properties, protogypsic_properties,
#   reducing_conditions, shrink_swell_cracks, sideralic_properties,
#   takyric_properties, vitric_properties, yermic_properties.
# ============================================================================


#' Abrupt textural difference (WRB 2022 Ch 3.2.1)
#'
#' Sharp clay-content increase between two superimposed mineral layers
#' meeting all of (WRB 2022 3.2.1, p. 72):
#' \itemize{
#'   \item underlying clay \\>= 15\% AND thickness \\>= 7.5 cm;
#'   \item underlying starts \\>= 10 cm below the mineral soil surface;
#'   \item underlying has, vs overlying: 2x clay if overlying < 20\%, OR
#'         \\>= 20pp (absolute) more clay if overlying \\>= 20\%;
#'   \item transitional layer, if any, \\<= 2 cm.
#' }
#' Since v0.9.312 criterion 5 is read from the distinctness of the lower
#' boundary of the overlying layer (\code{boundary_distinctness}): abrupt
#' (2 cm or less) or very abrupt passes, clear, gradual or diffuse fails, and
#' a boundary not recorded leaves the pair undecided (\code{NA}). Criterion 2
#' is counted from the mineral soil surface (below organic layers).
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
abrupt_textural_difference <- function(pedon) {
  h <- pedon$horizons
  passing <- integer(0); missing <- character(0); details <- list()
  if (nrow(h) < 2L) {
    return(DiagnosticResult$new(
      name = "abrupt_textural_difference", passed = FALSE,
      layers = integer(0), evidence = list(), missing = character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 3.2.1",
      notes = "Profile has fewer than 2 layers"
    ))
  }
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  bd <- as.character(h$boundary_distinctness %||% rep(NA_character_, nrow(h)))
  undecided <- integer(0)
  for (i in seq.int(2L, nrow(h))) {
    above <- h$clay_pct[i - 1L]; here <- h$clay_pct[i]
    top   <- h$top_cm[i]
    bot   <- h$bottom_cm[i]
    if (is.na(above) || is.na(here)) {
      missing <- c(missing, "clay_pct"); next
    }
    if (is.na(top) || is.na(bot)) {
      missing <- c(missing, "top_cm", "bottom_cm"); next
    }
    underlying_clay_ok <- here >= 15
    thickness_ok      <- (bot - top) >= 7.5
    starts_below_10   <- top - ms >= 10
    jump_ok <- if (above < 20) here >= 2 * above
               else here - above >= 20
    # criterion 5: the transition, the lower boundary of the overlying layer,
    # is 2 cm or less (FAO abrupt, 0.5-2 cm; very abrupt, under 0.5 cm)
    b <- tolower(trimws(bd[i - 1L]))
    transition_ok <- if (is.na(b) || !nzchar(b)) NA
                     else grepl("abrupt", b) || grepl("sharp", b)
    geometry_ok <- underlying_clay_ok && thickness_ok && starts_below_10 && jump_ok
    layer_pass <- if (!geometry_ok) FALSE else transition_ok
    details[[as.character(i)]] <- list(
      between = c(i - 1L, i),
      above_clay = above, here_clay = here,
      thickness_cm = bot - top, top_cm = top,
      jump_path = if (above < 20) "2x" else "+20pp",
      geometry_ok = geometry_ok,
      boundary_distinctness = bd[i - 1L], transition_ok = transition_ok,
      passed = layer_pass
    )
    if (isTRUE(layer_pass)) passing <- c(passing, i)
    if (is.na(layer_pass)) {
      undecided <- c(undecided, i); missing <- c(missing, "boundary_distinctness")
    }
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(undecided) > 0L) NA
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name      = "abrupt_textural_difference",
    passed    = passed,
    layers    = passing,
    evidence  = list(layer_pairs = details, undecided = undecided),
    missing   = if (isTRUE(passed)) character(0) else unique(missing),
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.1 (p. 72)"
  )
}


#' Albeluvic glossae (WRB 2022 Ch 3.2.2)
#'
#' Tongues of bleached, coarser-textured material penetrating an argic
#' horizon. v0.3.3 detects via designation pattern \code{glossic|albeluvic}
#' on a layer that overlies an argic-horizon-passing layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
albeluvic_glossae <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation",
                                              "glossic|albeluvic|tongue")
  arg <- argic(pedon)
  tests$argic_present <- list(
    passed  = arg$passed,
    layers  = arg$layers,
    missing = arg$missing %||% character(0),
    details = list(argic = arg$passed)
  )
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name      = "albeluvic_glossae",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.2",
    notes     = "v0.3.3: designation-pattern fallback (vertical/horizontal extension and area-coverage criteria deferred)"
  )
}


#' Continuous rock (WRB 2022 Ch 3.2.5)
#'
#' Consolidated material below the soil. v0.3.3: detects via designation
#' \code{R} or \code{Cr} on the lowermost (or any) layer.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
continuous_rock <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  # v0.9.220: R, RCr and R/Cr only (also below a lithic discontinuity, 2R).
  # Cr, weathered or soft bedrock, does not "remain intact when an air-dried
  # specimen ... is submerged in water for 1 hour" (Ch 3.2.5); it counted.
  tests$designation <- test_pattern_match(h, "designation",
                                              .WRB_ROCK_DESIGNATION)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "continuous_rock", passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.5"
  )
}


#' Lithic discontinuity (WRB 2022 Ch 3.2.7)
#'
#' Significant abrupt change in parent material between two layers.
#' v0.3.3 simplified: detects via large discontinuity in
#' coarse_fragments_pct (>= 10pp absolute jump) OR rock_origin difference
#' between consecutive layers.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
lithic_discontinuity <- function(pedon) {
  h <- pedon$horizons
  layers_pass <- integer(0)
  details <- list()
  missing <- character(0)
  if (nrow(h) < 2L) {
    return(DiagnosticResult$new(
      name = "lithic_discontinuity",
      passed = FALSE, layers = integer(0), evidence = list(),
      missing = character(0),
      reference = "IUSS Working Group WRB (2022), Chapter 3.2.7",
      notes = "Profile has fewer than 2 layers"
    ))
  }
  for (i in 2:nrow(h)) {
    j <- i - 1L
    cf_a <- h$coarse_fragments_pct[j]
    cf_b <- h$coarse_fragments_pct[i]
    or_a <- h$rock_origin[j]
    or_b <- h$rock_origin[i]
    cf_jump <- !is.na(cf_a) && !is.na(cf_b) && abs(cf_b - cf_a) >= 10
    or_diff <- !is.na(or_a) && !is.na(or_b) && or_a != or_b
    if (is.na(cf_a) && is.na(cf_b) && is.na(or_a) && is.na(or_b)) {
      missing <- c(missing, "coarse_fragments_pct", "rock_origin")
    } else {
      details[[as.character(i)]] <- list(
        between = c(j, i),
        coarse_jump_pp = if (!is.na(cf_a) && !is.na(cf_b))
                            abs(cf_b - cf_a) else NA,
        origin_diff = or_diff,
        passed = cf_jump || or_diff
      )
      if (cf_jump || or_diff) layers_pass <- c(layers_pass, i)
    }
  }
  passed <- if (length(layers_pass) > 0L) TRUE
            else if (length(missing) > 0L && length(details) == 0L) NA
            else FALSE
  DiagnosticResult$new(
    name = "lithic_discontinuity",
    passed = passed, layers = layers_pass,
    evidence = list(layer_pairs = details),
    missing = unique(missing),
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.7",
    notes = "v0.3.3 simplification: coarse-fragment jump or rock_origin change"
  )
}


#' Protocalcic properties (WRB 2022 Ch 3.2.8)
#'
#' Accumulations of secondary carbonates, visible when moist, that occupy
#' \\>= 5\% of the exposed area with masses, nodules, concretions or
#' filaments, or cover \\>= 10\% of the surfaces of aggregates, biopore
#' walls or the undersides of coarse fragments (WRB 2022 3.2.8, p. 78).
#' The test reads the recorded abundance of visible secondary carbonates,
#' \code{secondary_carbonates_pct} (or the visual \code{carbonate_soft_pct}
#' plus \code{carbonate_hard_pct}). Laboratory CaCO3 is not evidence of
#' secondary carbonates: with no abundance recorded, a layer with no CaCO3
#' (\code{caco3_pct} 0, or no effervescence) has no protocalcic properties
#' and any other layer is undecided. A \code{k} suffix with no abundance is
#' undecided too, unless \code{options(soilKey.morphological_inference =
#' TRUE)}, where it is accepted as morphological evidence.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_secondary_pct Minimum visible secondary carbonates, \% of the
#'   exposed area (default 5).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic
#'   is present, the qualifying layers, and the supporting evidence; the
#'   per-layer status (TRUE / FALSE / NA) is in \code{evidence$layer_status}.
#' @export
protocalcic_properties <- function(pedon, min_secondary_pct = 5) {
  h <- pedon$horizons
  st <- .protocalcic_layer_status(h, min_secondary_pct)
  passed <- if (nrow(h) == 0L) NA else .any3(st$status)
  DiagnosticResult$new(
    name = "protocalcic_properties",
    passed = passed,
    layers = which(st$status %in% TRUE),
    evidence = list(layer_status = st$status, source = st$source,
                    threshold_pct = min_secondary_pct),
    missing = if (is.na(passed)) "secondary_carbonates_pct" else character(0),
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.8, Protocalcic properties (p. 78)"
  )
}

# Per-layer protocalcic status, TRUE / FALSE / NA, and what decided it.
# WRB 2022 3.2.8 (p. 78): protocalcic properties are VISIBLE secondary
# carbonates (>= 5% of the exposed area as masses, nodules, concretions or
# filaments, or >= 10% of aggregate surfaces, biopore walls or undersides of
# coarse fragments), read here from the recorded abundance. Lab CaCO3 alone
# does not show them (Hugo, 2026-10-29): it rules them out only when there is
# no CaCO3 at all. A k suffix with no abundance is undecided, unless the
# opt-in morphological inference is on.
.protocalcic_layer_status <- function(h, min_pct = 5) {
  n <- nrow(h)
  col <- function(x) { v <- h[[x]]; if (is.null(v)) rep(NA, n) else v }
  sc   <- suppressWarnings(as.numeric(col("secondary_carbonates_pct")))
  soft <- suppressWarnings(as.numeric(col("carbonate_soft_pct")))
  hard <- suppressWarnings(as.numeric(col("carbonate_hard_pct")))
  ca   <- suppressWarnings(as.numeric(col("caco3_pct")))
  eff  <- tolower(trimws(as.character(col("effervescence_class"))))
  desg <- as.character(col("designation"))
  k_suffix <- !is.na(desg) & grepl("k", gsub("[^a-z]", "", desg), fixed = TRUE)
  status <- rep(NA, n)
  source <- rep("not recorded", n)
  for (i in seq_len(n)) {
    vis <- if (is.na(soft[i]) && is.na(hard[i])) NA_real_
           else sum(soft[i], hard[i], na.rm = TRUE)
    vis_known <- !is.na(soft[i]) && !is.na(hard[i])
    if (!is.na(sc[i])) {
      status[i] <- sc[i] >= min_pct; source[i] <- "secondary_carbonates_pct"
    } else if (!is.na(vis) && (vis >= min_pct || vis_known)) {
      status[i] <- vis >= min_pct; source[i] <- "carbonate_soft_pct + carbonate_hard_pct"
    } else if (!is.na(ca[i]) && ca[i] <= 0) {
      status[i] <- FALSE; source[i] <- "no CaCO3 (caco3_pct 0)"
    } else if (!is.na(eff[i]) && grepl("^non|^none|^nil|^n$", eff[i])) {
      status[i] <- FALSE; source[i] <- "no effervescence"
    } else if (k_suffix[i] && .morph_inference_enabled()) {
      status[i] <- TRUE; source[i] <- "k suffix (morphological inference)"
    }
  }
  list(status = status, source = source)
}


#' Protogypsic properties (WRB 2022 Ch 3.2.9): visible secondary gypsum
#' \\>= 1\% but below the gypsic gate.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_caso4_pct Numeric threshold or option (see Details).
#' @param max_caso4_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
protogypsic_properties <- function(pedon, min_caso4_pct = 1.0,
                                      max_caso4_pct = 5.0) {
  h <- pedon$horizons
  tests <- list()
  tests$caso4_low <- test_numeric_above(h, "caso4_pct",
                                            threshold = min_caso4_pct)
  if (isTRUE(tests$caso4_low$passed)) {
    high <- test_numeric_above(h, "caso4_pct",
                                  threshold = max_caso4_pct,
                                  candidate_layers = tests$caso4_low$layers)
    not_gypsic <- setdiff(tests$caso4_low$layers, high$layers)
    if (length(not_gypsic) == 0L) {
      tests$caso4_low$passed <- FALSE
      tests$caso4_low$layers <- integer(0)
    } else {
      tests$caso4_low$layers <- not_gypsic
    }
  }
  desg <- test_pattern_match(h, "designation", "y|By")
  if (!isTRUE(tests$caso4_low$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "protogypsic_properties",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.9"
  )
}


#' Reducing conditions (WRB 2022 Ch 3.2.10) -- per-pedon test wrapping
#' \code{test_reducing_conditions}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_redox_pct Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
reducing_conditions <- function(pedon, min_redox_pct = 5) {
  h <- pedon$horizons
  tests <- list()
  tests$redox <- test_reducing_conditions(h, min_redox_pct = min_redox_pct)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "reducing_conditions",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.10"
  )
}


#' Shrink-swell cracks (WRB 2022 Ch 3.2.12) -- per-pedon test wrapping
#' \code{test_shrink_swell_cracks}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_width_cm Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
shrink_swell_cracks <- function(pedon, min_width_cm = 0.5) {
  h <- pedon$horizons
  tests <- list()
  tests$cracks <- test_shrink_swell_cracks(h, min_width_cm = min_width_cm)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "shrink_swell_cracks",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.12"
  )
}


#' Sideralic properties (WRB 2022 Ch 3.2.13)
#'
#' Mineral material with a relatively low CEC. WRB 2022 (3.2.13) requires
#' BOTH:
#' \enumerate{
#'   \item one or both of: clay >= 8\% AND CEC/clay < 24 cmol_c/kg clay; OR
#'         bulk CEC < 2 cmol_c/kg soil;
#'   \item evidence of soil formation as defined in criterion 3 of the cambic
#'         horizon (\code{test_cambic_soil_formation}).
#' }
#' Both must be met by the SAME layer. Criterion 2 was added in v0.9.127
#' (previously only criterion 1 was enforced); where the soil-formation
#' evidence cannot be assessed (no Munsell/clay/Fe/carbonate adjacency data)
#' the result is \code{NA} rather than a false positive.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cec_per_clay Numeric threshold or option (see Details).
#' @param max_bulk_cec Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
sideralic_properties <- function(pedon, max_cec_per_clay = 24,
                                    max_bulk_cec = 2) {
  h <- pedon$horizons
  tests <- list()
  tests$clay_above_8 <- test_numeric_above(h, "clay_pct", threshold = 8)
  tests$cec_per_clay <- test_cec_per_clay(h,
                                              max_cmol_per_kg_clay = max_cec_per_clay,
                                              candidate_layers = tests$clay_above_8$layers)
  # Alternative path: bulk CEC < 2.
  bulk <- integer(0); missing <- character(0); details <- list()
  for (i in seq_len(nrow(h))) {
    cec <- h$cec_cmol[i]
    if (is.na(cec)) { missing <- c(missing, "cec_cmol"); next }
    details[[as.character(i)]] <- list(idx = i, cec_cmol = cec,
                                        threshold = max_bulk_cec,
                                        passed = cec < max_bulk_cec)
    if (cec < max_bulk_cec) bulk <- c(bulk, i)
  }
  tests$bulk_cec_alt <- .subtest_result(
    passed = if (length(bulk) > 0L) TRUE
             else if (length(details) == 0L && length(missing) > 0L) NA
             else FALSE,
    layers = bulk, missing = missing, details = details
  )
  # Criterion 1: low-CEC layers (either path).
  crit1 <- union(tests$cec_per_clay$layers, tests$bulk_cec_alt$layers)
  crit1_na <- is.na(tests$cec_per_clay$passed) ||
                is.na(tests$bulk_cec_alt$passed)

  # Criterion 2: evidence of soil formation (cambic criterion 3) on the
  # SAME layers. Only assessed where criterion 1 holds.
  tests$soil_formation <- test_cambic_soil_formation(h,
                                                       candidate_layers = crit1)
  shared <- intersect(crit1, tests$soil_formation$layers)

  passed <- if (length(shared) > 0L) TRUE
            else if (length(crit1) == 0L) {
              if (crit1_na) NA else FALSE
            } else if (is.na(tests$soil_formation$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "sideralic_properties",
    passed = passed, layers = shared,
    evidence = tests,
    missing = unique(c(tests$cec_per_clay$missing,
                         tests$bulk_cec_alt$missing,
                         tests$soil_formation$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.13"
  )
}


#' Takyric properties (WRB 2022 Ch 3.2.15) -- per-pedon test wrapping
#' \code{test_takyric_surface}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
takyric_properties <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$crust <- test_takyric_surface(h)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "takyric_properties",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.15"
  )
}


#' Vitric properties (WRB 2022 Ch 3.2.16)
#'
#' Volcanic glass \\>= 5\% in 0.02-2 mm fraction, Al_ox + 1/2 Fe_ox \\>=
#' 0.4\%, phosphate retention \\>= 25\%.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_glass_pct Numeric threshold or option (see Details).
#' @param min_alfe Numeric threshold or option (see Details).
#' @param min_p_retention Numeric threshold or option (see Details).
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
vitric_properties <- function(pedon, min_glass_pct = 5,
                                 min_alfe = 0.4,
                                 min_p_retention = 25) {
  h <- pedon$horizons
  tests <- list()
  tests$volcanic_glass <- test_numeric_above(h, "volcanic_glass_pct",
                                                threshold = min_glass_pct)
  tests$alfe_ox <- test_alfe_ox_above(h, min_pct = min_alfe,
                                          candidate_layers = tests$volcanic_glass$layers)
  tests$phosphate_ret <- test_numeric_above(h, "phosphate_retention_pct",
                                                threshold = min_p_retention,
                                                candidate_layers = tests$alfe_ox$layers)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "vitric_properties",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.16"
  )
}

# Vitric properties per layer (WRB 2022 Ch 3.2.16, p 83) as TRUE / FALSE /
# NA: all three criteria (volcanic glass >= 5%, Alox + 1/2 Feox >= 0.4%,
# phosphate retention >= 25%) met, one of them failed, or not known. Without
# volcanic glass data the layer stays unknown (Hugo, N3: no proxy). Read by
# andosol() to tell a missing value from an absent property.
.vitric_layer_status <- function(h, min_glass_pct = 5, min_alfe = 0.4,
                                 min_p_retention = 25) {
  n <- nrow(h)
  glass <- h$volcanic_glass_pct %||% rep(NA_real_, n)
  al <- h$al_ox_pct %||% rep(NA_real_, n)
  fe <- h$fe_ox_pct %||% rep(NA_real_, n)
  pr <- h$phosphate_retention_pct %||% rep(NA_real_, n)
  alfe <- al + fe / 2
  alfe_ok <- ifelse(!is.na(alfe), alfe >= min_alfe,
                    ifelse(!is.na(al) & al >= min_alfe, TRUE, NA))
  (glass >= min_glass_pct) & alfe_ok & (pr >= min_p_retention)
}


#' Yermic properties (WRB 2022 Ch 3.2.17) -- per-pedon test wrapping
#' \code{test_yermic_surface}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}} recording whether the diagnostic is present, the qualifying layers, and the supporting evidence.
#' @export
yermic_properties <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$pavement <- test_yermic_surface(h)
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "yermic_properties",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.17"
  )
}
