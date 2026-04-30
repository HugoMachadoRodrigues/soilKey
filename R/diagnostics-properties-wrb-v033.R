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
#' meeting all of:
#' \itemize{
#'   \item underlying clay \\>= 15\% AND thickness \\>= 7.5 cm;
#'   \item underlying starts \\>= 10 cm below mineral soil surface;
#'   \item underlying has, vs overlying: 2x clay if overlying < 20\%, OR
#'         \\>= 20pp (absolute) more clay if overlying \\>= 20\%;
#'   \item transitional layer, if any, \\<= 2 cm.
#' }
#' v0.3.3 enforces criteria 1, 2, 3. The transitional-layer check is
#' deferred (the canonical horizon schema does not carry a "transitional"
#' marker; it can be added later via boundary_distinctness inspection).
#' @param pedon A \code{\link{PedonRecord}}.
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
    starts_below_10   <- top >= 10
    jump_ok <- if (above < 20) here >= 2 * above
               else here - above >= 20
    layer_pass <- underlying_clay_ok && thickness_ok &&
                    starts_below_10  && jump_ok
    details[[as.character(i)]] <- list(
      between = c(i - 1L, i),
      above_clay = above, here_clay = here,
      thickness_cm = bot - top, top_cm = top,
      jump_path = if (above < 20) "2x" else "+20pp",
      passed = layer_pass
    )
    if (layer_pass) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE
  DiagnosticResult$new(
    name      = "abrupt_textural_difference",
    passed    = passed,
    layers    = passing,
    evidence  = list(layer_pairs = details),
    missing   = unique(missing),
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.1"
  )
}


#' Albeluvic glossae (WRB 2022 Ch 3.2.2)
#'
#' Tongues of bleached, coarser-textured material penetrating an argic
#' horizon. v0.3.3 detects via designation pattern \code{glossic|albeluvic}
#' on a layer that overlies an argic-horizon-passing layer.
#' @param pedon A \code{\link{PedonRecord}}.
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
#' @export
continuous_rock <- function(pedon) {
  h <- pedon$horizons
  tests <- list()
  tests$designation <- test_pattern_match(h, "designation",
                                              "^R$|^Cr|^Rk")
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
#' Visible secondary carbonate accumulations, less than the calcic gate.
#' Detects via caco3_pct between 0.5 and the calcic threshold (15) AND
#' designation effervescence pattern (\code{k}).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_caco3_pct Numeric threshold or option (see Details).
#' @param max_caco3_pct Numeric threshold or option (see Details).
#' @export
protocalcic_properties <- function(pedon, min_caco3_pct = 0.5,
                                      max_caco3_pct = 15) {
  h <- pedon$horizons
  tests <- list()
  tests$caco3_low <- test_numeric_above(h, "caco3_pct",
                                            threshold = min_caco3_pct)
  if (isTRUE(tests$caco3_low$passed)) {
    high <- test_numeric_above(h, "caco3_pct",
                                  threshold = max_caco3_pct,
                                  candidate_layers = tests$caco3_low$layers)
    not_calcic <- setdiff(tests$caco3_low$layers, high$layers)
    if (length(not_calcic) == 0L) {
      tests$caco3_low$passed <- FALSE
      tests$caco3_low$layers <- integer(0)
    } else {
      tests$caco3_low$layers <- not_calcic
    }
  }
  desg <- test_pattern_match(h, "designation", "k|Bk|Bw\\(k\\)")
  if (!isTRUE(tests$caco3_low$passed) && isTRUE(desg$passed)) {
    tests$designation_proxy <- desg
  }
  agg <- aggregate_subtests(tests)
  DiagnosticResult$new(
    name = "protocalcic_properties",
    passed = agg$passed, layers = agg$layers,
    evidence = tests, missing = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.8"
  )
}


#' Protogypsic properties (WRB 2022 Ch 3.2.9): visible secondary gypsum
#' \\>= 1\% but below the gypsic gate.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_caso4_pct Numeric threshold or option (see Details).
#' @param max_caso4_pct Numeric threshold or option (see Details).
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
#' \code{\link{test_reducing_conditions}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_redox_pct Numeric threshold or option (see Details).
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
#' \code{\link{test_shrink_swell_cracks}}.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_width_cm Numeric threshold or option (see Details).
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
#' Mineral material with low CEC: clay >= 8\% AND CEC/clay < 24, OR
#' bulk CEC < 2 cmol_c/kg soil. Plus evidence of soil formation
#' (cambic-style criterion 3).
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cec_per_clay Numeric threshold or option (see Details).
#' @param max_bulk_cec Numeric threshold or option (see Details).
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
  shared <- union(tests$cec_per_clay$layers, tests$bulk_cec_alt$layers)
  passed <- if (length(shared) > 0L) TRUE
            else if (is.na(tests$cec_per_clay$passed) ||
                     is.na(tests$bulk_cec_alt$passed)) NA
            else FALSE
  DiagnosticResult$new(
    name = "sideralic_properties",
    passed = passed, layers = shared,
    evidence = tests,
    missing = unique(c(tests$cec_per_clay$missing,
                         tests$bulk_cec_alt$missing)),
    reference = "IUSS Working Group WRB (2022), Chapter 3.2.13"
  )
}


#' Takyric properties (WRB 2022 Ch 3.2.15) -- per-pedon test wrapping
#' \code{\link{test_takyric_surface}}.
#' @param pedon A \code{\link{PedonRecord}}.
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


#' Yermic properties (WRB 2022 Ch 3.2.17) -- per-pedon test wrapping
#' \code{\link{test_yermic_surface}}.
#' @param pedon A \code{\link{PedonRecord}}.
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
