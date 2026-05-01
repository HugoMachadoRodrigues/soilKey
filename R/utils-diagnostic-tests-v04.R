# ================================================================
# WRB 2022 -- v0.4 diagnostic test helpers
#
# This file collects the additional sub-test predicates introduced
# in v0.9.15 to lift the seven "v0.3 simplified" diagnostics to
# canonical WRB 2022 coverage:
#
#   1. test_oc_cumulative_thickness    -- histic cumulative variant
#   2. test_coarse_fragments_above     -- leptic coarse-fragments path
#   3. test_permafrost_temp_below      -- cryic permafrost-temp path
#   4. test_phosphate_retention_above  -- andic alternative
#   5. test_volcanic_glass_above       -- andic alternative
#   6. test_geomembrane_within_depth   -- technic alternative
#   7. test_technic_hardmaterial_at_surface -- technic alternative
#   8. test_polyhedral_or_nutty_structure   -- nitic supplementary
#   9. test_clay_decreases_with_depth       -- nitic supplementary
#  10. test_shiny_ped_surfaces              -- nitic supplementary
#  11. test_anthric_horizon_properties      -- anthric full impl
#
# Plus an OR-aggregator for diagnostics that key on any of several
# alternative paths (used by histic, leptic, cryic, andic, technic).
# ================================================================


#' Aggregate alternative-path subtests with OR semantics
#'
#' Each "path" is a named list of subtests that combine with AND
#' (intersect their layers). Paths combine with OR: the diagnostic
#' passes if any path passes; passing layers are the union across
#' passing paths; missing attributes are unioned across all paths
#' that did not pass and reported NA. Used by diagnostics where WRB
#' specifies several alternative qualifying conditions.
#'
#' @param paths Named list of named subtest lists. Each inner list is
#'        a set of subtests that combine with AND.
#' @return A list with elements \code{passed}, \code{layers},
#'         \code{missing}, and \code{passing_path} (the name of the
#'         first path that passed, or \code{NA_character_}).
#' @keywords internal
aggregate_alternatives <- function(paths) {
  per_path <- lapply(paths, function(p) aggregate_subtests(p))

  passing_idx <- which(vapply(per_path,
                                function(x) isTRUE(x$passed),
                                logical(1)))
  na_idx      <- which(vapply(per_path,
                                function(x) is.na(x$passed),
                                logical(1)))

  if (length(passing_idx) > 0L) {
    layers <- Reduce(union,
                     lapply(per_path[passing_idx], function(x) x$layers))
    return(list(
      passed       = TRUE,
      layers       = if (is.null(layers)) integer(0) else layers,
      missing      = character(0),
      passing_path = names(per_path)[passing_idx[1]]
    ))
  }

  missing <- unique(unlist(lapply(per_path, function(x) x$missing)))
  if (is.null(missing)) missing <- character(0)
  passed <- if (length(na_idx) > 0L && length(missing) > 0L) NA else FALSE

  list(passed       = passed,
       layers       = integer(0),
       missing      = missing,
       passing_path = NA_character_)
}


# ============================================================ histic =====

#' Test cumulative organic-carbon thickness within a depth window
#'
#' WRB 2022 alternative criterion for the histic horizon: organic
#' material >= \code{min_oc} \% summing to \code{min_thickness_cm}
#' cumulative thickness within the upper \code{max_depth_cm}, even if
#' no single contiguous layer reaches the standard 10 cm. Relevant for
#' folic / mossy Histosols on slopes.
#'
#' @keywords internal
test_oc_cumulative_thickness <- function(h,
                                            min_oc          = 12,
                                            min_thickness_cm = 40,
                                            max_depth_cm     = 80) {
  if (is.null(h) || nrow(h) == 0L)
    return(.subtest_result(passed = FALSE, layers = integer(0)))

  oc <- h$oc_pct
  top  <- h$top_cm
  bot  <- h$bottom_cm

  cumul <- 0
  passing <- integer(0)
  missing <- character(0)
  for (i in seq_len(nrow(h))) {
    if (is.na(top[i]) || is.na(bot[i])) {
      missing <- c(missing, "top_cm", "bottom_cm")
      next
    }
    if (top[i] >= max_depth_cm) next
    if (is.na(oc[i])) {
      missing <- c(missing, "oc_pct")
      next
    }
    if (oc[i] < min_oc) next
    eff_bot <- min(bot[i], max_depth_cm)
    cumul   <- cumul + (eff_bot - top[i])
    passing <- c(passing, i)
  }
  passed <- if (cumul >= min_thickness_cm) TRUE
            else if (length(passing) == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = if (isTRUE(passed)) passing else integer(0),
    missing = unique(missing),
    details = list(cumulative_thickness_cm = cumul,
                   min_thickness_cm        = min_thickness_cm,
                   max_depth_cm            = max_depth_cm)
  )
}


# ============================================================ leptic =====

#' Test coarse-fragments percent above a threshold
#'
#' WRB 2022 alternative criterion for the leptic feature: coarse
#' fragments >= \code{min_pct} \% by volume in a layer within
#' \code{max_depth} of the surface. Used as an OR-alternative to the
#' R / Cr designation pattern.
#'
#' @keywords internal
test_coarse_fragments_above <- function(h,
                                           min_pct  = 90,
                                           max_top_cm = 25) {
  if (is.null(h) || nrow(h) == 0L)
    return(.subtest_result(passed = FALSE, layers = integer(0)))

  passing <- integer(0)
  missing <- character(0)
  for (i in seq_len(nrow(h))) {
    if (is.na(h$top_cm[i])) {
      missing <- c(missing, "top_cm")
      next
    }
    if (h$top_cm[i] > max_top_cm) next
    if (is.na(h$coarse_fragments_pct[i])) {
      missing <- c(missing, "coarse_fragments_pct")
      next
    }
    if (h$coarse_fragments_pct[i] >= min_pct) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                  missing = unique(missing))
}


# ============================================================ cryic =====

#' Test mean annual permafrost-zone temperature at or below threshold
#'
#' WRB 2022 alternative criterion for cryic conditions: a horizon
#' within the upper \code{max_depth_cm} reporting \code{permafrost_temp_C}
#' at or below \code{max_temp_C} (default 0 C). Used as an explicit
#' OR-alternative to the designation-pattern path.
#'
#' @keywords internal
test_permafrost_temp_below <- function(h,
                                          max_temp_C   = 0,
                                          max_top_cm   = 100) {
  if (is.null(h) || nrow(h) == 0L)
    return(.subtest_result(passed = FALSE, layers = integer(0)))

  passing <- integer(0)
  missing <- character(0)
  for (i in seq_len(nrow(h))) {
    if (is.na(h$top_cm[i])) {
      missing <- c(missing, "top_cm")
      next
    }
    if (h$top_cm[i] > max_top_cm) next
    if (is.na(h$permafrost_temp_C[i])) {
      missing <- c(missing, "permafrost_temp_C")
      next
    }
    if (h$permafrost_temp_C[i] <= max_temp_C)
      passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                  missing = unique(missing))
}


# ============================================================ andic =====

#' Test phosphate retention above threshold
#'
#' WRB 2022 alternative for andic properties: P retention >= 70 \%.
#'
#' @keywords internal
test_phosphate_retention_above <- function(h,
                                              min_pct           = 70,
                                              candidate_layers  = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  for (i in cl) {
    v <- h$phosphate_retention_pct[i]
    if (is.na(v)) {
      missing <- c(missing, "phosphate_retention_pct")
      next
    }
    if (v >= min_pct) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                  missing = unique(missing))
}


#' Test volcanic glass content above threshold
#'
#' WRB 2022 alternative for andic properties: glass content >= 30 \%
#' in the 0.02--2 mm sand fraction.
#'
#' @keywords internal
test_volcanic_glass_above <- function(h,
                                         min_pct           = 30,
                                         candidate_layers  = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  for (i in cl) {
    v <- h$volcanic_glass_pct[i]
    if (is.na(v)) {
      missing <- c(missing, "volcanic_glass_pct")
      next
    }
    if (v >= min_pct) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                  missing = unique(missing))
}


# ============================================================ technic =====

#' Test for a continuous geomembrane within a depth window
#'
#' WRB 2022 alternative for technic features: any layer with
#' \code{geomembrane_present == TRUE} within the upper
#' \code{max_top_cm}.
#'
#' @keywords internal
test_geomembrane_within_depth <- function(h, max_top_cm = 100) {
  if (is.null(h) || nrow(h) == 0L || !"geomembrane_present" %in% names(h))
    return(.subtest_result(passed = FALSE, layers = integer(0),
                            missing = "geomembrane_present"))

  passing <- integer(0)
  missing <- character(0)
  for (i in seq_len(nrow(h))) {
    if (is.na(h$top_cm[i])) {
      missing <- c(missing, "top_cm")
      next
    }
    if (h$top_cm[i] > max_top_cm) next
    if (is.na(h$geomembrane_present[i])) {
      missing <- c(missing, "geomembrane_present")
      next
    }
    if (isTRUE(h$geomembrane_present[i])) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                  missing = unique(missing))
}


#' Test for technic hard material covering the surface
#'
#' WRB 2022 alternative for technic features: a layer at the surface
#' (top_cm <= \code{max_top_cm}, default 5) with
#' \code{technic_hardmaterial_pct >= min_pct} (default 95).
#'
#' @keywords internal
test_technic_hardmaterial_at_surface <- function(h,
                                                    min_pct    = 95,
                                                    max_top_cm = 5) {
  if (is.null(h) || nrow(h) == 0L
      || !"technic_hardmaterial_pct" %in% names(h))
    return(.subtest_result(passed = FALSE, layers = integer(0),
                            missing = "technic_hardmaterial_pct"))

  passing <- integer(0)
  missing <- character(0)
  for (i in seq_len(nrow(h))) {
    if (is.na(h$top_cm[i])) {
      missing <- c(missing, "top_cm")
      next
    }
    if (h$top_cm[i] > max_top_cm) next
    if (is.na(h$technic_hardmaterial_pct[i])) {
      missing <- c(missing, "technic_hardmaterial_pct")
      next
    }
    if (h$technic_hardmaterial_pct[i] >= min_pct)
      passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                  missing = unique(missing))
}


# ============================================================ nitic =====

#' Test for polyhedral / nutty structure type
#'
#' WRB 2022 supplementary criterion for the nitic horizon:
#' \code{structure_type} matches "polyhedral" or "nutty"
#' (case-insensitive). Treated as evidence-supportive: when the field
#' is missing entirely, the diagnostic does not fail; when present and
#' clearly NOT polyhedral/nutty, the diagnostic fails.
#'
#' @keywords internal
test_polyhedral_or_nutty_structure <- function(h,
                                                  candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  if (length(cl) == 0L)
    return(.subtest_result(passed = FALSE, layers = integer(0)))

  st <- h$structure_type
  if (all(is.na(st[cl]))) {
    return(.subtest_result(passed = NA, layers = cl,
                            missing = "structure_type"))
  }
  passing <- cl[grepl("polyhedr|nutty|sub.?angular.*block",
                        st[cl], ignore.case = TRUE)]
  if (length(passing) == 0L) {
    # No structure layer matches -- treat as soft fail.
    return(.subtest_result(passed = FALSE, layers = integer(0),
                            details = list(structure_types = st[cl])))
  }
  .subtest_result(passed = TRUE, layers = passing,
                  details = list(structure_types = st[cl]))
}


#' Test that clay does NOT decrease abruptly with depth (nitic)
#'
#' WRB 2022 supplementary criterion for the nitic horizon: clay
#' percent should NOT show a maximum at the top of the B with abrupt
#' decrease below. Operationally: across the candidate layers,
#' clay_pct must not drop by more than \code{max_drop_pct} between
#' consecutive layers within 50 cm depth. Returns NA when clay is
#' missing in fewer than two candidate layers.
#'
#' @keywords internal
test_clay_decreases_with_depth <- function(h,
                                              candidate_layers = NULL,
                                              max_drop_pct = 8,
                                              max_depth_cm = 50) {
  cl <- .candidate_layers(h, candidate_layers)
  cl <- cl[!is.na(h$top_cm[cl]) & h$top_cm[cl] <= max_depth_cm]
  if (length(cl) < 2L)
    return(.subtest_result(passed = TRUE, layers = cl,
                            details = list(reason = "fewer than two layers")))
  ord    <- order(h$top_cm[cl])
  cl_ord <- cl[ord]
  clay   <- h$clay_pct[cl_ord]
  if (sum(!is.na(clay)) < 2L)
    return(.subtest_result(passed = NA, layers = cl_ord,
                            missing = "clay_pct"))
  drops <- diff(clay)
  if (any(!is.na(drops) & drops < -max_drop_pct))
    return(.subtest_result(passed = FALSE, layers = integer(0),
                            details = list(drops_pct = drops,
                                            max_drop_pct = max_drop_pct)))
  .subtest_result(passed = TRUE, layers = cl_ord,
                  details = list(drops_pct = drops))
}


#' Test for shiny ped surfaces (informational only)
#'
#' WRB 2022 mentions shiny faces of polyhedral peds as a supportive
#' criterion for the nitic horizon. The horizon schema does not carry
#' a dedicated "shiny_peds" field; \code{slickensides} is a poor proxy
#' (slickensides are shrink-swell features, distinct from
#' Fe-oxide-coated polyhedral ped faces). This predicate therefore
#' returns the slickensides evidence non-gating: the result is always
#' \code{passed = NA} when the field is missing or "absent" (no
#' evidence either way) and \code{TRUE} when slickensides is present
#' (taken as suggestive). The diagnostic does not fail on this test.
#'
#' @keywords internal
test_shiny_ped_surfaces <- function(h, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  if (length(cl) == 0L)
    return(.subtest_result(passed = NA, layers = integer(0)))
  s <- h$slickensides[cl]
  if (all(is.na(s)))
    return(.subtest_result(passed = NA, layers = cl,
                            missing = "slickensides"))
  passing <- cl[!is.na(s) & !grepl("^absent$|^none$", s, ignore.case = TRUE)]
  if (length(passing) == 0L)
    return(.subtest_result(passed = NA, layers = integer(0),
                            details = list(slickensides = s,
                                            note = "slickensides absent; not gating")))
  .subtest_result(passed = TRUE, layers = passing,
                  details = list(slickensides = s))
}


# ============================================================ anthric =====

#' Test for anthric / pretic / hortic / plaggic / terric / irragric
#' horizon properties (full diagnostic)
#'
#' WRB 2022 specifies five anthropogenic surface horizons that are all
#' diagnostic for Anthrosols. Rather than relying on the designation
#' pattern alone, this predicate also checks property-based evidence:
#' a surface horizon (top_cm <= 5) with elevated dark colour
#' (Munsell value <= 4 moist) AND elevated plant-available P
#' (\code{p_mehlich3_mg_kg} >= 50) AND minimum thickness 20 cm.
#' Either path (designation OR property-based) qualifies.
#'
#' @keywords internal
test_anthric_horizon_properties <- function(h, min_thickness_cm = 20,
                                                min_p_mg_kg     = 50,
                                                max_munsell_value = 4) {
  if (is.null(h) || nrow(h) == 0L)
    return(.subtest_result(passed = FALSE, layers = integer(0)))

  passing <- integer(0)
  missing <- character(0)
  for (i in seq_len(nrow(h))) {
    if (is.na(h$top_cm[i]) || is.na(h$bottom_cm[i])) {
      missing <- c(missing, "top_cm", "bottom_cm")
      next
    }
    if (h$top_cm[i] > 5) next
    thick <- h$bottom_cm[i] - h$top_cm[i]
    if (thick < min_thickness_cm) next
    p_ok    <- !is.na(h$p_mehlich3_mg_kg[i])    && h$p_mehlich3_mg_kg[i] >= min_p_mg_kg
    dark_ok <- !is.na(h$munsell_value_moist[i]) && h$munsell_value_moist[i] <= max_munsell_value
    if (is.na(h$p_mehlich3_mg_kg[i]))    missing <- c(missing, "p_mehlich3_mg_kg")
    if (is.na(h$munsell_value_moist[i])) missing <- c(missing, "munsell_value_moist")
    if (p_ok && dark_ok) passing <- c(passing, i)
  }
  passed <- if (length(passing) > 0L) TRUE
            else if (length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                  missing = unique(missing))
}
