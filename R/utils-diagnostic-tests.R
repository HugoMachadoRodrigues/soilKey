# ================================================================
# Diagnostic sub-tests
#
# These primitives are the atoms of WRB and SiBCS diagnostic functions.
# Each sub-test:
#   - takes a horizons data.table (the canonical schema)
#   - optionally takes candidate_layers to restrict its scope
#   - returns a list with: passed, layers, missing, details, notes
#
# Sub-tests never throw on NA. They return passed = NA when the inputs
# they need are absent for all candidate layers, and report the missing
# attribute names. The diagnostic-level functions (argic, ferralic,
# mollic) aggregate these results and emit DiagnosticResult objects.
# ================================================================


# ---------------------------------------------------------------- helpers ----
#' Internal helper: .subtest_result

#' @keywords internal
.subtest_result <- function(passed,
                             layers  = integer(0),
                             missing = character(0),
                             details = NULL,
                             notes   = NA_character_) {
  list(
    passed  = passed,
    layers  = as.integer(layers),
    missing = unique(as.character(missing)),
    details = details,
    notes   = notes
  )
}
#' Internal helper: .candidate_layers

#' @keywords internal
.candidate_layers <- function(h, candidate_layers = NULL) {
  if (is.null(candidate_layers)) seq_len(nrow(h))
  else as.integer(candidate_layers)
}

#' Texture predicate: "sandy loam or finer"
#'
#' WRB 2022 (Annex 1) and the USDA texture triangle agree on
#' \code{silt + 2 * clay >= 30} as the boundary between loamy sand and
#' sandy loam. Returns \code{TRUE}/\code{FALSE}/\code{NA}.
#'
#' @param sand,silt,clay Numeric percentages.
#' @keywords internal
is_sandy_loam_or_finer <- function(sand, silt, clay) {
  if (is.na(sand) || is.na(silt) || is.na(clay)) return(NA)
  silt + 2 * clay >= 30
}

#' Texture predicate: "loamy sand or finer"
#'
#' Boundary: \code{silt + 2 * clay >= 15}. Returns
#' \code{TRUE}/\code{FALSE}/\code{NA}.
#'
#' @keywords internal
is_loamy_sand_or_finer <- function(sand, silt, clay) {
  if (is.na(sand) || is.na(silt) || is.na(clay)) return(NA)
  silt + 2 * clay >= 15
}

#' CEC per kg clay (cmol_c)
#'
#' \code{cec_cmol * 100 / clay_pct}. Returns \code{NA} when either input is
#' missing or \code{clay_pct <= 0}.
#'
#' @keywords internal
cec_per_clay <- function(cec_cmol, clay_pct) {
  if (is.na(cec_cmol) || is.na(clay_pct) || clay_pct <= 0) return(NA_real_)
  cec_cmol * 100 / clay_pct
}

#' Effective CEC from sum of bases plus exchangeable Al
#'
#' If any of \code{ca_cmol}, \code{mg_cmol}, \code{k_cmol}, \code{na_cmol},
#' \code{al_cmol} are missing, returns \code{NA}.
#'
#' @keywords internal
compute_ecec <- function(ca, mg, k, na, al) {
  parts <- c(ca, mg, k, na, al)
  if (any(is.na(parts))) return(NA_real_)
  sum(parts)
}

#' ECEC per kg clay (cmol_c)
#'
#' @keywords internal
ecec_per_clay <- function(ecec_cmol, clay_pct) {
  if (is.na(ecec_cmol) || is.na(clay_pct) || clay_pct <= 0) return(NA_real_)
  ecec_cmol * 100 / clay_pct
}


# =========================================================== argic sub-tests ====

#' Test the argic / argillic clay-increase criterion
#'
#' Tests every horizon in the profile against the clay-increase rules
#' of either WRB 2022 (default, \code{system = "wrb2022"}) or USDA Soil
#' Taxonomy 13th edition (\code{system = "usda"}). The two systems
#' use the SAME structural rule (three brackets keyed on overlying
#' eluvial clay percent) but DIFFERENT thresholds:
#'
#' \tabular{lll}{
#'   \strong{Eluvial clay} \tab \strong{WRB 2022 argic} \tab \strong{KST 13ed argillic} \cr
#'   \code{< 15\%}     \tab \code{>= +6 pp absolute}    \tab \code{>= +3 pp absolute} \cr
#'   \code{15-X\%}     \tab \code{>= 1.4x ratio} (X=50) \tab \code{>= 1.2x ratio} (X=40) \cr
#'   \code{>= X\%}     \tab \code{>= +20 pp absolute}   \tab \code{>= +8 pp absolute}
#' }
#'
#' KST 13ed thresholds are taken from Chapter 3, "Argillic horizon"
#' (p. 4); WRB 2022 thresholds from Chapter 3.1.3, "Argic horizon"
#' (p. 36). v0.9.26 introduces the per-system switch -- earlier
#' versions used WRB thresholds for both systems, which under-detected
#' the argillic horizon in KSSL profiles where clay increase is in
#' the 1.2-1.4 ratio band or +3 to +6 pp absolute band.
#'
#' Returns the indices of horizons that satisfy as argic candidates.
#'
#' @param h Horizons data.table (canonical schema).
#' @param system One of \code{"wrb2022"} (default) or \code{"usda"}.
#'        Selects the threshold set.
#' @return Sub-test result list.
#' @references IUSS Working Group WRB (2022), Chapter 3.1.3, Argic
#'   horizon, criteria 2.a.iv-vi (p. 36); Soil Survey Staff (2022),
#'   Keys to Soil Taxonomy 13th ed., Chapter 3, Argillic horizon (p. 4).
#' @export
test_clay_increase_argic <- function(h, system = c("wrb2022", "usda")) {
  system <- match.arg(system)
  if (nrow(h) < 2L) {
    return(.subtest_result(
      passed = FALSE,
      notes  = "Fewer than 2 horizons -- clay increase test inapplicable"
    ))
  }

  candidates <- integer(0)
  details    <- list()
  missing    <- character(0)

  # v0.9.23: KST 13ed Ch 3 (argillic horizon, p 4) and WRB 2022
  # Ch 3.1.3 (argic horizon, p 36) define the clay-increase test
  # as a comparison of the (illuvial) candidate horizon against the
  # OVERLYING ELUVIAL horizon, NOT against the immediate predecessor.
  # The canonical eluvial reference is the lowest-clay layer
  # ANYWHERE above the candidate (typically the E or A) -- NOT the
  # adjacent layer. The pre-v0.9.23 implementation only compared
  # i vs i-1, which missed gradual clay increases through a thick A
  # / E / Bw / Bt sequence (FEBR Hapludalfs were the obvious fail
  # mode: clay goes 13 -> 15 -> 21 -> 27 -> 31, no two adjacent
  # layers triggered, but the A-to-Bt jump 13 -> 31 is canonical).
  # We now also try a "min-above" reference and a "adjacent" check;
  # if EITHER triggers the candidate is accepted.

  for (i in seq.int(2L, nrow(h))) {
    here <- h$clay_pct[i]
    if (is.na(here)) {
      missing <- c(missing, "clay_pct")
      next
    }
    above_idx_set <- seq.int(1L, i - 1L)
    above_clays   <- h$clay_pct[above_idx_set]
    has_clay      <- !is.na(above_clays)
    if (!any(has_clay)) {
      missing <- c(missing, "clay_pct")
      next
    }
    # Reference 1: minimum clay above (canonical KST 13ed eluvial-
    # illuvial comparison).
    above_min     <- min(above_clays, na.rm = TRUE)
    above_min_idx <- above_idx_set[has_clay][which.min(above_clays[has_clay])]
    # Reference 2: immediate predecessor (back-compat with WRB
    # adjacent-layer interpretation when a thick eluvial is absent).
    above_adj     <- h$clay_pct[i - 1L]

    eval_rule <- function(above) {
      if (is.na(above)) return(list(passed = FALSE, rule = "NA above"))
      # Per-system thresholds. WRB 2022 (4th ed Ch 3.1.3 p 36) is
      # stricter; KST 13ed (Ch 3 p 4) is looser by design (USDA
      # argillic includes more profiles than WRB argic).
      if (system == "usda") {
        rule_label <- if (above < 15)      "<15%: +3pp absolute"
                      else if (above < 40) "15 to <40%: ratio >= 1.2"
                      else                 ">=40%: +8pp absolute"
        passed_rule <- if (above < 15)      here - above >= 3
                       else if (above < 40) here / above >= 1.2
                       else                 here - above >= 8
      } else {
        rule_label <- if (above < 15)      "<15%: +6pp absolute"
                      else if (above < 50) "15 to <50%: ratio >= 1.4"
                      else                 ">=50%: +20pp absolute"
        passed_rule <- if (above < 15)      here - above >= 6
                       else if (above < 50) here / above >= 1.4
                       else                 here - above >= 20
      }
      list(passed = passed_rule, rule = rule_label)
    }
    chk_min <- eval_rule(above_min)
    chk_adj <- eval_rule(above_adj)
    rule_passed <- isTRUE(chk_min$passed) || isTRUE(chk_adj$passed)
    rule_label  <- if (isTRUE(chk_min$passed)) sprintf("min-above (idx=%d, clay=%.1f): %s",
                                                          above_min_idx, above_min,
                                                          chk_min$rule)
                   else if (isTRUE(chk_adj$passed)) sprintf("adjacent (idx=%d, clay=%.1f): %s",
                                                              i - 1L, above_adj,
                                                              chk_adj$rule)
                   else sprintf("no rule passed (min=%.1f, adj=%.1f)",
                                  above_min, above_adj %||% NA_real_)

    details[[as.character(i)]] <- list(
      above_min_idx  = above_min_idx,
      above_min_clay = above_min,
      above_adj_clay = above_adj,
      here_idx       = i,
      here_clay      = here,
      rule           = rule_label,
      passed         = rule_passed
    )

    if (rule_passed) candidates <- c(candidates, i)
  }

  any_evaluable <- length(details) > 0L
  passed <- if (length(candidates) > 0L) TRUE
            else if (!any_evaluable && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = candidates,
    missing = missing,
    details = details
  )
}

#' Test minimum horizon thickness
#'
#' For each candidate layer, checks \code{bottom_cm - top_cm >= min_cm}.
#' Used by argic (default 7.5), ferralic (30), mollic (20), and others.
#'
#' @param h Horizons data.table.
#' @param min_cm Minimum thickness in cm.
#' @param candidate_layers Integer vector of horizon indices to test.
#'                         If NULL, all layers are tested.
#' @export
test_minimum_thickness <- function(h, min_cm = 7.5, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  if (length(cl) == 0L) {
    return(.subtest_result(passed = FALSE, layers = integer(0)))
  }

  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    top    <- h$top_cm[i]
    bottom <- h$bottom_cm[i]
    if (is.na(top) || is.na(bottom)) {
      missing <- c(missing, "top_cm", "bottom_cm")
      next
    }
    thick  <- bottom - top
    ok     <- thick >= min_cm
    details[[as.character(i)]] <- list(
      idx = i, top = top, bottom = bottom,
      thickness = thick, threshold = min_cm, passed = ok
    )
    if (ok) passing <- c(passing, i)
  }

  passed <- if (length(passing) > 0L) TRUE
            else if (length(details) == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = missing,
    details = details
  )
}

#' Test sandy-loam-or-finer texture (used by argic, ferralic)
#'
#' @param h Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_texture_argic <- function(h, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  if (length(cl) == 0L) {
    return(.subtest_result(passed = FALSE, layers = integer(0)))
  }

  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    s <- is_sandy_loam_or_finer(h$sand_pct[i], h$silt_pct[i], h$clay_pct[i])
    details[[as.character(i)]] <- list(
      idx = i, sand = h$sand_pct[i], silt = h$silt_pct[i],
      clay = h$clay_pct[i], result = s
    )
    if (is.na(s)) {
      missing <- c(missing, "sand_pct", "silt_pct", "clay_pct")
    } else if (s) {
      passing <- c(passing, i)
    }
  }

  evaluated <- sum(!vapply(details, function(d) is.na(d$result), logical(1)))
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = missing,
    details = details
  )
}

#' Test for albeluvic glossic features that exclude argic (-> Retisol path)
#'
#' v0.1 implementation: scans horizon designations for the substrings
#' \code{"glossic"} or \code{"albeluvic"}. A more rigorous implementation
#' would inspect tongue features, fragic properties, and morphological
#' descriptions; that is scheduled for v0.2.
#'
#' @keywords internal
test_not_albeluvic <- function(h) {
  flags <- grepl("glossic|albeluvic|retic", h$designation,
                 ignore.case = TRUE)
  if (any(flags, na.rm = TRUE)) {
    .subtest_result(
      passed = FALSE,
      notes  = sprintf(
        "Glossic/albeluvic/retic feature detected at horizon(s) %s -- Retisol path",
        paste(which(flags), collapse = ", ")
      )
    )
  } else {
    .subtest_result(passed = TRUE, layers = seq_len(nrow(h)))
  }
}


# ========================================================= ferralic sub-tests ====

#' Test CEC (1M NH4OAc, pH 7) per kg clay <= threshold
#'
#' Default threshold is 16 cmol_c/kg clay (WRB 2022 ferralic horizon).
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_cmol_per_kg_clay Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_cec_per_clay <- function(h, max_cmol_per_kg_clay = 16,
                                candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    cpc <- cec_per_clay(h$cec_cmol[i], h$clay_pct[i])
    details[[as.character(i)]] <- list(
      idx = i, cec_cmol = h$cec_cmol[i], clay_pct = h$clay_pct[i],
      cec_per_clay = cpc, threshold = max_cmol_per_kg_clay
    )
    if (is.na(cpc)) {
      if (is.na(h$cec_cmol[i]))  missing <- c(missing, "cec_cmol")
      if (is.na(h$clay_pct[i]))  missing <- c(missing, "clay_pct")
      next
    }
    details[[as.character(i)]]$passed <- cpc <= max_cmol_per_kg_clay
    if (cpc <= max_cmol_per_kg_clay) passing <- c(passing, i)
  }

  evaluated <- sum(vapply(details, function(d) !is.null(d$passed), logical(1)))
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = missing,
    details = details
  )
}

#' Test effective CEC (sum of bases + Al) per kg clay <= threshold
#'
#' Default threshold is 12 cmol_c/kg clay (WRB 2022 ferralic horizon). If
#' \code{ecec_cmol} is missing, computes ECEC from \code{ca_cmol +
#' mg_cmol + k_cmol + na_cmol + al_cmol} when those are available.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_cmol_per_kg_clay Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_ecec_per_clay <- function(h, max_cmol_per_kg_clay = 12,
                                 candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    ecec <- h$ecec_cmol[i]
    if (is.na(ecec)) {
      ecec <- compute_ecec(h$ca_cmol[i], h$mg_cmol[i],
                            h$k_cmol[i], h$na_cmol[i], h$al_cmol[i])
    }
    epc <- ecec_per_clay(ecec, h$clay_pct[i])
    details[[as.character(i)]] <- list(
      idx = i, ecec_cmol = ecec, clay_pct = h$clay_pct[i],
      ecec_per_clay = epc, threshold = max_cmol_per_kg_clay
    )
    if (is.na(epc)) {
      if (is.na(ecec)) {
        missing <- c(missing, "ecec_cmol or ca_cmol+mg_cmol+k_cmol+na_cmol+al_cmol")
      }
      if (is.na(h$clay_pct[i])) missing <- c(missing, "clay_pct")
      next
    }
    details[[as.character(i)]]$passed <- epc <= max_cmol_per_kg_clay
    if (epc <= max_cmol_per_kg_clay) passing <- c(passing, i)
  }

  evaluated <- sum(vapply(details, function(d) !is.null(d$passed), logical(1)))
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = missing,
    details = details
  )
}

#' Ferralic minimum thickness >= 30 cm (WRB 2022)
#'
#' Wraps \code{\link{test_minimum_thickness}}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_cm Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_ferralic_thickness <- function(h, min_cm = 30, candidate_layers = NULL) {
  test_minimum_thickness(h, min_cm = min_cm,
                          candidate_layers = candidate_layers)
}

#' Ferralic texture: sandy loam or finer (same predicate as argic)
#'
#' @param h Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_ferralic_texture <- function(h, candidate_layers = NULL) {
  test_texture_argic(h, candidate_layers = candidate_layers)
}


# =========================================================== mollic sub-tests ====

#' Mollic Munsell color test (WRB 2022)
#'
#' Moist value <= 3 AND moist chroma <= 3 AND dry value <= 5. If
#' \code{munsell_value_dry} is missing, uses the conservative substitute
#' \code{munsell_value_moist + 1}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_value_moist Numeric threshold or option (see Details).
#' @param max_chroma_moist Numeric threshold or option (see Details).
#' @param max_value_dry Numeric threshold or option (see Details).
#' @param candidate_layers Optional restriction.
#' @param allow_oc_inference If \code{TRUE} (default), accept OC \\>=
#'        1.5 \% in a surface A horizon as evidence of dark colour
#'        when both moist and dry Munsell are missing.
#' @export
test_mollic_color <- function(h,
                                max_value_moist  = 3,
                                max_chroma_moist = 3,
                                max_value_dry    = 5,
                                candidate_layers = NULL,
                                allow_oc_inference = TRUE) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    vm <- h$munsell_value_moist[i]
    cm <- h$munsell_chroma_moist[i]
    vd <- h$munsell_value_dry[i]
    oc <- h$oc_pct[i]

    has_moist <- !is.na(vm) && !is.na(cm)
    has_dry   <- !is.na(vd)

    # Path 1 (canonical): full moist Munsell.
    if (has_moist) {
      moist_ok <- vm <= max_value_moist && cm <= max_chroma_moist
      dry_ok   <- if (has_dry) vd <= max_value_dry
                  else         (vm + 1) <= max_value_dry
      details[[as.character(i)]] <- list(
        idx = i, source = "moist_munsell",
        value_moist  = vm, chroma_moist = cm, value_dry = vd,
        moist_ok = moist_ok, dry_ok = dry_ok,
        passed = moist_ok && dry_ok
      )
      if (moist_ok && dry_ok) passing <- c(passing, i)
      next
    }

    # Path 2 (v0.9.18): only dry Munsell available -- use the dry
    # value test. Empirical correspondence: a value moist ~ value
    # dry - 1, so a horizon with value_dry <= max_value_dry that
    # also reports chroma_moist (or chroma_dry) <= max_chroma_moist
    # qualifies. When chroma is also missing we accept on the dry
    # value alone, because moisture darkens chroma too.
    if (has_dry) {
      dry_ok <- vd <= max_value_dry
      cd <- if ("munsell_chroma_dry" %in% names(h)) h$munsell_chroma_dry[i]
            else NA_real_
      chroma_evidence <- if (!is.na(cm)) cm
                          else if (!is.na(cd)) cd
                          else NA_real_
      chroma_ok <- is.na(chroma_evidence) || chroma_evidence <= max_chroma_moist
      details[[as.character(i)]] <- list(
        idx = i, source = "dry_munsell_only",
        value_dry = vd, chroma_evidence = chroma_evidence,
        dry_ok = dry_ok, chroma_ok = chroma_ok,
        passed = dry_ok && chroma_ok
      )
      if (dry_ok && chroma_ok) passing <- c(passing, i)
      next
    }

    # Path 3 (v0.9.18): no Munsell at all -- infer dark colour from
    # high OC. Empirical convention: oc_pct >= 1.5 in a surface A
    # horizon implies value moist <= 3 in nearly all tropical /
    # temperate Mollic / Umbric / Chernozemic / Phaeozem profiles
    # (Embrapa Manual de Metodos 2017; KST 13ed Ch 3 commentary on
    # mollic indicator profile descriptions). The fallback only
    # fires when allow_oc_inference is TRUE (default) AND OC is
    # measured; when OC is also missing we record the field as
    # missing and the layer remains unevaluated.
    if (allow_oc_inference && !is.na(oc) && oc >= 1.5) {
      details[[as.character(i)]] <- list(
        idx = i, source = "oc_inferred",
        oc_pct = oc, threshold = 1.5,
        passed = TRUE
      )
      passing <- c(passing, i)
      next
    }

    if (is.na(vm) && is.na(vd)) {
      missing <- c(missing, "munsell_value_moist", "munsell_value_dry")
    }
    if (is.na(cm) && !"munsell_chroma_dry" %in% names(h)) {
      missing <- c(missing, "munsell_chroma_moist")
    }
    if (allow_oc_inference && is.na(oc)) missing <- c(missing, "oc_pct")
  }

  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = unique(missing),
    details = details
  )
}

#' Mollic organic-carbon test (WRB 2022, default >= 0.6\%)
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_mollic_organic_carbon <- function(h, min_pct = 0.6,
                                         candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    oc <- h$oc_pct[i]
    if (is.na(oc)) {
      missing <- c(missing, "oc_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, oc_pct = oc, threshold = min_pct, passed = oc >= min_pct
    )
    if (oc >= min_pct) passing <- c(passing, i)
  }

  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = missing,
    details = details
  )
}

#' Mollic base-saturation test (NH4OAc, pH 7, default >= 50\%)
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @param allow_inference If \code{TRUE} (default), fall back to
#'        sum-of-cations / CEC arithmetic OR \code{al_sat_pct < 20}
#'        OR \code{ph_h2o >= 5.8} when \code{bs_pct} is missing.
#' @export
test_mollic_base_saturation <- function(h, min_pct = 50,
                                          candidate_layers = NULL,
                                          allow_inference = TRUE) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    bs <- h$bs_pct[i]

    # Path 1: measured BS.
    if (!is.na(bs)) {
      details[[as.character(i)]] <- list(
        idx = i, source = "measured", bs_pct = bs,
        threshold = min_pct, passed = bs >= min_pct
      )
      if (bs >= min_pct) passing <- c(passing, i)
      next
    }

    if (!isTRUE(allow_inference)) {
      missing <- c(missing, "bs_pct")
      next
    }

    # Path 2 (v0.9.18): derive from sum-of-bases / CEC when both
    # available. BS_calc = (Ca + Mg + K + Na) / CEC * 100. This is
    # exactly the canonical USDA / SiBCS BS formula, just computed
    # internally when bs_pct itself is missing.
    cations <- sum(c(h$ca_cmol[i], h$mg_cmol[i],
                       h$k_cmol[i], h$na_cmol[i]),
                     na.rm = TRUE)
    have_cations <- sum(!is.na(c(h$ca_cmol[i], h$mg_cmol[i],
                                    h$k_cmol[i], h$na_cmol[i]))) >= 2L
    cec <- h$cec_cmol[i]
    if (have_cations && !is.na(cec) && cec > 0) {
      bs_calc <- cations / cec * 100
      details[[as.character(i)]] <- list(
        idx = i, source = "computed_from_cations",
        bs_pct = bs_calc, threshold = min_pct,
        passed = bs_calc >= min_pct
      )
      if (bs_calc >= min_pct) passing <- c(passing, i)
      next
    }

    # Path 3 (v0.9.18): infer BS-high from low Al saturation OR
    # high pH. Mollic / Phaeozem / Chernozem profiles in the FEBR
    # archive routinely report neither bs_pct nor bases but show
    # pH(H2O) >= 5.8 (the empirical threshold above which BS
    # exceeds 50 in essentially all temperate / tropical soils).
    # al_sat_pct < 30 is the equivalent low-Al criterion.
    al_sat <- h$al_sat_pct[i]
    if (!is.na(al_sat) && al_sat < 20) {
      details[[as.character(i)]] <- list(
        idx = i, source = "al_sat_below_20",
        al_sat_pct = al_sat, passed = TRUE
      )
      passing <- c(passing, i)
      next
    }
    ph <- h$ph_h2o[i]
    if (!is.na(ph) && ph >= 5.8) {
      details[[as.character(i)]] <- list(
        idx = i, source = "ph_above_5.8",
        ph_h2o = ph, passed = TRUE
      )
      passing <- c(passing, i)
      next
    }

    # No evidence at all.
    missing <- c(missing, "bs_pct")
  }

  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE

  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = unique(missing),
    details = details
  )
}

#' Mollic thickness test (default >= 20 cm in v0.1)
#'
#' WRB 2022 has more nuanced thickness criteria depending on whether the
#' soil overlies continuous rock at <75 cm, but the simple absolute
#' threshold is the predominant case for non-shallow soils. Cumulative
#' thickness across multiple contiguous mollic-qualifying horizons is a
#' v0.2 refinement.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_cm Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_mollic_thickness <- function(h, min_cm = 20, candidate_layers = NULL) {
  test_minimum_thickness(h, min_cm = min_cm,
                          candidate_layers = candidate_layers)
}

#' Mollic structure test (WRB 2022)
#'
#' Excludes horizons that are simultaneously massive AND very hard when
#' dry. v0.1 implementation reads \code{structure_grade} and
#' \code{consistence_moist} as text and looks for the keyword pair.
#'
#' @param h Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_mollic_structure <- function(h, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    sg <- h$structure_grade[i]
    cm <- h$consistence_moist[i]

    if (is.na(sg)) {
      missing <- c(missing, "structure_grade")
      passing <- c(passing, i) # default-pass if structure not described
      details[[as.character(i)]] <- list(
        idx = i, default_pass = TRUE
      )
      next
    }

    is_massive   <- grepl("massive", sg, ignore.case = TRUE)
    is_very_hard <- !is.na(cm) && grepl("very hard", cm, ignore.case = TRUE)
    ok           <- !(is_massive && is_very_hard)

    details[[as.character(i)]] <- list(
      idx = i, structure_grade = sg, consistence_moist = cm,
      is_massive = is_massive, is_very_hard = is_very_hard,
      passed = ok
    )
    if (ok) passing <- c(passing, i)
  }

  passed <- if (length(passing) > 0L) TRUE else FALSE
  .subtest_result(
    passed  = passed,
    layers  = passing,
    missing = missing,
    details = details
  )
}


# ============================================================== v0.2 sub-tests ====

#' Test for CaCO3 concentration above threshold (per layer)
#'
#' Default 15\% (calcic horizon, WRB 2022 Chapter 3). Used by
#' \code{\link{calcic}}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_caco3_concentration <- function(h, min_pct = 15,
                                       candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$caco3_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "caco3_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, caco3_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for CaSO4 (gypsum) concentration above threshold (per layer)
#'
#' Default 5\% (gypsic horizon, WRB 2022 Chapter 3). Used by
#' \code{\link{gypsic}}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_caso4_concentration <- function(h, min_pct = 5,
                                       candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$caso4_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "caso4_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, caso4_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for plinthite concentration above threshold (per layer)
#'
#' Default 15\% by volume (plinthic horizon, WRB 2022 Chapter 3). Used
#' by \code{\link{plinthic}}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_plinthite_concentration <- function(h, min_pct = 15,
                                           candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$plinthite_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "plinthite_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, plinthite_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test the spodic Al/Fe oxalate criterion: (al_ox + 0.5*fe_ox) >= threshold
#'
#' Default 0.5\% (WRB 2022 Chapter 3, Spodic horizon). Used by
#' \code{\link{spodic}}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_spodic_aluminum_iron <- function(h, min_pct = 0.5,
                                        candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    al_ox <- h$al_ox_pct[i]
    fe_ox <- h$fe_ox_pct[i]
    if (is.na(al_ox) || is.na(fe_ox)) {
      if (is.na(al_ox)) missing <- c(missing, "al_ox_pct")
      if (is.na(fe_ox)) missing <- c(missing, "fe_ox_pct")
      next
    }
    val <- al_ox + fe_ox / 2
    details[[as.character(i)]] <- list(
      idx = i, al_ox = al_ox, fe_ox = fe_ox,
      al_plus_half_fe = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that ph_h2o is at or below a threshold
#'
#' Default 5.9 (Spodic horizon supplementary criterion, WRB 2022).
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_ph Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_ph_below <- function(h, max_ph = 5.9, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$ph_h2o[i]
    if (is.na(val)) {
      missing <- c(missing, "ph_h2o")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, ph_h2o = val,
      threshold = max_ph, passed = val <= max_ph
    )
    if (val <= max_ph) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for gleyic redoximorphic features within top 50 cm
#'
#' v0.2 implementation: requires \code{redoximorphic_features_pct} to be
#' reported and >= \code{min_redox_pct} (default 5\%) within
#' \code{max_top_cm} (default 50). The Munsell-color proxy
#' (chroma <= 2, value >= 4) is too inclusive for albic / bleached
#' horizons and is therefore not used as a primary criterion in v0.2;
#' v0.3 will distinguish reductimorphic from albic via additional
#' indicators. If \code{redoximorphic_features_pct} is missing for all
#' candidate layers, returns NA.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param min_redox_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_gleyic_features <- function(h, max_top_cm = 50, min_redox_pct = 5,
                                   candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  cl <- cl[!is.na(h$top_cm[cl]) & h$top_cm[cl] <= max_top_cm]
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$redoximorphic_features_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "redoximorphic_features_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, redoximorphic_features_pct = val,
      threshold = min_redox_pct, top_cm = h$top_cm[i],
      passed = val >= min_redox_pct
    )
    if (val >= min_redox_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that clay_pct is at or above a threshold
#'
#' Default 30\% (vertic features minimum, WRB 2022 Chapter 3).
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_clay_above <- function(h, min_pct = 30, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$clay_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "clay_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, clay_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for slickensides at or above a presence level
#'
#' Default accepted levels are \code{c("common", "many", "continuous")}
#' (vertic features, WRB 2022). The \code{slickensides} column accepts
#' \code{c("absent", "few", "common", "many", "continuous")}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param levels Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_slickensides_present <- function(h,
                                        levels = c("common", "many",
                                                    "continuous"),
                                        candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$slickensides[i]
    if (is.na(val)) {
      missing <- c(missing, "slickensides")
      next
    }
    ok <- val %in% levels
    details[[as.character(i)]] <- list(
      idx = i, slickensides = val,
      accepted_levels = levels, passed = ok
    )
    if (ok) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for electrical conductivity above threshold (per layer)
#'
#' Default 15 dS/m (salic horizon, WRB 2022 Ch 3.1.20). The WRB salic
#' horizon also accepts an alkaline alternate: EC \\>= 8 dS/m if
#' pH(H2O) \\>= 8.5. Pass \code{alkaline_min_dS_m = 8} and
#' \code{alkaline_min_pH = 8.5} to enable that path -- a layer is then
#' \"qualifying\" if it satisfies the primary OR the alkaline gate. The
#' \code{path} field in each \code{details} entry records which gate
#' carried the layer.
#'
#' @param h Horizons table.
#' @param min_dS_m Primary EC threshold (default 15).
#' @param alkaline_min_dS_m Optional alkaline-path EC threshold
#'        (default \code{NA}: alkaline path disabled).
#' @param alkaline_min_pH Required pH(H2O) for the alkaline path
#'        (default 8.5; only used when \code{alkaline_min_dS_m} is set).
#' @param candidate_layers Optional layer index restriction.
#' @export
test_ec_concentration <- function(h, min_dS_m = 15,
                                    alkaline_min_dS_m = NA_real_,
                                    alkaline_min_pH   = 8.5,
                                    candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  alkaline_enabled <- !is.na(alkaline_min_dS_m)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$ec_dS_m[i]
    if (is.na(val)) {
      missing <- c(missing, "ec_dS_m")
      next
    }
    primary_ok <- val >= min_dS_m
    alk_ok <- FALSE
    pH_val <- NA_real_
    if (alkaline_enabled) {
      pH_val <- if ("ph_h2o" %in% names(h)) h$ph_h2o[i] else NA_real_
      if (!is.na(pH_val)) {
        alk_ok <- val >= alkaline_min_dS_m && pH_val >= alkaline_min_pH
      }
    }
    layer_pass <- primary_ok || alk_ok
    path <- if (primary_ok) "primary" else if (alk_ok) "alkaline" else "none"
    details[[as.character(i)]] <- list(
      idx = i, ec_dS_m = val, ph_h2o = pH_val,
      threshold = min_dS_m,
      alkaline_threshold = alkaline_min_dS_m,
      alkaline_min_pH = alkaline_min_pH,
      path = path, passed = layer_pass
    )
    if (layer_pass) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test the salic horizon EC * thickness product (WRB 2022)
#'
#' Tests whether each candidate layer's product
#' \code{ec_dS_m * (bottom_cm - top_cm)} reaches the canonical WRB 2022
#' threshold (Ch 3.1.20, p. 49):
#' \itemize{
#'   \item \code{>= 450} dS/m * cm for the primary path (EC \\>= 15);
#'   \item \code{>= 240} dS/m * cm for the alkaline path
#'         (EC \\>= 8 with pH(H2O) \\>= 8.5).
#' }
#' The path used per layer is taken from a prior
#' \code{\link{test_ec_concentration}} result (its \code{details[[i]]\\$path}
#' field). When no prior is supplied, every candidate is treated as
#' "primary" and the 450 threshold is applied uniformly.
#'
#' @param h Horizons table.
#' @param min_product Primary product threshold (default 450).
#' @param alkaline_min_product Alkaline-path product threshold
#'        (default 240).
#' @param ec_path_lookup Optional named list (keys = layer index as
#'        character) returning either "primary" or "alkaline" per layer
#'        -- typically built by passing
#'        \code{test_ec_concentration(...)\\$details}.
#' @param candidate_layers Layer index restriction (typically the layers
#'        that already passed the primary EC gate).
#' @export
test_salic_product <- function(h, min_product = 450,
                                alkaline_min_product = 240,
                                ec_path_lookup = NULL,
                                candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    ec <- h$ec_dS_m[i]
    top <- h$top_cm[i]
    bot <- h$bottom_cm[i]
    if (is.na(ec)) { missing <- c(missing, "ec_dS_m"); next }
    if (is.na(top) || is.na(bot)) {
      missing <- c(missing, "top_cm", "bottom_cm"); next
    }
    thk <- bot - top
    prod <- ec * thk
    path <- if (!is.null(ec_path_lookup) &&
                  !is.null(ec_path_lookup[[as.character(i)]])) {
              ec_path_lookup[[as.character(i)]]$path %||% "primary"
            } else "primary"
    threshold <- if (identical(path, "alkaline")) alkaline_min_product
                 else min_product
    layer_pass <- prod >= threshold
    details[[as.character(i)]] <- list(
      idx = i, ec_dS_m = ec, thickness_cm = thk,
      product = prod, path = path,
      threshold = threshold, passed = layer_pass
    )
    if (layer_pass) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


# ============================================================ v0.2c sub-tests ====

#' Compute aluminium saturation (\%) from exchangeable bases and Al
#'
#' Returns \code{al_cmol / (ca + mg + k + na + al) * 100}, or NA if any
#' input is missing or the sum (ECEC) is non-positive.
#'
#' @keywords internal
compute_al_saturation <- function(ca, mg, k, na, al) {
  parts <- c(ca, mg, k, na, al)
  if (any(is.na(parts))) return(NA_real_)
  ecec <- sum(parts)
  if (ecec <= 0) return(NA_real_)
  al / ecec * 100
}


#' Test that CEC per kg clay is at or above a threshold
#'
#' Default 24 cmol_c/kg clay -- WRB 2022 boundary that distinguishes
#' "low-activity-clay" RSGs (Acrisols, Lixisols) from "high-activity-
#' clay" RSGs (Alisols, Luvisols).
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_cmol_per_kg_clay Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_cec_per_clay_above <- function(h, min_cmol_per_kg_clay = 24,
                                       candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    cpc <- cec_per_clay(h$cec_cmol[i], h$clay_pct[i])
    details[[as.character(i)]] <- list(
      idx = i, cec_cmol = h$cec_cmol[i], clay_pct = h$clay_pct[i],
      cec_per_clay = cpc, threshold = min_cmol_per_kg_clay
    )
    if (is.na(cpc)) {
      if (is.na(h$cec_cmol[i]))  missing <- c(missing, "cec_cmol")
      if (is.na(h$clay_pct[i]))  missing <- c(missing, "clay_pct")
      next
    }
    details[[as.character(i)]]$passed <- cpc >= min_cmol_per_kg_clay
    if (cpc >= min_cmol_per_kg_clay) passing <- c(passing, i)
  }
  evaluated <- sum(vapply(details, function(d) !is.null(d$passed), logical(1)))
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that base saturation is at or above a threshold
#'
#' Default 50\% (Lixisol / Luvisol RSG criterion). Reads
#' \code{bs_pct} directly.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_bs_above <- function(h, min_pct = 50, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$bs_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "bs_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, bs_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that base saturation is below a threshold
#'
#' Default 50\% (Acrisol RSG criterion). Reads \code{bs_pct}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_bs_below <- function(h, max_pct = 50, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$bs_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "bs_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, bs_pct = val,
      threshold = max_pct, passed = val < max_pct
    )
    if (val < max_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that aluminium saturation is at or above a threshold
#'
#' Default 50\% (Alisol RSG criterion). Uses \code{al_sat_pct} when
#' reported; otherwise falls back to
#' \code{al_cmol / (ca+mg+k+na+al)_cmol * 100}.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_al_saturation_above <- function(h, min_pct = 50,
                                       candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$al_sat_pct[i]
    if (is.na(val)) {
      val <- compute_al_saturation(h$ca_cmol[i], h$mg_cmol[i],
                                     h$k_cmol[i], h$na_cmol[i],
                                     h$al_cmol[i])
    }
    if (is.na(val)) {
      missing <- c(missing, "al_sat_pct (or ca+mg+k+na+al_cmol)")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, al_sat_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that aluminium saturation is below a threshold
#'
#' Default 50\% (Luvisol RSG criterion). Uses \code{al_sat_pct} when
#' reported; otherwise falls back to computation from exchangeable
#' bases and Al.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_al_saturation_below <- function(h, max_pct = 50,
                                       candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$al_sat_pct[i]
    if (is.na(val)) {
      val <- compute_al_saturation(h$ca_cmol[i], h$mg_cmol[i],
                                     h$k_cmol[i], h$na_cmol[i],
                                     h$al_cmol[i])
    }
    if (is.na(val)) {
      missing <- c(missing, "al_sat_pct (or ca+mg+k+na+al_cmol)")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, al_sat_pct = val,
      threshold = max_pct, passed = val < max_pct
    )
    if (val < max_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


# ============================================================ v0.2d sub-tests ====

#' Test for any layer with caco3_pct above a (low) threshold
#'
#' Default threshold is 0.01\% -- effectively "any measurable secondary
#' carbonate". Used to distinguish Phaeozems (no carbonates within 100
#' cm) from Chernozems and Kastanozems.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_carbonates_present <- function(h, min_pct = 0.01,
                                      candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$caco3_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "caco3_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, caco3_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for chroma <= 2 (moist) within the upper part of the profile
#'
#' Default upper boundary is 20 cm (Chernozem criterion: dark colour in
#' the upper 20 cm of the mollic horizon).
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param max_chroma Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_chernic_color <- function(h, max_top_cm = 20, max_chroma = 2,
                                  candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  cl <- cl[!is.na(h$top_cm[cl]) & h$top_cm[cl] < max_top_cm]
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$munsell_chroma_moist[i]
    if (is.na(val)) {
      missing <- c(missing, "munsell_chroma_moist")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, top_cm = h$top_cm[i], chroma_moist = val,
      threshold = max_chroma, passed = val <= max_chroma
    )
    if (val <= max_chroma) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


# ============================================================ v0.3a sub-tests ====

#' Test that organic carbon is at or above a threshold
#'
#' Default 12\% (histic horizon, WRB 2022 Chapter 3).
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_oc_above <- function(h, min_pct = 12, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$oc_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "oc_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, oc_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that a candidate layer starts at or above a top_cm threshold
#'
#' Used to require surface contact (default top_cm <= 0, i.e., layer
#' must reach the surface) or near-surface presence.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_top_at_or_above <- function(h, max_top_cm = 0,
                                    candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$top_cm[i]
    if (is.na(val)) {
      missing <- c(missing, "top_cm")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, top_cm = val,
      threshold = max_top_cm, passed = val <= max_top_cm
    )
    if (val <= max_top_cm) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that a horizon designation matches a regex pattern
#'
#' Useful for diagnostics that key on field-described features
#' (e.g., glossic tongues for retic, R / Cr for leptic, "f" suffix
#' for cryic / frozen, hortic / irragric / plaggic / pretic / terric
#' for anthric).
#'
#' @param pattern A regex (case-insensitive).
#' @param h Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_designation_pattern <- function(h, pattern,
                                       candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$designation[i]
    if (is.na(val)) {
      missing <- c(missing, "designation")
      next
    }
    ok <- grepl(pattern, val, ignore.case = TRUE)
    details[[as.character(i)]] <- list(
      idx = i, designation = val,
      pattern = pattern, passed = ok
    )
    if (ok) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for coarse texture throughout the upper part of the profile
#'
#' Default predicate: \code{silt + 2 * clay < 15} (loamy sand or
#' coarser) in EVERY layer that intersects the upper
#' \code{max_top_cm} (default 100). Diagnostic for Arenosols.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_coarse_texture_throughout <- function(h, max_top_cm = 100,
                                              candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  cl <- cl[!is.na(h$top_cm[cl]) & h$top_cm[cl] < max_top_cm]
  if (length(cl) == 0L) {
    return(.subtest_result(passed = FALSE, layers = integer(0)))
  }

  passing <- integer(0); missing <- character(0); details <- list()
  all_coarse <- TRUE
  for (i in cl) {
    s <- is_loamy_sand_or_finer(h$sand_pct[i], h$silt_pct[i], h$clay_pct[i])
    if (is.na(s)) {
      missing <- c(missing, "sand_pct", "silt_pct", "clay_pct")
      all_coarse <- NA
      next
    }
    is_coarse <- !s   # loamy sand boundary -- if NOT loamy-sand-or-finer, coarse
    # Actually arenic uses "loamy sand or coarser" = NOT sandy loam or finer
    # silt + 2*clay < 30 is "coarser than sandy loam"
    is_coarse <- (h$silt_pct[i] + 2 * h$clay_pct[i]) < 30
    details[[as.character(i)]] <- list(
      idx = i, sand = h$sand_pct[i], silt = h$silt_pct[i],
      clay = h$clay_pct[i],
      silt_plus_2clay = h$silt_pct[i] + 2 * h$clay_pct[i],
      is_coarse = is_coarse
    )
    if (is_coarse) {
      passing <- c(passing, i)
    } else {
      all_coarse <- FALSE
    }
  }

  passed <- if (isTRUE(all_coarse) && length(passing) == length(cl)) TRUE
            else if (is.na(all_coarse) && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test the andic Al/Fe oxalate criterion: (al_ox + 0.5*fe_ox) >= 2.0\%
#'
#' Distinct from spodic (which uses 0.5\%); the andic threshold is
#' four times higher per WRB 2022 Chapter 3.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_andic_alfe <- function(h, min_pct = 2.0, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    al_ox <- h$al_ox_pct[i]; fe_ox <- h$fe_ox_pct[i]
    if (is.na(al_ox) || is.na(fe_ox)) {
      if (is.na(al_ox)) missing <- c(missing, "al_ox_pct")
      if (is.na(fe_ox)) missing <- c(missing, "fe_ox_pct")
      next
    }
    val <- al_ox + fe_ox / 2
    details[[as.character(i)]] <- list(
      idx = i, al_ox = al_ox, fe_ox = fe_ox,
      al_plus_half_fe = val, threshold = min_pct,
      passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that bulk density is at or below a threshold
#'
#' Default 0.9 g/cm^3 (andic property, WRB 2022).
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_g_cm3 Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_bulk_density_below <- function(h, max_g_cm3 = 0.9,
                                       candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$bulk_density_g_cm3[i]
    if (is.na(val)) {
      missing <- c(missing, "bulk_density_g_cm3")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, bulk_density_g_cm3 = val,
      threshold = max_g_cm3, passed = val <= max_g_cm3
    )
    if (val <= max_g_cm3) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that artefacts_pct >= threshold within the upper max_top_cm
#'
#' Default 20\% by volume (Technosols criterion, WRB 2022).
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_artefacts_concentration <- function(h, min_pct = 20, max_top_cm = 100,
                                            candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  cl <- cl[!is.na(h$top_cm[cl]) & h$top_cm[cl] < max_top_cm]
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$artefacts_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "artefacts_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, artefacts_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that duripan_pct >= threshold (Si-cemented nodules)
#'
#' Default 10\% per WRB 2022 Ch 3.1.7 (Duric horizon, p. 41).
#' v0.3.1 reduced default from 15\% to 10\% to match the canonical text.
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_duripan_concentration <- function(h, min_pct = 10,
                                          candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$duripan_pct[i]
    if (is.na(val)) {
      missing <- c(missing, "duripan_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, duripan_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for fluvic stratification: irregular OC pattern + texture
#' variability across consecutive horizons
#'
#' v0.3 simplified: returns TRUE when (a) at least 3 layers within the
#' upper 100 cm exist, AND (b) clay_pct varies by >= 8 percentage
#' points across consecutive layers (indicating depositional
#' alternation), AND (c) OC does not decrease monotonically with depth.
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param min_clay_swing Numeric threshold or option (see Details).
#' @export
test_fluvic_stratification <- function(h, max_top_cm = 100,
                                          min_clay_swing = 8) {
  cl <- which(!is.na(h$top_cm) & h$top_cm < max_top_cm)
  if (length(cl) < 3L) {
    return(.subtest_result(
      passed = FALSE, layers = integer(0),
      notes = sprintf("Need >= 3 layers within top %g cm; have %d",
                       max_top_cm, length(cl))
    ))
  }

  clays <- h$clay_pct[cl]
  ocs   <- h$oc_pct[cl]

  if (any(is.na(clays))) {
    return(.subtest_result(passed = NA, layers = integer(0),
                            missing = "clay_pct"))
  }

  swings <- abs(diff(clays))
  texture_alternates <- any(swings >= min_clay_swing)

  oc_irregular <- if (any(is.na(ocs))) NA
                   else any(diff(ocs) > 0.1)   # any increase with depth

  if (is.na(oc_irregular)) {
    # We can still flag based on texture alone, but mark missing
    if (texture_alternates) {
      return(.subtest_result(passed = TRUE, layers = cl,
                              missing = "oc_pct",
                              notes = "Stratification by texture; OC pattern unverified"))
    } else {
      return(.subtest_result(passed = FALSE, layers = integer(0),
                              missing = "oc_pct"))
    }
  }

  passed <- texture_alternates && oc_irregular
  .subtest_result(
    passed  = passed,
    layers  = if (passed) cl else integer(0),
    details = list(
      texture_swings   = swings,
      texture_alternates = texture_alternates,
      oc_diffs         = diff(ocs),
      oc_irregular     = oc_irregular
    )
  )
}


# ============================================================ v0.3b sub-tests ====

#' Compute exchangeable sodium percentage (ESP)
#'
#' \code{na_cmol / cec_cmol * 100}, returning NA on missing/zero CEC.
#'
#' @keywords internal
compute_esp <- function(na_cmol, cec_cmol) {
  if (is.na(na_cmol) || is.na(cec_cmol) || cec_cmol <= 0) return(NA_real_)
  na_cmol / cec_cmol * 100
}


#' Test exchangeable sodium percentage above threshold
#'
#' Default 15\% (natric horizon, WRB 2022 Chapter 3).
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_esp_above <- function(h, min_pct = 15, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- compute_esp(h$na_cmol[i], h$cec_cmol[i])
    if (is.na(val)) {
      if (is.na(h$na_cmol[i]))  missing <- c(missing, "na_cmol")
      if (is.na(h$cec_cmol[i])) missing <- c(missing, "cec_cmol")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, na_cmol = h$na_cmol[i], cec_cmol = h$cec_cmol[i],
      esp_pct = val, threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for high free-iron content (\code{fe_dcb_pct} >= threshold)
#'
#' Default 4\% (an indicator of strong red colour and Fe-richness; used
#' as a v0.3 simplified marker for nitic horizon's typical Fe content).
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_pct Numeric threshold or option (see Details).
#' @param candidate_layers Numeric threshold or option (see Details).
#' @export
test_fe_dcb_above <- function(h, min_pct = 4, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$fe_dcb_pct[i]
    if (is.na(val)) { missing <- c(missing, "fe_dcb_pct"); next }
    details[[as.character(i)]] <- list(
      idx = i, fe_dcb_pct = val,
      threshold = min_pct, passed = val >= min_pct
    )
    if (val >= min_pct) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for an abrupt textural change between adjacent horizons
#'
#' WRB 2022 planic criterion: clay content of the underlying horizon is
#' at least double that of the overlying horizon, with the transition
#' occurring within 7.5 cm vertical distance. v0.3 implements the
#' clay-doubling test plus an optional \code{boundary_distinctness}
#' check (must be \code{"abrupt"} or \code{"very abrupt"} on the upper
#' horizon).
#'
#' @param h Numeric threshold or option (see Details).
#' @param min_ratio Numeric threshold or option (see Details).
#' @param require_abrupt_boundary Numeric threshold or option (see Details).
#' @export
test_abrupt_textural_change <- function(h, min_ratio = 2.0,
                                          require_abrupt_boundary = TRUE) {
  if (nrow(h) < 2L) {
    return(.subtest_result(
      passed = FALSE, layers = integer(0),
      notes = "Fewer than 2 horizons -- abrupt textural change inapplicable"
    ))
  }
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in seq.int(2L, nrow(h))) {
    above <- h$clay_pct[i - 1L]; here <- h$clay_pct[i]
    if (is.na(above) || is.na(here)) {
      missing <- c(missing, "clay_pct")
      next
    }
    if (above <= 0) next
    ratio <- here / above
    boundary_ok <- if (!require_abrupt_boundary) TRUE
                    else if (is.na(h$boundary_distinctness[i - 1L])) FALSE
                    else grepl("abrupt", h$boundary_distinctness[i - 1L],
                                  ignore.case = TRUE)
    if (require_abrupt_boundary && is.na(h$boundary_distinctness[i - 1L])) {
      missing <- c(missing, "boundary_distinctness")
    }
    ok <- isTRUE(ratio >= min_ratio) && isTRUE(boundary_ok)
    details[[as.character(i)]] <- list(
      above_idx = i - 1L, here_idx = i,
      above_clay = above, here_clay = here, ratio = ratio,
      boundary = h$boundary_distinctness[i - 1L],
      boundary_ok = boundary_ok, passed = ok
    )
    if (ok) passing <- c(passing, i)
  }
  any_evaluable <- length(details) > 0L
  passed <- if (length(passing) > 0L) TRUE
            else if (!any_evaluable && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test for stagnic redox features (perched water signature)
#'
#' Distinct from gleyic (groundwater): stagnic = redoximorphic features
#' in some layer within the upper \code{max_top_cm} (default 100) AND
#' redox in deeper layers DROPS substantially (decay to < third of the
#' shallow value). The decay condition is what separates perched water
#' (sits above an impermeable layer; deeper soil is not saturated)
#' from groundwater-driven gleying (saturation continues with depth).
#'
#' @param h Numeric threshold or option (see Details).
#' @param max_top_cm Numeric threshold or option (see Details).
#' @param min_redox_pct Numeric threshold or option (see Details).
#' @param decay_factor Numeric threshold or option (see Details).
#' @export
test_stagnic_pattern <- function(h, max_top_cm = 100, min_redox_pct = 5,
                                    decay_factor = 3) {
  cl <- which(!is.na(h$top_cm) & h$top_cm <= max_top_cm)
  if (length(cl) < 2L) {
    return(.subtest_result(passed = FALSE, layers = integer(0)))
  }
  redox <- h$redoximorphic_features_pct
  if (all(is.na(redox))) {
    return(.subtest_result(passed = NA, layers = integer(0),
                            missing = "redoximorphic_features_pct"))
  }

  # For every candidate layer with redox >= threshold, check whether
  # ALL deeper layers fall below redox / decay_factor (i.e., the redox
  # decays substantially with depth -> perched-water signature).
  passing <- integer(0)
  details <- list()
  for (i in cl) {
    r_i <- redox[i]
    if (is.na(r_i) || r_i < min_redox_pct) next
    deeper <- which(!is.na(h$top_cm) & h$top_cm >= h$bottom_cm[i])
    if (length(deeper) == 0L) next
    deeper_redox <- redox[deeper]
    if (all(is.na(deeper_redox))) next
    deeper_max <- max(deeper_redox, na.rm = TRUE)
    decays <- deeper_max < r_i / decay_factor
    details[[as.character(i)]] <- list(
      shallow_idx = i, shallow_redox = r_i,
      deeper_max = deeper_max, decay_factor = decay_factor,
      passed = decays
    )
    if (decays) passing <- c(passing, i)
  }

  .subtest_result(
    passed  = length(passing) > 0L,
    layers  = passing,
    details = details
  )
}


# ============================================================== aggregation ====

#' Aggregate sub-test results into a passed/missing summary
#'
#' Used by every diagnostic-level function. \code{layers_passing} is the
#' intersection of \code{layers} across the listed sub-tests; \code{passed}
#' is \code{TRUE} if that intersection is non-empty, \code{NA} if no test
#' could be evaluated and missing attributes were reported, and
#' \code{FALSE} otherwise.
#'
#' @keywords internal
aggregate_subtests <- function(tests, layer_tests = NULL,
                                  exclusions = character(0)) {
  if (is.null(layer_tests)) layer_tests <- names(tests)
  layer_tests <- setdiff(layer_tests, exclusions)

  layer_lists <- lapply(tests[layer_tests], function(t) {
    if (is.null(t$layers)) integer(0) else t$layers
  })
  layers_passing <- if (length(layer_lists) == 0L) {
    integer(0)
  } else {
    Reduce(intersect, layer_lists)
  }

  excluded_failed <- vapply(exclusions, function(e) {
    isFALSE(tests[[e]]$passed)
  }, logical(1))
  if (any(excluded_failed)) {
    layers_passing <- integer(0)
  }

  missing <- unique(unlist(lapply(tests, function(t) t$missing)))
  if (is.null(missing)) missing <- character(0)

  any_test_na <- any(vapply(tests, function(t) is.na(t$passed), logical(1)))

  passed <- if (length(layers_passing) > 0L) {
    TRUE
  } else if (any_test_na && length(missing) > 0L) {
    NA
  } else {
    FALSE
  }

  list(passed = passed, layers = layers_passing, missing = missing)
}
