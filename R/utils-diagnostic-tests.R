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

#' Test the argic clay-increase criterion (WRB 2022)
#'
#' Tests every consecutive pair of horizons in the profile against the
#' WRB 2022 clay-increase rules:
#' \itemize{
#'   \item overlying clay < 15\%: argic must contain at least 3\% absolute
#'         more clay;
#'   \item overlying clay 15-40\%: argic / overlying ratio must be >= 1.2;
#'   \item overlying clay >= 40\%: argic must contain at least 8\% absolute
#'         more clay.
#' }
#' Returns the indices of horizons that satisfy as the argic candidate.
#'
#' @param h Horizons data.table (canonical schema).
#' @return Sub-test result list.
#' @references IUSS Working Group WRB (2022), Chapter 3, Argic horizon.
#' @export
test_clay_increase_argic <- function(h) {
  if (nrow(h) < 2L) {
    return(.subtest_result(
      passed = FALSE,
      notes  = "Fewer than 2 horizons -- clay increase test inapplicable"
    ))
  }

  candidates <- integer(0)
  details    <- list()
  missing    <- character(0)

  for (i in seq.int(2L, nrow(h))) {
    above <- h$clay_pct[i - 1L]
    here  <- h$clay_pct[i]

    if (is.na(above) || is.na(here)) {
      missing <- c(missing, "clay_pct")
      next
    }

    rule_label <- if (above < 15)        "<15%: +3pp absolute"
                  else if (above < 40)   "15-40%: ratio >= 1.2"
                  else                   ">=40%: +8pp absolute"

    rule_passed <- if (above < 15) {
      here - above >= 3
    } else if (above < 40) {
      here / above >= 1.2
    } else {
      here - above >= 8
    }

    details[[as.character(i)]] <- list(
      above_idx  = i - 1L,
      here_idx   = i,
      above_clay = above,
      here_clay  = here,
      rule       = rule_label,
      passed     = rule_passed
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
#' @export
test_ferralic_thickness <- function(h, min_cm = 30, candidate_layers = NULL) {
  test_minimum_thickness(h, min_cm = min_cm,
                          candidate_layers = candidate_layers)
}

#' Ferralic texture: sandy loam or finer (same predicate as argic)
#'
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
#' @export
test_mollic_color <- function(h,
                                max_value_moist  = 3,
                                max_chroma_moist = 3,
                                max_value_dry    = 5,
                                candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    vm <- h$munsell_value_moist[i]
    cm <- h$munsell_chroma_moist[i]
    vd <- h$munsell_value_dry[i]

    if (is.na(vm) || is.na(cm)) {
      if (is.na(vm)) missing <- c(missing, "munsell_value_moist")
      if (is.na(cm)) missing <- c(missing, "munsell_chroma_moist")
      next
    }

    moist_ok <- vm <= max_value_moist && cm <= max_chroma_moist
    dry_ok   <- if (is.na(vd)) (vm + 1) <= max_value_dry
                else            vd     <= max_value_dry

    details[[as.character(i)]] <- list(
      idx = i,
      value_moist  = vm, chroma_moist = cm, value_dry = vd,
      moist_ok = moist_ok, dry_ok = dry_ok,
      passed   = moist_ok && dry_ok
    )

    if (moist_ok && dry_ok) passing <- c(passing, i)
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

#' Mollic organic-carbon test (WRB 2022, default >= 0.6%)
#'
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

#' Mollic base-saturation test (NH4OAc, pH 7, default >= 50%)
#'
#' @export
test_mollic_base_saturation <- function(h, min_pct = 50,
                                          candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0)
  missing <- character(0)
  details <- list()

  for (i in cl) {
    bs <- h$bs_pct[i]
    if (is.na(bs)) {
      missing <- c(missing, "bs_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, bs_pct = bs, threshold = min_pct, passed = bs >= min_pct
    )
    if (bs >= min_pct) passing <- c(passing, i)
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

#' Mollic thickness test (default >= 20 cm in v0.1)
#'
#' WRB 2022 has more nuanced thickness criteria depending on whether the
#' soil overlies continuous rock at <75 cm, but the simple absolute
#' threshold is the predominant case for non-shallow soils. Cumulative
#' thickness across multiple contiguous mollic-qualifying horizons is a
#' v0.2 refinement.
#'
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
#' Default 15 dS/m (salic horizon, WRB 2022 Chapter 3). Used by
#' \code{\link{salic}}.
#'
#' @export
test_ec_concentration <- function(h, min_dS_m = 15,
                                    candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$ec_dS_m[i]
    if (is.na(val)) {
      missing <- c(missing, "ec_dS_m")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, ec_dS_m = val,
      threshold = min_dS_m, passed = val >= min_dS_m
    )
    if (val >= min_dS_m) passing <- c(passing, i)
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
