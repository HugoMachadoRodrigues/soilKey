# ============================================================================
# v0.3.3 sub-tests: building blocks for the missing WRB 2022 Ch 3.1 / 3.2 /
# 3.3 diagnostics. Every function here follows the canonical sub-test
# contract (.subtest_result with passed / layers / missing / details).
# ============================================================================


#' Robust per-layer column accessor.
#'
#' `h[[col]][i]` returns `NULL` (length 0) when the column is absent
#' from the horizon schema entirely (e.g. older fixtures pre-dating a
#' schema extension). Downstream code then reaches `is.na(NULL)`,
#' which is `logical(0)`, and crashes inside `if (...)`. This helper
#' converts an absent column to `NA` of the requested mode so the
#' "missing" branch in every sub-test is exercised cleanly.
#'
#' Added 2026-04-30 after the canonical-fixture benchmark surfaced
#' five errors of the form "argument is of length zero" coming from
#' `test_numeric_above`, `test_pattern_match`, `test_shrink_swell_cracks`
#' on fixtures whose schema predates v0.3.3 column extensions.
#'
#' @keywords internal
.col_at <- function(h, column, i, default = NA) {
  v <- h[[column]]
  if (is.null(v)) return(default)
  out <- v[i]
  if (length(out) == 0L) return(default)
  out
}


# ---- generic numeric column tests -------------------------------------------

#' Test that an arbitrary numeric column exceeds a threshold per layer
#'
#' Generic helper: returns the layers where \code{h[[column]] >= threshold}.
#' Used by many of the v0.3.3 diagnostics that boil down to
#' "layer with attribute X above value V".
#'
#' @param h Horizons table.
#' @param column Name of the numeric column to test.
#' @param threshold Minimum value (inclusive).
#' @param candidate_layers Optional layer index restriction.
#' @return Sub-test result list.
#' @keywords internal
test_numeric_above <- function(h, column, threshold,
                                  candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- .col_at(h, column, i, default = NA_real_)
    if (is.na(val)) { missing <- c(missing, column); next }
    details[[as.character(i)]] <- list(
      idx = i, value = val, threshold = threshold,
      passed = val >= threshold
    )
    if (val >= threshold) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that a character column matches a regex per layer
#'
#' @param h Horizons table.
#' @param column Character column name.
#' @param pattern Regex (case-insensitive).
#' @param candidate_layers Optional restriction.
#' @return Sub-test result.
#' @keywords internal
test_pattern_match <- function(h, column, pattern,
                                  candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- .col_at(h, column, i, default = NA_character_)
    if (is.na(val)) { missing <- c(missing, column); next }
    matched <- grepl(pattern, val, ignore.case = TRUE, perl = TRUE)
    details[[as.character(i)]] <- list(
      idx = i, value = val, pattern = pattern, passed = matched
    )
    if (matched) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


#' Test that a layer's top is at or below a target depth
#'
#' Inverse of \code{\link{test_top_at_or_above}}: returns layers whose top
#' is shallower than or equal to \code{max_top_cm}, i.e. that start within
#' the upper part of the profile.
#'
#' @keywords internal
test_starts_within <- function(h, max_top_cm,
                                 candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    top <- h$top_cm[i]
    if (is.na(top)) { missing <- c(missing, "top_cm"); next }
    details[[as.character(i)]] <- list(
      idx = i, top_cm = top, max_top_cm = max_top_cm,
      passed = top <= max_top_cm
    )
    if (top <= max_top_cm) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


# ---- cementation -----------------------------------------------------------

#' Test that a layer is at least moderately cemented
#'
#' Used by petric variants (petrocalcic / petroduric / petrogypsic /
#' petroplinthic). The WRB 2022 ladder is: weakly < moderately <
#' strongly < indurated. Default threshold is "moderately".
#'
#' @param h Horizons table.
#' @param min_class One of "weakly", "moderately", "strongly", "indurated".
#' @param candidate_layers Optional restriction.
#' @keywords internal
test_cemented <- function(h, min_class = "moderately",
                            candidate_layers = NULL) {
  ladder <- c("none" = 0L, "weakly" = 1L, "moderately" = 2L,
              "strongly" = 3L, "indurated" = 4L)
  if (!min_class %in% names(ladder)) {
    rlang::abort(sprintf("Unknown cementation class: %s", min_class))
  }
  threshold <- ladder[[min_class]]
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    val <- h$cementation_class[i]
    if (is.na(val)) { missing <- c(missing, "cementation_class"); next }
    rank <- ladder[val] %||% 0L
    if (length(rank) != 1L || is.na(rank)) rank <- 0L
    details[[as.character(i)]] <- list(
      idx = i, class = val, rank = unname(rank),
      threshold = threshold, passed = rank >= threshold
    )
    if (rank >= threshold) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


# ---- albic horizon (claric eluvial) ----------------------------------------

#' Test for "claric" Munsell colour per layer (WRB 2022 Ch 3.3.4)
#'
#' Claric material is light-coloured fine earth meeting one of the WRB
#' Munsell criteria:
#' \itemize{
#'   \item dry: value \\>= 7 with chroma \\<= 3, OR value \\>= 5 with chroma
#'         \\<= 2;
#'   \item moist: value \\>= 6 with chroma \\<= 4, OR value \\>= 5 with
#'         chroma \\<= 3, OR value \\>= 4 with chroma \\<= 2, OR (hue 5YR or
#'         redder AND value \\>= 4 AND chroma \\<= 3 AND \\>= 25\% of sand /
#'         coarse silt grains uncoated).
#' }
#' v0.3.3 implementation: requires moist Munsell value/chroma to satisfy
#' the four moist alternatives (the dry alternatives are checked when dry
#' Munsell is present); the uncoated-grain check is deferred (treated as
#' satisfied when the colour passes).
#'
#' @keywords internal
test_claric_munsell <- function(h, candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    vm <- h$munsell_value_moist[i]
    cm <- h$munsell_chroma_moist[i]
    vd <- h$munsell_value_dry[i]
    cd <- h$munsell_chroma_dry[i]
    hu <- h$munsell_hue_moist[i]
    have_moist <- !is.na(vm) && !is.na(cm)
    have_dry   <- !is.na(vd) && !is.na(cd)
    if (!have_moist && !have_dry) {
      missing <- c(missing, "munsell_value_moist", "munsell_chroma_moist")
      next
    }
    moist_ok <- have_moist && (
      (vm >= 6 && cm <= 4) ||
      (vm >= 5 && cm <= 3) ||
      (vm >= 4 && cm <= 2) ||
      (!is.na(hu) && grepl("^(5YR|2\\.5YR|10R|7\\.5R|5R|2\\.5R)",
                              hu, ignore.case = TRUE) &&
         vm >= 4 && cm <= 3)
    )
    dry_ok <- have_dry && (
      (vd >= 7 && cd <= 3) ||
      (vd >= 5 && cd <= 2)
    )
    layer_pass <- isTRUE(moist_ok) || isTRUE(dry_ok)
    details[[as.character(i)]] <- list(
      idx = i, moist = c(value = vm, chroma = cm),
      dry = c(value = vd, chroma = cd),
      moist_ok = moist_ok, dry_ok = dry_ok, passed = layer_pass
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


# ---- shrink-swell cracks ---------------------------------------------------

#' Test for shrink-swell cracks meeting the WRB 2022 Ch 3.2.12 width
#' (>= 0.5 cm when soil is dry)
#'
#' If \code{cracks_width_cm} is missing, the test falls back to
#' designation pattern matching (\code{Bss}, \code{Css}, etc.) and
#' \code{slickensides} >= "common" as proxy evidence.
#'
#' @keywords internal
test_shrink_swell_cracks <- function(h, min_width_cm = 0.5,
                                        min_depth_cm = 0,
                                        candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    w    <- .col_at(h, "cracks_width_cm", i, default = NA_real_)
    d    <- .col_at(h, "cracks_depth_cm", i, default = NA_real_)
    desg <- .col_at(h, "designation",     i, default = NA_character_)
    sl   <- .col_at(h, "slickensides",    i, default = NA_character_)
    if (!is.na(w)) {
      ok_width <- w >= min_width_cm
      ok_depth <- is.na(d) || d >= min_depth_cm
      layer_pass <- ok_width && ok_depth
      details[[as.character(i)]] <- list(
        idx = i, cracks_width_cm = w, cracks_depth_cm = d,
        threshold_width = min_width_cm, threshold_depth = min_depth_cm,
        path = "measured", passed = layer_pass
      )
    } else if ((!is.na(desg) && grepl("ss|Vss", desg)) ||
               (!is.na(sl) && sl %in% c("common", "many", "continuous"))) {
      layer_pass <- TRUE
      details[[as.character(i)]] <- list(
        idx = i, designation = desg, slickensides = sl,
        path = "designation_proxy", passed = TRUE
      )
    } else {
      missing <- c(missing, "cracks_width_cm")
      next
    }
    if (isTRUE(layer_pass)) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


# ---- reducing conditions ---------------------------------------------------

#' Test for WRB 2022 reducing conditions (Ch 3.2.10) per layer
#'
#' Reducing conditions show one or more of:
#' \itemize{
#'   \item rH < 20 (we don't carry rH so this is deferred);
#'   \item presence of free Fe2+ (alpha,alpha-dipyridyl test) -- detected via
#'         designation \code{r}, \code{g}, \code{Br}, etc., or via the
#'         \code{redoximorphic_features_pct} >= 5\%;
#'   \item iron sulfide (designation pattern \code{S}, \code{Aj}, \code{Ar});
#'   \item methane (not in schema, deferred).
#' }
#'
#' @keywords internal
test_reducing_conditions <- function(h, min_redox_pct = 5,
                                        candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    desg <- h$designation[i]
    redox <- h$redoximorphic_features_pct[i]
    sulf  <- h$sulfidic_s_pct[i]
    have_signal <- FALSE; reasons <- character(0)
    if (!is.na(desg) && grepl("^[A-Z]+(g|r|j|Br|Bj|Bg|Cr|Cg|Sj)",
                                  desg, ignore.case = FALSE)) {
      have_signal <- TRUE; reasons <- c(reasons, "reduction_designation")
    }
    if (!is.na(redox) && redox >= min_redox_pct) {
      have_signal <- TRUE; reasons <- c(reasons, "redox_features")
    }
    if (!is.na(sulf) && sulf >= 0.01) {
      have_signal <- TRUE; reasons <- c(reasons, "sulfidic_s")
    }
    if (!have_signal && is.na(desg) &&
        is.na(redox) && is.na(sulf)) {
      missing <- c(missing, "redoximorphic_features_pct")
      next
    }
    details[[as.character(i)]] <- list(
      idx = i, reasons = reasons, passed = have_signal
    )
    if (have_signal) passing <- c(passing, i)
  }
  evaluated <- length(details)
  passed <- if (length(passing) > 0L) TRUE
            else if (evaluated == 0L && length(missing) > 0L) NA
            else FALSE
  .subtest_result(passed = passed, layers = passing,
                   missing = missing, details = details)
}


# ---- vitric / tephric -----------------------------------------------------

#' Test for the andic+vitric Al_ox + 1/2 Fe_ox sum
#'
#' Reuses \code{compute_alfe_ox()} (declared inline below to keep the file
#' self-contained); pass thresholds for andic (>=2.0) or vitric (>=0.4).
#'
#' @keywords internal
test_alfe_ox_above <- function(h, min_pct,
                                 candidate_layers = NULL) {
  cl <- .candidate_layers(h, candidate_layers)
  passing <- integer(0); missing <- character(0); details <- list()
  for (i in cl) {
    al <- h$al_ox_pct[i]
    fe <- h$fe_ox_pct[i]
    if (is.na(al) || is.na(fe)) {
      if (is.na(al)) missing <- c(missing, "al_ox_pct")
      if (is.na(fe)) missing <- c(missing, "fe_ox_pct")
      next
    }
    val <- al + 0.5 * fe
    details[[as.character(i)]] <- list(
      idx = i, al_ox_pct = al, fe_ox_pct = fe,
      alfe_ox_pct = val, threshold = min_pct,
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


# ---- desert pavement / yermic -------------------------------------------

#' Test for WRB 2022 Ch 3.2.17 yermic surface signature
#'
#' Coarse surface fragments (\code{desert_pavement_pct}) >= 20\% AND
#' (\code{varnish_pct} >= 10 OR \code{ventifact_pct} >= 10 OR
#' \code{vesicular_pores} \%in\% c("common", "many")) on the surface
#' layer (top_cm <= 5).
#' @keywords internal
test_yermic_surface <- function(h) {
  surface <- which(!is.na(h$top_cm) & h$top_cm <= 5)
  if (length(surface) == 0L) {
    return(.subtest_result(passed = NA, layers = integer(0),
                            missing = "top_cm",
                            details = list(reason = "no surface layer")))
  }
  i <- surface[1]
  pavement  <- h$desert_pavement_pct[i]
  varnish   <- h$varnish_pct[i]
  ventifact <- h$ventifact_pct[i]
  vesicular <- h$vesicular_pores[i]
  if (is.na(pavement) && is.na(vesicular)) {
    return(.subtest_result(passed = NA, layers = integer(0),
                            missing = "desert_pavement_pct",
                            details = list(idx = i)))
  }
  pav_ok <- !is.na(pavement) && pavement >= 20
  acc_ok <- (!is.na(varnish)   && varnish   >= 10) ||
            (!is.na(ventifact) && ventifact >= 10) ||
            (!is.na(vesicular) && vesicular %in% c("common", "many"))
  layer_pass <- pav_ok && acc_ok
  details <- list(
    idx = i, desert_pavement_pct = pavement,
    varnish_pct = varnish, ventifact_pct = ventifact,
    vesicular_pores = vesicular, passed = layer_pass
  )
  .subtest_result(
    passed  = layer_pass,
    layers  = if (layer_pass) i else integer(0),
    missing = character(0),
    details = list(`1` = details)
  )
}


# ---- takyric surface crust -------------------------------------------------

#' Test for WRB 2022 Ch 3.2.15 takyric surface-crust signature
#'
#' Surface mineral crust with: clay-loam-or-finer texture, platy/massive
#' structure, polygonal cracks >= 2 cm deep with spacing <= 20 cm,
#' rupture-resistance \\>= "hard" when dry, plasticity \\>= "moderately
#' plastic" when moist, EC < 4 dS/m OR >= 1 dS/m less than the layer
#' below. v0.3.3 enforces texture + structure + cracks + EC.
#'
#' @keywords internal
test_takyric_surface <- function(h) {
  surface <- which(!is.na(h$top_cm) & h$top_cm <= 5)
  if (length(surface) == 0L) {
    return(.subtest_result(passed = NA, layers = integer(0),
                            missing = "top_cm",
                            details = list(reason = "no surface layer")))
  }
  i <- surface[1]
  clay <- h$clay_pct[i]; silt <- h$silt_pct[i]
  struct <- h$structure_grade[i] %||% NA_character_
  type   <- h$structure_type[i]  %||% NA_character_
  cracks_d <- h$cracks_depth_cm[i]
  spacing  <- h$polygonal_cracks_spacing_cm[i]
  ec <- h$ec_dS_m[i]
  if (is.na(clay)) {
    return(.subtest_result(passed = NA, layers = integer(0),
                            missing = "clay_pct",
                            details = list(idx = i)))
  }
  texture_ok <- !is.na(clay) && clay >= 27 &&
                  (!is.na(silt) && (silt + clay) >= 40)
  struct_ok <- (!is.na(type) && grepl("platy|massive", type,
                                          ignore.case = TRUE))
  cracks_ok <- !is.na(cracks_d) && cracks_d >= 2 &&
                 (is.na(spacing) || spacing <= 20)
  ec_ok <- is.na(ec) || ec < 4
  layer_pass <- texture_ok && struct_ok && cracks_ok && ec_ok
  details <- list(
    idx = i, clay_pct = clay, silt_pct = silt,
    structure_type = type, cracks_depth_cm = cracks_d,
    polygonal_spacing_cm = spacing, ec_dS_m = ec,
    texture_ok = texture_ok, structure_ok = struct_ok,
    cracks_ok = cracks_ok, ec_ok = ec_ok, passed = layer_pass
  )
  .subtest_result(
    passed  = layer_pass,
    layers  = if (layer_pass) i else integer(0),
    missing = character(0),
    details = list(`1` = details)
  )
}
