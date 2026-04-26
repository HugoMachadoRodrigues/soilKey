# ================================================================
# WRB 2022 -- argic-derived RSG diagnostics
#
# These four diagnostics test whether a profile that satisfies the
# argic horizon also satisfies the activity-of-clay and chemistry
# criteria that distinguish the four argic-derived RSGs:
#
#   Acrisol (AC): argic + CEC <  24 cmol_c/kg clay + BS <  50%
#   Lixisol (LX): argic + CEC <  24 cmol_c/kg clay + BS >= 50%
#   Alisol  (AL): argic + CEC >= 24 cmol_c/kg clay + Al sat >= 50%
#   Luvisol (LV): argic + CEC >= 24 cmol_c/kg clay + Al sat <  50%
#
# Each function calls argic() internally to identify the argic
# horizon(s) and then applies the chemistry tests on those layers.
#
# Tests are written so that each fixture passes exactly one of the
# four functions; this is the cleanest way to demonstrate that the
# four "argic family" RSGs are mutually disambiguating.
# ================================================================


#' Acrisol RSG diagnostic (WRB 2022)
#'
#' Tests whether a profile satisfies the Acrisol RSG criteria: an
#' argic horizon with low-activity clay (CEC < 24 cmol_c/kg clay) AND
#' low base saturation (BS < 50%) within at least one argic layer.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cec Maximum CEC per kg clay (default 24).
#' @param max_bs Maximum base saturation \% (default 50).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Acrisols.
#' @export
acrisol <- function(pedon, max_cec = 24, max_bs = 50) {
  arg <- argic(pedon)

  if (!isTRUE(arg$passed)) {
    return(.argic_derived_negative("acrisol", arg,
      "Profile lacks an argic horizon -- Acrisol RSG cannot apply."))
  }

  h <- pedon$horizons
  layers <- arg$layers

  tests <- list(argic = arg)
  tests$cec_low <- test_cec_per_clay(h, max_cmol_per_kg_clay = max_cec,
                                       candidate_layers = layers)
  tests$bs_low  <- test_bs_below(h, max_pct = max_bs,
                                   candidate_layers = layers)

  agg <- .argic_derived_aggregate(tests, layer_keys = c("cec_low", "bs_low"))

  DiagnosticResult$new(
    name      = "acrisol",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Acrisols"
  )
}


#' Lixisol RSG diagnostic (WRB 2022)
#'
#' argic + CEC < 24 cmol_c/kg clay + BS >= 50\%.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cec Maximum CEC per kg clay (default 24).
#' @param min_bs Minimum base saturation \% (default 50).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Lixisols.
#' @export
lixisol <- function(pedon, max_cec = 24, min_bs = 50) {
  arg <- argic(pedon)

  if (!isTRUE(arg$passed)) {
    return(.argic_derived_negative("lixisol", arg,
      "Profile lacks an argic horizon -- Lixisol RSG cannot apply."))
  }

  h <- pedon$horizons
  layers <- arg$layers

  tests <- list(argic = arg)
  tests$cec_low <- test_cec_per_clay(h, max_cmol_per_kg_clay = max_cec,
                                       candidate_layers = layers)
  tests$bs_high <- test_bs_above(h, min_pct = min_bs,
                                   candidate_layers = layers)

  agg <- .argic_derived_aggregate(tests, layer_keys = c("cec_low", "bs_high"))

  DiagnosticResult$new(
    name      = "lixisol",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Lixisols"
  )
}


#' Alisol RSG diagnostic (WRB 2022)
#'
#' argic + CEC >= 24 cmol_c/kg clay + Al saturation >= 50\%.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cec Minimum CEC per kg clay (default 24).
#' @param min_al_sat Minimum Al saturation \% (default 50).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Alisols.
#' @export
alisol <- function(pedon, min_cec = 24, min_al_sat = 50) {
  arg <- argic(pedon)

  if (!isTRUE(arg$passed)) {
    return(.argic_derived_negative("alisol", arg,
      "Profile lacks an argic horizon -- Alisol RSG cannot apply."))
  }

  h <- pedon$horizons
  layers <- arg$layers

  tests <- list(argic = arg)
  tests$cec_high     <- test_cec_per_clay_above(h,
                                                  min_cmol_per_kg_clay = min_cec,
                                                  candidate_layers     = layers)
  tests$al_sat_high  <- test_al_saturation_above(h,
                                                   min_pct = min_al_sat,
                                                   candidate_layers = layers)

  agg <- .argic_derived_aggregate(tests,
                                    layer_keys = c("cec_high", "al_sat_high"))

  DiagnosticResult$new(
    name      = "alisol",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Alisols"
  )
}


#' Luvisol RSG diagnostic (WRB 2022)
#'
#' argic + CEC >= 24 cmol_c/kg clay + Al saturation < 50\%.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cec Minimum CEC per kg clay (default 24).
#' @param max_al_sat Maximum Al saturation \% (default 50).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Luvisols.
#' @export
luvisol <- function(pedon, min_cec = 24, max_al_sat = 50) {
  arg <- argic(pedon)

  if (!isTRUE(arg$passed)) {
    return(.argic_derived_negative("luvisol", arg,
      "Profile lacks an argic horizon -- Luvisol RSG cannot apply."))
  }

  h <- pedon$horizons
  layers <- arg$layers

  tests <- list(argic = arg)
  tests$cec_high   <- test_cec_per_clay_above(h,
                                                min_cmol_per_kg_clay = min_cec,
                                                candidate_layers     = layers)
  tests$al_sat_low <- test_al_saturation_below(h,
                                                  max_pct = max_al_sat,
                                                  candidate_layers = layers)

  agg <- .argic_derived_aggregate(tests,
                                    layer_keys = c("cec_high", "al_sat_low"))

  DiagnosticResult$new(
    name      = "luvisol",
    passed    = agg$passed,
    layers    = agg$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Luvisols"
  )
}


# ----------------------------------------------------------- helpers ----

#' @keywords internal
.argic_derived_negative <- function(name, arg_res, note) {
  DiagnosticResult$new(
    name      = name,
    passed    = if (is.na(arg_res$passed)) NA else FALSE,
    layers    = integer(0),
    evidence  = list(argic = arg_res),
    missing   = arg_res$missing %||% character(0),
    reference = sprintf("IUSS Working Group WRB (2022), Chapter 5, %s",
                          tools::toTitleCase(name)),
    notes     = note
  )
}

#' @keywords internal
.argic_derived_aggregate <- function(tests, layer_keys) {
  # Layers passing = intersection of argic layers AND each chemistry test's layers
  layer_lists <- list(tests$argic$layers)
  for (k in layer_keys) {
    layer_lists[[length(layer_lists) + 1]] <- tests[[k]]$layers
  }
  layers_passing <- Reduce(intersect, layer_lists)

  missing <- unique(unlist(lapply(tests, function(t) t$missing %||% character(0))))
  if (is.null(missing)) missing <- character(0)

  any_test_na <- any(vapply(tests, function(t) is.na(t$passed), logical(1)))

  passed <- if (length(layers_passing) > 0L) TRUE
            else if (any_test_na && length(missing) > 0L) NA
            else FALSE

  list(passed = passed, layers = layers_passing, missing = missing)
}
