# ================================================================
# WRB 2022 -- argic-derived RSG diagnostics
#
# v0.9.310: the four gates follow the WRB 2022 key (Ch 4, p. 119-122),
# which tests them in this order, each on the "other soils":
#
#   Acrisols (p. 119): argic starting <= 100 cm + CEC < 24 cmolc/kg clay
#                      in some argic subhorizon within 150 cm + exch. Al >
#                      exch. (Ca+Mg+K+Na) in half or more of 50-100 cm
#   Lixisols (p. 120): argic starting <= 100 cm + the CEC test
#   Alisols  (p. 121): argic starting <= 100 cm + the Al test
#   Luvisols (p. 122): argic starting <= 100 cm
#
# Until v0.9.309 they used the WRB 2014 tests (base saturation below or
# above 50% and Al saturation in the argic layers, CEC >= 24 for Alisols
# and Luvisols) and no depth limit. Each gate tests only its own criteria;
# the key's order separates them (a Lixisol is not an Acrisol because the
# Acrisols come first). The Acric, Lixic, Alic and Luvic qualifiers (Ch 5)
# are defined apart and are in qualifiers-wrb2022.R.
# ================================================================


#' Acrisol RSG diagnostic (WRB 2022)
#'
#' The Acrisols of the WRB 2022 key (Ch 4, p. 119): an argic horizon starting
#' \eqn{\le} 100 cm from the mineral soil surface; a CEC (1 M NH4OAc, pH 7) of
#' < 24 cmolc/kg clay in some subhorizon of the argic horizon within 150 cm of
#' the mineral soil surface; and exchangeable Al > exchangeable (Ca+Mg+K+Na) in
#' half or more of 50-100 cm from the mineral soil surface, or of the lower half
#' of the mineral soil above a limiting layer starting \eqn{\le} 100 cm,
#' whichever is shallower. Where the cations are not available, the pH is read
#' by WRB 2022 Annex 2, Table 9.3.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cec CEC per kg clay below which the clay is of low activity
#'        (default 24).
#' @param max_bs Unused since v0.9.310 (WRB 2022 does not use the base
#'        saturation here), kept so that existing calls still run.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 4, Acrisols (p. 119).
#' @export
acrisol <- function(pedon, max_cec = 24, max_bs = 50) {
  .argic_derived_gate(pedon, "acrisol", cec = "low", al = "gt", max_cec = max_cec,
                      ref = "IUSS Working Group WRB (2022), Chapter 4, Acrisols (p. 119)")
}


#' Lixisol RSG diagnostic (WRB 2022)
#'
#' The Lixisols of the WRB 2022 key (Ch 4, p. 120): an argic horizon starting
#' \eqn{\le} 100 cm from the mineral soil surface and a CEC of < 24 cmolc/kg
#' clay in some subhorizon of the argic horizon within 150 cm. The key reaches
#' them after the Acrisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_cec CEC per kg clay below which the clay is of low activity
#'        (default 24).
#' @param min_bs Unused since v0.9.310, kept so that existing calls still run.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 4, Lixisols (p. 120).
#' @export
lixisol <- function(pedon, max_cec = 24, min_bs = 50) {
  .argic_derived_gate(pedon, "lixisol", cec = "low", al = "none", max_cec = max_cec,
                      ref = "IUSS Working Group WRB (2022), Chapter 4, Lixisols (p. 120)")
}


#' Alisol RSG diagnostic (WRB 2022)
#'
#' The Alisols of the WRB 2022 key (Ch 4, p. 121): an argic horizon starting
#' \eqn{\le} 100 cm from the mineral soil surface and exchangeable Al >
#' exchangeable (Ca+Mg+K+Na) in half or more of 50-100 cm (or of the lower half
#' of the mineral soil above a limiting layer starting \eqn{\le} 100 cm). The
#' key reaches them after the Acrisols and Lixisols.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cec,min_al_sat Unused since v0.9.310, kept so that existing
#'        calls still run.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 4, Alisols (p. 121).
#' @export
alisol <- function(pedon, min_cec = 24, min_al_sat = 50) {
  .argic_derived_gate(pedon, "alisol", cec = "none", al = "gt",
                      ref = "IUSS Working Group WRB (2022), Chapter 4, Alisols (p. 121)")
}


#' Luvisol RSG diagnostic (WRB 2022)
#'
#' The Luvisols of the WRB 2022 key (Ch 4, p. 122): "Other soils having an
#' argic horizon starting \eqn{\le} 100 cm from the mineral soil surface".
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_cec,max_al_sat Unused since v0.9.310, kept so that existing
#'        calls still run.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 4, Luvisols (p. 122).
#' @export
luvisol <- function(pedon, min_cec = 24, max_al_sat = 50) {
  .argic_derived_gate(pedon, "luvisol", cec = "none", al = "none",
                      ref = "IUSS Working Group WRB (2022), Chapter 4, Luvisols (p. 122)")
}


# ----------------------------------------------------------- helpers ----

#' The tests of the argic family (WRB 2022 Ch 4 p. 119-122, and the Acric,
#' Alic, Lixic and Luvic qualifiers of Ch 5): \code{cec} is "low" (< max_cec
#' in some argic subhorizon within 150 cm), "high" (>= max_cec throughout the
#' argic horizon within 150 cm) or "none"; \code{al} is "gt" (Al > bases in
#' half or more of the depth range), "le" (Al <= bases in half or more) or
#' "none". Every gate asks for an argic horizon starting <= 100 cm from the
#' mineral soil surface.
#' @noRd
.argic_derived_gate <- function(pedon, name, cec = c("none", "low", "high"),
                                al = c("none", "gt", "le"), max_cec = 24,
                                ref = NULL) {
  cec <- match.arg(cec); al <- match.arg(al)
  ref <- ref %||% sprintf("IUSS Working Group WRB (2022), Chapter 4, %s",
                          tools::toTitleCase(name))
  arg <- argic(pedon)
  if (!isTRUE(arg$passed))
    return(.argic_derived_negative(name, arg, sprintf(
      "Profile lacks an argic horizon -- %s cannot apply.", tools::toTitleCase(name)),
      reference = ref))
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  argic_top <- min(h$top_cm[arg$layers], na.rm = TRUE)
  tests <- list(argic = arg)
  tests$argic_within_100 <- argic_top - ms <= 100
  # "in some subhorizon of the argic horizon within 150 cm of the mineral soil
  # surface": the argic layers that start above 150 cm.
  within150 <- arg$layers[!is.na(h$top_cm[arg$layers]) & h$top_cm[arg$layers] - ms < 150]
  if (cec != "none") {
    t <- test_cec_per_clay(h, max_cmol_per_kg_clay = max_cec,
                           candidate_layers = within150)
    known <- vapply(t$details, function(d) !is.null(d$passed), logical(1))
    all_known <- length(within150) > 0L && length(known) == length(within150) && all(known)
    low_some <- if (length(t$layers)) TRUE else if (all_known) FALSE else NA
    # Lixisols and the Acric and Lixic qualifiers: "< 24 ... in some
    # subhorizon"; the Alic and Luvic qualifiers: ">= 24 ... throughout".
    tests$cec <- if (cec == "low") low_some
                 else if (isTRUE(low_some)) FALSE else if (all_known) TRUE else NA
    tests$cec_detail <- t
  }
  if (al != "none") {
    ad <- .wrb_al_dominance(pedon)
    tests$al <- if (al == "gt") ad$al_gt else ad$al_le
    tests$al_detail <- ad
  }
  crit <- unlist(tests[intersect(c("argic_within_100", "cec", "al"), names(tests))])
  passed <- .and3(crit)
  missing <- character(0)
  if (is.na(passed)) {
    if ("cec" %in% names(tests) && is.na(tests$cec))
      missing <- c(missing, tests$cec_detail$missing, "cec_cmol", "clay_pct")
    if ("al" %in% names(tests) && is.na(tests$al))
      missing <- c(missing, "al_cmol", "ca_cmol", "mg_cmol", "k_cmol", "na_cmol",
                   "ph_h2o", "oc_pct")
  }
  DiagnosticResult$new(
    name      = name,
    passed    = passed,
    layers    = if (isTRUE(passed)) arg$layers else integer(0),
    evidence  = tests,
    missing   = unique(missing),
    reference = ref
  )
}

#' Internal helper: .argic_derived_negative
#' @noRd
.argic_derived_negative <- function(name, arg_res, note, reference = NULL) {
  DiagnosticResult$new(
    name      = name,
    passed    = if (is.na(arg_res$passed)) NA else FALSE,
    layers    = integer(0),
    evidence  = list(argic = arg_res),
    missing   = arg_res$missing %||% character(0),
    reference = reference %||% sprintf("IUSS Working Group WRB (2022), Chapter 4, %s",
                                       tools::toTitleCase(name)),
    notes     = note
  )
}
