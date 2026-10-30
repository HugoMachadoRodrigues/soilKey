# ================================================================
# WRB 2022 -- mollic-derived RSG diagnostics
#
# Three diagnostics that disambiguate the "mollic family" RSGs by
# combining the mollic horizon test with carbonate and colour
# criteria:
#
#   Chernozem (CH):  mollic + secondary carbonates + chroma <= 2 in
#                    the upper 20 cm of the mollic
#   Kastanozem (KS): mollic + secondary carbonates + NOT Chernozem
#                    colour (chroma > 2 in the upper 20 cm)
#   Phaeozem (PH):   mollic + BS >= 50% throughout to 100 cm (v0.9.309)
#
# chernozem() and kastanozem() are the v0.2d simplifications (any layer
# with caco3_pct > 0 counts as "carbonates present"); the key uses
# chernozem_strict() and kastanozem_strict() (R/diagnostics-rsg-tier2.R).
# phaeozem() follows WRB 2022 Ch 4 (p. 113) since v0.9.309.
# ================================================================


#' Chernozem RSG diagnostic (WRB 2022)
#'
#' Tests whether a profile satisfies the Chernozem RSG criteria:
#' a mollic horizon plus secondary carbonates somewhere in the
#' profile, plus chroma (moist) <= 2 in at least one layer of the
#' upper 20 cm.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_chroma_upper Maximum moist chroma in the upper part
#'        (default 2, per WRB 2022).
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Chernozems.
#' @export
chernozem <- function(pedon, max_chroma_upper = 2) {
  mol <- mollic(pedon)
  if (!isTRUE(mol$passed)) {
    return(.mollic_derived_negative("chernozem", mol,
      "Profile lacks a mollic horizon -- Chernozem RSG cannot apply."))
  }

  h <- pedon$horizons
  tests <- list(mollic = mol)
  tests$carbonates <- test_carbonates_present(h)
  tests$dark_upper <- test_chernic_color(h, max_chroma = max_chroma_upper)

  agg <- .mollic_derived_aggregate(
    tests,
    require_pass = c("mollic", "carbonates", "dark_upper")
  )

  DiagnosticResult$new(
    name      = "chernozem",
    passed    = agg$passed,
    layers    = mol$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Chernozems"
  )
}


#' Kastanozem RSG diagnostic (WRB 2022)
#'
#' Tests whether a profile satisfies the Kastanozem RSG criteria: a
#' mollic horizon plus secondary carbonates plus NOT-Chernozem colour
#' (chroma (moist) > 2 in the upper 20 cm).
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param max_chroma_upper Maximum moist chroma to qualify as
#'        Chernozem (default 2). Kastanozem requires the upper-20-cm
#'        chroma to EXCEED this value.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Kastanozems.
#' @export
kastanozem <- function(pedon, max_chroma_upper = 2) {
  mol <- mollic(pedon)
  if (!isTRUE(mol$passed)) {
    return(.mollic_derived_negative("kastanozem", mol,
      "Profile lacks a mollic horizon -- Kastanozem RSG cannot apply."))
  }

  h <- pedon$horizons
  tests <- list(mollic = mol)
  tests$carbonates <- test_carbonates_present(h)
  tests$dark_upper <- test_chernic_color(h, max_chroma = max_chroma_upper)
  # Kastanozem = NOT chernozem-dark; we negate the dark_upper test
  tests$not_dark_upper <- list(
    passed  = if (is.na(tests$dark_upper$passed)) NA
               else !isTRUE(tests$dark_upper$passed),
    layers  = if (isTRUE(tests$dark_upper$passed)) integer(0)
               else seq_len(nrow(h)),
    missing = tests$dark_upper$missing,
    details = list(dark_upper_passed = tests$dark_upper$passed),
    notes   = if (isTRUE(tests$dark_upper$passed))
                "Profile too dark in upper 20 cm -- Chernozem path"
              else NA_character_
  )

  agg <- .mollic_derived_aggregate(
    tests,
    require_pass = c("mollic", "carbonates", "not_dark_upper")
  )

  DiagnosticResult$new(
    name      = "kastanozem",
    passed    = agg$passed,
    layers    = mol$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Kastanozems"
  )
}


#' Phaeozem RSG diagnostic (WRB 2022)
#'
#' Tests whether a profile satisfies the Phaeozem RSG criteria (WRB 2022
#' Ch 4, p. 113): a mollic horizon AND a base saturation (1 M NH4OAc, pH 7)
#' \\>= 50\% throughout to 100 cm from the mineral soil surface or to a
#' limiting layer, whichever is shallower. Carbonates are allowed: the
#' Chernozems and Kastanozems are keyed out before. Where BS is not
#' recorded, pH is read as in WRB 2022 Annex 2, Table 9.2. A profile not
#' described down to that depth is undecided.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 4, Phaeozems (p. 113).
#' @export
phaeozem <- function(pedon) {
  mol <- mollic(pedon)
  if (!isTRUE(mol$passed)) {
    return(.mollic_derived_negative("phaeozem", mol,
      "Profile lacks a mollic horizon -- Phaeozem RSG cannot apply."))
  }

  # v0.9.309: WRB 2022 Ch 4, Phaeozems (p. 113), "a base saturation ... of
  # >= 50% throughout to a depth of 100 cm from the mineral soil surface or to
  # a limiting layer, whichever is shallower". The old test (no CaCO3 in any
  # layer) is not in the text.
  h <- pedon$horizons
  ms <- .q_mineral_surface_cm(h)
  if (is.na(ms)) ms <- 0
  lim <- .barrier_top_cm(pedon)
  bottom <- if (!is.na(lim)) min(ms + 100, lim) else ms + 100
  win <- .wrb_window_layers(h, ms, bottom)
  bs <- .wrb_bs_throughout(h, win$layers, min_bs = 50)
  described <- win$covered_cm >= win$window_cm - 1e-6
  # Hugo (2026-10-29): a profile described above 100 cm (or with gaps) is
  # decided on the part that is described
  passed <- if (isFALSE(bs$passed)) FALSE
            else if (isTRUE(bs$passed) && length(win$layers)) TRUE
            else NA

  tests <- list(
    mollic = mol,
    base_saturation = list(
      passed = passed,
      layers = win$layers,
      missing = c(bs$missing,
                  character(0)),
      details = list(window_cm = c(ms, bottom), covered_cm = win$covered_cm,
                     limiting_layer_top_cm = lim, layers = bs$details)
    )
  )

  DiagnosticResult$new(
    name      = "phaeozem",
    passed    = passed,
    layers    = mol$layers,
    evidence  = tests,
    missing   = if (is.na(passed)) unique(tests$base_saturation$missing)
                else character(0),
    reference = "IUSS Working Group WRB (2022), Chapter 4, Phaeozems (p. 113)"
  )
}


# ----------------------------------------------------------- helpers ----
#' Internal helper: .mollic_derived_negative

#' @noRd
.mollic_derived_negative <- function(name, mol_res, note) {
  DiagnosticResult$new(
    name      = name,
    passed    = if (is.na(mol_res$passed)) NA else FALSE,
    layers    = integer(0),
    evidence  = list(mollic = mol_res),
    missing   = mol_res$missing %||% character(0),
    reference = sprintf("IUSS Working Group WRB (2022), Chapter 5, %s",
                          tools::toTitleCase(name)),
    notes     = note
  )
}
#' Internal helper: .mollic_derived_aggregate

#' @noRd
.mollic_derived_aggregate <- function(tests, require_pass) {
  passed_vec <- vapply(tests[require_pass],
                        function(t) isTRUE(t$passed), logical(1))
  na_vec     <- vapply(tests[require_pass],
                        function(t) is.na(t$passed), logical(1))

  missing <- unique(unlist(lapply(tests, function(t) t$missing %||% character(0))))
  if (is.null(missing)) missing <- character(0)

  passed <- if (all(passed_vec)) TRUE
            else if (any(na_vec) && all(passed_vec | na_vec) &&
                       length(missing) > 0L) NA
            else FALSE

  list(passed = passed, missing = missing)
}
