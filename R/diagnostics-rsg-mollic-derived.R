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
#   Phaeozem (PH):   mollic + NO secondary carbonates anywhere
#
# v0.2d simplification: the WRB depth restrictions on the carbonate
# accumulation ("within 50 cm of the lower limit of the mollic" /
# "within 100 cm of soil surface") are not enforced -- any layer with
# caco3_pct > 0 is treated as "carbonates present". Refinement is
# scheduled for v0.3 when carbonate depth localisation joins the
# qualifier-resolution work.
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
#' Tests whether a profile satisfies the Phaeozem RSG criteria: a
#' mollic horizon AND no secondary carbonate accumulation anywhere in
#' the profile.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references IUSS Working Group WRB (2022), Chapter 5, Phaeozems.
#' @export
phaeozem <- function(pedon) {
  mol <- mollic(pedon)
  if (!isTRUE(mol$passed)) {
    return(.mollic_derived_negative("phaeozem", mol,
      "Profile lacks a mollic horizon -- Phaeozem RSG cannot apply."))
  }

  h <- pedon$horizons
  tests <- list(mollic = mol)
  tests$carbonates <- test_carbonates_present(h)
  tests$no_carbonates <- list(
    passed  = if (is.na(tests$carbonates$passed)) NA
               else !isTRUE(tests$carbonates$passed),
    layers  = if (isTRUE(tests$carbonates$passed)) integer(0)
               else seq_len(nrow(h)),
    missing = tests$carbonates$missing,
    details = list(carbonates_passed = tests$carbonates$passed),
    notes   = if (isTRUE(tests$carbonates$passed))
                "Profile has secondary carbonates -- Chernozem/Kastanozem path"
              else NA_character_
  )

  agg <- .mollic_derived_aggregate(
    tests,
    require_pass = c("mollic", "no_carbonates")
  )

  DiagnosticResult$new(
    name      = "phaeozem",
    passed    = agg$passed,
    layers    = mol$layers,
    evidence  = tests,
    missing   = agg$missing,
    reference = "IUSS Working Group WRB (2022), Chapter 5, Phaeozems"
  )
}


# ----------------------------------------------------------- helpers ----

#' @keywords internal
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

#' @keywords internal
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
