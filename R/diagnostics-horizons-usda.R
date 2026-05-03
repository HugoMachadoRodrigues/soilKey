# ================================================================
# USDA Soil Taxonomy -- Diagnostic horizons (v0.2 scaffold)
#
# v0.2 scaffold scope:
#   - oxic_horizon_usda  : delegated to WRB ferralic() (the criteria
#                            agree on the central tendency: low-activity
#                            clay, low CEC/clay, low-weatherable-mineral
#                            content). Differences in the two
#                            standards' edge cases are scheduled for v0.8.
#   - argillic_usda      : delegated to WRB argic() with the same caveat.
#                            Real USDA argillic also requires illuviation
#                            evidence (clay films) which v0.8 will enforce.
#
# All other diagnostic horizons (mollic_usda, umbric_usda, ochric,
# kandic, spodic_usda, cambic_usda, calcic_usda, gypsic_usda,
# duripan, fragipan, placic, albic, petrocalcic, petrogypsic, ...)
# are scheduled for v0.8 alongside the parallel USDA Soil Taxonomy
# implementation.
# ================================================================


#' Oxic horizon (USDA Soil Taxonomy)
#'
#' The USDA oxic horizon is the diagnostic of Oxisols. Its central
#' criteria match the WRB 2022 ferralic horizon closely enough that
#' v0.2 simply delegates: every fixture that classifies as Oxisol via
#' USDA also classifies as Ferralsol via WRB and vice-versa. The
#' fine-grained differences (USDA's water-dispersible-clay test, the
#' sand-fraction weatherable-mineral cut-offs) are tracked in the
#' diagnostics.yaml for v0.8 refinement.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Passed to \code{\link{ferralic}}.
#' @return A \code{\link{DiagnosticResult}} (with \code{name = "oxic_usda"}).
#' @references Soil Survey Staff (2014). \emph{Keys to Soil Taxonomy},
#'   12th edition. USDA-NRCS, Washington DC. Chapter 3 -- Diagnostic
#'   Horizons; oxic.
#' @export
oxic_usda <- function(pedon, ...) {
  res <- ferralic(pedon, ...)
  res$name      <- "oxic_usda"
  res$reference <- paste0("Soil Survey Staff (2014), Keys to Soil Taxonomy, ",
                            "Ch. 3, oxic horizon -- delegating to WRB ",
                            "ferralic() in v0.2 scaffold")
  res$notes     <- "v0.2 scaffold delegates to WRB ferralic; refinement v0.8"
  res
}


#' Argillic horizon (USDA Soil Taxonomy)
#'
#' v0.2 scaffold delegating to WRB \code{\link{argic}}. The two
#' diagnostics' clay-increase rules are essentially the same; USDA
#' argillic additionally requires evidence of clay illuviation (clay
#' films / clay bridges) on at least 1\% of the surface area, which
#' v0.8 will enforce against the \code{clay_films_amount} column.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param ... Passed to \code{\link{argic}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2014), Keys to Soil Taxonomy,
#'   Ch. 3 -- argillic horizon.
#' @export
argillic_usda <- function(pedon, ...) {
  # v0.9.27: KST 13ed Ch 3 (p 4) argillic = clay increase + clay
  # illuviation evidence. The two-tier strategy:
  #
  #   tier 1 (FULL evidence): KST thresholds (3/1.2/8) +
  #     clay-films-test gate. Requires either NASIS pediagfeatures
  #     "Argillic horizon" featkind OR per-horizon clay_films_amount
  #     filled in. Catches the looser-threshold profiles that the
  #     v0.9.26 +0 pp setting was missing because we couldn't verify
  #     clay illuviation.
  #
  #   tier 2 (PROXY): when clay-films evidence is absent (lab-only
  #     loaders, profiles without NASIS enrichment), fall back to
  #     WRB stricter thresholds (6/1.4/20) which act as a
  #     conservative proxy. Empirically this prevents the regression
  #     of -1.28 pp Order documented in v0.9.26.
  #
  # The fluvic-pattern exclusion (v0.9.10) is preserved across both
  # tiers -- depositional clay distributions are NOT argillic regardless
  # of clay-films evidence, because the increase is non-pedogenic.

  has_clay_films_evidence <- argillic_clay_films_test(pedon)$passed
  used_system <- if (isTRUE(has_clay_films_evidence)) "usda" else "wrb2022"
  res <- argic(pedon, system = used_system, ...)

  # v0.9.10: fluvic-pattern exclusion (depositional, not pedogenic).
  if (isTRUE(res$passed)) {
    fluv <- carater_fluvico(pedon)
    if (isTRUE(fluv$passed)) {
      res$passed <- FALSE
      res$layers <- integer(0)
      res$notes  <- paste0("v0.9.10: argic clay-jump matched but ",
                             "carater_fluvico(pedon) is TRUE -- ",
                             "depositional pattern, not pedogenic. ",
                             "Argillic excluded.")
    }
  }

  # v0.9.27: record which tier was used in evidence for trace.
  if (is.null(res$evidence)) res$evidence <- list()
  res$evidence$argillic_tier <- list(
    clay_films_evidence = isTRUE(has_clay_films_evidence),
    threshold_system    = used_system,
    note = if (isTRUE(has_clay_films_evidence))
              "v0.9.27: clay-films evidence present -> KST 13ed thresholds (3/1.2/8)"
           else
              "v0.9.27: no clay-films evidence -> WRB stricter thresholds (6/1.4/20) as proxy"
  )

  res$name      <- "argillic_usda"
  res$reference <- paste0("Soil Survey Staff (2022), Keys to Soil Taxonomy ",
                            "13th ed., Ch. 3, argillic horizon")
  res
}


#' Test for clay-illuviation evidence (KST 13ed Ch 3 p 4)
#'
#' KST 13ed argillic horizon requires "evidence of illuvial accumulation
#' of clay" alongside the clay-increase rule. Acceptable evidence:
#' \itemize{
#'   \item oriented clays bridging sand grains in >= 1\% of the horizon;
#'   \item clay films lining pores or coating ped faces;
#'   \item lamellae more than 5 mm thick.
#' }
#'
#' This test reads three complementary slots, in order of evidence strength:
#' \enumerate{
#'   \item \code{pedon$site$nasis_diagnostic_features} -- the NASIS
#'         \code{pediagfeatures.featkind} vector. The surveyor's
#'         explicit "Argillic horizon" entry directly confirms
#'         clay-illuviation evidence (~13 500 entries in the 2021
#'         NASIS snapshot). Strongest evidence.
#'   \item \code{pedon$horizons$clay_films_amount} -- per-horizon
#'         clay-film abundance derived from NASIS \code{phpvsf}.
#'         Values: \code{"few"}, \code{"common"}, \code{"many"},
#'         \code{"continuous"}. Direct measurement.
#'   \item \code{pedon$horizons$designation} containing a 't'
#'         master suffix (e.g. \code{Bt}, \code{Btk}, \code{Btx},
#'         \code{Bt1}, \code{2Bt}). v0.9.28: the pedologist who
#'         wrote that designation explicitly identified the
#'         horizon as clay-illuvial -- per KST 13ed Ch 18, the 't'
#'         suffix means "accumulation of silicate clay" -- so it
#'         counts as positive evidence even when NASIS records are
#'         absent. This unlocks the KST 13ed argillic thresholds
#'         for the ~47 % of KSSL profiles that lack NASIS
#'         pediagfeatures and phpvsf records.
#' }
#'
#' Any of the three sources counts as positive evidence (logical OR).
#' \code{passed = NA} when none is populated AND no horizon designation
#' field is present at all (lab-only loaders without horizon
#' descriptions). \code{passed = FALSE} when designations exist but
#' none has a 't' suffix and NASIS slots are empty.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @return A \code{\link{DiagnosticResult}}.
#' @references Soil Survey Staff (2022), Keys to Soil Taxonomy 13th
#'   ed., Ch. 3, argillic horizon (clay-illuviation criteria, p. 4);
#'   Ch. 18, master horizon symbols (\code{t}: silicate-clay
#'   accumulation, p. 332).
#' @export
argillic_clay_films_test <- function(pedon) {
  feats <- pedon$site$nasis_diagnostic_features
  has_pediag <- !is.null(feats) && length(feats) > 0L &&
                  any(!is.na(feats) &
                        grepl("argillic", feats, ignore.case = TRUE))

  h <- pedon$horizons
  cf_col <- h$clay_films_amount
  has_films_any <- !is.null(cf_col) &&
                     any(!is.na(cf_col) & nzchar(cf_col))

  # v0.9.28: designation-based proxy. KST 13ed Ch 18 (master horizon
  # symbols) defines the suffix 't' as "an accumulation of silicate
  # clay that has either formed in the horizon and is subsequently
  # translocated within it, or has been moved into the horizon by
  # illuviation". A pedologist who wrote 'Bt' / 'Btk' / 'Btx' / etc.
  # in the field designation is making exactly the clay-illuviation
  # claim that the KST 13ed argillic test requires. We treat that as
  # positive evidence equivalent to a phpvsf clay-films record. The
  # match is strict: the suffix must be a master horizon AND have 't'
  # AS A SUFFIX OF THE MASTER (not just any 't' in the string -- "BAt"
  # is fine, "Btx" is fine, but "test" is not, and a hypothetical
  # made-up "BTest" would be rejected because we anchor to the master
  # horizon letters [ABCEORW]).
  des_col <- h$designation
  has_t_designation <- FALSE
  designation_layers <- integer(0)
  if (!is.null(des_col) && length(des_col) > 0L) {
    # Match: optional digit prefix + ONE OR MORE master letters
    # [ABCEORW] (transitional horizons like AB / BA / BC have multiple
    # master letters) + a 't' suffix possibly followed by other
    # suffixes/digits. The 't' must be a lowercase suffix letter,
    # NOT a master horizon letter.
    # Examples that match: Bt, Bt1, Btk, Btx, 2Bt, Btss, BAt, ABt, B't.
    # Examples that do NOT match: A, Bw, Bk, BC, C, R, O, Bg, Bs, Bh,
    #                              "test" (no master letters).
    t_pat <- "^[0-9']*[ABCEORW]+[a-z]*t[a-z0-9']*$"
    designation_layers <- which(!is.na(des_col) & nzchar(des_col) &
                                  grepl(t_pat, des_col, ignore.case = FALSE))
    has_t_designation <- length(designation_layers) > 0L
  }

  has_evidence <- isTRUE(has_pediag) ||
                    isTRUE(has_films_any) ||
                    isTRUE(has_t_designation)

  designation_absent <- is.null(des_col) || length(des_col) == 0L ||
                          all(is.na(des_col) | !nzchar(des_col))
  nasis_absent <- (is.null(feats) || length(feats) == 0L) &&
                    (is.null(cf_col) || all(is.na(cf_col)))
  evidence_absent <- nasis_absent && designation_absent

  passed <- if (has_evidence) TRUE
            else if (evidence_absent) NA
            else FALSE

  films_layers <- if (!is.null(cf_col))
                      which(!is.na(cf_col) & nzchar(cf_col))
                    else integer(0)

  # Identify which evidence source produced the TRUE: useful for
  # downstream provenance / debugging.
  evidence_source <- if (isTRUE(has_pediag)) "nasis_pediagfeatures"
                     else if (isTRUE(has_films_any)) "nasis_phpvsf"
                     else if (isTRUE(has_t_designation)) "designation_t_suffix"
                     else NA_character_

  DiagnosticResult$new(
    name = "argillic_clay_films_test",
    passed = passed,
    layers = unique(c(films_layers, designation_layers)),
    evidence = list(
      pediagfeatures_argillic_flag = has_pediag,
      horizons_with_clay_films     = length(films_layers),
      films_summary                = if (length(films_layers) > 0L)
                                          unique(cf_col[films_layers])
                                       else character(0),
      horizons_with_t_designation  = length(designation_layers),
      t_designations               = if (length(designation_layers) > 0L)
                                          des_col[designation_layers]
                                       else character(0),
      evidence_source              = evidence_source
    ),
    missing = if (evidence_absent)
                c("nasis_diagnostic_features", "clay_films_amount", "designation")
              else character(0),
    reference = paste0("Soil Survey Staff (2022), Keys to Soil Taxonomy ",
                         "13th ed., Ch. 3, argillic horizon (clay illuviation, p 4); ",
                         "Ch. 18, master horizon symbols (t suffix, p 332)")
  )
}
