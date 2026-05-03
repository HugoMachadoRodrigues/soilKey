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
  # v0.9.26 design note: KST 13ed Ch 3 (p 4) defines argillic with
  # LOOSER clay-increase thresholds than WRB argic (3/1.2/8 vs
  # 6/1.4/20) BUT REQUIRES additional evidence of clay illuviation
  # -- oriented clays bridging sand grains, clay films lining pores
  # or coating peds, OR lamellae > 5 mm thick. KSSL does not store
  # this evidence reliably (NASIS does, sparsely). Empirically,
  # using the LOOSER thresholds without the clay-illuviation test
  # produces many false-positive argillic detections (KSSL+NASIS
  # n=865 A/B: -1.28 pp Order, -0.92 pp Suborder).
  #
  # Pragmatic solution: keep the STRICTER WRB thresholds as a proxy
  # for "argillic with strong clay-increase evidence" -- the +6/1.4/20
  # cutoffs are restrictive enough that profiles passing them are
  # very likely to also have clay films, even when we can't verify
  # directly. Switch to system = "usda" once clay-films testing is
  # implemented (probably via NASIS pediagfeatures `argillic` flag).
  res <- argic(pedon, system = "wrb2022", ...)
  # v0.9.10: USDA Keys to Soil Taxonomy 13ed Ch. 3 (argillic horizon)
  # explicitly excludes horizons whose clay-distribution pattern is
  # depositional (alluvial / lithologic discontinuity / fluvic
  # material) -- the increase must be PEDOGENIC (illuvial clay
  # accumulation evidenced by clay films, lamellae, or oriented clay
  # bridges). Without this exclusion the canonical Fluvisol fixture
  # was incorrectly assigned to Alfisols (Paleudalfs) because the
  # C1->C2 stratification triggered argic. The exclusion mirrors the
  # SiBCS Cap 4 fix in `neossolo()` for the same fixture.
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
  res$name      <- "argillic_usda"
  res$reference <- paste0("Soil Survey Staff (2022), Keys to Soil Taxonomy ",
                            "13th ed., Ch. 3, argillic horizon")
  res
}
