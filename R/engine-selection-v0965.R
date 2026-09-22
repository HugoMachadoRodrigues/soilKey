# =============================================================================
# v0.9.65 -- per-pedon engine-selection heuristic.
#
# v0.9.62 / v0.9.63 introduced the `engine = c("soilkey", "aqp")`
# argument on argic / cambic + the global option
# `soilKey.diagnostic_engine`. The v0.9.63 BDsolos RJ benchmark
# showed the trade-off:
#   * aqp helps when morphology is rich (BDsolos RJ: SiBCS Order
#     40.3% -> 44.4%)
#   * aqp hurts when morphology is sparse (BDsolos nation-wide:
#     33.3% -> 30.2%)
#
# Conclusion: the right engine is per-pedon, not per-package.
# `pick_engine()` chooses based on data-completeness heuristics:
# pedons with full morphology (designation, structure, clay-films,
# Munsell) get aqp's canonical NRCS thresholds; pedons with sparse
# morphology stay on soilKey's data-quality-aware hand-coded path.
# =============================================================================


#' Choose the best diagnostic engine for a single pedon
#'
#' Per-pedon heuristic: returns \code{"aqp"} if the pedon's horizon
#' table has the morphological richness that makes aqp's canonical
#' NRCS dispatch reliable, otherwise returns \code{"soilkey"} (the
#' more permissive hand-coded path).
#'
#' @section Heuristic:
#'
#' We score each pedon on a 0-5 morphology-completeness scale; aqp
#' fires when score \\>= \code{min_score} (default 3). The five
#' axes:
#' \enumerate{
#'   \item \strong{Designation present} (any layer has a non-blank
#'         \code{designation}, e.g. "A1", "Bt2", "Bw").
#'   \item \strong{Texture quantitative} (any layer has both
#'         \code{clay_pct} and \code{sand_pct} populated).
#'   \item \strong{Munsell complete} (any layer has all three of
#'         \code{munsell_hue_moist}, \code{munsell_value_moist},
#'         \code{munsell_chroma_moist} populated).
#'   \item \strong{Structure recorded} (any layer has a non-blank
#'         \code{structure_grade}).
#'   \item \strong{Clay films / argic evidence} (any layer has
#'         a non-blank \code{clay_films_amount} or designation
#'         pattern matching \code{Bt}).
#' }
#'
#' @section Why this matters:
#'
#' On BDsolos RJ (data-rich), the heuristic recommends aqp for
#' ~99% of pedons (their full morphology + chemistry justifies the
#' canonical thresholds). On LUCAS topsoil-only (data-sparse), it
#' recommends aqp for ~0% of pedons, because the structure /
#' clay-films / designation axes are unfilled. Calling
#' \code{classify_*(pedon)} routed through the heuristic gives the
#' correct engine per pedon, recovering both the BDsolos RJ lift
#' AND the LUCAS robustness.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param min_score Integer (1-5). Minimum completeness score for
#'        \code{"aqp"} engine to fire (default 3).
#' @return Character: \code{"aqp"} or \code{"soilkey"}.
#' @seealso \code{\link{argic}}, \code{\link{cambic}}.
#' @export
pick_engine <- function(pedon, min_score = 3L) {
  hz <- pedon$horizons
  if (is.null(hz) || nrow(hz) == 0L) return("soilkey")
  score <- 0L

  # 1. Designation present
  if (any(!is.na(hz$designation) & nzchar(trimws(hz$designation %||% ""))))
    score <- score + 1L

  # 2. Texture quantitative
  has_clay <- any(!is.na(hz$clay_pct))
  has_sand <- any(!is.na(hz$sand_pct))
  if (has_clay && has_sand) score <- score + 1L

  # 3. Munsell complete (hue + value + chroma)
  has_munsell <- any(!is.na(hz$munsell_hue_moist) &
                       !is.na(hz$munsell_value_moist) &
                       !is.na(hz$munsell_chroma_moist))
  if (has_munsell) score <- score + 1L

  # 4. Structure recorded
  if (!is.null(hz$structure_grade) &&
        any(!is.na(hz$structure_grade) &
              nzchar(trimws(hz$structure_grade %||% ""))))
    score <- score + 1L

  # 5. Clay films / argic evidence
  has_films <- !is.null(hz$clay_films_amount) &&
                 any(!is.na(hz$clay_films_amount) &
                       nzchar(trimws(hz$clay_films_amount %||% "")))
  has_bt    <- !is.null(hz$designation) &&
                 any(!is.na(hz$designation) &
                       grepl("Bt", hz$designation, fixed = TRUE))
  if (has_films || has_bt) score <- score + 1L

  if (score >= min_score) "aqp" else "soilkey"
}


#' Per-pedon batch engine recommendation
#'
#' Vectorised version of \code{\link{pick_engine}} returning the
#' recommended engine for each pedon in a list.
#'
#' @param pedons A list of \code{\link{PedonRecord}} objects.
#' @param min_score Integer; forwarded to \code{pick_engine}.
#' @return Character vector of length(pedons) with values
#'   "aqp" or "soilkey".
#' @export
pick_engine_batch <- function(pedons, min_score = 3L) {
  vapply(pedons, function(p) pick_engine(p, min_score = min_score),
         character(1L))
}


#' Classify a pedon with the engine chosen by `pick_engine()`
#'
#' Convenience wrapper that routes \code{\link{classify_wrb2022}} /
#' \code{\link{classify_sibcs}} / \code{\link{classify_usda}}
#' through whichever engine the heuristic recommends for the
#' specific pedon.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param system One of \code{"wrb2022"}, \code{"sibcs"}, \code{"usda"}.
#' @param min_score Forwarded to \code{pick_engine}.
#' @param ... Forwarded to the underlying classifier.
#' @return The result of the chosen classifier (a
#'   \code{\link{ClassificationResult}}). The chosen engine is
#'   captured in \code{$trace$engine_used}.
#' @export
classify_with_engine_heuristic <- function(pedon,
                                              system = c("wrb2022",
                                                          "sibcs",
                                                          "usda"),
                                              min_score = 3L,
                                              ...) {
  system <- match.arg(system)
  engine <- pick_engine(pedon, min_score = min_score)
  old_opt <- getOption("soilKey.diagnostic_engine", NULL)
  options(soilKey.diagnostic_engine = engine)
  on.exit(options(soilKey.diagnostic_engine = old_opt), add = TRUE)
  classifier <- switch(system,
                          wrb2022 = classify_wrb2022,
                          sibcs   = classify_sibcs,
                          usda    = classify_usda)
  res <- classifier(pedon, ...)
  if (!is.null(res$trace) && is.list(res$trace))
    res$trace$engine_used <- engine
  res
}
