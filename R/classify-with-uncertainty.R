# =============================================================================
# v0.9.100 -- provenance-weighted classification uncertainty.
#
# classification_robustness() answers "does the class hold?" with a single
# percentage. classify_with_uncertainty() answers the richer question "what is
# the probability distribution over classes?", and crucially it scales the
# Monte-Carlo noise by each cell's evidence grade -- so a profile resting on
# VLM-extracted or assumed values is correctly reported as more uncertain than
# one resting on laboratory measurements.
# =============================================================================


#' Posterior distribution over classification outcomes
#'
#' Runs \code{n} Monte-Carlo perturbations of a pedon and tallies the
#' resulting classes into an empirical posterior. Unlike
#' \code{\link{classification_robustness}}, the perturbation magnitude of
#' every \code{(horizon, attribute)} cell is scaled by its provenance
#' evidence grade (see \code{\link{get_perturbation_scale}}): an A-grade
#' measurement is nudged by a few percent, an E-grade assumption by a
#' third of its value. The posterior therefore reflects not just how
#' close the profile sits to a key boundary, but how trustworthy the
#' inputs that placed it there actually are.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param n Number of Monte-Carlo draws (default 200).
#' @param system One of \code{"wrb2022"}, \code{"sibcs"}, \code{"usda"}.
#' @param level \code{"rsg"} (default; compare the RSG / order) or
#'        \code{"name"} (compare the full classification name, qualifiers
#'        included -- strictly more uncertain).
#' @param scales Optional named list overriding the default per-grade
#'        magnitudes; each element has the shape returned by
#'        \code{\link{get_perturbation_scale}}, keyed by grade letter.
#' @param sensitivity If \code{TRUE} (default) also computes a
#'        leave-one-attribute-out sensitivity ranking. Set \code{FALSE}
#'        to skip that extra pass when only the posterior is needed.
#' @param seed Random seed for reproducibility.
#' @return A list of class \code{"soilkey_uncertainty"} with elements:
#'         \code{posterior} (named numeric vector summing to 1, sorted
#'         descending), \code{top1} (the modal class), \code{entropy}
#'         (Shannon entropy of the posterior, natural log), \code{sensitivity}
#'         (a \code{data.table} of \code{attribute} / \code{importance},
#'         or \code{NULL}), \code{n_runs}, \code{n_success},
#'         \code{baseline}, \code{system} and \code{level}.
#' @seealso \code{\link{classification_robustness}},
#'          \code{\link{get_perturbation_scale}},
#'          \code{\link{compute_per_attribute_evidence_grade}}.
#' @examples
#' \donttest{
#' p <- make_ferralsol_canonical()
#' u <- classify_with_uncertainty(p, n = 50, system = "wrb2022")
#' u$posterior   # P(RSG = x)
#' u$entropy     # near 0 for a robust profile
#' }
#' @export
classify_with_uncertainty <- function(pedon,
                                      n = 200L,
                                      system = c("wrb2022", "sibcs", "usda"),
                                      level = c("rsg", "name"),
                                      scales = NULL,
                                      sensitivity = TRUE,
                                      seed = 42L) {
  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord")
  }
  system <- match.arg(system)
  level  <- match.arg(level)
  n <- as.integer(n)
  if (is.na(n) || n < 1L) rlang::abort("`n` must be a positive integer")

  classify_fn <- switch(
    system,
    wrb2022 = function(p) classify_wrb2022(p, on_missing = "silent"),
    sibcs   = function(p) classify_sibcs(p,   on_missing = "silent"),
    usda    = function(p) classify_usda(p,    on_missing = "silent"))

  pick <- function(cls) {
    if (is.null(cls)) return(NA_character_)
    val <- if (level == "rsg") cls$rsg_or_order else cls$name
    val %||% NA_character_
  }

  baseline <- pick(tryCatch(classify_fn(pedon), error = function(e) NULL))

  build_result <- function(posterior, top1, entropy, sens, n_success) {
    structure(
      list(posterior = posterior, top1 = top1, entropy = entropy,
           sensitivity = sens, n_runs = n, n_success = n_success,
           baseline = baseline, system = system, level = level),
      class = "soilkey_uncertainty")
  }
  empty_sens <- data.table::data.table(attribute  = character(0),
                                       importance = numeric(0))

  pcols <- .perturbable_columns(pedon)
  if (length(pcols) == 0L) {
    rlang::warn("classify_with_uncertainty(): no perturbable attributes")
    return(build_result(NA, baseline, NA_real_, empty_sens, 0L))
  }

  grade_lookup <- .build_grade_lookup(pedon)

  set.seed(seed)
  results <- character(n)
  for (i in seq_len(n)) {
    pp <- .perturb_pedon_provenance(pedon, grade_lookup, scales)
    results[i] <- pick(tryCatch(classify_fn(pp), error = function(e) NULL))
  }
  ok <- results[!is.na(results)]
  if (length(ok) == 0L) {
    rlang::warn("classify_with_uncertainty(): every Monte-Carlo run failed")
    return(build_result(NA, baseline, NA_real_, empty_sens, 0L))
  }

  tab       <- table(ok)
  posterior <- sort(stats::setNames(as.numeric(tab) / sum(tab), names(tab)),
                    decreasing = TRUE)
  pp_pos    <- posterior[posterior > 0]
  # max(0, .) keeps a degenerate posterior at exactly 0, not -0.
  entropy   <- max(0, -sum(pp_pos * log(pp_pos)))
  top1      <- names(posterior)[1L]

  # Leave-one-attribute-out sensitivity: an attribute is "important" when
  # holding it fixed removes much of the classification's instability.
  sens <- NULL
  if (isTRUE(sensitivity)) {
    n_sens    <- min(n, 25L)
    base_flip <- mean(ok != baseline)
    imp <- vapply(seq_along(pcols), function(j) {
      set.seed(seed + j)
      r2 <- character(n_sens)
      for (i in seq_len(n_sens)) {
        pp2 <- .perturb_pedon_provenance(pedon, grade_lookup, scales,
                                         exclude_cols = pcols[j])
        r2[i] <- pick(tryCatch(classify_fn(pp2), error = function(e) NULL))
      }
      r2ok <- r2[!is.na(r2)]
      flip_without <- if (length(r2ok) == 0L) 0 else mean(r2ok != baseline)
      base_flip - flip_without
    }, numeric(1L))
    sens <- data.table::data.table(attribute = pcols, importance = imp)
    data.table::setorder(sens, -importance)
  }

  build_result(posterior, top1, entropy, sens, length(ok))
}


#' @export
print.soilkey_uncertainty <- function(x, ...) {
  cat(sprintf("<soilkey_uncertainty>  system=%s  level=%s\n",
              x$system, x$level))
  cat(sprintf("  baseline    : %s\n", x$baseline %||% "NA"))
  cat(sprintf("  MC runs     : %d (%d successful)\n",
              x$n_runs, x$n_success %||% 0L))
  if (length(x$posterior) == 1L && is.na(x$posterior[[1L]])) {
    cat("  posterior   : (not estimated -- no perturbable attributes)\n")
    return(invisible(x))
  }
  cat(sprintf("  entropy     : %.3f\n", x$entropy))
  cat("  posterior   :\n")
  top <- utils::head(x$posterior, 5L)
  for (nm in names(top)) {
    cat(sprintf("    %-32s %5.1f%%\n", nm, 100 * top[[nm]]))
  }
  if (length(x$posterior) > 5L) {
    cat(sprintf("    ... %d more class(es)\n", length(x$posterior) - 5L))
  }
  if (!is.null(x$sensitivity) && nrow(x$sensitivity) > 0L) {
    cat("  most decisive attribute: ",
        x$sensitivity$attribute[1L], "\n", sep = "")
  }
  invisible(x)
}
