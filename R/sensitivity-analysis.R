# =============================================================================
# v0.9.42 -- sensitivity analysis on classification outputs.
#
# For a given pedon, perturb the input attributes (clay/sand/silt by ±5%,
# pH by ±0.2, OC by ±0.5%, etc.) and measure how often the classification
# changes. Useful for:
#   * defending paper claims ("85 % of classifications robust to 5 %
#     analytical error")
#   * identifying brittle profiles (where the classification flips)
#   * UX feedback (Shiny app could colour-code result by stability)
# =============================================================================


#' Robustness of classification under input perturbation
#'
#' For a given \code{\link{PedonRecord}}, perturb a chosen list of
#' horizon attributes by a configured fractional amount, re-classify
#' under the requested system, and report how often the classification
#' \code{$rsg_or_order} (or full \code{$name}) matches the unperturbed
#' baseline.
#'
#' Default perturbation panel:
#' \itemize{
#'   \item \code{clay_pct}: ±5 % of value
#'   \item \code{sand_pct}: ±5 % of value
#'   \item \code{silt_pct}: ±5 % of value
#'   \item \code{ph_h2o}: ±0.2 absolute
#'   \item \code{oc_pct}: ±10 % of value
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param system One of \code{"wrb2022"}, \code{"sibcs"}, \code{"usda"}.
#' @param level Either \code{"order"} (compare \code{$rsg_or_order})
#'        or \code{"name"} (compare full classification name).
#' @param n Number of Monte-Carlo perturbed runs (default 50).
#' @param perturbations Named list. Each name is a horizon column;
#'        each element is a function taking the original value and
#'        returning a perturbed value. NA-tolerant.
#' @param seed Random seed for reproducibility.
#' @return A list with elements \code{baseline} (the unperturbed
#'         classification name), \code{n} (number of MC runs),
#'         \code{robustness} (fraction of perturbed runs matching
#'         baseline), \code{flipped_to} (table of alternative
#'         classifications when the perturbation flipped the result).
#' @examples
#' \dontrun{
#' p <- make_ferralsol_canonical()
#' classification_robustness(p, system = "wrb2022", n = 50)
#' #> $baseline    : "Ferralsols"
#' #> $robustness  : 0.96  (48 / 50 perturbed runs landed on Ferralsols)
#' #> $flipped_to  : table(c("Cambisols" = 1, "Acrisols" = 1))
#' }
#' @export
classification_robustness <- function(pedon,
                                         system = c("wrb2022", "sibcs", "usda"),
                                         level  = c("order", "name"),
                                         n      = 50L,
                                         perturbations = NULL,
                                         seed   = 42L) {
  system <- match.arg(system)
  level  <- match.arg(level)

  classify_fn <- switch(system,
                          wrb2022 = function(p) classify_wrb2022(p, on_missing = "silent"),
                          sibcs   = function(p) classify_sibcs(p,   on_missing = "silent"),
                          usda    = function(p) classify_usda(p,    on_missing = "silent"))

  # Default Monte-Carlo perturbation panel.
  if (is.null(perturbations)) {
    perturbations <- list(
      clay_pct = function(x) x * (1 + stats::runif(length(x), -0.05, 0.05)),
      sand_pct = function(x) x * (1 + stats::runif(length(x), -0.05, 0.05)),
      silt_pct = function(x) x * (1 + stats::runif(length(x), -0.05, 0.05)),
      ph_h2o   = function(x) x + stats::runif(length(x), -0.2, 0.2),
      oc_pct   = function(x) x * (1 + stats::runif(length(x), -0.10, 0.10))
    )
  }

  # Baseline classification.
  baseline_cls <- classify_fn(pedon)
  baseline_value <- if (level == "order") baseline_cls$rsg_or_order
                    else                   baseline_cls$name
  if (is.null(baseline_value)) baseline_value <- NA_character_

  # Monte-Carlo perturbed runs.
  set.seed(seed)
  results <- character(n)
  for (i in seq_len(n)) {
    p_perturbed <- .perturb_pedon(pedon, perturbations)
    cls <- tryCatch(classify_fn(p_perturbed), error = function(e) NULL)
    results[i] <- if (is.null(cls)) NA_character_
                  else if (level == "order") cls$rsg_or_order %||% NA_character_
                  else                       cls$name %||% NA_character_
  }

  # Tally.
  match_baseline <- !is.na(results) & results == baseline_value
  robustness <- mean(match_baseline)
  flipped <- results[!match_baseline & !is.na(results)]
  flipped_table <- table(flipped)

  list(
    baseline   = baseline_value,
    n          = n,
    robustness = robustness,
    flipped_to = flipped_table,
    results    = results
  )
}


# Internal helper: perturb a pedon by applying each named perturbation
# function to the corresponding horizon column.
.perturb_pedon <- function(pedon, perturbations) {
  h <- data.table::copy(pedon$horizons)
  for (col in names(perturbations)) {
    if (col %in% names(h)) {
      f <- perturbations[[col]]
      vals <- h[[col]]
      ok <- !is.na(vals)
      if (any(ok)) {
        new_vals <- f(vals[ok])
        h[[col]][ok] <- pmax(new_vals, 0)  # clip to non-negative
      }
    }
  }
  PedonRecord$new(site = pedon$site, horizons = h)
}


#' Batch robustness across many pedons
#'
#' Runs \code{\link{classification_robustness}} on each pedon in a
#' list and returns a tidy data.frame with one row per pedon. Useful
#' for paper-grade claims like "85 % of classifications are robust
#' to a 5 % analytical-error perturbation".
#'
#' @param pedons List of \code{\link{PedonRecord}} objects.
#' @param ... Passed to \code{\link{classification_robustness}}.
#' @return A data.frame with columns \code{id}, \code{baseline},
#'         \code{robustness}, \code{n_flipped}.
#' @examples
#' \dontrun{
#' pedons <- list(make_ferralsol_canonical(),
#'                  make_luvisol_canonical(),
#'                  make_chernozem_canonical())
#' batch_robustness(pedons, system = "wrb2022", n = 50)
#' #>            id   baseline robustness n_flipped
#' #> 1 FR-canon-01 Ferralsols       0.96         2
#' #> 2 LV-canon-01   Luvisols       1.00         0
#' #> 3 CH-canon-01 Chernozems       0.94         3
#' }
#' @export
batch_robustness <- function(pedons, ...) {
  out <- vector("list", length(pedons))
  for (i in seq_along(pedons)) {
    p <- pedons[[i]]
    res <- classification_robustness(p, ...)
    out[[i]] <- data.frame(
      id         = p$site$id %||% paste0("pedon-", i),
      baseline   = res$baseline,
      robustness = res$robustness,
      n_flipped  = sum(!is.na(res$results) &
                          res$results != res$baseline),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, out)
}
