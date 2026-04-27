# ================================================================
# Module 3 -- Spatial prior (v0.5)
#
# Top-level dispatcher. Returns a probability distribution over the
# Reference Soil Groups (or USDA Soil Orders) at the pedon's location.
#
# Design principles (see ARCHITECTURE.md sec. 8):
#   - The prior NEVER overrides the deterministic key.
#   - It is used for (a) ambiguity tie-breaking, (b) a sanity-check
#     warning when the assigned RSG has very low local probability,
#     and (c) optional Bayesian posterior via posterior_classify().
#   - Backends are pluggable: SoilGrids (global, ISRIC) and Embrapa
#     (Brazil 1:5M map, local raster).
#
# The function shape matches sec. 8.1 of the architecture spec; the
# returned data.table always has the canonical columns
# (rsg_code, probability) and probabilities sum to 1.
# ================================================================


#' Spatial prior over RSGs (or Orders) at a pedon's location
#'
#' Top-level dispatcher. Reads a categorical raster of soil classes
#' (SoilGrids globally, Embrapa for Brazil), buffers the pedon's
#' coordinates, tallies pixel classes within the buffer, and returns
#' the empirical class frequency as a probability distribution.
#'
#' The prior is intentionally separate from the deterministic key.
#' Pass the returned data.table to \code{\link{classify_wrb2022}} via
#' the \code{prior} argument; the result will then carry a
#' \code{prior_check} entry (consistent / inconsistent / not_run).
#'
#' @param pedon A \code{\link{PedonRecord}} with non-NULL
#'        \code{site$lat} / \code{site$lon}.
#' @param source Backend to query: \code{"soilgrids"} (default) or
#'        \code{"embrapa"}.
#' @param system Classification system: \code{"wrb2022"} (default) or
#'        \code{"usda"}. Embrapa source forces \code{"sibcs5"}
#'        internally regardless of this argument.
#' @param ... Passed through to the backend
#'        (\code{\link{spatial_prior_soilgrids}} or
#'        \code{\link{spatial_prior_embrapa}}).
#' @return A \code{data.table} with columns \code{rsg_code} (character)
#'         and \code{probability} (numeric, summing to 1). Empty if the
#'         buffer extracts no valid pixels -- callers should check
#'         \code{nrow()}.
#' @export
spatial_prior <- function(pedon,
                            source = c("soilgrids", "embrapa"),
                            system = c("wrb2022", "usda"),
                            ...) {
  source <- match.arg(source)
  system <- match.arg(system)

  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("pedon must be a PedonRecord")
  }
  if (is.null(pedon$site) ||
      is.null(pedon$site$lat) ||
      is.null(pedon$site$lon)) {
    rlang::abort(
      "pedon$site must carry numeric lat and lon for spatial_prior()"
    )
  }

  switch(source,
    soilgrids = spatial_prior_soilgrids(pedon, system = system, ...),
    embrapa   = spatial_prior_embrapa(pedon, ...)
  )
}


#' Validate / normalise a prior data.table
#'
#' Internal helper used by all backends. Coerces input to data.table
#' with canonical columns, drops NA codes, and renormalises so that
#' probabilities sum to 1.
#'
#' @keywords internal
normalize_prior <- function(prior) {
  if (!data.table::is.data.table(prior)) {
    prior <- data.table::as.data.table(prior)
  }
  if (!all(c("rsg_code", "probability") %in% names(prior))) {
    rlang::abort(
      "prior must have columns 'rsg_code' and 'probability'"
    )
  }
  prior <- prior[!is.na(prior$rsg_code) & !is.na(prior$probability), ]
  if (nrow(prior) == 0L) {
    return(data.table::data.table(
      rsg_code    = character(),
      probability = numeric()
    ))
  }
  total <- sum(prior$probability)
  if (total <= 0) {
    return(data.table::data.table(
      rsg_code    = character(),
      probability = numeric()
    ))
  }
  prior$probability <- prior$probability / total
  prior[order(-prior$probability), ]
}


#' Bayesian posterior classifier (optional)
#'
#' Combines a deterministic \code{\link{ClassificationResult}} with a
#' spatial prior. The deterministic key remains authoritative -- this
#' function reports only an alternative probabilistic view useful for
#' downstream uncertainty quantification.
#'
#' Posterior is computed under the simple model:
#' \deqn{P(rsg | site, evidence) \propto L(rsg | evidence) \times P(rsg | site)}
#' where the likelihood \code{L} is concentrated on the deterministic
#' assignment (delta-1 at that code) by default, optionally smoothed
#' if \code{key_passed_others} is supplied.
#'
#' @param result A \code{\link{ClassificationResult}} from
#'        \code{\link{classify_wrb2022}}.
#' @param prior A spatial-prior data.table (as returned by
#'        \code{\link{spatial_prior}}).
#' @param epsilon Small smoothing constant added to all prior entries
#'        before normalising, so RSGs unseen by the prior do not
#'        receive zero posterior.
#' @return A \code{data.table} with columns \code{rsg_code},
#'         \code{prior}, \code{likelihood}, \code{posterior}.
#' @export
posterior_classify <- function(result, prior, epsilon = 1e-3) {
  if (!inherits(result, "ClassificationResult")) {
    rlang::abort("result must be a ClassificationResult")
  }
  prior <- normalize_prior(prior)

  trace_codes <- vapply(result$trace, function(t) t$code %||% NA_character_,
                         character(1))
  rsg_codes <- unique(c(prior$rsg_code, trace_codes))
  rsg_codes <- rsg_codes[!is.na(rsg_codes)]

  prior_vec <- setNames(rep(epsilon, length(rsg_codes)), rsg_codes)
  prior_vec[prior$rsg_code] <- prior_vec[prior$rsg_code] + prior$probability
  prior_vec <- prior_vec / sum(prior_vec)

  # Likelihood: 1 on the assigned RSG (or those that "passed"), epsilon
  # elsewhere. The deterministic key already vetoed everything else, so
  # this is overwhelmingly concentrated on the assignment.
  passed_codes <- vapply(result$trace, function(t) {
    if (isTRUE(t$passed)) t$code else NA_character_
  }, character(1))
  passed_codes <- passed_codes[!is.na(passed_codes)]

  lik_vec <- setNames(rep(epsilon, length(rsg_codes)), rsg_codes)
  if (length(passed_codes) > 0L) {
    lik_vec[passed_codes] <- 1
  }

  post <- prior_vec * lik_vec
  post <- post / sum(post)

  data.table::data.table(
    rsg_code   = rsg_codes,
    prior      = unname(prior_vec),
    likelihood = unname(lik_vec),
    posterior  = unname(post)
  )[order(-post), ]
}
