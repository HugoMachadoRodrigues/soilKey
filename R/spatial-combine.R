# ================================================================
# Module 3 -- Combine multiple spatial priors
#
# When several backends are available (e.g. SoilGrids globally + a
# higher-resolution local raster), we combine them via weighted
# geometric mean -- the natural pooling rule for independent
# probability distributions in log-space (Genest & Zidek 1986).
#
# The result preserves the support union of all inputs; codes that
# appear in only some priors are smoothed with a small floor before
# pooling.
# ================================================================


#' Combine multiple spatial priors via weighted geometric mean
#'
#' Given a list of priors (each a data.table with \code{rsg_code,
#' probability}), pools them into a single distribution using a
#' weighted geometric mean and renormalises to sum to 1.
#'
#' Geometric pooling has two desirable properties for soil-class
#' priors:
#' \enumerate{
#'   \item externally Bayesian (the pooled posterior under any common
#'         likelihood matches what one would get by individual
#'         updates), and
#'   \item zero-preserving: a class assigned probability 0 by any
#'         prior is suppressed in the pooled distribution. To avoid
#'         that, classes absent from a given prior are imputed with
#'         the smoothing constant \code{epsilon}.
#' }
#'
#' @param priors A list of \code{data.table}s with columns
#'        \code{rsg_code} and \code{probability}.
#' @param weights Optional non-negative numeric vector of length
#'        \code{length(priors)}. Defaults to equal weights. Will be
#'        renormalised to sum to 1.
#' @param epsilon Smoothing floor for classes missing from a prior
#'        (default 1e-6). Must be > 0 -- otherwise any class missing
#'        from a single prior is suppressed entirely.
#' @return A \code{data.table} with columns \code{rsg_code},
#'         \code{probability}, sorted by descending probability.
#' @export
combine_priors <- function(priors, weights = NULL, epsilon = 1e-6) {
  if (!is.list(priors) || length(priors) == 0L) {
    rlang::abort("priors must be a non-empty list of prior data.tables")
  }

  # Normalise each input first, dropping empties.
  priors <- lapply(priors, normalize_prior)
  keep <- vapply(priors, function(p) nrow(p) > 0L, logical(1))
  priors <- priors[keep]
  if (length(priors) == 0L) {
    return(data.table::data.table(
      rsg_code    = character(),
      probability = numeric()
    ))
  }

  if (!is.null(weights)) {
    weights <- weights[keep]
    if (length(weights) != length(priors)) {
      rlang::abort("length(weights) must match length(priors)")
    }
    if (any(weights < 0)) {
      rlang::abort("weights must be non-negative")
    }
    if (sum(weights) <= 0) {
      rlang::abort("weights must have positive sum")
    }
    weights <- weights / sum(weights)
  } else {
    weights <- rep(1 / length(priors), length(priors))
  }

  if (epsilon <= 0) {
    rlang::abort("epsilon must be > 0 (zero-preservation otherwise wipes out classes)")
  }

  all_codes <- unique(unlist(lapply(priors, function(p) p$rsg_code)))
  all_codes <- all_codes[!is.na(all_codes)]

  # Stack into a matrix [codes x priors] of probabilities, with
  # epsilon for missing entries.
  M <- matrix(epsilon, nrow = length(all_codes), ncol = length(priors),
                dimnames = list(all_codes, NULL))
  for (j in seq_along(priors)) {
    p <- priors[[j]]
    M[p$rsg_code, j] <- pmax(p$probability, epsilon)
  }

  log_pool <- as.numeric(log(M) %*% weights)
  pooled   <- exp(log_pool - max(log_pool))   # numeric stability
  pooled   <- pooled / sum(pooled)

  out <- data.table::data.table(
    rsg_code    = all_codes,
    probability = pooled
  )
  out[order(-out$probability), ]
}
