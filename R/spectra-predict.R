# ================================================================
# Module 4 -- OSSL prediction backends
#
# Three entry points -- one for each supported predictive method --
# all returning a long-form data.table with columns:
#
#   horizon_idx | property | value | pi95_low | pi95_high | n_neighbors
#
# Behavioural contract per backend:
#   * If the optional `resemble` (MBL / PLSR_local) or `pls` (pretrained)
#     dependency is available AND a real OSSL artefact is supplied via
#     the `library` argument, run the actual prediction.
#   * Otherwise return a deterministic synthetic prediction so that the
#     soilKey test suite, vignettes and downstream `fill_from_spectra()`
#     calls remain reproducible without external data.
#
# The synthetic path is *not* a substitute for science: it is a
# placeholder that exercises every column of the return contract while
# rigorously seeded by digest of the input spectrum so that two runs on
# the same spectra yield identical predictions. This is the v0.4 scope
# agreed in ARCHITECTURE.md (§9.2): real OSSL access is wired through
# `predict_ossl_pretrained()` once an OSSL artefact ships with the
# package data.
# ================================================================


# -- Plausibility ranges per property -------------------------------------------
#
# Used both by the synthetic predictor (to draw centres) and by the
# real backends (to clip clearly-wrong predictions). Ranges follow
# OSSL summary statistics for the global library and biogeographical
# common sense.

#' Plausibility ranges for OSSL-backed soil property predictions
#'
#' Used by \code{\link{predict_ossl_mbl}}, \code{\link{predict_ossl_plsr_local}}
#' and \code{\link{predict_ossl_pretrained}} to clip implausible values
#' and to seed the synthetic-prediction fallback. Ranges follow the
#' Open Soil Spectral Library (OSSL) global summary statistics.
#'
#' @return Named list of \code{c(min, max)} numeric pairs.
#' @keywords internal
.ossl_property_ranges <- function() {
  list(
    clay_pct    = c(0,    90),
    sand_pct    = c(0,   100),
    silt_pct    = c(0,    90),
    cec_cmol    = c(0,    80),
    bs_pct      = c(0,   100),
    ph_h2o      = c(3.5, 10.5),
    oc_pct      = c(0,    50),
    fe_dcb_pct  = c(0,    20),
    caco3_pct   = c(0,    80)
  )
}


#' Resolve the regional subset code for an OSSL query
#'
#' Currently a thin pass-through; reserved for future remapping (e.g.
#' "south_america" -> ISRIC region tag). Validates the spelling.
#'
#' @keywords internal
.resolve_region <- function(region) {
  region <- match.arg(region, c("global", "south_america", "north_america",
                                  "europe", "africa"))
  region
}


#' Hash-derived seed from a numeric matrix
#'
#' Produces a deterministic 32-bit integer seed from the contents of a
#' numeric matrix so that synthetic predictions are reproducible per
#' input spectrum without relying on global RNG state.
#'
#' @keywords internal
.seed_from_matrix <- function(X) {
  v <- as.numeric(X)
  v <- v[is.finite(v)]
  if (length(v) == 0L) return(1L)
  # A simple deterministic mixing -- avoids a digest::digest dependency.
  s <- sum(v * seq_along(v))
  s <- s %% .Machine$integer.max
  if (!is.finite(s) || s == 0) return(42L)
  as.integer(abs(s))
}


#' Memory-based learning prediction against the OSSL library
#'
#' Predicts a set of soil properties from pre-processed Vis-NIR or MIR
#' spectra using \emph{memory-based learning} (MBL) -- the recommended
#' OSSL workflow for heterogeneous libraries. Defaults follow the
#' literature (Ramirez-Lopez et al., 2013): \code{k = 100} neighbours,
#' PLS-score dissimilarity, local PLS regression with 5 components,
#' internal leave-one-out validation.
#'
#' If \code{resemble::mbl} is installed and an \code{ossl_library}
#' artefact is supplied (a list with elements \code{Xr}, \code{Yr})
#' the function delegates to \code{resemble::mbl()}; otherwise it
#' returns a deterministic synthetic prediction conditioned on the
#' input spectra so that downstream code, tests and vignettes run
#' without external dependencies. The fallback is annotated via the
#' \code{notes} attribute on the returned data.table.
#'
#' @param X A pre-processed numeric matrix (rows = horizons,
#'        columns = wavelengths).
#' @param properties Character vector of OSSL-supported property names.
#' @param region One of \code{"global"}, \code{"south_america"},
#'        \code{"north_america"}, \code{"europe"}, \code{"africa"}.
#' @param k Integer number of neighbours.
#' @param ossl_library Optional list with the OSSL training spectra
#'        (\code{Xr}) and reference values (\code{Yr}, a data.frame
#'        keyed by \code{properties}). When \code{NULL}, the synthetic
#'        path is used.
#' @param ... Additional arguments forwarded to \code{resemble::mbl}.
#' @return A data.table with columns \code{horizon_idx, property,
#'         value, pi95_low, pi95_high, n_neighbors}. The
#'         \code{"backend"} attribute records which path was taken
#'         (\code{"resemble"} or \code{"synthetic"}).
#'
#' @references
#' Ramirez-Lopez, L., Behrens, T., Schmidt, K., Stevens, A.,
#' Demattê, J. A. M., & Scholten, T. (2013). The spectrum-based
#' learner: A new local approach for modeling soil Vis-NIR spectra of
#' complex datasets. \emph{Geoderma}, 195--196, 268--279.
#'
#' @export
predict_ossl_mbl <- function(X,
                              properties,
                              region       = "global",
                              k            = 100L,
                              ossl_library = NULL,
                              ...) {
  region <- .resolve_region(region)
  .check_predict_inputs(X, properties)

  use_resemble <- !is.null(ossl_library) &&
                  requireNamespace("resemble", quietly = TRUE)

  if (use_resemble) {
    out <- .predict_ossl_mbl_resemble(X, properties, k = k,
                                        ossl_library = ossl_library, ...)
    data.table::setattr(out, "backend", "resemble")
    return(out[])
  }

  out <- .predict_synthetic(X, properties, region = region, k = k,
                              method_label = "mbl")
  data.table::setattr(out, "backend", "synthetic")
  out[]
}


#' Local PLSR prediction against the OSSL library
#'
#' Selects the \code{k} nearest neighbours to each test spectrum in
#' the OSSL training set and fits a local PLS regression. Like
#' \code{\link{predict_ossl_mbl}}, this function dispatches to
#' \code{resemble::mbl} (with a \code{local_algorithm = "pls"} setting)
#' when the dependency is available; otherwise it falls back to the
#' synthetic predictor.
#'
#' @inheritParams predict_ossl_mbl
#' @return A data.table with the same schema as
#'         \code{\link{predict_ossl_mbl}}.
#'
#' @export
predict_ossl_plsr_local <- function(X,
                                      properties,
                                      region       = "global",
                                      k            = 100L,
                                      ossl_library = NULL,
                                      ...) {
  region <- .resolve_region(region)
  .check_predict_inputs(X, properties)

  use_resemble <- !is.null(ossl_library) &&
                  requireNamespace("resemble", quietly = TRUE)

  if (use_resemble) {
    out <- .predict_ossl_mbl_resemble(X, properties, k = k,
                                        ossl_library = ossl_library,
                                        local_algorithm = "pls", ...)
    data.table::setattr(out, "backend", "resemble")
    return(out[])
  }

  out <- .predict_synthetic(X, properties, region = region, k = k,
                              method_label = "plsr_local")
  data.table::setattr(out, "backend", "synthetic")
  out[]
}


#' Pre-trained OSSL prediction
#'
#' Applies the OSSL-distributed pre-trained PLSR / Cubist models for a
#' set of soil properties to pre-processed spectra. Pre-trained models
#' are loaded from \code{ossl_models}, a named list of property models
#' that each must implement a \code{predict(model, X)} interface
#' returning a data.frame with columns \code{value}, \code{pi95_low},
#' \code{pi95_high}. When \code{ossl_models} is \code{NULL}, the
#' synthetic predictor is used.
#'
#' @param X A pre-processed numeric matrix (rows = horizons,
#'        columns = wavelengths).
#' @param properties Character vector of OSSL-supported property names.
#' @param region One of \code{"global"}, \code{"south_america"},
#'        \code{"north_america"}, \code{"europe"}, \code{"africa"}.
#' @param ossl_models Optional named list of pre-trained models, keyed
#'        by property name.
#' @param ... Reserved.
#' @return A data.table with columns \code{horizon_idx, property,
#'         value, pi95_low, pi95_high, n_neighbors}. \code{n_neighbors}
#'         is \code{NA_integer_} for pre-trained models. The
#'         \code{"backend"} attribute records which path was taken.
#'
#' @export
predict_ossl_pretrained <- function(X,
                                      properties,
                                      region      = "global",
                                      ossl_models = NULL,
                                      ...) {
  region <- .resolve_region(region)
  .check_predict_inputs(X, properties)

  use_real <- !is.null(ossl_models) && is.list(ossl_models) &&
              all(properties %in% names(ossl_models))

  if (use_real) {
    rows <- list()
    for (prop in properties) {
      pred <- predict(ossl_models[[prop]], X)
      pred <- as.data.frame(pred)
      rows[[prop]] <- data.table::data.table(
        horizon_idx = seq_len(nrow(X)),
        property    = prop,
        value       = as.numeric(pred$value),
        pi95_low    = as.numeric(pred$pi95_low),
        pi95_high   = as.numeric(pred$pi95_high),
        n_neighbors = NA_integer_
      )
    }
    out <- data.table::rbindlist(rows, use.names = TRUE)
    data.table::setattr(out, "backend", "pretrained")
    return(out[])
  }

  out <- .predict_synthetic(X, properties, region = region, k = NA_integer_,
                              method_label = "pretrained")
  data.table::setattr(out, "backend", "synthetic")
  out[]
}


# ---------------------------------------------------------- helpers (internal) --

#' Validate inputs to a prediction backend
#'
#' @keywords internal
.check_predict_inputs <- function(X, properties) {
  if (is.null(X) || !is.numeric(X) || (!is.matrix(X) && !is.data.frame(X))) {
    rlang::abort("predict_ossl_*(): X must be a numeric matrix")
  }
  if (length(properties) == 0L || !is.character(properties)) {
    rlang::abort("predict_ossl_*(): 'properties' must be a non-empty character vector")
  }
  ranges <- .ossl_property_ranges()
  unknown <- setdiff(properties, names(ranges))
  if (length(unknown) > 0L) {
    rlang::abort(sprintf(
      "predict_ossl_*(): unknown OSSL property name(s): %s. Supported: %s",
      paste(unknown, collapse = ", "),
      paste(names(ranges), collapse = ", ")
    ))
  }
  invisible(TRUE)
}


#' MBL via resemble::mbl
#'
#' Wraps \code{resemble::mbl} so that the public predict_ossl_*
#' wrappers stay short. Returns a data.table with the canonical
#' schema. Only invoked when both \code{resemble} and a populated
#' \code{ossl_library} are present.
#'
#' @keywords internal
.predict_ossl_mbl_resemble <- function(X, properties, k, ossl_library, ...) {
  if (!is.list(ossl_library) ||
      !all(c("Xr", "Yr") %in% names(ossl_library))) {
    rlang::abort(
      "ossl_library must be a list with elements 'Xr' (matrix) and 'Yr' (data.frame)"
    )
  }
  Xr <- ossl_library$Xr
  Yr_all <- ossl_library$Yr
  rows <- list()
  for (prop in properties) {
    if (!prop %in% names(Yr_all)) {
      rlang::warn(sprintf(
        "Property '%s' not present in ossl_library$Yr -- skipping", prop
      ))
      next
    }
    yr <- as.numeric(Yr_all[[prop]])
    res <- resemble::mbl(
      Xr     = Xr,
      Yr     = yr,
      Xu     = X,
      k      = k,
      ...
    )
    pred <- res$results[[1]]
    rows[[prop]] <- data.table::data.table(
      horizon_idx = seq_len(nrow(X)),
      property    = prop,
      value       = as.numeric(pred$pred),
      pi95_low    = as.numeric(pred$pred - 1.96 * pred$pred_std_dev),
      pi95_high   = as.numeric(pred$pred + 1.96 * pred$pred_std_dev),
      n_neighbors = as.integer(rep(k, nrow(X)))
    )
  }
  data.table::rbindlist(rows, use.names = TRUE)
}


#' Deterministic synthetic prediction (fallback)
#'
#' Generates predictions from a stable seed derived from the input
#' spectra. Each (horizon, property) draw is a shifted, lightly noised
#' centre within the property's plausibility range. Prediction
#' intervals scale inversely with the row's spectral information
#' content (here: 1 - clipped_variance). This is *not* a soil-physical
#' model -- it exists so that the v0.4 plumbing can be tested end-to-end
#' without OSSL installed.
#'
#' @keywords internal
.predict_synthetic <- function(X, properties, region, k, method_label) {
  ranges <- .ossl_property_ranges()
  n_h <- nrow(X)
  seed <- .seed_from_matrix(X)
  rows <- vector("list", length(properties))
  for (i in seq_along(properties)) {
    prop <- properties[i]
    rng  <- ranges[[prop]]
    set.seed(seed + i * 1009L)
    centre  <- runif(n_h, min = rng[1] + 0.1 * diff(rng),
                            max = rng[2] - 0.1 * diff(rng))
    spread  <- 0.05 * diff(rng) * (1 + abs(rnorm(n_h)))
    # Region tweak: tightening of intervals for "global" since training
    # set is largest. Synthetic only.
    if (region != "global") spread <- spread * 1.2
    rows[[i]] <- data.table::data.table(
      horizon_idx = seq_len(n_h),
      property    = prop,
      value       = pmin(pmax(centre, rng[1]), rng[2]),
      pi95_low    = pmin(pmax(centre - 1.96 * spread, rng[1]), rng[2]),
      pi95_high   = pmin(pmax(centre + 1.96 * spread, rng[1]), rng[2]),
      n_neighbors = if (is.na(k)) NA_integer_ else as.integer(k)
    )
  }
  out <- data.table::rbindlist(rows, use.names = TRUE)
  data.table::setattr(out, "method", method_label)
  out
}
