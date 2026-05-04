# =============================================================================
# v0.9.46 -- training of OSSL-backed PLS models + named convenience API.
#
# Until v0.9.45 the package shipped:
#   - download_ossl_subset()                  : fetch real OSSL data
#   - predict_ossl_mbl / plsr_local           : online resemble::mbl
#   - predict_ossl_pretrained(ossl_models)    : runtime-supplied models
#   - fill_from_spectra(method = "pretrained"): wrapper
#
# What was missing was the loop that produces those `ossl_models` from a
# downloaded `ossl_library`. Without it, users who want the pretrained
# path had to write their own pls::plsr training pipeline. v0.9.46
# closes the gap with three exported functions:
#
#   train_pls_from_ossl()    -- per-property PLSR training, optimal
#                                ncomp via cross-validation, returns a
#                                list of `soilKey_pls_model` objects
#                                directly usable by predict_ossl_pretrained().
#
#   predict_from_spectra()   -- ergonomic alias around fill_from_spectra
#                                (PedonRecord input) or
#                                predict_ossl_pretrained() (raw matrix
#                                input), pre-processing the spectra in
#                                both cases.
#
#   save_ossl_models() /     -- on-disk persistence with metadata
#   load_ossl_models()          (soilKey version, training time,
#                                preprocess label, ncomp_opt, RMSE).
# =============================================================================


#' Train pre-trained PLSR models from an OSSL library
#'
#' Iterates over \code{properties} and fits one PLSR model per target
#' against the OSSL spectra in \code{ossl_library$Xr}, with internal
#' cross-validation to pick the optimal number of components per
#' property. The returned list is a drop-in replacement for the
#' \code{ossl_models} argument of \code{\link{predict_ossl_pretrained}}
#' and \code{\link{fill_from_spectra}}.
#'
#' Spectra are pre-processed inside the function (default
#' \code{"snv+sg1"}); the same preprocessing is used downstream by
#' \code{\link{predict_from_spectra}} so the user does not have to
#' remember which transform was applied at training time.
#'
#' @param ossl_library A list with two named elements: \code{Xr}
#'        (numeric matrix of training spectra) and \code{Yr}
#'        (data.frame keyed by property name, one row per training
#'        spectrum). See \code{\link{ossl_library_template}}.
#' @param properties Character vector of column names in
#'        \code{ossl_library$Yr} to train models for. Defaults to the
#'        six core soil properties exposed by OSSL.
#' @param ncomp_max Integer. Upper bound on the number of PLS
#'        components to consider during cross-validation. Defaults
#'        to 20.
#' @param validation One of \code{"CV"} (default, k-fold),
#'        \code{"LOO"} (leave-one-out, slow), \code{"none"} (uses
#'        \code{ncomp_max} components without selection).
#' @param segments Number of CV segments when
#'        \code{validation = "CV"}. Default 10.
#' @param preprocess Pre-processing label passed to
#'        \code{\link{preprocess_spectra}}. Stored on the trained
#'        models so \code{\link{predict_from_spectra}} can reapply it.
#' @param min_n Minimum number of valid training samples (after
#'        dropping rows with non-finite y or X). Properties below this
#'        threshold are skipped with a warning. Default 50.
#' @param verbose If \code{TRUE} (default), prints a per-property
#'        summary on completion.
#' @return A named list of \code{soilKey_pls_model} objects, one per
#'         successfully trained property. Carries
#'         \code{trained_at}, \code{soilKey_version} and
#'         \code{preprocess} attributes for provenance.
#'
#' @examples
#' \dontrun{
#' lib <- download_ossl_subset(region = "south_america")
#' models <- train_pls_from_ossl(lib,
#'                                properties = c("clay_pct", "ph_h2o"))
#' result <- predict_from_spectra(my_pedon, models = models)
#' }
#' @export
train_pls_from_ossl <- function(ossl_library,
                                  properties = c("clay_pct", "sand_pct",
                                                  "silt_pct", "cec_cmol",
                                                  "ph_h2o", "oc_pct"),
                                  ncomp_max  = 20L,
                                  validation = c("CV", "LOO", "none"),
                                  segments   = 10L,
                                  preprocess = "snv+sg1",
                                  min_n      = 50L,
                                  verbose    = TRUE) {
  if (!requireNamespace("pls", quietly = TRUE)) {
    stop("Package 'pls' is required for train_pls_from_ossl(). ",
         "Install with `install.packages(\"pls\")`.")
  }
  validation <- match.arg(validation)
  if (!is.list(ossl_library) ||
        !all(c("Xr", "Yr") %in% names(ossl_library))) {
    stop("train_pls_from_ossl(): 'ossl_library' must be a list with ",
         "elements 'Xr' (matrix) and 'Yr' (data.frame). See ",
         "ossl_library_template().")
  }
  Xr <- ossl_library$Xr
  Yr <- ossl_library$Yr
  if (!is.matrix(Xr)) Xr <- as.matrix(Xr)
  if (!is.data.frame(Yr)) Yr <- as.data.frame(Yr)
  if (nrow(Xr) != nrow(Yr)) {
    stop(sprintf(
      "train_pls_from_ossl(): nrow(Xr) (%d) != nrow(Yr) (%d)",
      nrow(Xr), nrow(Yr)
    ))
  }
  if (nrow(Xr) < min_n) {
    stop(sprintf(
      "train_pls_from_ossl(): only %d training rows -- below min_n = %d",
      nrow(Xr), min_n
    ))
  }

  Xp <- preprocess_spectra(Xr, method = preprocess)

  ncomp_max <- min(as.integer(ncomp_max), nrow(Xp) - 2L, ncol(Xp) - 1L)
  if (ncomp_max < 1L) {
    stop("train_pls_from_ossl(): not enough samples / wavelengths to fit PLS.")
  }

  models <- list()
  diagnostics <- list()
  for (prop in properties) {
    if (!prop %in% names(Yr)) {
      warning(sprintf("Property '%s' not in ossl_library$Yr -- skipped.", prop))
      next
    }
    y <- as.numeric(Yr[[prop]])
    keep <- is.finite(y) & stats::complete.cases(Xp)
    n_kept <- sum(keep)
    if (n_kept < min_n) {
      warning(sprintf(
        "Property '%s': only %d valid samples (< %d) -- skipped.",
        prop, n_kept, min_n))
      next
    }
    df <- data.frame(y = y[keep])
    df$X <- I(Xp[keep, , drop = FALSE])

    val_arg <- switch(validation, CV = "CV", LOO = "LOO", none = "none")
    seg_arg <- if (val_arg == "CV") min(segments, n_kept - 1L) else NULL

    fit <- if (val_arg == "none") {
      pls::plsr(y ~ X, data = df, ncomp = ncomp_max,
                  validation = "none", scale = FALSE)
    } else if (val_arg == "LOO") {
      pls::plsr(y ~ X, data = df, ncomp = ncomp_max,
                  validation = "LOO", scale = FALSE)
    } else {
      pls::plsr(y ~ X, data = df, ncomp = ncomp_max,
                  validation = "CV", segments = seg_arg, scale = FALSE)
    }

    if (val_arg %in% c("CV", "LOO")) {
      rmsep <- pls::RMSEP(fit, estimate = "CV")$val
      rmsep_vec <- as.numeric(rmsep[1L, 1L, ])
      ncomp_opt <- which.min(rmsep_vec) - 1L  # offset: 0 = null model
      if (ncomp_opt < 1L) ncomp_opt <- 1L
      rmse_train <- as.numeric(rmsep_vec[ncomp_opt + 1L])
    } else {
      ncomp_opt <- ncomp_max
      rmse_train <- NA_real_
    }

    models[[prop]] <- structure(
      list(
        fit         = fit,
        ncomp_opt   = as.integer(ncomp_opt),
        rmse_train  = rmse_train,
        property    = prop,
        n_train     = as.integer(n_kept),
        preprocess  = preprocess,
        wavelengths = colnames(Xr),
        ncol_in     = as.integer(ncol(Xp))
      ),
      class = "soilKey_pls_model"
    )
    diagnostics[[prop]] <- c(n_train = n_kept,
                              ncomp_opt = ncomp_opt,
                              rmse_train = rmse_train)
  }

  attr(models, "trained_at")      <- Sys.time()
  attr(models, "soilKey_version") <- as.character(utils::packageVersion("soilKey"))
  attr(models, "preprocess")      <- preprocess
  attr(models, "diagnostics")     <- diagnostics

  if (verbose && length(models) > 0L) {
    cli::cli_h2(sprintf("train_pls_from_ossl(): %d / %d properties trained",
                          length(models), length(properties)))
    for (prop in names(models)) {
      m <- models[[prop]]
      cli::cli_alert_info(sprintf(
        "{.field %s}: n=%d, ncomp_opt=%d, RMSE_CV=%.3g",
        prop, m$n_train, m$ncomp_opt, m$rmse_train
      ))
    }
  }

  models
}


#' Predict from a soilKey_pls_model
#'
#' S3 method that applies a trained PLSR model from
#' \code{\link{train_pls_from_ossl}} to a (pre-processed) numeric
#' matrix and returns predictions plus a 95% prediction interval
#' built from the cross-validated training RMSE.
#'
#' @param object A \code{soilKey_pls_model} object.
#' @param X A pre-processed numeric matrix (rows = samples,
#'        columns = wavelengths). Must have the same column count
#'        used at training time.
#' @param ... Reserved.
#' @return A data.frame with columns \code{value}, \code{pi95_low},
#'         \code{pi95_high}, one row per sample.
#' @method predict soilKey_pls_model
#' @export
predict.soilKey_pls_model <- function(object, X, ...) {
  if (!requireNamespace("pls", quietly = TRUE)) {
    stop("Package 'pls' is required for predict.soilKey_pls_model().")
  }
  if (!is.matrix(X)) X <- as.matrix(X)
  if (ncol(X) != object$ncol_in) {
    stop(sprintf(
      "predict.soilKey_pls_model(): newdata has %d columns; model expects %d.",
      ncol(X), object$ncol_in
    ))
  }
  newdata <- list(X = X)
  yhat <- as.numeric(stats::predict(object$fit,
                                      newdata = newdata,
                                      ncomp = object$ncomp_opt))
  rmse <- object$rmse_train
  if (is.null(rmse) || !is.finite(rmse)) rmse <- NA_real_
  data.frame(
    value     = yhat,
    pi95_low  = yhat - 1.96 * rmse,
    pi95_high = yhat + 1.96 * rmse,
    stringsAsFactors = FALSE
  )
}


#' Print method for soilKey_pls_model
#'
#' @param x A \code{soilKey_pls_model} object.
#' @param ... Reserved.
#' @return The object, invisibly.
#' @method print soilKey_pls_model
#' @export
print.soilKey_pls_model <- function(x, ...) {
  cat(sprintf(
    "<soilKey_pls_model: %s>\n  n_train=%d  ncomp_opt=%d  RMSE_CV=%.3g  preprocess=%s\n",
    x$property, x$n_train, x$ncomp_opt, x$rmse_train, x$preprocess
  ))
  invisible(x)
}


#' Predict soil properties from spectra
#'
#' Ergonomic, named entry point for the OSSL-backed predictive
#' pipeline. Accepts either a \code{\link{PedonRecord}} or a numeric
#' spectra matrix, applies the same preprocessing used at training
#' time (recorded on each model), and returns predictions in the
#' canonical long-form schema.
#'
#' When \code{pedon_or_spectra} is a \code{PedonRecord}, this
#' function delegates to \code{\link{fill_from_spectra}} with
#' \code{method = "pretrained"} and the predictions are written back
#' to the pedon (with \code{source = "predicted_spectra"} provenance).
#' When \code{pedon_or_spectra} is a numeric matrix or vector, this
#' function returns the prediction data.table directly without
#' touching any pedon.
#'
#' @param pedon_or_spectra A \code{\link{PedonRecord}} (predictions
#'        merged into the pedon) OR a numeric matrix / vector of raw
#'        Vis-NIR spectra (rows = horizons, columns = wavelengths).
#' @param models A named list of \code{soilKey_pls_model} objects
#'        (output of \code{\link{train_pls_from_ossl}}). Required.
#' @param properties Character vector of property names to predict.
#'        Defaults to all properties in \code{models}.
#' @param overwrite Passed to \code{\link{fill_from_spectra}} when
#'        \code{pedon_or_spectra} is a PedonRecord.
#' @param verbose Verbosity passed downstream.
#' @param ... Ignored (reserved for future backends).
#' @return Either the mutated \code{PedonRecord} (invisibly) or a
#'         data.table with columns \code{horizon_idx}, \code{property},
#'         \code{value}, \code{pi95_low}, \code{pi95_high},
#'         \code{n_neighbors}.
#'
#' @examples
#' \dontrun{
#' lib <- download_ossl_subset(region = "south_america")
#' models <- train_pls_from_ossl(lib,
#'                                 properties = c("clay_pct", "ph_h2o"))
#' predict_from_spectra(my_pedon, models = models)
#' }
#' @export
predict_from_spectra <- function(pedon_or_spectra,
                                   models     = NULL,
                                   properties = NULL,
                                   overwrite  = FALSE,
                                   verbose    = TRUE,
                                   ...) {
  if (is.null(models) || length(models) == 0L) {
    stop("predict_from_spectra(): 'models' is required. ",
         "Use train_pls_from_ossl() first.")
  }
  if (!all(vapply(models, inherits, logical(1L), "soilKey_pls_model"))) {
    stop("predict_from_spectra(): every element of 'models' must inherit ",
         "from 'soilKey_pls_model' (see train_pls_from_ossl()).")
  }
  if (is.null(properties)) properties <- names(models)
  unknown <- setdiff(properties, names(models))
  if (length(unknown) > 0L) {
    stop(sprintf(
      "predict_from_spectra(): no models for properties: %s",
      paste(unknown, collapse = ", ")
    ))
  }
  preprocess <- attr(models, "preprocess") %||%
                  models[[1L]]$preprocess %||% "snv+sg1"

  if (inherits(pedon_or_spectra, "PedonRecord")) {
    fill_from_spectra(
      pedon_or_spectra,
      library     = "ossl",
      method      = "pretrained",
      properties  = properties,
      preprocess  = preprocess,
      ossl_models = models,
      overwrite   = overwrite,
      verbose     = verbose
    )
  } else if (is.matrix(pedon_or_spectra) ||
                is.data.frame(pedon_or_spectra) ||
                is.numeric(pedon_or_spectra)) {
    X_raw <- pedon_or_spectra
    if (is.data.frame(X_raw)) X_raw <- as.matrix(X_raw)
    if (is.numeric(X_raw) && is.null(dim(X_raw))) {
      X_raw <- matrix(X_raw, nrow = 1L)
    }
    Xp <- preprocess_spectra(X_raw, method = preprocess)
    predict_ossl_pretrained(
      Xp,
      properties  = properties,
      ossl_models = models
    )
  } else {
    stop("predict_from_spectra(): expected a PedonRecord, numeric matrix, ",
         "data.frame, or numeric vector.")
  }
}


#' Save / load trained OSSL-backed PLSR models
#'
#' Thin wrappers around \code{saveRDS} / \code{readRDS} that also
#' verify the deserialised object's shape. The on-disk file carries
#' the soilKey version, training time and preprocess label as
#' attributes; \code{\link{load_ossl_models}} preserves them.
#'
#' @param models Output of \code{\link{train_pls_from_ossl}}.
#' @param path File path. Use \code{.rds} or \code{.RData} as the
#'        suffix (saveRDS is used regardless).
#' @return \code{save_ossl_models()} returns \code{path} invisibly.
#'         \code{load_ossl_models()} returns the model list.
#' @name save_ossl_models
#' @export
save_ossl_models <- function(models, path) {
  if (!is.list(models) || length(models) == 0L) {
    stop("save_ossl_models(): 'models' must be a non-empty list.")
  }
  if (!all(vapply(models, inherits, logical(1L), "soilKey_pls_model"))) {
    stop("save_ossl_models(): every element must inherit from ",
         "'soilKey_pls_model'.")
  }
  saveRDS(models, file = path)
  invisible(path)
}

#' @rdname save_ossl_models
#' @export
load_ossl_models <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("load_ossl_models(): file not found: %s", path))
  }
  obj <- readRDS(path)
  if (!is.list(obj) ||
        !all(vapply(obj, inherits, logical(1L), "soilKey_pls_model"))) {
    stop(sprintf(
      "load_ossl_models(): file '%s' does not contain a list of soilKey_pls_model objects.",
      path
    ))
  }
  obj
}
