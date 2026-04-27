# ================================================================
# Module 4 -- OSSL spectra -> PedonRecord gap-filler
#
# Public entry point: fill_from_spectra().
#
# Pipeline:
#   1. Pull pedon$spectra$vnir (rows = horizons, cols = wavelengths).
#   2. Pre-process via preprocess_spectra().
#   3. Dispatch to one of three predictive backends.
#   4. For each (horizon_idx, property) prediction call
#      pedon$add_measurement(..., source = "predicted_spectra"),
#      letting the PedonRecord's authority logic decide whether the
#      predicted value should overwrite an existing cell.
# ================================================================


#' Map a 95\% prediction interval to a [0, 1] confidence score
#'
#' Tightens confidence as the prediction interval narrows relative to
#' the predicted value: \code{confidence = 1 - (PI95_width / |value|) / 4},
#' floored at 0 and capped at 1. When \code{value} is near zero we
#' fall back to an absolute-width heuristic so we never blow up.
#'
#' Properties of the mapping:
#' \itemize{
#'   \item Zero-width interval -> confidence = 1.
#'   \item Interval whose width equals \code{|value| * 4} -> confidence = 0.
#'   \item NA value or NA bounds -> confidence = 0.5 (neutral).
#' }
#'
#' @param pi95_low Lower 2.5\% quantile of the prediction.
#' @param pi95_high Upper 97.5\% quantile of the prediction.
#' @param value Optional point prediction. When supplied, normalisation
#'        is by \code{|value|}; otherwise by \code{|midpoint|}.
#' @return Numeric in \code{[0, 1]}.
#'
#' @export
pi_to_confidence <- function(pi95_low, pi95_high, value = NULL) {
  if (length(pi95_low) != length(pi95_high)) {
    rlang::abort("pi_to_confidence(): pi95_low and pi95_high must be same length")
  }
  width <- pi95_high - pi95_low
  if (is.null(value)) {
    value <- (pi95_low + pi95_high) / 2
  }
  abs_v <- abs(value)
  out <- numeric(length(width))
  for (i in seq_along(width)) {
    w <- width[i]
    av <- abs_v[i]
    if (is.na(w) || is.na(av)) {
      out[i] <- 0.5
    } else if (av < .Machine$double.eps) {
      # Anchor on absolute width when the value is essentially zero.
      out[i] <- pmax(0, pmin(1, 1 - w / 4))
    } else {
      out[i] <- pmax(0, pmin(1, 1 - (w / av) / 4))
    }
  }
  out
}


#' Fill missing soil attributes from spectra via OSSL
#'
#' Given a \code{\link{PedonRecord}} carrying a \code{spectra$vnir}
#' matrix (rows = horizons, columns = wavelengths in nm), pre-processes
#' the spectra, predicts the requested soil properties using the chosen
#' OSSL-backed method, and writes the predictions into the pedon's
#' horizons table via \code{pedon$add_measurement(..., source =
#' "predicted_spectra")}. Each call updates the pedon's provenance log
#' so that downstream classification can derive an evidence grade.
#'
#' By default, predicted values do \strong{not} overwrite measured
#' values (the \code{add_measurement()} authority logic protects them).
#' Setting \code{overwrite = TRUE} forces overwrite of any non-measured
#' value.
#'
#' @param pedon A \code{\link{PedonRecord}} with a
#'        \code{spectra$vnir} matrix.
#' @param library Currently only \code{"ossl"} is supported.
#' @param region One of \code{"global"}, \code{"south_america"},
#'        \code{"north_america"}, \code{"europe"}, \code{"africa"}.
#'        Used to subset the OSSL training data when supported by the
#'        underlying backend.
#' @param properties Character vector of OSSL-supported property names
#'        to predict. Default covers the most-requested
#'        WRB/SiBCS-relevant attributes.
#' @param method One of \code{"mbl"}, \code{"plsr_local"},
#'        \code{"pretrained"}.
#' @param preprocess Pre-processing pipeline; passed to
#'        \code{\link{preprocess_spectra}}.
#' @param k_neighbors Number of neighbours for memory-based methods.
#' @param overwrite If \code{FALSE} (default), only fill cells whose
#'        existing provenance is weaker than \code{predicted_spectra}.
#' @param ossl_library Optional OSSL library object (see
#'        \code{\link{predict_ossl_mbl}}).
#' @param ossl_models Optional named list of pretrained models (see
#'        \code{\link{predict_ossl_pretrained}}).
#' @param verbose If \code{TRUE}, prints a cli summary.
#' @return The mutated pedon, invisibly. Provenance entries with
#'         \code{source = "predicted_spectra"} are added per
#'         (horizon, property).
#'
#' @seealso \code{\link{preprocess_spectra}}, \code{\link{predict_ossl_mbl}},
#'          \code{\link{predict_ossl_plsr_local}},
#'          \code{\link{predict_ossl_pretrained}},
#'          \code{\link{pi_to_confidence}}.
#'
#' @export
fill_from_spectra <- function(pedon,
                                library     = "ossl",
                                region      = c("global", "south_america",
                                                "north_america", "europe", "africa"),
                                properties  = c("clay_pct", "sand_pct", "silt_pct",
                                                "cec_cmol", "bs_pct", "ph_h2o",
                                                "oc_pct", "fe_dcb_pct", "caco3_pct"),
                                method      = c("mbl", "plsr_local", "pretrained"),
                                preprocess  = "snv+sg1",
                                k_neighbors = 100L,
                                overwrite   = FALSE,
                                ossl_library = NULL,
                                ossl_models  = NULL,
                                verbose     = TRUE) {
  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("fill_from_spectra(): 'pedon' must be a PedonRecord")
  }
  region <- match.arg(region)
  method <- match.arg(method)
  if (library != "ossl") {
    rlang::abort(sprintf(
      "fill_from_spectra(): library = '%s' not supported; only 'ossl' (v0.4)",
      library
    ))
  }

  if (is.null(pedon$spectra) || is.null(pedon$spectra$vnir)) {
    rlang::abort(
      "fill_from_spectra(): pedon$spectra$vnir is missing. Provide a numeric matrix (rows = horizons, cols = wavelengths)."
    )
  }
  X_raw <- pedon$spectra$vnir
  if (!is.matrix(X_raw)) X_raw <- as.matrix(X_raw)
  if (nrow(X_raw) != nrow(pedon$horizons)) {
    rlang::abort(sprintf(
      "fill_from_spectra(): nrow(spectra) = %d != nrow(horizons) = %d",
      nrow(X_raw), nrow(pedon$horizons)
    ))
  }

  # 1. Pre-process
  X <- preprocess_spectra(X_raw, method = preprocess)

  # 2. Predict
  preds <- switch(
    method,
    mbl        = predict_ossl_mbl(X, properties = properties,
                                    region = region, k = k_neighbors,
                                    ossl_library = ossl_library),
    plsr_local = predict_ossl_plsr_local(X, properties = properties,
                                            region = region, k = k_neighbors,
                                            ossl_library = ossl_library),
    pretrained = predict_ossl_pretrained(X, properties = properties,
                                          region = region,
                                          ossl_models = ossl_models)
  )
  backend <- attr(preds, "backend") %||% "synthetic"

  if (verbose) {
    cli::cli_alert_info(sprintf(
      "fill_from_spectra(): {.field method}={method}, {.field region}={region}, {.field backend}={backend}, predictions={nrow(preds)}"
    ))
  }

  # 3. Merge into pedon (pedon$add_measurement handles overwrite policy)
  n_written <- 0L
  n_skipped <- 0L
  for (i in seq_len(nrow(preds))) {
    r <- preds[i, ]
    if (!r$property %in% names(pedon$horizons)) {
      n_skipped <- n_skipped + 1L
      next
    }
    conf <- pi_to_confidence(r$pi95_low, r$pi95_high, r$value)
    notes <- sprintf(
      "OSSL/%s/%s; PI95=[%.3g, %.3g]%s",
      method, region, r$pi95_low, r$pi95_high,
      if (!is.na(r$n_neighbors)) sprintf("; k=%d", r$n_neighbors) else ""
    )
    before <- nrow(pedon$provenance)
    pedon$add_measurement(
      horizon_idx = r$horizon_idx,
      attribute   = r$property,
      value       = r$value,
      source      = "predicted_spectra",
      confidence  = conf,
      notes       = notes,
      overwrite   = overwrite
    )
    if (nrow(pedon$provenance) > before) n_written <- n_written + 1L
    else                                  n_skipped <- n_skipped + 1L
  }

  if (verbose) {
    cli::cli_alert_success(sprintf(
      "fill_from_spectra(): wrote {n_written} cell(s), skipped {n_skipped} (existing measurements / unknown columns)"
    ))
  }

  invisible(pedon)
}


#' Build a synthetic PedonRecord with attached spectra (testing aid)
#'
#' Generates a small, deterministic \code{\link{PedonRecord}} with
#' \code{n_horizons} horizons and a Vis-NIR spectral matrix
#' (\code{350:2500} nm). Useful for exercising
#' \code{\link{fill_from_spectra}} in tests and vignettes.
#'
#' @param n_horizons Integer number of horizons (default 5).
#' @param wavelengths Integer vector of wavelengths (default
#'        \code{350:2500}).
#' @param seed Integer seed for the RNG used to generate the spectra.
#' @return A \code{\link{PedonRecord}} with a \code{$spectra$vnir}
#'         matrix attached.
#' @export
make_synthetic_pedon_with_spectra <- function(n_horizons  = 5L,
                                                wavelengths = 350:2500,
                                                seed        = 1L) {
  n_horizons  <- as.integer(n_horizons)
  wavelengths <- as.integer(wavelengths)
  set.seed(seed)

  # Generate physically plausible-ish reflectance: a smooth baseline
  # with a few absorption features that vary by horizon.
  base <- 0.25 + 0.0001 * (wavelengths - 350)
  noise <- matrix(rnorm(n_horizons * length(wavelengths), 0, 0.005),
                    nrow = n_horizons)
  feature <- outer(seq_len(n_horizons),
                     wavelengths,
                     function(i, w) 0.08 * exp(-((w - 1400 - 30 * i)^2) / 1e4))
  vnir <- sweep(noise + feature, 2, base, `+`)
  colnames(vnir) <- as.character(wavelengths)

  hz <- data.table::data.table(
    top_cm    = seq.int(0, by = 20, length.out = n_horizons),
    bottom_cm = seq.int(20, by = 20, length.out = n_horizons),
    designation = c("A", paste0("B", seq_len(n_horizons - 1L)))[seq_len(n_horizons)]
  )

  PedonRecord$new(
    site = list(
      id      = "SYNTH-01",
      lat     = -22.5,
      lon     = -43.7,
      crs     = 4326,
      country = "BR"
    ),
    horizons = hz,
    spectra  = list(vnir = vnir,
                      metadata = list(unit = "reflectance",
                                        wavelengths_nm = wavelengths))
  )
}
