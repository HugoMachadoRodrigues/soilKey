# =============================================================================
# soilKey -- spectral nearest-neighbour classification.
#
# `classify_by_spectral_neighbours()` answers a different question than
# `predict_ossl_mbl()`:
#
#   - predict_ossl_mbl()        -- given a spectrum, predict ATTRIBUTE
#                                  values (clay, OC, ...).
#   - classify_by_spectral_neighbours() -- given a spectrum, find the
#                                  K most similar OSSL profiles and
#                                  return their soil-classification
#                                  labels as a probabilistic class
#                                  prediction.
#
# This is the "spectral analogy" use case: the user has a Vis-NIR
# scan of a horizon (or a stack of horizon scans), and wants a
# data-driven hint about which RSG / Ordem / Order this profile
# probably belongs to, BEFORE running the deterministic key. The
# architectural invariant still holds: the deterministic key remains
# the only thing that ASSIGNS a class. This function is a *guide*,
# tagging its output with `inferred_prior` provenance.
#
# The recommended flow:
#
#   1. classify_by_spectral_neighbours(spectrum, region, ...)
#      -> ranked list of likely classes from spectral similarity.
#   2. The user collects the field / lab data needed to confirm.
#   3. classify_wrb2022(pedon) on the populated PedonRecord assigns
#      the canonical class deterministically.
# =============================================================================


#' Classify a soil by spectral similarity to OSSL reference profiles
#'
#' Given a Vis-NIR (or MIR) spectrum and an OSSL reference library
#' enriched with WRB / SiBCS / USDA labels, returns the K most
#' spectrally similar profiles plus a probabilistic class prediction
#' aggregated from their labels.
#'
#' This is the **spectral analogy** classifier. It does not replace
#' the deterministic key in
#' \code{\link{classify_wrb2022}} / \code{\link{classify_sibcs}} /
#' \code{\link{classify_usda}}; instead it provides a high-prior
#' "expected class" before the user has lab data, reducing the
#' search space when collecting confirming attributes.
#'
#' @section Distance metric:
#' By default we compute distances on PLS scores (matching the
#' resemble / OSSL recipe), with PLS components fit on the OSSL
#' reference Yr matrix. When \code{resemble} is unavailable, we fall
#' back to PCA scores from \code{stats::prcomp} on the preprocessed
#' Xr -- a defensible-but-simpler heuristic.
#'
#' @section Region filter:
#' Optional \code{lat / lon / radius_km} arguments filter the OSSL
#' library to profiles within \code{radius_km} (great-circle) of the
#' query location before computing distances. This implements the
#' "biome-aware" use case the architecture document calls for: a
#' Cerrado profile shouldn't have its class inferred from spectral
#' neighbours in the Boreal taiga.
#'
#' @param spectrum Numeric vector or 1-row matrix (the query
#'        spectrum). Must align (after preprocessing) with the
#'        column space of \code{ossl_library$Xr}.
#' @param ossl_library A list with \code{Xr} (numeric matrix, rows
#'        = OSSL training profiles, cols = wavelengths) and \code{Yr}
#'        (data frame keyed by property; \emph{must include} a
#'        column named \code{wrb_rsg} and / or \code{sibcs_ordem} /
#'        \code{usda_order} for the labels to aggregate over).
#'        \code{ossl_library} may also carry \code{lat} and
#'        \code{lon} columns in \code{Yr} for the regional filter.
#' @param system One of \code{"wrb2022"} (default), \code{"sibcs"},
#'        \code{"usda"}. Controls which label column of \code{Yr}
#'        is aggregated.
#' @param k Number of nearest neighbours (default 25).
#' @param preprocess Pre-processing pipeline; passed to
#'        \code{\link{preprocess_spectra}}. Default \code{"snv+sg1"}.
#' @param region Optional \code{list(lat, lon, radius_km)} for a
#'        regional filter on \code{ossl_library$Yr$lat / lon}.
#' @param verbose Emit a \code{cli} summary.
#' @return A list with three elements:
#'   \describe{
#'     \item{\code{distribution}}{A \code{data.table} with columns
#'           \code{class}, \code{n_neighbours}, \code{probability}
#'           (= \code{n_neighbours / k}), sorted by probability.}
#'     \item{\code{neighbours}}{A \code{data.table} with one row per
#'           neighbour (top K), columns \code{rank}, \code{distance},
#'           \code{class}, plus any other columns present in
#'           \code{ossl_library$Yr}.}
#'     \item{\code{query}}{The query metadata (system, k,
#'           region filter, n_library_rows, n_filtered).}
#'   }
#' @examples
#' \dontrun{
#' # Toy run against the bundled demo library (synthetic):
#' data(ossl_demo_sa)
#' # Inject a fake label column for the demo (real OSSL has it):
#' ossl_demo_sa$Yr$wrb_rsg <- sample(c("FR", "AC", "LX", "AL"),
#'                                     nrow(ossl_demo_sa$Yr),
#'                                     replace = TRUE)
#' query <- ossl_demo_sa$Xr[1, ]
#' res <- classify_by_spectral_neighbours(query, ossl_demo_sa,
#'                                         k = 10)
#' res$distribution    # ranked classes
#' res$neighbours      # the 10 most similar profiles
#' }
#' @seealso \code{\link{predict_ossl_mbl}} (predicts attributes),
#'          \code{\link{classify_wrb2022}} (the deterministic key).
#' @export
classify_by_spectral_neighbours <- function(spectrum,
                                              ossl_library,
                                              system     = c("wrb2022",
                                                             "sibcs",
                                                             "usda"),
                                              k          = 25L,
                                              preprocess = "snv+sg1",
                                              region     = NULL,
                                              verbose    = TRUE) {
  system <- match.arg(system)
  label_col <- switch(system,
                        wrb2022 = "wrb_rsg",
                        sibcs   = "sibcs_ordem",
                        usda    = "usda_order")

  # Validate inputs.
  if (!is.list(ossl_library) ||
        !all(c("Xr", "Yr") %in% names(ossl_library)))
    stop("`ossl_library` must be a list with elements `Xr` and `Yr`. ",
         "Use `ossl_library_template()` to build one.")
  Xr <- ossl_library$Xr
  Yr <- as.data.frame(ossl_library$Yr)
  if (!(label_col %in% names(Yr))) {
    stop(sprintf(
      paste0("Column `%s` not found in ossl_library$Yr. The selected ",
             "system='%s' requires that column.\n  Available: %s"),
      label_col, system, paste(names(Yr), collapse = ", ")
    ))
  }
  if (!is.numeric(spectrum))
    stop("`spectrum` must be numeric.")
  if (is.null(dim(spectrum))) spectrum <- matrix(spectrum, nrow = 1L)
  if (ncol(spectrum) != ncol(Xr))
    stop(sprintf(
      paste0("Spectrum has %d wavelengths; OSSL library has %d. ",
             "Resample the query before calling."),
      ncol(spectrum), ncol(Xr)))

  # Optional region filter.
  filtered_idx <- seq_len(nrow(Xr))
  if (!is.null(region)) {
    if (!all(c("lat", "lon", "radius_km") %in% names(region)))
      stop("`region` must have `lat`, `lon`, and `radius_km`.")
    if (!all(c("lat", "lon") %in% names(Yr)))
      stop("To use a region filter, ossl_library$Yr must include ",
           "`lat` and `lon` columns.")
    d_km <- .haversine_km(region$lat, region$lon, Yr$lat, Yr$lon)
    filtered_idx <- which(!is.na(d_km) & d_km <= region$radius_km)
    if (length(filtered_idx) == 0L) {
      cli::cli_alert_warning(c(
        "No OSSL profiles within {.val {region$radius_km}} km of ",
        "(lat={region$lat}, lon={region$lon}). Falling back to the ",
        "global library."
      ))
      filtered_idx <- seq_len(nrow(Xr))
    } else if (verbose) {
      cli::cli_alert_info(
        "{.val {length(filtered_idx)}} OSSL profile{?s} within {.val {region$radius_km}} km of the query."
      )
    }
  }

  Xr_f <- Xr[filtered_idx, , drop = FALSE]
  Yr_f <- Yr[filtered_idx, , drop = FALSE]
  k    <- min(k, nrow(Xr_f))
  if (k < 1L)
    stop("No reference profiles available after filtering.")

  # Preprocess (query and library together so the same shift is
  # applied).
  X_query <- preprocess_spectra(spectrum, method = preprocess)
  X_lib   <- preprocess_spectra(Xr_f, method = preprocess)

  # Reduce dimensionality. Prefer PLS scores (resemble); fall back to
  # PCA (stats::prcomp) when resemble is unavailable.
  scores <- .reduce_for_neighbours(X_lib, X_query, Yr_f[[label_col]])
  lib_scores   <- scores$lib
  query_score  <- scores$query

  # Compute Euclidean distance in score space.
  dist_vec <- as.vector(sqrt(rowSums((lib_scores -
                                          rep(query_score,
                                                each = nrow(lib_scores)))^2)))
  ord <- order(dist_vec)
  topK <- ord[seq_len(k)]

  # Build neighbour table.
  classes <- as.character(Yr_f[[label_col]][topK])
  neighbours <- data.table::data.table(
    rank     = seq_len(k),
    distance = dist_vec[topK],
    class    = classes
  )
  # Append any other columns from Yr (lat, lon, country, ...).
  extras <- setdiff(names(Yr_f), label_col)
  for (col in extras) {
    neighbours[[col]] <- Yr_f[[col]][topK]
  }

  # Aggregate distribution. We avoid data.table's `:=` operator
  # because it requires a cedta()-aware caller and triggers a hard
  # error when the function is invoked from a sub-agent / unloaded
  # context. Plain column assignment works regardless.
  dist_table <- as.data.frame(table(class = classes),
                                stringsAsFactors = FALSE)
  dist_table$n_neighbours <- dist_table$Freq
  dist_table$probability  <- dist_table$Freq / k
  dist_table$Freq         <- NULL
  dist_table <- dist_table[order(-dist_table$probability), ,
                              drop = FALSE]
  data.table::setDT(dist_table)

  if (verbose) {
    top1 <- dist_table[1, ]
    cli::cli_alert_success(
      "Spectral analogy: top class = {.strong {top1$class}} ({sprintf('%.0f%%', 100 * top1$probability)} of {k} neighbours)"
    )
  }

  list(
    distribution = dist_table,
    neighbours   = neighbours,
    query        = list(
      system          = system,
      k               = k,
      region          = region,
      n_library_rows  = nrow(Xr),
      n_filtered      = length(filtered_idx)
    )
  )
}


# ---- internals -------------------------------------------------------------


#' Reduce X (library + query) to a small score space.
#' @keywords internal
.reduce_for_neighbours <- function(X_lib, X_query, y_label) {
  # Try resemble's PLS scores when available -- this matches the
  # OSSL reference workflow (Ramirez-Lopez et al., 2013).
  use_pls <- requireNamespace("resemble", quietly = TRUE)
  if (use_pls && is.character(y_label)) {
    # PLS needs a numeric response; encode classes as 1..K.
    y_num <- as.integer(as.factor(y_label))
    fit <- tryCatch(
      resemble::ortho_projection(Xr = X_lib, Yr = y_num,
                                   method = "pls", pc_selection =
                                     list(method = "manual", value = 8L)),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      scores_lib   <- fit$scores
      scores_query <- predict(fit, X_query)
      return(list(lib = scores_lib, query = as.numeric(scores_query)))
    }
  }
  # Fallback: PCA on the joint matrix.
  joint <- rbind(X_lib, X_query)
  pca <- stats::prcomp(joint, center = TRUE, scale. = FALSE,
                          rank. = min(8L, ncol(joint), nrow(joint) - 1L))
  scores_lib   <- pca$x[seq_len(nrow(X_lib)), , drop = FALSE]
  scores_query <- pca$x[nrow(X_lib) + 1L, ]
  list(lib = scores_lib, query = as.numeric(scores_query))
}


#' Great-circle distance (km) between (lat1, lon1) and the elementwise
#' (lat2, lon2) vectors.
#' @keywords internal
.haversine_km <- function(lat1, lon1, lat2, lon2) {
  R <- 6371.0
  to_rad <- pi / 180
  d_lat <- (lat2 - lat1) * to_rad
  d_lon <- (lon2 - lon1) * to_rad
  a <- sin(d_lat / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) * sin(d_lon / 2)^2
  c_ <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R * c_
}
