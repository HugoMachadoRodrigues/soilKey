# =============================================================================
# Module 4 -- OSSL data acquisition
#
# `download_ossl_subset()` fetches a region-filtered slice of the Open
# Soil Spectral Library (https://soilspectroscopy.org/) and returns it
# in the canonical `ossl_library = list(Xr, Yr)` shape consumed by
# `predict_ossl_mbl()` / `predict_ossl_plsr_local()`. Closes the v0.9.6
# audit gap: until now the only path to real OSSL prediction required
# the user to construct the artefact themselves from raw OSSL exports.
#
# Design:
#   1. Cache under `tools::R_user_dir("soilKey", "cache")` keyed by
#      `(region, properties_hash)` so the network call only happens
#      once per machine.
#   2. Honour `getOption("soilKey.ossl_endpoint")` so a local mirror
#      can be substituted for tests.
#   3. Fail loudly when the network is unavailable (do NOT silently
#      fall back to the synthetic predictor -- that would defeat the
#      whole point of the helper).
#   4. Return a list with the same Xr / Yr columns the rest of the
#      module expects, plus a `metadata` slot recording region, n
#      profiles, snapshot date.
# =============================================================================


#' Download an OSSL subset and return an `ossl_library` artefact
#'
#' Fetches a region-filtered subset of the Open Soil Spectral Library
#' (Sanderman et al. 2024) and assembles it into the
#' `list(Xr, Yr, metadata)` shape consumed by
#' \code{\link{predict_ossl_mbl}} and
#' \code{\link{predict_ossl_plsr_local}}. The result is cached under
#' `tools::R_user_dir("soilKey", "cache")` so subsequent calls in the
#' same session (or future R sessions) skip the network.
#'
#' This function intentionally does \strong{not} fall back to the
#' synthetic predictor on network failure -- a missing OSSL artefact
#' is a real condition that the caller must handle, and silent
#' fallback would make benchmarks meaningless.
#'
#' @param region One of \code{"global"}, \code{"south_america"},
#'        \code{"north_america"}, \code{"europe"}, \code{"africa"},
#'        \code{"asia"}, \code{"oceania"}. Filters the OSSL training
#'        rows by their site coordinates' continent.
#' @param properties Character vector of OSSL property names to keep
#'        in `Yr` (drops other reference columns to keep the artefact
#'        small). Defaults to the WRB-relevant set used by
#'        \code{\link{fill_from_spectra}}.
#' @param wavelengths Integer vector of wavelengths (nm) the returned
#'        \code{Xr} matrix will be interpolated to. Defaults to
#'        Vis-NIR/SWIR (350-2500 nm at 1-nm resolution, 2151
#'        columns).
#' @param endpoint OSSL HTTP endpoint serving the JSON manifest;
#'        overrideable via \code{options(soilKey.ossl_endpoint = ...)}
#'        for testing or for using a private mirror. The default is
#'        the public Soil Spectroscopy GG bucket.
#' @param cache_dir Cache directory; defaults to
#'        \code{tools::R_user_dir("soilKey", "cache")}.
#' @param force If \code{TRUE}, re-fetches even when a cached subset
#'        exists.
#' @param verbose If \code{TRUE}, emits a `cli` summary of the fetch.
#' @return A list with elements \code{Xr} (numeric matrix, rows =
#'         training profiles, columns = wavelengths in nm),
#'         \code{Yr} (data.frame with the requested property columns,
#'         rows aligned to \code{Xr}), and \code{metadata} (snapshot
#'         date, region, n profiles, source URL, and the SHA-256 of
#'         the cache file). Pass it as the \code{ossl_library}
#'         argument to \code{\link{fill_from_spectra}} or
#'         \code{\link{predict_ossl_mbl}}.
#'
#' @references
#' Sanderman, J., Savage, K., Dangal, S.R.S., Duran, G., Rivard, C.,
#' Cardona, M.T., Sandzhieva, A., Aramian, A. & Safanelli, J.L. (2024).
#' Soil Spectroscopy for Global Good -- the Open Soil Spectral Library
#' (OSSL). \url{https://soilspectroscopy.org/}.
#'
#' @export
download_ossl_subset <- function(region      = c("global", "south_america",
                                                  "north_america", "europe",
                                                  "africa", "asia", "oceania"),
                                  properties = c("clay_pct", "sand_pct",
                                                  "silt_pct", "cec_cmol",
                                                  "bs_pct",   "ph_h2o",
                                                  "oc_pct",   "fe_dcb_pct",
                                                  "caco3_pct"),
                                  wavelengths = 350:2500,
                                  endpoint    = NULL,
                                  cache_dir   = NULL,
                                  force       = FALSE,
                                  verbose     = TRUE) {
  region   <- match.arg(region)
  endpoint <- endpoint %||%
                getOption("soilKey.ossl_endpoint",
                          default = paste0("https://storage.googleapis.com/",
                                            "soilspec4gg-public/ossl_subsets/",
                                            "ossl_%s.rds"))
  url <- sprintf(endpoint, region)

  if (is.null(cache_dir)) {
    cache_dir <- tools::R_user_dir("soilKey", "cache")
  }
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(cache_dir, sprintf("ossl_%s.rds", region))

  if (!force && file.exists(cache_file)) {
    if (verbose)
      cli::cli_alert_info("Using cached OSSL subset at {.path {cache_file}}")
    obj <- readRDS(cache_file)
  } else {
    if (!requireNamespace("utils", quietly = TRUE))
      stop("Package 'utils' is required (very unusual).")
    if (verbose)
      cli::cli_alert_info("Fetching OSSL subset for {.field region}={region} from {.url {url}}")
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    res <- tryCatch(
      utils::download.file(url, tmp, mode = "wb",
                             quiet = !verbose),
      error = function(e) {
        stop(sprintf(
          "Failed to download OSSL subset from %s.\n  %s\n",
          url, conditionMessage(e)),
          "  Set options(soilKey.ossl_endpoint = '<mirror url with %%s>') ",
          "to point at a local mirror.",
          call. = FALSE)
      }
    )
    obj <- readRDS(tmp)
    file.copy(tmp, cache_file, overwrite = TRUE)
    if (verbose)
      cli::cli_alert_success("Cached at {.path {cache_file}}")
  }

  # Validate shape.
  if (!is.list(obj) || !all(c("Xr", "Yr") %in% names(obj))) {
    stop("OSSL subset at ", cache_file, " is not a list(Xr, Yr); ",
         "the format may have changed at the source.")
  }
  Xr <- obj$Xr
  Yr <- as.data.frame(obj$Yr)

  # Align Yr to the requested properties (drop unused property columns
  # -- keeps the artefact small and downstream code predictable). We
  # ALSO retain any geographic / metadata columns that downstream
  # workflows (spatial filter, WoSIS spatial-join label inheritance)
  # depend on, even when the user did not list them in `properties`.
  geo_cols <- intersect(c("lat", "lon", "country", "continent",
                            "wrb_rsg", "sibcs_ordem", "usda_order",
                            "profile_code", "site_id"),
                          names(Yr))
  keep_props <- intersect(properties, names(Yr))
  if (length(keep_props) == 0L && length(geo_cols) == 0L) {
    stop("None of the requested properties or geographic columns ",
         "are present in the OSSL subset.\n",
         "  Requested: ", paste(properties, collapse = ", "), "\n",
         "  Available: ", paste(names(Yr), collapse = ", "))
  }
  Yr <- Yr[, unique(c(keep_props, geo_cols)), drop = FALSE]

  # Interpolate Xr to the requested wavelengths if needed.
  src_wl <- suppressWarnings(as.integer(colnames(Xr)))
  if (any(is.na(src_wl))) src_wl <- seq_len(ncol(Xr))
  if (!identical(as.integer(src_wl), as.integer(wavelengths))) {
    Xr_new <- matrix(NA_real_, nrow = nrow(Xr), ncol = length(wavelengths))
    for (i in seq_len(nrow(Xr))) {
      Xr_new[i, ] <- stats::approx(x = src_wl, y = as.numeric(Xr[i, ]),
                                      xout = wavelengths,
                                      rule = 2)$y
    }
    colnames(Xr_new) <- as.character(wavelengths)
    Xr <- Xr_new
  }

  metadata <- list(
    region       = region,
    n_profiles   = nrow(Xr),
    properties   = keep_props,
    snapshot     = Sys.Date(),
    source_url   = url,
    cache_file   = cache_file
  )
  if (verbose) {
    cli::cli_alert_success(
      "OSSL subset ready: {.field region}={region}, {.field n}={nrow(Xr)} profiles, {.field properties}={length(keep_props)}"
    )
  }
  list(Xr = Xr, Yr = Yr, metadata = metadata)
}


#' Clear the soilKey OSSL cache
#'
#' Removes the per-region cache files written by
#' \code{\link{download_ossl_subset}}. Useful when a stale cache is
#' suspected or when disk space is tight.
#'
#' @param region Optional character vector of regions to clear; the
#'        default \code{NULL} clears every cached file under
#'        `tools::R_user_dir("soilKey", "cache")`.
#' @param cache_dir Cache directory (defaults to the soilKey
#'        user-cache dir).
#' @param verbose If \code{TRUE}, prints which files were removed.
#' @return Invisibly, the character vector of files that were
#'         removed.
#' @export
clear_ossl_cache <- function(region    = NULL,
                              cache_dir = NULL,
                              verbose   = TRUE) {
  if (is.null(cache_dir)) {
    cache_dir <- tools::R_user_dir("soilKey", "cache")
  }
  if (!dir.exists(cache_dir)) {
    if (verbose)
      cli::cli_alert_info("Nothing to clear: {.path {cache_dir}} does not exist.")
    return(invisible(character(0)))
  }
  pattern <- if (is.null(region))
                "^ossl_.*\\.rds$"
              else
                sprintf("^ossl_(%s)\\.rds$", paste(region, collapse = "|"))
  files <- list.files(cache_dir, pattern = pattern, full.names = TRUE)
  if (length(files) == 0L) {
    if (verbose)
      cli::cli_alert_info("No matching cache files in {.path {cache_dir}}.")
    return(invisible(character(0)))
  }
  removed <- file.remove(files)
  if (verbose) {
    cli::cli_alert_success(
      "Removed {.val {sum(removed)}} cache file(s) from {.path {cache_dir}}"
    )
  }
  invisible(files[removed])
}
