# =============================================================================
# v0.9.99 -- SoilGrids depth-resolved prior.
#
# spatial_prior_soilgrids() returns a site-level RSG probability vector. The
# photo-only pipeline needs something different: depth-resolved estimates of
# the horizon attributes a deterministic key consumes (clay, sand, silt, pH,
# organic carbon, CEC). apply_soilgrids_depth_prior() fetches the six standard
# SoilGrids depth slices at the profile coordinates and interpolates each
# horizon's mid-depth value, recording every fill as an "inferred_prior"
# provenance entry.
# =============================================================================

# Mid-depths (cm) of the six standard SoilGrids 2.0 depth intervals
# 0-5, 5-15, 15-30, 30-60, 60-100, 100-200.
.SOILGRIDS_DEPTH_LABELS <- c("0-5cm", "5-15cm", "15-30cm",
                             "30-60cm", "60-100cm", "100-200cm")
.SOILGRIDS_DEPTH_MIDS   <- c(2.5, 10, 22.5, 45, 80, 150)

# soilKey horizon column -> SoilGrids 2.0 property name.
.soilgrids_property_map <- function() {
  c(clay_pct = "clay",
    sand_pct = "sand",
    silt_pct = "silt",
    ph_h2o   = "phh2o",
    oc_pct   = "soc",
    cec_cmol = "cec")
}

# Linear interpolation of a six-slice depth profile at one mid-depth.
# Clamps to the shallowest / deepest slice and clips negatives to zero.
.interp_depth_profile <- function(mid_cm, mids, values) {
  ok <- !is.na(values)
  if (!any(ok)) return(NA_real_)
  mids <- mids[ok]; values <- values[ok]
  if (length(mids) == 1L)      return(max(0, values[1L]))
  if (mid_cm <= mids[1L])      return(max(0, values[1L]))
  if (mid_cm >= mids[length(mids)])
    return(max(0, values[length(values)]))
  v <- stats::approx(mids, values, xout = mid_cm)$y
  max(0, v)
}

# Query the ISRIC SoilGrids REST API for the six-slice mean profile of
# each requested property. Returns a named list (soilKey attr -> numeric
# vector of length 6). Network path; not exercised by the test suite.
.soilgrids_rest_fetch <- function(lat, lon, attrs, timeout = 30) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    rlang::abort(paste0("apply_soilgrids_depth_prior() needs 'httr' and ",
                        "'jsonlite' for the live SoilGrids fetch; install ",
                        "them or pass depth_profiles= explicitly."))
  }
  pmap   <- .soilgrids_property_map()
  props  <- unname(pmap[attrs])
  query  <- c(
    list(lon = lon, lat = lat, value = "mean"),
    stats::setNames(as.list(props), rep("property", length(props))),
    stats::setNames(as.list(.SOILGRIDS_DEPTH_LABELS),
                    rep("depth", length(.SOILGRIDS_DEPTH_LABELS)))
  )
  resp <- httr::GET("https://rest.isric.org/soilgrids/v2.0/properties/query",
                    query = query, httr::timeout(timeout))
  httr::stop_for_status(resp)
  parsed <- jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE)

  rev_map <- stats::setNames(names(pmap), unname(pmap))
  out <- list()
  for (layer in parsed$properties$layers %||% list()) {
    attr_name <- rev_map[[layer$name %||% ""]]
    if (is.null(attr_name)) next
    d_factor <- layer$unit_measure$d_factor %||% 1
    by_label <- stats::setNames(
      lapply(layer$depths, function(d) d$values$mean %||% NA),
      vapply(layer$depths, function(d) d$label %||% NA_character_,
             character(1)))
    vals <- vapply(.SOILGRIDS_DEPTH_LABELS, function(lbl) {
      v <- by_label[[lbl]]
      if (is.null(v) || is.na(v)) NA_real_ else as.numeric(v) / d_factor
    }, numeric(1))
    out[[attr_name]] <- unname(vals)
  }
  out
}


#' Fill missing horizon attributes from a SoilGrids depth prior
#'
#' For each horizon and each requested attribute, interpolates the value
#' at the horizon's mid-depth from the six standard SoilGrids 2.0 depth
#' slices (0-5, 5-15, 15-30, 30-60, 60-100, 100-200 cm) and writes it
#' into the pedon with \code{source = "inferred_prior"}. Existing values
#' are preserved unless \code{overwrite = TRUE}; the
#' \code{\link{PedonRecord}} authority order means a SoilGrids prior can
#' never silently displace a measured, spectra-predicted or VLM-extracted
#' value.
#'
#' This is the depth-resolved companion to
#' \code{\link{spatial_prior_soilgrids}} (which returns a site-level RSG
#' probability vector, not horizon attributes), and the attribute-fill
#' stage of \code{\link{classify_from_photos}}.
#'
#' @param pedon A \code{\link{PedonRecord}} with at least one horizon.
#'        For the live fetch it must also carry \code{site$lat} and
#'        \code{site$lon}.
#' @param attrs Character vector of horizon columns to fill. Defaults to
#'        all SoilGrids-backed attributes: \code{clay_pct}, \code{sand_pct},
#'        \code{silt_pct}, \code{ph_h2o}, \code{oc_pct}, \code{cec_cmol}.
#' @param depth_profiles Optional named list mapping an attribute to a
#'        numeric vector of six slice values (0-5 ... 100-200 cm). When
#'        supplied the SoilGrids network call is skipped entirely -- this
#'        is the path the test suite and offline users take.
#' @param overwrite If \code{FALSE} (default) only \code{NA} cells are
#'        filled. If \code{TRUE}, every requested cell is overwritten
#'        (subject to the provenance authority order).
#' @return Invisibly, the mutated \code{pedon}. An attribute
#'         \code{"soilgrids_depth_fill"} on the return value records how
#'         many cells were filled.
#' @examples
#' \dontrun{
#' p <- make_cambisol_canonical()
#' p$horizons$clay_pct <- NA_real_
#' # Offline: supply the six-slice profiles directly.
#' apply_soilgrids_depth_prior(
#'   p, attrs = "clay_pct",
#'   depth_profiles = list(clay_pct = c(18, 20, 24, 28, 30, 30)))
#' }
#' @export
apply_soilgrids_depth_prior <- function(pedon,
                                        attrs = NULL,
                                        depth_profiles = NULL,
                                        overwrite = FALSE) {
  if (!inherits(pedon, "PedonRecord")) {
    rlang::abort("`pedon` must be a PedonRecord")
  }
  h <- pedon$horizons
  if (is.null(h) || nrow(h) == 0L) {
    rlang::warn("apply_soilgrids_depth_prior(): pedon has no horizons; skipping")
    return(invisible(pedon))
  }

  pmap <- .soilgrids_property_map()
  if (is.null(attrs)) attrs <- names(pmap)
  attrs <- intersect(intersect(attrs, names(pmap)), names(h))
  if (length(attrs) == 0L) {
    rlang::warn("apply_soilgrids_depth_prior(): no SoilGrids-backed attributes requested")
    return(invisible(pedon))
  }

  # Obtain the six-slice depth profiles.
  if (is.null(depth_profiles)) {
    lat <- pedon$site$lat %||% NA_real_
    lon <- pedon$site$lon %||% NA_real_
    if (is.na(lat) || is.na(lon)) {
      rlang::warn(paste0("apply_soilgrids_depth_prior(): pedon has no ",
                         "coordinates; skipping SoilGrids fill"))
      return(invisible(pedon))
    }
    depth_profiles <- tryCatch(
      .soilgrids_rest_fetch(lat, lon, attrs),
      error = function(e) {
        rlang::warn(sprintf("SoilGrids fetch failed (%s); skipping",
                            conditionMessage(e)))
        NULL
      })
    if (is.null(depth_profiles)) return(invisible(pedon))
  }

  n_filled <- 0L
  for (a in attrs) {
    prof <- depth_profiles[[a]]
    if (is.null(prof) || all(is.na(prof))) next
    for (i in seq_len(nrow(h))) {
      if (!overwrite && !is.na(h[[a]][i])) next
      mid <- (h$top_cm[i] + h$bottom_cm[i]) / 2
      if (is.na(mid)) next
      val <- .interp_depth_profile(mid, .SOILGRIDS_DEPTH_MIDS, prof)
      if (is.na(val)) next
      pedon$add_measurement(
        i, a, value = val,
        source     = "inferred_prior",
        confidence = 0.5,
        notes      = "SoilGrids 2.0 depth prior",
        overwrite  = overwrite
      )
      n_filled <- n_filled + 1L
    }
  }
  attr(pedon, "soilgrids_depth_fill") <- list(n_filled = n_filled,
                                              attrs = attrs)
  invisible(pedon)
}
