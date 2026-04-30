# ================================================================
# Module 3 -- SoilGrids backend
#
# ISRIC SoilGrids 2.0 publishes the dominant WRB Reference Soil Group
# at each ~250 m pixel as a Cloud-Optimized GeoTIFF served over their
# WCS (and via direct COG URLs on https://maps.isric.org/). The
# canonical "MostProbable" raster maps integer codes to RSG codes
# (e.g. 1 = AC for Acrisols, 14 = FR for Ferralsols, ...).
#
# Reference URLs:
#   https://www.isric.org/explore/soilgrids
#   https://www.isric.org/explore/soilgrids/faq-soilgrids
#   https://maps.isric.org/
#   https://files.isric.org/soilgrids/latest/data/wrb/
#
# This file does not embed any remote download in the test path. The
# user supplies either:
#   - a remote URL (passed to terra::rast() -- works for COG / VRT)
#   - a local raster path
#   - or for testing, sets options(soilKey.test_raster = "/tmp/x.tif")
#     and we read that.
#
# The raster may be either:
#   (a) categorical with integer values that map to RSG codes via the
#       \code{rsg_code_lut} table (preferred -- this is what SoilGrids
#       MostProbable uses), or
#   (b) categorical with integer values that ALREADY are interpretable
#       as factor levels whose labels are RSG codes -- in that case we
#       use terra::levels() / categorical raster machinery.
# ================================================================


#' SoilGrids spatial prior
#'
#' Reads a categorical raster of dominant Reference Soil Groups around
#' the pedon's site, buffers the point in metric coordinates, extracts
#' all pixel values within the buffer, and returns the empirical class
#' frequency as a probability distribution over RSG codes.
#'
#' @section Data source:
#' For real use, pass \code{source_url} pointing at a SoilGrids
#' "MostProbable WRB" GeoTIFF / COG, e.g. one of the regional cuts
#' published at \code{https://files.isric.org/soilgrids/latest/data/wrb/}.
#' For tests, set \code{options(soilKey.test_raster = "/path/to/syn.tif")}
#' to point at a local synthetic raster -- this avoids network access
#' in CI.
#'
#' @section Coordinate handling:
#' We use \code{sf::st_transform} when sf is available; otherwise we
#' fall back to \code{terra::project} on a single-point SpatVector.
#' The buffer is constructed in metric (UTM) coordinates so
#' \code{buffer_m} is in metres regardless of the pedon CRS. The
#' raster itself is queried in its native CRS via terra's automatic
#' reprojection.
#'
#' @param pedon A \code{\link{PedonRecord}} with non-NULL
#'        \code{site$lat} and \code{site$lon}.
#' @param system Classification system; \code{"wrb2022"} (default) maps
#'        SoilGrids integer codes through the WRB lookup table.
#'        \code{"usda"} is reserved for a future SoilGrids-USDA layer.
#' @param buffer_m Buffer radius in metres around the point (default
#'        250 m, i.e. one SoilGrids pixel).
#' @param source_url Optional. A path or URL accepted by
#'        \code{terra::rast}. If NULL, falls back to
#'        \code{getOption("soilKey.test_raster")}.
#' @param n_classes_top Keep only the top N classes by frequency
#'        (default 10). Set to \code{Inf} to keep all.
#' @param lut Optional named integer vector mapping raster values to
#'        RSG codes. Default is \code{\link{soilgrids_wrb_lut}}; pass
#'        a custom one if your raster uses different codes.
#' @param ... Reserved for future use.
#' @return A \code{data.table} with columns \code{rsg_code},
#'         \code{probability}.
#' @seealso \code{\link{spatial_prior}}, \code{\link{soilgrids_wrb_lut}}.
#' @export
spatial_prior_soilgrids <- function(pedon,
                                      system        = c("wrb2022", "usda"),
                                      buffer_m      = 250,
                                      source_url    = NULL,
                                      n_classes_top = 10,
                                      lut           = NULL,
                                      ...) {
  system <- match.arg(system)

  if (!requireNamespace("terra", quietly = TRUE)) {
    rlang::abort(
      "Package 'terra' is required for spatial_prior_soilgrids() -- install with install.packages('terra')"
    )
  }

  if (is.null(lut)) {
    lut <- if (system == "wrb2022") soilgrids_wrb_lut() else soilgrids_usda_lut()
  }

  if (is.null(source_url)) {
    source_url <- getOption("soilKey.test_raster", default = NULL)
  }
  if (is.null(source_url) || !nzchar(source_url)) {
    rlang::abort(paste0(
      "No raster source given. Pass source_url= explicitly, or set ",
      "options(soilKey.test_raster = '/path/to/raster.tif') for testing."
    ))
  }

  rst <- terra::rast(source_url)

  buf <- soilgrids_buffer_vect(pedon, buffer_m = buffer_m)

  # terra::extract on a polygon returns a data.frame with one row per
  # pixel-polygon intersection; the value column is the layer name.
  ex <- terra::extract(rst, buf, touches = TRUE)
  vals <- ex[[ncol(ex)]]
  vals <- vals[!is.na(vals)]

  if (length(vals) == 0L) {
    return(data.table::data.table(
      rsg_code    = character(),
      probability = numeric()
    ))
  }

  # Translate integer raster codes to RSG codes via lut. Values that
  # don't appear in the lut are dropped (or labelled "??") -- we drop.
  vals_int <- as.integer(round(as.numeric(vals)))
  rsg_chr  <- unname(lut[as.character(vals_int)])
  rsg_chr  <- rsg_chr[!is.na(rsg_chr)]

  if (length(rsg_chr) == 0L) {
    return(data.table::data.table(
      rsg_code    = character(),
      probability = numeric()
    ))
  }

  freq <- table(rsg_chr)
  prior <- data.table::data.table(
    rsg_code    = names(freq),
    probability = as.numeric(freq) / sum(as.numeric(freq))
  )
  prior <- prior[order(-prior$probability), ]
  if (is.finite(n_classes_top) && nrow(prior) > n_classes_top) {
    prior <- prior[seq_len(n_classes_top), ]
  }
  normalize_prior(prior)
}


#' Build a metric-buffered SpatVector around a pedon's site
#'
#' Internal: prefers \code{sf} for the geographic-to-UTM transform if
#' available; otherwise uses terra's own projection machinery. The
#' returned SpatVector is in lon/lat (EPSG:4326) so it can be passed
#' to terra::extract regardless of the raster CRS.
#'
#' @keywords internal
#' @param pedon A \code{\link{PedonRecord}}.
soilgrids_buffer_vect <- function(pedon, buffer_m = 250) {
  lon <- pedon$site$lon
  lat <- pedon$site$lat
  src_crs <- pedon$site$crs %||% 4326

  utm_crs <- utm_crs_for_point(lon = lon, lat = lat)

  if (requireNamespace("sf", quietly = TRUE)) {
    pt    <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = src_crs)
    pt_m  <- sf::st_transform(pt, utm_crs)
    buf_m <- sf::st_buffer(pt_m, dist = buffer_m)
    buf_g <- sf::st_transform(buf_m, 4326)
    return(terra::vect(buf_g))
  }

  # terra-only fallback
  pt    <- terra::vect(matrix(c(lon, lat), ncol = 2),
                        type = "points",
                        crs  = paste0("EPSG:", src_crs))
  pt_m  <- terra::project(pt, paste0("EPSG:", utm_crs))
  buf_m <- terra::buffer(pt_m, width = buffer_m)
  terra::project(buf_m, "EPSG:4326")
}


#' UTM zone EPSG code for a lon/lat point
#'
#' Picks the appropriate WGS84 UTM zone (32601..32660 northern,
#' 32701..32760 southern) for a single coordinate. Used for metric
#' buffering.
#'
#' @keywords internal
utm_crs_for_point <- function(lon, lat) {
  zone <- floor((lon + 180) / 6) + 1
  zone <- max(1L, min(60L, as.integer(zone)))
  if (lat >= 0) 32600L + zone else 32700L + zone
}


#' SoilGrids -> WRB code lookup table
#'
#' Maps the integer raster values used by the SoilGrids 2.0
#' "MostProbable WRB" layer to soilKey's two-letter RSG codes (the
#' codes used in \code{inst/rules/wrb2022/key.yaml}).
#'
#' The numeric values follow the order used by ISRIC; users with a
#' different convention can override this via the \code{lut} argument
#' to \code{\link{spatial_prior_soilgrids}}.
#'
#' @return Named character vector: names are integer-as-character
#'         (\code{"1"}, \code{"2"}, ...), values are RSG codes.
#' @export
soilgrids_wrb_lut <- function() {
  c(
    "1"  = "AC",  # Acrisols
    "2"  = "AL",  # Alisols
    "3"  = "AN",  # Andosols
    "4"  = "AR",  # Arenosols
    "5"  = "CL",  # Calcisols
    "6"  = "CM",  # Cambisols
    "7"  = "CH",  # Chernozems
    "8"  = "CR",  # Cryosols
    "9"  = "DU",  # Durisols
    "10" = "FR",  # Ferralsols
    "11" = "FL",  # Fluvisols
    "12" = "GL",  # Gleysols
    "13" = "GY",  # Gypsisols
    "14" = "HS",  # Histosols
    "15" = "KS",  # Kastanozems
    "16" = "LP",  # Leptosols
    "17" = "LX",  # Lixisols
    "18" = "LV",  # Luvisols
    "19" = "NT",  # Nitisols
    "20" = "PH",  # Phaeozems
    "21" = "PL",  # Planosols
    "22" = "PT",  # Plinthosols
    "23" = "PZ",  # Podzols
    "24" = "RG",  # Regosols
    "25" = "RT",  # Retisols
    "26" = "SC",  # Solonchaks
    "27" = "SN",  # Solonetz
    "28" = "ST",  # Stagnosols
    "29" = "TC",  # Technosols
    "30" = "UM",  # Umbrisols
    "31" = "VR",  # Vertisols
    "32" = "AT"   # Anthrosols
  )
}


#' SoilGrids -> USDA Soil Order lookup table (placeholder)
#'
#' Reserved for the future SoilGrids USDA layer. Currently returns the
#' 12 USDA Order codes mapped to integers 1..12.
#'
#' @return Named character vector.
#' @export
soilgrids_usda_lut <- function() {
  c(
    "1"  = "GE",  # Gelisols
    "2"  = "HI",  # Histosols
    "3"  = "SP",  # Spodosols
    "4"  = "AN",  # Andisols
    "5"  = "OX",  # Oxisols
    "6"  = "VE",  # Vertisols
    "7"  = "AR",  # Aridisols
    "8"  = "UL",  # Ultisols
    "9"  = "MO",  # Mollisols
    "10" = "AL",  # Alfisols
    "11" = "IN",  # Inceptisols
    "12" = "EN"   # Entisols
  )
}
