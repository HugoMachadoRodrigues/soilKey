# ================================================================
# Module 3 -- Embrapa national raster backend (Brazil)
#
# Embrapa publishes "Mapa de Solos do Brasil" (1:5.000.000) -- a
# polygon map of SiBCS soil orders / suborders covering the whole
# country. For soilKey we expect the user to have rasterised that
# polygon layer (e.g. with QGIS / gdal_rasterize) at ~250 m resolution
# and stored it locally; v0.5 does not embed the file.
#
# Download / styling guidance:
#   https://www.embrapa.br/solos/sibcs
#   https://www.embrapa.br/solos/sibcs
#
# Future work (v0.6+):
#   - direct WFS query of the polygon layer
#   - distribution of legend code -> SiBCS-5 RSG mapping as an
#     internal package data object
# ================================================================


#' Embrapa national soil-class spatial prior (Brazil only)
#'
#' v0.5 stub. Reads a user-provided categorical raster of SiBCS orders
#' / suborders, buffers the pedon's site, tallies pixel classes, and
#' returns a probability distribution over SiBCS codes (or, with a
#' user-provided LUT, over WRB equivalents).
#'
#' Unlike SoilGrids, Embrapa does not publish per-pixel probabilities,
#' so the empirical frequency over a neighbourhood window (default 15
#' x 15 cells = ~3.75 km radius at 250 m resolution) is used as an
#' approximation.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param raster_path Required. Path to a local categorical raster
#'        (GeoTIFF) of Embrapa SiBCS classes. There is no built-in
#'        file in v0.5 -- download the polygon map from
#'        \url{https://www.embrapa.br/solos/sibcs} and rasterise it.
#' @param buffer_m Buffer radius in metres (default 3750, i.e.
#'        ~15-cell neighbourhood at 250 m resolution).
#' @param lut Optional named character vector mapping raster integer
#'        values to soil-class codes. If NULL, raster categories are
#'        used as-is (terra::levels).
#' @param n_classes_top Keep only the top N classes (default 10).
#' @param ... Reserved.
#' @return A \code{data.table} with columns \code{rsg_code},
#'         \code{probability}.
#' @export
spatial_prior_embrapa <- function(pedon,
                                    raster_path   = NULL,
                                    buffer_m      = 3750,
                                    lut           = NULL,
                                    n_classes_top = 10,
                                    ...) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    rlang::abort(
      "Package 'terra' is required for spatial_prior_embrapa() -- install with install.packages('terra')"
    )
  }

  if (is.null(raster_path) || !nzchar(raster_path)) {
    raster_path <- getOption("soilKey.embrapa_raster", default = NULL)
  }
  if (is.null(raster_path) || !file.exists(raster_path)) {
    rlang::abort(paste0(
      "Embrapa raster not found. v0.5 requires a local raster path. ",
      "Pass raster_path= or set options(soilKey.embrapa_raster = '...'). ",
      "Download instructions: https://www.embrapa.br/solos/sibcs"
    ))
  }

  rst <- terra::rast(raster_path)
  buf <- soilgrids_buffer_vect(pedon, buffer_m = buffer_m)

  ex <- terra::extract(rst, buf, touches = TRUE)
  vals <- ex[[ncol(ex)]]
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) {
    return(data.table::data.table(
      rsg_code    = character(),
      probability = numeric()
    ))
  }

  if (!is.null(lut)) {
    vals_int <- as.integer(round(as.numeric(vals)))
    rsg_chr  <- unname(lut[as.character(vals_int)])
    rsg_chr  <- rsg_chr[!is.na(rsg_chr)]
  } else if (is.factor(vals) || is.character(vals)) {
    rsg_chr <- as.character(vals)
  } else {
    # Use terra's own category table if the raster carries one.
    levs <- tryCatch(terra::levels(rst)[[1]], error = function(e) NULL)
    if (!is.null(levs) && is.data.frame(levs) && nrow(levs) > 0L) {
      vals_int <- as.integer(round(as.numeric(vals)))
      idx <- match(vals_int, levs[[1]])
      rsg_chr <- as.character(levs[[2]][idx])
      rsg_chr <- rsg_chr[!is.na(rsg_chr)]
    } else {
      rsg_chr <- as.character(as.integer(round(as.numeric(vals))))
    }
  }

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
