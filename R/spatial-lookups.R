# =============================================================================
# v0.9.48 -- spatial lookup utilities (Brazil + global).
#
# Two new validation axes for soilKey, complementing the ESDB raster
# axis added in v0.9.44:
#
#   * lookup_mapbiomas_solos()
#       Brazilian national raster of SiBCS classes (MapBiomas Solos
#       Collection 2, 2023+, 30m). Local-file lookup pattern, mirroring
#       lookup_esdb(). CRS-agnostic (auto-reprojection from WGS84).
#
#   * lookup_soilgrids()
#       Global continuous soil property predictions (ISRIC SoilGrids
#       250m, Hengl et al. 2017, 2021). Remote Cloud-Optimized GeoTIFF
#       (COG) reads via terra::rast("/vsicurl/...") -- no download
#       required; only the pixel under each query coordinate is
#       transferred.
#
# Both functions return a numeric / character vector of length
# nrow(coords) so they can drive validation axes uniformly.
# =============================================================================


# ---- Brazil: MapBiomas Solos (Collection 2, 2023+) ----------------------

#' Look up a MapBiomas Solos raster value at WGS84 coordinates
#'
#' MapBiomas Solos (Project MapBiomas, Brazil) distributes a national
#' raster of SiBCS classes at 30 m, downloadable from
#' \url{https://mapbiomas.org/en/produtos}. This helper mirrors the
#' shape of \code{\link{lookup_esdb}} but is local-file only: pass
#' the path of the unpacked GeoTIFF and the function reprojects the
#' user's WGS84 lat/lon to the raster's native CRS, extracts the
#' pixel and (optionally) decodes the integer class code via a
#' user-supplied legend.
#'
#' MapBiomas does not bundle a `.vat.dbf`; the canonical legend is
#' published as a CSV / dictionary on their website. Pass it via
#' \code{legend} as a two-column data.frame
#' (\code{value, class_name}) to enable decoding.
#'
#' @param coords A 2-column matrix or data.frame with \code{lon},
#'        \code{lat} (WGS84 decimal degrees), or a length-2 numeric
#'        vector for a single query.
#' @param raster_path Path to the unpacked MapBiomas Solos GeoTIFF.
#' @param legend Optional two-column data.frame
#'        (first column = numeric value, second = SiBCS class name).
#'        When provided, the integer raster value is decoded; when
#'        \code{NULL}, the raw integer is returned.
#' @return Character vector of decoded class names (when
#'         \code{legend} is supplied) or numeric vector of raster
#'         values. Same length as \code{nrow(coords)}. \code{NA}
#'         for points outside the raster footprint.
#' @examples
#' \dontrun{
#' tif <- "~/data/mapbiomas_solos_collection2_2023.tif"
#' legend <- data.frame(
#'   value = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L),
#'   class_name = c("Latossolo Vermelho-Amarelo",
#'                    "Latossolo Amarelo",
#'                    "Argissolo Vermelho-Amarelo",
#'                    "Argissolo Amarelo",
#'                    "Neossolo Quartzarenico",
#'                    "Cambissolo Haplico",
#'                    "Espodossolo",
#'                    "Gleissolo",
#'                    "Nitossolo",
#'                    "Planossolo",
#'                    "Plintossolo",
#'                    "Vertisolo",
#'                    "Outros")
#' )
#' lookup_mapbiomas_solos(c(-43.0, -22.0), tif, legend)
#' }
#' @seealso \code{\link{lookup_esdb}}, \code{\link{lookup_soilgrids}}.
#' @export
lookup_mapbiomas_solos <- function(coords, raster_path, legend = NULL) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for lookup_mapbiomas_solos().")
  if (!file.exists(raster_path))
    stop(sprintf("MapBiomas Solos raster not found: %s", raster_path))

  coords <- .coerce_lonlat(coords)

  r <- terra::rast(raster_path)
  pts <- terra::vect(coords, type = "points", crs = "EPSG:4326")
  pts_proj <- terra::project(pts, terra::crs(r))
  vals <- as.numeric(terra::extract(r, pts_proj)[[2]])

  if (is.null(legend)) return(vals)

  if (!is.data.frame(legend) || ncol(legend) < 2L) {
    stop("`legend` must be a two-column data.frame: value, class_name.")
  }
  val_col <- which(vapply(legend, is.numeric, logical(1)))[1]
  if (is.na(val_col)) {
    val_col <- 1L  # try the first column anyway
    legend[[1L]] <- as.numeric(legend[[1L]])
  }
  lab_col <- if (ncol(legend) >= 2L) 2L else val_col
  match_idx <- match(vals, legend[[val_col]])
  decoded <- as.character(legend[[lab_col]])[match_idx]
  decoded[is.na(vals)] <- NA_character_
  decoded
}


# ---- Global: ISRIC SoilGrids 250m via remote COG reads ------------------

#' Look up a SoilGrids 250m soil property at WGS84 coordinates
#'
#' Reads ISRIC SoilGrids 250m (Hengl et al. 2017, 2021) directly
#' from the ISRIC Cloud-Optimized GeoTIFF (COG) endpoint at
#' \url{https://files.isric.org/soilgrids/latest/data/} -- no
#' download required, only the pixel under each query coordinate is
#' transferred over HTTPS.
#'
#' SoilGrids stores integer rasters scaled per property; this helper
#' applies the canonical conversion factor so the returned value is
#' in conventional soil units (\%, pH, g/kg, cmol(c)/kg, g/cm^3).
#'
#' @param coords A 2-column matrix or data.frame with \code{lon},
#'        \code{lat} (WGS84 decimal degrees), or a length-2 numeric
#'        vector for a single query.
#' @param property One of the SoilGrids 250m predicted properties:
#'        \code{"clay"}, \code{"sand"}, \code{"silt"},
#'        \code{"phh2o"}, \code{"soc"}, \code{"cec"},
#'        \code{"bdod"}, \code{"nitrogen"}, \code{"ocd"},
#'        \code{"ocs"}, \code{"cfvo"}.
#' @param depth Depth interval. One of \code{"0-5cm"},
#'        \code{"5-15cm"}, \code{"15-30cm"}, \code{"30-60cm"},
#'        \code{"60-100cm"}, \code{"100-200cm"}.
#' @param quantile Output quantile. One of \code{"mean"} (default),
#'        \code{"Q0.05"}, \code{"Q0.5"}, \code{"Q0.95"},
#'        \code{"uncertainty"}.
#' @param baseurl Base URL of the SoilGrids COG endpoint. Default
#'        is the canonical ISRIC location; override only for a
#'        local mirror.
#' @param raw If \code{TRUE}, returns the integer raster value
#'        without scaling. Default \code{FALSE} (returns the value
#'        in conventional units).
#' @return Numeric vector of length \code{nrow(coords)}. \code{NA}
#'         outside the SoilGrids footprint or on network errors.
#'
#' @examples
#' \dontrun{
#' # Single point
#' lookup_soilgrids(c(-43.0, -22.0),
#'                   property = "phh2o",
#'                   depth = "0-5cm",
#'                   quantile = "mean")
#'
#' # Vector + multiple properties
#' coords <- rbind(c(-43.0, -22.0), c( -9.14, 38.72))
#' lookup_soilgrids(coords, "clay",  "0-5cm", "mean")
#' lookup_soilgrids(coords, "phh2o", "0-5cm", "mean")
#' }
#' @seealso \code{\link{lookup_esdb}},
#'          \code{\link{lookup_mapbiomas_solos}}.
#' @export
lookup_soilgrids <- function(coords,
                              property = c("clay", "sand", "silt",
                                            "phh2o", "soc", "cec",
                                            "bdod", "nitrogen",
                                            "ocd", "ocs", "cfvo"),
                              depth = c("0-5cm", "5-15cm", "15-30cm",
                                         "30-60cm", "60-100cm", "100-200cm"),
                              quantile = c("mean", "Q0.05", "Q0.5",
                                              "Q0.95", "uncertainty"),
                              baseurl = "https://files.isric.org/soilgrids/latest/data",
                              raw = FALSE) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for lookup_soilgrids().")
  property <- match.arg(property)
  depth    <- match.arg(depth)
  quantile <- match.arg(quantile)

  coords <- .coerce_lonlat(coords)

  url <- sprintf("/vsicurl/%s/%s/%s_%s_%s.vrt",
                  baseurl, property, property, depth, quantile)
  r <- tryCatch(terra::rast(url),
                  error = function(e) {
                    warning(sprintf(
                      "lookup_soilgrids(): could not open '%s': %s",
                      url, conditionMessage(e)
                    ))
                    NULL
                  })
  if (is.null(r)) return(rep(NA_real_, nrow(coords)))

  pts <- terra::vect(coords, type = "points", crs = "EPSG:4326")
  pts_proj <- terra::project(pts, terra::crs(r))
  vals <- as.numeric(terra::extract(r, pts_proj)[[2]])

  if (isTRUE(raw)) return(vals)

  scale <- .soilgrids_scale(property)
  vals * scale
}


#' Canonical SoilGrids 250m unit-conversion factor per property
#'
#' SoilGrids stores integer rasters; the published conversion factors
#' are documented in \emph{Hengl et al. (2017)} and the SoilGrids
#' README. This internal lookup table applies the right factor so
#' \code{\link{lookup_soilgrids}} returns conventional units.
#'
#' @param property One of the SoilGrids properties.
#' @return Numeric scalar. The native integer value times this scale
#'         yields the conventional unit:
#' \itemize{
#'   \item clay/sand/silt -- 0.1 (g/kg integer -> percent)
#'   \item phh2o          -- 0.1 (pH * 10 integer -> pH)
#'   \item soc            -- 0.1 (dg/kg integer -> g/kg)
#'   \item bdod           -- 0.01 (cg/cm^3 integer -> g/cm^3)
#'   \item cec            -- 0.1 (mmol(c)/kg integer -> cmol(c)/kg)
#'   \item nitrogen       -- 0.01 (cg/kg integer -> g/kg)
#'   \item ocd            -- 0.1 (hg/m^3 integer -> kg/m^3)
#'   \item ocs            -- 0.1 (hg/m^2 integer -> kg/m^2)
#'   \item cfvo           -- 0.1 (cm^3/dm^3 integer -> percent vol)
#' }
#' @keywords internal
.soilgrids_scale <- function(property) {
  switch(property,
    clay      = 0.1,
    sand      = 0.1,
    silt      = 0.1,
    phh2o     = 0.1,
    soc       = 0.1,
    cec       = 0.1,
    bdod      = 0.01,
    nitrogen  = 0.01,
    ocd       = 0.1,
    ocs       = 0.1,
    cfvo      = 0.1,
    1.0
  )
}


# ---- Internal: lat/lon coercion helper ---------------------------------

.coerce_lonlat <- function(coords) {
  if (is.numeric(coords) && length(coords) == 2L && is.null(dim(coords))) {
    coords <- matrix(coords, nrow = 1L)
  } else if (is.data.frame(coords)) {
    coords <- as.matrix(coords[, 1:2])
  } else if (!is.matrix(coords)) {
    stop("`coords` must be a length-2 numeric vector, a 2-col matrix, or a data.frame.")
  }
  if (ncol(coords) != 2L)
    stop("`coords` must have exactly 2 columns: lon, lat.")
  storage.mode(coords) <- "double"
  colnames(coords) <- c("lon", "lat")
  coords
}
