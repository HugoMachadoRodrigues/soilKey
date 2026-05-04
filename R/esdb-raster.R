# =============================================================================
# v0.9.44 -- ESDB Raster Library lookup utility.
#
# The European Soil Database (ESDB) Raster Library distributes 71 thematic
# rasters at 1 km resolution covering EU+EurAsia under LAEA Europe (EPSG:
# 3035). Each attribute is a folder with a `.tif` (the raster), `.vat.dbf`
# (the Value Attribute Table mapping integer raster values to coded
# strings), and `.txt` (the human-readable legend).
#
# Most useful for soilKey users:
#   * WRBLV1   -- WRB Reference Soil Group (RSG)        [Level 1, 23 codes]
#   * WRBFU    -- WRB full classification (RSG + qualifier)
#   * WRBADJ1  -- WRB qualifier 1 (e.g. "Calcic", "Eutric")
#   * FAO90LV1 -- FAO 1990 Major Group (cross-system check)
#
# Plus thematic / morphological rasters: clay/sand/silt fraction sub +
# topsoil, OC sub + topsoil, parent material, slope, depth-to-rock, etc.
#
# This module exports:
#   * lookup_esdb(coords, attribute, raster_root)
#       Given WGS84 lat/lon, return the value(s) at those coords.
#   * available_esdb_attributes(raster_root)
#       List the 71 attribute names found at a given raster root.
# =============================================================================


#' List ESDB Raster Library attributes available at a given root
#'
#' Walks `raster_root` and returns the folder names that contain a
#' valid `<NAME>.tif` raster. Useful for discovery before calling
#' \code{\link{lookup_esdb}}.
#'
#' @param raster_root Path to the unpacked ESDB raster directory
#'        (typically `<some>/ESDB-Raster-Library-1k-GeoTIFF-...`).
#' @return A character vector of attribute names (sorted).
#' @examples
#' \dontrun{
#' available_esdb_attributes("~/data/ESDB-Raster-Library-1k-GeoTIFF-20240507")
#' #> [1] "AGLI1NNI" "AGLI2NNI" "AGLIM1" "AGLIM2" "ALT" "ATC" "AWC_SUB" ...
#' #>     [continued: 71 attributes]
#' }
#' @export
available_esdb_attributes <- function(raster_root) {
  if (!dir.exists(raster_root))
    stop(sprintf("ESDB raster root does not exist: %s", raster_root))
  subdirs <- list.dirs(raster_root, recursive = FALSE, full.names = FALSE)
  has_tif <- vapply(subdirs, function(d) {
    f <- file.path(raster_root, d, paste0(d, ".tif"))
    file.exists(f)
  }, logical(1))
  sort(subdirs[has_tif])
}


#' Look up an ESDB raster value at WGS84 coordinates
#'
#' Loads the requested attribute raster, reprojects WGS84 lat/lon
#' input to the raster's native CRS (typically LAEA Europe,
#' EPSG:3035), and extracts the value(s). When a Value Attribute
#' Table (`.vat.dbf`) is available, the integer raster value is
#' decoded to its coded string (e.g. `21` -> `"LV"` -> Luvisol).
#'
#' Coordinates outside the European raster footprint return `NA`
#' silently (rather than erroring) so vectorised calls degrade
#' gracefully.
#'
#' @param coords A two-column matrix or data.frame with `lon` and
#'        `lat` (WGS84 decimal degrees) -- in that order. A single
#'        \code{c(lon, lat)} vector is also accepted.
#' @param attribute Name of the ESDB attribute folder, e.g.
#'        \code{"WRBLV1"} or \code{"WRBFU"}. See
#'        \code{\link{available_esdb_attributes}}.
#' @param raster_root Path to the unpacked ESDB raster directory.
#' @param decode If \code{TRUE} (default), decode the integer raster
#'        value to the VAT-coded string (e.g. \code{"21"} ->
#'        \code{"LV"}). If \code{FALSE}, return the raw integer.
#' @return Character vector (decoded codes) or numeric vector (raw
#'         values) of the same length as \code{nrow(coords)}.
#'         \code{NA} for points outside the raster footprint.
#' @examples
#' \dontrun{
#' root <- "~/data/ESDB-Raster-Library-1k-GeoTIFF-20240507"
#'
#' # Single point: Wageningen, Netherlands (5.66 E, 51.97 N)
#' lookup_esdb(c(5.66, 51.97), "WRBLV1", root)
#' #> [1] "GL"   # Gleysol per the ESDB 1km raster
#'
#' # Vector: Lisbon + Berlin + Helsinki
#' coords <- rbind(c(-9.14, 38.72), c(13.40, 52.52), c(24.94, 60.17))
#' lookup_esdb(coords, "WRBLV1", root)
#' #> [1] "CM" "LV" "PZ"   # Cambisol, Luvisol, Podzol
#' }
#' @seealso \code{\link{available_esdb_attributes}}
#' @export
lookup_esdb <- function(coords, attribute, raster_root, decode = TRUE) {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Package 'terra' is required for lookup_esdb().")
  if (!dir.exists(raster_root))
    stop(sprintf("ESDB raster root does not exist: %s", raster_root))

  # Coerce coords to a 2-column matrix.
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

  # Locate the raster.
  tif_path <- file.path(raster_root, attribute, paste0(attribute, ".tif"))
  if (!file.exists(tif_path))
    stop(sprintf("Raster not found: %s", tif_path))

  r <- terra::rast(tif_path)

  # Build a SpatVector in WGS84 and project to the raster's CRS.
  pts <- terra::vect(coords, type = "points",
                       crs = "EPSG:4326")
  pts_proj <- terra::project(pts, terra::crs(r))

  vals <- terra::extract(r, pts_proj)[[2]]   # second column = the raster
  vals <- as.numeric(vals)

  if (!isTRUE(decode)) return(vals)

  # Try to decode via VAT.
  vat_path <- file.path(raster_root, attribute, paste0(attribute, ".vat.dbf"))
  if (!file.exists(vat_path)) {
    warning(sprintf("VAT not found for %s -- returning raw integer values",
                      attribute))
    return(vals)
  }
  if (!requireNamespace("foreign", quietly = TRUE))
    stop("Package 'foreign' is required to decode the VAT.")
  vat <- foreign::read.dbf(vat_path, as.is = TRUE)
  # The .vat.dbf typically has columns Value + something (often the
  # 3rd column carries the coded label; column 2 is the count).
  if (ncol(vat) < 2L) {
    warning("VAT has fewer than 2 columns -- returning raw integer values")
    return(vals)
  }
  # Find the value column (integer) and the label column (string).
  val_col <- which(vapply(vat, is.numeric, logical(1)))[1]
  if (is.na(val_col)) {
    warning("Could not identify Value column in VAT")
    return(vals)
  }
  # Pick the LAST non-numeric column as the label (count is usually first
  # numeric after Value; the actual coded string is the last char column).
  char_cols <- which(vapply(vat, is.character, logical(1)))
  if (length(char_cols) == 0L) {
    warning("VAT has no character column to decode against")
    return(vals)
  }
  lab_col <- char_cols[length(char_cols)]

  match_idx <- match(vals, vat[[val_col]])
  decoded <- vat[[lab_col]][match_idx]
  decoded[is.na(vals)] <- NA_character_
  decoded
}
