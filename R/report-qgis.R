# =============================================================================
# soilKey -- Export classification results as a GeoPackage for QGIS.
#
# `report_to_qgis(pedon, classifications, file)` writes a single
# GeoPackage with three layers:
#
#   1. `pedon_point`     -- POINT geometry at the profile coordinates
#                           with attribute columns: site id, country,
#                           lat / lon / crs, the three classification
#                           names, RSG / Ordem / Order codes, the
#                           evidence grade per system, the principal
#                           qualifiers, the WRB Ch 6 supplementary
#                           tag list (concatenated), and a hyperlink
#                           to the rendered HTML report.
#   2. `horizons_table`  -- attribute-only layer (no geometry) with
#                           the canonical horizons schema columns,
#                           one row per horizon. QGIS users join this
#                           to `pedon_point` by `site_id`.
#   3. `provenance_log`  -- attribute-only layer with the per-
#                           (horizon, attribute, source) provenance
#                           rows. Lets a downstream user audit which
#                           values drove the classification.
#
# The output is consumed natively by QGIS (Layer -> Add Vector
# Layer -> .gpkg). PostGIS / DuckDB / GDAL / sf all read it. This
# closes the v0.9.14 promise of "produce a deliverable a pedologist
# can drop into a soil-survey GIS without writing any code".
# =============================================================================


#' Export a classification result + pedon to a QGIS GeoPackage
#'
#' Writes a single GeoPackage (\code{.gpkg}) that QGIS reads
#' natively, containing one POINT layer (the profile location with
#' all classification metadata as attributes) plus two attribute-only
#' tables (the horizons schema and the provenance log). Lets a
#' pedologist overlay the soilKey result on a soil-survey base map
#' or join it with field-campaign vector data without writing R or
#' SQL.
#'
#' @section Geometry handling:
#' The point geometry uses the pedon's site CRS
#' (\code{pedon$site$crs}, default EPSG:4326). When the site has no
#' coordinates, the function still writes the two attribute tables
#' but skips the point layer and emits a warning.
#'
#' @section Layer schema:
#' \describe{
#'   \item{\code{pedon_point}}{site_id, country, year, lat, lon,
#'         crs, wrb_name, wrb_rsg, wrb_grade, wrb_principal,
#'         wrb_supplementary, sibcs_name, sibcs_ordem, sibcs_grade,
#'         usda_name, usda_order, usda_grade, n_horizons,
#'         report_html (relative path), generated_at.}
#'   \item{\code{horizons_table}}{site_id, horizon_idx, top_cm,
#'         bottom_cm, designation, plus the canonical
#'         \code{horizon_column_spec()} attributes when present.}
#'   \item{\code{provenance_log}}{site_id, horizon_idx, attribute,
#'         source, confidence, notes.}
#' }
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param classifications A list of one to three
#'        \code{\link{ClassificationResult}} objects, named
#'        \code{wrb} / \code{sibcs} / \code{usda}. Pass the output of
#'        \code{\link{classify_from_documents}} verbatim, or build
#'        the list manually.
#' @param file Output path (\code{.gpkg}). Created with parents.
#' @param report_html Optional path to a sibling HTML report
#'        (rendered via \code{\link{report_html}}) -- stored in the
#'        \code{report_html} attribute of \code{pedon_point} so
#'        QGIS users can launch the report from the feature pop-up.
#' @param overwrite If \code{TRUE} (default), an existing
#'        \code{file} is replaced; otherwise an error is thrown.
#' @return The output \code{file} path, invisibly. Side-effect:
#'         writes a multi-layer GeoPackage.
#'
#' @examples
#' \dontrun{
#' pedon <- make_ferralsol_canonical()
#' results <- list(
#'   wrb   = classify_wrb2022(pedon, on_missing = "silent"),
#'   sibcs = classify_sibcs(pedon, include_familia = TRUE),
#'   usda  = classify_usda(pedon)
#' )
#' report_to_qgis(pedon, results,
#'                file        = "perfil_042.gpkg",
#'                report_html = "perfil_042.html")
#' # In QGIS: Layer -> Add Layer -> Add Vector Layer -> perfil_042.gpkg
#' }
#' @seealso \code{\link{report}} for HTML / PDF reports;
#'          \code{\link{classify_from_documents}} for the high-level
#'          one-liner that produces compatible \code{classifications}.
#' @export
report_to_qgis <- function(pedon,
                             classifications,
                             file,
                             report_html = NULL,
                             overwrite   = TRUE) {
  if (!inherits(pedon, "PedonRecord"))
    stop("`pedon` must be a PedonRecord.")
  if (!is.list(classifications))
    stop("`classifications` must be a named list of ClassificationResult.")
  if (missing(file) || !is.character(file) || !nzchar(file))
    stop("`file` must be a non-empty path ending in .gpkg.")
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Package 'sf' is required for report_to_qgis(). ",
         "Install with install.packages('sf').")

  ext <- tolower(tools::file_ext(file))
  if (!ext %in% c("gpkg")) {
    stop("`file` must end in .gpkg (GeoPackage). Got: .", ext)
  }
  dir.create(dirname(normalizePath(file, mustWork = FALSE)),
             recursive = TRUE, showWarnings = FALSE)
  if (file.exists(file)) {
    if (isTRUE(overwrite)) unlink(file, force = TRUE)
    else stop(sprintf("`%s` already exists; pass overwrite = TRUE to replace.",
                         file))
  }

  # ---- Build pedon_point row --------------------------------------------
  point_row <- .build_pedon_point_row(pedon, classifications,
                                         report_html)
  has_geom <- !is.null(point_row$lat) && !is.null(point_row$lon) &&
    !is.na(point_row$lat) && !is.na(point_row$lon)

  if (has_geom) {
    crs <- pedon$site$crs %||% 4326
    pt <- sf::st_as_sf(
      data.frame(point_row, stringsAsFactors = FALSE),
      coords = c("lon", "lat"),
      crs    = crs,
      remove = FALSE
    )
    sf::st_write(pt, file, layer = "pedon_point",
                  delete_dsn = FALSE, quiet = TRUE)
  } else {
    warning("Pedon has no (lat, lon); writing attribute-only layers (",
              "no `pedon_point` geometry layer).", call. = FALSE)
    # Use the point_row as a flat data frame in the GPKG.
    df <- as.data.frame(point_row, stringsAsFactors = FALSE)
    sf::st_write(df, file, layer = "pedon_point_attributes",
                  delete_dsn = FALSE, quiet = TRUE)
  }

  # ---- horizons_table ---------------------------------------------------
  if (!is.null(pedon$horizons) && nrow(pedon$horizons) > 0L) {
    h <- as.data.frame(pedon$horizons)
    h$site_id     <- pedon$site$id %||% NA_character_
    h$horizon_idx <- seq_len(nrow(h))
    # Move site_id + horizon_idx to the front for QGIS readability.
    h <- h[, unique(c("site_id", "horizon_idx",
                        setdiff(names(h), c("site_id", "horizon_idx"))))]
    sf::st_write(h, file, layer = "horizons_table",
                  delete_dsn = FALSE, quiet = TRUE)
  }

  # ---- provenance_log ----------------------------------------------------
  if (!is.null(pedon$provenance) && nrow(pedon$provenance) > 0L) {
    pv <- as.data.frame(pedon$provenance)
    pv$site_id <- pedon$site$id %||% NA_character_
    pv <- pv[, unique(c("site_id", setdiff(names(pv), "site_id")))]
    sf::st_write(pv, file, layer = "provenance_log",
                  delete_dsn = FALSE, quiet = TRUE)
  }

  invisible(file)
}


# ---- internals -------------------------------------------------------------


#' Build a single-row tibble describing the profile + classifications
#' for the GPKG `pedon_point` layer.
#' @keywords internal
.build_pedon_point_row <- function(pedon, classifications, report_html) {
  s <- pedon$site %||% list()
  wrb   <- classifications$wrb
  sibcs <- classifications$sibcs
  usda  <- classifications$usda

  qual_join <- function(qx) {
    if (is.null(qx) || length(qx) == 0L) return(NA_character_)
    paste(qx, collapse = ", ")
  }

  list(
    site_id  = s$id %||% NA_character_,
    country  = s$country %||% NA_character_,
    year     = s$year %||% NA_integer_,
    lat      = s$lat %||% NA_real_,
    lon      = s$lon %||% NA_real_,
    crs      = s$crs %||% 4326,

    wrb_name    = if (!is.null(wrb))   wrb$name           else NA_character_,
    wrb_rsg     = if (!is.null(wrb))   wrb$rsg_or_order   else NA_character_,
    wrb_grade   = if (!is.null(wrb))   wrb$evidence_grade else NA_character_,
    wrb_principal     = if (!is.null(wrb))
                          qual_join(wrb$qualifiers$principal)     else NA_character_,
    wrb_supplementary = if (!is.null(wrb))
                          qual_join(wrb$qualifiers$supplementary) else NA_character_,

    sibcs_name  = if (!is.null(sibcs)) sibcs$name           else NA_character_,
    sibcs_ordem = if (!is.null(sibcs)) sibcs$rsg_or_order   else NA_character_,
    sibcs_grade = if (!is.null(sibcs)) sibcs$evidence_grade else NA_character_,

    usda_name   = if (!is.null(usda))  usda$name            else NA_character_,
    usda_order  = if (!is.null(usda))  usda$rsg_or_order    else NA_character_,
    usda_grade  = if (!is.null(usda))  usda$evidence_grade  else NA_character_,

    n_horizons   = if (!is.null(pedon$horizons))
                     nrow(pedon$horizons) else 0L,
    report_html  = report_html %||% NA_character_,
    generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  )
}
