# =============================================================================
# soilKey -- self-contained report assets (v0.9.168): the app logo and a static
# locator map, both returned as base64 data: URIs so the HTML/PDF reports stay
# single-file with no external network requests.
# =============================================================================

# Base64 data: URI for a bundled binary asset, or "" if unavailable.
.report_asset_data_uri <- function(path, mime) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return("")
  if (!requireNamespace("base64enc", quietly = TRUE)) return("")
  paste0("data:", mime, ";base64,", base64enc::base64encode(path))
}

# The soilKey app logo as a data: URI (empty string if it cannot be found).
.report_logo_data_uri <- function() {
  p <- system.file("shiny", "classify_app_pro", "www", "logo.png",
                   package = "soilKey")
  .report_asset_data_uri(p, "image/png")
}

# Extract plottable points from one or more PedonRecords: a data frame with
# id, lat, lon (finite rows only).
.report_pedon_points <- function(pedons) {
  if (inherits(pedons, "PedonRecord")) pedons <- list(pedons)
  rows <- lapply(pedons, function(p) {
    if (is.null(p) || is.null(p$site)) return(NULL)
    lat <- suppressWarnings(as.numeric(p$site$lat %||% NA))
    lon <- suppressWarnings(as.numeric(p$site$lon %||% NA))
    data.frame(id = as.character(p$site$id %||% "pedon"),
               lat = lat, lon = lon, stringsAsFactors = FALSE)
  })
  df <- do.call(rbind, Filter(Negate(is.null), rows))
  if (is.null(df)) return(df)
  df[is.finite(df$lat) & is.finite(df$lon), , drop = FALSE]
}

# Draw a static locator map of the profile point(s) to `out_file` (a PNG path).
# Uses the 'maps' world coastline when available (Suggests); otherwise a
# labelled lat/lon panel. Returns TRUE on success, FALSE when there is no finite
# coordinate or the device cannot be opened.
.report_map_png <- function(pedons, out_file, width_px = 760L,
                            height_px = 430L) {
  pts <- .report_pedon_points(pedons)
  if (is.null(pts) || nrow(pts) == 0L) return(FALSE)

  latr <- range(pts$lat); lonr <- range(pts$lon)
  padx <- max(diff(lonr) * 0.4, 5); pady <- max(diff(latr) * 0.4, 5)
  xlim <- c(lonr[1] - padx, lonr[2] + padx)
  ylim <- c(latr[1] - pady, latr[2] + pady)
  xlim <- pmax(pmin(xlim, 180), -180)
  ylim <- pmax(pmin(ylim,  90),  -90)

  opened <- FALSE
  ok <- tryCatch({
    grDevices::png(out_file, width = width_px, height = height_px, res = 110)
    opened <- TRUE
    op <- graphics::par(mar = c(2.3, 2.6, 0.6, 0.6))
    asp <- 1 / cos(mean(ylim) * pi / 180)
    graphics::plot.new()
    graphics::plot.window(xlim = xlim, ylim = ylim, asp = asp)
    usr <- graphics::par("usr")                                 # actual bounds
    graphics::rect(usr[1], usr[3], usr[2], usr[4],
                   col = "#d7e6f0", border = NA)                 # ocean
    if (requireNamespace("maps", quietly = TRUE)) {
      tryCatch(
        maps::map("world", add = TRUE, fill = TRUE, resolution = 0,
                  col = "#ece4d2", border = "#c7bda6", lwd = 0.6),
        error = function(e) NULL)
    }
    graphics::abline(v = pretty(xlim), h = pretty(ylim),
                     col = "#ffffff90", lwd = 0.6)               # graticule
    graphics::axis(1, col = "#b9b0a1", col.axis = "#6b5c4d", cex.axis = 0.7,
                   tcl = -0.25, mgp = c(1.4, 0.35, 0))
    graphics::axis(2, col = "#b9b0a1", col.axis = "#6b5c4d", cex.axis = 0.7,
                   las = 1, tcl = -0.25, mgp = c(1.4, 0.45, 0))
    graphics::box(col = "#b9b0a1")
    graphics::points(pts$lon, pts$lat, pch = 21, bg = "#B5652E",
                     col = "#4A3226", cex = 1.5, lwd = 1.3)
    if (nrow(pts) <= 24L)
      graphics::text(pts$lon, pts$lat, labels = pts$id, pos = 3,
                     cex = 0.62, col = "#3a2a1e", offset = 0.45)
    graphics::par(op)
    TRUE
  }, error = function(e) FALSE)
  if (opened) grDevices::dev.off()                              # flush + close
  isTRUE(ok) && file.exists(out_file) && file.info(out_file)$size >= 100
}

# A static locator map as a base64 PNG data: URI (self-contained for HTML).
# Returns "" when no finite coordinate is available.
.report_map_data_uri <- function(pedons, width_px = 760L, height_px = 430L) {
  tf <- tempfile(fileext = ".png")
  if (!.report_map_png(pedons, tf, width_px, height_px)) {
    unlink(tf); return("")
  }
  uri <- .report_asset_data_uri(tf, "image/png")
  unlink(tf)
  uri
}
