# Generate a 1200x630 Open Graph share card for soilKey Pro.
suppressPackageStartupMessages(library(png))
logo <- tryCatch(png::readPNG("inst/shiny/classify_app_pro/www/logo.png"),
                 error = function(e) NULL)
out <- "inst/shiny/classify_app_pro/www/og-preview.png"

png(out, width = 1200, height = 630, type = "quartz", bg = "#FAF6EF")
op <- par(mar = c(0, 0, 0, 0)); on.exit(par(op))
plot.new(); plot.window(xlim = c(0, 1200), ylim = c(0, 630), asp = NA)

# Warm cream background
rect(0, 0, 1200, 630, col = "#FAF6EF", border = NA)

# Right-hand soil-profile column (a pedon motif): reddish Ferralsol-like bands
colx0 <- 905; colx1 <- 1145
bands <- c("#5A3A22", "#7A3F28", "#9A4A2E", "#B5652E", "#A9673A", "#8B5E34")
ny <- length(bands); ytop <- 600; ybot <- 40; bh <- (ytop - ybot) / ny
for (i in seq_len(ny)) {
  y1 <- ytop - (i - 1) * bh
  rect(colx0, y1 - bh, colx1, y1, col = bands[i], border = NA)
}
# thin moss accent rule under the whole card
rect(0, 0, 1200, 12, col = "#5E7B3B", border = NA)

# Logo, top-left
if (!is.null(logo)) rasterImage(logo, 74, 466, 184, 576)

# Wordmark + tagline
text(74, 420, "soilKey Pro", col = "#4A3117", cex = 5.6, font = 2, adj = c(0, 0.5))
text(74, 342, "Transparent, deterministic soil classification",
     col = "#7A5230", cex = 2.7, adj = c(0, 0.5))
text(74, 278, "WRB 2022    ·    SiBCS 5    ·    USDA Soil Taxonomy",
     col = "#5E7B3B", cex = 2.4, font = 2, adj = c(0, 0.5))
text(74, 220, "Full key trace · provenance-aware evidence grade · never a black box",
     col = "#8A7A63", cex = 1.75, adj = c(0, 0.5))

# URL + channels
text(74, 118, "soilkeypro.com", col = "#B5652E", cex = 3.0, font = 2, adj = c(0, 0.5))
text(74, 66, "R package on CRAN + GitHub  ·  web app, no install needed",
     col = "#8A7A63", cex = 1.75, adj = c(0, 0.5))

invisible(dev.off())
cat("wrote", out, "\n")
