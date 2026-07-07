# Reproducible generator for the bundled SoilGrids WRB demo raster used by the
# Pro app's Map tab (offline "Demo" overlay source). The previous demo raster
# was ~14 km/pixel and near-uniform (FR/CM) over SE Brazil, so any small-area
# "predict"/prior collapsed to 1-2 classes. This one is ~4.4 km/pixel with
# spatially-coherent BUT locally-diverse WRB classes over a Brazil AOI, so a
# realistic map view samples several classes and the demo looks meaningful.
#
# Integer codes follow soilgrids_wrb_lut() (the demo/offline path uses the
# numeric LUT; the live ISRIC raster is read via its embedded category labels).
suppressMessages(library(terra))
try(soilKey::auto_set_proj_env(), silent = TRUE)

out <- "inst/shiny/classify_app_pro/www/soilgrids_wrb_demo.tif"
ext <- terra::ext(-55, -38, -30, -5)          # central/SE Brazil, incl. Rio
res <- 0.04                                    # ~4.4 km/pixel (finer than 0.125)
r   <- terra::rast(ext, resolution = res, crs = "EPSG:4326")

xy  <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
lon <- xy[, 1]; lat <- xy[, 2]

# Soil-forming-factor-like fields: coastal proximity, latitude, and two smooth
# sinusoidal "landform" fields give coherent regions with local variation.
coast <- pmin(1, pmax(0, (lon + 55) / 17))                    # 0 inland .. 1 east
band  <- (lat + 30) / 25                                       # 0 south .. 1 north
f1 <- sin(lon * 0.9) * cos(lat * 1.1)
f2 <- sin((lon + lat) * 0.6) * cos((lon - lat) * 0.5)
score <- 0.45 * coast + 0.30 * band + 0.15 * f1 + 0.10 * f2

# 10 WRB classes (codes per soilgrids_wrb_lut): FR AC LV NT CM AR GL PT PL LP
codes <- c(FR = 10, AC = 1, LV = 18, NT = 19, CM = 6,
           AR = 4, GL = 12, PT = 22, PL = 21, LP = 16)
# bin the score into the classes, then add fine-grained local swaps so a small
# window is not monochrome (deterministic: keyed on cell index parity + fields)
qs  <- stats::quantile(score, probs = seq(0, 1, length.out = length(codes) + 1))
bin <- cut(score, breaks = qs, labels = FALSE, include.lowest = TRUE)
val <- codes[bin]
# local diversity: flip ~18% of cells to a neighbouring class by a checker field
flip <- (sin(lon * 7.3) + cos(lat * 6.7)) > 1.1
val[flip] <- codes[pmin(length(codes), pmax(1, bin[flip] + 1L))]
# a few coherent blobs of distinctive classes for visual interest
val[(lon + 44)^2 + (lat + 14)^2 < 3] <- codes["PT"]   # Plinthosols blob
val[(lon + 50)^2 + (lat + 20)^2 < 2] <- codes["GL"]   # Gleysols blob

terra::values(r) <- as.integer(val)
terra::writeRaster(r, out, overwrite = TRUE, datatype = "INT2U",
                   gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2"))

info <- terra::rast(out)
cat("wrote", out, "-", round(file.info(out)$size / 1024), "KB\n")
cat("dims", terra::nrow(info), "x", terra::ncol(info),
    "res", round(terra::res(info)[1], 3), "deg\n")
tb <- sort(table(terra::values(info)), decreasing = TRUE)
lut <- soilKey::soilgrids_wrb_lut()
cat("classes:", paste(sprintf("%s(%d)", lut[names(tb)], tb), collapse = " "), "\n")
# what a ~1-deg window around Rio (-43.7,-22.5) contains:
win <- terra::crop(info, terra::ext(-44.7, -42.7, -23.5, -21.5))
cat("Rio +-1deg classes:", paste(sort(unique(lut[as.character(terra::values(win))])), collapse = ","), "\n")
