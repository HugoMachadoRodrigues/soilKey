# Derive soil moisture and temperature regimes from climate alone, with fixed
# textbook rules (no fitting to the labels), for every pedon in the panel.
#
# Climate: PRISM 1991-2020 normals, 4 km, monthly tmean / ppt (CONUS),
#   from https://ftp.prism.oregonstate.edu/normals/us/4km/ .
# Temperature regime: MAST = MAAT + 1 C (NRCS rule of thumb); summer soil
#   temperature damped from air (0.6 x amplitude); cryic if MSST < 15 C.
# Moisture regime: Thornthwaite PET and an aridity index AI = P / PET;
#   aridic AI < 0.35; xeric if winter rain >= 3 x summer rain (Mediterranean
#   pattern) and AI < 1.2; ustic AI < 0.85; udic otherwise; perudic if P > PET
#   in every month. Aquic cannot come from climate and is left blank.
# A second moisture regime (smr_clim_hg) uses Hargreaves-Samani PET, which
#   unlike Thornthwaite does not underestimate PET in arid climates.
#
# Usage: Rscript 03_climate_regimes.R <cache_dir> <out_dir> <prism_tif_dir>

args <- commandArgs(trailingOnly = TRUE)
cache <- args[1]; out <- args[2]; PR <- args[3]
library(terra); library(data.table)

stk <- function(v) rast(sprintf("%s/prism_%s_us_25m_2020%02d_avg_30y.tif", PR, v, 1:12))
tmean <- stk("tmean"); prec <- stk("ppt"); tmin <- stk("tmin"); tmax <- stk("tmax")

pan <- readRDS(file.path(cache, "kssl_panel.rds"))
pts <- data.table(pedon_key = vapply(pan$nasis, function(p) p$site$id, ""),
                  lon = vapply(pan$nasis, function(p) p$site$lon, 0),
                  lat = vapply(pan$nasis, function(p) p$site$lat, 0))
v  <- project(vect(as.data.frame(pts), geom = c("lon", "lat"), crs = "EPSG:4326"), crs(tmean))
Tm <- as.matrix(extract(tmean, v, ID = FALSE))
P  <- as.matrix(extract(prec, v, ID = FALSE))
Tn <- as.matrix(extract(tmin, v, ID = FALSE)); Tx <- as.matrix(extract(tmax, v, ID = FALSE))

pet_harg <- function(T, tn, tx, lat) {
  phi <- lat * pi / 180
  d   <- 0.409 * sin(2 * pi * mid_doy / 365 - 1.39)
  dr  <- 1 + 0.033 * cos(2 * pi * mid_doy / 365)
  ws  <- acos(pmin(pmax(-tan(phi) * tan(d), -1), 1))
  Ra  <- 24 * 60 / pi * 0.082 * dr * (ws * sin(phi) * sin(d) + cos(phi) * cos(d) * sin(ws))
  pmax(0, 0.0023 * 0.408 * Ra * (T + 17.8) * sqrt(pmax(tx - tn, 0))) * ndays
}
smr_rule <- function(p, pet, lat) {
  ai <- sum(p) / max(sum(pet), 1)
  summer <- if (lat >= 0) c(6, 7, 8) else c(12, 1, 2)
  winter <- if (lat >= 0) c(12, 1, 2) else c(6, 7, 8)
  wet_winter <- sum(p[c(winter, if (lat >= 0) c(11, 3) else c(5, 9))]) / 5
  medit <- wet_winter >= 3 * max(mean(p[summer]), 1)
  smr <- if (ai < 0.35) "aridic" else if (medit && ai < 1.2) "xeric" else
         if (ai < 0.85) "ustic" else if (all(p > pet)) "perudic" else "udic"
  list(smr = smr, ai = ai)
}

# Thornthwaite PET with day-length correction
mid_doy <- c(15, 46, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
ndays   <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
daylen <- function(lat, doy) {
  d <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  ws <- acos(pmin(pmax(-tan(lat * pi / 180) * tan(d), -1), 1))
  24 * ws / pi
}
pet_thorn <- function(T, lat) {
  Tp <- pmax(T, 0)
  I <- sum((Tp / 5)^1.514)
  if (I <= 0) return(rep(0, 12))
  a <- 6.75e-7 * I^3 - 7.71e-5 * I^2 + 1.792e-2 * I + 0.49239
  L <- daylen(lat, mid_doy)
  16 * (10 * Tp / I)^a * (L / 12) * (ndays / 30)
}

res <- rbindlist(lapply(seq_len(nrow(pts)), function(i) {
  T <- Tm[i, ]; p <- P[i, ]; lat <- pts$lat[i]
  if (anyNA(T) || anyNA(p)) return(data.table(smr_clim = NA_character_, smr_clim_hg = NA_character_,
                                               str_clim = NA_character_, maat = NA_real_,
                                               ai = NA_real_, ai_hg = NA_real_, map = NA_real_))
  pet <- pet_thorn(T, lat)
  ai  <- sum(p) / max(sum(pet), 1)
  maat <- mean(T); mast <- maat + 1
  summer <- if (lat >= 0) c(6, 7, 8) else c(12, 1, 2)
  winter <- if (lat >= 0) c(12, 1, 2) else c(6, 7, 8)
  amp  <- mean(T[summer]) - mean(T[winter])
  msst <- mast + 0.6 * (mean(T[summer]) - maat)
  iso  <- 0.6 * amp < 6
  str <- if (mast < 0) "gelic" else if (mast < 8) {
           if (msst < 15) "cryic" else "frigid"
         } else if (mast < 15) "mesic" else if (mast < 22) "thermic" else "hyperthermic"
  if (iso && str %in% c("frigid", "mesic", "thermic", "hyperthermic"))
    str <- paste0("iso", str)
  wet_winter <- sum(p[c(winter, if (lat >= 0) c(11, 3) else c(5, 9))]) / 5
  dry_summer <- mean(p[summer])
  medit <- wet_winter >= 3 * max(dry_summer, 1)
  smr <- if (ai < 0.35) "aridic" else if (medit && ai < 1.2) "xeric" else
         if (ai < 0.85) "ustic" else if (all(p > pet)) "perudic" else "udic"
  hg <- smr_rule(p, pet_harg(T, Tn[i, ], Tx[i, ], lat), lat)
  data.table(smr_clim = smr, smr_clim_hg = hg$smr, str_clim = str, maat = maat,
             ai = ai, ai_hg = hg$ai, map = sum(p))
}))
clim <- cbind(pts, res)
fwrite(clim, file.path(out, "kssl_climate_regimes.csv"))
message("climate regimes: ", sum(!is.na(clim$smr_clim)), " of ", nrow(clim))
