# Build a random KSSL + NASIS validation panel for the "key ceiling" study.
# Output: <cache>/kssl_panel.rds  (list of PedonRecord + site metadata)
#
# Usage: Rscript 01_build_panel.R <cache_dir> [n_sample]

args  <- commandArgs(trailingOnly = TRUE)
cache <- args[1]
n_sam <- if (length(args) >= 2) as.integer(args[2]) else 3000L
dir.create(cache, showWarnings = FALSE, recursive = TRUE)

SK   <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/Packages/soilKey"
GPKG <- file.path(SK, "soil_data/KSSL/ncss_labdata.gpkg")
SQL  <- file.path(SK, "soil_data/KSSL/NASIS_Morphological_09142021.sqlite")
suppressMessages(pkgload::load_all(SK, quiet = TRUE))
library(data.table)

# ---- 1. all lab pedons (cached) --------------------------------------------
f_all <- file.path(cache, "kssl_lab_all.rds")
if (!file.exists(f_all)) {
  t0 <- Sys.time()
  all <- load_kssl_pedons_gpkg(GPKG, head = NULL, verbose = TRUE)
  all <- Filter(Negate(is.null), all)
  saveRDS(all, f_all, compress = FALSE)
  message("loaded ", length(all), " lab pedons in ", format(Sys.time() - t0))
} else all <- readRDS(f_all)

# ---- 2. site metadata from the combine table -------------------------------
meta <- as.data.table(DBI::dbGetQuery(
  DBI::dbConnect(RSQLite::SQLite(), GPKG),
  "SELECT pedon_key, peiid, samp_taxorder, samp_taxsuborder, samp_taxgrtgroup,
          samp_taxsubgrp, samp_taxmoistscl, samp_taxtempregime, samp_taxtempcl,
          mlra_key, state_key, samp_classdate
     FROM lab_combine_nasis_ncss"))
meta[, pedon_key := as.character(pedon_key)]

ids <- vapply(all, function(p) p$site$id, character(1))
has_coords <- vapply(all, function(p) isTRUE(is.finite(p$site$lat) & is.finite(p$site$lon)), logical(1))
in_conus <- vapply(all, function(p) {
  la <- p$site$lat; lo <- p$site$lon
  isTRUE(la > 24 & la < 50 & lo > -125 & lo < -66)
}, logical(1))
has_nasis <- ids %in% meta[!is.na(peiid), pedon_key]

pool <- which(has_coords & in_conus & has_nasis)
message("pool (CONUS, coords, linked to NASIS): ", length(pool), " of ", length(all))

# ---- 3. simple random sample (keeps the class distribution honest) ---------
set.seed(20261125)
pick <- sort(sample(pool, min(n_sam, length(pool))))
panel_lab <- all[pick]
rm(all); gc()

# ---- 4. enrich the sample with NASIS morphology ----------------------------
# load_kssl_pedons_with_nasis() calls load_kssl_pedons_gpkg() internally;
# temporarily make that call return our sample instead of re-reading the gpkg.
ns <- asNamespace("soilKey")
orig <- get("load_kssl_pedons_gpkg", envir = ns)
unlockBinding("load_kssl_pedons_gpkg", ns)
assign("load_kssl_pedons_gpkg", function(...) panel_lab, envir = ns)
panel <- load_kssl_pedons_with_nasis(GPKG, SQL, verbose = TRUE)
assign("load_kssl_pedons_gpkg", orig, envir = ns)

# keep a lab-only twin (morphology stripped) for the "lab only" scenario
saveRDS(list(lab = panel_lab, nasis = panel,
             meta = meta[pedon_key %in% ids[pick]]),
        file.path(cache, "kssl_panel.rds"), compress = FALSE)  # R6 lists: gzip is very slow
message("panel saved: ", length(panel), " pedons")
