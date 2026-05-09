#!/usr/bin/env Rscript
# inst/benchmarks/run_kssl_nasis_v0975_wrb.R
#
# v0.9.75 -- KSSL + NASIS morphological-enriched WRB benchmark.
#
# Joins the KSSL gpkg (5.5 GB lab data) with the companion NASIS
# Morphological sqlite (Munsell + structure + clay films + slickensides)
# via load_kssl_pedons_with_nasis(), applies the v0.9.74 USDA -> WRB
# cross-walk, and runs the full v0.9.69-72 fallback ladder.

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

GPKG  <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/KSSL/ncss_labdata.gpkg"
NASIS <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/KSSL/NASIS_Morphological_09142021.sqlite"
HEAD_N <- 200L
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT  <- file.path("inst/benchmarks/reports",
                        sprintf("kssl_nasis_v0975_wrb_%s.rds", DATE_TAG))
TXT_OUT  <- file.path("inst/benchmarks/reports",
                        sprintf("kssl_nasis_v0975_wrb_%s.md", DATE_TAG))
dir.create(dirname(RDS_OUT), recursive = TRUE, showWarnings = FALSE)

if (file.exists(GPKG) && file.exists(NASIS)) {
  cat(sprintf("[kssl-nasis-v0975] reading first %d pedons + NASIS join\n", HEAD_N))
  peds <- load_kssl_pedons_with_nasis(GPKG, NASIS, head = HEAD_N, verbose = TRUE)
} else {
  cat("[kssl-nasis-v0975] gpkg/sqlite not local; falling back to bundled sample\n")
  peds <- load_kssl_nasis_sample()$pedons
}
peds <- annotate_wrb_from_usda(peds)
cat(sprintf("[kssl-nasis-v0975] %d pedons\n", length(peds)))

run_one <- function(label, opts) {
  cat(sprintf("\n[kssl-nasis-v0975] === %s ===\n", label))
  for (k in names(opts)) options(setNames(list(opts[[k]]), k))
  on.exit(for (k in names(opts)) options(setNames(list(NULL), k)), add = TRUE)
  t0 <- Sys.time()
  res <- benchmark_wrb_vs_usda(peds, verbose = FALSE)
  dt <- as.numeric(Sys.time() - t0, "secs")
  cat(sprintf("  %.1fs, accuracy = %.1f%% on n = %d\n",
              dt, 100 * res$accuracy, res$n_compared))
  c(list(label = label, opts = opts, elapsed_s = dt), res)
}

results <- list(
  baseline = run_one("baseline (no opt-ins)", list()),
  aqp      = run_one("+aqp engine", list(soilKey.diagnostic_engine = "aqp")),
  fbck     = run_one("+aqp + ECEC + tex-morph (v0.9.69-70)",
                       list(soilKey.diagnostic_engine = "aqp",
                            soilKey.ferralic_ecec_fallback = TRUE,
                            soilKey.ferralic_texture_morphological_fallback = TRUE)),
  full     = run_one("+full v0.9.69-72 stack",
                       list(soilKey.diagnostic_engine = "aqp",
                            soilKey.ferralic_ecec_fallback = TRUE,
                            soilKey.ferralic_texture_morphological_fallback = TRUE,
                            soilKey.gleyic_designation_inference = TRUE,
                            soilKey.plinthic_designation_inference = TRUE,
                            soilKey.vertic_designation_inference = TRUE))
)

saveRDS(list(date = DATE_TAG, n_pedons = length(peds),
              soilKey_version = as.character(utils::packageVersion("soilKey")),
              results = results), RDS_OUT)

sink(TXT_OUT)
cat(sprintf("# KSSL + NASIS WRB benchmark report -- v%s -- %s\n",
             utils::packageVersion("soilKey"), DATE_TAG))
cat(sprintf("\n**Source:** `load_kssl_pedons_with_nasis(GPKG, NASIS, head = %d)` ",
             HEAD_N))
cat("with derived WRB labels via `usda_to_wrb_rsg()`\n\n")
cat("## Top-1 accuracy ladder\n\n")
cat("| Configuration | Accuracy |\n|---|---:|\n")
for (r in results) {
  cat(sprintf("| %s | %d/%d (%.1f%%) |\n",
              r$label,
              sum(r$refs[r$refs == r$preds & !is.na(r$refs)] == r$preds[r$refs == r$preds & !is.na(r$refs)], na.rm = TRUE),
              r$n_compared, 100 * r$accuracy))
}
cat("\n## Per-RSG recall (full stack)\n\n")
print(results$full$per_class_recall)
sink()
cat(sprintf("\n[kssl-nasis-v0975] wrote %s\n", RDS_OUT))
