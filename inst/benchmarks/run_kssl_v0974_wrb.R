#!/usr/bin/env Rscript
# inst/benchmarks/run_kssl_v0974_wrb.R
#
# v0.9.74 -- KSSL/NCSS WRB benchmark via USDA -> WRB cross-walk.
#
# The KSSL gpkg (NCSS Lab Data Mart, 5.5 GB) ships with rich
# USDA Soil Taxonomy classification at all 4 levels (Order/Suborder/
# Greatgroup/Subgroup) but no WRB labels. v0.9.74 builds a
# USDA -> WRB cross-walk (usda_to_wrb_rsg(), based on IUSS WRB
# 2022 Annex 6) and benchmarks soilKey's classify_wrb2022 against
# the cross-walked ground truth.
#
# Run on the full local gpkg with head = 200:
#   Rscript inst/benchmarks/run_kssl_v0974_wrb.R
# Or use the bundled 100-pedon sample:
#   Rscript -e 'load_all("."); s <- load_kssl_sample();
#                benchmark_wrb_vs_usda(s$pedons)'

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

GPKG <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/KSSL/ncss_labdata.gpkg"
HEAD_N <- 200L
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT  <- file.path("inst/benchmarks/reports",
                        sprintf("kssl_v0974_wrb_%s.rds", DATE_TAG))
TXT_OUT  <- file.path("inst/benchmarks/reports",
                        sprintf("kssl_v0974_wrb_%s.md", DATE_TAG))
dir.create(dirname(RDS_OUT), recursive = TRUE, showWarnings = FALSE)

cat(sprintf("[kssl-v0974] reading first %d pedons from %s\n",
             HEAD_N, basename(GPKG)))
peds <- if (file.exists(GPKG)) load_kssl_pedons_gpkg(GPKG, head = HEAD_N,
                                                       verbose = TRUE)
         else { cat("[kssl-v0974] gpkg not local; falling back to bundled sample\n")
                load_kssl_sample()$pedons }

peds <- annotate_wrb_from_usda(peds)
cat(sprintf("[kssl-v0974] %d pedons annotated with derived WRB labels\n", length(peds)))

run_one <- function(label, opts) {
  cat(sprintf("\n[kssl-v0974] === %s ===\n", label))
  for (k in names(opts)) options(setNames(list(opts[[k]]), k))
  on.exit(for (k in names(opts)) options(setNames(list(NULL), k)), add = TRUE)
  t0 <- Sys.time()
  res <- benchmark_wrb_vs_usda(peds, verbose = FALSE)
  dt <- as.numeric(Sys.time() - t0, "secs")
  cat(sprintf("  %.1fs, top-1 = %d/%d (%.1f%%)\n",
              dt, sum(res$refs[res$refs == res$preds & !is.na(res$refs)] == res$preds[res$refs == res$preds & !is.na(res$refs)], na.rm = TRUE),
              res$n_compared, 100 * res$accuracy))
  c(list(label = label, opts = opts, elapsed_s = dt), res)
}

results <- list(
  baseline = run_one("baseline (no opt-ins)", list()),
  aqp      = run_one("+aqp engine",
                       list(soilKey.diagnostic_engine = "aqp")),
  fbck     = run_one("+aqp + ECEC + tex-morph (v0.9.69-70)",
                       list(soilKey.diagnostic_engine = "aqp",
                            soilKey.ferralic_ecec_fallback = TRUE,
                            soilKey.ferralic_texture_morphological_fallback = TRUE)),
  full     = run_one("+full v0.9.69-72 stack (g/f/v inferences)",
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
cat(sprintf("# KSSL WRB benchmark report -- v%s -- %s\n",
             utils::packageVersion("soilKey"), DATE_TAG))
cat(sprintf("\n**Source:** `load_kssl_pedons_gpkg(GPKG, head = %d)` ",
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
cat(sprintf("\n[kssl-v0974] wrote %s\n", RDS_OUT))
cat(sprintf("[kssl-v0974] wrote %s\n", TXT_OUT))
