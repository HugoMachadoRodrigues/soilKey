#!/usr/bin/env Rscript
# inst/benchmarks/run_redape_v0971.R
#
# Benchmark soilKey against the Vaz et al. 2023 Redape curated dataset
# (DOI 10.48432/PYKKA7). 96 hand-reviewed Brazilian soil profiles
# served as gold-standard reference for SiBCS classification.
#
# This is the first benchmark on a CURATED dataset, distinguishing
# soilKey logic gaps from data-quality artifacts that plague the raw
# BDsolos export.

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

REDAPE_DIR <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/redape_geotab"
DATE_TAG   <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT    <- file.path("inst/benchmarks/reports",
                          sprintf("redape_v0971_%s.rds", DATE_TAG))
TXT_OUT    <- file.path("inst/benchmarks/reports",
                          sprintf("redape_v0971_%s.txt", DATE_TAG))
dir.create(dirname(RDS_OUT), recursive = TRUE, showWarnings = FALSE)

# Auto-download if needed
if (!dir.exists(REDAPE_DIR) || length(list.files(REDAPE_DIR, "\\.json$")) == 0L) {
  cat("[redape] dataset not found locally; downloading ...\n")
  download_redape_dataset(REDAPE_DIR, verbose = TRUE)
}

cat("[redape] loading ...\n")
peds <- load_redape_pedons(REDAPE_DIR, verbose = TRUE)
cat(sprintf("[redape] %d unique pedons loaded.\n", length(peds)))

run_one <- function(label, level, opts = list()) {
  cat(sprintf("\n[redape] === %s (level=%s) ===\n", label, level))
  for (k in names(opts)) options(setNames(list(opts[[k]]), k))
  on.exit(for (k in names(opts)) options(setNames(list(NULL), k)), add = TRUE)
  t0 <- Sys.time()
  res <- benchmark_redape(peds, level = level, verbose = FALSE)
  dt <- as.numeric(Sys.time() - t0, "secs")
  cat(sprintf("  %.1fs, acc=%.3f, n=%d / %d\n",
               dt, res$accuracy, res$n_compared, res$n_total))
  list(label = label, level = level, opts = opts,
       elapsed_s = dt, accuracy = res$accuracy,
       n_compared = res$n_compared, n_total = res$n_total,
       confusion = res$confusion,
       per_class_recall = res$per_class_recall)
}

results <- list(
  baseline_order = run_one("baseline (no fallbacks)", "order", list()),
  aqp_order      = run_one("engine=aqp", "order",
                              list(soilKey.diagnostic_engine = "aqp")),
  aqp_full_order = run_one("engine=aqp + ECEC + texture-morph", "order",
                              list(soilKey.diagnostic_engine = "aqp",
                                   soilKey.ferralic_ecec_fallback = TRUE,
                                   soilKey.ferralic_texture_morphological_fallback = TRUE))
)

saveRDS(list(date = DATE_TAG,
              soilKey_version = as.character(utils::packageVersion("soilKey")),
              n_pedons = length(peds), results = results), RDS_OUT)
cat(sprintf("\n[redape] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat("Redape (Vaz et al 2023) benchmark -- soilKey v0.9.71+\n")
cat(sprintf("Date           : %s\n", DATE_TAG))
cat(sprintf("soilKey version: %s\n",
            as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Pedons         : %d\n\n", length(peds)))

cat("=== Order-level accuracy ladder ===\n")
cat(sprintf("%-40s | %s | %s\n", "configuration", "accuracy", "n_compared"))
cat(strrep("-", 70), "\n", sep = "")
for (r in results) {
  cat(sprintf("%-40s | %8.3f | %d\n",
              r$label, r$accuracy, r$n_compared))
}

cat("\n=== Per-class recall (baseline) ===\n")
print(results$baseline_order$per_class_recall)
cat("\n=== Confusion matrix (baseline) ===\n")
print(results$baseline_order$confusion)
sink()
cat(sprintf("[redape] wrote %s\n", TXT_OUT))
