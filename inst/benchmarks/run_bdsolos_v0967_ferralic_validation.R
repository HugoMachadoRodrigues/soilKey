#!/usr/bin/env Rscript
# inst/benchmarks/run_bdsolos_v0967_ferralic_validation.R
#
# Quantify the v0.9.67 ferralic regional CTC tolerance on BDsolos RJ.
#
# v0.9.65 NEWS noted: "88/115 (76.5%) RJ Latossolos fail ferralic
# due to CTC argila > 17 cmol(c)/kg in the data". v0.9.67 adds
# engine="aqp" with a 20-cmol threshold (Embrapa lab methodology
# tolerance). This script measures the Latossolos recall lift.

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0967_RJ_%s.rds", DATE_TAG))
TXT_OUT <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0967_RJ_%s.txt", DATE_TAG))
dir.create(dirname(RDS_OUT), recursive = TRUE, showWarnings = FALSE)

cat("[v0.9.67-ferralic] loading RJ.csv ...\n")
peds <- load_bdsolos_csv(RJ, verbose = FALSE)
cat(sprintf("[v0.9.67-ferralic] loaded %d perfis\n", length(peds)))

run_with_engine <- function(label, engine_value) {
  cat(sprintf("\n[v0.9.67-ferralic] === %s (engine=%s) ===\n",
              label, engine_value))
  options(soilKey.diagnostic_engine = engine_value)
  on.exit(options(soilKey.diagnostic_engine = NULL), add = TRUE)
  t0 <- Sys.time()
  res <- benchmark_bdsolos(peds, systems = "sibcs",
                              sibcs_level = "order", verbose = FALSE)
  dt <- as.numeric(Sys.time() - t0, "secs")
  acc <- res$per_system$sibcs$accuracy
  n   <- res$per_system$sibcs$n_compared
  cat(sprintf("[v0.9.67-ferralic] %s: %.1f s, acc=%.3f n=%d\n",
              label, dt, acc, n))
  list(label = label, engine = engine_value, elapsed_s = dt,
       accuracy = acc, n_compared = n,
       confusion = res$per_system$sibcs$confusion,
       per_class_recall = res$per_system$sibcs$per_class_recall)
}

results <- list()
results$soilkey <- run_with_engine("soilkey (baseline strict 16)", "soilkey")
results$aqp     <- run_with_engine("aqp (regional 20)",            "aqp")

# Latossolos-specific recall change
extract_latossolos_row <- function(per_class_recall) {
  if (is.null(per_class_recall) || nrow(per_class_recall) == 0L)
    return(NULL)
  m <- grepl("^Latossolos$", per_class_recall$reference_rsg)
  if (!any(m)) return(NULL)
  per_class_recall[m, , drop = FALSE]
}
lat_soilkey <- extract_latossolos_row(results$soilkey$per_class_recall)
lat_aqp     <- extract_latossolos_row(results$aqp$per_class_recall)

saveRDS(list(date = DATE_TAG,
              soilKey_version = as.character(utils::packageVersion("soilKey")),
              n_pedons = length(peds),
              results = results,
              lat_soilkey = lat_soilkey,
              lat_aqp     = lat_aqp), RDS_OUT)
cat(sprintf("\n[v0.9.67-ferralic] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat("BDsolos RJ ferralic regional-tolerance validation -- v0.9.67\n")
cat(sprintf("Date           : %s\n", DATE_TAG))
cat(sprintf("soilKey version: %s\n",
            as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Pedons         : %d (RJ.csv)\n\n", length(peds)))

cat("=== Per-engine SiBCS Order accuracy ===\n")
cat(sprintf("%-30s | %-9s | %-9s | %s\n",
            "engine", "elapsed_s", "accuracy", "n_compared"))
cat(strrep("-", 75), "\n", sep = "")
for (r in results) {
  cat(sprintf("%-30s | %9.1f | %9.3f | %d\n",
              r$label, r$elapsed_s, r$accuracy, r$n_compared))
}

cat("\n=== Latossolos recall change ===\n")
if (!is.null(lat_soilkey) && !is.null(lat_aqp)) {
  cat(sprintf("soilkey: %d / %d (%.1f%%)\n",
              lat_soilkey$n_correct, lat_soilkey$n,
              100 * lat_soilkey$recall))
  cat(sprintf("aqp    : %d / %d (%.1f%%)\n",
              lat_aqp$n_correct, lat_aqp$n,
              100 * lat_aqp$recall))
  cat(sprintf("delta  : +%.1f pp\n",
              100 * (lat_aqp$recall - lat_soilkey$recall)))
} else {
  cat("Latossolos row missing in per_class_recall.\n")
}

cat("\n=== aqp engine confusion ===\n")
print(results$aqp$confusion)
sink()
cat(sprintf("[v0.9.67-ferralic] wrote %s\n", TXT_OUT))
