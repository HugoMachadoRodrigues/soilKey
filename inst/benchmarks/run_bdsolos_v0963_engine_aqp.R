#!/usr/bin/env Rscript
# inst/benchmarks/run_bdsolos_v0963_engine_aqp.R
#
# Re-run the v0.9.61 BDsolos RJ benchmark with the v0.9.63
# `soilKey.diagnostic_engine = "aqp"` option turned on, to verify
# that the canonical aqp::getArgillicBounds / getCambicBounds engines
# lift the SiBCS Order accuracy beyond the v0.9.61 RJ headline (40.3%).

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0963_RJ_engine_aqp_%s.rds", DATE_TAG))
TXT_OUT <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0963_RJ_engine_aqp_%s.txt", DATE_TAG))

cat("[v0.9.63-engine-aqp] loading RJ.csv ...\n")
peds <- load_bdsolos_csv(RJ, verbose = FALSE)
cat(sprintf("[v0.9.63-engine-aqp] loaded %d perfis\n", length(peds)))

run_with_engine <- function(label, engine_value) {
  cat(sprintf("\n[v0.9.63-engine-aqp] === %s (engine=%s) ===\n",
              label, engine_value))
  options(soilKey.diagnostic_engine = engine_value)
  on.exit(options(soilKey.diagnostic_engine = NULL), add = TRUE)
  t0 <- Sys.time()
  res <- benchmark_bdsolos(peds, systems = "sibcs",
                              sibcs_level = "order", verbose = FALSE)
  dt <- as.numeric(Sys.time() - t0, "secs")
  cat(sprintf("[v0.9.63-engine-aqp] %s: %.1f s, acc=%.3f n=%d\n",
              label, dt, res$per_system$sibcs$accuracy,
              res$per_system$sibcs$n_compared))
  list(label = label, engine = engine_value, elapsed_s = dt,
       result = res$per_system$sibcs)
}

results <- list()
results$soilkey <- run_with_engine("soilkey (baseline)", "soilkey")
results$aqp     <- run_with_engine("aqp (engine override)", "aqp")

saveRDS(list(date = DATE_TAG,
              soilKey_version = as.character(utils::packageVersion("soilKey")),
              n_pedons = length(peds),
              results = results), RDS_OUT)
cat(sprintf("\n[v0.9.63-engine-aqp] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat("BDsolos RJ engine-AQP benchmark -- v0.9.63\n")
cat(sprintf("Date           : %s\n", DATE_TAG))
cat(sprintf("soilKey version: %s\n",
            as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Pedons         : %d (RJ.csv)\n\n", length(peds)))

cat("=== Per-engine SiBCS Order accuracy ===\n")
cat(sprintf("%-25s | %-9s | %-9s | %s\n",
            "engine", "elapsed_s", "accuracy", "n_compared"))
cat(strrep("-", 60), "\n", sep = "")
for (k in names(results)) {
  r <- results[[k]]
  cat(sprintf("%-25s | %9.1f | %8.3f | %d\n",
              r$engine, r$elapsed_s,
              r$result$accuracy, r$result$n_compared))
}

cat("\n=== Per-class recall: aqp engine vs soilkey ===\n")
sk_pc <- results$soilkey$result$per_class
aq_pc <- results$aqp$result$per_class
if (!is.null(sk_pc) && !is.null(aq_pc)) {
  ranked <- merge(
    sk_pc[, c("reference", "n_ref", "recall")],
    aq_pc[, c("reference", "recall")],
    by = "reference",
    suffixes = c(".soilkey", ".aqp"))
  ranked$delta_pp <- 100 * (ranked$recall.aqp - ranked$recall.soilkey)
  ranked <- ranked[order(-ranked$n_ref), ]
  print(head(ranked, 15), row.names = FALSE)
}
sink()
cat(sprintf("[v0.9.63-engine-aqp] wrote %s\n", TXT_OUT))
cat("[v0.9.63-engine-aqp] DONE\n")
