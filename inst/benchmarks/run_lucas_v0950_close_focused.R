#!/usr/bin/env Rscript
# inst/benchmarks/run_lucas_v0950_close_focused.R
#
# 30-pedon subsoil-fill close of v0.9.50. The full 100-pedon
# 3-stage script (run_lucas_v0950_close.R) takes 1-2h because
# every SoilGrids COG range read is ~3-4s and the full run
# needs 100 x 18 = 1800 calls. This focused variant does:
#   * 30 stratified pedons (10 each from FR/PL/IT)
#   * stage 1: baseline_no_fill (replica v0.9.49)
#   * stage 2: subsoil_soilgrids (the v0.9.50 path that was
#     never published empirically)
# Skips the topsoil_plus_subsoil_soilgrids configuration.
# Wall-clock ~12-15 min.

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

SOIL_ROOT <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/eu_lucas"
LUCAS_CSV <- file.path(SOIL_ROOT,
                       "LUCAS-SOIL-2018-data-report-readme-v2",
                       "LUCAS-SOIL-2018-v2", "LUCAS-SOIL-2018.csv")
ESDB_ROOT <- file.path(SOIL_ROOT,
                       "ESDB-Raster-Library-1k-GeoTIFF-20240507")

stopifnot(file.exists(LUCAS_CSV), dir.exists(ESDB_ROOT))

REPORT_DIR <- "inst/benchmarks/reports"
DATE_TAG   <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT    <- file.path(REPORT_DIR,
                          sprintf("lucas_v0950_close_focused_%s.rds", DATE_TAG))
TXT_OUT    <- file.path(REPORT_DIR,
                          sprintf("lucas_v0950_close_focused_%s.txt", DATE_TAG))

set.seed(20260506L)

cat("[v0950-close-focused] loading 30 pedons (FR/PL/IT, 10 each)...\n")
PER_CC <- 10L
ped_list <- list()
for (cc in c("FR", "PL", "IT")) {
  p <- load_lucas_soil_2018(LUCAS_CSV, countries = cc,
                              max_n = NULL, verbose = FALSE)
  if (length(p) > PER_CC) p <- sample(p, PER_CC)
  ped_list <- c(ped_list, p)
  cat(sprintf("  %s : %d pedons\n", cc, length(p)))
}
cat(sprintf("[v0950-close-focused] total pedons: %d\n", length(ped_list)))

run_block <- function(label, fill_topsoil, fill_subsoil) {
  cat(sprintf("\n[v0950-close-focused] ==== %s ====\n", label))
  cat(sprintf("  fill_topsoil_from = %s, fill_subsoil_from = %s\n",
              fill_topsoil, fill_subsoil))
  t0 <- Sys.time()
  res <- tryCatch(
    benchmark_lucas_2018(
      pedons              = ped_list,
      esdb_root           = ESDB_ROOT,
      attribute           = "WRBLV1",
      fill_topsoil_from   = fill_topsoil,
      fill_subsoil_from   = fill_subsoil,
      classify_with       = "wrb2022",
      verbose             = TRUE
    ),
    error = function(e) {
      cat(sprintf("  ERROR: %s\n", conditionMessage(e)))
      NULL
    }
  )
  dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
  cat(sprintf("[v0950-close-focused] %s done in %.1f s\n", label, dt))
  if (!is.null(res)) {
    cat(sprintf("  in_scope = %d / %d, errors = %d, accuracy = %.3f\n",
                res$n_in_scope, res$n_total, res$n_errors, res$accuracy))
  }
  list(label = label, fill_topsoil = fill_topsoil,
       fill_subsoil = fill_subsoil, elapsed_s = dt, result = res)
}

results <- list()
results$baseline <- run_block("baseline_no_fill",
                                 fill_topsoil = "none",
                                 fill_subsoil = "none")

# Save partial after stage 1 so we don't lose it if stage 2 hangs
saveRDS(results, RDS_OUT)
cat(sprintf("[v0950-close-focused] partial RDS after stage 1: %s\n", RDS_OUT))

results$subsoil <- run_block("subsoil_soilgrids",
                                 fill_topsoil = "none",
                                 fill_subsoil = "soilgrids")
saveRDS(results, RDS_OUT)

sink(TXT_OUT)
cat("LUCAS WRB benchmark -- v0.9.50 empirical close (focused, 30 pedons)\n")
cat(sprintf("Date: %s\n", DATE_TAG))
cat(sprintf("Pedons: %d (stratified FR/PL/IT, %d each)\n",
            length(ped_list), PER_CC))
cat(sprintf("ESDB attribute: WRBLV1\n"))
cat(sprintf("Classifier: classify_wrb2022()\n\n"))
cat(sprintf("%-32s | %-10s | %-9s | %s\n",
            "configuration", "elapsed_s", "accuracy", "in_scope"))
cat(strrep("-", 72), "\n", sep = "")
for (k in names(results)) {
  r <- results[[k]]
  acc <- if (is.null(r$result)) NA_real_ else r$result$accuracy
  ins <- if (is.null(r$result)) NA_integer_ else
           sprintf("%d / %d", r$result$n_in_scope, r$result$n_total)
  cat(sprintf("%-32s | %10.1f | %8.3f | %s\n",
              r$label, r$elapsed_s, acc, ins))
}
cat("\nPer-RSG recall (subsoil_soilgrids):\n")
if (!is.null(results$subsoil$result)) {
  print(results$subsoil$result$per_rsg)
}
cat("\nConfusion (subsoil_soilgrids):\n")
if (!is.null(results$subsoil$result)) {
  print(results$subsoil$result$confusion)
}
sink()
cat(sprintf("[v0950-close-focused] wrote %s\n", TXT_OUT))
cat("[v0950-close-focused] DONE\n")
