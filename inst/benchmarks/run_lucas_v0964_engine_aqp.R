#!/usr/bin/env Rscript
# inst/benchmarks/run_lucas_v0964_engine_aqp.R
#
# Re-run the v0.9.50 LUCAS WRB benchmark with the v0.9.63
# `soilKey.diagnostic_engine = "aqp"` option turned on. v0.9.62
# diagnosed that soilKey's hand-coded cambic() fires 0% on BDsolos
# RJ vs aqp's 40.6% -- the SAME gap in Europe likely explains the
# v0.9.50 LUCAS WRB 0% baseline (Cambisols dominate the LUCAS
# reference at ~50%, and our cambic test never fires).
#
# Hypothesis: with engine="aqp" routing argic + cambic via the
# canonical NRCS aqp::getArgillicBounds / getCambicBounds, the
# LUCAS Cambisols recall should lift from 0% to a non-trivial
# fraction.
#
# Wall-clock estimate: ~30-90 min for 30 perfis x 9 properties via
# SoilGrids COG round-trips (the slow part is unchanged from v0.9.61).

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
                          sprintf("lucas_v0964_engine_aqp_%s.rds", DATE_TAG))
TXT_OUT    <- file.path(REPORT_DIR,
                          sprintf("lucas_v0964_engine_aqp_%s.txt", DATE_TAG))

set.seed(20260508L)

cat("[v0964-lucas-aqp] loading 30 pedons (FR/PL/IT, 10 each)...\n")
PER_CC <- 10L
ped_list <- list()
for (cc in c("FR", "PL", "IT")) {
  p <- load_lucas_soil_2018(LUCAS_CSV, countries = cc,
                              max_n = NULL, verbose = FALSE)
  if (length(p) > PER_CC) p <- sample(p, PER_CC)
  ped_list <- c(ped_list, p)
  cat(sprintf("  %s : %d pedons\n", cc, length(p)))
}

run_block <- function(label, fill_topsoil, fill_subsoil, engine_value) {
  cat(sprintf("\n[v0964-lucas-aqp] === %s (engine=%s, fill_subsoil=%s) ===\n",
              label, engine_value, fill_subsoil))
  options(soilKey.diagnostic_engine = engine_value)
  on.exit(options(soilKey.diagnostic_engine = NULL), add = TRUE)
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
  cat(sprintf("[v0964-lucas-aqp] %s done in %.1f s\n", label, dt))
  if (!is.null(res))
    cat(sprintf("  in_scope = %d / %d, accuracy = %.3f\n",
                res$n_in_scope, res$n_total, res$accuracy))
  list(label = label, engine = engine_value, fill = fill_subsoil,
       elapsed_s = dt, result = res)
}

results <- list()

# Stage 1: baseline soilkey engine, no fill (replicate v0.9.49 / v0.9.50)
results$baseline <- run_block("baseline_soilkey_no_fill",
                                fill_topsoil = "none",
                                fill_subsoil = "none",
                                engine_value = "soilkey")
saveRDS(results, RDS_OUT)

# Stage 2: aqp engine, no fill (the v0.9.63 hypothesis test)
results$aqp_no_fill <- run_block("aqp_no_fill",
                                    fill_topsoil = "none",
                                    fill_subsoil = "none",
                                    engine_value = "aqp")
saveRDS(results, RDS_OUT)

# Stage 3: aqp engine + subsoil fill (the v0.9.50 + v0.9.63 promise)
results$aqp_subsoil_fill <- run_block("aqp_subsoil_soilgrids",
                                          fill_topsoil = "none",
                                          fill_subsoil = "soilgrids",
                                          engine_value = "aqp")
saveRDS(results, RDS_OUT)

cat(sprintf("\n[v0964-lucas-aqp] saved %s\n", RDS_OUT))

sink(TXT_OUT)
cat("LUCAS WRB benchmark -- v0.9.64 engine=aqp\n")
cat(sprintf("Date: %s\n", DATE_TAG))
cat(sprintf("soilKey: %s\n",
            as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Pedons: %d (FR/PL/IT, %d each)\n", length(ped_list), PER_CC))
cat(sprintf("ESDB attribute: WRBLV1\n\n"))

cat(sprintf("%-30s | %-7s | %-9s | %-9s | %s\n",
            "configuration", "engine", "elapsed_s",
            "accuracy", "in_scope"))
cat(strrep("-", 80), "\n", sep = "")
for (k in names(results)) {
  r <- results[[k]]
  acc <- if (is.null(r$result)) NA else r$result$accuracy
  ins <- if (is.null(r$result)) NA else
           sprintf("%d / %d", r$result$n_in_scope, r$result$n_total)
  cat(sprintf("%-30s | %-7s | %9.1f | %8.3f | %s\n",
              r$label, r$engine, r$elapsed_s, acc, ins))
}

for (k in names(results)) {
  if (is.null(results[[k]]$result)) next
  cat(sprintf("\n=== Per-RSG recall (%s) ===\n", k))
  print(results[[k]]$result$per_rsg)
  cat(sprintf("\n=== Confusion (%s) ===\n", k))
  print(results[[k]]$result$confusion)
}
sink()
cat(sprintf("[v0964-lucas-aqp] wrote %s\n", TXT_OUT))
cat("[v0964-lucas-aqp] DONE\n")
