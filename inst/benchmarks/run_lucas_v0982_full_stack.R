#!/usr/bin/env Rscript
# inst/benchmarks/run_lucas_v0982_full_stack.R
#
# v0.9.82 LUCAS Stage 3 rerun with the full
# v0.9.66 + v0.9.72 + v0.9.77 + v0.9.78 + v0.9.79 + v0.9.80 stack:
#
#   * v0.9.66 leptic shallow-rock-evidence gate (auto, default)
#   * v0.9.72 gleyic_designation_inference     (opt-in)
#   * v0.9.77 vertisol cracks_at_surface gate  (auto, default)
#   * v0.9.78 mollic contiguous-stack          (auto, default)
#   * v0.9.79 mollic-priority intergrade gate  (auto, default)
#   * v0.9.80 andic_oc_bd_proxy                (opt-in)
#
# vs the v0.9.64 baseline (Stage 1 = soilkey engine no fill, Stage 2 =
# aqp engine no fill, Stage 3 = aqp engine + SoilGrids subsoil fill).
#
# Wall-clock estimate: ~3 s for Stages 1+2, ~90 min for Stage 3
# SoilGrids round-trips (n=30 pedons, FR/PL/IT, 10 each).

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
                          sprintf("lucas_v0982_full_stack_%s.rds", DATE_TAG))
TXT_OUT    <- file.path(REPORT_DIR,
                          sprintf("lucas_v0982_full_stack_%s.txt", DATE_TAG))

set.seed(20260508L)  # match v0.9.64 sample exactly

cat("[v0982-lucas-full] loading 30 pedons (FR/PL/IT, 10 each)...\n")
PER_CC <- 10L
ped_list <- list()
for (cc in c("FR", "PL", "IT")) {
  p <- load_lucas_soil_2018(LUCAS_CSV, countries = cc,
                              max_n = NULL, verbose = FALSE)
  if (length(p) > PER_CC) p <- sample(p, PER_CC)
  ped_list <- c(ped_list, p)
  cat(sprintf("  %s : %d pedons\n", cc, length(p)))
}

run_block <- function(label, fill_topsoil, fill_subsoil, opts) {
  cat(sprintf("\n[v0982-lucas-full] === %s ===\n", label))
  cat("  options:\n")
  for (nm in names(opts)) {
    cat(sprintf("    %-45s = %s\n", nm, opts[[nm]]))
  }
  cat(sprintf("  fill_topsoil = %s, fill_subsoil = %s\n",
              fill_topsoil, fill_subsoil))
  withr::with_options(opts, {
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
    cat(sprintf("[v0982-lucas-full] %s done in %.1f s\n", label, dt))
    if (!is.null(res))
      cat(sprintf("  in_scope = %d / %d, accuracy = %.3f\n",
                   res$n_in_scope, res$n_total, res$accuracy))
    list(label = label, opts = opts,
         fill_topsoil = fill_topsoil, fill_subsoil = fill_subsoil,
         elapsed_s = dt, result = res)
  })
}

results <- list()

# Stage 1: full soilkey baseline (no opt-ins, no fill)
results$baseline <- run_block(
  "stage1_baseline_soilkey",
  fill_topsoil = "none", fill_subsoil = "none",
  opts = list(soilKey.diagnostic_engine = "soilkey")
)
saveRDS(results, RDS_OUT)

# Stage 2: aqp engine + the v0.9.72 + v0.9.80 opt-ins, no fill
results$aqp_no_fill <- run_block(
  "stage2_aqp_full_stack_no_fill",
  fill_topsoil = "none", fill_subsoil = "none",
  opts = list(
    soilKey.diagnostic_engine                          = "aqp",
    soilKey.gleyic_designation_inference                = TRUE,
    soilKey.andic_oc_bd_proxy                            = TRUE,
    soilKey.ferralic_ecec_fallback                      = TRUE,
    soilKey.ferralic_texture_morphological_fallback      = TRUE
  )
)
saveRDS(results, RDS_OUT)

# Stage 3: aqp + full opt-in stack + SoilGrids subsoil fill
results$stage3 <- run_block(
  "stage3_aqp_full_stack_subsoil_soilgrids",
  fill_topsoil = "none", fill_subsoil = "soilgrids",
  opts = list(
    soilKey.diagnostic_engine                          = "aqp",
    soilKey.gleyic_designation_inference                = TRUE,
    soilKey.andic_oc_bd_proxy                            = TRUE,
    soilKey.ferralic_ecec_fallback                      = TRUE,
    soilKey.ferralic_texture_morphological_fallback      = TRUE
  )
)
saveRDS(results, RDS_OUT)

cat(sprintf("\n[v0982-lucas-full] saved %s\n", RDS_OUT))

sink(TXT_OUT)
cat("LUCAS WRB benchmark -- v0.9.82 full stack rerun\n")
cat(sprintf("Date: %s\n", DATE_TAG))
cat(sprintf("soilKey: %s\n",
            as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Pedons: %d (FR/PL/IT, %d each)\n", length(ped_list), PER_CC))
cat(sprintf("ESDB attribute: WRBLV1\n\n"))

cat(sprintf("%-40s | %-9s | %-9s | %s\n",
            "configuration", "elapsed_s", "accuracy", "in_scope"))
cat(strrep("-", 80), "\n", sep = "")
for (k in names(results)) {
  r <- results[[k]]
  acc <- if (is.null(r$result)) NA else r$result$accuracy
  ins <- if (is.null(r$result)) NA else
           sprintf("%d / %d", r$result$n_in_scope, r$result$n_total)
  cat(sprintf("%-40s | %9.1f | %8.3f | %s\n",
              r$label, r$elapsed_s, acc, ins))
}

for (k in names(results)) {
  if (is.null(results[[k]]$result)) next
  cat(sprintf("\n=== Per-RSG recall (%s) ===\n", k))
  print(results[[k]]$result$per_rsg)
  cat(sprintf("\n=== Confusion (%s) ===\n", k))
  print(results[[k]]$result$confusion)
}
sink()
cat(sprintf("[v0982-lucas-full] wrote %s\n", TXT_OUT))
cat("[v0982-lucas-full] DONE\n")
