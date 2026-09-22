#!/usr/bin/env Rscript
# inst/benchmarks/run_bdsolos_v0960_focused.R
#
# Focused (RJ-only) variant of the v0.9.60 BDsolos benchmark. The
# full 27-UF script (run_bdsolos_v0960.R) hits cumulative R6/IO
# slowness on the OneDrive-mounted soil_data path; this stripped-
# down variant runs the same benchmark on a single UF (RJ.csv,
# 722 perfis, the largest individual dataset that loads cleanly
# in <10s) so we can ship v0.9.60 with a real accuracy number.

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

BD <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos"
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT  <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0960_RJ_%s.rds", DATE_TAG))
TXT_OUT  <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0960_RJ_%s.txt", DATE_TAG))

cat("[bdsolos-v0960-focused] loading RJ.csv (722 perfis expected)...\n")
t0 <- Sys.time()
peds <- load_bdsolos_csv(file.path(BD, "RJ.csv"), verbose = TRUE)
cat(sprintf("[bdsolos-v0960-focused] loaded %d perfis in %.1fs\n",
            length(peds), as.numeric(Sys.time() - t0, "secs")))

# Coverage audit before benchmark
sibcs_n <- sum(vapply(peds, function(p) !is.na(p$site$reference_sibcs %||% NA), logical(1L)))
wrb_n   <- sum(vapply(peds, function(p) !is.na(p$site$reference_wrb   %||% NA), logical(1L)))
usda_n  <- sum(vapply(peds, function(p) !is.na(p$site$reference_st    %||% NA), logical(1L)))
coords_n <- sum(vapply(peds, function(p) isTRUE(is.finite(p$site$lat)), logical(1L)))
muns_n   <- sum(vapply(peds, function(p) any(!is.na(p$horizons$munsell_hue_moist)), logical(1L)))
cat(sprintf("[bdsolos-v0960-focused] coverage: sibcs=%d  wrb=%d  usda=%d  coords=%d  munsell=%d\n",
            sibcs_n, wrb_n, usda_n, coords_n, muns_n))

# -- Run the benchmark on all systems --
cat("[bdsolos-v0960-focused] benchmark all 3 systems (Order level)...\n")
t0 <- Sys.time()
res <- benchmark_bdsolos(peds,
                          systems     = c("wrb2022", "sibcs", "usda"),
                          sibcs_level = "order",
                          verbose     = TRUE)
elapsed <- round(as.numeric(Sys.time() - t0, "secs"), 1)
cat(sprintf("[bdsolos-v0960-focused] benchmark done in %.1fs\n\n", elapsed))

# -- Persist + summary --
result <- list(
  scope            = "RJ.csv (single UF)",
  date             = DATE_TAG,
  soilKey_version  = as.character(utils::packageVersion("soilKey")),
  load_seconds     = round(as.numeric(t0 - Sys.time(), "secs"), 1) * -1,  # negative; recompute below
  benchmark_seconds = elapsed,
  result           = res
)
saveRDS(result, RDS_OUT)
cat(sprintf("[bdsolos-v0960-focused] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat("BDsolos v0.9.60 focused benchmark (RJ only)\n")
cat(sprintf("Date           : %s\n", DATE_TAG))
cat(sprintf("soilKey version: %s\n", as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Pedons loaded  : %d\n", length(peds)))
cat(sprintf("Coverage       : sibcs=%d  wrb=%d  usda=%d  coords=%d  munsell=%d\n",
            sibcs_n, wrb_n, usda_n, coords_n, muns_n))
cat(sprintf("Benchmark time : %.1fs\n\n", elapsed))

cat("=== Per-system result ===\n")
for (sys in names(res$per_system)) {
  ps <- res$per_system[[sys]]
  cov <- res$coverage[[sys]]
  cat(sprintf("  %-7s | label_cov=%5.1f%% (%d/%d)  acc=%6s  n_compared=%d  n_correct=%d  errors=%d  msg=%s\n",
              sys, cov$pct, cov$n_with_ref, cov$n_total,
              if (is.na(ps$accuracy %||% NA_real_)) "NA"
                else sprintf("%.3f", ps$accuracy),
              ps$n_compared, ps$n_correct, ps$n_errors,
              ps$message %||% "OK"))
}

cat("\n=== SiBCS Order confusion (top-12 reference orders) ===\n")
cm <- res$per_system$sibcs$confusion
if (!is.null(cm)) {
  top <- names(sort(rowSums(cm), decreasing = TRUE))[1:min(12, nrow(cm))]
  print(cm[top, intersect(top, colnames(cm)), drop = FALSE])
}

cat("\n=== SiBCS per-class recall ===\n")
pc <- res$per_system$sibcs$per_class
if (!is.null(pc)) {
  pc <- pc[order(-pc$n_ref), ]
  print(head(pc, 15), row.names = FALSE)
}

if (!is.null(res$per_system$wrb2022$confusion)) {
  cat("\n=== WRB confusion ===\n")
  print(res$per_system$wrb2022$confusion)
}
if (!is.null(res$per_system$usda$confusion)) {
  cat("\n=== USDA confusion ===\n")
  print(res$per_system$usda$confusion)
}
sink()
cat(sprintf("[bdsolos-v0960-focused] wrote %s\n", TXT_OUT))
cat("[bdsolos-v0960-focused] DONE\n")
