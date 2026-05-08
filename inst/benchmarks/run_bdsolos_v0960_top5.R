#!/usr/bin/env Rscript
# inst/benchmarks/run_bdsolos_v0960_top5.R
#
# Top-5 UFs variant: loads only the 5 BDsolos UFs that cover the
# most pedons (BA + AM + MG + RJ + PA -- ~3500-4000 perfis), runs
# benchmark_bdsolos() per UF and aggregates. Avoids the v0.9.60-
# observed R6 accumulation slowdown that locks up after ~7 UFs in
# the full 27-UF script.
#
# Wall-clock: ~3-5 min (loaders) + ~1-2 min (benchmark).

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

BD <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos"
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT  <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0960_top5_%s.rds", DATE_TAG))
TXT_OUT  <- file.path("inst/benchmarks/reports",
                       sprintf("bdsolos_v0960_top5_%s.txt", DATE_TAG))

UFS <- c("BA", "AM", "RJ", "PA", "MG")  # top 5 by file size

cat("[bdsolos-top5] loading 5 UFs...\n")
ped_chunks <- list()
for (uf in UFS) {
  t0 <- Sys.time()
  pp <- tryCatch(load_bdsolos_csv(file.path(BD, paste0(uf, ".csv")),
                                     verbose = FALSE),
                  error = function(e) {
                    cat(sprintf("  %s : LOAD FAIL: %s\n", uf, conditionMessage(e)))
                    NULL
                  })
  if (is.null(pp)) next
  for (p in pp) p$site$uf <- uf
  ped_chunks[[uf]] <- pp
  cat(sprintf("  %s : %4d perfis (%.1fs)\n", uf, length(pp),
              as.numeric(Sys.time() - t0, "secs")))
  invisible(gc(verbose = FALSE))
}
all_pedons <- unlist(ped_chunks, recursive = FALSE, use.names = FALSE)
cat(sprintf("[bdsolos-top5] total perfis: %d\n\n", length(all_pedons)))

# Coverage audit per UF
audit <- do.call(rbind, lapply(UFS, function(uf) {
  pp <- ped_chunks[[uf]]
  if (is.null(pp)) return(NULL)
  s_ok  <- sum(vapply(pp, function(p) !is.na(p$site$reference_sibcs %||% NA), logical(1L)))
  w_ok  <- sum(vapply(pp, function(p) !is.na(p$site$reference_wrb   %||% NA), logical(1L)))
  st_ok <- sum(vapply(pp, function(p) !is.na(p$site$reference_st    %||% NA), logical(1L)))
  data.frame(uf = uf, n = length(pp), sibcs = s_ok, wrb = w_ok,
              usda = st_ok, stringsAsFactors = FALSE)
}))
print(audit, row.names = FALSE)

cat("\n[bdsolos-top5] running benchmark_bdsolos() across all 5 UFs...\n")
t0 <- Sys.time()
res <- benchmark_bdsolos(all_pedons,
                          systems     = c("wrb2022", "sibcs", "usda"),
                          sibcs_level = "order",
                          verbose     = TRUE)
elapsed <- round(as.numeric(Sys.time() - t0, "secs"), 1)
cat(sprintf("[bdsolos-top5] benchmark done in %.1fs\n", elapsed))

# Per-UF SiBCS Order
cat("[bdsolos-top5] per-UF SiBCS Order...\n")
per_uf_sibcs <- list()
for (uf in UFS) {
  pp <- ped_chunks[[uf]]
  if (is.null(pp)) next
  r <- tryCatch(
    benchmark_bdsolos(pp, systems = "sibcs", sibcs_level = "order",
                        verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(r)) next
  ps <- r$per_system$sibcs
  per_uf_sibcs[[uf]] <- list(n_compared = ps$n_compared,
                              n_correct  = ps$n_correct,
                              accuracy   = ps$accuracy)
  cat(sprintf("  %s : n=%4d  acc=%.3f\n", uf, ps$n_compared,
              ps$accuracy %||% NA_real_))
  invisible(gc(verbose = FALSE))
}

result <- list(scope = "BDsolos top-5 UFs (BA + AM + RJ + PA + MG)",
                date = DATE_TAG,
                soilKey_version = as.character(utils::packageVersion("soilKey")),
                audit = audit,
                pooled = res,
                per_uf_sibcs = per_uf_sibcs,
                elapsed_s = elapsed)
saveRDS(result, RDS_OUT)
cat(sprintf("[bdsolos-top5] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat("BDsolos v0.9.60 top-5 UFs benchmark\n")
cat(sprintf("Date           : %s\n", DATE_TAG))
cat(sprintf("soilKey version: %s\n", as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Total perfis   : %d\n\n", length(all_pedons)))

cat("Per-UF coverage:\n")
print(audit, row.names = FALSE)

cat("\n=== Pooled per-system result (5 UFs combined) ===\n")
for (sys in names(res$per_system)) {
  ps <- res$per_system[[sys]]
  cov <- res$coverage[[sys]]
  cat(sprintf("  %-7s | label_cov=%5.1f%% (%d/%d)  acc=%6s  n_compared=%d  errors=%d\n",
              sys, cov$pct, cov$n_with_ref, cov$n_total,
              if (is.na(ps$accuracy %||% NA_real_)) "NA" else sprintf("%.3f", ps$accuracy),
              ps$n_compared, ps$n_errors))
}

cat("\n=== Per-UF SiBCS Order accuracy ===\n")
cat(sprintf("%-3s | %-6s | %-7s | %-9s\n", "UF", "n", "n_corr", "acc"))
cat(strrep("-", 32), "\n", sep = "")
for (uf in names(per_uf_sibcs)) {
  r <- per_uf_sibcs[[uf]]
  cat(sprintf("%-3s | %-6d | %-7d | %.3f\n",
              uf, r$n_compared, r$n_correct, r$accuracy %||% NA_real_))
}

cat("\n=== Pooled SiBCS Order confusion (top-15 reference orders) ===\n")
cm <- res$per_system$sibcs$confusion
if (!is.null(cm)) {
  top <- names(sort(rowSums(cm), decreasing = TRUE))[1:min(15, nrow(cm))]
  print(cm[top, intersect(top, colnames(cm)), drop = FALSE])
}

cat("\n=== Pooled SiBCS per-class recall (top-15) ===\n")
pc <- res$per_system$sibcs$per_class
if (!is.null(pc)) {
  pc <- pc[order(-pc$n_ref), ]
  print(head(pc, 15), row.names = FALSE)
}
sink()
cat(sprintf("[bdsolos-top5] wrote %s\n", TXT_OUT))
cat("[bdsolos-top5] DONE\n")
