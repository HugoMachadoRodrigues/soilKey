#!/usr/bin/env Rscript
# inst/benchmarks/run_bdsolos_v0960.R -- triple-system benchmark on BDsolos.
#
# Loads all 27 UF CSVs from soil_data/embrapa_bdsolos/BD_solos/, audits
# triple-label coverage (SiBCS / WRB / USDA), runs benchmark_bdsolos()
# nation-wide and per-UF, writes:
#   inst/benchmarks/reports/bdsolos_v0960_<DATE>.rds  -- full result list
#   inst/benchmarks/reports/bdsolos_v0960_<DATE>.txt  -- numeric summary
#
# Usage (run after the bdsolos header-line fix lands):
#   Rscript inst/benchmarks/run_bdsolos_v0960.R

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

BD_ROOT <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos"
stopifnot(dir.exists(BD_ROOT))

REPORT_DIR <- "inst/benchmarks/reports"
DATE_TAG   <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT    <- file.path(REPORT_DIR, sprintf("bdsolos_v0960_%s.rds", DATE_TAG))
TXT_OUT    <- file.path(REPORT_DIR, sprintf("bdsolos_v0960_%s.txt", DATE_TAG))

UFS <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
         "RO","RR","RS","SC","SE","SP","TO")

cat("[bdsolos-v0960] loading 27 UF CSVs (preallocated, no O(N^2) c())...\n")
# Preallocate a list-of-lists, flatten ONCE at the end -- avoids the
# O(N^2) repeated `c(all_pedons, pp)` copy that froze the v0.9.59
# audit on small UFs (the previous loop body silently spent its time
# in list-copy, not in load_bdsolos_csv).
ped_chunks <- vector("list", length(UFS))
names(ped_chunks) <- UFS
per_uf_n   <- list()
for (i in seq_along(UFS)) {
  uf <- UFS[[i]]
  f <- file.path(BD_ROOT, paste0(uf, ".csv"))
  if (!file.exists(f)) { cat(sprintf("  %s : MISSING file\n", uf)); next }
  t0 <- Sys.time()
  pp <- tryCatch(load_bdsolos_csv(f, verbose = FALSE),
                  error = function(e) {
                    cat(sprintf("  %s : LOAD FAIL: %s\n", uf, conditionMessage(e)))
                    NULL
                  })
  if (is.null(pp)) next
  for (p in pp) p$site$uf <- uf
  ped_chunks[[uf]] <- pp
  per_uf_n[[uf]]   <- length(pp)
  cat(sprintf("  %s : %4d perfis (%.1fs)\n", uf, length(pp),
              as.numeric(Sys.time() - t0, "secs")))
  invisible(gc(verbose = FALSE))   # avoid R6 GC pressure cumulative slowdown
}
all_pedons <- unlist(ped_chunks, recursive = FALSE, use.names = FALSE)
cat(sprintf("[bdsolos-v0960] total perfis: %d\n\n", length(all_pedons)))

# -- Coverage audit per UF (uses ped_chunks directly, no Filter scan) -----
audit <- do.call(rbind, lapply(UFS, function(uf) {
  pp <- ped_chunks[[uf]]
  if (is.null(pp) || length(pp) == 0L) return(NULL)
  s_ok  <- sum(vapply(pp, function(p) !is.na(p$site$reference_sibcs %||% NA), logical(1L)))
  w_ok  <- sum(vapply(pp, function(p) !is.na(p$site$reference_wrb   %||% NA), logical(1L)))
  st_ok <- sum(vapply(pp, function(p) !is.na(p$site$reference_st    %||% NA), logical(1L)))
  c_ok  <- sum(vapply(pp, function(p) isTRUE(is.finite(p$site$lat)), logical(1L)))
  m_ok  <- sum(vapply(pp, function(p) any(!is.na(p$horizons$munsell_hue_moist)), logical(1L)))
  data.frame(uf = uf, n = length(pp), sibcs = s_ok, wrb = w_ok,
              usda = st_ok, coords = c_ok, munsell = m_ok,
              stringsAsFactors = FALSE)
}))
cat("[bdsolos-v0960] per-UF coverage:\n")
print(audit, row.names = FALSE)

cat("\n[bdsolos-v0960] running benchmark_bdsolos() nation-wide...\n")
t0 <- Sys.time()
nat <- benchmark_bdsolos(all_pedons,
                          systems = c("wrb2022", "sibcs", "usda"),
                          sibcs_level = "order",
                          verbose = TRUE)
dt <- round(as.numeric(Sys.time() - t0, units = "secs"), 1)
cat(sprintf("[bdsolos-v0960] nation-wide done in %.1f s\n\n", dt))

# Per-UF SiBCS-only (the dense path) -- gives per-state breakdown
cat("[bdsolos-v0960] per-UF SiBCS Order benchmark...\n")
per_uf_sibcs <- list()
for (uf in UFS) {
  pp <- ped_chunks[[uf]]
  if (is.null(pp) || length(pp) == 0L) next
  res <- tryCatch(
    benchmark_bdsolos(pp, systems = "sibcs", sibcs_level = "order",
                        verbose = FALSE),
    error = function(e) NULL
  )
  if (is.null(res)) next
  ps <- res$per_system$sibcs
  per_uf_sibcs[[uf]] <- list(
    n_compared = ps$n_compared,
    accuracy   = ps$accuracy
  )
  cat(sprintf("  %s : n=%4d  acc=%.3f\n", uf, ps$n_compared,
              ps$accuracy %||% NA_real_))
}

results <- list(
  audit          = audit,
  nationwide     = nat,
  per_uf_sibcs   = per_uf_sibcs,
  elapsed_total  = dt,
  date           = DATE_TAG,
  soilKey_version = as.character(utils::packageVersion("soilKey"))
)
saveRDS(results, RDS_OUT)
cat(sprintf("\n[bdsolos-v0960] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat("BDsolos triple-system benchmark -- v0.9.60\n")
cat(sprintf("Date           : %s\n", DATE_TAG))
cat(sprintf("soilKey version: %s\n", as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Total perfis   : %d\n", length(all_pedons)))
cat(sprintf("Nation-wide elapsed: %.1f s\n\n", dt))

cat("Per-UF coverage (n / sibcs / wrb / usda / coords / munsell):\n")
print(audit, row.names = FALSE)

cat("\n=== Nation-wide accuracy per system ===\n")
for (sys in names(nat$per_system)) {
  ps <- nat$per_system[[sys]]
  cov <- nat$coverage[[sys]]
  cat(sprintf("  %-7s | label_cov=%5.1f%% (%d/%d)  acc=%6.3f  n_compared=%d  errors=%d\n",
              sys,
              cov$pct, cov$n_with_ref, cov$n_total,
              ps$accuracy %||% NA_real_,
              ps$n_compared, ps$n_errors))
}

cat("\n=== Per-UF SiBCS Order accuracy ===\n")
cat(sprintf("%-3s | %-6s | %-7s\n", "UF", "n", "acc"))
cat(strrep("-", 24), "\n", sep = "")
for (uf in names(per_uf_sibcs)) {
  r <- per_uf_sibcs[[uf]]
  cat(sprintf("%-3s | %-6d | %.3f\n",
              uf, r$n_compared, r$accuracy %||% NA_real_))
}

cat("\n=== Nation-wide SiBCS confusion (top-15 reference orders) ===\n")
cm <- nat$per_system$sibcs$confusion
if (!is.null(cm)) {
  top_orders <- names(sort(rowSums(cm), decreasing = TRUE))[1:min(15, nrow(cm))]
  print(cm[top_orders, intersect(top_orders, colnames(cm)), drop = FALSE])
}
sink()
cat(sprintf("[bdsolos-v0960] wrote %s\n", TXT_OUT))
cat("[bdsolos-v0960] DONE\n")
