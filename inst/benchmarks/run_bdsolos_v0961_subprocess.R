#!/usr/bin/env Rscript
# inst/benchmarks/run_bdsolos_v0961_subprocess.R
#
# At-scale BDsolos 27-UF benchmark via Rscript-per-UF subprocess.
#
# v0.9.60 ran into an R6/PedonRecord accumulation slowdown when more
# than ~2500 R6 objects had been created in a single R session: ES.csv
# (124 perfis, 1s standalone) hung indefinitely after BA + AM + RJ + ...
# had been loaded. Even gc() between UFs did not help. The slowdown
# was not present in fresh R sessions.
#
# Workaround: run load_bdsolos_csv + benchmark_bdsolos in a fresh
# Rscript subprocess per UF, write per-UF result to RDS, then
# aggregate at the end. Fresh R session per UF -> no accumulated state.
#
# Usage:
#   Rscript inst/benchmarks/run_bdsolos_v0961_subprocess.R

REPO   <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/.claude/worktrees/zealous-goldwasser-0bd728"
BD     <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos"
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
REPORT_DIR <- file.path(REPO, "inst/benchmarks/reports")
SCRATCH    <- tempfile("bdsolos_v0961_uf_"); dir.create(SCRATCH)
on.exit(unlink(SCRATCH, recursive = TRUE), add = TRUE)

UFS <- c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA",
         "MG","MS","MT","PA","PB","PE","PI","PR","RJ","RN",
         "RO","RR","RS","SC","SE","SP","TO")

# Worker script written once to scratch.
WORKER <- file.path(SCRATCH, "uf_worker.R")
ENGINE <- Sys.getenv("SOILKEY_ENGINE", unset = "soilkey")
writeLines(c(
  "args <- commandArgs(trailingOnly = TRUE)",
  "uf  <- args[1]; csv_path <- args[2]; out_rds <- args[3]; repo_path <- args[4]",
  "engine <- args[5]",
  "setwd(repo_path)",
  "suppressMessages(suppressWarnings({",
  "  pkgload::load_all('.', quiet = TRUE, helpers = FALSE)",
  "}))",
  "options(soilKey.diagnostic_engine = engine)",
  "t0 <- Sys.time()",
  "peds <- tryCatch(load_bdsolos_csv(csv_path, verbose = FALSE),",
  "                  error = function(e) NULL)",
  "if (is.null(peds) || length(peds) == 0L) {",
  "  saveRDS(list(uf = uf, n = 0L, error = 'load failed'), out_rds)",
  "  quit(save = 'no', status = 0L)",
  "}",
  "t_load <- as.numeric(Sys.time() - t0, 'secs')",
  "t1 <- Sys.time()",
  "res <- benchmark_bdsolos(peds,",
  "                          systems     = c('wrb2022','sibcs','usda'),",
  "                          sibcs_level = 'order',",
  "                          verbose     = FALSE)",
  "t_bench <- as.numeric(Sys.time() - t1, 'secs')",
  "saveRDS(list(",
  "  uf       = uf,",
  "  n        = length(peds),",
  "  load_s   = round(t_load, 1),",
  "  bench_s  = round(t_bench, 1),",
  "  result   = res",
  "), out_rds)",
  "cat(sprintf('[%s] n=%d load=%.1fs bench=%.1fs sibcs_acc=%.3f sibcs_n=%d\\n',",
  "             uf, length(peds), t_load, t_bench,",
  "             res$per_system$sibcs$accuracy %||% NA_real_,",
  "             res$per_system$sibcs$n_compared))"
), WORKER)

cat(sprintf("[bdsolos-v0961] worker: %s\n", WORKER))
cat(sprintf("[bdsolos-v0961] running 27 UFs as subprocess (R session per UF)\n"))

per_uf_res <- list()
total_t0 <- Sys.time()
for (uf in UFS) {
  csv_path <- file.path(BD, paste0(uf, ".csv"))
  if (!file.exists(csv_path)) {
    cat(sprintf("  %s : MISSING\n", uf)); next
  }
  out_rds <- file.path(SCRATCH, paste0(uf, ".rds"))
  cmd <- sprintf("Rscript --no-save --no-restore %s %s %s %s %s %s",
                  shQuote(WORKER), shQuote(uf), shQuote(csv_path),
                  shQuote(out_rds), shQuote(REPO), shQuote(ENGINE))
  status <- system(cmd, intern = FALSE)
  if (file.exists(out_rds)) {
    per_uf_res[[uf]] <- readRDS(out_rds)
  } else {
    cat(sprintf("  %s : SUBPROCESS FAILED (status=%d)\n", uf, status))
  }
}
total_t <- as.numeric(Sys.time() - total_t0, "secs")
cat(sprintf("\n[bdsolos-v0961] all UFs done in %.1f s wall-clock\n", total_t))

# -- Aggregate --
aggregate_per_system <- function(per_uf_res, sys) {
  n_compared <- 0L; n_correct <- 0L; n_with_ref <- 0L; n_total <- 0L
  cm_pooled <- NULL
  for (r in per_uf_res) {
    if (is.null(r) || is.null(r$result)) next
    cov <- r$result$coverage[[sys]]
    if (is.null(cov)) next
    n_total    <- n_total    + cov$n_total
    n_with_ref <- n_with_ref + cov$n_with_ref
    ps <- r$result$per_system[[sys]]
    if (is.null(ps) || is.na(ps$accuracy)) next
    n_compared <- n_compared + ps$n_compared
    n_correct  <- n_correct  + ps$n_correct
    if (!is.null(ps$confusion)) {
      if (is.null(cm_pooled)) {
        cm_pooled <- ps$confusion
      } else {
        cm_pooled <- merge_confusions(cm_pooled, ps$confusion)
      }
    }
  }
  list(n_total = n_total, n_with_ref = n_with_ref,
       n_compared = n_compared, n_correct = n_correct,
       accuracy = if (n_compared > 0L) n_correct / n_compared else NA_real_,
       confusion = cm_pooled)
}
merge_confusions <- function(a, b) {
  rows <- union(rownames(a), rownames(b))
  cols <- union(colnames(a), colnames(b))
  out <- matrix(0L, nrow = length(rows), ncol = length(cols),
                  dimnames = list(reference = rows, predicted = cols))
  for (r in rownames(a)) for (c in colnames(a))
    out[r, c] <- out[r, c] + a[r, c]
  for (r in rownames(b)) for (c in colnames(b))
    out[r, c] <- out[r, c] + b[r, c]
  as.table(out)
}

pooled <- list(
  wrb2022 = aggregate_per_system(per_uf_res, "wrb2022"),
  sibcs   = aggregate_per_system(per_uf_res, "sibcs"),
  usda    = aggregate_per_system(per_uf_res, "usda")
)

per_uf_summary <- do.call(rbind, lapply(per_uf_res, function(r) {
  if (is.null(r) || is.null(r$result)) return(NULL)
  ps <- r$result$per_system$sibcs
  data.frame(uf = r$uf, n = r$n,
              n_compared = ps$n_compared,
              n_correct  = ps$n_correct,
              acc        = ps$accuracy,
              load_s     = r$load_s,
              bench_s    = r$bench_s,
              stringsAsFactors = FALSE)
}))

# -- Report --
tag_suffix <- if (ENGINE == "aqp") "_engine_aqp" else ""
RDS_OUT <- file.path(REPORT_DIR,
                       sprintf("bdsolos_v0961_27uf%s_%s.rds",
                                 tag_suffix, DATE_TAG))
TXT_OUT <- file.path(REPORT_DIR,
                       sprintf("bdsolos_v0961_27uf%s_%s.txt",
                                 tag_suffix, DATE_TAG))

saveRDS(list(per_uf_res = per_uf_res, pooled = pooled,
              per_uf_summary = per_uf_summary, total_s = round(total_t, 1),
              date = DATE_TAG,
              soilKey_version = as.character(utils::packageVersion("soilKey"))),
        RDS_OUT)
cat(sprintf("[bdsolos-v0961] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat(sprintf("BDsolos v0.9.61 27-UF benchmark (subprocess workaround)\n"))
cat(sprintf("Date            : %s\n", DATE_TAG))
cat(sprintf("soilKey version : %s\n", as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Wall-clock total: %.1f s\n", total_t))
cat(sprintf("UFs successful  : %d / 27\n\n", length(per_uf_res)))

cat("Per-UF SiBCS Order:\n")
print(per_uf_summary, row.names = FALSE)

cat("\nTotals (sum of UFs):\n")
cat(sprintf("  perfis loaded       : %d\n",
            sum(per_uf_summary$n)))
cat(sprintf("  perfis with sibcs ref: %d\n",
            pooled$sibcs$n_with_ref))

cat("\n=== Pooled per-system accuracy (nation-wide) ===\n")
for (sys in names(pooled)) {
  ps <- pooled[[sys]]
  cat(sprintf("  %-7s | label_cov=%.1f%% (%d/%d)  acc=%6s  n_compared=%d  n_correct=%d\n",
              sys,
              if (ps$n_total > 0) 100 * ps$n_with_ref / ps$n_total else 0,
              ps$n_with_ref, ps$n_total,
              if (is.na(ps$accuracy)) "NA" else sprintf("%.3f", ps$accuracy),
              ps$n_compared, ps$n_correct))
}

cat("\n=== Pooled SiBCS confusion (top-15 reference orders) ===\n")
cm <- pooled$sibcs$confusion
if (!is.null(cm)) {
  top <- names(sort(rowSums(cm), decreasing = TRUE))[1:min(15, nrow(cm))]
  print(cm[top, intersect(top, colnames(cm)), drop = FALSE])
}
sink()
cat(sprintf("[bdsolos-v0961] wrote %s\n", TXT_OUT))
cat("[bdsolos-v0961] DONE\n")
