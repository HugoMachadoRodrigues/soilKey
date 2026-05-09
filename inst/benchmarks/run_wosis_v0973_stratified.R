#!/usr/bin/env Rscript
# inst/benchmarks/run_wosis_v0973_stratified.R
#
# v0.9.73 -- stratified WoSIS WRB benchmark.
#
# The flagship WoSIS GraphQL pull (continent = "South America",
# n_max = 200+, unfiltered) hit "statement timeout" errors on the
# ISRIC server side as of 2026-05-09. RSG-filtered small queries
# (n_max = 5, wrb_rsg = "<one>") DO work, so v0.9.73 ships:
#
#  - load_wosis_stratified_sample(): bundled cache of 130 profiles,
#    5 per RSG x 26 RSGs, pulled 2026-05-09.
#  - This driver: re-runs the benchmark on the cache and reports
#    overall + per-RSG accuracy under each fallback configuration.

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

s <- load_wosis_stratified_sample()
peds <- s$pedons
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT  <- file.path("inst/benchmarks/reports",
                        sprintf("wosis_v0973_stratified_%s.rds", DATE_TAG))
TXT_OUT  <- file.path("inst/benchmarks/reports",
                        sprintf("wosis_v0973_stratified_%s.md", DATE_TAG))
dir.create(dirname(RDS_OUT), recursive = TRUE, showWarnings = FALSE)

cat(sprintf("[wosis-v0973] %d pedons (5 per RSG x 26 RSGs)\n", length(peds)))

normalize_pred <- function(p) if (is.na(p)) NA_character_ else sub("s$", "", p)

run_one <- function(label, opts) {
  cat(sprintf("\n[wosis-v0973] === %s ===\n", label))
  for (k in names(opts)) options(setNames(list(opts[[k]]), k))
  on.exit(for (k in names(opts)) options(setNames(list(NULL), k)), add = TRUE)
  t0 <- Sys.time()
  preds <- vapply(peds, function(pr) {
    res <- tryCatch(classify_wrb2022(pr, on_missing = "silent"),
                     error = function(e) NULL)
    if (is.null(res)) NA_character_ else res$rsg_or_order
  }, character(1))
  refs <- vapply(peds, function(p) p$site$wosis_rsg %||% NA_character_, character(1))
  preds_n <- vapply(preds, normalize_pred, character(1))
  in_scope <- !is.na(refs) & !is.na(preds_n)
  n_correct <- sum(in_scope & refs == preds_n)
  n_total   <- sum(in_scope)
  acc <- if (n_total > 0) n_correct / n_total else NA_real_
  dt <- as.numeric(Sys.time() - t0, "secs")
  cat(sprintf("  %.1fs, top-1 = %d/%d (%.1f%%)\n",
              dt, n_correct, n_total, 100 * acc))
  list(label = label, opts = opts, acc = acc,
       n_correct = n_correct, n_total = n_total,
       refs = refs, preds = preds_n, in_scope = in_scope)
}

results <- list(
  baseline = run_one("baseline (no opt-ins)", list()),
  aqp      = run_one("+aqp engine",
                       list(soilKey.diagnostic_engine = "aqp")),
  fbck     = run_one("+aqp + ECEC + tex-morph (v0.9.69-70)",
                       list(soilKey.diagnostic_engine = "aqp",
                            soilKey.ferralic_ecec_fallback = TRUE,
                            soilKey.ferralic_texture_morphological_fallback = TRUE)),
  full     = run_one("+full v0.9.69-72 stack (g/f/v inferences)",
                       list(soilKey.diagnostic_engine = "aqp",
                            soilKey.ferralic_ecec_fallback = TRUE,
                            soilKey.ferralic_texture_morphological_fallback = TRUE,
                            soilKey.gleyic_designation_inference = TRUE,
                            soilKey.plinthic_designation_inference = TRUE,
                            soilKey.vertic_designation_inference = TRUE))
)

# Per-RSG comparison
b <- results$baseline; f <- results$full
rsgs <- sort(unique(b$refs[b$in_scope]))
per_rsg <- do.call(rbind, lapply(rsgs, function(rsg) {
  m <- b$refs == rsg & b$in_scope
  data.frame(rsg = rsg, n = sum(m),
              base_correct = sum(m & b$preds == rsg),
              full_correct = sum(m & f$preds == rsg))
}))
per_rsg$delta <- per_rsg$full_correct - per_rsg$base_correct

saveRDS(list(date = DATE_TAG, n_pedons = length(peds),
              soilKey_version = as.character(utils::packageVersion("soilKey")),
              results = results, per_rsg = per_rsg), RDS_OUT)

sink(TXT_OUT)
cat(sprintf("# WoSIS stratified benchmark report -- v%s -- %s\n",
             utils::packageVersion("soilKey"), DATE_TAG))
cat("\n**Source:** `load_wosis_stratified_sample()` (130 pedons, 5 per RSG x 26 RSGs)\n\n")
cat("## Top-1 accuracy ladder\n\n")
cat("| Configuration | Accuracy |\n|---|---:|\n")
for (r in results) {
  cat(sprintf("| %s | %d/%d (%.1f%%) |\n",
              r$label, r$n_correct, r$n_total, 100 * r$acc))
}
cat("\n## Per-RSG recall (baseline vs full v0.9.72 stack)\n\n")
cat("| RSG | n | baseline | +full | delta |\n|---|---:|---:|---:|---:|\n")
for (i in seq_len(nrow(per_rsg))) {
  cat(sprintf("| %s | %d | %d | %d | %+d |\n",
              per_rsg$rsg[i], per_rsg$n[i],
              per_rsg$base_correct[i], per_rsg$full_correct[i], per_rsg$delta[i]))
}
sink()

cat(sprintf("\n[wosis-v0973] wrote %s\n", RDS_OUT))
cat(sprintf("[wosis-v0973] wrote %s\n", TXT_OUT))
