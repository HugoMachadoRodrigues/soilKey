# Summarise the key-ceiling scenarios: accuracy per level, error anatomy,
# per-order recall, and agreement of climate-derived regimes with the record.
#
# Usage: Rscript 04_summarise.R <results_dir>

args <- commandArgs(trailingOnly = TRUE); res <- args[1]
suppressMessages(library(data.table))

fs <- list.files(res, "^kssl_scenarios.*\\.csv$", full.names = TRUE)
d  <- unique(rbindlist(lapply(fs, fread, colClasses = list(character = "pedon_key"))),
             by = c("scenario", "pedon_key"))
norm <- function(x) tolower(trimws(x))
d[, `:=`(ref_order = norm(ref_order), pred_order = norm(pred_order),
         ref_suborder = norm(ref_suborder), pred_suborder = norm(pred_suborder),
         ref_gg = norm(ref_gg), pred_gg = norm(pred_gg))]

kappa <- function(r, p) {
  lv <- union(r, p); t <- table(factor(r, lv), factor(p, lv)); n <- sum(t)
  po <- sum(diag(t)) / n; pe <- sum(rowSums(t) * colSums(t)) / n^2
  (po - pe) / (1 - pe)
}
balacc <- function(r, p) mean(tapply(r == p, r, mean))
ci <- function(k, n) { b <- binom.test(k, n)$conf.int * 100; sprintf("%.1f–%.1f", b[1], b[2]) }

lab <- c(A = "lab only", B = "+ NASIS morphology", C = "+ designation inference",
         D = "C + recorded STR (oracle)", E = "C + recorded SMR (oracle)",
         F = "C + both recorded (oracle)", G = "C + PRISM climate (Thornthwaite)",
         H = "C + PRISM climate (Hargreaves)",
         S = "SOLUS properties + climate", U = "S + lab designations",
         T = "S at SOLUS depth slices")

acc <- d[, {
  ok <- ref_order == pred_order
  list(label = lab[scenario[1]], n = .N,
       order_acc = round(100 * mean(ok, na.rm = TRUE), 1),
       order_ci = ci(sum(ok, na.rm = TRUE), sum(!is.na(ok))),
       order_balacc = round(100 * balacc(ref_order, pred_order), 1),
       order_kappa = round(kappa(ref_order, pred_order), 3),
       suborder_acc = round(100 * mean(ref_suborder == pred_suborder, na.rm = TRUE), 1),
       gg_acc = round(100 * mean(ref_gg == pred_gg, na.rm = TRUE), 1))
}, by = scenario][order(scenario)]
nir <- d[scenario == "A", round(100 * max(table(ref_order)) / .N, 1)]
cat("\nNo-information rate (majority order):", nir, "%\n\n")
print(acc)
fwrite(acc, file.path(res, "summary_accuracy.csv"))

# error anatomy: preemption (another order fired first) vs the true order failing
d[, err_class := fifelse(err_type == "correct", "correct",
                 fifelse(grepl("^preempted", err_type), "preempted by an earlier order",
                 fifelse(err_type == "ref_undecided_missing", "true order undecided (missing data)",
                 fifelse(err_type == "ref_failed_criteria", "true order failed its criteria",
                         "other"))))]
an <- dcast(d[, .N, by = .(scenario, err_class)], err_class ~ scenario, value.var = "N", fill = 0)
cat("\nError anatomy (counts):\n"); print(an)
fwrite(an, file.path(res, "summary_error_anatomy.csv"))

pre <- d[grepl("^preempted", err_type), .N, by = .(scenario, err_type)][order(scenario, -N)]
cat("\nTop pre-empting orders:\n"); print(pre[, head(.SD, 4), by = scenario])
fwrite(pre, file.path(res, "summary_preemption.csv"))

miss <- d[err_type == "ref_undecided_missing",
          .(attr = unlist(strsplit(err_detail, ";"))), by = .(scenario, ref_order)]
mt <- miss[, .N, by = .(scenario, attr)][order(scenario, -N)]
cat("\nMost frequent missing attributes blocking the true order:\n")
print(mt[, head(.SD, 6), by = scenario])
fwrite(mt, file.path(res, "summary_missing_attributes.csv"))

rec <- dcast(d[, .(recall = round(100 * mean(pred_order == ref_order, na.rm = TRUE), 0)),
               by = .(scenario, ref_order)],
             ref_order ~ scenario, value.var = "recall")
rec <- merge(d[scenario == "A", .(n = .N), by = ref_order], rec, by = "ref_order")
cat("\nRecall per reference order (%):\n"); print(rec[order(-n)])
fwrite(rec, file.path(res, "summary_recall_by_order.csv"))

conf <- d[scenario %in% c("C", "G", "H", "S", "T", "U"), .N, by = .(scenario, ref_order, pred_order)]
fwrite(conf, file.path(res, "confusion_C_G.csv"))

# climate vs recorded regimes
fc <- file.path(res, "kssl_climate_regimes.csv")
if (file.exists(fc)) {
  cl <- fread(fc, colClasses = list(character = "pedon_key"))
  x  <- merge(cl, d[scenario == "A", .(pedon_key, smr_oracle, str_oracle)], by = "pedon_key")
  x[smr_oracle == "", smr_oracle := NA]; x[str_oracle == "", str_oracle := NA]
  s  <- x[!is.na(smr_oracle) & smr_oracle != "aquic"]
  cat("\nSMR climate vs record (aquic excluded): Thornthwaite",
      round(100 * mean(s$smr_clim == s$smr_oracle), 1), "% | Hargreaves",
      round(100 * mean(s$smr_clim_hg == s$smr_oracle), 1), "% of ", nrow(s), "\n")
  print(s[, .N, by = .(record = smr_oracle, hargreaves = smr_clim_hg)][order(-N)][1:10])
  print(s[, .N, by = .(record = smr_oracle, climate = smr_clim)][order(-N)][1:10])
  t  <- x[!is.na(str_oracle)]
  cat("STR climate vs record: ", round(100 * mean(t$str_clim == t$str_oracle), 1),
      "% of ", nrow(t), "\n")
}
