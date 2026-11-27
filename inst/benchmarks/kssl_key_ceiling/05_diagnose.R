# Per-order diagnosis of the remaining errors in one scenario, and the key's
# accuracy on pedons whose true order could be decided (data complete enough).
#
# Usage: Rscript 05_diagnose.R <results_dir> [scenario]

args <- commandArgs(trailingOnly = TRUE); res <- args[1]
sc <- if (length(args) >= 2) args[2] else "H"
suppressMessages(library(data.table))
d <- rbindlist(lapply(list.files(res, "^kssl_scenarios.*\\.csv$", full.names = TRUE),
                      fread, colClasses = list(character = "pedon_key")))
d <- unique(d, by = c("scenario", "pedon_key"))[scenario == sc]
d[, `:=`(ref = tolower(ref_order), pred = tolower(pred_order))]

und <- d$err_type == "ref_undecided_missing"
cat(sprintf("Scenario %s: overall %.1f%% | decidable pedons %.1f%% (n=%d) | undecided %d\n\n",
            sc, 100 * mean(d$ref == d$pred), 100 * mean(d[!und, ref == pred]),
            sum(!und), sum(und)))

out <- list()
for (o in d[, .N, by = ref][order(-N)]$ref) {
  x <- d[ref == o]
  et <- x[, .N, by = err_type][order(-N)]
  pr <- x[ref != pred, .N, by = pred][order(-N)]
  xm <- x[err_type == "ref_undecided_missing"]
  ms <- if (nrow(xm)) xm[, .(a = unlist(strsplit(err_detail, ";")))][, .N, by = a][order(-N)] else data.table()
  cat(sprintf("== %s (n=%d, recall %.0f%%)\n", o, nrow(x), 100 * mean(x$ref == x$pred)))
  cat("   errors:   ", paste(sprintf("%s %d", et$err_type, et$N), collapse = " | "), "\n")
  cat("   predicted:", paste(sprintf("%s %d", pr$pred, pr$N), collapse = " | "), "\n")
  if (nrow(ms)) cat("   missing:  ", paste(sprintf("%s %d", head(ms$a, 6), head(ms$N, 6)), collapse = " | "), "\n")
  out[[o]] <- data.table(ref = o, n = nrow(x), recall = mean(x$ref == x$pred),
                         undecided = sum(x$err_type == "ref_undecided_missing"),
                         failed = sum(x$err_type == "ref_failed_criteria"),
                         preempted = sum(grepl("^preempted", x$err_type)))
}
fwrite(rbindlist(out), file.path(res, sprintf("diagnosis_by_order_%s.csv", sc)))
