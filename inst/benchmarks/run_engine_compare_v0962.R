#!/usr/bin/env Rscript
# inst/benchmarks/run_engine_compare_v0962.R
#
# A/B comparison of soilKey hand-coded diagnostics vs aqp::getArgillicBounds
# / getCambicBounds, on the BDsolos RJ.csv (722 perfis, the v0.9.61
# headline dataset). For each pedon, runs both engines for
# argic + cambic, tabulates agreement, and reports per-engine
# pass-rate so we can decide whether to wire aqp into the SiBCS /
# WRB classifier paths in v0.9.63.

suppressWarnings(suppressMessages({
  pkgload::load_all(".", quiet = TRUE, helpers = FALSE)
}))

RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
DATE_TAG <- format(Sys.Date(), "%Y-%m-%d")
RDS_OUT  <- file.path("inst/benchmarks/reports",
                       sprintf("engine_compare_v0962_RJ_%s.rds", DATE_TAG))
TXT_OUT  <- file.path("inst/benchmarks/reports",
                       sprintf("engine_compare_v0962_RJ_%s.txt", DATE_TAG))

cat("[engine-compare] loading RJ.csv (722 perfis expected)...\n")
peds <- load_bdsolos_csv(RJ, verbose = FALSE)
cat(sprintf("[engine-compare] loaded %d perfis\n\n", length(peds)))

run_pair <- function(p, diag) {
  cmp <- tryCatch(compare_engines(p, diag),
                   error = function(e) NULL)
  if (is.null(cmp)) {
    return(list(soilkey = NA, aqp = NA, agree = NA, error = TRUE))
  }
  list(
    soilkey = isTRUE(cmp$soilkey$passed),
    aqp     = isTRUE(cmp$aqp$passed),
    agree   = isTRUE(cmp$agree),
    error   = FALSE
  )
}

cat("[engine-compare] running argic on all perfis...\n")
t0 <- Sys.time()
arg_rows <- lapply(seq_along(peds), function(i) {
  r <- run_pair(peds[[i]], "argic")
  c(idx = i, id = peds[[i]]$site$id %||% NA_character_,
    diagnostic = "argic", r)
})
cat(sprintf("[engine-compare] argic done in %.1f s\n",
            as.numeric(Sys.time() - t0, "secs")))

cat("[engine-compare] running cambic on all perfis...\n")
t1 <- Sys.time()
cam_rows <- lapply(seq_along(peds), function(i) {
  r <- run_pair(peds[[i]], "cambic")
  c(idx = i, id = peds[[i]]$site$id %||% NA_character_,
    diagnostic = "cambic", r)
})
cat(sprintf("[engine-compare] cambic done in %.1f s\n",
            as.numeric(Sys.time() - t1, "secs")))

all_rows <- do.call(rbind,
                     lapply(c(arg_rows, cam_rows),
                              function(r) as.data.frame(r,
                                                          stringsAsFactors = FALSE)))

# Per-engine pass-rate + agreement
for_diag <- function(d) {
  sub <- all_rows[all_rows$diagnostic == d, , drop = FALSE]
  list(
    n         = nrow(sub),
    n_errors  = sum(sub$error == TRUE | sub$error == "TRUE", na.rm = TRUE),
    soilkey_pass = sum(sub$soilkey == TRUE | sub$soilkey == "TRUE", na.rm = TRUE),
    aqp_pass     = sum(sub$aqp     == TRUE | sub$aqp     == "TRUE", na.rm = TRUE),
    agree        = sum(sub$agree   == TRUE | sub$agree   == "TRUE", na.rm = TRUE),
    disagree_only_sk = sum((sub$soilkey == TRUE | sub$soilkey == "TRUE") &
                              !(sub$aqp == TRUE | sub$aqp == "TRUE"), na.rm = TRUE),
    disagree_only_aq = sum(!(sub$soilkey == TRUE | sub$soilkey == "TRUE") &
                              (sub$aqp == TRUE | sub$aqp == "TRUE"), na.rm = TRUE)
  )
}

argic_stats  <- for_diag("argic")
cambic_stats <- for_diag("cambic")

result <- list(
  date = DATE_TAG,
  soilKey_version = as.character(utils::packageVersion("soilKey")),
  n_pedons = length(peds),
  rows = all_rows,
  argic_stats = argic_stats,
  cambic_stats = cambic_stats
)
saveRDS(result, RDS_OUT)
cat(sprintf("[engine-compare] wrote %s\n", RDS_OUT))

sink(TXT_OUT)
cat("Engine A/B comparison: soilKey vs aqp -- v0.9.62\n")
cat(sprintf("Date           : %s\n", DATE_TAG))
cat(sprintf("soilKey version: %s\n",
            as.character(utils::packageVersion("soilKey"))))
cat(sprintf("Pedons         : %d (BDsolos RJ.csv)\n\n", length(peds)))

prn <- function(d, st) {
  cat(sprintf("=== %s ===\n", d))
  cat(sprintf("  n              : %d\n",   st$n))
  cat(sprintf("  errors         : %d\n",   st$n_errors))
  cat(sprintf("  soilkey passes : %d  (%.1f%%)\n",
              st$soilkey_pass, 100 * st$soilkey_pass / st$n))
  cat(sprintf("  aqp     passes : %d  (%.1f%%)\n",
              st$aqp_pass, 100 * st$aqp_pass / st$n))
  cat(sprintf("  agree          : %d  (%.1f%%)\n",
              st$agree, 100 * st$agree / st$n))
  cat(sprintf("  disagree (sk only): %d\n",
              st$disagree_only_sk))
  cat(sprintf("  disagree (aqp only): %d\n",
              st$disagree_only_aq))
  cat("\n")
}
prn("argic",  argic_stats)
prn("cambic", cambic_stats)

cat("Interpretation:\n")
cat("- 'agree' rate >> 80% -> engines are roughly interchangeable.\n")
cat("- aqp passes >> soilkey passes -> aqp is more permissive (KST 13ed thresholds).\n")
cat("- aqp passes << soilkey passes -> aqp is stricter (require_t = FALSE may help).\n")
cat("\nNext step: integrate the engine that better predicts the SiBCS\n")
cat("Argissolos vs Latossolos boundary on this dataset.\n")
sink()
cat(sprintf("[engine-compare] wrote %s\n", TXT_OUT))
cat("[engine-compare] DONE\n")
