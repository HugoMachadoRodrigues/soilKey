# =============================================================================
# v0.9.106 -- run_all_benchmarks(): one reproducible entry point that runs
# every benchmark whose data is available and writes a consolidated report.
#
# Replaces the manual "source 22 run_*.R scripts by hand" workflow. It is
# tolerant: datasets whose files are absent are skipped with a note, the
# offline canonical-fixture sanity row always runs, and offline lazy-fetch
# samples (AfSP) are added when their .rds is present. The classification
# engine is not touched -- this only measures it.
# =============================================================================


#' Run the full soilKey benchmark suite and (optionally) write a report
#'
#' Auto-detects which reference datasets are available locally, runs each via
#' \code{\link{benchmark_unified}}, adds the offline canonical sanity row and
#' the AfSP sample when present, and returns a tidy accuracy summary. When
#' \code{report_path} is given, a consolidated Markdown report is written.
#'
#' @param datasets \code{"auto"} (default) detects available datasets;
#'        otherwise any subset of \code{c("bdsolos", "febr", "kssl",
#'        "lucas_esdb", "redape")}, the literal \code{"canonical"} (only the
#'        fixture sanity row), or \code{"all"} (every dataset regardless of
#'        availability -- absent ones are skipped).
#' @param paths Named list of dataset paths (see
#'        \code{\link{benchmark_unified}}). \code{NULL} uses the package
#'        defaults (override the root via
#'        \code{options(soilKey.benchmark_root = "...")}).
#' @param max_n Cap on pedons per dataset (keeps the run fast). Default 300.
#' @param level Comparison level forwarded where supported (currently the
#'        suite reports at \code{"order"} / top level).
#' @param report_path File to write the Markdown report to, \code{TRUE} to
#'        auto-name one under \code{inst/benchmarks/reports/}, or \code{NULL}
#'        (default) for no file.
#' @param verbose Print progress.
#' @return Invisibly, a list with \code{summary} (data.frame: dataset, system,
#'         n_compared, accuracy), \code{per_system} (pooled), \code{raw}
#'         (full \code{benchmark_unified} output), \code{weak} (zero-recall
#'         classes) and \code{config}.
#' @seealso \code{\link{benchmark_unified}}, \code{\link{benchmark_redape}}.
#' @examples
#' \dontrun{
#' res <- run_all_benchmarks(max_n = 250,
#'                           report_path = TRUE)
#' res$summary
#' }
#' @export
run_all_benchmarks <- function(datasets = "auto", paths = NULL, max_n = 300L,
                               level = "order", report_path = NULL,
                               verbose = TRUE) {
  paths <- paths %||% .benchmark_default_paths()
  say <- function(...) if (isTRUE(verbose)) cli::cli_alert_info(sprintf(...))

  canonical_only <- identical(datasets, "canonical")
  if (identical(datasets, "auto")) {
    datasets <- .benchmark_available_datasets(paths)
    say("Auto-detected datasets: %s",
        if (length(datasets)) paste(datasets, collapse = ", ") else "(none)")
  } else if (identical(datasets, "all")) {
    datasets <- c("bdsolos", "febr", "kssl", "lucas_esdb", "redape")
  } else if (canonical_only) {
    datasets <- character(0)
  }

  rows <- list()
  raw  <- NULL

  # ---- reference datasets via benchmark_unified --------------------------
  if (length(datasets)) {
    present <- intersect(datasets, .benchmark_available_datasets(paths))
    missing <- setdiff(datasets, present)
    if (length(missing))
      say("Skipping (data absent): %s", paste(missing, collapse = ", "))
    if (length(present)) {
      raw <- tryCatch(
        benchmark_unified(systems = "all", datasets = present, paths = paths,
                          max_n_per_dataset = max_n, verbose = verbose),
        error = function(e) {
          cli::cli_alert_warning(sprintf("benchmark_unified failed: %s",
                                         conditionMessage(e)))
          NULL
        })
      if (!is.null(raw)) {
        for (tag in names(raw$per_system_per_dataset)) {
          r  <- raw$per_system_per_dataset[[tag]]
          ds <- sub("/.*$", "", tag); sy <- sub("^.*/", "", tag)
          rows[[tag]] <- .suite_row(ds, sy, r$n_compared, r$accuracy,
                                    confusion = r$confusion)
        }
      }
    }
  }

  # ---- AfSP offline sample (WRB) -----------------------------------------
  afsp <- tryCatch(
    if (exists("load_afsp_sample", mode = "function"))
      .suite_run_afsp(max_n, verbose) else NULL,
    error = function(e) NULL)
  if (!is.null(afsp)) rows[["afsp_sample/wrb2022"]] <- afsp

  # ---- canonical fixture sanity row (always) -----------------------------
  rows[["canonical/all"]] <- .suite_canonical_row()

  summary <- do.call(rbind, rows)
  rownames(summary) <- NULL
  summary <- summary[order(summary$dataset, summary$system), , drop = FALSE]

  weak <- if (!is.null(raw)) .suite_weak_classes(raw) else list()

  config <- list(
    soilKey_version = as.character(utils::packageVersion("soilKey")),
    max_n = max_n, level = level,
    datasets_run = unique(summary$dataset))

  if (!is.null(report_path) && !isFALSE(report_path)) {
    if (isTRUE(report_path)) {
      dir <- file.path("inst", "benchmarks", "reports")
      report_path <- file.path(
        dir, sprintf("benchmark_suite_v%s.md",
                     gsub("[.]", "", config$soilKey_version)))
    }
    md <- .suite_report_md(summary, weak, config)
    dir.create(dirname(report_path), recursive = TRUE, showWarnings = FALSE)
    writeLines(md, report_path)
    say("Report written: %s", report_path)
  }

  invisible(list(summary = summary,
                 per_system = if (!is.null(raw)) raw$per_system else NULL,
                 raw = raw, weak = weak, config = config))
}


# ---- internals -------------------------------------------------------------

# Build one summary row with the full v0.9.110 metric set. Every row (unified,
# AfSP, canonical) goes through this so the rbind'd summary has identical
# columns. Metrics derive from the row's own confusion matrix (NA when absent,
# e.g. the canonical coverage row). The flag annotates rows the reader must not
# over-interpret: a tiny n, or the LUCAS topsoil-only lower bound.
.suite_row <- function(dataset, system, n_compared, accuracy,
                       confusion = NULL) {
  m  <- if (is.null(confusion)) NULL
        else .benchmark_metrics_from_confusion(confusion)
  ci <- if (is.null(confusion)) NULL
        else .benchmark_bootstrap_metrics(confusion)
  n_compared <- as.integer(n_compared %||% 0L)
  # When a confusion matrix is present, take accuracy FROM it so the point
  # estimate and the bootstrap CI are always internally consistent (they are
  # the same number in real data anyway); else fall back to the passed value.
  accuracy <- if (!is.null(m) && !is.na(m$accuracy)) m$accuracy
              else accuracy %||% NA_real_
  flag <- if (identical(dataset, "lucas_esdb")) "lower-bound (topsoil-only)"
          else if (n_compared > 0L && n_compared < 30L) "n<30 -- indicative only"
          else ""
  data.frame(
    dataset      = dataset, system = system,
    n_compared   = n_compared,
    accuracy     = accuracy,
    acc_lo       = if (is.null(ci)) NA_real_ else ci$accuracy[1],
    acc_hi       = if (is.null(ci)) NA_real_ else ci$accuracy[2],
    balanced_acc = if (is.null(m)) NA_real_ else m$balanced_accuracy,
    macro_f1     = if (is.null(m)) NA_real_ else m$macro_f1,
    kappa        = if (is.null(m)) NA_real_ else m$kappa,
    nir          = if (is.null(m)) NA_real_ else m$nir,
    flag         = flag,
    stringsAsFactors = FALSE)
}

# Classify every canonical fixture under all three systems; the row reports
# the share that classify to a non-NA name (a coverage sanity check ~ 100%).
.suite_canonical_row <- function() {
  fx <- grep("^make_.*_canonical$", ls(asNamespace("soilKey")), value = TRUE)
  n_ok <- 0L; n_tot <- 0L
  for (f in fx) {
    pr <- tryCatch(get(f, asNamespace("soilKey"))(), error = function(e) NULL)
    if (is.null(pr)) next
    res <- tryCatch(classify_all(pr, on_missing = "silent"),
                    error = function(e) NULL)
    if (is.null(res)) next
    for (nm in c("wrb", "sibcs", "usda")) {
      n_tot <- n_tot + 1L
      if (!is.null(res[[nm]]) && !is.na(res[[nm]]$name %||% NA)) n_ok <- n_ok + 1L
    }
  }
  .suite_row("canonical", "all", n_tot,
             if (n_tot > 0L) n_ok / n_tot else NA_real_, confusion = NULL)
}

# AfSP offline sample, classified under WRB.
.suite_run_afsp <- function(max_n, verbose) {
  peds <- tryCatch(load_afsp_sample(), error = function(e) NULL)
  if (is.null(peds) || !length(peds)) return(NULL)
  if (!is.null(max_n)) peds <- peds[seq_len(min(max_n, length(peds)))]
  res <- tryCatch(benchmark_afsp(peds, verbose = FALSE), error = function(e) NULL)
  if (is.null(res)) return(NULL)
  # AfSP carries its own confusion matrix -> gets the full metric set, making it
  # a second independent OFFLINE WRB benchmark (Africa, RSG level) alongside FEBR.
  .suite_row("afsp_sample", "wrb2022",
             res$n_compared %||% res$n_in_scope %||% 0L,
             res$accuracy %||% NA_real_, confusion = res$confusion)
}

# Zero-recall classes per (dataset, system) from the pooled confusion data.
.suite_weak_classes <- function(raw) {
  out <- list()
  for (tag in names(raw$per_system_per_dataset)) {
    pc <- raw$per_system_per_dataset[[tag]]$per_class
    if (is.null(pc) || !nrow(pc)) next
    rec_col <- intersect(c("recall"), names(pc))
    if (!length(rec_col)) next
    zero <- pc[!is.na(pc$recall) & pc$recall == 0, , drop = FALSE]
    if (nrow(zero)) {
      cls_col <- intersect(c("reference_rsg", "rsg_code", "class"), names(zero))[1]
      out[[tag]] <- as.character(zero[[cls_col]])
    }
  }
  out
}

# Render the consolidated Markdown report.
.suite_report_md <- function(summary, weak, config) {
  pct  <- function(x) if (is.null(x) || is.na(x)) "n/a"
                      else sprintf("%.1f%%", 100 * x)
  has  <- function(col) col %in% names(summary)
  cell <- function(i, col) if (has(col)) summary[[col]][i] else NA_real_
  acc_ci <- function(i) {
    a <- pct(cell(i, "accuracy"))
    lo <- cell(i, "acc_lo"); hi <- cell(i, "acc_hi")
    if (is.na(lo) || is.na(hi)) a
    else sprintf("%s (%s-%s)", a, pct(lo), pct(hi))
  }
  flagcell <- function(i) { f <- cell(i, "flag"); if (is.na(f)) "" else f }

  lines <- c(
    sprintf("# soilKey benchmark suite -- v%s", config$soilKey_version),
    "",
    sprintf("Generated by `run_all_benchmarks()` (max_n = %s, level = %s).",
            config$max_n, config$level),
    "",
    "## Accuracy by dataset x system",
    "",
    paste("Headline metric for imbalanced classes is **balanced accuracy /",
          "macro-F1**, read against the **NIR** (no-information-rate)",
          "majority-class baseline. Point accuracy carries a bootstrap 95% CI."),
    "",
    "| Dataset | System | n | Accuracy [95% CI] | Bal. acc | Macro-F1 | Kappa | NIR | Flag |",
    "|---------|--------|--:|-------------------|---------:|---------:|------:|----:|------|")
  for (i in seq_len(nrow(summary))) {
    lines <- c(lines, sprintf(
      "| %s | %s | %d | %s | %s | %s | %s | %s | %s |",
      summary$dataset[i], summary$system[i], summary$n_compared[i],
      acc_ci(i), pct(cell(i, "balanced_acc")), pct(cell(i, "macro_f1")),
      if (is.na(cell(i, "kappa"))) "n/a" else sprintf("%.2f", cell(i, "kappa")),
      pct(cell(i, "nir")), flagcell(i)))
  }
  if (length(weak)) {
    lines <- c(lines, "", "## Zero-recall classes (improvement targets)", "")
    for (tag in names(weak))
      lines <- c(lines, sprintf("- **%s**: %s", tag,
                                paste(weak[[tag]], collapse = ", ")))
  }
  lines <- c(lines, "", "## Notes", "",
    paste("- The **canonical** row is an offline fixture sanity check",
          "(coverage, not field accuracy); it has no confusion matrix, so its",
          "per-class metrics are blank."),
    paste("- Rows flagged **n<30** are statistically indicative only.",
          "External-dataset rows reflect the local data snapshot and `max_n`."),
    paste("- **lucas_esdb/wrb2022** is a topsoil-only **lower bound** (LUCAS",
          "ships 0-20 cm chemistry only); the honest WRB-at-scale number is the",
          "morphologically-complete **FEBR** row. For a LUCAS estimate with a",
          "synthetic subsoil, run the opt-in (network, ~1 h):",
          "`benchmark_lucas_2018(pedons, fill_subsoil_from = \"soilgrids\")`."),
    paste("- **kssl/usda** uses a head-N (not random) sample of the gpkg;",
          "**bdsolos** accumulates leading (state-clustered) CSVs until the",
          "label cap is met. Both are documented samples, not full random",
          "draws."))
  lines
}
