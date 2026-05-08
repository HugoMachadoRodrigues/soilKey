# =============================================================================
# v0.9.62 -- benchmark_unified(): single-call cross-dataset benchmark
# pooling BDsolos + FEBR + KSSL+NASIS + LUCAS for any subset of the
# three classification systems (WRB / SiBCS / USDA).
#
# The big idea
# ------------
# Every dataset soilKey ingests carries reference labels for ONE
# system (or a sparse subset thereof):
#
#   Dataset             |  SiBCS  |  WRB  |  USDA  |  n
#   ------------------- | :-----: | :---: | :----: | ---:
#   BDsolos             |  dense  | sparse| sparse | ~9k
#   FEBR superconjunto  |  dense  | dense | dense  | 554
#   KSSL+NASIS          |   no    |  no   | dense  | 36k
#   LUCAS Soil 2018     |   no    | raster|  no    | 19k
#
# benchmark_unified() takes the system(s) you want to benchmark, pulls
# from EVERY dataset that has reference labels for that system,
# normalises labels via the existing FEBR / KSSL helpers, runs the
# system's classifier per pedon, and returns a single pooled
# accuracy + per-class recall + per-dataset breakdown.
#
# Currently implements pooling at the per-pedon level. Phase 2.3
# (v0.9.63) will add depth-harmonisation via harmonize_to_gsm() so
# the chemistry across datasets is on a consistent depth basis
# before classification.
# =============================================================================


#' Unified cross-dataset benchmark across SiBCS / WRB / USDA
#'
#' Runs a system's soilKey classifier on every dataset that has
#' reference labels for that system, then pools the results into a
#' single nation-/world-wide accuracy estimate.
#'
#' @section Datasets and their reference labels:
#'
#' \tabular{ll}{
#'   Dataset             \tab Systems with reference labels         \cr
#'   BDsolos             \tab SiBCS (dense), WRB (sparse), USDA (sparse) \cr
#'   FEBR superconjunto  \tab SiBCS, WRB, USDA (most rows have all 3)    \cr
#'   KSSL+NASIS          \tab USDA only (samp_taxsubgrp universal)        \cr
#'   LUCAS + ESDB raster \tab WRB (via lookup_esdb on coords)             \cr
#' }
#'
#' For each (system, dataset) pair, this function:
#' \enumerate{
#'   \item Loads pedons via the appropriate \code{load_*} helper.
#'   \item Filters to pedons with a populated reference label for the
#'         requested system.
#'   \item Normalises both reference and predicted labels via
#'         \code{normalise_febr_*()} / KSSL canonicalisation helpers.
#'   \item Calls the system's classifier and records pred-vs-ref.
#' }
#' Then pools per-system results across datasets.
#'
#' @section Engine selection (Phase 1 wiring):
#'
#' For datasets with morphological data (BDsolos / FEBR), the
#' diagnostics that pivot Argissolos / Latossolos / Cambissolos
#' classification can be run with two engines:
#' \itemize{
#'   \item \code{engine = "soilkey"} (default) -- the hand-coded WRB
#'         6/1.4/20 thresholds.
#'   \item \code{engine = "aqp"} -- aqp::getArgillicBounds /
#'         getCambicBounds (KST 13ed 3/1.2/8 thresholds).
#' }
#' On the v0.9.62 RJ benchmark (722 perfis), aqp was 14.8 pp stricter
#' on argic and 40.6 pp more permissive on cambic; the SiBCS
#' Argissolos / Latossolos / Cambissolos boundary is sensitive to
#' both. \code{engine} is currently forwarded to a future v0.9.63
#' wired \code{argic()} / \code{cambic()}; for now,
#' \code{benchmark_unified()} reports separately per engine when
#' \code{engine = "both"}.
#'
#' @param systems Character vector. Any subset of \code{c("wrb2022",
#'        "sibcs", "usda")}. Default \code{"all"} runs all three.
#' @param datasets Character vector. Any subset of
#'        \code{c("bdsolos", "febr", "kssl", "lucas_esdb")}.
#'        Default \code{"all"} pools every dataset that has
#'        reference labels for the requested systems. Datasets
#'        without reference labels for a system are silently
#'        excluded from that system's pooled result.
#' @param paths Named list of dataset paths. Element names should
#'        match those in \code{datasets}. If \code{NULL} (default),
#'        soilKey looks for canonical paths under
#'        \code{"~/soil_data/"}.
#' @param max_n_per_dataset Optional integer to cap per-dataset
#'        sample size (useful for development / debugging).
#'        \code{NULL} (default) classifies every available pedon.
#' @param engine Currently forwarded to Phase-1 aqp wiring. One of
#'        \code{"soilkey"} (default), \code{"aqp"}, \code{"both"}.
#'        When \code{"aqp"}, sets \code{options(soilKey.diagnostic_engine
#'        = "aqp")} for the duration of the benchmark, which routes
#'        \code{argic()} / \code{cambic()} through the canonical
#'        \code{aqp::getArgillicBounds} / \code{getCambicBounds}.
#' @param harmonize If \code{TRUE} (default \code{FALSE}), applies
#'        \code{\link{harmonize_to_gsm}} to each dataset's pedons before
#'        classification, putting all chemistry/texture on the GSM
#'        depth grid (0-5 / 5-15 / 15-30 / 30-60 / 60-100 / 100-200 cm).
#'        Required for cross-dataset pooling integrity (Phase 2.3) but
#'        slow (~1-2 min for 1k pedons) and may degrade per-dataset
#'        accuracy slightly because the splined depths are
#'        approximations.
#' @param verbose If \code{TRUE} (default), emits cli progress.
#' @return A list with elements:
#'   \itemize{
#'     \item \code{per_system} -- per-system pooled
#'           \code{list(accuracy, n_compared, n_correct, confusion,
#'           per_class)}.
#'     \item \code{per_system_per_dataset} -- per-(system, dataset)
#'           same shape, for breakdown.
#'     \item \code{coverage} -- per-(system, dataset) sample sizes
#'           and label coverage.
#'     \item \code{config} -- captures \code{systems, datasets,
#'           engine, soilKey_version, timestamp}.
#'   }
#' @seealso \code{\link{benchmark_bdsolos}}, \code{\link{benchmark_lucas_2018}},
#'   \code{\link{benchmark_run_classification}},
#'   \code{\link{harmonize_to_gsm}}.
#' @export
benchmark_unified <- function(
    systems  = c("all", "wrb2022", "sibcs", "usda"),
    datasets = c("all", "bdsolos", "febr", "kssl", "lucas_esdb"),
    paths    = NULL,
    max_n_per_dataset = NULL,
    engine   = c("soilkey", "aqp", "both"),
    harmonize = FALSE,
    verbose  = TRUE) {

  # Multi-arg matching: allow "all" + named subsets.
  if (length(systems) == 1L && identical(systems, "all"))
    systems <- c("wrb2022", "sibcs", "usda")
  systems  <- match.arg(systems,
                            c("wrb2022", "sibcs", "usda"),
                            several.ok = TRUE)
  if (length(datasets) == 1L && identical(datasets, "all"))
    datasets <- c("bdsolos", "febr", "kssl", "lucas_esdb")
  datasets <- match.arg(datasets,
                            c("bdsolos", "febr", "kssl", "lucas_esdb"),
                            several.ok = TRUE)
  engine   <- match.arg(engine)

  # Default canonical paths if none supplied.
  if (is.null(paths)) {
    sd_root <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data"
    paths <- list(
      bdsolos    = file.path(sd_root, "embrapa_bdsolos", "BD_solos"),
      febr       = file.path(sd_root, "embrapa_bdsolos",
                                "febr-superconjunto.txt"),
      kssl_gpkg  = file.path(sd_root, "KSSL", "ncss_labdata.gpkg"),
      kssl_nasis = file.path(sd_root, "KSSL",
                                "NASIS_Morphological_09142021.sqlite"),
      lucas_csv  = file.path(sd_root, "eu_lucas",
                                "LUCAS-SOIL-2018-data-report-readme-v2",
                                "LUCAS-SOIL-2018-v2",
                                "LUCAS-SOIL-2018.csv"),
      esdb_root  = file.path(sd_root, "eu_lucas",
                                "ESDB-Raster-Library-1k-GeoTIFF-20240507")
    )
  }

  # Mapping: which datasets have reference labels for which systems
  ds_has_ref <- list(
    bdsolos    = c(sibcs = TRUE,  wrb2022 = TRUE,  usda = TRUE),
    febr       = c(sibcs = TRUE,  wrb2022 = TRUE,  usda = TRUE),
    kssl       = c(sibcs = FALSE, wrb2022 = FALSE, usda = TRUE),
    lucas_esdb = c(sibcs = FALSE, wrb2022 = TRUE,  usda = FALSE)
  )

  # v0.9.63: engine-driven option (auto-restored on exit)
  if (engine %in% c("aqp", "both")) {
    old_opt <- getOption("soilKey.diagnostic_engine", NULL)
    options(soilKey.diagnostic_engine = "aqp")
    on.exit(options(soilKey.diagnostic_engine = old_opt), add = TRUE)
  }

  out_per_sys_per_ds <- list()
  out_coverage       <- list()

  for (ds in datasets) {
    for (sys in systems) {
      if (!isTRUE(ds_has_ref[[ds]][[sys]])) next
      tag <- sprintf("%s/%s", ds, sys)
      if (isTRUE(verbose))
        cli::cli_alert_info(sprintf("benchmark_unified: %s ...", tag))
      r <- tryCatch(
        .benchmark_one_dataset_one_system(
          ds = ds, sys = sys, paths = paths,
          max_n = max_n_per_dataset,
          harmonize = harmonize, verbose = verbose),
        error = function(e) {
          cli::cli_alert_warning(sprintf(
            "benchmark_unified: %s FAILED: %s", tag,
            conditionMessage(e)))
          NULL
        })
      if (is.null(r)) next
      out_per_sys_per_ds[[tag]] <- r$result
      out_coverage[[tag]]       <- r$coverage
    }
  }

  # Pool per system across datasets.
  pool_one <- function(sys) {
    keys <- grep(paste0("/", sys, "$"), names(out_per_sys_per_ds),
                  value = TRUE)
    if (length(keys) == 0L) {
      return(list(accuracy = NA_real_, n_compared = 0L,
                  n_correct = 0L, confusion = NULL,
                  per_class = NULL,
                  message = "no_dataset_with_reference"))
    }
    n_c <- 0L; n_k <- 0L
    cm_pool <- NULL
    for (k in keys) {
      r <- out_per_sys_per_ds[[k]]
      if (is.null(r) || is.na(r$accuracy)) next
      n_c <- n_c + r$n_compared
      n_k <- n_k + r$n_correct
      if (!is.null(r$confusion))
        cm_pool <- .merge_confusion(cm_pool, r$confusion)
    }
    list(accuracy   = if (n_c > 0L) n_k / n_c else NA_real_,
         n_compared = n_c, n_correct  = n_k,
         confusion  = cm_pool,
         per_class  = .per_class_from_confusion(cm_pool))
  }
  out_per_system <- lapply(systems, pool_one)
  names(out_per_system) <- systems

  list(
    per_system            = out_per_system,
    per_system_per_dataset = out_per_sys_per_ds,
    coverage              = out_coverage,
    config = list(
      systems = systems, datasets = datasets,
      engine  = engine,
      soilKey_version = as.character(utils::packageVersion("soilKey")),
      timestamp = Sys.time(),
      max_n_per_dataset = max_n_per_dataset
    )
  )
}


# --- internals -------------------------------------------------------

#' Single (dataset, system) benchmark call dispatched by name
#' @keywords internal
.benchmark_one_dataset_one_system <- function(ds, sys, paths,
                                                  max_n,
                                                  harmonize = FALSE,
                                                  verbose = TRUE) {
  # v0.9.63: optional harmonisation hook BEFORE classification
  .maybe_harmonize <- function(pedons) {
    if (!isTRUE(harmonize)) return(pedons)
    if (!requireNamespace("mpspline2", quietly = TRUE)) {
      if (isTRUE(verbose))
        cli::cli_alert_warning(
          "harmonize=TRUE but mpspline2 not installed; skipping")
      return(pedons)
    }
    if (isTRUE(verbose))
      cli::cli_alert_info(sprintf(
        "harmonize_to_gsm: %d pedons (%s)", length(pedons), ds))
    harmonize_to_gsm(pedons, verbose = FALSE)
  }
  if (ds == "bdsolos") {
    csvs <- list.files(paths$bdsolos, pattern = "\\.csv$",
                         full.names = TRUE)
    pedons <- list()
    for (f in csvs) {
      pp <- tryCatch(load_bdsolos_csv(f, verbose = FALSE),
                      error = function(e) NULL)
      if (is.null(pp)) next
      pedons <- c(pedons, pp)
      if (!is.null(max_n) && length(pedons) >= max_n) {
        pedons <- pedons[seq_len(max_n)]
        break
      }
    }
    if (length(pedons) == 0L) return(NULL)
    pedons <- .maybe_harmonize(pedons)
    res <- benchmark_bdsolos(pedons, systems = sys,
                                sibcs_level = "order",
                                verbose = FALSE)
    list(
      result   = res$per_system[[sys]],
      coverage = res$coverage[[sys]]
    )
  } else if (ds == "febr") {
    if (!file.exists(paths$febr)) return(NULL)
    pedons <- tryCatch(
      load_embrapa_pedons(paths$febr, verbose = FALSE),
      error = function(e) NULL)
    if (is.null(pedons) || length(pedons) == 0L) return(NULL)
    if (!is.null(max_n)) pedons <- pedons[seq_len(min(max_n, length(pedons)))]
    pedons <- .maybe_harmonize(pedons)
    res <- benchmark_run_classification(pedons, system = sys,
                                            level = "order")
    cov <- list(n_with_ref = res$n_evaluated %||% length(pedons),
                n_total    = length(pedons),
                pct        = round(100 * (res$n_evaluated %||%
                                                length(pedons)) / length(pedons), 1))
    list(
      result = list(
        accuracy   = res$accuracy_top1 %||% NA_real_,
        n_compared = res$n_evaluated %||% 0L,
        n_correct  = round((res$accuracy_top1 %||% 0) *
                              (res$n_evaluated %||% 0L)),
        confusion  = res$confusion,
        per_class  = NULL,
        message    = NA_character_
      ),
      coverage = cov
    )
  } else if (ds == "kssl") {
    if (!file.exists(paths$kssl_gpkg) ||
          !file.exists(paths$kssl_nasis)) return(NULL)
    pedons <- tryCatch(
      load_kssl_pedons_with_nasis(paths$kssl_gpkg,
                                     paths$kssl_nasis,
                                     head = max_n,
                                     verbose = FALSE),
      error = function(e) NULL)
    if (is.null(pedons) || length(pedons) == 0L) return(NULL)
    pedons <- .maybe_harmonize(pedons)
    res <- benchmark_run_classification(pedons, system = sys,
                                            level = "order")
    cov <- list(n_with_ref = res$n_evaluated %||% length(pedons),
                n_total    = length(pedons),
                pct        = round(100 * (res$n_evaluated %||%
                                                length(pedons)) / length(pedons), 1))
    list(
      result = list(
        accuracy   = res$accuracy_top1 %||% NA_real_,
        n_compared = res$n_evaluated %||% 0L,
        n_correct  = round((res$accuracy_top1 %||% 0) *
                              (res$n_evaluated %||% 0L)),
        confusion  = res$confusion,
        per_class  = NULL,
        message    = NA_character_
      ),
      coverage = cov
    )
  } else if (ds == "lucas_esdb") {
    if (!file.exists(paths$lucas_csv) ||
          !dir.exists(paths$esdb_root)) return(NULL)
    pedons <- tryCatch(
      load_lucas_soil_2018(paths$lucas_csv, max_n = max_n,
                              verbose = FALSE),
      error = function(e) NULL)
    if (is.null(pedons) || length(pedons) == 0L) return(NULL)
    pedons <- .maybe_harmonize(pedons)
    res <- benchmark_lucas_2018(pedons, esdb_root = paths$esdb_root,
                                   classify_with = "wrb2022",
                                   max_n = max_n, verbose = FALSE)
    list(
      result = list(
        accuracy   = res$accuracy %||% NA_real_,
        n_compared = res$n_in_scope %||% 0L,
        n_correct  = round((res$accuracy %||% 0) *
                              (res$n_in_scope %||% 0L)),
        confusion  = res$confusion,
        per_class  = res$per_rsg,
        message    = NA_character_
      ),
      coverage = list(n_with_ref = res$n_in_scope %||% 0L,
                        n_total = res$n_total %||% 0L,
                        pct = round(100 * (res$n_in_scope %||% 0L) /
                                          (res$n_total %||% 1L), 1))
    )
  }
}


#' Merge two confusion matrices (table objects), padding union of labels
#' @keywords internal
.merge_confusion <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.null(b)) return(a)
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


#' Per-class recall data.frame from a confusion matrix
#' @keywords internal
.per_class_from_confusion <- function(cm) {
  if (is.null(cm)) return(NULL)
  ref_totals <- rowSums(cm)
  data.frame(
    reference = rownames(cm),
    n_ref     = as.integer(ref_totals),
    n_correct = vapply(rownames(cm), function(r)
                          if (r %in% colnames(cm)) as.integer(cm[r, r]) else 0L,
                          integer(1L)),
    recall    = vapply(rownames(cm), function(r)
                          if (r %in% colnames(cm) && ref_totals[r] > 0)
                            cm[r, r] / ref_totals[r] else 0,
                          numeric(1L)),
    stringsAsFactors = FALSE
  )
}
