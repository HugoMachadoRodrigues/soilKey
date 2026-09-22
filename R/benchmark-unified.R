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
#' @param gapfill If not \code{FALSE} (the default), applies
#'        \code{\link{gapfill_within_pedon}} to each dataset's pedons before
#'        classification, filling interior \code{NA} cells of the continuous
#'        depth-trending attributes by within-pedon linear interpolation.
#'        Accepts the same values as the \code{gapfill} argument of
#'        \code{\link{classify_all}} (\code{TRUE}, a character vector of
#'        attributes, or a named list). Lets you measure the ON/OFF accuracy
#'        lift of gap-fill reproducibly through the harness.
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
    gapfill  = FALSE,
    verbose  = TRUE) {

  # Multi-arg matching: allow "all" + named subsets.
  if (length(systems) == 1L && identical(systems, "all"))
    systems <- c("wrb2022", "sibcs", "usda")
  systems  <- match.arg(systems,
                            c("wrb2022", "sibcs", "usda"),
                            several.ok = TRUE)
  if (length(datasets) == 1L && identical(datasets, "all"))
    datasets <- c("bdsolos", "febr", "kssl", "lucas_esdb", "redape")
  datasets <- match.arg(datasets,
                            c("bdsolos", "febr", "kssl", "lucas_esdb",
                              "redape"),
                            several.ok = TRUE)
  engine   <- match.arg(engine)

  # Default canonical paths if none supplied.
  if (is.null(paths)) paths <- .benchmark_default_paths()

  # Mapping: which datasets have reference labels for which systems
  ds_has_ref <- list(
    bdsolos    = c(sibcs = TRUE,  wrb2022 = TRUE,  usda = TRUE),
    febr       = c(sibcs = TRUE,  wrb2022 = TRUE,  usda = TRUE),
    kssl       = c(sibcs = FALSE, wrb2022 = FALSE, usda = TRUE),
    lucas_esdb = c(sibcs = FALSE, wrb2022 = TRUE,  usda = FALSE),
    redape     = c(sibcs = TRUE,  wrb2022 = FALSE, usda = FALSE)
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
          harmonize = harmonize, gapfill = gapfill, verbose = verbose),
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
         per_class  = .per_class_from_confusion(cm_pool),
         # v0.9.110: richer, imbalance-aware metrics + bootstrap CIs, all
         # derived from the pooled confusion matrix. Existing fields above are
         # untouched (back-compatible).
         metrics    = .benchmark_metrics_from_confusion(cm_pool),
         metrics_ci = if (is.null(cm_pool)) NULL
                      else .benchmark_bootstrap_metrics(cm_pool))
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

#' Reproducible random row sample (seed 42), restoring the global RNG state
#' so the benchmark never perturbs the caller's randomness.
#' @noRd
.benchmark_reproducible_sample <- function(n, size) {
  old <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  set.seed(42L)
  idx <- sort(sample.int(n, size))
  if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
  else if (exists(".Random.seed", envir = .GlobalEnv))
    rm(".Random.seed", envir = .GlobalEnv)
  idx
}

#' Site field that carries a system's reference label (mirrors the
#' `ref_field` switch in `benchmark_run_classification`).
#' @noRd
.benchmark_ref_field <- function(sys) {
  switch(sys,
         wrb2022 = "reference_wrb",
         sibcs   = "reference_sibcs",
         usda    = "reference_usda",
         NA_character_)
}

#' Does a pedon carry a usable reference label for `sys`?
#' @noRd
.benchmark_has_reference <- function(pedon, sys) {
  fld <- .benchmark_ref_field(sys)
  if (is.na(fld)) return(FALSE)
  v <- pedon$site[[fld]]
  !is.null(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(as.character(v)))
}

#' Filter a pedon list to those carrying `sys`'s reference label, THEN cap at
#' `max_n` via the reproducible (seed-42) sample. The order matters: capping
#' before filtering (the pre-v0.9.110 bug) starves sparsely-labelled systems --
#' e.g. only a handful of FEBR pedons carry a USDA label, so a head/random cap
#' over the whole set left FEBR-USDA at n=3 despite hundreds of labelled rows.
#' @noRd
.benchmark_filter_then_cap <- function(pedons, sys, max_n) {
  keep   <- vapply(pedons, .benchmark_has_reference, logical(1L), sys = sys)
  pedons <- pedons[keep]
  if (!is.null(max_n) && length(pedons) > max_n)
    pedons <- pedons[.benchmark_reproducible_sample(length(pedons), max_n)]
  pedons
}

#' Canonicalise FEBR WRB/USDA reference labels to the order/RSG comparison
#' level (no-op for SiBCS, which `benchmark_run_classification` canonicalises
#' itself). `normalise_febr_wrb`/`_usda` are idempotent on already-reduced WRB
#' names but return NA on an already-order USDA string, so apply only to the
#' raw reference field, only once, only for FEBR.
#' @noRd
.benchmark_normalise_febr_ref <- function(pedons, sys) {
  nf <- switch(sys,
               wrb2022 = normalise_febr_wrb,
               usda    = normalise_febr_usda,
               NULL)
  if (is.null(nf)) return(pedons)
  fld <- .benchmark_ref_field(sys)
  for (i in seq_along(pedons)) {
    cur <- pedons[[i]]$site[[fld]]
    if (!is.null(cur) && length(cur) == 1L && !is.na(cur))
      pedons[[i]]$site[[fld]] <- nf(cur)
  }
  pedons
}

#' Default local paths for the reference benchmark datasets
#'
#' Override the root via \code{options(soilKey.benchmark_root = "...")}.
#' @noRd
.benchmark_default_paths <- function() {
  sd_root <- getOption(
    "soilKey.benchmark_root",
    "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data")
  list(
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
                              "ESDB-Raster-Library-1k-GeoTIFF-20240507"),
    redape     = file.path(sd_root, "redape_geotab")
  )
}

#' Which datasets in `paths` actually have their files/dirs present?
#' @noRd
.benchmark_available_datasets <- function(paths) {
  checks <- list(
    bdsolos    = function() dir.exists(paths$bdsolos %||% ""),
    febr       = function() file.exists(paths$febr %||% ""),
    kssl       = function() file.exists(paths$kssl_gpkg %||% "") &&
                              file.exists(paths$kssl_nasis %||% ""),
    lucas_esdb = function() file.exists(paths$lucas_csv %||% "") &&
                              dir.exists(paths$esdb_root %||% ""),
    redape     = function() dir.exists(paths$redape %||% "")
  )
  names(Filter(function(f) isTRUE(tryCatch(f(), error = function(e) FALSE)),
               checks))
}

#' Single (dataset, system) benchmark call dispatched by name
#' @noRd
.benchmark_one_dataset_one_system <- function(ds, sys, paths,
                                                  max_n,
                                                  harmonize = FALSE,
                                                  gapfill = FALSE,
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
  # v0.9.120: optional within-pedon gap-fill hook BEFORE classification.
  # Composed inside each .maybe_harmonize() call site so the ON/OFF accuracy
  # of gapfill is reproducible through the package's own harness.
  .maybe_gapfill <- function(pedons) {
    if (isFALSE(gapfill) || is.null(gapfill)) return(pedons)
    if (isTRUE(verbose))
      cli::cli_alert_info(sprintf(
        "gapfill_within_pedon: %d pedons (%s)", length(pedons), ds))
    lapply(pedons, function(p) {
      if (!inherits(p, "PedonRecord")) return(p)
      tryCatch({
        if (isTRUE(gapfill))            gapfill_within_pedon(p)
        else if (is.character(gapfill)) gapfill_within_pedon(p, attrs = gapfill)
        else if (is.list(gapfill))
          do.call(gapfill_within_pedon, c(list(pedon = p), gapfill))
        p
      }, error = function(e) p)
    })
  }
  if (ds == "bdsolos") {
    csvs <- list.files(paths$bdsolos, pattern = "\\.csv$",
                         full.names = TRUE)
    # Accumulate CSVs until we hold at least `max_n` pedons that carry THIS
    # system's reference label, then stop (the per-CSV parse is the cost). The
    # old code stopped at the first max_n pedons regardless of label, which
    # left the sparse WRB/USDA labels near-empty; this stops at max_n *labelled*
    # pedons instead. Sampling is by leading CSV (state-clustered) -- noted in
    # the report -- and `.benchmark_filter_then_cap` applies the final cap.
    pedons <- list()
    for (f in csvs) {
      pp <- tryCatch(load_bdsolos_csv(f, verbose = FALSE),
                      error = function(e) NULL)
      if (!is.null(pp)) pedons <- c(pedons, pp)
      if (!is.null(max_n) &&
          sum(vapply(pedons, .benchmark_has_reference, logical(1L),
                     sys = sys)) >= max_n)
        break
    }
    if (length(pedons) == 0L) return(NULL)
    pedons <- .benchmark_filter_then_cap(pedons, sys, max_n)
    if (length(pedons) == 0L) return(NULL)
    pedons <- .maybe_harmonize(.maybe_gapfill(pedons))
    res <- benchmark_bdsolos(pedons, systems = sys,
                                sibcs_level = "order",
                                verbose = FALSE)
    list(
      result   = res$per_system[[sys]],
      coverage = res$coverage[[sys]]
    )
  } else if (ds == "febr") {
    if (!file.exists(paths$febr)) return(NULL)
    # febr-superconjunto.txt is FEBR-format, not BDsolos-format -- use the
    # FEBR loader (the BDsolos loader needs an id_perfil column and fails).
    # Load with require_classification = "any" so all three reference labels
    # survive (the loader default keeps only SiBCS-labelled pedons, which
    # masked the WRB/USDA-labelled subset). Then FILTER to the requested
    # system's label BEFORE the reproducible cap, so a sparsely-labelled
    # system (e.g. USDA) is sampled from its real pool, not the SiBCS pool.
    pedons <- tryCatch(
      load_febr_pedons(paths$febr, require_classification = "any",
                       verbose = FALSE),
      error = function(e) NULL)
    if (is.null(pedons) || length(pedons) == 0L) return(NULL)
    pedons <- .benchmark_filter_then_cap(pedons, sys, max_n)
    if (length(pedons) == 0L) return(NULL)
    # Reduce the raw FEBR WRB/USDA reference labels to the comparison level
    # (RSG / order). They arrive as full names -- "HAPLIC ACRISOL (...)",
    # "AQUEPT" -- which never match the predicted "Acrisols"/"Inceptisols"
    # without this. (SiBCS is canonicalised inside benchmark_run_classification;
    # the WRB/USDA path there assumes the reference is already in order form.)
    pedons <- .benchmark_normalise_febr_ref(pedons, sys)
    pedons <- .maybe_harmonize(.maybe_gapfill(pedons))
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
    # No filter-then-cap here: the KSSL gpkg loader already filters to
    # USDA-labelled rows before `head`, so the cap is not label-starved. It is
    # a head-N (not random) sample -- flagged in the report -- but restructuring
    # the 8.8 GB read for a random draw is out of scope for B1.
    pedons <- tryCatch(
      load_kssl_pedons_with_nasis(paths$kssl_gpkg,
                                     paths$kssl_nasis,
                                     head = max_n,
                                     verbose = FALSE),
      error = function(e) NULL)
    if (is.null(pedons) || length(pedons) == 0L) return(NULL)
    pedons <- .maybe_harmonize(.maybe_gapfill(pedons))
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
    # No filter-then-cap: LUCAS carries no WRB label at load time -- the
    # reference RSG is assigned inside benchmark_lucas_2018() from the ESDB
    # raster. This row is also topsoil-only (a lower-bound baseline -- the
    # report labels it as such); the honest subsoil-fill path is opt-in.
    pedons <- tryCatch(
      load_lucas_soil_2018(paths$lucas_csv, max_n = max_n,
                              verbose = FALSE),
      error = function(e) NULL)
    if (is.null(pedons) || length(pedons) == 0L) return(NULL)
    pedons <- .maybe_harmonize(.maybe_gapfill(pedons))
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
  } else if (ds == "redape") {
    # Redape (Vaz et al. 2023): pedologist-curated SiBCS gold standard. Every
    # pedon carries a SiBCS order label (~100% coverage), so the loader's
    # random max_n draw is not label-starved -- no filter-then-cap needed.
    if (is.null(paths$redape) || !dir.exists(paths$redape)) return(NULL)
    pedons <- tryCatch(
      load_redape_pedons(paths$redape, max_n = max_n, verbose = FALSE),
      error = function(e) NULL)
    if (is.null(pedons) || length(pedons) == 0L) return(NULL)
    pedons <- .maybe_harmonize(.maybe_gapfill(pedons))
    res <- benchmark_redape(pedons, level = "order", verbose = FALSE)
    n_cmp <- res$n_compared %||% 0L
    list(
      result = list(
        accuracy   = res$accuracy %||% NA_real_,
        n_compared = n_cmp,
        n_correct  = round((res$accuracy %||% 0) * n_cmp),
        confusion  = res$confusion,
        per_class  = res$per_class_recall,
        message    = NA_character_
      ),
      coverage = list(n_with_ref = n_cmp,
                      n_total = length(pedons),
                      pct = round(100 * n_cmp / max(1L, length(pedons)), 1))
    )
  }
}


#' Merge two confusion matrices (table objects), padding union of labels
#' @noRd
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
#' @noRd
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


#' Comprehensive classification metrics from a pooled confusion matrix
#'
#' All metrics are computed closed-form from \code{cm} (a \code{table} with
#' \code{reference} rows / \code{predicted} columns, as produced by
#' \code{.merge_confusion}). The matrix is first padded to the union of its row
#' and column labels, so a class that is only ever predicted (or only ever
#' referenced) is handled, and every division is guarded so an absent class
#' never yields \code{NaN}. \code{balanced_accuracy} and \code{macro_f1} average
#' only over classes that have reference support (a zero-support class neither
#' inflates nor deflates them). \code{nir} is the no-information rate (the
#' accuracy of always predicting the majority \emph{reference} class) -- the
#' baseline an accuracy figure must beat to be meaningful.
#' @noRd
.benchmark_metrics_from_confusion <- function(cm) {
  if (is.null(cm)) return(NULL)
  cm   <- as.matrix(cm)
  labs <- union(rownames(cm), colnames(cm))
  M    <- matrix(0L, length(labs), length(labs), dimnames = list(labs, labs))
  if (nrow(cm) > 0L && ncol(cm) > 0L) M[rownames(cm), colnames(cm)] <- cm
  R <- rowSums(M); C <- colSums(M); N <- sum(M)
  safe <- function(num, den) if (den > 0) num / den else 0
  precision <- vapply(labs, function(k) safe(M[k, k], C[k]), numeric(1L))
  recall    <- vapply(labs, function(k) safe(M[k, k], R[k]), numeric(1L))
  f1        <- vapply(labs, function(k) {
                 p <- precision[[k]]; r <- recall[[k]]
                 if (p + r > 0) 2 * p * r / (p + r) else 0
               }, numeric(1L))
  present <- R > 0
  per_class <- data.frame(
    class = labs, support = as.integer(R),
    precision = unname(precision), recall = unname(recall), f1 = unname(f1),
    stringsAsFactors = FALSE, row.names = NULL)
  p_o <- if (N > 0) sum(diag(M)) / N else NA_real_
  p_e <- if (N > 0) sum(R * C) / (N^2) else NA_real_
  kappa <- if (is.na(p_o)) NA_real_
           else if (is.na(p_e) || abs(1 - p_e) < .Machine$double.eps)
             (if (isTRUE(all.equal(p_o, 1))) 1 else 0)
           else (p_o - p_e) / (1 - p_e)
  list(
    accuracy          = p_o,
    nir               = if (N > 0) max(R) / N else NA_real_,
    balanced_accuracy = if (any(present)) mean(recall[present]) else NA_real_,
    macro_f1          = if (any(present)) mean(f1[present]) else NA_real_,
    kappa             = kappa,
    n                 = as.integer(N),
    per_class         = per_class
  )
}


#' Reproducible bootstrap 95\% CIs for the headline metrics
#'
#' Expands the pooled confusion matrix back to its per-item
#' (reference, predicted) pairs, resamples them with replacement \code{B} times,
#' and returns percentile 95\% CIs for accuracy / balanced accuracy / macro-F1 /
#' kappa. Seeded (default 42) and RNG-state-preserving, exactly like
#' \code{.benchmark_reproducible_sample}, so the CI is reproducible and never
#' perturbs the caller's randomness. Degenerate inputs (\code{< 2} items or
#' \code{< 2} reference classes) return \code{c(NA, NA)} per metric.
#' @noRd
.benchmark_bootstrap_metrics <- function(cm, B = 1000L, seed = 42L) {
  na2 <- c(NA_real_, NA_real_)
  degenerate <- list(accuracy = na2, balanced_accuracy = na2,
                     macro_f1 = na2, kappa = na2)
  if (is.null(cm)) return(degenerate)
  cm   <- as.matrix(cm)
  labs <- union(rownames(cm), colnames(cm))
  M    <- matrix(0L, length(labs), length(labs), dimnames = list(labs, labs))
  if (nrow(cm) > 0L && ncol(cm) > 0L) M[rownames(cm), colnames(cm)] <- cm
  N <- sum(M)
  if (N < 2L || sum(rowSums(M) > 0) < 2L) return(degenerate)
  cells <- which(M > 0, arr.ind = TRUE)
  ref   <- factor(rep(labs[cells[, 1]], times = M[cells]), levels = labs)
  pred  <- factor(rep(labs[cells[, 2]], times = M[cells]), levels = labs)
  old <- if (exists(".Random.seed", envir = .GlobalEnv))
    get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (!is.null(old)) assign(".Random.seed", old, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv))
      rm(".Random.seed", envir = .GlobalEnv)
  }, add = TRUE)
  set.seed(seed)
  acc <- bal <- mf1 <- kap <- numeric(B)
  for (b in seq_len(B)) {
    s   <- sample.int(N, N, replace = TRUE)
    cmb <- table(reference = ref[s], predicted = pred[s])
    m   <- .benchmark_metrics_from_confusion(cmb)
    acc[b] <- m$accuracy; bal[b] <- m$balanced_accuracy
    mf1[b] <- m$macro_f1; kap[b] <- m$kappa
  }
  q <- function(x) unname(stats::quantile(x, c(0.025, 0.975), na.rm = TRUE))
  list(accuracy = q(acc), balanced_accuracy = q(bal),
       macro_f1 = q(mf1), kappa = q(kap))
}
