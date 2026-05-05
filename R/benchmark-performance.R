# =============================================================================
# v0.9.53 -- performance benchmark.
#
# Measures end-to-end latency and batch throughput of the three classifiers
# (classify_wrb2022, classify_sibcs, classify_usda) on synthetic pedons.
# Goal: give users a realistic estimate of how long a full batch run
# (e.g. 18,984 LUCAS pedons or 36,000 KSSL pedons) will take, and to
# detect regressions over future releases.
# =============================================================================


#' Run the soilKey performance benchmark
#'
#' Generates \code{n} synthetic pedons (5 horizons each, with the
#' chemistry / morphology populated for typical Argissolo /
#' Latossolo / Cambissolo cases), calls each classifier on each
#' pedon, and reports per-call latency + total throughput.
#'
#' Designed to be a one-shot reproducible benchmark: the synthetic
#' pedons use a fixed RNG seed so timings on the same machine are
#' comparable across releases.
#'
#' @param n Integer. Number of synthetic pedons to generate.
#'        Default 100; pass 1000 or higher for batch-level
#'        measurements.
#' @param systems Character vector. Which classifiers to time.
#'        Default \code{c("wrb2022", "sibcs", "usda")} (all three).
#' @param include_familia Pass-through to \code{classify_sibcs}
#'        when \code{"sibcs"} is in \code{systems}. Default
#'        \code{FALSE}.
#' @param seed RNG seed for reproducibility. Default 42.
#' @param verbose If \code{TRUE} (default), prints a per-system
#'        summary line.
#' @return A list with elements:
#'   \describe{
#'     \item{\code{summary}}{data.frame: \code{system, n_pedons,
#'           total_seconds, mean_seconds, median_seconds,
#'           pedons_per_minute}.}
#'     \item{\code{per_pedon}}{data.frame with one row per
#'           (pedon, system) call: \code{i, system,
#'           seconds, status}.}
#'     \item{\code{config}}{list with \code{n}, \code{seed},
#'           \code{soilKey_version}, \code{R_version},
#'           \code{platform}.}
#'   }
#'
#' @examples
#' \dontrun{
#' bench <- benchmark_performance(n = 100)
#' bench$summary
#' #>     system n_pedons total_seconds mean_seconds median_seconds pedons_per_minute
#' #> 1 wrb2022      100        ~ 5-12      0.05-0.12      ~          ~
#' #> 2   sibcs      100        ~ 5-15      0.05-0.15      ~          ~
#' #> 3    usda      100        ~ 4-10      0.04-0.10      ~          ~
#' }
#' @export
benchmark_performance <- function(n = 100L,
                                    systems = c("wrb2022", "sibcs", "usda"),
                                    include_familia = FALSE,
                                    seed    = 42L,
                                    verbose = TRUE) {
  systems <- match.arg(systems, several.ok = TRUE)
  if (n < 1L) stop("benchmark_performance(): n must be >= 1.")
  set.seed(as.integer(seed))

  pedons <- vector("list", n)
  for (i in seq_len(n)) {
    pedons[[i]] <- .make_synth_perf_pedon(i)
  }

  per_pedon <- vector("list", 0L)
  for (sys in systems) {
    fn <- switch(sys,
                  wrb2022 = function(p) classify_wrb2022(p, on_missing = "silent"),
                  sibcs   = function(p) classify_sibcs(p, on_missing = "silent",
                                                          include_familia = include_familia),
                  usda    = function(p) classify_usda(p, on_missing = "silent"))
    for (i in seq_len(n)) {
      t0 <- proc.time()[["elapsed"]]
      ok <- TRUE
      tryCatch(fn(pedons[[i]]),
                error = function(e) { ok <<- FALSE })
      t1 <- proc.time()[["elapsed"]]
      per_pedon[[length(per_pedon) + 1L]] <- data.frame(
        i        = i,
        system   = sys,
        seconds  = t1 - t0,
        status   = if (ok) "OK" else "ERROR",
        stringsAsFactors = FALSE
      )
    }
  }
  per_pedon <- do.call(rbind, per_pedon)

  summary_rows <- list()
  for (sys in systems) {
    sub <- per_pedon[per_pedon$system == sys & per_pedon$status == "OK", ]
    if (nrow(sub) == 0L) next
    total <- sum(sub$seconds)
    summary_rows[[sys]] <- data.frame(
      system            = sys,
      n_pedons          = nrow(sub),
      total_seconds     = total,
      mean_seconds      = mean(sub$seconds),
      median_seconds    = stats::median(sub$seconds),
      pedons_per_minute = if (total > 0) 60 * nrow(sub) / total else NA_real_,
      stringsAsFactors  = FALSE
    )
  }
  summary <- do.call(rbind, summary_rows)
  rownames(summary) <- NULL

  if (isTRUE(verbose) && !is.null(summary)) {
    cli::cli_h2(sprintf("benchmark_performance(): n = %d", n))
    for (i in seq_len(nrow(summary))) {
      r <- summary[i, ]
      cli::cli_alert_info(sprintf(
        "{.field %-7s}: %.3fs/pedon (median), %.1f pedons/min, total %.2fs",
        r$system, r$median_seconds, r$pedons_per_minute, r$total_seconds
      ))
    }
  }

  list(
    summary   = summary,
    per_pedon = per_pedon,
    config    = list(
      n               = as.integer(n),
      seed            = as.integer(seed),
      soilKey_version = as.character(utils::packageVersion("soilKey")),
      R_version       = paste(R.version$major, R.version$minor, sep = "."),
      platform        = R.version$platform
    )
  )
}


#' Synthesise a small but realistic 5-horizon pedon for benchmarking
#' @keywords internal
.make_synth_perf_pedon <- function(i) {
  rho <- runif(1L, 0.7, 1.5)
  hue <- sample(c("10R", "2.5YR", "5YR", "7.5YR", "10YR", "2.5Y"),
                  size = 1L)
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   55,   115,  170),
    bottom_cm = c(20,   55,   115,  170,  220),
    designation = c("A","AB","Bt","Bt2","BC"),
    munsell_hue_moist    = rep(hue, 5L),
    munsell_value_moist  = c(4, 4, 4, 3, 3),
    munsell_chroma_moist = c(3, 5, 6, 6, 6),
    structure_grade      = c("moderate","moderate","strong","strong","moderate"),
    structure_type       = c("granular","subangular blocky",
                                "subangular blocky","subangular blocky",
                                "subangular blocky"),
    clay_films_amount    = c(NA, "few", "common", "common", "few"),
    clay_pct = pmin(70, pmax(5, 18 * rho + c(0, 10, 27, 24, 20))),
    silt_pct = c(30, 25, 20, 22, 24),
    sand_pct = pmax(0, 100 - (pmin(70, pmax(5, 18 * rho +
                                              c(0, 10, 27, 24, 20))) +
                                  c(30, 25, 20, 22, 24))),
    ph_h2o   = c(5.5, 5.3, 5.0, 5.0, 5.1) + runif(1L, -0.5, 0.5),
    oc_pct   = c(1.5, 0.6, 0.3, 0.2, 0.2) * rho,
    cec_cmol = c(8, 6, 5.5, 4.5, 4.0) * rho,
    bs_pct   = pmin(100, pmax(0, c(35, 25, 20, 18, 20) + runif(1L, -10, 30))),
    al_cmol  = c(0.5, 0.8, 1.0, 1.2, 1.1) * rho
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(
    site = list(id = sprintf("synth-%05d", i),
                  lat = -22 + runif(1L, -2, 2),
                  lon = -43 + runif(1L, -2, 2),
                  country = "BR"),
    horizons = hz
  )
}
