# =============================================================================
# soilKey -- WoSIS benchmark driver
#
# Run-once-per-release script that produces the agreement statistics reported
# in the methodological paper accompanying soilKey v1.0. Reads a snapshot of
# WoSIS profiles, builds PedonRecords, runs classify_wrb2022(), and writes a
# versioned report under inst/benchmarks/reports/wosis_<DATE>.md.
#
# This driver is *not* called automatically. It is intended to be sourced
# manually by a maintainer with WoSIS API credentials (or a local extract):
#
#   options(soilKey.wosis_endpoint = "https://wosis.isric.org/api/v3/profiles")
#   source("inst/benchmarks/run_wosis_benchmark.R")
#   report <- run_wosis_benchmark(n_max = 5000L)
#
# The vignette 06-wosis-benchmark.Rmd documents the protocol in full.
# =============================================================================


#' Pull a paginated set of WoSIS profiles via the WoSIS REST API.
#'
#' @keywords internal
read_wosis_profiles <- function(url       = getOption("soilKey.wosis_endpoint"),
                                  page_size = 500L,
                                  n_max     = Inf) {
  if (is.null(url) || !nzchar(url))
    stop("Set options(soilKey.wosis_endpoint = '...') before calling read_wosis_profiles().")
  if (!requireNamespace("httr",     quietly = TRUE)) stop("Install 'httr'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install 'jsonlite'.")

  out  <- list()
  page <- 1L
  while (length(out) < n_max) {
    resp <- httr::GET(url,
                       query = list(page = page, page_size = page_size,
                                      format = "json"))
    httr::stop_for_status(resp)
    body <- jsonlite::fromJSON(httr::content(resp, as = "text",
                                                 encoding = "UTF-8"),
                                 simplifyVector = FALSE)
    if (length(body$results) == 0L) break
    out <- c(out, body$results)
    if (length(body$results) < page_size) break
    page <- page + 1L
  }
  utils::head(out, n_max)
}


#' Convert a single WoSIS profile (parsed JSON list) into a PedonRecord.
#'
#' @keywords internal
build_pedon_from_wosis <- function(profile) {
  hz <- data.table::rbindlist(
    lapply(profile$horizons, function(h) {
      data.table::data.table(
        top_cm      = h$top_cm,
        bottom_cm   = h$bottom_cm,
        designation = h$designation %||% NA_character_,
        clay_pct    = h$clay        %||% NA_real_,
        silt_pct    = h$silt        %||% NA_real_,
        sand_pct    = h$sand        %||% NA_real_,
        ph_h2o      = h$ph_h2o      %||% NA_real_,
        oc_pct      = h$oc          %||% NA_real_,
        cec_cmol    = h$cec         %||% NA_real_,
        bs_pct      = h$bs          %||% NA_real_,
        caco3_pct   = h$caco3       %||% NA_real_
      )
    }),
    fill = TRUE
  )
  PedonRecord$new(
    site = list(
      id              = profile$id,
      lat             = profile$lat,
      lon             = profile$lon,
      country         = profile$country %||% NA_character_,
      parent_material = profile$parent_material %||% NA_character_,
      wosis_rsg       = profile$cwrb_reference_soil_group
    ),
    horizons = hz
  )
}


#' Run the benchmark and emit the report.
#'
#' @param n_max Maximum number of WoSIS profiles to include (caps run time).
#' @param subset Optional region filter, e.g. "South America" or "Brazil".
#' @keywords internal
run_wosis_benchmark <- function(n_max = 5000L, subset = NULL) {
  profiles <- read_wosis_profiles(n_max = n_max)
  if (!is.null(subset)) {
    profiles <- Filter(function(p)
      identical(p$region, subset) || identical(p$country, subset),
      profiles)
  }
  message(sprintf("WoSIS subset: %d profiles", length(profiles)))

  pedons <- lapply(profiles, build_pedon_from_wosis)

  classifications <- lapply(pedons, function(p) {
    tryCatch(classify_wrb2022(p, on_missing = "silent"),
              error = function(e) NULL)
  })

  bench <- do.call(rbind, Map(function(c, p) {
    if (is.null(c)) return(NULL)
    data.frame(
      profile_id = p$site$id,
      target     = p$site$wosis_rsg %||% NA_character_,
      assigned   = c$rsg_or_order,
      grade      = c$evidence_grade,
      stringsAsFactors = FALSE
    )
  }, classifications, pedons))

  bench$match <- bench$target == bench$assigned

  report <- list(
    snapshot_date = Sys.Date(),
    n_profiles    = nrow(bench),
    top1          = mean(bench$match, na.rm = TRUE),
    indeterminate = mean(is.na(bench$assigned)),
    grade_table   = table(bench$grade, useNA = "ifany"),
    confusion     = table(target = bench$target, assigned = bench$assigned)
  )

  out_path <- file.path("inst", "benchmarks", "reports",
                          sprintf("wosis_%s.md", report$snapshot_date))
  dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
  cat(sprintf("# WoSIS benchmark report -- %s\n\n",       report$snapshot_date),
      sprintf("* Profiles: %d\n",                          report$n_profiles),
      sprintf("* Top-1 agreement: %.3f\n",                 report$top1),
      sprintf("* Indeterminate (NA assignments): %.3f\n",  report$indeterminate),
      "\n## Evidence-grade distribution\n\n",
      paste(capture.output(print(report$grade_table)),    collapse = "\n"),
      "\n\n## Confusion matrix (RSG-level)\n\n",
      paste(capture.output(print(report$confusion)),       collapse = "\n"),
      "\n",
      file = out_path, sep = "")

  message(sprintf("Report written to %s", out_path))
  invisible(report)
}
