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
#' v0.9.10 hardening:
#' - Aligns the request schema with WoSIS REST v3
#'   (`https://wosis.isric.org/api/v3/profiles`):
#'   pagination via `offset` + `limit` (the v3 default), not page+page_size.
#' - Adds `subset = c("global", "south_america", ...)` filter that
#'   maps to the v3 `country` and `bbox` query parameters per region.
#' - Honours `getOption("soilKey.wosis_endpoint")` for testing /
#'   private mirrors.
#' - Wraps every HTTP call in `tryCatch` and reports a clear error
#'   when offline or when the server returns a non-200 status.
#'
#' @param url      WoSIS REST v3 endpoint (e.g.
#'                 \code{"https://wosis.isric.org/api/v3/profiles"}).
#' @param subset   Optional region subset name; one of
#'                 \code{c("global","south_america","north_america",
#'                 "europe","africa","asia","oceania","brazil")}. The
#'                 South America bbox is approximate; tighten via
#'                 \code{options(soilKey.wosis_bbox_<region> = c(xmin, ymin, xmax, ymax))}.
#' @param limit    Profiles per page (REST v3 default: 100; max 500).
#' @param n_max    Maximum number of profiles to return.
#' @param verbose  Emit per-page progress.
#' @keywords internal
read_wosis_profiles <- function(url       = getOption("soilKey.wosis_endpoint",
                                                        "https://wosis.isric.org/api/v3/profiles"),
                                  subset    = c("global", "south_america",
                                                "north_america", "europe",
                                                "africa", "asia", "oceania",
                                                "brazil"),
                                  limit     = 100L,
                                  n_max     = Inf,
                                  verbose   = TRUE) {
  if (is.null(url) || !nzchar(url))
    stop("Set options(soilKey.wosis_endpoint = '...') before calling read_wosis_profiles().")
  if (!requireNamespace("httr",     quietly = TRUE)) stop("Install 'httr'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install 'jsonlite'.")
  subset <- match.arg(subset)

  # Region-specific filter parameters (bbox = c(xmin, ymin, xmax, ymax)).
  region_filter <- switch(subset,
    global         = list(),
    south_america  = list(bbox = "-82,-56,-34,13"),
    north_america  = list(bbox = "-170,15,-50,84"),
    europe         = list(bbox = "-25,34,45,72"),
    africa         = list(bbox = "-20,-35,52,38"),
    asia           = list(bbox = "26,-12,180,82"),
    oceania        = list(bbox = "110,-50,180,0"),
    brazil         = list(country = "BR")
  )
  # Allow user override per region (e.g. for SiBCS tighter bbox).
  override <- getOption(paste0("soilKey.wosis_bbox_", subset))
  if (!is.null(override)) {
    region_filter <- list(bbox = paste(override, collapse = ","))
  }

  out    <- list()
  offset <- 0L
  while (length(out) < n_max) {
    page_limit <- min(limit, n_max - length(out))
    qparams <- c(list(offset = offset, limit = page_limit, format = "json"),
                   region_filter)
    resp <- tryCatch(
      httr::GET(url, query = qparams,
                  httr::user_agent("soilKey (https://github.com/HugoMachadoRodrigues/soilKey)")),
      error = function(e)
        stop(sprintf("WoSIS HTTP request failed: %s\n", conditionMessage(e)),
             "  Check network connectivity, then retry.\n",
             "  Endpoint: ", url, call. = FALSE)
    )
    if (httr::status_code(resp) != 200L) {
      stop(sprintf("WoSIS returned HTTP %d for %s\n",
                     httr::status_code(resp), url),
           "  Body: ", httr::content(resp, as = "text",
                                       encoding = "UTF-8"),
           call. = FALSE)
    }
    body <- jsonlite::fromJSON(httr::content(resp, as = "text",
                                                 encoding = "UTF-8"),
                                 simplifyVector = FALSE)
    page <- body$results %||% body$features %||% body
    if (!is.list(page) || length(page) == 0L) break
    out <- c(out, page)
    if (verbose)
      message(sprintf("[WoSIS] offset=%d, fetched=%d, running total=%d",
                        offset, length(page), length(out)))
    if (length(page) < page_limit) break  # last page
    offset <- offset + page_limit
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
#' @param n_max  Maximum number of WoSIS profiles to include (caps run
#'        time). Default 5 000.
#' @param subset Region subset (passed through to
#'        \code{read_wosis_profiles}). Default \code{"global"}.
#' @keywords internal
run_wosis_benchmark <- function(n_max  = 5000L,
                                  subset = c("global", "south_america",
                                             "north_america", "europe",
                                             "africa", "asia", "oceania",
                                             "brazil")) {
  subset   <- match.arg(subset)
  profiles <- read_wosis_profiles(n_max = n_max, subset = subset)
  message(sprintf("WoSIS subset (%s): %d profiles",
                    subset, length(profiles)))

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

  if (is.null(bench) || nrow(bench) == 0L) {
    stop("No WoSIS profiles to benchmark -- empty subset.")
  }
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


# =============================================================================
# Offline canonical-fixture benchmark
#
# A network-free, fully-reproducible mini-benchmark over the 31 canonical
# fixtures shipped under inst/extdata/. Each fixture has a *known target
# RSG / order* (encoded in the filename), so the run produces real
# concordance numbers for all three classification systems without any
# external dataset.
#
# Used as a sanity check on every release and as the headline figure of
# the methodological paper before the full WoSIS pull is available.
# =============================================================================


#' Known target RSG / SiBCS order / USDA order for each canonical fixture.
#' Names are filename stems (without `_canonical`); values are lists of
#' the expected class names in the three systems. Each entry is a
#' character vector -- when more than one canonical class is acceptable
#' under the published cross-system correspondence (Schad 2023 Annex
#' Table 1 for WRB <-> USDA; SiBCS 5ª ed. Annex A for WRB <-> SiBCS),
#' all are listed. The benchmark counts a match when the assigned
#' class is in the target vector. An `NA` entry indicates the target
#' is ambiguous or out-of-scope for that system.
#'
#' @keywords internal
.canonical_targets <- function() {
  list(
    acrisol      = list(wrb = "Acrisols",   sibcs = "Argissolos",   usda = "Ultisols"),
    alisol       = list(wrb = "Alisols",    sibcs = "Argissolos",   usda = "Ultisols"),
    andosol      = list(wrb = "Andosols",   sibcs = "Cambissolos",  usda = "Andisols"),
    # Anthrosols span Mollisols / Inceptisols / Alfisols depending on
    # subgroup (Hortic / Plaggic / Hydragric) and moisture regime.
    anthrosol    = list(wrb = "Anthrosols", sibcs = NA_character_,
                          usda = c("Inceptisols", "Mollisols", "Alfisols")),
    arenosol     = list(wrb = "Arenosols",  sibcs = "Neossolos",    usda = "Entisols"),
    calcisol     = list(wrb = "Calcisols",  sibcs = NA_character_,  usda = "Aridisols"),
    cambisol     = list(wrb = "Cambisols",  sibcs = "Cambissolos",  usda = "Inceptisols"),
    chernozem    = list(wrb = "Chernozems", sibcs = "Chernossolos", usda = "Mollisols"),
    cryosol      = list(wrb = "Cryosols",   sibcs = NA_character_,  usda = "Gelisols"),
    durisol      = list(wrb = "Durisols",   sibcs = NA_character_,  usda = "Aridisols"),
    ferralsol    = list(wrb = "Ferralsols", sibcs = "Latossolos",   usda = "Oxisols"),
    fluvisol     = list(wrb = "Fluvisols",  sibcs = "Neossolos",    usda = "Entisols"),
    # Gleysols with developed B map to Inceptisols (Aquepts); with
    # weak development -> Entisols (Aquents).
    gleysol      = list(wrb = "Gleysols",   sibcs = "Gleissolos",
                          usda = c("Entisols", "Inceptisols")),
    gypsisol     = list(wrb = "Gypsisols",  sibcs = NA_character_,  usda = "Aridisols"),
    histosol     = list(wrb = "Histosols",  sibcs = "Organossolos", usda = "Histosols"),
    kastanozem   = list(wrb = "Kastanozems",sibcs = "Chernossolos", usda = "Mollisols"),
    leptosol     = list(wrb = "Leptosols",  sibcs = "Neossolos",    usda = "Entisols"),
    lixisol      = list(wrb = "Lixisols",   sibcs = "Argissolos",   usda = "Alfisols"),
    luvisol      = list(wrb = "Luvisols",   sibcs = "Luvissolos",   usda = "Alfisols"),
    # Nitisols span Alfisols (high BS) / Ultisols (low BS) / Oxisols
    # (deep ferralic) / Inceptisols (gradual clay without clear
    # argillic/kandic/oxic) per Schad Table 1.
    nitisol      = list(wrb = "Nitisols",   sibcs = "Nitossolos",
                          usda = c("Alfisols", "Ultisols", "Oxisols", "Inceptisols")),
    phaeozem     = list(wrb = "Phaeozems",  sibcs = "Chernossolos", usda = "Mollisols"),
    planosol     = list(wrb = "Planosols",  sibcs = "Planossolos",  usda = "Alfisols"),
    # Plinthosols: Plinthudults (Ultisols) / Plinthudox (Oxisols) /
    # Plinthaquults / Inceptisols (when plinthite is shallow / weak).
    plinthosol   = list(wrb = "Plinthosols",sibcs = "Plintossolos",
                          usda = c("Oxisols", "Ultisols", "Inceptisols")),
    podzol       = list(wrb = "Podzols",    sibcs = "Espodossolos", usda = "Spodosols"),
    # Retisols (Albeluvisols) -> Glossic Alfisols, Aquepts, or
    # Spodosols depending on the dominant feature.
    retisol      = list(wrb = "Retisols",   sibcs = NA_character_,
                          usda = c("Alfisols", "Inceptisols", "Spodosols")),
    solonchak    = list(wrb = "Solonchaks", sibcs = NA_character_,  usda = "Aridisols"),
    # Solonetz: Natrudalfs (Alfisols, udic) / Natrustalfs / Natrustalls
    # / Natraquolls / Natrargids (Aridisols, aridic).
    solonetz     = list(wrb = "Solonetz",   sibcs = NA_character_,
                          usda = c("Aridisols", "Alfisols", "Mollisols")),
    stagnosol    = list(wrb = "Stagnosols", sibcs = NA_character_,  usda = "Inceptisols"),
    technosol    = list(wrb = "Technosols", sibcs = NA_character_,  usda = "Entisols"),
    umbrisol     = list(wrb = "Umbrisols",  sibcs = NA_character_,  usda = "Inceptisols"),
    vertisol     = list(wrb = "Vertisols",  sibcs = "Vertissolos",  usda = "Vertisols")
  )
}


#' Run the offline canonical-fixture benchmark and emit the report.
#'
#' Reads the 31 fixtures from \code{inst/extdata/}, runs all three keys,
#' compares to the known target encoded in the filename, and writes a
#' versioned report under \code{inst/benchmarks/reports/canonical_<DATE>.md}.
#'
#' Unlike \code{\link{run_wosis_benchmark}}, this function makes zero
#' network calls and is safe to run from `R CMD check` or CI.
#'
#' @param fixture_dir Directory holding `*_canonical.rds`. Defaults to
#'                    `inst/extdata/`.
#' @param out_dir Directory to write the report into.
#' @param verbose Emit progress messages.
#' @return The aggregated `data.frame` of per-fixture classifications,
#'         invisibly. Side-effect: writes the report file.
#' @keywords internal
run_canonical_benchmark <- function(fixture_dir = file.path("inst", "extdata"),
                                      out_dir     = file.path("inst", "benchmarks",
                                                                "reports"),
                                      verbose     = TRUE) {
  files <- list.files(fixture_dir, pattern = "_canonical\\.rds$",
                       full.names = TRUE)
  if (length(files) == 0L)
    stop("No canonical fixtures found under ", fixture_dir)

  targets <- .canonical_targets()

  rows <- vector("list", length(files))
  for (i in seq_along(files)) {
    f    <- files[i]
    stem <- sub("_canonical\\.rds$", "", basename(f))
    p    <- tryCatch(readRDS(f), error = function(e) NULL)
    if (is.null(p)) next
    tgt  <- targets[[stem]] %||% list(wrb = NA_character_,
                                         sibcs = NA_character_,
                                         usda  = NA_character_)

    cls_wrb   <- tryCatch(classify_wrb2022(p, on_missing = "silent"),
                            error = function(e) NULL)
    cls_sibcs <- tryCatch(classify_sibcs(p, include_familia = FALSE),
                            error = function(e) NULL)
    cls_usda  <- tryCatch(classify_usda(p),
                            error = function(e) NULL)

    .target_string <- function(t) {
      if (length(t) == 0L || (length(t) == 1L && is.na(t))) NA_character_
      else paste(t, collapse = "|")
    }
    rows[[i]] <- data.frame(
      fixture            = stem,
      target_wrb         = .target_string(tgt$wrb),
      assigned_wrb       = if (is.null(cls_wrb))   NA_character_ else cls_wrb$rsg_or_order,
      grade_wrb          = if (is.null(cls_wrb))   NA_character_ else cls_wrb$evidence_grade,
      target_sibcs       = .target_string(tgt$sibcs),
      assigned_sibcs     = if (is.null(cls_sibcs)) NA_character_ else cls_sibcs$rsg_or_order,
      grade_sibcs        = if (is.null(cls_sibcs)) NA_character_ else cls_sibcs$evidence_grade,
      target_usda        = .target_string(tgt$usda),
      assigned_usda      = if (is.null(cls_usda))  NA_character_ else cls_usda$rsg_or_order,
      grade_usda         = if (is.null(cls_usda))  NA_character_ else cls_usda$evidence_grade,
      stringsAsFactors   = FALSE
    )
    if (verbose)
      message(sprintf("[%2d/%d] %-12s -> WRB: %-13s SiBCS: %-13s USDA: %s",
                        i, length(files), stem,
                        rows[[i]]$assigned_wrb       %||% "(NA)",
                        rows[[i]]$assigned_sibcs     %||% "(NA)",
                        rows[[i]]$assigned_usda      %||% "(NA)"))
  }
  bench <- do.call(rbind, rows)

  # Multi-valued targets are stored as "A|B|C"; treat the assigned
  # class as a match if it appears in any of the |-separated tokens.
  .match_in <- function(target, assigned) {
    out <- logical(length(target))
    for (i in seq_along(target)) {
      if (is.na(target[i]) || is.na(assigned[i])) {
        out[i] <- NA
      } else {
        out[i] <- assigned[i] %in% strsplit(target[i], "|", fixed = TRUE)[[1]]
      }
    }
    out
  }
  match_wrb   <- .match_in(bench$target_wrb,   bench$assigned_wrb)
  match_sibcs <- .match_in(bench$target_sibcs, bench$assigned_sibcs)
  match_usda  <- .match_in(bench$target_usda,  bench$assigned_usda)

  agg <- data.frame(
    system   = c("WRB 2022", "SiBCS 5",  "USDA ST 13"),
    n_total  = c(sum(!is.na(bench$target_wrb)),
                  sum(!is.na(bench$target_sibcs)),
                  sum(!is.na(bench$target_usda))),
    n_match  = c(sum(match_wrb,   na.rm = TRUE),
                  sum(match_sibcs, na.rm = TRUE),
                  sum(match_usda,  na.rm = TRUE)),
    stringsAsFactors = FALSE
  )
  agg$top1 <- agg$n_match / pmax(agg$n_total, 1L)

  grade_wrb   <- as.data.frame(table(grade = bench$grade_wrb,   useNA = "ifany"))
  grade_sibcs <- as.data.frame(table(grade = bench$grade_sibcs, useNA = "ifany"))
  grade_usda  <- as.data.frame(table(grade = bench$grade_usda,  useNA = "ifany"))

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir,
                          sprintf("canonical_%s.md", Sys.Date()))

  fmt_grade <- function(df) {
    if (nrow(df) == 0) return("(no data)")
    paste(sprintf("  - %s: %d", df$grade, df$Freq), collapse = "\n")
  }

  bench_lines <- vapply(seq_len(nrow(bench)), function(i) {
    r <- bench[i, ]
    mark <- function(t, a) {
      if (is.na(t) || is.na(a)) return(".")
      tokens <- strsplit(t, "|", fixed = TRUE)[[1]]
      if (a %in% tokens) "OK" else "MISS"
    }
    # Display vector targets as "A / B / C" for readability (vs. the
    # internal "A|B|C" pipe-separated form).
    fmt <- function(s) {
      if (is.na(s)) "."
      else gsub("|", " / ", s, fixed = TRUE)
    }
    sprintf("| %-12s | %-26s | %-13s | %-4s | %-26s | %-13s | %-4s | %-26s | %-13s | %-4s |",
              r$fixture,
              fmt(r$target_wrb),    r$assigned_wrb   %||% ".",
              mark(r$target_wrb,    r$assigned_wrb),
              fmt(r$target_sibcs),  r$assigned_sibcs %||% ".",
              mark(r$target_sibcs,  r$assigned_sibcs),
              fmt(r$target_usda),   r$assigned_usda  %||% ".",
              mark(r$target_usda,   r$assigned_usda))
  }, character(1))

  agg_lines <- vapply(seq_len(nrow(agg)), function(i)
    sprintf("| %-10s | %d | %d | %.3f |",
              agg$system[i], agg$n_total[i], agg$n_match[i], agg$top1[i]),
    character(1))

  report_lines <- c(
    "# soilKey -- canonical fixtures benchmark (offline)",
    "",
    sprintf("**Run:** %s &middot; **Package version:** %s &middot; **Fixtures:** %d",
              format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
              .soilkey_version(),
              length(files)),
    "",
    "This is the network-free benchmark over the canonical fixtures",
    "shipped under `inst/extdata/`. Each fixture is a real published",
    "profile (WRB 2022 didactic exemplars, ISRIC ISMC monoliths, Soil",
    "Atlas of Europe), tagged with its known target RSG / SiBCS order /",
    "USDA order. The full-WoSIS run (see `run_wosis_benchmark()`)",
    "produces the paper-grade numbers; this offline run is the",
    "release-time sanity check.",
    "",
    "## Top-1 agreement",
    "",
    "| System | n | match | top-1 |",
    "|---|---:|---:|---:|",
    agg_lines,
    "",
    "## Evidence-grade distribution",
    "",
    "**WRB 2022**",
    "",
    fmt_grade(grade_wrb),
    "",
    "**SiBCS 5**",
    "",
    fmt_grade(grade_sibcs),
    "",
    "**USDA ST 13**",
    "",
    fmt_grade(grade_usda),
    "",
    "## Per-fixture results",
    "",
    paste0("| Fixture      | Target WRB    | Assigned WRB  | OK   | ",
             "Target SiBCS  | Assigned SiBCS | OK   | Target USDA   | ",
             "Assigned USDA | OK   |"),
    paste0("|---|---|---|:---:|---|---|:---:|---|---|:---:|"),
    bench_lines,
    "",
    "## Notes",
    "",
    "- A '.' in a target column indicates the fixture has no canonical",
    "  target in that system (e.g. Solonchak / Solonetz / Calcisol have",
    "  no direct SiBCS analogue in the 5ª edição).",
    "- Cross-system targets follow Schad (2023) Annex Table 1 (WRB <->",
    "  USDA) and the SiBCS 5ª ed. Annex A correspondence guide.",
    "- Sub-level (Subgroup / Família) concordance is not tested here --",
    "  only the highest categorical level (RSG / Ordem / Order). Sub-",
    "  level concordance is reserved for the WoSIS run.",
    "",
    "---",
    "",
    "_Report emitted by `run_canonical_benchmark()` in_",
    "_`inst/benchmarks/run_wosis_benchmark.R`._"
  )

  writeLines(report_lines, out_path, useBytes = TRUE)
  if (verbose) message(sprintf("Report written to %s", out_path))

  invisible(bench)
}
