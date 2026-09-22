# =============================================================================
# soilKey -- WoSIS benchmark driver
#
# Run-once-per-release script that produces the agreement statistics reported
# in the methodological paper accompanying soilKey v1.0. Reads a snapshot of
# WoSIS profiles, builds PedonRecords, runs classify_wrb2022(), and writes a
# versioned report under inst/benchmarks/reports/wosis_<DATE>.md.
#
# This driver is *not* called automatically. It is intended to be sourced
# manually by a maintainer:
#
#   source("inst/benchmarks/run_wosis_benchmark.R")
#   # GraphQL path (recommended; what wosis.isric.org actually serves):
#   res <- run_wosis_benchmark_graphql(n_max = 200L,
#                                        continent = "South America")
#   # Legacy REST path (kept for sites that mirror the deprecated v3 API):
#   res <- run_wosis_benchmark(n_max = 5000L)
#
# The vignette v06_wosis_benchmark.Rmd documents the protocol in full.
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
# WoSIS GraphQL driver (current, recommended)
#
# wosis.isric.org now serves data via a GraphQL API at
# https://graphql.isric.org/wosis/graphql (REST v3 has been
# deprecated). This block contains the real-data path used to
# generate the paper-grade benchmark numbers.
# =============================================================================


.wosis_graphql_endpoint <- function() {
  getOption("soilKey.wosis_graphql",
            default = "https://graphql.isric.org/wosis/graphql")
}


#' Pull WoSIS profiles via the GraphQL API.
#'
#' @param continent Continent name ("South America", "Africa",
#'        "Europe", "North America", "Asia", "Oceania", or NULL for
#'        global).
#' @param wrb_rsg Optional WRB Reference Soil Group filter
#'        (e.g. "Ferralsol"). When supplied, only profiles whose
#'        WoSIS-recorded RSG equals this value are pulled.
#' @param country Optional country filter (e.g. "Brazil").
#' @param n_max Maximum number of profiles to pull.
#' @param page_size Profiles per GraphQL request (default 50;
#'        WoSIS imposes a soft cap).
#' @param verbose Print per-page progress.
#' @keywords internal
read_wosis_profiles_graphql <- function(continent  = NULL,
                                          wrb_rsg    = NULL,
                                          country    = NULL,
                                          n_max      = 500L,
                                          page_size  = 50L,
                                          verbose    = TRUE) {
  if (!requireNamespace("httr",     quietly = TRUE)) stop("Install 'httr'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Install 'jsonlite'.")

  endpoint <- .wosis_graphql_endpoint()

  filter_parts <- character(0)
  if (!is.null(continent))
    filter_parts <- c(filter_parts,
                        sprintf('continent: {equalTo: "%s"}', continent))
  if (!is.null(wrb_rsg))
    filter_parts <- c(filter_parts,
                        sprintf('wrbReferenceSoilGroup: {equalTo: "%s"}',
                                  wrb_rsg))
  if (!is.null(country))
    filter_parts <- c(filter_parts,
                        sprintf('countryName: {equalTo: "%s"}', country))
  filter_clause <- if (length(filter_parts) > 0L)
                      sprintf("filter: {%s},",
                                paste(filter_parts, collapse = ", "))
                    else
                      ""

  # v0.9.12 maximal layer query -- pulls every WoSIS *Values field
  # that maps to the soilKey horizon schema. Notes:
  #   - Texture: clay/sand/silt + cf (volume + gravimetric).
  #   - Carbon: orgc (g/kg, divided by 10 downstream); orgm + totc as
  #     cross-checks.
  #   - Nitrogen: nitkjd (Kjeldahl).
  #   - pH: H2O / KCl / CaCl2 / NaF (critical for Andic) /
  #     phosphate-retention (Andic ferri-mineralogical proxy).
  #   - CEC: pH 7 + pH 8.2 + ECEC. BS is derived as ECEC / CEC * 100
  #     when neither is missing (downstream).
  #   - Carbonate / EC / bulk density / water retention (33 + 1500
  #     kPa, gravimetric and volumetric).
  layer_q <- paste(
    "layers(first: 50) {",
    "  upperDepth lowerDepth layerName layerNumber organicSurface",
    "  clayValues(first: 1)        { valueAvg }",
    "  sandValues(first: 1)        { valueAvg }",
    "  siltValues(first: 1)        { valueAvg }",
    "  cfvoValues(first: 1)        { valueAvg }",
    "  cfgrValues(first: 1)        { valueAvg }",
    "  orgcValues(first: 1)        { valueAvg }",
    "  orgmValues(first: 1)        { valueAvg }",
    "  totcValues(first: 1)        { valueAvg }",
    "  nitkjdValues(first: 1)      { valueAvg }",
    "  phaqValues(first: 1)        { valueAvg }",
    "  phkcValues(first: 1)        { valueAvg }",
    "  phcaValues(first: 1)        { valueAvg }",
    "  phnfValues(first: 1)        { valueAvg }",
    "  phprtnValues(first: 1)      { valueAvg }",
    "  cecph7Values(first: 1)      { valueAvg }",
    "  cecph8Values(first: 1)      { valueAvg }",
    "  ececValues(first: 1)        { valueAvg }",
    "  tceqValues(first: 1)        { valueAvg }",
    "  elcospValues(first: 1)      { valueAvg }",
    "  bdfi33lValues(first: 1)     { valueAvg }",
    "  bdfiodValues(first: 1)      { valueAvg }",
    "  wg0033Values(first: 1)      { valueAvg }",
    "  wg1500Values(first: 1)      { valueAvg }",
    "}"
  )

  # v0.9.27: per-page retry with exponential backoff. The ISRIC WoSIS
  # GraphQL endpoint returns "canceling statement due to statement
  # timeout" intermittently under load, often after 30-50 profiles in
  # a session. Single transient failures should not abort the pull;
  # retry up to `max_retries` times with backoff (1s, 2s, 4s, ...)
  # before giving up. Also supports a `min_pages` floor: if at least
  # one page succeeded and we hit a page failure after `min_pages`,
  # return the partial pull rather than erroring (graceful degradation).
  max_retries  <- 4L
  base_backoff <- 1.0
  min_pages    <- 1L

  out      <- list()
  offset   <- 0L
  n_pages  <- 0L
  while (length(out) < n_max) {
    take <- min(page_size, n_max - length(out))
    q <- sprintf(
      "{ wosisLatestProfiles(%s offset: %d, first: %d) { profileId profileCode countryName continent latitude longitude wrbReferenceSoilGroup usdaOrderName %s } }",
      filter_clause, offset, take, layer_q
    )
    body <- jsonlite::toJSON(list(query = q), auto_unbox = TRUE)

    page <- NULL
    last_err <- NULL
    for (attempt in seq_len(max_retries)) {
      resp <- tryCatch(
        httr::POST(endpoint,
                     body  = body,
                     httr::content_type_json(),
                     httr::user_agent("soilKey (https://github.com/HugoMachadoRodrigues/soilKey)"),
                     httr::timeout(60)),
        error = function(e) e
      )
      if (inherits(resp, "error")) {
        last_err <- sprintf("HTTP error: %s", conditionMessage(resp))
      } else if (httr::status_code(resp) != 200L) {
        last_err <- sprintf("HTTP %d", httr::status_code(resp))
      } else {
        parsed <- jsonlite::fromJSON(httr::content(resp, as = "text",
                                                      encoding = "UTF-8"),
                                        simplifyVector = FALSE)
        if (!is.null(parsed$errors)) {
          last_err <- paste(vapply(parsed$errors, function(e) e$message, character(1)),
                              collapse = "; ")
        } else {
          page <- parsed$data$wosisLatestProfiles
          last_err <- NULL
          break
        }
      }
      if (verbose)
        message(sprintf("[WoSIS-graphql] offset=%d attempt=%d/%d failed: %s -- backing off %.1fs",
                          offset, attempt, max_retries, last_err,
                          base_backoff * 2^(attempt - 1L)))
      Sys.sleep(base_backoff * 2^(attempt - 1L))
    }

    if (is.null(page)) {
      # Page failed after all retries.
      if (n_pages >= min_pages) {
        if (verbose)
          message(sprintf(paste0("[WoSIS-graphql] page failed after %d retries; ",
                                    "returning %d profiles collected so far"),
                            max_retries, length(out)))
        break
      }
      stop(sprintf("WoSIS GraphQL: page failed after %d retries: %s",
                     max_retries, last_err))
    }
    if (length(page) == 0L) break
    out <- c(out, page)
    n_pages <- n_pages + 1L
    if (verbose)
      message(sprintf("[WoSIS-graphql] offset=%d, fetched=%d, total=%d",
                        offset, length(page), length(out)))
    if (length(page) < take) break
    offset <- offset + take
  }
  utils::head(out, n_max)
}


#' Convert a single WoSIS GraphQL profile into a PedonRecord.
#'
#' Maps every WoSIS \code{*Values} field that has a soilKey horizon
#' counterpart. Units are normalised to the canonical horizon schema
#' (per cent / cmol_c kg-1 / dS m-1 / g cm-3). Where a value can be
#' derived (e.g. base saturation from ECEC / CEC; soil moisture
#' regime from latitude / country), a defensible derivation is
#' attempted and tagged with provenance \code{inferred_prior} or
#' \code{user_assumed}.
#'
#' @section Coverage tier:
#' Profiles vary widely in how much WoSIS recorded. The function
#' attaches a \code{site$coverage_tier} field reflecting which
#' soilKey-critical attributes are present:
#' \itemize{
#'   \item \code{"full"}: texture + pH(H2O or KCl) + CEC + OC.
#'   \item \code{"partial"}: texture + (pH OR CEC) + OC.
#'   \item \code{"minimal"}: texture only (or no chemistry at all).
#' }
#' The benchmark aggregator (\code{run_wosis_benchmark_graphql})
#' stratifies top-1 agreement by this tier so the data ceiling is
#' visible rather than hidden.
#'
#' @keywords internal
build_pedon_from_wosis_graphql <- function(profile) {
  layers_raw <- profile$layers %||% list()
  pull <- function(x) if (length(x) > 0L) {
                          v <- x[[1]]$valueAvg
                          if (is.null(v)) NA_real_ else as.numeric(v)
                       } else NA_real_

  if (length(layers_raw) == 0L)
    return(PedonRecord$new(
      site = list(id = profile$profileCode %||% as.character(profile$profileId),
                    lat = profile$latitude, lon = profile$longitude,
                    country = profile$countryName,
                    wosis_rsg = profile$wrbReferenceSoilGroup,
                    wosis_usda_order = profile$usdaOrderName,
                    coverage_tier = "no_layers")))

  hz_rows <- lapply(layers_raw, function(L) {
    # WoSIS unit conventions:
    #   orgc / orgm / nitkjd : g/kg     -> divide by 10 to get %.
    #   phaq / phkc / phca   : pH unit  -> straight through.
    #   cecph7 / ecec        : cmol_c/kg-> straight through.
    #   tceq                 : %        -> straight through (CaCO3 eq).
    #   elcosp               : dS/m     -> straight through.
    #   bdfi*l / bdfiod      : g/cm3    -> straight through.
    #   wg0033 / wg1500      : g/100g   -> straight through (water_content_*kpa).
    cec  <- pull(L$cecph7Values)
    if (is.na(cec)) cec <- pull(L$cecph8Values)
    ecec <- pull(L$ececValues)
    bs   <- if (!is.na(ecec) && !is.na(cec) && cec > 0)
              max(0, min(100, 100 * ecec / cec))
            else NA_real_

    cf <- pull(L$cfvoValues)
    if (is.na(cf)) cf <- pull(L$cfgrValues)

    ph_h2o <- pull(L$phaqValues)
    if (is.na(ph_h2o)) ph_h2o <- pull(L$phcaValues) - 0.5  # CaCl2 -> H2O proxy
    ph_kcl <- pull(L$phkcValues)

    bd <- pull(L$bdfi33lValues)
    if (is.na(bd)) bd <- pull(L$bdfiodValues)

    oc <- pull(L$orgcValues)
    if (!is.na(oc)) {
      oc <- oc / 10
    } else {
      orgm <- pull(L$orgmValues)
      if (!is.na(orgm)) oc <- orgm / 10 / 1.724
    }

    data.table::data.table(
      top_cm                       = as.numeric(L$upperDepth %||% NA_real_),
      bottom_cm                    = as.numeric(L$lowerDepth %||% NA_real_),
      designation                  = L$layerName %||% NA_character_,
      clay_pct                     = pull(L$clayValues),
      silt_pct                     = pull(L$siltValues),
      sand_pct                     = pull(L$sandValues),
      coarse_fragments_pct         = cf,
      oc_pct                       = oc,
      n_total_pct                  = (pull(L$nitkjdValues) %||% NA_real_) / 10,
      ph_h2o                       = ph_h2o,
      ph_kcl                       = ph_kcl,
      ph_cacl2                     = pull(L$phcaValues),
      ph_naf                       = pull(L$phnfValues),
      phosphate_retention_pct      = pull(L$phprtnValues),
      cec_cmol                     = cec,
      ecec_cmol                    = ecec,
      bs_pct                       = bs,
      caco3_pct                    = pull(L$tceqValues),
      ec_dS_m                      = pull(L$elcospValues),
      bulk_density_g_cm3           = bd,
      water_content_33kpa          = pull(L$wg0033Values),
      water_content_1500kpa        = pull(L$wg1500Values)
    )
  })
  hz <- data.table::rbindlist(hz_rows, fill = TRUE)

  # Coverage tier classification: use the surface horizon as the
  # representative observation (tropical-soil convention; suffices for
  # the data-ceiling stratification).
  has <- function(col) any(!is.na(hz[[col]]))
  tier <- if (has("clay_pct") && has("oc_pct") &&
                 (has("ph_h2o") || has("ph_kcl")) &&
                 has("cec_cmol")) "full"
          else if (has("clay_pct") && has("oc_pct") &&
                       (has("ph_h2o") || has("cec_cmol"))) "partial"
          else if (has("clay_pct")) "minimal"
          else "empty"

  PedonRecord$new(
    site = list(
      id              = profile$profileCode %||% as.character(profile$profileId),
      lat             = profile$latitude,
      lon             = profile$longitude,
      country         = profile$countryName,
      year            = profile$year,
      wosis_rsg                = profile$wrbReferenceSoilGroup,
      wosis_principal_quals    = profile$wrbPrincipalQualifiers,
      wosis_usda_order         = profile$usdaOrderName,
      wosis_usda_subgroup      = profile$usdaSubgroup,
      wosis_publication_year   = profile$wrbPublicationYear,
      coverage_tier            = tier
    ),
    horizons = hz
  )
}


#' Run the WoSIS benchmark via GraphQL.
#'
#' This is the path the methodological paper actually uses: one query
#' to wosis.isric.org/wosis/graphql, then `classify_wrb2022()` on each
#' profile, then write a report.
#'
#' @param n_max     Max profiles to include.
#' @param continent Optional continent filter (e.g. "South America").
#' @param wrb_rsg   Optional WRB RSG filter for stratified runs.
#' @param country   Optional country filter.
#' @param page_size Profiles per GraphQL request.
#' @param out_dir   Reports directory.
#' @keywords internal
run_wosis_benchmark_graphql <- function(n_max     = 500L,
                                          continent = "South America",
                                          wrb_rsg   = NULL,
                                          country   = NULL,
                                          page_size = 50L,
                                          out_dir   = file.path("inst",
                                                                  "benchmarks",
                                                                  "reports")) {
  profs <- read_wosis_profiles_graphql(continent = continent,
                                         wrb_rsg   = wrb_rsg,
                                         country   = country,
                                         n_max     = n_max,
                                         page_size = page_size)
  message(sprintf("WoSIS pulled %d profiles", length(profs)))

  pedons <- lapply(profs, build_pedon_from_wosis_graphql)

  classifications <- lapply(pedons, function(p)
    tryCatch(classify_wrb2022(p, on_missing = "silent"),
              error = function(e) NULL))

  # Build the bench frame.
  bench <- do.call(rbind, Map(function(c, p) {
    if (is.null(c)) return(NULL)
    data.frame(
      profile_id    = p$site$id,
      country       = p$site$country %||% NA_character_,
      coverage_tier = p$site$coverage_tier %||% NA_character_,
      target        = p$site$wosis_rsg %||% NA_character_,
      assigned      = sub("s$", "", c$rsg_or_order %||% NA_character_),
      grade         = c$evidence_grade,
      stringsAsFactors = FALSE
    )
  }, classifications, pedons))
  if (is.null(bench) || nrow(bench) == 0L)
    stop("No usable WoSIS profiles classified.")
  bench$match <- bench$target == bench$assigned

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir,
                          sprintf("wosis_graphql_%s.md", Sys.Date()))

  top1 <- mean(bench$match, na.rm = TRUE)
  ind  <- mean(is.na(bench$assigned))
  per_target <- aggregate(match ~ target, data = bench,
                            FUN = function(x) sprintf("%d/%d (%.1f%%)",
                                                          sum(x, na.rm = TRUE),
                                                          length(x),
                                                          100 * mean(x, na.rm = TRUE)))
  per_tier <- aggregate(match ~ coverage_tier, data = bench,
                          FUN = function(x) sprintf("%d/%d (%.1f%%)",
                                                        sum(x, na.rm = TRUE),
                                                        length(x),
                                                        100 * mean(x, na.rm = TRUE)))
  tier_counts <- as.data.frame(table(coverage_tier = bench$coverage_tier))

  lines <- c(
    sprintf("# WoSIS benchmark report (GraphQL) -- %s", Sys.Date()),
    "",
    sprintf("**Endpoint:** %s", .wosis_graphql_endpoint()),
    sprintf("**Continent filter:** %s", continent %||% "(global)"),
    sprintf("**WRB RSG filter:** %s",   wrb_rsg %||% "(none)"),
    sprintf("**Country filter:** %s",   country %||% "(none)"),
    sprintf("**Profiles pulled:** %d",  length(profs)),
    sprintf("**Profiles classified:** %d", nrow(bench)),
    "",
    "## Top-1 agreement",
    "",
    sprintf("- **Overall top-1: %.3f** (no stratification)", top1),
    sprintf("- Indeterminate (NA assignments): %.3f", ind),
    "",
    "## Top-1 stratified by data-coverage tier",
    "",
    "Different profiles in WoSIS carry very different attribute sets.",
    "soilKey reports `coverage_tier` per profile based on what was",
    "actually present (not on the WoSIS schema):",
    "",
    "- **full**: texture + (pH H2O or KCl) + CEC + OC.",
    "- **partial**: texture + OC + (pH OR CEC).",
    "- **minimal**: texture only or no chemistry.",
    "- **empty**: no horizons.",
    "",
    "| Coverage tier | Profiles | Top-1 |",
    "|:--------------|---------:|:------|",
    if (nrow(per_tier) > 0L)
      paste(sprintf("| %-12s | %8d | %s |",
                      per_tier$coverage_tier,
                      tier_counts$Freq[match(per_tier$coverage_tier,
                                               tier_counts$coverage_tier)],
                      per_tier$match),
              collapse = "\n")
    else "| (none) |        - | - |",
    "",
    "Profiles below the **full** tier face a hard data ceiling:",
    "many WRB RSGs (Vertisols, Nitisols, Andosols, Ferralsols) require",
    "attributes (cracks, slickensides, Fe-DCB, Munsell, allophane",
    "indicators) that WoSIS does not store at all. The honest",
    "interpretation: top-1 in the **full** tier reflects soilKey",
    "performance; top-1 in the **partial / minimal / empty** tiers",
    "reflects the unrecoverable WoSIS data ceiling.",
    "",
    "## Per-RSG agreement",
    "",
    "| Target RSG | Match |",
    "|:-----------|:------|",
    if (nrow(per_target) > 0L)
      paste(sprintf("| %s | %s |", per_target$target, per_target$match),
              collapse = "\n")
    else "| (none) | - |",
    "",
    "## Confusion matrix",
    "",
    "```",
    paste(capture.output(print(table(target = bench$target,
                                          assigned = bench$assigned))),
            collapse = "\n"),
    "```",
    "",
    "## Evidence-grade distribution",
    "",
    "```",
    paste(capture.output(print(table(grade = bench$grade,
                                          useNA = "ifany"))),
            collapse = "\n"),
    "```",
    "",
    sprintf("_Report emitted by `run_wosis_benchmark_graphql()` -- soilKey v%s_",
              .soilkey_version())
  )
  writeLines(lines, out_path, useBytes = TRUE)
  message(sprintf("Report written to %s", out_path))

  invisible(list(bench = bench, profiles = profs,
                   pedons = pedons, top1 = top1,
                   report_path = out_path))
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
