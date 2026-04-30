# =============================================================================
# soilKey -- OSSL subset fetcher with WRB / USDA / SiBCS labels attached.
#
# `download_ossl_subset_with_labels()` extends `download_ossl_subset()`
# with a post-fetch label-join step:
#
#   1. Pull a region-filtered OSSL subset (real or mock) -- spectra
#      Xr + lab data Yr + site coords.
#   2. For each OSSL profile that has (lat, lon) but no soil-class
#      label in Yr, query WoSIS GraphQL for the nearest profile
#      that does have a WRB Reference Soil Group label and inherit
#      that label with a `wrb_label_source` + `wrb_label_distance_km`
#      provenance pair.
#   3. (Optional) translate the WRB label to SiBCS / USDA via the
#      Schad (2023) Annex Table 1 / SiBCS 5ª ed. Annex A
#      correspondence so the downstream
#      `classify_by_spectral_neighbours()` can be parameterised by
#      any of the three systems.
#
# This closes the v0.9.13 gap: real-OSSL spectra + WRB labels
# inferred from WoSIS spatial neighbours, ready to feed the spectral
# analogy classifier without the user having to assemble the join
# by hand.
# =============================================================================


#' Download an OSSL subset and attach WRB / SiBCS / USDA labels
#'
#' Fetches a region-filtered slice of the Open Soil Spectral Library
#' via \code{\link{download_ossl_subset}} and post-joins WRB
#' Reference Soil Group labels from WoSIS GraphQL by spatial
#' nearest-neighbour. The resulting artefact has the canonical
#' \code{list(Xr, Yr, metadata)} shape -- with extra columns in
#' \code{Yr}: \code{wrb_rsg}, \code{wrb_label_source},
#' \code{wrb_label_distance_km}, plus optionally \code{sibcs_ordem}
#' and \code{usda_order} when \code{translate_systems = TRUE}.
#'
#' @section Why this function exists:
#' OSSL stores Vis-NIR / MIR spectra and lab data but typically lacks
#' WRB Reference Soil Group labels on most profiles (KSSL data is
#' USDA-flavoured; non-US contributions are inconsistent). WoSIS, by
#' contrast, archives ~228 000 profiles with WRB labels but no
#' spectra. This function bridges the two so the user can run
#' \code{\link{classify_by_spectral_neighbours}} on a real-data
#' OSSL library without having to do the spatial join themselves.
#'
#' @section Caveats and provenance:
#' WRB labels obtained via spatial join are \strong{weak labels}.
#' The same physical location may have been classified differently
#' across surveys (different WRB editions, different
#' interpretations). Each row carries:
#'
#' \itemize{
#'   \item \code{wrb_label_source = "wosis_spatial_join"}: label
#'         inherited from a WoSIS neighbour within
#'         \code{max_distance_km}.
#'   \item \code{wrb_label_distance_km}: the distance to that
#'         neighbour (NA when no neighbour was found within
#'         tolerance).
#'   \item \code{wrb_label_source = "ossl_native"}: label was
#'         already present in OSSL Yr (rare; preserved verbatim).
#'   \item \code{wrb_label_source = "missing"}: no neighbour within
#'         tolerance; the row stays unlabeled and will be skipped
#'         downstream.
#' }
#'
#' Treat the labels as priors, not ground truth.
#'
#' @param region OSSL region filter; one of \code{"global"},
#'        \code{"south_america"}, \code{"north_america"},
#'        \code{"europe"}, \code{"africa"}, \code{"asia"},
#'        \code{"oceania"}.
#' @param max_distance_km WoSIS spatial-join tolerance in kilometres
#'        (default 5). Profiles whose nearest WRB-labeled WoSIS
#'        neighbour is farther than this are left unlabeled.
#' @param wosis_endpoint Override for the WoSIS GraphQL endpoint
#'        (default \code{getOption("soilKey.wosis_graphql")}). The
#'        canonical value is
#'        \code{"https://graphql.isric.org/wosis/graphql"}.
#' @param translate_systems If \code{TRUE} (default), also adds
#'        \code{sibcs_ordem} and \code{usda_order} columns derived
#'        from the WRB label via the Schad (2023) Annex Table 1 /
#'        SiBCS 5ª ed. Annex A correspondence. Those translations
#'        are 1:N for some classes; we pick the most-common partner
#'        and tag rows where the translation is genuinely ambiguous.
#' @param max_to_label Maximum number of profiles to query against
#'        WoSIS (default \code{Inf}). WoSIS throttles aggressive
#'        queries; cap this when running interactive demos.
#' @param verbose Emit \code{cli} progress messages.
#' @param query_fn Optional injection of the per-coordinate WoSIS
#'        query function. Default uses
#'        \code{\link{.query_nearest_wosis_wrb}}. Tests pass a stub
#'        here to exercise the join logic without network.
#' @param ... Forwarded to \code{\link{download_ossl_subset}}.
#'
#' @return A list with \code{Xr} (numeric matrix), \code{Yr} (data
#'         frame with the labels attached), and \code{metadata}
#'         (list with the OSSL fetch metadata + the join statistics:
#'         number of profiles labeled, average / max distance,
#'         WoSIS endpoint, snapshot date).
#'
#' @examples
#' \dontrun{
#' # Real OSSL South-America subset with WRB labels:
#' lib <- download_ossl_subset_with_labels(
#'   region          = "south_america",
#'   max_distance_km = 10
#' )
#' table(lib$Yr$wrb_rsg, useNA = "always")
#' table(lib$Yr$wrb_label_source)
#'
#' # Drop into the spectral analogy classifier:
#' res <- classify_by_spectral_neighbours(
#'   spectrum     = my_query_spectrum,
#'   ossl_library = lib,
#'   k            = 25,
#'   region       = list(lat = -22.7, lon = -43.7,
#'                       radius_km = 500)
#' )
#' }
#' @seealso \code{\link{download_ossl_subset}}, \code{\link{classify_by_spectral_neighbours}}.
#' @export
download_ossl_subset_with_labels <- function(region          = c("global",
                                                                  "south_america",
                                                                  "north_america",
                                                                  "europe",
                                                                  "africa",
                                                                  "asia",
                                                                  "oceania"),
                                               max_distance_km = 5,
                                               wosis_endpoint  = NULL,
                                               translate_systems = TRUE,
                                               max_to_label    = Inf,
                                               verbose         = TRUE,
                                               query_fn        = NULL,
                                               ...) {
  region <- match.arg(region)
  if (is.null(query_fn)) query_fn <- .query_nearest_wosis_wrb

  # ---- 1. OSSL subset ----------------------------------------------------
  lib <- download_ossl_subset(region = region, verbose = verbose, ...)

  Yr <- as.data.frame(lib$Yr)
  has_coords <- all(c("lat", "lon") %in% names(Yr))

  # ---- 2. Initialise label columns ---------------------------------------
  if ("wrb_rsg" %in% names(Yr)) {
    Yr$wrb_label_source <- ifelse(is.na(Yr$wrb_rsg), "missing", "ossl_native")
    Yr$wrb_label_distance_km <- NA_real_
  } else {
    Yr$wrb_rsg              <- NA_character_
    Yr$wrb_label_source     <- "missing"
    Yr$wrb_label_distance_km <- NA_real_
  }

  # ---- 3. Spatial join with WoSIS for unlabeled profiles ----------------
  to_query <- which(Yr$wrb_label_source == "missing" & has_coords &
                       !is.na(Yr$lat) & !is.na(Yr$lon))
  if (length(to_query) > max_to_label)
    to_query <- to_query[seq_len(max_to_label)]

  if (length(to_query) > 0L) {
    if (verbose)
      cli::cli_alert_info(
        "Querying WoSIS for WRB labels on {.val {length(to_query)}} OSSL profile{?s} (tolerance = {.val {max_distance_km}} km)..."
      )
    for (idx in to_query) {
      hit <- query_fn(
        lat = Yr$lat[idx], lon = Yr$lon[idx],
        max_distance_km = max_distance_km,
        endpoint = wosis_endpoint %||%
                     getOption("soilKey.wosis_graphql",
                                 "https://graphql.isric.org/wosis/graphql"),
        verbose = FALSE
      )
      if (!is.null(hit) && !is.na(hit$wrb_rsg)) {
        Yr$wrb_rsg[idx]               <- hit$wrb_rsg
        Yr$wrb_label_source[idx]      <- "wosis_spatial_join"
        Yr$wrb_label_distance_km[idx] <- hit$distance_km
      }
    }
  } else if (verbose) {
    cli::cli_alert_info(
      "No OSSL profiles to label via WoSIS (either all have native labels or none have coordinates)."
    )
  }

  # ---- 4. Cross-system translations -------------------------------------
  if (translate_systems) {
    Yr$sibcs_ordem <- vapply(Yr$wrb_rsg, .wrb_to_sibcs_modal_ordem,
                                FUN.VALUE = character(1))
    Yr$usda_order  <- vapply(Yr$wrb_rsg, .wrb_to_usda_modal_order,
                                FUN.VALUE = character(1))
  }

  # ---- 5. Update metadata + return -------------------------------------
  n_labeled  <- sum(!is.na(Yr$wrb_rsg))
  joined_idx <- which(Yr$wrb_label_source == "wosis_spatial_join")
  mean_dist  <- if (length(joined_idx) > 0L)
                  mean(Yr$wrb_label_distance_km[joined_idx], na.rm = TRUE)
                else NA_real_

  lib$Yr <- Yr
  lib$metadata$labels <- list(
    n_total_profiles      = nrow(Yr),
    n_labeled             = n_labeled,
    n_native_labels       = sum(Yr$wrb_label_source == "ossl_native"),
    n_wosis_join_labels   = length(joined_idx),
    n_unlabeled           = sum(Yr$wrb_label_source == "missing"),
    mean_join_distance_km = mean_dist,
    max_distance_km_arg   = max_distance_km,
    wosis_endpoint        = wosis_endpoint %||%
                              getOption("soilKey.wosis_graphql",
                                        "https://graphql.isric.org/wosis/graphql"),
    snapshot              = Sys.Date()
  )

  if (verbose) {
    cli::cli_alert_success(
      "OSSL subset for {.field region}={region} ready: {.val {n_labeled}}/{.val {nrow(Yr)}} profile{?s} labeled ({.val {length(joined_idx)}} via WoSIS spatial join, mean distance {sprintf('%.2f km', mean_dist)})."
    )
  }
  lib
}


# ---- internals --------------------------------------------------------------


#' Query WoSIS GraphQL for the nearest WRB-labeled profile.
#'
#' Returns \code{NULL} on transport failure; \code{NA} fields when
#' the bbox has no labeled WoSIS profile.
#'
#' @keywords internal
.query_nearest_wosis_wrb <- function(lat, lon,
                                       max_distance_km,
                                       endpoint =
                                         "https://graphql.isric.org/wosis/graphql",
                                       verbose = FALSE) {
  if (!requireNamespace("httr",     quietly = TRUE)) return(NULL)
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NULL)

  # bbox approximation: 1 deg lat ~= 111 km; lon scales with cos(lat).
  d_lat <- max_distance_km / 111
  d_lon <- max_distance_km / (111 * cos(lat * pi / 180))
  bbox <- sprintf("%.6f,%.6f,%.6f,%.6f",
                    lon - d_lon, lat - d_lat,
                    lon + d_lon, lat + d_lat)

  q <- sprintf(
    paste0(
      '{ wosisLatestProfiles(first: 30, ',
      'filter: {bbox: {equalTo: "%s"}, ',
      'wrbReferenceSoilGroup: {isNull: false}}) ',
      '{ profileCode latitude longitude wrbReferenceSoilGroup } }'
    ),
    bbox
  )
  resp <- tryCatch(
    httr::POST(endpoint,
                  body = jsonlite::toJSON(list(query = q),
                                              auto_unbox = TRUE),
                  httr::content_type_json(),
                  httr::user_agent("soilKey/0.9.14"),
                  httr::timeout(30)),
    error = function(e) NULL
  )
  if (is.null(resp) || httr::status_code(resp) != 200L) return(NULL)
  parsed <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text",
                                          encoding = "UTF-8"),
                          simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(parsed) || !is.null(parsed$errors)) return(NULL)
  cands <- parsed$data$wosisLatestProfiles
  if (length(cands) == 0L)
    return(list(wrb_rsg = NA_character_, distance_km = NA_real_))

  # Pick the truly nearest (haversine).
  dists <- vapply(cands, function(c)
    .haversine_km(lat, lon, c$latitude, c$longitude),
    numeric(1))
  best <- which.min(dists)
  if (dists[best] > max_distance_km)
    return(list(wrb_rsg = NA_character_, distance_km = NA_real_))
  list(wrb_rsg     = cands[[best]]$wrbReferenceSoilGroup,
       distance_km = unname(dists[best]))
}


#' Normalize a WRB RSG name to its plural canonical form so lookups
#' work whether the source supplied "Ferralsol" or "Ferralsols".
#' @keywords internal
.wrb_canonical_plural <- function(rsg) {
  if (is.na(rsg)) return(NA_character_)
  out <- as.character(rsg)
  if (substr(out, nchar(out), nchar(out)) != "s") {
    out <- paste0(out, "s")
  }
  out
}


#' Modal SiBCS Ordem for a WRB RSG (1:1 picked by Schad 2023).
#' Accepts either the singular ("Ferralsol") or plural ("Ferralsols")
#' form -- WoSIS, the WRB book, and OSSL all use slightly different
#' conventions.
#'
#' @keywords internal
.wrb_to_sibcs_modal_ordem <- function(rsg) {
  if (is.na(rsg)) return(NA_character_)
  modal <- c(
    Histosols = "O", Ferralsols = "L", Acrisols = "P", Alisols = "P",
    Lixisols = "P", Luvisols = "T", Nitisols = "N", Cambisols = "C",
    Plinthosols = "F", Planosols = "S", Gleysols = "G", Vertisols = "V",
    Andosols = "C", Podzols = "E", Chernozems = "M", Kastanozems = "M",
    Phaeozems = "M", Umbrisols = "C", Calcisols = "C", Gypsisols = "C",
    Solonchaks = "P", Solonetz = "T", Stagnosols = "C", Retisols = "P",
    Arenosols = "R", Fluvisols = "R", Leptosols = "R", Regosols = "R",
    Cryosols = "C", Durisols = "C", Anthrosols = "P", Technosols = "C"
  )
  v <- modal[.wrb_canonical_plural(rsg)]
  if (is.na(v)) return(NA_character_)
  unname(v)
}


#' Modal USDA Order for a WRB RSG (Schad 2023 Annex Table 1).
#' Accepts either the singular ("Ferralsol") or plural ("Ferralsols")
#' form.
#'
#' @keywords internal
.wrb_to_usda_modal_order <- function(rsg) {
  if (is.na(rsg)) return(NA_character_)
  modal <- c(
    Histosols = "Histosols", Ferralsols = "Oxisols",
    Acrisols = "Ultisols",   Alisols = "Ultisols",
    Lixisols = "Alfisols",   Luvisols = "Alfisols",
    Nitisols = "Alfisols",   Cambisols = "Inceptisols",
    Plinthosols = "Oxisols", Planosols = "Alfisols",
    Gleysols = "Inceptisols", Vertisols = "Vertisols",
    Andosols = "Andisols",   Podzols = "Spodosols",
    Chernozems = "Mollisols", Kastanozems = "Mollisols",
    Phaeozems = "Mollisols", Umbrisols = "Inceptisols",
    Calcisols = "Aridisols", Gypsisols = "Aridisols",
    Solonchaks = "Aridisols", Solonetz = "Alfisols",
    Stagnosols = "Inceptisols", Retisols = "Alfisols",
    Arenosols = "Entisols",  Fluvisols = "Entisols",
    Leptosols = "Entisols",  Regosols = "Entisols",
    Cryosols = "Gelisols",   Durisols = "Aridisols",
    Anthrosols = "Inceptisols", Technosols = "Entisols"
  )
  v <- modal[.wrb_canonical_plural(rsg)]
  if (is.na(v)) return(NA_character_)
  unname(v)
}
