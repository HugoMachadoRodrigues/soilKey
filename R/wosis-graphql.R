# =============================================================================
# soilKey -- live WoSIS queries.
#
# soilKey ships no soil observations (see inst/DATA-PROVENANCE.md). This is how it
# reads them instead: each call goes from the caller's own machine to ISRIC,
# under the caller's own acceptance of ISRIC's terms, and nothing is written to
# disk. soilKey never holds a copy, so it never redistributes.
#
# LICENCE IS NOT UNIFORM ACROSS WoSIS, and this is the part that matters.
# ISRIC licences per profile, as the provider specified: the "WoSIS latest"
# record states "Licenced per profile, as specified by data provider and
# indicated in the data". Sampling 800 layers of wosis_latest found roughly half
# CC BY (3.0/4.0 and US public domain) and half CC BY-NC. A query that does not
# look at the `licence` field will therefore return NonCommercial material about
# half the time, silently.
#
# That is why `licence_filter` defaults to "permissive" and why every profile
# returned carries its licence and dataset with it. CC BY requires attribution
# on reuse; dropping those fields, as an earlier bundled snapshot did, is what
# turns a licensed use into an unattributed one.
# =============================================================================

.WOSIS_ENDPOINT <- "https://graphql.isric.org/wosis/graphql"

#' Is this licence string free of a NonCommercial restriction?
#' @noRd
.wosis_is_permissive <- function(licence) {
  if (is.null(licence) || !length(licence)) return(logical(0))
  l <- as.character(licence)
  # is.na() is checked explicitly: `%||%` only catches NULL, and nzchar(NA) is
  # TRUE by default, so an NA licence would otherwise read as permissive. That
  # is the fail-OPEN direction, which here means shipping somebody else's data
  # under rights nobody granted.
  !is.na(l) & nzchar(l) & !grepl("NonCommercial|BY-NC", l, ignore.case = TRUE)
}

#' Short label for a WoSIS licence string, for display in a table.
#' @noRd
.wosis_licence_short <- function(licence) {
  l <- as.character(licence %||% "")
  ifelse(!nzchar(l), NA_character_,
  ifelse(grepl("NonCommercial", l, ignore.case = TRUE),
         ifelse(grepl("4\\.0", l), "CC BY-NC 4.0", "CC BY-NC 3.0"),
  ifelse(grepl("Public Domain", l, ignore.case = TRUE), "Public domain",
  ifelse(grepl("4\\.0", l), "CC BY 4.0", "CC BY 3.0"))))
}

#' Query WoSIS profiles live from ISRIC
#'
#' Reads soil profiles straight from ISRIC's WoSIS GraphQL endpoint. Nothing is
#' cached to disk: soilKey distributes no soil observations, and each call is
#' made by you, to ISRIC, under ISRIC's terms.
#'
#' @section Licence:
#' WoSIS is licensed \emph{per profile} as each data provider specified, and
#' about half of it carries a NonCommercial restriction. \code{licence_filter =
#' "permissive"} (the default) returns only profiles free of that restriction
#' (CC BY, or public domain); \code{"any"} returns everything. Either way the
#' licence and the source dataset travel with every profile, because CC BY
#' requires attribution when you reuse the data.
#'
#' @param country Optional country name, e.g. \code{"Brazil"}.
#' @param wrb_rsg Optional WRB Reference Soil Group, e.g. \code{"Ferralsols"}.
#' @param n_max Maximum profiles to return (default 25).
#' @param licence_filter \code{"permissive"} (default) or \code{"any"}.
#' @param timeout Seconds to wait for ISRIC (default 30).
#' @return A data.frame, one row per profile, with \code{profile_id},
#'   \code{profile_code}, \code{country}, \code{lat}, \code{lon},
#'   \code{wrb_rsg}, \code{usda_order}, \code{dataset}, \code{licence} and
#'   \code{licence_short}. Zero rows if nothing matched. Never throws on a
#'   network failure: it returns zero rows with a \code{"wosis_error"}
#'   attribute, since an unreachable server is an expected condition.
#' @seealso \code{\link{wosis_profile_to_pedon}} to turn a row into a
#'   \code{\link{PedonRecord}}.
#' @examples
#' \donttest{
#' # Requires network access to ISRIC.
#' p <- read_wosis_profiles_graphql(country = "Brazil", n_max = 5)
#' if (nrow(p)) p[, c("profile_code", "wrb_rsg", "licence_short")]
#' }
#' @export
read_wosis_profiles_graphql <- function(country = NULL,
                                         wrb_rsg = NULL,
                                         n_max   = 25L,
                                         licence_filter = c("permissive", "any"),
                                         timeout = 30) {
  licence_filter <- match.arg(licence_filter)
  empty <- data.frame(
    profile_id = character(0), profile_code = character(0),
    country = character(0), lat = numeric(0), lon = numeric(0),
    wrb_rsg = character(0), usda_order = character(0),
    dataset = character(0), licence = character(0),
    licence_short = character(0), stringsAsFactors = FALSE)
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)) {
    attr(empty, "wosis_error") <- "httr and jsonlite are required"
    return(empty)
  }

  # Over-fetch when filtering, since an unknown share of the page will be
  # NonCommercial and dropped.
  fetch_n <- if (identical(licence_filter, "permissive"))
    min(as.integer(n_max) * 4L, 200L) else as.integer(n_max)

  flt <- character(0)
  if (!is.null(country) && nzchar(country))
    flt <- c(flt, sprintf('countryName: {equalTo: "%s"}', country))
  if (!is.null(wrb_rsg) && nzchar(wrb_rsg))
    flt <- c(flt, sprintf('wrbReferenceSoilGroup: {equalTo: "%s"}', wrb_rsg))
  filter_arg <- if (length(flt))
    sprintf("filter: {%s}, ", paste(flt, collapse = ", ")) else ""

  q <- sprintf(paste0(
    '{ wosisLatestProfiles(%sfirst: %d) { profileId profileCode countryName ',
    'latitude longitude wrbReferenceSoilGroup usdaOrderName datasetCode ',
    'layers(first: 1) { licence } } }'), filter_arg, fetch_n)

  resp <- tryCatch(
    httr::POST(.WOSIS_ENDPOINT,
               body = list(query = q), encode = "json",
               httr::content_type_json(), httr::timeout(timeout)),
    error = function(e) e)
  if (inherits(resp, "error")) {
    attr(empty, "wosis_error") <- conditionMessage(resp); return(empty)
  }
  if (httr::status_code(resp) != 200L) {
    attr(empty, "wosis_error") <- paste("ISRIC returned HTTP",
                                        httr::status_code(resp))
    return(empty)
  }
  body <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"),
                   error = function(e) NULL)
  if (!is.null(body$errors)) {
    attr(empty, "wosis_error") <-
      as.character(body$errors[[1]]$message %||% "GraphQL error")
    return(empty)
  }
  rows <- body$data$wosisLatestProfiles
  if (is.null(rows) || !length(rows)) return(empty)

  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)[1]
  num <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x)[1])
  out <- do.call(rbind, lapply(rows, function(p) {
    lic <- chr((p$layers[[1]]$licence)[1] %||% NA_character_)
    data.frame(
      profile_id   = chr(p$profileId),
      profile_code = chr(p$profileCode),
      country      = chr(p$countryName),
      lat          = num(p$latitude),
      lon          = num(p$longitude),
      wrb_rsg      = chr(p$wrbReferenceSoilGroup),
      usda_order   = chr(p$usdaOrderName),
      dataset      = chr(p$datasetCode),
      licence      = lic,
      licence_short = .wosis_licence_short(lic),
      stringsAsFactors = FALSE)
  }))

  # A profile whose licence could not be read is dropped under "permissive":
  # unknown terms are not permissive terms.
  n_seen <- nrow(out)
  if (identical(licence_filter, "permissive")) {
    keep <- .wosis_is_permissive(out$licence)
    excluded <- out[!keep, , drop = FALSE]
    out <- out[keep, , drop = FALSE]
    # Report what was withheld and under which licence. A picker that simply
    # shows nothing for a country looks broken; one that says "12 profiles
    # withheld (CC BY-NC 4.0)" is telling the truth about why.
    attr(out, "wosis_excluded") <- nrow(excluded)
    attr(out, "wosis_excluded_licences") <-
      sort(unique(stats::na.omit(excluded$licence_short)))
  } else {
    attr(out, "wosis_excluded") <- 0L
    attr(out, "wosis_excluded_licences") <- character(0)
  }
  attr(out, "wosis_seen") <- n_seen
  if (nrow(out) > n_max) out <- out[seq_len(n_max), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Turn one WoSIS profile row into a PedonRecord
#'
#' The licence and source dataset are carried onto the record's site metadata,
#' so that attribution survives into anything the user later exports. CC BY
#' requires it, and an extract that drops it turns a licensed use into an
#' unattributed one.
#'
#' @param row One row of \code{\link{read_wosis_profiles_graphql}}.
#' @return A \code{\link{PedonRecord}} with site metadata and no horizons; the
#'   profile's layers are not fetched here.
#' @export
wosis_profile_to_pedon <- function(row) {
  stopifnot(is.data.frame(row), nrow(row) == 1L)
  PedonRecord$new(site = list(
    id              = row$profile_code,
    lat             = row$lat,
    lon             = row$lon,
    country         = row$country,
    wosis_rsg       = row$wrb_rsg,
    wosis_usda_order = row$usda_order,
    source          = "WoSIS (ISRIC)",
    dataset         = row$dataset,
    licence         = row$licence,
    attribution     = paste0(
      "Soil profile ", row$profile_code, " from dataset ", row$dataset,
      ", obtained through ISRIC WoSIS (https://www.isric.org/explore/wosis). ",
      "Licence: ", row$licence)))
}
