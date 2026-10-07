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
#
# ATTRIBUTION. The ISRIC Data and Software Policy requires that "data users must
# incorporate in their products, web-services, scientific papers, and reports
# proper reference to the data provider and acknowledgements to the fact that
# the underlying data were acquired through ISRIC". wosis_citation() returns the
# citation ISRIC asks for, and every pedon built from WoSIS carries it, the
# licence, the source dataset and the date it was read.
#
# COURTESY. The endpoint is a free public service. Queries run only on an
# explicit request, ask for one profile's layers at a time, and identify soilKey
# in the User-Agent so ISRIC can see where the traffic comes from.
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
  # NA is treated like an empty string. Without this, nzchar(NA) is TRUE and an
  # unknown licence fell through every test to the last branch and was labelled
  # "CC BY 3.0" -- displaying unknown terms as permissive ones.
  l[is.na(l)] <- ""
  ifelse(!nzchar(l), NA_character_,
  ifelse(grepl("NonCommercial", l, ignore.case = TRUE),
         ifelse(grepl("4\\.0", l), "CC BY-NC 4.0", "CC BY-NC 3.0"),
  ifelse(grepl("Public Domain", l, ignore.case = TRUE), "Public domain",
  ifelse(grepl("4\\.0", l), "CC BY 4.0", "CC BY 3.0"))))
}

#' The more restrictive of a set of licence strings (NonCommercial wins).
#' @noRd
.wosis_most_restrictive <- function(licences) {
  l <- unique(stats::na.omit(as.character(licences)))
  l <- l[nzchar(l)]
  if (!length(l)) return(NA_character_)
  nc <- l[!.wosis_is_permissive(l)]
  if (length(nc)) nc[1] else l[1]
}

#' POST one GraphQL query to the WoSIS endpoint.
#'
#' Variables are passed separately from the query text, so user input (a country
#' name, say) can never alter the query that is sent to ISRIC.
#' @return list(data = <parsed data or NULL>, error = <message or NULL>)
#' @noRd
.wosis_post <- function(query, variables = NULL, timeout = 30) {
  if (!requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE))
    return(list(data = NULL, error = "httr and jsonlite are required"))
  body <- list(query = query)
  if (length(variables)) body$variables <- variables
  ua <- sprintf("soilKey/%s (R package; https://github.com/HugoMachadoRodrigues/soilKey)",
                tryCatch(as.character(utils::packageVersion("soilKey")),
                         error = function(e) "dev"))
  resp <- tryCatch(
    httr::POST(.WOSIS_ENDPOINT, body = body, encode = "json",
               httr::content_type_json(), httr::user_agent(ua),
               httr::timeout(timeout)),
    error = function(e) e)
  if (inherits(resp, "error"))
    return(list(data = NULL, error = conditionMessage(resp)))
  parsed <- tryCatch(httr::content(resp, as = "parsed", type = "application/json"),
                     error = function(e) NULL)
  if (httr::status_code(resp) != 200L) {
    # A rejected query comes back as HTTP 400 with the reason in the body; keep
    # the reason, since "HTTP 400" alone tells nobody what went wrong.
    why <- if (!is.null(parsed$errors)) as.character(parsed$errors[[1]]$message) else NULL
    return(list(data = NULL,
                error = paste0("ISRIC returned HTTP ", httr::status_code(resp),
                               if (length(why)) paste0(": ", why) else "")))
  }
  if (is.null(parsed))
    return(list(data = NULL, error = "could not parse the ISRIC response"))
  if (!is.null(parsed$errors))
    return(list(data = NULL,
                error = as.character(parsed$errors[[1]]$message %||% "GraphQL error")))
  list(data = parsed$data, error = NULL)
}

#' Citation for data read from WoSIS-latest
#'
#' Returns the citation ISRIC asks users of WoSIS-latest to give, with the date
#' the data were read. The ISRIC Data and Software Policy requires products,
#' web services, papers and reports that use the data to reference the provider
#' and acknowledge that the data were acquired through ISRIC.
#'
#' @param accessed Date the data were read (default today).
#' @return A character string.
#' @examples
#' wosis_citation(as.Date("2026-10-06"))
#' @export
wosis_citation <- function(accessed = Sys.Date()) {
  paste0("Batjes NH, Calisto L and de Sousa LM, 2024. WoSIS-latest: ",
         "Standardised world soil profile data. ISRIC Soil Data Hub resource ",
         "identifier: https://tinyurl.com/39xhaa9d. Date downloaded: ",
         format(as.Date(accessed), "%Y-%m-%d"), ".")
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
#' requires attribution when you reuse the data. Cite the data with
#' \code{\link{wosis_citation}}.
#'
#' @section Load on ISRIC:
#' The endpoint accepts at most 100 profiles per request, so profiles are read
#' in pages of 100, in a stable order, and at most \code{max_scan} are examined
#' per call. The number of layers and the depth of each listed profile come
#' from one light query on the layers table, which lets a caller tell a full
#' profile from a single topsoil sample before loading it.
#'
#' @param country Optional country name, e.g. \code{"Brazil"}.
#' @param wrb_rsg Optional WRB Reference Soil Group, e.g. \code{"Ferralsols"}.
#' @param n_max Maximum profiles to return (default 25, at most 100).
#' @param licence_filter \code{"permissive"} (default) or \code{"any"}.
#' @param timeout Seconds to wait for each request (default 30).
#' @param max_scan Most profiles examined per call (default 300).
#' @return A data.frame, one row per profile, with \code{profile_id},
#'   \code{profile_code}, \code{country}, \code{lat}, \code{lon},
#'   \code{wrb_rsg}, \code{usda_order}, \code{dataset}, \code{licence},
#'   \code{licence_short}, \code{n_layers} and \code{depth_cm}. Zero rows if
#'   nothing matched. Never throws on a network failure: it returns zero rows
#'   with a \code{"wosis_error"} attribute, since an unreachable server is an
#'   expected condition. Attributes \code{"wosis_excluded"} and
#'   \code{"wosis_excluded_licences"} report what the licence filter withheld,
#'   and \code{"wosis_no_layers"} how many profiles had no layers at all.
#' @seealso \code{\link{wosis_profile_to_pedon}} to turn a row into a
#'   \code{\link{PedonRecord}} with its horizons.
#' @examples
#' \donttest{
#' # Requires network access to ISRIC.
#' p <- read_wosis_profiles_graphql(country = "Argentina", n_max = 5)
#' if (nrow(p)) p[, c("profile_code", "wrb_rsg", "n_layers", "licence_short")]
#' }
#' @export
read_wosis_profiles_graphql <- function(country = NULL,
                                         wrb_rsg = NULL,
                                         n_max   = 25L,
                                         licence_filter = c("permissive", "any"),
                                         timeout = 30,
                                         max_scan = 300L) {
  licence_filter <- match.arg(licence_filter)
  n_max <- max(1L, min(as.integer(n_max), 100L))
  empty <- data.frame(
    profile_id = character(0), profile_code = character(0),
    country = character(0), lat = numeric(0), lon = numeric(0),
    wrb_rsg = character(0), usda_order = character(0),
    dataset = character(0), licence = character(0),
    licence_short = character(0), n_layers = integer(0),
    depth_cm = numeric(0), stringsAsFactors = FALSE)

  flt <- list()
  if (!is.null(country) && nzchar(country))
    flt$countryName <- list(equalTo = as.character(country))
  if (!is.null(wrb_rsg) && nzchar(wrb_rsg))
    flt$wrbReferenceSoilGroup <- list(equalTo = as.character(wrb_rsg))

  q <- paste0(
    "query($f: WosisLatestProfileFilter, $n: Int, $o: Int) { ",
    "wosisLatestProfiles(filter: $f, first: $n, offset: $o, orderBy: [PROFILE_ID_ASC]) ",
    "{ profileId profileCode countryName latitude longitude wrbReferenceSoilGroup ",
    "usdaOrderName datasetCode layers(first: 1) { licence } } }")

  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)[1]
  num <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x)[1])
  to_rows <- function(prof) do.call(rbind, lapply(prof, function(p) {
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

  kept <- list(); excluded <- list()
  n_seen <- 0L; n_no_layers <- 0L; offset <- 0L; n_kept <- 0L
  repeat {
    vars <- list(n = 100L, o = offset)
    if (length(flt)) vars$f <- flt
    res <- .wosis_post(q, vars, timeout)
    if (!is.null(res$error)) {
      if (!n_kept) { attr(empty, "wosis_error") <- res$error; return(empty) }
      break  # keep what the earlier pages returned
    }
    page <- res$data$wosisLatestProfiles %||% list()
    offset <- offset + length(page)
    # A profile with no layers has nothing to classify, and its licence cannot
    # be read (it is carried per layer). It is left out of the listing and
    # counted on its own, rather than crashing the search -- as it did before
    # (`p$layers[[1]]` out of bounds) -- or being reported as NonCommercial.
    has_layers <- vapply(page, function(p) length(p$layers) > 0L, logical(1))
    n_no_layers <- n_no_layers + sum(!has_layers)
    page <- page[has_layers]
    if (length(page)) {
      rows <- to_rows(page)
      n_seen <- n_seen + nrow(rows)
      # A profile whose licence could not be read is dropped under "permissive":
      # unknown terms are not permissive terms.
      ok <- if (identical(licence_filter, "permissive"))
        .wosis_is_permissive(rows$licence) else rep(TRUE, nrow(rows))
      kept[[length(kept) + 1L]] <- rows[ok, , drop = FALSE]
      excluded[[length(excluded) + 1L]] <- rows[!ok, , drop = FALSE]
      n_kept <- n_kept + sum(ok)
    }
    if (length(res$data$wosisLatestProfiles %||% list()) < 100L) break
    if (n_kept >= n_max || offset >= max_scan) break
  }

  out <- if (length(kept)) do.call(rbind, kept) else empty[, 1:10]
  exc <- if (length(excluded)) do.call(rbind, excluded) else empty[, 1:10]
  if (nrow(out) > n_max) out <- out[seq_len(n_max), , drop = FALSE]

  # Number of layers and depth of each listed profile, from one light query on
  # the layers table (profileId and lowerDepth only).
  out$n_layers <- rep(NA_integer_, nrow(out))
  out$depth_cm <- rep(NA_real_, nrow(out))
  if (nrow(out)) {
    ids <- suppressWarnings(as.integer(out$profile_id))
    qd <- paste0(
      "query($f: WosisLatestLayerFilter, $n: Int, $o: Int) { ",
      "wosisLatestLayers(filter: $f, first: $n, offset: $o, orderBy: [LAYER_ID_ASC]) ",
      "{ profileId lowerDepth upperDepth } }")
    got <- list(); o <- 0L
    repeat {
      r <- .wosis_post(qd, list(f = list(profileId = list(`in` = ids)),
                                n = 100L, o = o), timeout)
      if (!is.null(r$error)) break
      lay <- r$data$wosisLatestLayers %||% list()
      got <- c(got, lay)
      if (length(lay) < 100L || o > 5000L) break
      o <- o + 100L
    }
    if (length(got)) {
      pid <- vapply(got, function(x) as.character(x$profileId %||% NA), character(1))
      dep <- vapply(got, function(x) max(num(x$lowerDepth), num(x$upperDepth), na.rm = TRUE),
                    numeric(1))
      dep[!is.finite(dep)] <- NA_real_
      cnt <- table(pid)
      out$n_layers <- as.integer(cnt[out$profile_id])
      out$depth_cm <- vapply(out$profile_id, function(id) {
        d <- dep[pid == id]; if (any(is.finite(d))) max(d, na.rm = TRUE) else NA_real_
      }, numeric(1), USE.NAMES = FALSE)
    }
  }

  if (identical(licence_filter, "permissive")) {
    # Report what was withheld and under which licence. A picker that simply
    # shows nothing for a country looks broken; one that says "12 profiles
    # withheld (CC BY-NC 4.0)" is telling the truth about why.
    attr(out, "wosis_excluded") <- nrow(exc)
    attr(out, "wosis_excluded_licences") <-
      sort(unique(stats::na.omit(exc$licence_short)))
  } else {
    attr(out, "wosis_excluded") <- 0L
    attr(out, "wosis_excluded_licences") <- character(0)
  }
  attr(out, "wosis_seen") <- n_seen
  attr(out, "wosis_no_layers") <- n_no_layers
  rownames(out) <- NULL
  out
}

#' How WoSIS layer properties map onto the soilKey horizon schema.
#'
#' Units come from ISRIC's own catalogue, read from the endpoint itself
#' (wosisLatestObservations: code, property, procedure, unit). Only properties
#' whose method matches what soilKey's column means are mapped:
#'
#'   * CFVO (volume %) is used for coarse fragments; CFGR is by mass and would
#'     need the bulk density to convert, so it is not.
#'   * ELCOSP (saturated paste) is used for EC; ELCO20/25/50 are 1:x extracts and
#'     are not equivalent.
#'   * WG (gravimetric, g/100g) is used for water retention, as soilKey defines
#'     it; WV is volumetric.
#'   * CECPH7 is used for CEC; CECPH8 is a different method.
#'   * BDFI33 is preferred for bulk density, with BDFIOD (oven-dry) as the
#'     fallback; the code actually used is recorded in the provenance notes.
#'
#' WoSIS serves no exchangeable bases, base saturation or Fe/Al oxides, so those
#' columns stay empty and the classifier reports them as missing data rather
#' than having them invented.
#' @noRd
.wosis_layer_map <- function() {
  data.frame(
    column = c("clay_pct", "silt_pct", "sand_pct", "coarse_fragments_pct",
               "ph_h2o", "ph_kcl", "ph_cacl2",
               "oc_pct", "n_total_pct", "cec_cmol", "ecec_cmol", "ec_dS_m",
               "caco3_pct", "bulk_density_g_cm3", "bulk_density_g_cm3",
               "water_content_33kpa", "water_content_1500kpa",
               "p_mehlich3_mg_kg", "phosphate_retention_pct"),
    field  = c("clay", "silt", "sand", "cfvo", "phaq", "phkc", "phca",
               "orgc", "nitkjd", "cecph7", "ecec", "elcosp",
               "tceq", "bdfi33l", "bdfiod", "wg0033", "wg1500",
               "phetm3", "phprtn"),
    code   = c("CLAY", "SILT", "SAND", "CFVO", "PHAQ", "PHKC", "PHCA",
               "ORGC", "NITKJD", "CECPH7", "ECEC", "ELCOSP",
               "TCEQ", "BDFI33", "BDFIOD", "WG0033", "WG1500",
               "PHETM3", "PHPRTN"),
    unit   = c("g/100g", "g/100g", "g/100g", "cm3/100cm3", "-", "-", "-",
               "g/kg", "g/kg", "cmol(c)/kg", "cmol(c)/kg", "dS/m",
               "g/kg", "kg/dm3", "kg/dm3", "g/100g", "g/100g",
               "mg/kg", "g/100g"),
    # multiply the WoSIS value by this to get soilKey's unit
    factor = c(1, 1, 1, 1, 1, 1, 1,
               0.1, 0.1, 1, 1, 1,
               0.1, 1, 1, 1, 1,
               1, 1),
    stringsAsFactors = FALSE)
}

#' One number from a WoSIS property list: the layer average, else the mean of
#' the reported values.
#' @noRd
.wosis_value <- function(v) {
  if (is.null(v) || !length(v)) return(NA_real_)
  avg <- suppressWarnings(as.numeric(unlist(lapply(v, function(e) e$valueAvg %||% NA))))
  avg <- avg[is.finite(avg)]
  if (length(avg)) return(mean(avg))
  raw <- suppressWarnings(as.numeric(unlist(lapply(v, function(e) e$value))))
  raw <- raw[is.finite(raw)]
  if (length(raw)) mean(raw) else NA_real_
}

#' The analytical method WoSIS records for a property, if any.
#' @noRd
.wosis_method <- function(v) {
  if (is.null(v) || !length(v)) return(NA_character_)
  m <- unlist(lapply(v, function(e) e$methodOptions %||% NA_character_))
  m <- m[!is.na(m) & nzchar(trimws(m))]
  if (length(m)) trimws(m[1]) else NA_character_
}

#' Read one WoSIS profile's layers live from ISRIC
#'
#' Fetches the layers of a single profile from ISRIC's WoSIS GraphQL endpoint
#' and returns them in the soilKey horizon schema. Nothing is written to disk.
#'
#' Units are converted from ISRIC's own catalogue (organic carbon, total
#' nitrogen and carbonate equivalent arrive in g/kg and become \%), and only
#' properties whose method matches the soilKey column are mapped: volumetric
#' coarse fragments, saturated-paste EC, gravimetric water retention and CEC at
#' pH 7. WoSIS serves no exchangeable bases, base saturation or Fe/Al oxides,
#' so those columns stay empty and the classifier reports them as missing.
#'
#' @section Load on ISRIC:
#' The endpoint is a free public service whose database cancels any statement
#' that runs longer than about 30 seconds, and the cost of a query grows with the
#' number of layers times the number of properties. The layers are therefore
#' counted first with a cheap query, then read in pages of \code{page_size} in a
#' stable order. Profiles recorded as many fine depth increments (some have 90
#' one-centimetre layers) exceed \code{max_layers} and are not read unless you
#' raise it deliberately.
#'
#' @param profile_id A WoSIS \code{profile_id}, as returned by
#'   \code{\link{read_wosis_profiles_graphql}}.
#' @param timeout Seconds to wait for each request (default 30).
#' @param max_layers Largest profile, in layers, that is read (default 40).
#' @param page_size Layers per request (default 10).
#' @param progress Optional \code{function(done, total)} called after each page,
#'   for a progress bar.
#' @return A data.frame of horizons in the soilKey schema, top to bottom, with
#'   attributes \code{"provenance"} (one row per value: horizon, column, WoSIS
#'   code, unit and method), \code{"licence"} (the most restrictive licence
#'   among the layers), \code{"dataset"}, \code{"n_layers"} and
#'   \code{"depth_shift_cm"} (non-zero when depths were shifted so an organic
#'   surface layer starts at 0 cm). Zero rows, with a \code{"wosis_error"}
#'   attribute, when ISRIC cannot be reached, the profile has no layers, or it
#'   has more than \code{max_layers}.
#' @seealso \code{\link{wosis_profile_to_pedon}}, \code{\link{wosis_citation}}.
#' @examples
#' \donttest{
#' # Requires network access to ISRIC.
#' p <- read_wosis_profiles_graphql(country = "Argentina", n_max = 1)
#' if (nrow(p)) {
#'   h <- read_wosis_layers_graphql(p$profile_id[1])
#'   h[, c("top_cm", "bottom_cm", "designation", "clay_pct", "oc_pct")]
#' }
#' }
#' @export
read_wosis_layers_graphql <- function(profile_id, timeout = 30, max_layers = 40L,
                                      page_size = 10L, progress = NULL) {
  map <- .wosis_layer_map()
  empty <- data.frame(top_cm = numeric(0), bottom_cm = numeric(0),
                      designation = character(0), stringsAsFactors = FALSE)
  fail <- function(msg, n = NA_integer_) {
    attr(empty, "wosis_error") <- msg
    attr(empty, "n_layers") <- n
    empty
  }
  pid <- suppressWarnings(as.integer(profile_id)[1])
  if (is.na(pid)) return(fail("not a WoSIS profile id"))
  flt <- list(profileId = list(equalTo = pid))

  # 1. Count the layers with a query that asks for depths only (cheap).
  q_count <- paste0(
    "query($f: WosisLatestLayerFilter, $n: Int, $o: Int) { ",
    "wosisLatestLayers(filter: $f, first: $n, offset: $o, orderBy: [LAYER_ID_ASC]) ",
    "{ layerId } }")
  n_layers <- 0L
  repeat {
    r <- .wosis_post(q_count, list(f = flt, n = 100L, o = n_layers), timeout)
    if (!is.null(r$error)) return(fail(r$error))
    k <- length(r$data$wosisLatestLayers)
    n_layers <- n_layers + k
    if (k < 100L) break
  }
  if (n_layers == 0L) return(fail("this WoSIS profile has no layer data", 0L))
  if (n_layers > max_layers)
    return(fail(sprintf(paste0(
      "this profile has %d layers (fine depth increments rather than ",
      "horizons); reading them all would take over a minute and put a heavy ",
      "load on ISRIC's free service, so it is not loaded. Choose another ",
      "profile, or call read_wosis_layers_graphql(%d, max_layers = %d) in R ",
      "if you need it."), n_layers, pid, n_layers), n_layers))

  # 2. Read the properties a page at a time, in a stable order.
  fields <- unique(map$field)
  sel <- paste(sprintf("%sValues(first: 3) { value valueAvg methodOptions }", fields),
               collapse = " ")
  q <- paste0(
    "query($f: WosisLatestLayerFilter, $n: Int, $o: Int) { ",
    "wosisLatestLayers(filter: $f, first: $n, offset: $o, orderBy: [LAYER_ID_ASC]) { ",
    "layerId layerNumber upperDepth lowerDepth layerName organicSurface ",
    "licence datasetId ", sel, " } }")
  layers <- list()
  repeat {
    r <- .wosis_post(q, list(f = flt, n = as.integer(page_size),
                             o = length(layers)), timeout)
    if (!is.null(r$error)) return(fail(r$error, n_layers))
    page <- r$data$wosisLatestLayers
    layers <- c(layers, page)
    if (is.function(progress)) try(progress(length(layers), n_layers), silent = TRUE)
    if (length(page) < page_size || length(layers) >= n_layers) break
  }
  if (!length(layers)) return(fail("this WoSIS profile has no layer data", 0L))

  num <- function(x) if (is.null(x)) NA_real_ else suppressWarnings(as.numeric(x)[1])
  up  <- vapply(layers, function(l) num(l$upperDepth), numeric(1))
  lo  <- vapply(layers, function(l) num(l$lowerDepth), numeric(1))
  # Organic surface layers may be recorded with their depths inverted; take the
  # interval either way round.
  top <- pmin(up, lo); bot <- pmax(up, lo)
  ord <- order(top, bot, na.last = TRUE)
  layers <- layers[ord]; top <- top[ord]; bot <- bot[ord]
  # Depths above the mineral surface come back negative: shift the whole profile
  # so it starts at 0 cm, preserving every thickness.
  shift <- if (any(is.finite(top)) && min(top, na.rm = TRUE) < 0)
    -min(top, na.rm = TRUE) else 0
  top <- top + shift; bot <- bot + shift

  out <- data.frame(
    top_cm      = top,
    bottom_cm   = bot,
    designation = vapply(layers, function(l) {
      n <- l$layerName; if (is.null(n) || !nzchar(trimws(n))) NA_character_ else trimws(n)
    }, character(1)),
    stringsAsFactors = FALSE)

  prov <- list()
  for (i in seq_len(nrow(map))) {
    col <- map$column[i]
    vals <- vapply(layers, function(l) .wosis_value(l[[paste0(map$field[i], "Values")]]),
                   numeric(1)) * map$factor[i]
    meth <- vapply(layers, function(l) .wosis_method(l[[paste0(map$field[i], "Values")]]),
                   character(1))
    if (is.null(out[[col]])) out[[col]] <- NA_real_
    # A later entry for the same column is a fallback: fill only what is empty.
    fill <- is.na(out[[col]]) & is.finite(vals)
    if (!any(fill)) next
    out[[col]][fill] <- vals[fill]
    note <- sprintf("WoSIS %s (%s)%s", map$code[i], map$unit[i],
                    ifelse(is.na(meth[fill]), "", paste0("; method: ", meth[fill])))
    prov[[length(prov) + 1L]] <- data.frame(
      horizon_idx = which(fill), attribute = col, source = "measured",
      confidence = 1, notes = note, stringsAsFactors = FALSE)
  }
  prov <- if (length(prov)) do.call(rbind, prov) else
    data.frame(horizon_idx = integer(0), attribute = character(0),
               source = character(0), confidence = numeric(0),
               notes = character(0), stringsAsFactors = FALSE)

  lic <- vapply(layers, function(l) as.character(l$licence %||% NA_character_),
                character(1))
  ds  <- vapply(layers, function(l) as.character(l$datasetId %||% NA_character_),
                character(1))
  rownames(out) <- NULL
  attr(out, "provenance")     <- prov
  attr(out, "licence")        <- .wosis_most_restrictive(lic)
  attr(out, "dataset")        <- unique(stats::na.omit(ds))[1] %||% NA_character_
  attr(out, "n_layers")       <- n_layers
  attr(out, "depth_shift_cm") <- shift
  out
}

#' Turn one WoSIS profile into a PedonRecord, with its horizons
#'
#' Fetches the profile's layers live from ISRIC
#' (\code{\link{read_wosis_layers_graphql}}) and builds a
#' \code{\link{PedonRecord}}. Every value carries a provenance note naming its
#' WoSIS code, unit and method. The site metadata carries the licence, the source
#' dataset, the date the data were read and the citation ISRIC asks for
#' (\code{\link{wosis_citation}}), so attribution survives into anything the
#' user later exports. CC BY requires it, and an extract that drops it turns a
#' licensed use into an unattributed one.
#'
#' When the layers carry a more restrictive licence than the profile listing
#' reported, the more restrictive one is kept.
#'
#' @param row One row of \code{\link{read_wosis_profiles_graphql}}.
#' @param fetch_layers If \code{TRUE} (default) the layers are read from ISRIC;
#'   \code{FALSE} builds the site metadata only, with no network call.
#' @param timeout Seconds to wait for ISRIC (default 30).
#' @param progress Optional \code{function(done, total)} passed to
#'   \code{\link{read_wosis_layers_graphql}}.
#' @return A \code{\link{PedonRecord}}. If the layers could not be read, it has
#'   no horizons and \code{site$wosis_error} says why.
#' @export
wosis_profile_to_pedon <- function(row, fetch_layers = TRUE, timeout = 30,
                                   progress = NULL) {
  stopifnot(is.data.frame(row), nrow(row) == 1L)
  accessed <- Sys.Date()
  licence  <- row$licence
  site <- list(
    id               = row$profile_code,
    lat              = row$lat,
    lon              = row$lon,
    country          = row$country,
    wosis_profile_id = row$profile_id,
    wosis_rsg        = row$wrb_rsg,
    wosis_usda_order = row$usda_order,
    source           = "ISRIC WoSIS",
    dataset          = row$dataset,
    accessed         = format(accessed, "%Y-%m-%d"))

  horizons <- NULL
  prov     <- NULL
  if (isTRUE(fetch_layers)) {
    L <- read_wosis_layers_graphql(row$profile_id, timeout = timeout,
                                   progress = progress)
    if (nrow(L)) {
      licence  <- .wosis_most_restrictive(c(row$licence, attr(L, "licence")))
      horizons <- ensure_horizon_schema(data.table::as.data.table(L))
      p        <- attr(L, "provenance")
      prov     <- if (nrow(p)) data.table::as.data.table(p) else NULL
      if (isTRUE(attr(L, "depth_shift_cm") > 0))
        site$depth_note <- sprintf(
          "Depths shifted by %g cm so the organic surface layer starts at 0 cm.",
          attr(L, "depth_shift_cm"))
    } else {
      site$wosis_error <- attr(L, "wosis_error") %||% "no layer data"
    }
  }
  site$licence       <- licence
  site$licence_short <- .wosis_licence_short(licence)
  site$citation      <- wosis_citation(accessed)
  site$attribution   <- paste0(
    "Soil profile ", row$profile_code, " (dataset ", row$dataset,
    "), standardised by ISRIC - World Soil Information and obtained through ",
    "WoSIS-latest on ", format(accessed, "%Y-%m-%d"), ". Licence: ", licence, ".")
  PedonRecord$new(site = site, horizons = horizons, provenance = prov)
}
