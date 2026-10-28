# =============================================================================
# v0.9.295 -- many profiles from one horizon table.
#
# classify_all() classifies one PedonRecord. A survey or a database export is
# one table with a row per horizon and a column that says which profile each
# horizon belongs to, often with its own column names and units. The loaders
# (BDsolos, FEBR, WoSIS, NatSoil, ...) cover particular databases;
# classify_profiles() covers any such table: it renames and converts columns
# with a mapping table, checks the names once for the whole table, builds one
# pedon per profile and returns one row per profile. Asked for by Jelle
# Janssen (ISRIC), who classifies ISIS profiles this way.
# =============================================================================

#' Classify many profiles from one horizon table
#'
#' Builds one \code{\link{PedonRecord}} per profile from a table with one row
#' per horizon, and classifies each with \code{\link{classify_all}}. The
#' column named by \code{id} says which profile a horizon belongs to; the
#' other columns use soilKey's names (\code{\link{horizon_attribute_spec}}),
#' or are renamed and converted by \code{map}. Site properties
#' (\code{\link{site_attribute_spec}}), such as \code{soil_moisture_regime},
#' come from \code{sites}.
#'
#' The names are checked once for the whole table: a horizon column or site
#' property that no key reads is named in one warning (see
#' \code{\link{unrecognised_attributes}}), not once per profile. Within a
#' profile the horizons are put in order of \code{top_cm}. A profile that
#' cannot be built or classified gets its reason in the \code{error} column,
#' and the others are classified as usual.
#'
#' @param horizons A \code{data.frame} with one row per horizon and the
#'   column named by \code{id}.
#' @param sites Optional site properties: a \code{data.frame} with one row per
#'   profile and the same \code{id} column, or a list of lists named by
#'   profile id. A missing value (\code{NA}) is read as not recorded.
#'   Profiles without an entry get only their id.
#' @param id Name of the column that identifies the profile, in
#'   \code{horizons} and in a \code{sites} table (default
#'   \code{"profile_id"}).
#' @param map Optional \code{data.frame} with the columns \code{from} and
#'   \code{to}, and optionally \code{factor}: the column \code{from} of
#'   \code{horizons} becomes the column \code{to}, multiplied by
#'   \code{factor} (default 1) to convert its unit. For example, organic
#'   carbon in g/kg: \code{from = "ORGC", to = "oc_pct", factor = 0.1}.
#' @param systems,on_missing Forwarded to \code{\link{classify_all}}; the
#'   default \code{on_missing = "silent"} keeps a batch quiet, and the
#'   \code{missing} column reports what the keys did not find.
#' @param ... Other arguments forwarded to \code{\link{classify_all}}, such as
#'   \code{include_family}.
#' @return A \code{data.frame} with one row per profile, in the order of
#'   their first horizon in \code{horizons}: the \code{id} column, one column
#'   per system with the name of the class (as in the \code{summary} of
#'   \code{\link{classify_all}}), \code{missing} (the attributes the keys
#'   asked for and did not find, comma-separated) and \code{error} (\code{NA},
#'   or why the profile could not be classified). The attributes
#'   \code{"results"} and \code{"pedons"} hold the full
#'   \code{\link{classify_all}} results and the pedons, in lists named by
#'   profile id.
#' @seealso \code{\link{classify_all}} for one pedon,
#'   \code{\link{classify_csv}} for one profile in a spreadsheet,
#'   \code{\link{unrecognised_attributes}} to check the names first.
#' @examples
#' h <- rbind(
#'   cbind(profile_id = "p1", as.data.frame(make_ferralsol_canonical()$horizons)),
#'   cbind(profile_id = "p2", as.data.frame(make_phaeozem_canonical()$horizons)))
#' s <- data.frame(profile_id = c("p1", "p2"),
#'                 soil_moisture_regime = c("udic", "ustic"))
#' res <- classify_profiles(h, sites = s, systems = "usda")
#' res[, c("profile_id", "usda", "error")]
#'
#' \donttest{
#' # Columns named and measured differently: rename and convert with `map`.
#' h2 <- h
#' names(h2)[names(h2) == "oc_pct"] <- "ORGC"
#' h2$ORGC <- h2$ORGC * 10                      # g/kg
#' m <- data.frame(from = "ORGC", to = "oc_pct", factor = 0.1)
#' classify_profiles(h2, sites = s, map = m)[, c("profile_id", "wrb", "sibcs", "usda")]
#' }
#' @export
classify_profiles <- function(horizons, sites = NULL, id = "profile_id",
                              map = NULL, systems = "all",
                              on_missing = "silent", ...) {
  if (!is.data.frame(horizons)) {
    rlang::abort("classify_profiles(): 'horizons' must be a data.frame.")
  }
  if (!is.character(id) || length(id) != 1L || !id %in% names(horizons)) {
    rlang::abort(sprintf(
      "classify_profiles(): 'horizons' has no column '%s' (set 'id' to the column that identifies the profile).",
      paste(id, collapse = ", ")))
  }
  h <- as.data.frame(horizons, stringsAsFactors = FALSE)
  if (!identical(systems, "all")) .system_resolve(systems, several.ok = TRUE)
  h <- .sk_apply_column_map(h, map, id)

  ids <- as.character(h[[id]])
  if (anyNA(ids) || any(!nzchar(ids))) {
    n <- sum(is.na(ids) | !nzchar(ids))
    warning(sprintf("classify_profiles(): %d horizon(s) without a profile id are left out.", n),
            call. = FALSE)
    h   <- h[!is.na(ids) & nzchar(ids), , drop = FALSE]
    ids <- as.character(h[[id]])
  }
  site_list <- .sk_profile_sites(sites, id)

  # One check of the names for the whole table.
  site_names <- unique(unlist(lapply(site_list, names)))
  .sk_warn_unrecognised(
    ensure_horizon_schema(h[, setdiff(names(h), id), drop = FALSE]),
    stats::setNames(vector("list", length(site_names)), site_names))

  order_ids <- unique(ids)
  results <- stats::setNames(vector("list", length(order_ids)), order_ids)
  pedons  <- results
  rows <- lapply(order_ids, function(pid) {
    hz <- h[ids == pid, setdiff(names(h), id), drop = FALSE]
    if ("top_cm" %in% names(hz)) {
      hz <- hz[order(suppressWarnings(as.numeric(hz$top_cm)), na.last = TRUE), ,
               drop = FALSE]
    }
    site <- c(list(id = pid), site_list[[pid]])
    out <- tryCatch({
      p <- PedonRecord$new(site = site, horizons = hz)
      r <- classify_all(p, systems = systems, on_missing = on_missing, ...)
      list(pedon = p, result = r, error = NA_character_)
    }, error = function(e) list(pedon = NULL, result = NULL,
                                error = conditionMessage(e)))
    pedons[pid]  <<- list(out$pedon)
    results[pid] <<- list(out$result)
    out
  })

  sys_cols <- unique(unlist(lapply(rows, function(x)
    if (!is.null(x$result)) names(x$result$summary))))
  table <- do.call(rbind, lapply(seq_along(order_ids), function(i) {
    x  <- rows[[i]]
    df <- data.frame(id = order_ids[i], stringsAsFactors = FALSE)
    names(df) <- id
    for (s in sys_cols) {
      v <- if (!is.null(x$result) && s %in% names(x$result$summary))
             as.character(x$result$summary[[s]][1L]) else NA_character_
      df[[s]] <- v
    }
    miss <- if (is.null(x$result)) character() else
      unique(unlist(lapply(x$result[setdiff(names(x$result), "summary")],
        function(r) if (inherits(r, "ClassificationResult"))
                      as.character(r$missing_data) else NULL)))
    df$missing <- paste(sort(unique(miss[!is.na(miss) & nzchar(miss)])),
                        collapse = ", ")
    df$error <- x$error
    df
  }))
  rownames(table) <- NULL
  attr(table, "results") <- results
  attr(table, "pedons")  <- pedons
  table
}

# Rename and convert the columns of `h` with a map of from / to / factor.
.sk_apply_column_map <- function(h, map, id) {
  if (is.null(map)) return(h)
  if (!is.data.frame(map) || !all(c("from", "to") %in% names(map))) {
    rlang::abort("classify_profiles(): 'map' must be a data.frame with the columns 'from' and 'to' (and optionally 'factor').")
  }
  from   <- as.character(map$from)
  to     <- as.character(map$to)
  factor <- if ("factor" %in% names(map)) suppressWarnings(as.numeric(map$factor))
            else rep(1, length(from))
  factor[is.na(factor)] <- 1
  if (anyDuplicated(to)) {
    rlang::abort(sprintf("classify_profiles(): 'map' gives more than one column for: %s.",
                         paste(unique(to[duplicated(to)]), collapse = ", ")))
  }
  if (id %in% c(from, to)) {
    rlang::abort(sprintf("classify_profiles(): 'map' cannot rename the id column '%s'.", id))
  }
  absent <- setdiff(from, names(h))
  if (length(absent)) {
    warning(sprintf("classify_profiles(): the map's columns not in 'horizons' are ignored: %s.",
                    paste(absent, collapse = ", ")), call. = FALSE)
  }
  clash <- intersect(to[from %in% names(h) & from != to], names(h))
  if (length(clash)) {
    rlang::abort(sprintf(
      "classify_profiles(): 'horizons' already has the column(s) the map would write: %s.",
      paste(clash, collapse = ", ")))
  }
  for (i in which(from %in% names(h))) {
    v <- h[[from[i]]]
    if (factor[i] != 1) {
      num <- suppressWarnings(as.numeric(v))
      if (any(is.na(num) & !is.na(v))) {
        rlang::abort(sprintf(
          "classify_profiles(): column '%s' has values that are not numbers, so it cannot be multiplied by %g.",
          from[i], factor[i]))
      }
      v <- num * factor[i]
    }
    h[[from[i]]] <- NULL
    h[[to[i]]]   <- v
  }
  h
}

# The site properties of each profile, as a list named by profile id. NA is
# read as not recorded and dropped.
.sk_profile_sites <- function(sites, id) {
  if (is.null(sites)) return(list())
  drop_na <- function(s) s[!vapply(s, function(v) is.null(v) || all(is.na(v)),
                                   logical(1))]
  if (is.data.frame(sites)) {
    if (!id %in% names(sites)) {
      rlang::abort(sprintf("classify_profiles(): 'sites' has no column '%s'.", id))
    }
    sids <- as.character(sites[[id]])
    if (anyDuplicated(sids[!is.na(sids)])) {
      rlang::abort(sprintf("classify_profiles(): 'sites' has more than one row for: %s.",
                           paste(unique(sids[duplicated(sids)]), collapse = ", ")))
    }
    out <- lapply(seq_len(nrow(sites)), function(i) {
      s <- as.list(sites[i, setdiff(names(sites), id), drop = FALSE])
      drop_na(lapply(s, function(v) if (is.factor(v)) as.character(v) else v))
    })
    return(stats::setNames(out, sids))
  }
  if (is.list(sites) && !is.null(names(sites)) && all(nzchar(names(sites)))) {
    return(lapply(sites, function(s) drop_na(as.list(s))))
  }
  rlang::abort("classify_profiles(): 'sites' must be a data.frame with the id column, or a list of lists named by profile id.")
}
