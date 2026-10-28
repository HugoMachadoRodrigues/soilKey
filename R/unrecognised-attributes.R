# =============================================================================
# v0.9.294 -- a warning for the attributes no key reads.
#
# A horizon column that is not one of horizon_column_spec() (after the
# case-, separator- and alias-tolerant renaming of ensure_horizon_schema()),
# or a site property that is not one of site_attribute_spec(), is kept in the
# PedonRecord but read by no key. Until v0.9.293 that happened without a word,
# so a misspelt column ("organic_carbon_gkg") or a site property put in the
# wrong place ("soil_moisture_regime" as a horizon column) went unnoticed.
#
# PedonRecord$new() now warns when it is called from outside the package, and
# read_pedon_csv() warns for the file it reads. The loaders (BDsolos, FEBR,
# WoSIS, NatSoil, KSSL, ...) and every other construction inside soilKey stay
# quiet: they keep extra columns on purpose. unrecognised_attributes() returns
# the same list as a table, and options(soilKey.warn_unrecognised = FALSE)
# turns the warning off.
# =============================================================================

.SITE_ATTRIBUTE_FIELDS <- c("name", "section", "type", "unit", "description")

# The entries of inst/schemas/site-attributes.yaml, checked: one entry per
# name, each with the required fields and a known type.
.site_attribute_metadata <- function() {
  path <- system.file("schemas", "site-attributes.yaml", package = "soilKey")
  meta <- yaml::read_yaml(path)$attributes
  nms  <- vapply(meta, function(e) as.character(e$name %||% ""), character(1))
  if (anyDuplicated(nms) || any(!nzchar(nms))) {
    rlang::abort(sprintf("site-attributes.yaml: empty or repeated names: %s",
                         paste(unique(nms[duplicated(nms) | !nzchar(nms)]),
                               collapse = ", ")))
  }
  for (e in meta) {
    lacking <- setdiff(.SITE_ATTRIBUTE_FIELDS, names(e))
    if (length(lacking) > 0L) {
      rlang::abort(sprintf("site-attributes.yaml, '%s' lacks: %s",
                           e$name, paste(lacking, collapse = ", ")))
    }
    if (!e$type %in% c("numeric", "character", "logical", "list")) {
      rlang::abort(sprintf("site-attributes.yaml, '%s': unknown type '%s'",
                           e$name, e$type))
    }
  }
  stats::setNames(meta, nms)
}


#' Metadata of the site properties
#'
#' One row per site property that soilKey recognises (the named elements of
#' \code{PedonRecord$site}), with the section it belongs to, its type, unit
#' and description, the values a categorical property takes where the keys
#' compare it with a fixed set, and the classification keys that read it. The
#' properties of the last section, \emph{reference classes and provenance},
#' are stored by the loaders and reports and read by no key. The metadata
#' come from \code{inst/schemas/site-attributes.yaml}; the attribute reference
#' \code{inst/ATTRIBUTES.md} lists them after the horizon attributes.
#'
#' @return A \code{data.frame} with the columns \code{name}, \code{type},
#'   \code{section}, \code{unit}, \code{description}, and \code{allowed} and
#'   \code{systems} (comma-separated, empty when none).
#' @seealso \code{\link{horizon_attribute_spec}},
#'   \code{\link{unrecognised_attributes}}.
#' @examples
#' s <- site_attribute_spec()
#' s[s$name %in% c("drainage_class", "soil_moisture_regime"),
#'   c("name", "type", "description")]
#' @export
site_attribute_spec <- function() {
  meta <- .site_attribute_metadata()
  field <- function(f) vapply(meta, function(e)
    as.character(e[[f]] %||% ""), character(1), USE.NAMES = FALSE)
  joined <- function(f) vapply(meta, function(e)
    paste(as.character(unlist(e[[f]])), collapse = ", "), character(1),
    USE.NAMES = FALSE)
  data.frame(
    name        = field("name"),
    type        = field("type"),
    section     = field("section"),
    unit        = field("unit"),
    description = field("description"),
    allowed     = joined("allowed"),
    systems     = joined("systems"),
    stringsAsFactors = FALSE
  )
}


# Is token run `b` (a match key) a contiguous run of the tokens of key `a`?
.sk_key_contains <- function(a, b) {
  ta <- strsplit(a, "_", fixed = TRUE)[[1L]]
  tb <- strsplit(b, "_", fixed = TRUE)[[1L]]
  if (!length(tb) || length(tb) > length(ta)) return(FALSE)
  for (s in 0:(length(ta) - length(tb))) {
    if (identical(ta[s + seq_along(tb)], tb)) return(TRUE)
  }
  FALSE
}

# The recognised names closest to `name`: `cand` maps match keys (names) to
# the names to show (values). A candidate is close when its edit distance to
# the key is at most a quarter of the key's length (at least 1), or when one
# of the two, four characters or longer, is a whole run of the other's words
# ("organic_carbon" in "organic_carbon_gkg"). At most two, the closest first.
.sk_suggest_names <- function(name, cand) {
  key <- .norm_name_key(name)
  if (!nzchar(key) || !length(cand)) return(character())
  ck <- names(cand)
  d  <- as.vector(utils::adist(key, ck))
  inside <- vapply(ck, function(k)
    (nchar(k) >= 4L && .sk_key_contains(key, k)) ||
      (nchar(key) >= 4L && .sk_key_contains(k, key)),
    logical(1), USE.NAMES = FALSE)
  d[inside] <- pmin(d[inside], 1)
  ok <- d <= max(1L, nchar(key) %/% 4L)
  if (!any(ok)) return(character())
  ord <- order(d[ok], nchar(ck[ok]))
  utils::head(unique(unname(cand[ok][ord])), 2L)
}

# Match keys -> names: the canonical horizon columns and their aliases.
.sk_horizon_candidates <- function() {
  canon <- names(horizon_column_spec())
  al    <- .horizon_name_aliases()
  c(stats::setNames(canon, .norm_name_key(canon)), al)
}

.sk_site_candidates <- function() {
  s <- site_attribute_spec()$name
  stats::setNames(s, .norm_name_key(s))
}


#' Attributes that soilKey does not recognise
#'
#' Lists the horizon columns and site properties that no classification key
#' reads: a horizon column that is neither a canonical name of
#' \code{\link{horizon_column_spec}} nor one that
#' \code{\link{ensure_horizon_schema}} renames onto it (a difference of
#' capitalisation or separators, or a common alias such as \code{SOC}), and a
#' site property that is not in \code{\link{site_attribute_spec}}. Such
#' attributes are kept in a \code{\link{PedonRecord}} but have no effect on a
#' classification. \code{PedonRecord$new()} warns about them when it is called
#' from outside the package, and so does \code{\link{read_pedon_csv}};
#' \code{options(soilKey.warn_unrecognised = FALSE)} turns the warning off.
#'
#' @param x A \code{\link{PedonRecord}}, or a \code{data.frame} of horizons.
#' @param site A list of site properties, when \code{x} is a
#'   \code{data.frame}; ignored for a \code{PedonRecord}, whose own site is
#'   read.
#' @return A \code{data.frame} with one row per attribute that is not
#'   recognised and the columns \code{where} (\code{"horizons"} or
#'   \code{"site"}), \code{name}, \code{suggestion} (the closest recognised
#'   names, comma-separated, empty when none) and \code{note}: empty, or
#'   \code{"site property"} for a horizon column that is a site property,
#'   \code{"horizon attribute"} for a site property that is a horizon column,
#'   or \code{"already given"} when the column it would be renamed to is also
#'   present. No rows when everything is recognised.
#' @seealso \code{\link{horizon_attribute_spec}},
#'   \code{\link{site_attribute_spec}}, \code{inst/ATTRIBUTES.md}.
#' @examples
#' h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
#'                 clay = c(30, 45), organic_carbon_gkg = c(11, 4),
#'                 soil_moisture_regime = "ustic")
#' unrecognised_attributes(h, site = list(id = "p1", drainge_class = "well"))
#' @export
unrecognised_attributes <- function(x, site = NULL) {
  if (inherits(x, "PedonRecord")) {
    h    <- x$horizons
    site <- x$site
  } else {
    h <- if (is.null(x)) NULL else ensure_horizon_schema(x)
  }
  .sk_unrecognised_table(h, site)
}

# The rows of unrecognised_attributes() for a canonical horizon table `h`
# (already passed through ensure_horizon_schema()) and a site list.
.sk_unrecognised_table <- function(h, site) {
  out <- data.frame(where = character(), name = character(),
                    suggestion = character(), note = character(),
                    stringsAsFactors = FALSE)
  spec     <- names(horizon_column_spec())
  site_nms <- site_attribute_spec()$name
  h_cand   <- .sk_horizon_candidates()
  s_cand   <- .sk_site_candidates()
  hz_extra <- if (is.null(h)) character() else setdiff(names(h), spec)
  hz_extra <- hz_extra[nzchar(hz_extra)]
  for (nm in hz_extra) {
    key  <- .norm_name_key(nm)
    note <- ""
    sug  <- character()
    if (key %in% names(s_cand)) {
      note <- "site property"
      sug  <- unname(s_cand[[key]])
    } else if (key %in% names(h_cand)) {
      note <- "already given"
      sug  <- unname(h_cand[[key]])
    } else {
      sug <- .sk_suggest_names(nm, h_cand)
      if (!length(sug)) {
        sug <- .sk_suggest_names(nm, s_cand)
        if (length(sug)) note <- "site property"
      }
    }
    out[nrow(out) + 1L, ] <- list("horizons", nm, paste(sug, collapse = ", "),
                                  note)
  }
  s_names <- if (is.list(site)) names(site) else character()
  s_names <- s_names[!is.na(s_names) & nzchar(s_names %||% "")]
  for (nm in setdiff(s_names, site_nms)) {
    key  <- .norm_name_key(nm)
    note <- ""
    if (key %in% names(h_cand)) {
      note <- "horizon attribute"
      sug  <- unname(h_cand[[key]])
    } else {
      sug <- .sk_suggest_names(nm, s_cand)
      if (!length(sug)) {
        sug <- .sk_suggest_names(nm, h_cand)
        if (length(sug)) note <- "horizon attribute"
      }
    }
    out[nrow(out) + 1L, ] <- list("site", nm, paste(sug, collapse = ", "), note)
  }
  out
}

# "name (did you mean x?)" and the like, for the warning.
.sk_unrecognised_item <- function(name, suggestion, note) {
  sug <- strsplit(suggestion, ", ", fixed = TRUE)[[1L]]
  if (!length(sug)) return(name)
  what <- paste(sug, collapse = " or ")
  sprintf("%s (%s)", name, switch(note,
    "site property"     = sprintf("a site property: site$%s", what),
    "horizon attribute" = sprintf("a horizon column: %s", what),
    "already given"     = sprintf("%s is also given", what),
    sprintf("did you mean %s?", what)))
}

# The text of the warning for the rows of .sk_unrecognised_table().
.sk_unrecognised_message <- function(tab, id = NULL) {
  part <- function(where, label) {
    r <- tab[tab$where == where, , drop = FALSE]
    if (!nrow(r)) return(character())
    sprintf("  %s: %s.", label, paste(mapply(.sk_unrecognised_item, r$name,
                                             r$suggestion, r$note),
                                      collapse = ", "))
  }
  who <- if (length(id) == 1L && !is.na(id) && nzchar(as.character(id)))
           sprintf("Pedon '%s': some", as.character(id)) else "Some"
  paste(c(
    sprintf("%s attributes are not recognised, so no key reads them.", who),
    part("horizons", "Horizon columns"),
    part("site", "Site properties"),
    paste("See horizon_attribute_spec() and site_attribute_spec() for the",
          "names soilKey reads; options(soilKey.warn_unrecognised = FALSE)",
          "turns this warning off.")),
    collapse = "\n")
}

# Warn (class "soilKey_unrecognised_attributes") when the horizons or site of
# a pedon hold attributes that are not recognised. Returns the table,
# invisibly.
.sk_warn_unrecognised <- function(h, site) {
  if (!isTRUE(getOption("soilKey.warn_unrecognised", TRUE))) {
    return(invisible(NULL))
  }
  tab <- .sk_unrecognised_table(h, site)
  if (nrow(tab) > 0L) {
    id <- if (is.list(site)) site$id else NULL
    rlang::warn(.sk_unrecognised_message(tab, id),
                class = "soilKey_unrecognised_attributes")
  }
  invisible(tab)
}

# TRUE when `env` (the frame that called PedonRecord$new()) belongs to
# soilKey's own code: a loader, a fixture or a key. The top environment is
# compared by name: code can sit under the namespace or under the attached
# package environment (as testthat's test environment does), both "soilKey".
.sk_is_package_env <- function(env) {
  identical(environmentName(topenv(env)),
            environmentName(topenv(environment(.sk_is_package_env))))
}
