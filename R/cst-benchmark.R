# =============================================================================
# soilKey -- Chinese Soil Taxonomy (2001): reading a CST name written by hand.
#
# v0.9.378. Reference profiles often carry a Chinese Soil Taxonomy name as a
# free-text label (ISRIC's ISIS gives "Haplic latosol; Hapli-Udic Ferrosol",
# the 1991 first-proposal name beside the CST one). .cst_match_label() finds
# the CST part of such a label and the taxon of the key it names, at whatever
# level the label stops (order, suborder, group or subgroup). It serves the
# offline ISIS benchmark (inst/benchmarks/cst2001_isis.R) and never changes a
# classification.
#
# How a name is matched (soilKey's reading, readings.yaml):
#   1. the label is split at ";" and "|"; each part is tried, and the deepest
#      match wins (a 1991 name matches nothing);
#   2. a name and the table are compared on a key: lower case, letters only
#      (hyphens and spaces dropped, so "Hi-weatheri" is "Hiweatheri"), a final
#      "s" dropped (singular and plural alike);
#   3. the table holds every name of the key (14 orders, 40 suborders, 140
#      groups, 588 subgroups) with its aliases (the other printed forms);
#   4. failing an exact key, a name one letter off a single taxon is matched
#      as a typo ("Acidi-Undic", "Isohumisol"), flagged "typo"; two taxa as
#      close leave it unmatched.
# =============================================================================

# The match key of a name: lower case, letters only, one final "s" dropped.
.cst_label_key <- function(x) {
  k <- gsub("[^a-z]", "", tolower(as.character(x)))
  sub("s$", "", k)
}

# The table of the names of the key: one row per name or alias.
.cst_label_table <- function(rules = NULL) {
  if (is.null(rules) && !is.null(.cst_cache$label_table)) return(.cst_cache$label_table)
  r <- rules %||% load_rules("cst2001")
  rows <- list()
  add <- function(level, e) {
    nm <- as.character(e$name)
    al <- as.character(unlist(e$aliases %||% character(0)))
    rows[[length(rows) + 1L]] <<- data.frame(
      level = level, code = as.character(e$code), name = nm,
      form = c(nm, al), by = c("name", rep("alias", length(al))),
      stringsAsFactors = FALSE)
  }
  for (e in r$orders) add("orders", e)
  for (lv in c("suborders", "groups", "subgroups"))
    for (lst in r[[lv]]) for (e in lst) add(lv, e)
  tab <- do.call(rbind, rows)
  tab$key <- .cst_label_key(tab$form)
  tab <- tab[!duplicated(tab[c("key", "code")]), , drop = FALSE]
  rownames(tab) <- NULL
  if (is.null(rules)) .cst_cache$label_table <- tab
  tab
}

# One name against the table: a one-row data frame (code NA when unmatched).
.cst_match_one <- function(part, tab) {
  none <- data.frame(level = NA_character_, code = NA_character_, name = NA_character_,
                     matched_by = NA_character_, stringsAsFactors = FALSE)
  k <- .cst_label_key(part)
  if (is.na(k) || nchar(k) < 6L) return(none)
  hit <- tab[tab$key == k, , drop = FALSE]
  by <- NULL
  if (nrow(hit)) {
    by <- if (any(hit$by == "name")) "name" else "alias"
  } else {
    d <- as.vector(utils::adist(k, tab$key))
    hit <- tab[d == 1L, , drop = FALSE]
    by <- "typo"
  }
  if (!nrow(hit) || length(unique(hit$code)) != 1L) return(none)
  data.frame(level = hit$level[1], code = hit$code[1], name = hit$name[1],
             matched_by = by, stringsAsFactors = FALSE)
}

#' Match a hand-written Chinese Soil Taxonomy name to a taxon of the key
#'
#' @param x Character vector of labels (e.g. ISIS \code{classificationName}).
#' @param rules Optional \code{load_rules("cst2001")}.
#' @return A data frame, one row per label: \code{label}, \code{part} (the
#'   part of the label matched), \code{level}, \code{code}, \code{name} (the
#'   canonical name) and \code{matched_by} ("name", "alias" or "typo"); NA
#'   where no part names a CST taxon.
#' @keywords internal
#' @noRd
.cst_match_label <- function(x, rules = NULL) {
  tab <- .cst_label_table(rules)
  depth <- c(orders = 1L, suborders = 2L, groups = 3L, subgroups = 4L)
  out <- lapply(as.character(x), function(lab) {
    parts <- if (is.na(lab)) character(0) else trimws(strsplit(lab, "[;|]")[[1]])
    parts <- parts[nzchar(parts)]
    best <- NULL; best_part <- NA_character_
    for (p in parts) {
      m <- .cst_match_one(p, tab)
      if (!is.na(m$code) && (is.null(best) || depth[[m$level]] > depth[[best$level]])) {
        best <- m; best_part <- p
      }
    }
    if (is.null(best)) best <- .cst_match_one(NA_character_, tab)
    cbind(data.frame(label = lab, part = best_part, stringsAsFactors = FALSE), best)
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# The code of a CST taxon at a level above it: "K3.1.2" -> order "K",
# suborder "K3", group "K3.1", subgroup "K3.1.2"; NA where the code stops
# above that level.
.cst_code_at <- function(code, level = c("orders", "suborders", "groups", "subgroups")) {
  level <- match.arg(level)
  code <- as.character(code)
  n <- c(orders = 1L, suborders = 1L, groups = 2L, subgroups = 3L)[[level]]
  vapply(code, function(cd) {
    if (is.na(cd)) return(NA_character_)
    if (level == "orders") return(substr(cd, 1L, 1L))
    p <- strsplit(cd, ".", fixed = TRUE)[[1]]
    if (level == "suborders") return(if (nchar(p[1]) >= 2L) p[1] else NA_character_)
    if (length(p) < n) NA_character_ else paste(p[seq_len(n)], collapse = ".")
  }, "", USE.NAMES = FALSE)
}
