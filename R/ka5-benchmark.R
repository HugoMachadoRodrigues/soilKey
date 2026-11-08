# =============================================================================
# soilKey -- German KA5 (2005): reading an older German soil name.
#
# v0.9.379. Reference profiles and older surveys name German soils in the
# usage before the KA5: the KA4, the DBG systematics of 1998 and the regional
# keys (Schwarzerde, Haftnaessepseudogley, Rosterde, Typischer Gley ...).
# .ka5_match_label() reads such a name as the KA5 class, type or subtype it
# stands for. It serves the offline ISIS benchmark (inst/benchmarks/ka5_isis.R)
# and never changes a classification.
#
# How a name is read (inst/rules/ka5/legacy_names.yaml; readings.yaml):
#   1. a name and the table are compared on a key: lower case, the umlauts
#      written ae, oe, ue, the sharp s ss, other accents dropped, letters only;
#   2. the table holds the names of the key (21 classes, 56 types, the
#      subtypes; soilKey's bracketed codes left out) and the older names of
#      legacy_names.yaml;
#   3. a name of type names joined by hyphens (a transition) is read with the
#      older type names put in for their KA5 name: the subtype of the key when
#      the key prints it, else the type of the last part (p. 191);
#   4. only when the whole name is not read, one leading modifier is taken off
#      (Acker-, Typischer, Primaerer) and the rest is read as in 1-3;
#   5. a label is split at ";" and "|"; the deepest part read wins; only when
#      no part is read are the caller's spelling variants tried (an explicit
#      list, `variants`), the match then flagged "variant". There is no generic
#      tolerance of misspellings: a name not read is NA.
# =============================================================================

.ka5_bench_cache <- new.env(parent = emptyenv())

# The match key of a name: lower case, umlauts as ae/oe/ue, sharp s as ss,
# accents dropped, letters only (hyphens kept when `hyphens`).
.ka5_label_key <- function(x, hyphens = FALSE) {
  k <- tolower(enc2utf8(as.character(x)))
  from <- c("\u00e4", "\u00f6", "\u00fc", "\u00df", "\u00c4", "\u00d6", "\u00dc")
  to <- c("ae", "oe", "ue", "ss", "ae", "oe", "ue")
  for (i in seq_along(from)) k <- gsub(from[i], to[i], k, fixed = TRUE)
  k <- iconv(k, "UTF-8", "ASCII//TRANSLIT", sub = "")
  k <- tolower(k)
  if (hyphens) {
    k <- gsub("\\s*-\\s*", "-", k)
    k <- gsub("[^a-z-]", "", k)
    gsub("^-+|-+$", "", k)
  } else gsub("[^a-z]", "", k)
}

# The legacy file.
.ka5_legacy_rules <- function() {
  if (!is.null(.ka5_bench_cache$legacy)) return(.ka5_bench_cache$legacy)
  p <- system.file("rules", "ka5", "legacy_names.yaml", package = "soilKey")
  if (!nzchar(p)) rlang::abort("legacy_names.yaml of the KA5 not found; reinstall soilKey.")
  .ka5_bench_cache$legacy <- yaml::read_yaml(p)
  .ka5_bench_cache$legacy
}

# The hierarchy of the key: one row per class, type and subtype code, with its
# name, level, type and class (soilKey's bracketed codes left out).
.ka5_code_table <- function(rules = NULL) {
  if (is.null(rules) && !is.null(.ka5_bench_cache$codes)) return(.ka5_bench_cache$codes)
  r <- rules %||% load_rules("ka5")
  rows <- list()
  add <- function(level, code, name, type, class) {
    rows[[length(rows) + 1L]] <<- data.frame(level = level, code = code, name = name, type = type,
                                             class = class, stringsAsFactors = FALSE)
  }
  for (dv in r$classes) for (e in dv) add("classes", as.character(e$code), as.character(e$name), NA_character_, as.character(e$code))
  type_class <- character(0)
  for (cl in names(r$types)) for (e in r$types[[cl]]) {
    add("types", as.character(e$code), as.character(e$name), as.character(e$code), cl)
    type_class[[as.character(e$code)]] <- cl
  }
  for (ty in names(r$subtypes)) for (e in r$subtypes[[ty]]) {
    cd <- as.character(e$code)
    add("subtypes", cd, as.character(e$name), ty, unname(type_class[ty]))
  }
  tab <- do.call(rbind, rows)
  tab <- tab[!grepl("^\\[", tab$code) & !startsWith(tab$name, "..."), , drop = FALSE]
  rownames(tab) <- NULL
  if (is.null(rules)) .ka5_bench_cache$codes <- tab
  tab
}

# The table of names: the names of the key and the rows of legacy_names.yaml,
# one row per form, with its match key.
.ka5_label_table <- function(rules = NULL, legacy = NULL) {
  if (is.null(rules) && is.null(legacy) && !is.null(.ka5_bench_cache$labels)) return(.ka5_bench_cache$labels)
  codes <- .ka5_code_table(rules)
  lg <- legacy %||% .ka5_legacy_rules()
  key_rows <- data.frame(form = codes$name, code = codes$code, by = "name", variety = NA_character_,
                         part = codes$level == "types", reading = FALSE, stringsAsFactors = FALSE)
  leg <- lg$names %||% list()
  leg_rows <- data.frame(
    form = vapply(leg, function(e) as.character(e$name), ""),
    code = vapply(leg, function(e) as.character(e$code), ""),
    by = "legacy",
    variety = vapply(leg, function(e) as.character(e$variety %||% NA_character_), ""),
    part = vapply(leg, function(e) isTRUE(e$part), NA),
    reading = vapply(leg, function(e) identical(e$reading, "soilKey"), NA),
    stringsAsFactors = FALSE)
  tab <- rbind(key_rows, leg_rows)
  tab$key <- .ka5_label_key(tab$form)
  tab <- tab[!duplicated(tab[c("key", "code")]), , drop = FALSE]
  m <- match(tab$code, codes$code)
  tab$level <- codes$level[m]
  rownames(tab) <- NULL
  if (is.null(rules) && is.null(legacy)) .ka5_bench_cache$labels <- tab
  tab
}

.ka5_match_none <- function() {
  data.frame(level = NA_character_, code = NA_character_, name = NA_character_,
             type = NA_character_, class = NA_character_, variety = NA_character_,
             modifier = NA_character_, matched_by = NA_character_, reading = NA,
             stringsAsFactors = FALSE)
}

.ka5_match_row <- function(code, codes, by, variety = NA_character_, reading = FALSE, modifier = NA_character_) {
  j <- match(code, codes$code)
  data.frame(level = codes$level[j], code = code, name = codes$name[j], type = codes$type[j],
             class = codes$class[j], variety = variety, modifier = modifier, matched_by = by,
             reading = reading, stringsAsFactors = FALSE)
}

# One name (no modifier) against the table: exact, then as a transition.
.ka5_resolve_name <- function(nm, tab, codes) {
  k <- .ka5_label_key(nm)
  if (is.na(k) || !nzchar(k)) return(NULL)
  hit <- tab[tab$key == k, , drop = FALSE]
  if (nrow(hit)) {
    if (length(unique(hit$code)) != 1L) return(NULL)
    h <- if (any(hit$by == "name")) hit[hit$by == "name", , drop = FALSE][1L, ] else hit[1L, ]
    return(.ka5_match_row(h$code, codes, h$by, h$variety, isTRUE(h$reading)))
  }
  # a transition: type names joined by hyphens
  kh <- .ka5_label_key(nm, hyphens = TRUE)
  parts <- strsplit(kh, "-", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  if (length(parts) < 2L) return(NULL)
  pt <- tab[tab$part & tab$level == "types", , drop = FALSE]
  ptype <- vapply(parts, function(p) {
    h <- unique(pt$code[pt$key == gsub("[^a-z]", "", p)])
    if (length(h) == 1L) h else NA_character_
  }, "")
  if (anyNA(ptype)) return(NULL)
  used_legacy <- any(vapply(parts, function(p) !any(pt$by[pt$key == p] == "name"), NA))
  used_reading <- any(pt$reading[pt$key %in% parts & pt$by == "legacy"])
  # the KA5 name of the joined types
  joined <- paste(codes$name[match(ptype, codes$code)], collapse = "-")
  hit <- tab[tab$key == .ka5_label_key(joined) & tab$level == "subtypes", , drop = FALSE]
  if (nrow(hit) && length(unique(hit$code)) == 1L)
    return(.ka5_match_row(hit$code[1L], codes, if (used_legacy) "legacy" else "name", reading = used_reading))
  .ka5_match_row(ptype[length(ptype)], codes, "transition", reading = used_reading)
}

# The rest of `s` after a leading word `w` (compared on the match key), or
# NULL; unless `joined`, the word must end at a space or a hyphen.
.ka5_strip_lead <- function(s, w, joined = FALSE) {
  wk <- .ka5_label_key(w)
  n <- nchar(s)
  for (i in seq_len(n - 1L)) {
    k <- .ka5_label_key(substr(s, 1L, i))
    if (nchar(k) > nchar(wk)) return(NULL)
    if (!identical(k, wk)) next
    nxt <- substr(s, i + 1L, i + 1L)
    if (!joined && !grepl("^[[:space:]-]$", nxt)) next
    if (grepl("^[)]$", nxt)) next
    return(trimws(sub("^[[:space:]-]+", "", substr(s, i + 1L, n))))
  }
  NULL
}

# One part of a label: the whole name, else one leading modifier off.
.ka5_match_one <- function(part, tab, codes, legacy) {
  m <- .ka5_resolve_name(part, tab, codes)
  if (!is.null(m)) return(m)
  s <- trimws(enc2utf8(as.character(part)))
  if (is.na(s) || !nzchar(s)) return(.ka5_match_none())
  for (md in legacy$modifiers %||% list()) {
    for (w in as.character(unlist(md$words))) {
      rest <- .ka5_strip_lead(s, w, joined = isTRUE(md$joined))
      if (is.null(rest) || !nzchar(rest)) next
      m <- .ka5_resolve_name(rest, tab, codes)
      if (is.null(m)) next
      mod <- as.character(md$words[[1]])
      m$modifier <- mod
      m$reading <- isTRUE(m$reading) || identical(md$reading, "soilKey")
      if (identical(md$effect, "variety")) {
        m$variety <- paste(stats::na.omit(c(m$variety, md$variety)), collapse = ";")
      } else if (identical(md$effect, "norm")) {
        # Typisch: the Norm subtype of a type; on anything else, unread
        if (!identical(m$level, "types")) return(.ka5_match_none())
        nc <- paste0(m$code, "n")
        if (!nc %in% codes$code) return(.ka5_match_none())
        m <- .ka5_match_row(nc, codes, m$matched_by, m$variety, m$reading, mod)
      }
      return(m)
    }
  }
  .ka5_match_none()
}

#' Match an older German soil name to a class, type or subtype of the KA5
#'
#' @param x Character vector of labels.
#' @param rules Optional \code{load_rules("ka5")}.
#' @param variants Optional named character vector of spelling variants
#'   tried only when no part of a label is read exactly: names are regular
#'   expressions (case ignored), values their replacements.
#' @return A data frame, one row per label: \code{label}, \code{part} (the
#'   part read), \code{level} ("classes", "types" or "subtypes"),
#'   \code{code}, \code{name} (the KA5 name), \code{type}, \code{class},
#'   \code{variety} (a variety the name carries, not scored),
#'   \code{modifier}, \code{matched_by} ("name", "legacy", "transition" or
#'   "variant") and \code{reading} (TRUE when a soilKey reading was used);
#'   NA where no part is read.
#' @keywords internal
#' @noRd
.ka5_match_label <- function(x, rules = NULL, variants = NULL) {
  tab <- .ka5_label_table(rules)
  codes <- .ka5_code_table(rules)
  legacy <- .ka5_legacy_rules()
  depth <- c(classes = 1L, types = 2L, subtypes = 3L)
  deepest <- function(parts, fix = identity) {
    best <- NULL; best_part <- NA_character_
    for (p in parts) {
      m <- .ka5_match_one(fix(p), tab, codes, legacy)
      if (!is.na(m$code) && (is.null(best) || depth[[m$level]] > depth[[best$level]])) {
        best <- m; best_part <- p
      }
    }
    list(m = best, part = best_part)
  }
  fix_variants <- function(p) {
    for (k in seq_along(variants)) p <- gsub(names(variants)[k], variants[[k]], p, ignore.case = TRUE, perl = TRUE)
    p
  }
  out <- lapply(as.character(x), function(lab) {
    parts <- if (is.na(lab)) character(0) else trimws(strsplit(lab, "[;|]")[[1]])
    parts <- parts[nzchar(parts)]
    b <- deepest(parts)
    if (is.null(b$m) && length(variants)) {
      b <- deepest(parts, fix_variants)
      if (!is.null(b$m)) b$m$matched_by <- "variant"
    }
    if (is.null(b$m)) b$m <- .ka5_match_none()
    cbind(data.frame(label = lab, part = b$part, stringsAsFactors = FALSE), b$m)
  })
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# The code of a KA5 taxon at a level: a subtype gives its type and class, a
# type its class; NA below the code's own level.
.ka5_code_at <- function(code, level = c("classes", "types", "subtypes"), rules = NULL) {
  level <- match.arg(level)
  codes <- .ka5_code_table(rules)
  j <- match(as.character(code), codes$code)
  own <- codes$level[j]
  switch(level,
         classes = codes$class[j],
         types = ifelse(own %in% c("types", "subtypes"), codes$type[j], NA_character_),
         subtypes = ifelse(own %in% "subtypes", codes$code[j], NA_character_))
}
