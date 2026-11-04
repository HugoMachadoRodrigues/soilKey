# =============================================================================
# The correspondence tables of the Referentiel pedologique 2008 (AFES;
# Baize D., Girard M.-C. coord.; Editions Quae, 2009) and the labels of the
# RP of 1995 (Hugo's decision Q1, v0.9.354):
#
#   Annex 5  (pp. 378-392) the references and the WRB 2006, with its tables of
#            horizons and qualifiers and its "faux amis": crosswalk_wrb2006.yaml
#   Annex 6  (p. 393) the CPCS 1967 horizons and the RP codes:
#            crosswalk_cpcs_horizons.yaml (the notation reader keeps its own
#            copy, with soilKey's matching, in horizons.yaml)
#   Annex 7  (pp. 394-398) the CPCS 1967 categories and the RP:
#            crosswalk_cpcs_classes.yaml, keyed by the cpcs1967 codes
#   p. VII   the references of 1995 removed in 2008: legacy_rp1995.yaml
#
# Data for display and benchmarks only: no classifier reads them, and no
# result changes. The report and the Pro app show the WRB 2006 names of an
# RP result and the RP equivalents of a CPCS result as notes.
# =============================================================================

.rp_cw_cache <- new.env(parent = emptyenv())

# One of the four files, read once.
.rp_cw_read <- function(name) {
  if (!is.null(.rp_cw_cache[[name]])) return(.rp_cw_cache[[name]])
  path <- system.file("rules", "rp2008", paste0(name, ".yaml"), package = "soilKey")
  if (!nzchar(path)) rlang::abort(sprintf("rp2008/%s.yaml is not installed.", name))
  x <- yaml::read_yaml(path)
  assign(name, x, envir = .rp_cw_cache)
  x
}

# The 110 references of the list of pp. 402-403 with their GER, from key.yaml
# (a GER implemented lists codes, whose names are in its references file; a
# GER of a later release lists codes and names).
.rp_reference_catalogue <- function(rules = .rp_rules()) {
  rows <- list()
  for (g in rules$ger) for (r in g$references) {
    if (is.list(r)) { cd <- r$code; nm <- r$name }
    else { cd <- as.character(r); nm <- rules$ref_by_code[[cd]]$name %||% NA_character_ }
    rows[[length(rows) + 1L]] <- data.frame(code = cd, name = nm, ger = g$code, stringsAsFactors = FALSE)
  }
  do.call(rbind, rows)
}

# The level of a cpcs1967 code: I class, I-1 subclass, I/11 group, I/111
# subgroup.
.cpcs_code_level <- function(code) {
  ifelse(grepl("/[0-9]{3,}$", code), "subgroups",
         ifelse(grepl("/[0-9]{2}$", code), "groups",
                ifelse(grepl("-", code, fixed = TRUE), "subclasses", "classes")))
}

.cw_chr <- function(x) if (is.null(x)) NA_character_ else paste(as.character(unlist(x)), collapse = ", ")
.cw_lgl <- function(x) isTRUE(x)


#' Correspondence tables of the Referentiel pedologique 2008
#'
#' The correspondence tables of the Referentiel pedologique 2008 (AFES,
#' 2009) as data frames: codes and names as the book prints them, with the
#' printed page, its doubts ("?") kept as a flag and its slips noted. They
#' are notes for display and benchmarks: \code{\link{classify_rp2008}} and
#' \code{\link{classify_cpcs1967}} never read them.
#'
#' \describe{
#'   \item{\code{"wrb2006"}}{Annex 5 (pp. 379-390): one row per reference of
#'     the 110 (two for the brunisols eutriques, which the annex splits by
#'     their saturation, and one more for the lithosols stricts), with its
#'     WRB 2006 name. The WRB 2006 is the edition the book cites, not
#'     soilKey's WRB 2022.}
#'   \item{\code{"wrb2006_horizons"}}{Annex 5: the reference horizons the
#'     tables put beside a WRB 2006 diagnostic horizon (pp. 380, 383, 384).}
#'   \item{\code{"wrb2006_qualifiers"}}{Annex 5: the RP qualifiers and their
#'     WRB 2006 qualifiers, "most often imperfect" (pp. 379, 380, 385,
#'     391-392); \code{scope} is the group the book restricts a row to.}
#'   \item{\code{"wrb2006_faux_amis"}}{Annex 5: the WRB 2006 qualifiers
#'     spelled like an RP qualifier but meaning something else (p. 392).}
#'   \item{\code{"cpcs_horizons"}}{Annex 6 (p. 393): one row per CPCS 1967
#'     horizon symbol and RP code; \code{intertropical} for the codes of the
#'     intertropical domain, \code{slip} for the codes that are not RP codes,
#'     \code{after_1967} for the symbols not in the CPCS 1967.}
#'   \item{\code{"cpcs_classes"}}{Annex 7 (pp. 394-398): one row per CPCS
#'     category (keyed by its \code{cpcs1967} code) and RP entry: the entry's
#'     name, the reference codes or the group (GER) it names, its qualifiers,
#'     \code{double} for a double attachment, \code{none} where the book
#'     prints "???".}
#'   \item{\code{"cpcs_new_in_rp"}}{Annex 7: the RP categories the book calls
#'     completely new with respect to the CPCS (p. 398).}
#'   \item{\code{"legacy_rp1995"}}{The references of the RP of 1995 removed
#'     in 2008 (p. VII) and the names of 2008 the book gives for such solums;
#'     \code{reading} where the destination is soilKey's reading. Used by
#'     \code{\link{rp2008_from_label}}.}
#' }
#'
#' @param table Which table (see Details).
#' @return A data frame, one row per pair of the table.
#' @seealso \code{\link{rp2008_from_label}}, \code{\link{classify_rp2008}}.
#' @examples
#' head(rp2008_crosswalk("wrb2006"))
#' rp2008_crosswalk("cpcs_horizons")[1:6, ]
#' subset(rp2008_crosswalk("cpcs_classes"), cpcs == "V/111")
#' @export
rp2008_crosswalk <- function(table = c("wrb2006", "wrb2006_horizons", "wrb2006_qualifiers",
                                       "wrb2006_faux_amis", "cpcs_horizons", "cpcs_classes",
                                       "cpcs_new_in_rp", "legacy_rp1995")) {
  table <- match.arg(table)
  df <- function(rows) {
    if (!length(rows)) return(data.frame())
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    out
  }
  switch(table,
    wrb2006 = {
      cat <- .rp_reference_catalogue()
      df(lapply(.rp_cw_read("crosswalk_wrb2006")$references, function(r) data.frame(
        code = r$code, ger = cat$ger[match(r$code, cat$code)], rp = r$rp,
        wrb2006 = r$wrb2006 %||% NA_character_, doubtful = .cw_lgl(r$doubtful),
        note = r$note %||% NA_character_, page = as.integer(r$page), stringsAsFactors = FALSE)))
    },
    wrb2006_horizons = df(unlist(lapply(.rp_cw_read("crosswalk_wrb2006")$horizons, function(r)
      lapply(as.character(unlist(r$codes)), function(cd) data.frame(
        rp = cd, wrb2006 = r$wrb2006, note = r$note %||% NA_character_,
        page = as.integer(r$page), stringsAsFactors = FALSE))), recursive = FALSE)),
    wrb2006_qualifiers = df(lapply(.rp_cw_read("crosswalk_wrb2006")$qualifiers, function(r) data.frame(
      rp = r$rp, scope = r$scope %||% NA_character_, wrb2006 = r$wrb2006,
      note = r$note %||% NA_character_, page = as.integer(r$page), stringsAsFactors = FALSE))),
    wrb2006_faux_amis = {
      fa <- .rp_cw_read("crosswalk_wrb2006")$faux_amis
      data.frame(wrb2006 = as.character(unlist(fa$names)), page = as.integer(fa$page),
                 stringsAsFactors = FALSE)
    },
    cpcs_horizons = {
      a6 <- .rp_cw_read("crosswalk_cpcs_horizons")
      df(lapply(a6$rows, function(r) {
        rp <- as.character(unlist(r$rp)); it <- as.character(unlist(r$intertropical))
        codes <- c(rp, it)
        data.frame(cpcs = r$cpcs, rp = codes, intertropical = codes %in% it & !codes %in% rp,
                   slip = codes %in% as.character(unlist(r$slips)),
                   after_1967 = .cw_lgl(r$after_1967), note = r$note %||% NA_character_,
                   page = as.integer(a6$page), stringsAsFactors = FALSE)
      }))
    },
    cpcs_classes = df(lapply(.rp_cw_read("crosswalk_cpcs_classes")$rows, function(r) {
      base <- data.frame(cpcs = r$cpcs, cpcs_level = .cpcs_code_level(r$cpcs), cpcs_name = r$cpcs_name,
                         stringsAsFactors = FALSE)
      items <- r$rp
      if (!length(items))
        return(cbind(base, data.frame(item = 0L, rp_name = NA_character_, references = NA_character_,
                                      ger = NA_character_, qualifiers = NA_character_, double = FALSE,
                                      any_reference = FALSE, scope = NA_character_, doubtful = FALSE,
                                      none = .cw_lgl(r$none), open = .cw_lgl(r$open),
                                      page = as.integer(r$page), stringsAsFactors = FALSE)))
      do.call(rbind, lapply(seq_along(items), function(k) {
        it <- items[[k]]
        cbind(base, data.frame(item = k, rp_name = it$name, references = .cw_chr(it$references),
                               ger = it$ger %||% NA_character_, qualifiers = .cw_chr(it$qualifiers),
                               double = .cw_lgl(it$double), any_reference = .cw_lgl(it$any_reference),
                               scope = it$scope %||% NA_character_, doubtful = .cw_lgl(it$doubtful),
                               none = .cw_lgl(r$none), open = .cw_lgl(r$open),
                               page = as.integer(r$page), stringsAsFactors = FALSE))
      }))
    })),
    cpcs_new_in_rp = {
      nw <- .rp_cw_read("crosswalk_cpcs_classes")$new_in_rp
      df(lapply(nw$categories, function(it) data.frame(
        rp_name = it$name, references = .cw_chr(it$references), ger = it$ger %||% NA_character_,
        page = as.integer(nw$page), stringsAsFactors = FALSE)))
    },
    legacy_rp1995 = df(lapply(.rp_cw_read("legacy_rp1995")$labels, function(r)
      do.call(rbind, lapply(seq_along(r$rp2008), function(k) {
        it <- r$rp2008[[k]]
        data.frame(label = r$label, item = k, rp_name = it$name, references = .cw_chr(it$references),
                   ger = it$ger %||% NA_character_, qualifiers = .cw_chr(it$qualifiers),
                   note = it$note %||% NA_character_, reading = .cw_lgl(r$reading),
                   page = as.character(r$page), stringsAsFactors = FALSE)
      }))))
  )
}


# --- labels of French datasets ---------------------------------------------------

# A label folded for matching: lower case, no accents (ae for the ligature),
# punctuation as spaces, and each word (each part of a hyphenated word) in
# the singular: a final s dropped, -aux read as -al (distaux, distal).
.rp_label_fold <- function(x) {
  x <- tolower(enc2utf8(as.character(x)))
  x <- gsub("\u00e6", "ae", x, fixed = TRUE)
  x <- gsub("\u0153", "oe", x, fixed = TRUE)
  x <- chartr("\u00e0\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ee\u00ef\u00f4\u00f6\u00f9\u00fb\u00fc\u00e7",
              "aaaeeeeiioouuuc", x)
  x <- gsub("[^a-z0-9 -]+", " ", x)
  x <- gsub("\\s*-\\s*", "-", x)
  vapply(strsplit(trimws(x), "\\s+"), function(w) {
    if (!length(w)) return("")
    paste(vapply(w, function(word) {
      parts <- strsplit(word, "-", fixed = TRUE)[[1]]
      parts <- sub("aux$", "al", parts)
      parts <- ifelse(nchar(parts) > 2L, sub("s$", "", parts), parts)
      paste(parts, collapse = "-")
    }, ""), collapse = " ")
  }, "", USE.NAMES = FALSE)
}

# The names a label may start with: the 110 references of 2008, the labels
# of 1995 removed in 2008, and the groups (GER) by a one-word name (the four
# joint chapters by each of their two words, the calcium chapter by none).
.rp_label_dictionary <- function() {
  if (!is.null(.rp_cw_cache$dictionary)) return(.rp_cw_cache$dictionary)
  rules <- .rp_rules()
  cat <- .rp_reference_catalogue(rules)
  rows <- list()
  add <- function(key, refs, ger, source, note = NA_character_) {
    rows[[length(rows) + 1L]] <<- data.frame(key = key, references = paste(refs, collapse = ", "),
                                             ger = ger, source = source, note = note,
                                             stringsAsFactors = FALSE)
  }
  for (g in rules$ger) {
    if (identical(g$code, "calcium_magnesium")) next
    refs <- cat$code[cat$ger == g$code]
    words <- strsplit(.rp_label_fold(g$name), " et ", fixed = TRUE)[[1]]
    for (w in words) {
      sub_refs <- if (length(words) > 1L) refs[startsWith(.rp_label_fold(cat$name[cat$ger == g$code]), w)] else refs
      if (!grepl(" ", w)) add(w, sub_refs, g$code, "group")
    }
  }
  for (k in seq_len(nrow(cat))) add(.rp_label_fold(cat$name[k]), cat$code[k], cat$ger[k], "rp2008")
  for (r in .rp_cw_read("legacy_rp1995")$labels) {
    refs <- unique(unlist(lapply(r$rp2008, function(it) {
      rr <- as.character(unlist(it$references))
      if (!length(rr) && !is.null(it$ger)) cat$code[cat$ger == it$ger] else rr
    })))
    gers <- unique(c(unlist(lapply(r$rp2008, `[[`, "ger")), cat$ger[match(refs, cat$code)]))
    add(.rp_label_fold(r$label), refs, paste(stats::na.omit(gers), collapse = ", "), "rp1995",
        paste(vapply(r$rp2008, `[[`, "", "name"), collapse = "; "))
  }
  d <- do.call(rbind, rows)
  # a hyphenated name also written without the hyphen (oligo-sature,
  # oligosature, oligo sature)
  hy <- d[grepl("-", d$key, fixed = TRUE), ]
  d <- rbind(d, transform(hy, key = gsub("-", "", hy$key, fixed = TRUE)),
             transform(hy, key = gsub("-", " ", hy$key, fixed = TRUE)))
  # a reference named like its group (arenosols): the reference
  d <- d[order(d$key, match(d$source, c("rp2008", "rp1995", "group"))), ]
  d <- d[!duplicated(d$key), ]
  d <- d[order(-nchar(d$key)), ]
  assign("dictionary", d, envir = .rp_cw_cache)
  d
}

# The longest name of the dictionary the folded text starts with, ending at
# a word or a hyphen; NULL when none.
.rp_label_head <- function(s, dict) {
  for (i in which(startsWith(s, dict$key))) {
    k <- dict$key[i]
    nxt <- substr(s, nchar(k) + 1L, nchar(k) + 1L)
    if (!nzchar(nxt) || nxt %in% c(" ", "-")) return(list(row = dict[i, ], rest = substr(s, nchar(k) + 1L, nchar(s))))
  }
  NULL
}


#' Read French soil labels as references of the Referentiel pedologique 2008
#'
#' Reads labels of French datasets (a reference, a type with its
#' qualifiers, a double attachment written with a hyphen) as the references
#' of the RP 2008, for benchmarks. A label of the RP of 1995 whose reference
#' was removed in 2008 (p. VII: the four old brunisols, the pseudo-luvisols,
#' the reductisols dupliques ...) is read through
#' \code{rp2008_crosswalk("legacy_rp1995")}; a reference of 1995 that keeps
#' its name in 2008 is read as the 2008 one. Case, accents, the singular or
#' plural and the qualifiers after the name do not matter: the longest
#' name the label starts with decides.
#'
#' @param labels Character vector of labels.
#' @return A data frame, one row per label: \code{label}; \code{references}
#'   (the codes, comma-separated: several when the label leaves the choice
#'   open, any of them); \code{ger} (the group of references); \code{source}
#'   (\code{"rp2008"} a reference of 2008, \code{"rp1995"} a label of 1995
#'   removed in 2008, \code{"group"} a group named alone, \code{NA} not
#'   read); \code{double_with} (the codes of the second part of a double
#'   attachment, as in \code{luvisol typique-redoxisol}); \code{note}.
#' @seealso \code{\link{rp2008_crosswalk}}.
#' @examples
#' rp2008_from_label(c("Brunisol eutrique resature, limoneux",
#'                     "BRUNISOL SATURE", "luvisol typique-redoxisol",
#'                     "Sol brun lessive"))
#' @export
rp2008_from_label <- function(labels) {
  dict <- .rp_label_dictionary()
  labels <- as.character(labels)
  rows <- lapply(labels, function(lb) {
    out <- data.frame(label = lb, references = NA_character_, ger = NA_character_,
                      source = NA_character_, double_with = NA_character_, note = NA_character_,
                      stringsAsFactors = FALSE)
    if (is.na(lb) || !nzchar(trimws(lb))) return(out)
    h <- .rp_label_head(.rp_label_fold(lb), dict)
    if (is.null(h)) return(out)
    out$references <- h$row$references
    out$ger <- h$row$ger
    out$source <- h$row$source
    out$note <- h$row$note
    if (startsWith(h$rest, "-")) {
      h2 <- .rp_label_head(substr(h$rest, 2L, nchar(h$rest)), dict)
      if (!is.null(h2)) out$double_with <- h2$row$references
    }
    out
  })
  do.call(rbind, rows)
}


# --- notes for the report and the Pro app ----------------------------------------

# The correspondence notes of a result: for an RP 2008 result, the WRB 2006
# names of its references (Annex 5); for a CPCS 1967 result, the RP 2008
# equivalents of the most specific category of its chain that Annex 7 has a
# row for. list(kind, lines), kind "wrb2006" or "cpcs"; NULL for any other
# result or one with no class.
.rp_crosswalk_lines <- function(res) {
  id <- tryCatch(res$system_id, error = function(e) NULL)
  lv <- tryCatch(res$levels, error = function(e) NULL)
  if (is.null(id) || length(id) != 1L || is.na(id) || !is.data.frame(lv) || !nrow(lv)) return(NULL)
  if (identical(id, "rp2008")) {
    a5 <- .rp_cw_read("crosswalk_wrb2006")$references
    lines <- unlist(lapply(unique(lv$code), function(cd) {
      rows <- Filter(function(r) identical(r$code, cd), a5)
      vapply(rows, function(r) {
        w <- r$wrb2006 %||% "not taken into account by the WRB 2006"
        sprintf("%s: %s%s (p. %d)", r$rp, w,
                if (isTRUE(r$doubtful) && !grepl("?", w, fixed = TRUE)) " (?)" else "", as.integer(r$page))
      }, "")
    }))
    if (!length(lines)) return(NULL)
    return(list(kind = "wrb2006", lines = lines))
  }
  if (identical(id, "cpcs1967")) {
    a7 <- .rp_cw_read("crosswalk_cpcs_classes")$rows
    keys <- vapply(a7, `[[`, "", "cpcs")
    chain <- rev(as.character(stats::na.omit(lv$code)))
    if (!length(chain)) return(NULL)
    hit <- chain[chain %in% keys][1]
    if (is.na(hit)) return(list(kind = "cpcs", lines = sprintf(
      "Annex 7 has no row for %s or the categories above it.", chain[1])))
    r <- a7[[match(hit, keys)]]
    if (isTRUE(r$none))
      return(list(kind = "cpcs", lines = sprintf("%s (%s, p. %d): no equivalent in the RP 2008 (???).",
                                                 r$cpcs_name, r$cpcs, as.integer(r$page))))
    items <- vapply(r$rp, function(it) paste0(
      it$name, if (isTRUE(it$doubtful)) " (?)" else "",
      if (isTRUE(it$double)) " (double attachment)" else "",
      if (!is.null(it$scope)) sprintf(" [%s]", it$scope) else ""), "")
    return(list(kind = "cpcs", lines = sprintf("%s (%s, p. %d): %s%s", r$cpcs_name, r$cpcs,
                                               as.integer(r$page), paste(items, collapse = "; "),
                                               if (isTRUE(r$open)) ", etc." else ".")))
  }
  NULL
}
