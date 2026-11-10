# =============================================================================
# soilKey -- FAO-Unesco legend: the two editions side by side.
#
# v0.9.391. The correspondence between the Legend of 1974 (fao1974) and the
# Revised Legend of 1988 (fao1988), from Appendix 4 of the Revised Legend
# (pp. 113-126) with the three changes it does not flag in brackets
# (inst/rules/fao1988/crosswalk.yaml): fao_crosswalk() and the edition line
# of a result (print(), the reports, the Pro app). Display only (Hugo's
# decision Q8): it never changes a classification.
# =============================================================================

.fao_cw_cache <- new.env(parent = emptyenv())

.fao_cw_read <- function() {
  if (!is.null(.fao_cw_cache$cw)) return(.fao_cw_cache$cw)
  path <- system.file("rules", "fao1988", "crosswalk.yaml", package = "soilKey")
  if (!nzchar(path)) rlang::abort("fao1988/crosswalk.yaml is not installed.")
  x <- yaml::read_yaml(path)
  assign("cw", x, envir = .fao_cw_cache)
  x
}

# The names of the codes of one edition (groups and units), from the rule
# files: a named character vector code -> name.
.fao_code_names <- function(edition = c("1974", "1988")) {
  edition <- match.arg(edition)
  key <- paste0("names_", edition)
  if (!is.null(.fao_cw_cache[[key]])) return(.fao_cw_cache[[key]])
  if (edition == "1974") {
    g <- .F74_GROUPS
    u <- load_rules("fao1974")$units
  } else {
    g <- .F88_GROUPINGS
    u <- load_rules("fao1988")$units
  }
  nm <- stats::setNames(g$name, g$code)
  for (lst in u) for (e in lst) nm[as.character(e$code)] <- as.character(e$name)
  assign(key, nm, envir = .fao_cw_cache)
  nm
}

.FAO_CW_RELATION <- c(same = "same name", renamed = "renamed", amended = "amended",
                      renamed_amended = "renamed and amended", merged = "merged",
                      split = "split", deleted = "deleted", new = "new")

#' The correspondence between the FAO-Unesco legends of 1974 and 1988
#'
#' The official correspondence of Appendix 4 of the Revised Legend (FAO
#' 1988, "Main changes in the revised legend", pp. 113-126) between the 26
#' major soil groups and 106 soil units of the Legend of 1974 and the 28
#' major soil groupings and 153 soil units of 1988, with the three changes
#' the appendix does not flag added in brackets (Calcaric Gleysols to Calcic
#' Gleysols is a change of definition; the Plinthic Luvisols have no 1988
#' row; the Thionic Histosols are new). For display and for reading
#' labelled data only: it never classifies (\code{\link{classify_fao}} keys
#' each edition by its own text).
#'
#' @param code Optional character vector of codes to look up, of the
#'   edition \code{from} (a group, grouping or unit: \code{"Lo"}, \code{"L"};
#'   \code{"LXh"}, \code{"LX"}). \code{NULL} (default) for the whole table.
#' @param from \code{"1974"} (default): one row per 1974 class; the 1988
#'   codes it goes to. \code{"1988"}: the rows whose 1988 side names the
#'   code (as a target or as one of soilKey's alternatives), and the rows
#'   of the 1988 classes with no 1974 class (relation \code{"new"}).
#' @return A \code{data.frame} with \code{level} (\code{"group"} or
#'   \code{"unit"}), \code{code_1974}, \code{name_1974}, \code{code_1988}
#'   and \code{name_1988} (several joined by \code{", "}; empty when
#'   deleted), \code{relation} (\code{"same"}, \code{"renamed"},
#'   \code{"amended"}, \code{"renamed_amended"}, \code{"merged"},
#'   \code{"split"}, \code{"deleted"}, \code{"new"}), \code{also} and
#'   \code{also_when} (soilKey's alternative 1988 targets read from a group
#'   comment of the appendix, such as the Lixisols for a low activity
#'   Luvisol, and when they apply), \code{page} (printed page of 1988),
#'   \code{bracketed} (\code{TRUE} for a row the appendix does not print)
#'   and \code{note} (the comment, paraphrased).
#' @references FAO (1988). FAO/Unesco Soil Map of the World, Revised
#'   Legend. World Soil Resources Report 60, FAO, Rome; ISRIC reprint 1997,
#'   Appendix 4.
#' @seealso \code{\link{classify_fao}}.
#' @examples
#' fao_crosswalk(c("Lo", "Gc", "Vp", "Y"))[, c("code_1974", "code_1988", "relation", "also")]
#' fao_crosswalk("LXh", from = "1988")[, c("code_1974", "code_1988", "relation")]
#' @export
fao_crosswalk <- function(code = NULL, from = c("1974", "1988")) {
  from <- match.arg(as.character(from)[1L], c("1974", "1988"))
  cw <- .fao_cw_read()
  n74 <- .fao_code_names("1974")
  n88 <- .fao_code_names("1988")
  nm <- function(codes, tab) if (length(codes)) paste(unname(tab[codes]), collapse = ", ") else ""
  row <- function(level, e) {
    to <- as.character(unlist(e$to %||% character(0)))
    al <- as.character(unlist(e$also %||% character(0)))
    data.frame(level = level, code_1974 = as.character(e$from), name_1974 = unname(n74[as.character(e$from)]),
               code_1988 = paste(to, collapse = ", "), name_1988 = nm(to, n88),
               relation = as.character(e$relation),
               also = paste(al, collapse = ", "), also_when = as.character(e$also_when %||% ""),
               page = as.integer(e$page), bracketed = isTRUE(e$bracketed),
               note = as.character(e$note %||% ""), stringsAsFactors = FALSE)
  }
  new_row <- function(level, e) data.frame(
    level = level, code_1974 = NA_character_, name_1974 = NA_character_,
    code_1988 = as.character(e$to), name_1988 = unname(n88[as.character(e$to)]),
    relation = "new", also = "", also_when = "", page = as.integer(e$page),
    bracketed = isTRUE(e$bracketed), note = as.character(e$note %||% ""), stringsAsFactors = FALSE)
  tab <- do.call(rbind, c(lapply(cw$groups, function(e) row("group", e)),
                          lapply(cw$units, function(e) row("unit", e))))
  if (from == "1974") {
    if (!is.null(code)) tab <- tab[tab$code_1974 %in% as.character(code), , drop = FALSE]
    rownames(tab) <- NULL
    return(tab)
  }
  tab <- rbind(tab, do.call(rbind, c(lapply(cw$new_groups, function(e) new_row("group", e)),
                                     lapply(cw$new_units, function(e) new_row("unit", e)))))
  if (!is.null(code)) {
    split_codes <- function(x) strsplit(x, ", ", fixed = TRUE)
    hit <- vapply(seq_len(nrow(tab)), function(i)
      any(as.character(code) %in% c(split_codes(tab$code_1988[i])[[1]], split_codes(tab$also[i])[[1]])), NA)
    tab <- tab[hit, , drop = FALSE]
  }
  rownames(tab) <- NULL
  tab
}

# The edition line of a FAO-Unesco result (display only, never a class):
# for a 1974 result, the 1988 classes of its unit (or group); for a 1988
# result, the 1974 classes that went to its unit (or grouping). A character
# vector of lines; NULL for any other result or one with no class.
.fao_edition_lines <- function(res) {
  id <- tryCatch(res$system_id, error = function(e) NULL)
  lv <- tryCatch(res$levels, error = function(e) NULL)
  if (is.null(id) || length(id) != 1L || is.na(id) || !id %in% c("fao1974", "fao1988")) return(NULL)
  if (!is.data.frame(lv) || !nrow(lv) || !"code" %in% names(lv)) return(NULL)
  codes <- rev(as.character(stats::na.omit(lv$code)))
  if (!length(codes)) return(NULL)
  rel <- function(r) {
    s <- .FAO_CW_RELATION[[r$relation]]
    sprintf("%s, App. 4 p. %d", s, r$page)
  }
  if (id == "fao1974") {
    tab <- fao_crosswalk()
    # a code that is both a group and a unit (the Lithosols, Rankers and
    # Rendzinas) reads its unit row
    tab <- tab[order(tab$level != "unit"), , drop = FALSE]
    hit <- codes[codes %in% tab$code_1974][1]
    if (is.na(hit)) return(NULL)
    r <- tab[match(hit, tab$code_1974), ]
    main <- if (nzchar(r$code_1988)) {
      to <- strsplit(r$code_1988, ", ", fixed = TRUE)[[1]]
      sprintf("%s (%s)", paste(sprintf("%s %s", to, .fao_code_names("1988")[to]), collapse = " or "), rel(r))
    } else sprintf("no class of 1988 (%s)", rel(r))
    alt <- if (nzchar(r$also)) {
      al <- strsplit(r$also, ", ", fixed = TRUE)[[1]]
      sprintf(" [or %s: %s]", paste(sprintf("%s %s", al, .fao_code_names("1988")[al]), collapse = ", "), r$also_when)
    } else ""
    return(sprintf("Revised Legend (1988): %s%s.", main, alt))
  }
  tab <- fao_crosswalk(from = "1988")
  on_side <- function(x, cd) vapply(strsplit(x, ", ", fixed = TRUE), function(v) cd %in% v, NA)
  # the 1974 classes of the deepest code that has a row; the 1974 classes
  # that soilKey's alternatives send to that code or to the codes above it
  main <- tab[0, , drop = FALSE]
  for (cd in codes) {
    main <- tab[on_side(tab$code_1988, cd), , drop = FALSE]
    if (nrow(main)) break
  }
  alt <- tab[0, , drop = FALSE]
  for (cd in codes) {
    a <- tab[on_side(tab$also, cd) & !tab$code_1974 %in% main$code_1974, , drop = FALSE]
    if (any(a$level == "unit")) a <- a[a$level == "unit", , drop = FALSE]
    if (nrow(a)) { alt <- a; break }
  }
  if (!nrow(main) && !nrow(alt)) return(NULL)
  newr <- main[main$relation == "new", , drop = FALSE]
  main <- main[main$relation != "new", , drop = FALSE]
  parts <- character(0)
  if (nrow(main)) {
    ms <- vapply(seq_len(nrow(main)), function(i) rel(main[i, ]), "")
    parts <- c(parts, paste(sprintf("%s %s (%s)", main$code_1974, main$name_1974, ms), collapse = "; "))
  }
  if (nrow(newr)) parts <- c(parts, sprintf("new in 1988, no class of 1974 (App. 4 p. %d)%s", newr$page[1],
                                            if (isTRUE(newr$bracketed[1])) " [not flagged in App. 4]" else ""))
  if (nrow(alt)) {
    by_when <- split(sprintf("%s %s", alt$code_1974, alt$name_1974), factor(alt$also_when, unique(alt$also_when)))
    parts <- c(parts, sprintf("[some %s]", paste(vapply(names(by_when), function(w)
      sprintf("%s, %s", paste(by_when[[w]], collapse = ", "), w), ""), collapse = "; ")))
  }
  sprintf("Legend of 1974: %s.", paste(parts, collapse = "; "))
}


# --- reading a FAO-Unesco label (v0.9.392) -------------------------------------------------
#
# Reference profiles carry FAO-Unesco names written as names, not codes, in
# the singular and in title case, with an optional third-level subunit
# prefixed to the unit adjective and optional phases after the group:
# "[<Subunit>-]<Unit> <Group>[ <phase> ...]", e.g. "Orthi-Gleyic Acrisol",
# "Gleyic Cambisol petroferric". .fao_match_label() reads such a label in one
# edition and returns the codes of its grouping and unit; it serves the
# offline ISIS benchmark (inst/benchmarks/fao_isis.R) and never classifies.
#
# How a label is read (soilKey's readings, readings.yaml):
#   1. the edition: "1974" (fao1974) or "1988" (fao1988); a publication year
#      is accepted (1974 -> 1974; 1988, 1990 and 1997, the reprints of the
#      Revised Legend, -> 1988);
#   2. the group: the word that names a group of the edition, singular or
#      plural (case alike); the 1974 "Nitosol" and the 1988 "Nitisol" are
#      read in the other edition too, flagged "nitosol_nitisol";
#   3. the words after the group are phases, each matched to a phase of the
#      edition (phases.yaml ids); a word that is not one is reported apart;
#   4. the words before the group are the unit adjective; a hyphenated
#      prefix (ISRIC's third level, "Orthi-", "Abrupti-") is cut off and
#      reported as the subunit; the unit is "<adjective> <group plural>"
#      matched exactly to a unit name of that group; a single-unit group of
#      1974 (Lithosols, Rankers, Rendzinas) named alone is its own unit;
#   5. exact names only: no other spelling is accepted unless the caller's
#      `variants` (regular expression = replacement, case ignored) rewrite
#      the label first, and such a match is flagged "variant".

.fao_label_edition <- function(edition) {
  e <- as.character(edition)
  out <- rep(NA_character_, length(e))
  out[e %in% c("1974", "74", "fao1974")] <- "1974"
  out[e %in% c("1988", "88", "1990", "1997", "fao1988")] <- "1988"
  out
}

.fao_label_tables <- function(edition) {
  key <- paste0("labels_", edition)
  if (!is.null(.fao_cw_cache[[key]])) return(.fao_cw_cache[[key]])
  if (edition == "1974") {
    g <- .F74_GROUPS; u <- load_rules("fao1974")$units
    ph <- vapply(.fao74_phase_rules()$phases, function(p) as.character(p$id), "")
    single <- .F74_SINGLE
  } else {
    g <- .F88_GROUPINGS; u <- load_rules("fao1988")$units
    ph <- vapply(.fao88_phase_rules()$phases, function(p) as.character(p$id), "")
    single <- character(0)
  }
  units <- do.call(rbind, lapply(names(u), function(gc) data.frame(
    group = gc, code = vapply(u[[gc]], function(e) as.character(e$code), ""),
    name = vapply(u[[gc]], function(e) as.character(e$name), ""), stringsAsFactors = FALSE)))
  tab <- list(groups = data.frame(code = g$code, name = g$name, key = .fao_group_key(g$name),
                                  stringsAsFactors = FALSE),
              units = units, phases = ph, single = single)
  assign(key, tab, envir = .fao_cw_cache)
  tab
}

# A group word as a key: lower case, letters only, one final "s" dropped
# ("Solonetz" has none to drop).
.fao_group_key <- function(x) sub("s$", "", gsub("[^a-z]", "", tolower(as.character(x))))

#' Read a FAO-Unesco label in one edition
#'
#' @param x Character vector of labels ("Orthi-Gleyic Acrisol",
#'   "Gleyic Cambisol petroferric", "Lithosol").
#' @param edition The edition of each label (recycled): "1974" or "1988",
#'   or a publication year (1974; 1988, 1990 or 1997 for the Revised
#'   Legend).
#' @param variants Optional named character vector: regular expressions
#'   (case ignored) and their replacements, applied only to a label that is
#'   not read as it stands.
#' @return A data frame, one row per label: \code{label}, \code{edition},
#'   \code{subunit} (the hyphenated prefix cut off, or NA),
#'   \code{group_code}, \code{group_name}, \code{unit_code},
#'   \code{unit_name} (NA where not read), \code{phases} (the phase ids
#'   read, joined by ", "), \code{phases_unread} (words after the group that
#'   are no phase of the edition), \code{matched_by} ("name", "variant" or
#'   NA) and \code{flags} ("nitosol_nitisol", "single_unit_group").
#' @keywords internal
#' @noRd
.fao_match_label <- function(x, edition, variants = NULL) {
  x <- as.character(x)
  ed <- rep_len(.fao_label_edition(edition), length(x))
  one <- function(lab, e) {
    out <- data.frame(label = lab, edition = e, subunit = NA_character_,
                      group_code = NA_character_, group_name = NA_character_,
                      unit_code = NA_character_, unit_name = NA_character_,
                      phases = "", phases_unread = "", matched_by = NA_character_,
                      flags = "", stringsAsFactors = FALSE)
    if (is.na(lab) || is.na(e) || !nzchar(trimws(lab))) return(out)
    tab <- .fao_label_tables(e)
    read <- function(s) {
      w <- strsplit(trimws(gsub("\\s+", " ", s)), " ", fixed = TRUE)[[1]]
      k <- .fao_group_key(w)
      gk <- tab$groups$key
      flags <- character(0)
      # the other edition's spelling of the Nitosols / Nitisols
      alt <- c(nitosol = "nitisol", nitisol = "nitosol")
      gi <- which(k %in% gk)
      if (!length(gi)) {
        gi <- which(k %in% names(alt) & alt[k] %in% gk)
        if (length(gi)) { k[gi] <- alt[k[gi]]; flags <- "nitosol_nitisol" }
      }
      if (length(gi) != 1L) return(NULL)
      g <- tab$groups[match(k[gi], gk), ]
      res <- list(group_code = g$code, group_name = g$name, flags = flags,
                  subunit = NA_character_, unit_code = NA_character_, unit_name = NA_character_)
      after <- if (gi < length(w)) tolower(w[(gi + 1L):length(w)]) else character(0)
      res$phases <- paste(after[after %in% tab$phases], collapse = ", ")
      res$phases_unread <- paste(after[!after %in% tab$phases], collapse = ", ")
      before <- if (gi > 1L) w[seq_len(gi - 1L)] else character(0)
      if (!length(before)) {
        if (g$code %in% tab$single) {
          res$unit_code <- g$code; res$unit_name <- g$name
          res$flags <- c(res$flags, "single_unit_group")
        }
        return(res)
      }
      adj <- paste(before, collapse = " ")
      if (grepl("-", adj, fixed = TRUE)) {
        res$subunit <- sub("-[^-]*$", "", adj)
        adj <- sub("^.*-", "", adj)
      }
      cand <- tab$units[tab$units$group == g$code, , drop = FALSE]
      hit <- which(tolower(cand$name) == tolower(paste(adj, g$name)))
      if (length(hit) == 1L) {
        res$unit_code <- cand$code[hit]; res$unit_name <- cand$name[hit]
      }
      res
    }
    r <- read(lab)
    by <- "name"
    if ((is.null(r) || is.na(r$unit_code)) && length(variants)) {
      s <- lab
      for (i in seq_along(variants)) s <- gsub(names(variants)[i], variants[[i]], s, ignore.case = TRUE, perl = TRUE)
      if (!identical(s, lab)) {
        r2 <- read(s)
        if (!is.null(r2) && (is.null(r) || !is.na(r2$unit_code))) { r <- r2; by <- "variant" }
      }
    }
    if (is.null(r)) return(out)
    for (f in c("subunit", "group_code", "group_name", "unit_code", "unit_name", "phases", "phases_unread"))
      out[[f]] <- r[[f]]
    out$matched_by <- by
    out$flags <- paste(r$flags, collapse = ", ")
    out
  }
  res <- do.call(rbind, Map(one, x, ed))
  rownames(res) <- NULL
  res
}
