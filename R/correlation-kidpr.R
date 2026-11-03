# =============================================================================
# v0.9.347 -- the 1977 analogue of a Russian soil (Field Guide of 2008,
# chapter 6, pp. 139-161), as a note.
#
# Chapter 6 correlates every type of the classification of 2004 (its name
# and its 2004 profile formula) with the units of the classification of
# 1977 (Klassifikatsiya i diagnostika pochv SSSR): types, subtypes, species,
# genera, extra-taxonomic groups, or names of the literature, often "partly"
# or "approximately", and for about half of the types "not distinguished".
# inst/rules/kidpr2008/correlation_1977.yaml holds it as structured data in
# soilKey's words, each row resolved to the type of 2008 it stands for.
# classify_kidpr2008() adds the analogue of the type as a note (a line in
# warnings) and a field (qualifiers$analogue_1977), never changing the
# classification (Hugo's decision Q1, 2026-11-02): nothing when the type is
# unassigned, and the note says so when the table says the soils were not
# distinguished in 1977 or has no row for the type.
# =============================================================================

#' The correlation table of chapter 6 (inst/rules/kidpr2008/correlation_1977.yaml),
#' cached.
#' @noRd
.kidpr_c77_rules <- function() {
  if (is.null(.kidpr_cache$c77_rules)) {
    f <- system.file("rules", "kidpr2008", "correlation_1977.yaml", package = "soilKey")
    if (!nzchar(f)) rlang::abort("The 1977 correlation of the Russian classification is not installed.")
    .kidpr_cache$c77_rules <- yaml::read_yaml(f)$rows
  }
  .kidpr_cache$c77_rules
}

#' The rows of chapter 6 that stand for a type of 2008.
#' @noRd
.kidpr_c77_rows <- function(type) {
  Filter(function(r) identical(as.character(r$type %||% NA_character_), as.character(type)), .kidpr_c77_rules())
}

# The analogues of a row as a phrase: "subtype torfyanisto-podzolistye
# poverkhnostno-ogleennye, in the type bolotno-podzolistye", with the extent
# in parentheses where the table qualifies it ("(partly)").
.kidpr_c77_phrase <- function(a) {
  lev <- switch(as.character(a$level %||% ""), group = "extra-taxonomic group", literature = "the literature name",
                as.character(a$level %||% ""))
  ext <- switch(as.character(a$extent %||% "full"), approximately = " (approximately)", partly = " (partly)",
                mostly = " (mostly)", "")
  it <- if (!is.null(a$in_type) && !is.na(a$in_type) && nzchar(a$in_type)) sprintf(", in the type %s", a$in_type) else ""
  # what the table adds to the unit (its genus, varieties, erosion ...)
  ql <- if (!is.null(a$qualifier) && !is.na(a$qualifier) && nzchar(a$qualifier)) sprintf(", %s", a$qualifier) else ""
  sprintf("%s %s%s%s%s", lev, a$name, it, ql, ext)
}

# The analogue of one row as a sentence of the note.
.kidpr_c77_sentence <- function(r) {
  an <- r$analogues %||% list()
  if (isFALSE(r$distinguished)) {
    if (!length(an)) return("not distinguished in the 1977 classification")
    return(sprintf("not distinguished in the 1977 classification; formerly within: %s",
                   paste(vapply(an, .kidpr_c77_phrase, ""), collapse = "; ")))
  }
  paste(vapply(an, .kidpr_c77_phrase, ""), collapse = "; ")
}

# The analogue of a row as a field: its page, the name and formula of 2004,
# whether the soils were distinguished, the analogues as a table and the
# English sentence of soilKey's.
.kidpr_c77_field <- function(r) {
  an <- r$analogues %||% list()
  tb <- if (length(an)) data.frame(
    name = vapply(an, function(a) as.character(a$name), ""),
    level = vapply(an, function(a) as.character(a$level %||% NA_character_), ""),
    in_type = vapply(an, function(a) as.character(a$in_type %||% NA_character_), ""),
    extent = vapply(an, function(a) as.character(a$extent %||% NA_character_), ""),
    qualifier = vapply(an, function(a) as.character(a$qualifier %||% NA_character_), ""),
    stringsAsFactors = FALSE)
  else data.frame(name = character(0), level = character(0), in_type = character(0), extent = character(0),
                  qualifier = character(0), stringsAsFactors = FALSE)
  list(row = as.integer(r$row), page = as.integer(r$page), name_2004 = as.character(r$name_2004),
       formula_2004 = as.character(r$formula_2004), distinguished = !isFALSE(r$distinguished),
       analogues = tb, summary = .kidpr_c77_sentence(r), en = as.character(r$en %||% NA_character_))
}

# Add the 1977 analogue of the type to a classify_kidpr2008() result: the
# field qualifiers$analogue_1977 (the type, the rows of chapter 6 that stand
# for it, a status: given, not_distinguished or no_row) and one note in
# warnings. Nothing for a soil whose type is unassigned or a chemozem (CU,
# no types: the 1977 classification had no chemozems, p. 134).
.kidpr_add_1977 <- function(res) {
  lv <- res$levels
  if (is.null(lv) || nrow(lv) < 3L || isTRUE(.kidpr_dv_state$masking)) return(res)
  at <- which(lv$level == "types")
  if (!length(at) || is.na(lv$code[at[1L]])) return(res)
  type <- lv$code[at[1L]]
  if (startsWith(type, "[CU:")) return(res)
  rows <- .kidpr_c77_rows(type)
  q <- res$qualifiers %||% list()
  if (!length(rows)) {
    q$analogue_1977 <- list(type = type, status = "no_row", rows = list())
    res$qualifiers <- q
    res$warnings <- c(res$warnings, sprintf(
      "1977 analogue: chapter 6 of the Field Guide has no row for the type %s (a note only, never used to classify).", type))
    return(res)
  }
  fields <- lapply(rows, .kidpr_c77_field)
  dist <- vapply(fields, `[[`, NA, "distinguished")
  status <- if (all(!dist)) "not_distinguished" else "given"
  q$analogue_1977 <- list(type = type, status = status, rows = fields)
  res$qualifiers <- q
  pages <- unique(vapply(fields, `[[`, 0L, "page"))
  body <- paste(vapply(fields, function(f) if (length(fields) > 1L) sprintf("%s (%s): %s", f$name_2004, f$formula_2004, f$summary)
                                           else f$summary, ""), collapse = " | ")
  res$warnings <- c(res$warnings, sprintf(
    "1977 analogue (Field Guide, chapter 6, p. %s; a note only, never used to classify): %s.",
    paste(pages, collapse = ", "), body))
  res
}
