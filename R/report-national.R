# =============================================================================
# The national systems in the report (and in the Pro app).
#
# The results of the ordered national keys (classify_hierarchical(): Canada,
# Australia, New Zealand, Hungary) carry what the WRB, SiBCS and USDA results
# do not: the class assigned at each level ($levels), the classes left
# undecided before the assigned one ($provisional_because) and, for Hungary,
# every subtype and variety property of the type's table with its verdict
# ($qualifiers$status). The helpers below read those fields for the HTML and
# PDF reports; the Pro app calls the same ones, so the two say the same thing.
# A WRB, SiBCS or USDA result has none of the fields, and its report is
# byte-identical to before.
# =============================================================================

# The systems a report runs on a PedonRecord: registry ids in registry order.
# NULL gives the three that a report has always run.
.report_system_ids <- function(systems = NULL) {
  if (is.null(systems)) systems <- c("wrb2022", "sibcs", "usda")
  ids <- .system_resolve(as.character(systems), several.ok = TRUE)
  .system_ids()[.system_ids() %in% ids]
}

# Run one system other than WRB, SiBCS and USDA on a pedon for the report, as
# classify_all() calls it: on_missing = "silent" (the report lists the missing
# data itself) and the options its registry entry takes from classify_all()
# (the Australian family follows include_family).
.report_classify_other <- function(id, pedon, include_family = FALSE,
                                   specifiers = FALSE) {
  sys  <- .system_get(id)
  fn   <- get(sys$classifier, envir = asNamespace("soilKey"), mode = "function")
  vals <- list(include_family = include_family, specifiers = specifiers,
               include_familia = TRUE)
  opts <- unlist(sys$classify_all_options)
  args <- c(list(pedon, on_missing = "silent"),
            stats::setNames(lapply(unname(opts), function(v) vals[[v]]), names(opts)))
  do.call(fn, args)
}

# Is the result's system marked experimental in the registry?
.result_is_experimental <- function(res) {
  id <- res$system_id
  if (is.null(id) || length(id) != 1L || is.na(id)) return(FALSE)
  e <- tryCatch(.system_get(id), error = function(e) NULL)
  identical(e$status, "experimental")
}

# A level id ("great_groups", "subtype") as a label of the report language.
.report_level_label <- function(level) {
  vapply(level, function(l) {
    key <- paste0("report.level_", l)
    v <- .report_msg(key)
    if (identical(v, key)) gsub("_", " ", l, fixed = TRUE) else v
  }, character(1), USE.NAMES = FALSE)
}

# The classes left undecided before the assigned one, one row each: the level,
# the code, the class name (from the trace) and the attributes it lacked.
# Entries read "<level>:<code>", or "<level>:<class>/<code>" for a list taken
# from another class of the top level (traced as "<level>, as a <class>").
# NULL when there are none.
.provisional_detail <- function(res) {
  pb <- as.character(res$provisional_because %||% character(0))
  if (!length(pb)) return(NULL)
  tt <- tryCatch(.flatten_key_trace(res$trace), error = function(e) NULL)
  level <- sub(":.*$", "", pb)
  rest  <- sub("^[^:]*:", "", pb)
  code  <- sub("^.*/", "", rest)
  rows <- lapply(seq_along(pb), function(i) {
    hit <- if (!is.null(tt) && nrow(tt))
      which(tt$code == code[i] &
              (tt$phase == level[i] | startsWith(tt$phase, paste0(level[i], ", as a "))))
    else integer(0)
    data.frame(level = level[i], code = code[i],
               name = if (length(hit)) tt$name[hit[1]] else "",
               missing = if (length(hit)) tt$missing[hit[1]] else "",
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# The Hungarian subtype and variety properties to show: those the name
# carries and those the key could not decide, with the attributes that would
# decide them. NULL for a result without $qualifiers$status.
.hscs_property_table <- function(res) {
  st <- res$qualifiers$status
  if (!is.data.frame(st) || !nrow(st)) return(NULL)
  keep <- (st$named %in% TRUE) | is.na(st$passed)
  st <- st[keep, , drop = FALSE]
  if (!nrow(st)) return(NULL)
  cat <- tryCatch(.hscs_properties_rules()$properties, error = function(e) list())
  cname <- vapply(st$id, function(i) cat[[i]]$name %||% i, character(1), USE.NAMES = FALSE)
  cen   <- vapply(st$id, function(i) cat[[i]]$en %||% NA_character_, character(1),
                  USE.NAMES = FALSE)
  data.frame(
    level   = st$level,
    id      = st$id,
    label   = ifelse(st$named %in% TRUE & !is.na(st$label), st$label, cname),
    name_en = cen,
    status  = ifelse(st$named %in% TRUE, "named", "undecided"),
    missing = ifelse(st$named %in% TRUE, "", st$missing %||% ""),
    stringsAsFactors = FALSE)
}


# ---- HTML ---------------------------------------------------------------------

# The sections of a national result's card: the experimental note, the class
# at each level, the provisional classes, the Hungarian properties. "" for a
# result that has none of them (WRB, SiBCS, USDA).
.html_national_sections <- function(res) {
  out <- character(0)
  if (.result_is_experimental(res))
    out <- c(out, sprintf('<p class="muted">%s</p>\n',
                          .html_escape(.report_msg("report.experimental_note"))))
  lv <- res$levels
  if (is.data.frame(lv) && nrow(lv)) {
    has_en <- "name_en" %in% names(lv)
    rows <- vapply(seq_len(nrow(lv)), function(i)
      sprintf("<tr><td>%s</td><td><code>%s</code></td><td>%s</td>%s</tr>",
              .html_escape(.report_level_label(lv$level[i])),
              .html_escape(lv$code[i]), .html_escape(lv$name[i]),
              if (has_en) sprintf("<td>%s</td>", .html_escape(lv$name_en[i])) else ""),
      character(1))
    out <- c(out, sprintf(
      "<h3>%s</h3><table><thead><tr><th>%s</th><th>%s</th><th>%s</th>%s</tr></thead><tbody>%s</tbody></table>\n",
      .html_escape(.report_msg("report.levels")),
      .html_escape(.report_msg("report.level")),
      .html_escape(.report_msg("report.code")),
      .html_escape(.report_msg("report.name")),
      if (has_en) sprintf("<th>%s</th>", .html_escape(.report_msg("report.name_en"))) else "",
      paste(rows, collapse = "\n")))
  }
  pd <- .provisional_detail(res)
  if (!is.null(pd)) {
    items <- vapply(seq_len(nrow(pd)), function(i)
      sprintf("<li>%s: <code>%s</code> %s%s</li>",
              .html_escape(.report_level_label(pd$level[i])),
              .html_escape(pd$code[i]),
              .html_escape(pd$name[i]),
              if (nzchar(pd$missing[i]))
                sprintf(' <span class="muted">(%s)</span>',
                        .html_escape(sprintf(.report_msg("report.needs"), pd$missing[i])))
              else ""),
      character(1))
    out <- c(out, sprintf("<h3>%s</h3><p>%s</p><ul>%s</ul>\n",
                          .html_escape(.report_msg("report.provisional")),
                          .html_escape(.report_msg("report.provisional_intro")),
                          paste(items, collapse = "\n")))
  }
  hp <- .hscs_property_table(res)
  if (!is.null(hp)) {
    rows <- vapply(seq_len(nrow(hp)), function(i)
      sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
              .html_escape(.report_level_label(hp$level[i])),
              .html_escape(hp$label[i]), .html_escape(hp$name_en[i]),
              .html_escape(.report_msg(paste0("report.prop_", hp$status[i]))),
              .html_escape(hp$missing[i])),
      character(1))
    out <- c(out, sprintf(
      "<h3>%s</h3><p class=\"muted\">%s</p><table><thead><tr><th>%s</th><th>%s</th><th>%s</th><th>%s</th><th>%s</th></tr></thead><tbody>%s</tbody></table>\n",
      .html_escape(.report_msg("report.hscs_properties")),
      .html_escape(.report_msg("report.hscs_properties_desc")),
      .html_escape(.report_msg("report.level")),
      .html_escape(.report_msg("report.property")),
      .html_escape(.report_msg("report.name_en")),
      .html_escape(.report_msg("report.status")),
      .html_escape(.report_msg("report.would_decide")),
      paste(rows, collapse = "\n")))
  }
  paste(out, collapse = "")
}


# ---- PDF (Rmd) ----------------------------------------------------------------

# The same sections as Markdown for the PDF report; "" for WRB, SiBCS, USDA.
.rmd_national_sections <- function(res) {
  md_cell <- function(x) gsub("|", "/", as.character(x %||% ""), fixed = TRUE)
  out <- character(0)
  if (.result_is_experimental(res))
    out <- c(out, sprintf("_%s_\n\n", .report_msg("report.experimental_note")))
  lv <- res$levels
  if (is.data.frame(lv) && nrow(lv)) {
    has_en <- "name_en" %in% names(lv)
    rows <- vapply(seq_len(nrow(lv)), function(i)
      sprintf("| %s | %s | %s |%s", md_cell(.report_level_label(lv$level[i])),
              md_cell(lv$code[i]), md_cell(lv$name[i]),
              if (has_en) sprintf(" %s |", md_cell(lv$name_en[i])) else ""),
      character(1))
    out <- c(out, sprintf("### %s\n\n", .report_msg("report.levels")),
             sprintf("| %s | %s | %s |%s\n", .report_msg("report.level"),
                     .report_msg("report.code"), .report_msg("report.name"),
                     if (has_en) sprintf(" %s |", .report_msg("report.name_en")) else ""),
             if (has_en) "|---|---|---|---|\n" else "|---|---|---|\n",
             paste(rows, collapse = "\n"), "\n\n")
  }
  pd <- .provisional_detail(res)
  if (!is.null(pd)) {
    items <- vapply(seq_len(nrow(pd)), function(i)
      sprintf("- %s: `%s` %s%s", .report_level_label(pd$level[i]), pd$code[i],
              pd$name[i],
              if (nzchar(pd$missing[i]))
                sprintf(" (%s)", sprintf(.report_msg("report.needs"), pd$missing[i]))
              else ""),
      character(1))
    out <- c(out, sprintf("### %s\n\n%s\n\n", .report_msg("report.provisional"),
                          .report_msg("report.provisional_intro")),
             paste(items, collapse = "\n"), "\n\n")
  }
  hp <- .hscs_property_table(res)
  if (!is.null(hp)) {
    rows <- vapply(seq_len(nrow(hp)), function(i)
      sprintf("| %s | %s | %s | %s | %s |", md_cell(.report_level_label(hp$level[i])),
              md_cell(hp$label[i]), md_cell(hp$name_en[i]),
              md_cell(.report_msg(paste0("report.prop_", hp$status[i]))),
              md_cell(hp$missing[i])),
      character(1))
    out <- c(out, sprintf("### %s\n\n%s\n\n", .report_msg("report.hscs_properties"),
                          .report_msg("report.hscs_properties_desc")),
             sprintf("| %s | %s | %s | %s | %s |\n", .report_msg("report.level"),
                     .report_msg("report.property"), .report_msg("report.name_en"),
                     .report_msg("report.status"), .report_msg("report.would_decide")),
             "|---|---|---|---|---|\n", paste(rows, collapse = "\n"), "\n\n")
  }
  paste(out, collapse = "")
}
