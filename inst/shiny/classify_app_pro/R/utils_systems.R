# =============================================================================
# soilKey Pro -- the classification systems the Classify tab offers.
#
# WRB 2022, SiBCS and USDA are the three the app has always run, ticked by
# default. The national systems (the registry entries that classify_all() does
# not run by default: Canada, Australia, New Zealand, Hungary) are offered
# beside them, run only when ticked, and marked experimental while their
# registry status says so. The list, the editions and the status come from
# soilKey::soilkey_systems(), so a system added to the registry appears here,
# under its registry name until the catalogue gives it a label of its own.
#
# The results of the national keys carry the class at each level
# ($levels), the classes left undecided before the assigned one
# ($provisional_because) and, for Hungary, the subtype and variety properties
# ($qualifiers$status). The package's report reads them with the same helpers
# (R/report-national.R), so the app and the report say the same thing.
# =============================================================================

# The three systems the app has always run, as the Classify tab names them.
.SK_DEFAULT_SYSTEMS <- c("wrb2022", "sibcs", "usda")

# The registry rows of the national systems: id, name, edition, status.
.sk_national_systems <- function() {
  s <- tryCatch(soilKey::soilkey_systems(), error = function(e) NULL)
  cols <- c("id", "name", "edition", "status")
  if (is.null(s) || !nrow(s) || !all(c(cols, "default") %in% names(s)))
    return(data.frame(id = character(0), name = character(0),
                      edition = character(0), status = character(0),
                      stringsAsFactors = FALSE))
  s <- s[!s$default, cols, drop = FALSE]
  rownames(s) <- NULL
  s
}

# The slot of classify_all()'s result that holds a system's result.
.sk_result_slot <- function(id) {
  vapply(id, function(x) switch(x, wrb2022 = , wrb = "wrb", sibcs = , sibcs5 = "sibcs",
                                usda = "usda", x),
         character(1), USE.NAMES = FALSE)
}

# Ids or aliases (from a session file) as the Classify tab's own ids, in the
# tab's order; unknown names are dropped.
.sk_app_system_ids <- function(x) {
  x <- as.character(unlist(x))
  x <- x[!is.na(x) & nzchar(x)]
  if (!length(x)) return(character(0))
  ids <- unique(unlist(lapply(x, function(s)
    tryCatch(soilKey:::.system_resolve(s), error = function(e) character(0)))))
  ids[ids == "sibcs5"] <- "sibcs"
  known <- c(.SK_DEFAULT_SYSTEMS, .sk_national_systems()$id)
  known[known %in% ids]
}

# The systems ticked, for the session file: a top-level "classify" key (the
# pedon schema allows additional properties), an array even with one system.
.sk_session_classify <- function(systems) {
  if (is.null(systems)) return(NULL)
  list(systems = I(as.character(systems)))
}

# The systems a session file was saved with; NULL when it has none (a file
# saved by an earlier version: the ticks are then left as they are).
.sk_session_systems <- function(parsed) {
  cl <- if (is.list(parsed)) parsed$classify
  if (!is.list(cl) || !"systems" %in% names(cl)) return(NULL)
  as.character(unlist(cl$systems))
}

# A system's label in the session's language: the catalogue's
# "classify.sys_<id>", else the registry's name.
.sk_system_label <- function(id, registry_name = NULL) {
  key <- paste0("classify.sys_", id)
  v <- i18n(key)
  if (identical(v, key)) registry_name %||% id else v
}

# A national system's edition, from the registry. In Portuguese the
# catalogue's translation is shown while its English text is still the
# registry's, so an edition changed in the registry never shows a stale
# translation (test-app-national-systems.R checks the two agree).
.sk_system_edition <- function(id, edition) {
  key <- paste0("classify.edition_", id)
  if (identical(i18n(key, lang = "en"), edition)) i18n(key) else edition
}

# Labels of result slots, for the trace selector and the missing-data list.
.sk_slot_labels <- function(slots) {
  nat <- .sk_national_systems()
  fixed <- c(wrb = "WRB 2022", sibcs = "SiBCS 5", usda = "USDA ST 13")
  vapply(slots, function(s) {
    if (s %in% names(fixed)) return(fixed[[s]])
    i <- match(s, nat$id)
    .sk_system_label(s, if (is.na(i)) NULL else nat$name[i])
  }, character(1), USE.NAMES = FALSE)
}

# The checkbox choices of the national systems: the label over the edition.
.sk_national_choices <- function() {
  nat <- .sk_national_systems()
  list(
    names = lapply(seq_len(nrow(nat)), function(i) shiny::tags$span(
      class = "sk-sys-choice",
      shiny::tags$span(class = "sk-sys-name", .sk_system_label(nat$id[i], nat$name[i])),
      shiny::tags$span(class = "sk-sys-edition",
                       .sk_system_edition(nat$id[i], nat$edition[i])))),
    values = nat$id)
}

# The pill that marks an experimental system.
sk_experimental_badge <- function() {
  shiny::tags$span(class = "badge sk-exp-badge", i18n("classify.experimental"))
}

# A level id ("great_groups", "subtype") in the session's language.
.sk_level_label <- function(level) {
  vapply(level, function(l) {
    key <- paste0("classify.level_", l)
    v <- i18n(key)
    if (identical(v, key)) gsub("_", " ", l, fixed = TRUE) else v
  }, character(1), USE.NAMES = FALSE)
}

# The class at each level, as rows of a definition list: the class name with
# its code (and the English gloss where the system gives one); the Hungarian
# subtype and variety properties are listed together on one row each.
.sk_levels_rows <- function(lv) {
  if (!is.data.frame(lv) || !nrow(lv)) return(NULL)
  has_en <- "name_en" %in% names(lv)
  props <- lv$level %in% c("subtype", "variety")
  rows <- lapply(which(!props), function(i) shiny::tagList(
    shiny::tags$dt(class = "col-5", .sk_level_label(lv$level[i])),
    shiny::tags$dd(class = "col-7", lv$name[i],
                   shiny::tags$code(class = "ms-1 small", lv$code[i]),
                   if (has_en && !is.na(lv$name_en[i]))
                     shiny::div(class = "text-muted small", lv$name_en[i]))))
  for (lev in c("subtype", "variety")) {
    w <- which(lv$level == lev)
    if (!length(w)) next
    rows[[length(rows) + 1L]] <- shiny::tagList(
      shiny::tags$dt(class = "col-5", i18n(paste0("classify.levels_", lev))),
      shiny::tags$dd(class = "col-7", paste(lv$name[w], collapse = ", ")))
  }
  rows
}

# The classes left undecided before the assigned one, as a note: one line
# that says how many, opening to the list with what each would need (a list
# of a dozen classes would otherwise make the card a page long).
.sk_provisional_note <- function(res) {
  pd <- tryCatch(soilKey:::.provisional_detail(res), error = function(e) NULL)
  if (is.null(pd) || !nrow(pd)) return(NULL)
  shiny::tags$details(
    class = "sk-provisional small mt-2",
    shiny::tags$summary(
      shiny::icon("triangle-exclamation"), " ",
      shiny::strong(i18n("classify.provisional")), " ",
      i18n("classify.provisional_summary", nrow(pd))),
    shiny::p(class = "mt-1 mb-1", i18n("classify.provisional_intro")),
    shiny::tags$ul(lapply(seq_len(nrow(pd)), function(i) shiny::tags$li(
      .sk_level_label(pd$level[i]), ": ", pd$name[i],
      shiny::tags$code(class = "ms-1", pd$code[i]),
      if (nzchar(pd$missing[i])) shiny::tags$span(
        class = "sk-needs ms-1", sprintf(i18n("classify.needs"), pd$missing[i]))))))
}

# A card for the result of a national system: label and evidence grade; the
# name; the class at each level; the edition with the experimental pill; the
# provisional note.
pro_national_card <- function(res, label, edition = NULL, experimental = TRUE) {
  head <- shiny::div(
    class = "d-flex justify-content-between align-items-center gap-2",
    shiny::strong(label),
    if (!is.null(res) && !inherits(res, "error")) pro_grade_badge(res$evidence_grade))
  meta <- if (!is.null(edition) || experimental)
    shiny::div(class = "sk-sys-meta text-muted small mt-2",
               edition, if (experimental) sk_experimental_badge())
  if (is.null(res) || inherits(res, "error")) {
    msg <- if (inherits(res, "error")) conditionMessage(res) else i18n("ui.not_run")
    return(bslib::card(
      class = "sk-national-card",
      bslib::card_header(head),
      bslib::card_body(shiny::tags$em(class = "text-danger",
                                      paste(i18n("ui.no_result"), msg)), meta)))
  }
  nm <- res$name
  unnamed <- is.null(nm) || !length(nm) || is.na(nm[1]) || !nzchar(nm[1])
  bslib::card(
    class = "sk-national-card",
    bslib::card_header(head),
    bslib::card_body(
      shiny::tags$h5(if (unnamed) i18n("classify.not_classified") else nm),
      if (is.na(res$evidence_grade %||% NA))
        shiny::div(class = "alert alert-warning small py-2 mb-2",
                   i18n("ui.no_measured_data_note")),
      shiny::tags$dl(class = "row mb-0 small", .sk_levels_rows(res$levels)),
      meta,
      .sk_provisional_note(res)))
}

# The Hungarian properties table for display: the properties the name carries
# and those the key could not decide, with what would decide them. NULL when
# the result has none.
.sk_hscs_props_display <- function(res) {
  hp <- tryCatch(soilKey:::.hscs_property_table(res), error = function(e) NULL)
  if (is.null(hp) || !nrow(hp)) return(NULL)
  data.frame(
    level    = .sk_level_label(hp$level),
    property = hp$label,
    gloss    = ifelse(is.na(hp$name_en), "", hp$name_en),
    status   = ifelse(hp$status == "named", i18n("classify.prop_named"),
                      i18n("classify.prop_undecided")),
    missing  = hp$missing,
    stringsAsFactors = FALSE)
}
