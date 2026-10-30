# =============================================================================
# v0.9.316 -- the subtype and variety properties of the Hungarian diagnostic
# soil classification (HSCS 2024; Micheli et al. 2024, second approximation):
# 8.5 (altipus tulajdonsagok, p. 29-33), 8.6 (valtozati tulajdonsagok, p.
# 34-36) and the modifiers of 8.7 (pontosito jelzok, p. 37). Each type's table
# (section 7) lists them; inst/rules/hscs2024/properties.yaml holds the tables,
# the catalogue and the degree rows.
#
# Naming (p. 7): the subtype properties before the type name, the first with a
# capital letter; the variety properties after it in parentheses; in the order
# of the table, separated by commas. Every listed property whose definition
# the soil meets is named. Hugo's decisions of 2026-10-30: a row of degrees of
# one property names only the strongest that holds (none when a stronger one
# is undecided); a property defined by a layer takes the depth modifier of
# 8.7 from where its layer starts; intensity words only where the text defines
# them.
#
# Each property is a function .hscs_p_<id>(x), x = .hscs_horizons(pedon),
# returning .hscs_prop(): passed (TRUE / FALSE / NA); top, the depth (cm from
# the soil surface) where the qualifying layer starts, for a layer property;
# variant, a defined intensity word ("eros" / "gyenge", see .HSCS_VARIANT);
# label, a name that replaces the catalogue's (the texture class, the buried
# soil, the cementing agent); missing, the attributes that would decide it.
# =============================================================================

.hscs_properties_rules <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      f <- system.file("rules", "hscs2024", "properties.yaml", package = "soilKey")
      cache <<- yaml::read_yaml(f)
    }
    cache
  }
})

# The intensity words of 8.5 and 8.6, ASCII keys for the code.
.HSCS_VARIANT <- c(eros = "er\u0151sen", gyenge = "gyeng\u00e9n")

# The depth modifiers of 8.7 (p. 37).
.HSCS_DEPTH_WORD <- c(surface = "felsz\u00ednt\u0151l", shallow = "sek\u00e9lyen",
                      middle = "k\u00f6z\u00e9pm\u00e9lyen", deep = "m\u00e9lyben")

#' The result of one HSCS subtype or variety property.
#' @noRd
.hscs_prop <- function(passed, top = NA_real_, variant = NULL, label = NULL,
                       layers = integer(0), missing = character(0), note = NULL) {
  list(passed = as.logical(passed)[1], top = as.numeric(top)[1],
       variant = variant, label = label, layers = as.integer(layers),
       missing = unique(as.character(missing)), note = note)
}

# Top of the first run of adjacent rows (depth order) that surely hold `ok`,
# start above `top_max` and are `min_th` cm or more thick: the depth for the
# modifier of a layer property. NA when no such run is sure.
.hscs_run_top <- function(x, ok, top_max = Inf, min_th = 0) {
  h <- x$h; r <- x$ord
  s_top <- NA; s_th <- 0; prev <- NA
  for (i in r) {
    t <- h$top_cm[i]; th <- x$thick[i]
    if (!is.na(prev) && abs(t - prev) > 0.5) { s_top <- NA; s_th <- 0 }
    if (ok[i] %in% TRUE) {
      if (is.na(s_top)) s_top <- t
      s_th <- s_th + (if (is.na(th)) 0 else th)
      if (s_top < top_max - 0.25 && s_th >= min_th - 1e-9) return(s_top)
    } else { s_top <- NA; s_th <- 0 }
    prev <- h$bottom_cm[i]
  }
  NA_real_
}

# The depth modifier for a layer starting at `top` (cm), from the soil
# surface: felszintol at the surface, sekelyen within 50 cm, kozepmelyen
# 50-100 cm, melyben below 100 cm.
.hscs_depth_word <- function(x, top) {
  if (is.na(top)) return(NA_character_)
  s <- if (is.na(x$surface)) 0 else x$surface
  d <- top - s
  if (d <= 0.5) .HSCS_DEPTH_WORD[["surface"]]
  else if (d < 50) .HSCS_DEPTH_WORD[["shallow"]]
  else if (d < 100) .HSCS_DEPTH_WORD[["middle"]]
  else .HSCS_DEPTH_WORD[["deep"]]
}

# Evaluate one property: its function, or undecided when soilKey does not read
# it; an error is reported as undecided with its message.
.hscs_eval_property <- function(id, x) {
  fn <- get0(paste0(".hscs_p_", id), envir = asNamespace("soilKey"), inherits = FALSE)
  if (is.null(fn)) return(.hscs_prop(NA, note = "not read by soilKey"))
  tryCatch(fn(x), error = function(e) .hscs_prop(NA, note = paste("error:", conditionMessage(e))))
}

# The label of a property as it enters the name: depth modifier, intensity
# word, then the name (or the label the function gives).
.hscs_label <- function(id, p, cat, x) {
  nm <- p$label %||% cat$name
  if (!is.null(p$variant) && p$variant %in% names(.HSCS_VARIANT))
    nm <- paste(.HSCS_VARIANT[[p$variant]], nm)
  if (identical(cat$kind, "layer")) {
    w <- .hscs_depth_word(x, p$top)
    if (!is.na(w)) nm <- paste(w, nm)
  }
  nm
}

#' The subtype and variety properties of a type, by its table.
#'
#' @return A list: `subtypes` and `varieties` (labels in the order of the
#'   table), and `status`, a data.frame of every property evaluated (level,
#'   row, id, passed, top, label, missing, note, named).
#' @noRd
.hscs_type_properties <- function(pedon, type_code) {
  rules <- .hscs_properties_rules()
  tab <- rules$types[[type_code]]
  if (is.null(tab)) return(NULL)
  x <- .hscs_horizons(pedon)
  cat <- rules$properties
  rows <- list()
  named <- list(subtypes = character(0), varieties = character(0))
  for (level in c("subtypes", "varieties")) {
    for (k in seq_along(tab[[level]])) {
      item <- tab[[level]][[k]]
      ids <- if (is.list(item) && !is.null(item$one_of)) unlist(item$one_of) else as.character(item)
      degree <- is.list(item) && !is.null(item$one_of)
      taken <- FALSE; blocked <- FALSE
      for (id in ids) {
        ci <- cat[[id]]
        p <- if (!is.null(ci$not_in) && type_code %in% unlist(ci$not_in))
               .hscs_prop(FALSE, note = sprintf("not in this type (p. %s)", ci$page))
             else .hscs_eval_property(id, x)
        lab <- if (isTRUE(p$passed)) .hscs_label(id, p, ci, x) else NA_character_
        use <- isTRUE(p$passed) && !taken && !blocked
        if (degree) {
          # a stronger degree that is undecided leaves the row unnamed
          if (!taken && is.na(p$passed)) blocked <- TRUE
          if (use) taken <- TRUE
        }
        if (use) named[[level]] <- c(named[[level]], lab)
        rows[[length(rows) + 1L]] <- data.frame(
          level = c(subtypes = "subtype", varieties = "variety")[[level]], row = k, id = id, passed = p$passed, top = p$top,
          label = lab, missing = paste(p$missing, collapse = ", "),
          note = p$note %||% NA_character_, named = use, stringsAsFactors = FALSE)
      }
    }
  }
  list(subtypes = named$subtypes, varieties = named$varieties,
       status = do.call(rbind, rows))
}

# The full name (p. 7): "Savanyu, humuszszegeny Agyagbemosodasos talaj
# (sekelyen kozettormelekes)", with the accents.
.hscs_full_name <- function(type_name, subtypes, varieties) {
  nm <- type_name
  if (length(subtypes)) {
    s <- paste(subtypes, collapse = ", ")
    s <- paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
    nm <- paste(s, nm)
  }
  if (length(varieties)) nm <- sprintf("%s (%s)", nm, paste(varieties, collapse = ", "))
  nm
}

# Add the properties of the assigned type to a classify_hscs2024() result.
.hscs_add_properties <- function(res, pedon) {
  lv <- res$levels
  if (is.null(lv) || !nrow(lv) || !inherits(pedon, "PedonRecord")) return(res)
  code <- lv$code[lv$level == "types"][1]
  if (is.na(code)) return(res)
  pr <- .hscs_type_properties(pedon, code)
  if (is.null(pr)) return(res)
  type_name <- lv$name[lv$level == "types"][1]
  cat <- .hscs_properties_rules()$properties
  st <- pr$status[pr$status$named, , drop = FALSE]
  if (nrow(st)) {
    add <- data.frame(level = st$level, code = st$id, name = st$label,
                      name_en = vapply(st$id, function(i) cat[[i]]$en %||% NA_character_, ""),
                      stringsAsFactors = FALSE)
    res$levels <- rbind(lv, add)
  }
  res$name <- .hscs_full_name(type_name, pr$subtypes, pr$varieties)
  res$qualifiers <- list(subtypes = pr$subtypes, varieties = pr$varieties, status = pr$status)
  res
}
