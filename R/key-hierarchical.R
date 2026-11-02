# =============================================================================
# v0.9.265 -- the generic classifier of ordered keys.
#
# Most national systems (Canada, Australia, New Zealand, Japan, Hungary,
# China) are keys of the USDA kind: at each level the first class whose tests
# pass is assigned, and the next level is the list of classes under it. Their
# classifiers are thin wrappers around classify_hierarchical(), which walks
# the levels named in the system's registry entry.
#
# Two rules differ from the three older keys, on purpose:
#   * a class is assigned only when its tests pass (a `default: true` entry
#     passes by definition). When nothing passes, the level stays unassigned
#     and the name stops at the level above, instead of falling back to the
#     last class of the list (run_taxa_list() returns it anyway; the old keys
#     demote it at some levels only).
#   * a class that could not be decided for lack of data, tested before the
#     one assigned, makes the assignment provisional: the result lists it in
#     provisional_because, as the Australian key does with its confidence
#     levels.
# WRB, SiBCS and USDA keep their own classifiers.
# =============================================================================


# Run one level: the first class whose tests pass, its trace, and the classes
# left undecided before it (all of them when nothing passed), counted by
# position as the key is read. Unlike run_taxa_list(), no class is returned
# when none passes. A class without a code stops with an error naming it
# (.validate_rules() reports the same at test time).
.run_level <- function(pedon, taxa, level) {
  trace <- list()
  undecided <- character(0)
  for (i in seq_along(taxa)) {
    taxon <- taxa[[i]]
    code <- as.character(taxon$code %||% NA_character_)
    if (length(code) != 1L || is.na(code) || !nzchar(code)) {
      rlang::abort(sprintf("Class %d of the %s list has no code.", i, level))
    }
    result <- evaluate_rsg_tests(pedon, taxon$tests)
    trace[[code]] <- list(
      code     = code,
      name     = taxon$name,
      passed   = result$passed,
      evidence = result$evidence,
      missing  = result$missing %||% character(0),
      notes    = result$notes
    )
    if (isTRUE(result$passed)) {
      taxon$code <- code
      return(list(assigned = taxon, trace = trace, undecided = undecided))
    }
    if (is.na(result$passed)) undecided <- c(undecided, code)
  }
  list(assigned = NULL, trace = trace, undecided = undecided)
}


# The list of classes under a path: the full path ("CH-AA-BG"), the parent
# code ("BG"), then wildcard keys from the most specific ("CH-AA-*", "CH-*").
.level_list <- function(lists, path, parent_code) {
  keys <- c(paste(path, collapse = "-"), parent_code,
            if (length(path) > 1L) vapply(rev(seq_len(length(path) - 1L)),
                                          function(k) paste(c(path[seq_len(k)], "*"), collapse = "-"), ""))
  for (k in keys) if (!is.null(lists[[k]])) return(lists[[k]])
  NULL
}


# The list at level `li` taken from another class of the top level (a
# "<level>_from" entry of a level file: a Rudosolic Hydrosol takes the list of
# the Rudosol suborder it keys to, apart from wetness). `spec` is the code of
# that class, or a mapping with `class` and `site`, site fields set for the
# walk only, on a copy of the pedon. The soil is keyed under that class as the
# main loop would key it (a level with no list is skipped when the next one
# has a list named by the exact path), but only as deep as the choice of list
# needs: the walk stops one level above the list when no list below the path
# is named after the classes that follow ("RU-CY": all its great groups take
# "RU-CY-*", so they are not keyed), and anywhere when there is no list below
# it at all ("RU-EL"). A class left undecided before the one assigned counts only
# when it could lead to another list. Returns the list (empty for none), the
# trace of the walk as phases named "<level>, as a <class>", the undecided
# classes as "<class>/<code>", and a note when nothing passed and nothing was
# undecided.
.key_from <- function(pedon, rules, levels, li, spec) {
  from <- as.character(if (is.list(spec)) spec$class else spec)
  site <- if (is.list(spec)) spec$site else NULL
  if (length(site)) {
    if (inherits(pedon, "R6")) pedon <- pedon$clone(deep = TRUE)
    for (k in names(site)) pedon$site[[k]] <- site[[k]]
  }
  lists <- rules[[levels[li]]]
  nm <- names(lists) %||% character(0)
  bare <- any(!grepl("-", nm, fixed = TRUE))
  top <- Find(function(t) identical(as.character(t$code), from), rules[[levels[1L]]])
  top_name <- as.character(top$name %||% from)
  # the list the main loop finds at level li under a complete path
  at <- function(path) .level_list(lists, path, path[length(path)]) %||% list()
  # the next level to key under `path` from level lj, skipping a level with no
  # list as the main loop does; li when the path is complete, NA when the main
  # loop would stop there (no list)
  next_lj <- function(path, lj) {
    while (lj < li) {
      if (length(.level_list(rules[[levels[lj]]], path, path[length(path)]))) return(lj)
      if (length(rules[[levels[lj + 1L]]][[paste(path, collapse = "-")]]) == 0L) return(NA_integer_)
      lj <- lj + 1L
    }
    li
  }
  # the list at level li under `path` when it no longer depends on the classes
  # that follow (read through a code no list names), else NULL: a list below
  # the path is named after them, or lists are named by bare codes
  resolved <- function(path, lj) {
    k <- next_lj(path, lj)
    if (is.na(k)) return(list(list()))
    if (k == li) return(list(at(path)))
    key <- paste(path, collapse = "-")
    if (bare || any(startsWith(nm, paste0(key, "-")) & nm != paste0(key, "-*"))) return(NULL)
    out <- at(c(path, "\001"))
    # more than one level above, a class below might reach no list at all (the
    # main loop stops at a level with none): walk on, unless there is no list
    if (k < li - 1L && length(out)) return(NULL)
    list(out)
  }
  path <- from
  trace <- list()
  undecided <- character(0)
  done <- function(taxa, note = character(0))
    list(taxa = taxa, trace = trace, undecided = undecided, note = note)
  lj <- 2L
  repeat {
    s <- resolved(path, lj)
    if (!is.null(s)) return(done(s[[1L]]))
    lj <- next_lj(path, lj)
    lvl <- levels[lj]
    r <- .run_level(pedon, .level_list(rules[[lvl]], path, path[length(path)]), lvl)
    trace[[sprintf("%s, as a %s", lvl, top_name)]] <- r$trace
    if (is.null(r$assigned)) {
      if (length(r$undecided)) undecided <- c(undecided, paste0(from, "/", r$undecided))
      note <- if (!length(r$undecided)) sprintf(
        "No class of the %s list of the %s passed, so no %s list was found for this class.",
        lvl, top_name, levels[li]) else character(0)
      return(done(list(), note))
    }
    if (length(r$undecided)) {
      mine <- resolved(c(path, r$assigned$code), lj + 1L)
      keep <- vapply(r$undecided, function(u) {
        other <- resolved(c(path, u), lj + 1L)
        is.null(mine) || is.null(other) || !identical(other[[1L]], mine[[1L]])
      }, NA, USE.NAMES = FALSE)
      if (any(keep)) undecided <- c(undecided, paste0(from, "/", r$undecided[keep]))
    }
    path <- c(path, r$assigned$code)
    lj <- lj + 1L
  }
}

#' Classify a pedon with an ordered (first-match) key
#'
#' The engine behind the classifiers of ordered national keys: it walks the
#' levels of the system's registry entry from the top, assigns at each level
#' the first class whose tests pass, and descends into the classes listed
#' under it. The list under a class is found by the codes of the path to it
#' joined with "-" ("CH-AA", for keys whose codes repeat under different
#' parents), otherwise by the code of the class alone, and otherwise by a
#' list for every class under a part of the path ("CH-AA-*", then "CH-*").
#' A class with an empty list may take one from another class of the top
#' level, named under "<level>_from" in a level file: the soil is then keyed
#' under that class only as deep as the choice of list needs (a Rudosolic
#' Hydrosol takes the list of the Rudosol suborder it keys to), the walk is
#' traced as "<level>, as a <class>", and a class it leaves undecided before
#' the one assigned makes the level provisional when it could lead to another
#' list.
#'
#' @param system Registry id or alias of the system.
#' @param pedon A \code{\link{PedonRecord}}.
#' @param rules Optional pre-loaded rule set (\code{\link{load_rules}}).
#' @param on_missing One of \code{"warn"} (default), \code{"silent"},
#'        \code{"error"}.
#' @param gapfill Opt-in within-pedon gap-fill, as in
#'        \code{\link{classify_usda}}.
#' @return A \code{\link{ClassificationResult}}: \code{name} is the deepest
#'   class assigned (\code{NA} when no class of the top level passed),
#'   \code{levels} the class assigned at each level, \code{trace} one phase
#'   per level tested, and \code{provisional_because} the undecided classes
#'   tested before the assigned ones.
#' @noRd
classify_hierarchical <- function(system, pedon, rules = NULL,
                                  on_missing = c("warn", "silent", "error"),
                                  gapfill = FALSE) {
  on_missing <- match.arg(on_missing)
  id  <- .system_resolve(system)
  sys <- .system_get(id)
  .classify_require_horizons(pedon, sys$classifier)
  rules <- rules %||% load_rules(id)
  pedon <- .classify_apply_gapfill(pedon, gapfill)

  top <- sys$levels[1L]
  if (length(rules[[top]]) == 0L) {
    rlang::abort(sprintf("Rule set has no '%s' list", top))
  }

  trace <- list()
  chosen <- list()
  provisional <- character(0)
  stopped <- character(0)
  parent <- NULL
  path <- character(0)
  levels <- sys$levels
  for (li in seq_along(levels)) {
    lvl <- levels[li]
    # the list under the class above: by the codes of the path to it
    # ("CH-AA", where codes repeat under different parents), else by the
    # code of the parent alone, else by a list for every class under a part
    # of the path ("CH-AA-*", then "CH-*")
    taxa <- if (is.null(parent)) rules[[lvl]] else .level_list(rules[[lvl]], path, parent$code)
    # a class whose list is taken from another class of the top level
    # ("subgroups_from": the Rudosolic Hydrosols take the Rudosol lists)
    from <- if (!is.null(parent) && length(taxa) == 0L) rules[[paste0(lvl, "_from")]][[paste(path, collapse = "-")]]
    if (length(from)) {
      k <- .key_from(pedon, rules, levels, li, from)
      trace <- c(trace, k$trace)
      if (length(k$undecided)) provisional <- c(provisional, paste0(lvl, ":", k$undecided))
      if (length(k$taxa) == 0L) {
        stopped <- c(stopped, k$note)
        break
      }
      taxa <- k$taxa
    }
    if (length(taxa) == 0L) {
      # a class with no list at this level may still have one at the next,
      # named by its exact path (subgroups of a suborder that has no great
      # groups: "Calcareous, Stratic Rudosol"); the level is then skipped
      nxt <- if (!is.null(parent) && li < length(levels)) rules[[levels[li + 1L]]][[paste(path, collapse = "-")]]
      if (length(nxt) == 0L) break
      next
    }
    r <- .run_level(pedon, taxa, lvl)
    # a name written on the class above ("Bleached, {parent}"), in the trace too
    if (!is.null(parent)) {
      r$trace <- lapply(r$trace, function(e) {
        if (grepl("{parent}", e$name %||% "", fixed = TRUE)) e$name <- sub("{parent}", parent$name, e$name, fixed = TRUE)
        e
      })
    }
    trace[[lvl]] <- r$trace
    if (length(r$undecided) > 0L) {
      provisional <- c(provisional, paste0(lvl, ":", r$undecided))
    }
    if (is.null(r$assigned)) {
      # nothing passed below the top and nothing was left undecided: the
      # name stops at the level above, and the user should know why
      if (!is.null(parent) && length(r$undecided) == 0L) {
        stopped <- sprintf(
          "No class of the %s list under %s passed, so the name stops at %s.",
          lvl, parent$code, parent$name)
      }
      break
    }
    # a name may be written on the name of the class above ("Bleached,
    # {parent}", for a list shared by every class under a part of the path)
    if (!is.null(parent) && grepl("{parent}", r$assigned$name %||% "", fixed = TRUE))
      r$assigned$name <- sub("{parent}", parent$name, r$assigned$name, fixed = TRUE)
    # v0.9.323: the native name too ("{parent}" in name_native, the SCSJ
    # subgroups written before their group's Japanese name)
    if (!is.null(parent) && grepl("{parent}", r$assigned$name_native %||% "", fixed = TRUE))
      r$assigned$name_native <- sub("{parent}", parent$name_native %||% "", r$assigned$name_native, fixed = TRUE)
    chosen[[lvl]] <- r$assigned
    parent <- r$assigned
    path <- c(path, r$assigned$code)
  }

  levels <- data.frame(
    level = as.character(names(chosen)),
    code  = vapply(chosen, function(x) x$code, character(1), USE.NAMES = FALSE),
    name  = vapply(chosen, function(x) as.character(x$name %||% NA_character_),
                   character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  # an English gloss, for the systems whose classes carry one (v0.9.305,
  # the Hungarian types)
  if (any(vapply(chosen, function(x) !is.null(x$name_en), NA)))
    levels$name_en <- vapply(chosen, function(x) as.character(x$name_en %||% NA_character_),
                             character(1), USE.NAMES = FALSE)
  # the name in the system's own language, for the systems whose classes
  # carry one (v0.9.321, the Japanese names of the SCSJ)
  if (any(vapply(chosen, function(x) !is.null(x$name_native), NA)))
    levels$name_native <- vapply(chosen, function(x) as.character(x$name_native %||% NA_character_),
                                 character(1), USE.NAMES = FALSE)
  top_class <- chosen[[top]]
  name <- if (nrow(levels) > 0L) levels$name[nrow(levels)] else NA_character_

  top_trace    <- trace[[top]]
  ambiguities  <- find_ambiguities(top_trace,
                                   current = top_class$code %||% NA_character_)
  grade        <- compute_evidence_grade(pedon, top_trace)
  missing_data <- collect_missing_attributes(do.call(c, unname(trace)))

  warnings <- character(0)
  if (is.na(grade)) warnings <- c(warnings, .NO_PROPERTIES_WARNING)
  if (is.null(top_class)) {
    warnings <- c(warnings, sprintf(
      "No class of the %s key passed, so the profile is not classified; %s",
      top, "see $trace and $missing_data."))
  }
  warnings <- c(warnings, stopped)
  if (length(provisional) > 0L) {
    warnings <- c(warnings, sprintf(
      "Provisional: %d class(es) tested before the assigned one could not be decided (%s).",
      length(provisional), paste(provisional, collapse = ", ")))
  }
  if (length(missing_data) > 0L) {
    msg <- sprintf(
      "%d distinct attribute(s) missing across the key trace -- see $missing_data",
      length(missing_data)
    )
    if      (on_missing == "warn")  warnings <- c(warnings, msg)
    else if (on_missing == "error") rlang::abort(msg)
  }

  ClassificationResult$new(
    system         = sys$display$result %||% sys$display$en,
    name           = name,
    rsg_or_order   = top_class$name %||% NA_character_,
    qualifiers     = list(),
    trace          = trace,
    ambiguities    = ambiguities,
    missing_data   = missing_data,
    evidence_grade = grade,
    prior_check    = NULL,
    warnings       = warnings,
    system_id      = id,
    levels         = levels,
    provisional_because = provisional
  )
}
