# =============================================================================
# v0.9.334 -- the identification of diagnostic horizons, layer by layer, and a
# walker of horizon keys.
#
# A system whose types are profile formulas (the Russian Field Guide of 2008)
# first reads every layer as one of its diagnostic horizons, from the notation
# when the profile is written in it, else from the data by the system's key
# to horizons. identify_horizons() gives that reading as one frame, one row
# per layer, the frame match_horizon_sequence() reads; it dispatches to the
# system's .<namespace>_identify(), which no system has yet.
#
# .walk_key() runs such a key: positions are questions answered TRUE, FALSE
# or NA; an undecided question follows every branch and collects the outcomes
# reached: one outcome gives the horizon, several leave it undecided with the
# candidates (Hugo's decision Q4, 2026-11-02).
# =============================================================================

# The columns of the frame and their defaults (list columns hold character
# vectors).
.HORIZON_FRAME_LISTS <- c("overlay", "features", "features_na", "composition",
                          "missing", "candidates")

#' The empty horizon frame of a pedon
#'
#' One row per horizon of the pedon, in the pedon's order, with the columns
#' of \code{\link{identify_horizons}} at their defaults: no horizon read yet.
#' A system's identification starts from it.
#' @noRd
.horizon_frame <- function(pedon) {
  h <- pedon$horizons
  n <- if (is.null(h)) 0L else nrow(h)
  col <- function(k, f) if (n && !is.null(h[[k]])) f(h[[k]]) else rep(f(NA), n)
  out <- data.frame(
    horizon_idx = seq_len(n),
    top_cm = col("top_cm", as.numeric),
    bottom_cm = col("bottom_cm", as.numeric),
    designation = col("designation", as.character),
    code = rep(NA_character_, n),
    small = rep(FALSE, n),
    transitional = rep(FALSE, n),
    buried = rep(0L, n),
    rockflag = rep(NA_character_, n),
    mark = rep(NA_character_, n),
    route = rep(NA_character_, n),
    passed = rep(NA, n),
    evidence = rep(NA_character_, n),
    stringsAsFactors = FALSE)
  for (k in .HORIZON_FRAME_LISTS) out[[k]] <- rep(list(character(0)), n)
  out
}

#' Identify the diagnostic horizons of a pedon
#'
#' Reads every layer of a pedon as one of the diagnostic horizons of a
#' classification system, the reading its profile formulas are matched on
#' (\code{\link{match_horizon_sequence}}). The system's own identification
#' does the work; a system without one stops with an error. In this release
#' no system has one yet: the Russian Field Guide of 2008 will be the first.
#'
#' @param pedon A \code{\link{PedonRecord}}.
#' @param system Registry id or alias of the system
#'   (\code{\link{soilkey_systems}}).
#' @param ... Passed to the system's identification.
#' @return A data frame with one row per layer, in depth order:
#'   \code{horizon_idx} (the row of the pedon's horizons), \code{top_cm},
#'   \code{bottom_cm}, \code{designation}; \code{code} (the diagnostic
#'   horizon, NA when undecided or none); \code{overlay} (horizons laid over
#'   the main one in the same layer, a list column); \code{features} (the
#'   features read on the layer) and \code{features_na} (the features left
#'   undecided), list columns; \code{small} (a small horizon, a feature
#'   rather than a diagnostic horizon); \code{transitional} (a transition
#'   between horizons, not diagnostic); \code{buried} (0, or the number of
#'   the buried profile the layer belongs to); \code{rockflag}, \code{mark}
#'   and \code{composition} (rock flag, rock mark and turbated composition
#'   when written); \code{route} (\code{"notation"}, \code{"data"} or
#'   \code{"both"}); \code{passed} (TRUE a horizon read, FALSE no diagnostic
#'   horizon, NA undecided); \code{evidence}; \code{missing} (the attributes
#'   that would decide it) and \code{candidates} (the horizons still
#'   possible when undecided), list columns.
#' @seealso \code{\link{match_horizon_sequence}}.
#' @keywords internal
#' @examples
#' # no system identifies its horizons in this release; the error says so
#' tryCatch(identify_horizons(make_hscs2024_example("steppe"), "hscs2024"),
#'          error = function(e) conditionMessage(e))
#' @export
identify_horizons <- function(pedon, system, ...) {
  if (!inherits(pedon, "PedonRecord"))
    rlang::abort("'pedon' must be a PedonRecord.")
  id <- .system_resolve(system)
  sys <- .system_get(id)
  fn <- .horizon_identifier(sys$namespace %||% id)
  if (is.null(fn)) {
    rlang::abort(sprintf("The system '%s' has no horizon identification in soilKey.", id))
  }
  out <- fn(pedon, ...)
  if (!is.data.frame(out) || !"code" %in% names(out))
    rlang::abort(sprintf("The horizon identification of %s must return a data frame with a 'code' column.", id))
  .as_horizon_table(out)
}


# The horizon identification of a system's namespace, .<ns>_identify(), or
# NULL.
.horizon_identifier <- function(ns) {
  get0(paste0(".", ns, "_identify"), envir = asNamespace("soilKey"),
       mode = "function", inherits = FALSE)
}


#' Walk a key to horizons
#'
#' Runs a key written as a tree of positions for one layer. A position is a
#' list with \code{test}, \code{yes} and \code{no} (a binary question), or
#' with \code{branches}, an ordered list of \code{list(test, to, label)} (the
#' first branch whose test is TRUE is taken). A test is a function of
#' \code{(layer, context)} returning TRUE, FALSE or NA, or a list with
#' \code{passed} and \code{missing}; TRUE alone is "otherwise". A target is
#' the name of another position, or an outcome (any other string, or
#' \code{list(outcome = "X")}); NA or NULL is a dead end. The key is not
#' context-free, so \code{context} carries whatever the tests read (the
#' whole profile, the layers above and below).
#'
#' An undecided question (NA) follows its branch and the ones after it, so
#' every outcome the data leave possible is collected. One outcome, reached
#' with no dead end, is the result; several, or a dead end, leave the layer
#' undecided with the candidates. A test that fails with an error answers NA
#' and its message is kept in \code{notes}.
#'
#' @param tree A named list of positions; the first is the start.
#' @param layer What the tests read as the layer (opaque here).
#' @param context Passed to every test.
#' @param start The position to start from (default the first).
#' @return A list: \code{outcome} (NA when undecided), \code{passed} (TRUE
#'   when decided, NA otherwise), \code{candidates} (every outcome reached,
#'   in the order of the walk), \code{paths} (the path to each, as
#'   "position:branch"), \code{path} (the path of the outcome), \code{dead_end},
#'   \code{undecided} (the positions answered NA), \code{answers} (every
#'   answer), \code{missing} (attributes named by the undecided tests) and
#'   \code{notes}.
#' @noRd
.walk_key <- function(tree, layer, context = NULL, start = NULL) {
  if (!is.list(tree) || !length(tree) || is.null(names(tree)) ||
      anyNA(names(tree)) || any(!nzchar(names(tree))) || anyDuplicated(names(tree)))
    rlang::abort("A key must be a list of positions with unique names.")
  start <- start %||% names(tree)[1L]
  if (!start %in% names(tree)) rlang::abort(sprintf("The key has no position '%s'.", start))
  memo <- new.env(parent = emptyenv())
  active <- new.env(parent = emptyenv())
  answers <- logical(0)
  missing <- character(0)
  notes <- character(0)

  ask <- function(test, id) {
    if (is.logical(test) && length(test) == 1L) return(test)
    if (!is.function(test))
      rlang::abort(sprintf("Position '%s': a test must be a function or TRUE.", id))
    r <- tryCatch(test(layer, context), error = function(e) {
      notes <<- c(notes, sprintf("%s: %s", id, conditionMessage(e)))
      NA
    })
    miss <- character(0)
    if (is.list(r)) { miss <- as.character(unlist(r[["missing"]])); r <- r[["passed"]] }
    r <- suppressWarnings(as.logical(r))
    r <- if (length(r) == 1L) r else NA
    if (is.na(r)) missing <<- c(missing, miss)
    r
  }
  target <- function(t, id) {
    if (is.list(t) && !is.null(t[["outcome"]])) t <- list(outcome = as.character(t[["outcome"]])[1L])
    else if (is.null(t) || (length(t) == 1L && is.na(t))) return(list(dead = TRUE))
    else {
      t <- as.character(t)[1L]
      t <- if (t %in% names(tree)) list(node = t) else list(outcome = t)
    }
    if (!is.null(t$outcome) && (is.na(t$outcome) || !nzchar(t$outcome)))
      rlang::abort(sprintf("Position '%s': an outcome must be a non-empty string.", id))
    t
  }
  visit <- function(id) {
    if (!is.null(memo[[id]])) return(memo[[id]])
    if (isTRUE(active[[id]])) rlang::abort(sprintf("The key loops back to position '%s'.", id))
    active[[id]] <- TRUE
    node <- tree[[id]]
    multi <- !is.null(node[["branches"]])
    branches <- if (multi) node[["branches"]]
                else list(list(test = node[["test"]], to = node[["yes"]], label = "yes"),
                          list(test = TRUE, to = node[["no"]], label = "no"))
    if (!multi && is.null(node[["test"]]))
      rlang::abort(sprintf("Position '%s' has neither a test nor branches.", id))
    out <- list(outcomes = list(), dead = list())
    sure <- FALSE
    for (k in seq_along(branches)) {
      b <- branches[[k]]
      lab <- as.character(b[["label"]] %||% k)
      a <- ask(b[["test"]] %||% TRUE, id)
      if (multi || k == 1L) answers[[if (multi) paste0(id, "/", lab) else id]] <<- a
      if (isFALSE(a)) next
      step <- paste0(id, ":", lab)
      tg <- target(b[["to"]], id)
      sub <- if (!is.null(tg$node)) visit(tg$node)
             else if (!is.null(tg$outcome)) list(outcomes = stats::setNames(list(character(0)), tg$outcome), dead = list())
             else list(outcomes = list(), dead = list(character(0)))
      for (o in names(sub$outcomes)) {
        if (is.null(out$outcomes[[o]])) out$outcomes[[o]] <- c(step, sub$outcomes[[o]])
      }
      if (length(sub$dead) && !length(out$dead)) out$dead <- list(c(step, sub$dead[[1L]]))
      if (isTRUE(a)) { sure <- TRUE; break }
    }
    # no branch surely applies: that every one fails is possible, a dead end
    if (!sure && !length(out$dead)) out$dead <- list(paste0(id, ":none"))
    active[[id]] <- FALSE
    memo[[id]] <- out
    out
  }

  r <- visit(start)
  oc <- names(r$outcomes)
  decided <- length(oc) == 1L && !length(r$dead)
  list(outcome = if (decided) oc else NA_character_,
       passed = if (decided) TRUE else NA,
       candidates = oc,
       paths = r$outcomes,
       path = if (decided) r$outcomes[[1L]] else character(0),
       dead_end = length(r$dead) > 0L,
       undecided = names(answers)[is.na(answers)],
       answers = answers,
       missing = unique(missing),
       notes = notes)
}
