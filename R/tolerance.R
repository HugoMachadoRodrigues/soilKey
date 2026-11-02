# =============================================================================
# v0.9.344 -- tolerances on thresholds, and the tolerant mode of the rule
# engine.
#
# Some systems give a tolerance to some of their printed thresholds: a value
# beyond the threshold but within the tolerance still attaches the soil, as an
# imperfect attachment (the French Referentiel pedologique 2008, p. 64, is the
# first). Three pieces:
#   * within_tolerance(): one comparison, "pass" / "imperfect" / "fail" / NA.
#   * a tolerance table that a system supplies (.tolerance_table()): a
#     tolerance per kind of threshold (depth, pH, ...) and per threshold value,
#     or for every threshold of a kind; a threshold it does not list is strict.
#   * the tolerant mode: .evaluate_degree() (R/attachment.R) evaluates a test
#     block twice, strictly and then tolerantly. In the tolerant pass the
#     predicates that read their thresholds through .rule_threshold() or
#     .rule_check() see each threshold moved by its tolerance in the direction
#     that favours the class; inside a none_of block the direction flips (a
#     forbidden horizon "within 120 cm" is looked for within 100 cm, not 140).
# The mode is off unless .evaluate_degree() turns it on, so every predicate
# and every system that does not use it runs as before.
# =============================================================================


# The context of the rule engine: off (strict) outside .evaluate_degree().
# `negated` is flipped by evaluate_rsg_tests() on entering a none_of block, in
# the tolerant pass only.
.rule_ctx <- new.env(parent = emptyenv())
.rule_ctx$tolerant <- FALSE
.rule_ctx$negated  <- FALSE
.rule_ctx$table    <- NULL


#' Compare a value with a threshold that has a tolerance
#'
#' Some classification systems give a tolerance to some of their printed
#' thresholds: a value beyond the threshold but within the tolerance does not
#' fail the class, it attaches the soil to it imperfectly (the French
#' \emph{Referentiel pedologique} 2008 calls this an imperfect attachment). \code{within_tolerance()} makes one
#' such comparison.
#'
#' For a minimum (\code{direction = "min"}), a value at or above the threshold
#' passes, a value below it by no more than \code{tol} is imperfect, and a
#' value further below fails. For a maximum (\code{"max"}), a value at or
#' below the threshold passes, a value above it by no more than \code{tol} is
#' imperfect, and a value further above fails. The edge of the tolerance band
#' is inclusive and read with a relative margin of 1e-9, so that 5.3 is within
#' 0.2 of 5.5 despite the binary representation of decimals; the threshold
#' itself is read exactly. A tolerance of 0 makes the comparison strict (only
#' "pass" or "fail"). A missing value gives \code{NA}.
#'
#' @param value Numeric vector: the values compared.
#' @param threshold Numeric: the printed threshold, recycled.
#' @param direction \code{"min"} (the value must reach the threshold) or
#'   \code{"max"} (the value must not exceed it).
#' @param tol Non-negative number: the tolerance, in the unit of the value,
#'   recycled. \code{NA} or \code{NULL} reads as 0 (strict).
#' @return A character vector as long as \code{value}: \code{"pass"},
#'   \code{"imperfect"}, \code{"fail"} or \code{NA}.
#' @examples
#' # organic carbon 6.5 g/100 g against a minimum of 8 with a tolerance of 2
#' within_tolerance(6.5, 8, "min", 2)
#' # a horizon starting at 110 cm against "within 120 cm", tolerance 20
#' within_tolerance(c(100, 130, 150), 120, "max", 20)
#' # a threshold with no tolerance is strict
#' within_tolerance(39, 40, "min", 0)
#' @export
within_tolerance <- function(value, threshold, direction = c("min", "max"), tol = 0) {
  direction <- match.arg(direction)
  if (is.null(tol)) tol <- 0
  if (!is.numeric(threshold) || !length(threshold) || anyNA(threshold))
    rlang::abort("'threshold' must be a number.")
  tol <- as.numeric(tol)
  tol[is.na(tol)] <- 0
  if (any(tol < 0)) rlang::abort("'tol' must not be negative.")
  value <- suppressWarnings(as.numeric(value))
  n <- length(value)
  if (!n) return(character(0))
  t <- rep_len(as.numeric(threshold), n)
  tol <- rep_len(tol, n)
  eps <- 1e-9 * pmax(1, abs(t))
  out <- rep(NA_character_, n)
  ok <- !is.na(value)
  if (direction == "min") {
    pass <- value >= t
    band <- !pass & tol > 0 & value >= t - tol - eps
  } else {
    pass <- value <= t
    band <- !pass & tol > 0 & value <= t + tol + eps
  }
  out[ok & pass] <- "pass"
  out[ok & !pass & band] <- "imperfect"
  out[ok & !pass & !band] <- "fail"
  out
}


# A tolerance table: a data.frame with `kind`, `threshold` (NA = every
# threshold of the kind) and `tol`. From a data.frame with these columns, or
# from a list of entries as a YAML file gives them:
#   - {kind: depth, thresholds: [10, 20, 30, 35], tol: 5}
#   - {kind: depth, threshold: 120, tol: 20}
#   - {kind: ph, tol: 0.2}                     # every pH threshold
# An entry may also carry `tols`, one per threshold. A kind and threshold
# listed twice is an error.
.tolerance_table <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x) && !is.null(x$tolerances)) x <- x$tolerances
  if (is.data.frame(x)) {
    if (!all(c("kind", "tol") %in% names(x)))
      rlang::abort("A tolerance table needs the columns 'kind' and 'tol'.")
    tab <- data.frame(kind = as.character(x$kind),
                      threshold = as.numeric(x$threshold %||% NA_real_),
                      tol = as.numeric(x$tol), stringsAsFactors = FALSE)
  } else {
    rows <- lapply(seq_along(x), function(i) {
      e <- x[[i]]
      kind <- as.character(e$kind %||% NA_character_)
      if (length(kind) != 1L || is.na(kind) || !nzchar(kind))
        rlang::abort(sprintf("Tolerance entry %d has no kind.", i))
      th <- as.numeric(unlist(e$thresholds %||% e$threshold %||% NA_real_))
      tl <- as.numeric(unlist(e$tols %||% e$tol))
      if (!length(tl) || (length(tl) != 1L && length(tl) != length(th)))
        rlang::abort(sprintf("Tolerance entry %d (%s) has no tolerance, or not one per threshold.", i, kind))
      data.frame(kind = kind, threshold = th, tol = rep_len(tl, length(th)),
                 stringsAsFactors = FALSE)
    })
    tab <- if (length(rows)) do.call(rbind, rows)
           else data.frame(kind = character(0), threshold = numeric(0), tol = numeric(0))
  }
  if (anyNA(tab$kind) || anyNA(tab$tol) || any(tab$tol < 0))
    rlang::abort("Every tolerance needs a kind and a tolerance of 0 or more.")
  key <- paste(tab$kind, format(tab$threshold, digits = 15))
  if (anyDuplicated(key))
    rlang::abort(sprintf("The tolerance table lists twice: %s.",
                         paste(unique(key[duplicated(key)]), collapse = ", ")))
  rownames(tab) <- NULL
  tab
}

# The tolerance of one threshold of a kind: the row of that threshold, else
# the row for every threshold of the kind, else 0 (strict).
.tolerance_of <- function(table, kind, threshold) {
  if (is.null(table) || !nrow(table) || is.null(kind)) return(0)
  rows <- table$kind == kind
  if (!any(rows)) return(0)
  th <- table$threshold[rows]
  exact <- !is.na(th) & abs(th - threshold) <= 1e-9 * max(1, abs(threshold))
  if (any(exact)) return(table$tol[rows][which(exact)[1L]])
  all_k <- is.na(th)
  if (any(all_k)) return(table$tol[rows][which(all_k)[1L]])
  0
}

# The direction a predicate's threshold works in for the class: flipped inside
# a none_of block of the tolerant pass (the test must then fail for the class).
.rule_direction <- function(direction) {
  if (isTRUE(.rule_ctx$negated)) c(min = "max", max = "min")[[direction]] else direction
}


# For predicates: the threshold as the current pass reads it. In the strict
# pass (and in every system that does not use the tolerant mode) the
# threshold itself; in the tolerant pass moved by its tolerance (from the
# system's table, by `kind`) in the direction that favours the class: a
# minimum lowered, a maximum raised, the other way inside a none_of block.
.rule_threshold <- function(threshold, kind, direction = c("min", "max")) {
  if (!isTRUE(.rule_ctx$tolerant)) return(threshold)
  direction <- match.arg(direction)
  tol <- .tolerance_of(.rule_ctx$table, kind, threshold)
  if (tol == 0) return(threshold)
  if (.rule_direction(direction) == "min") threshold - tol else threshold + tol
}

# For predicates: does each value meet the threshold, as the current pass
# reads it? TRUE / FALSE / NA per value. A value at the edge of the tolerance
# band meets it (the margin of within_tolerance()).
.rule_check <- function(value, threshold, kind, direction = c("min", "max")) {
  direction <- match.arg(direction)
  value <- suppressWarnings(as.numeric(value))
  t <- .rule_threshold(threshold, kind, direction)
  # the margin favours the class: inside a none_of block it makes the
  # predicate stricter
  eps <- if (t != threshold) 1e-9 * max(1, abs(threshold)) else 0
  if (isTRUE(.rule_ctx$negated)) eps <- -eps
  if (direction == "min") value >= t - eps else value <= t + eps
}

# For predicates: how far the values are from what the class needs, in
# tolerance units of the printed threshold (0 when it is met; NA when the
# threshold has no tolerance, no table is in force, or no value is known).
# `combine` says how the predicate reads its values: "any" (one value meeting
# the threshold is enough: the nearest counts) or "all" (every value must:
# the furthest counts). Inside a none_of block of the tolerant pass the class
# needs the test to fail, so both the direction and the reading flip.
.rule_distance <- function(value, threshold, kind, direction = c("min", "max"),
                           combine = c("any", "all")) {
  direction <- match.arg(direction)
  combine <- match.arg(combine)
  tol <- .tolerance_of(.rule_ctx$table, kind, threshold)
  value <- suppressWarnings(as.numeric(value))
  value <- value[!is.na(value)]
  if (tol == 0 || !length(value)) return(NA_real_)
  if (isTRUE(.rule_ctx$negated)) {
    direction <- c(min = "max", max = "min")[[direction]]
    combine <- c(any = "all", all = "any")[[combine]]
  }
  short <- if (direction == "min") pmax(0, threshold - value) else pmax(0, value - threshold)
  d <- if (combine == "any") min(short) else max(short)
  d / tol
}


# A generic tolerance-aware predicate, the model for the predicates of a
# system that uses the tolerant mode (and the one the tests of the engine use):
# do the horizons overlapping [top_cm, bottom_cm), optionally only those whose
# designation matches `designation` (a regular expression), meet the threshold
# on `attribute`? `layers = "any"`: one horizon meeting it is enough; "all":
# every horizon must. Three-valued: undecided when a missing value could change
# the answer; FALSE when no horizon is in the window. The distance (in
# tolerance units, .rule_distance()) is returned for the attachment's nearest
# classes.
.attribute_threshold <- function(pedon, attribute, threshold, direction = c("min", "max"),
                                 kind = attribute, top_cm = 0, bottom_cm = Inf,
                                 designation = NULL, layers = c("any", "all")) {
  direction <- match.arg(direction)
  layers <- match.arg(layers)
  h <- pedon$horizons
  if (is.null(h) || !nrow(h))
    return(list(passed = NA, layers = integer(0), missing = "horizons",
                notes = "no horizons"))
  idx <- seq_len(nrow(h))
  top <- as.numeric(h$top_cm); bot <- as.numeric(h$bottom_cm)
  inwin <- !is.na(top) & top < bottom_cm & (is.na(bot) | bot > top_cm)
  if (!is.null(designation)) {
    des <- as.character(h$designation)
    inwin <- inwin & !is.na(des) & grepl(designation, des)
  }
  idx <- idx[inwin]
  if (!length(idx))
    return(list(passed = FALSE, layers = integer(0), missing = character(0),
                notes = "no horizon in the window"))
  v <- if (attribute %in% names(h)) suppressWarnings(as.numeric(h[[attribute]][idx]))
       else rep(NA_real_, length(idx))
  ok <- .rule_check(v, threshold, kind, direction)
  passed <- if (layers == "any") {
    if (any(ok %in% TRUE)) TRUE else if (anyNA(ok)) NA else FALSE
  } else {
    if (any(ok %in% FALSE)) FALSE else if (anyNA(ok)) NA else TRUE
  }
  list(passed = passed,
       layers = idx[ok %in% TRUE],
       missing = if (is.na(passed)) attribute else character(0),
       evidence = list(attribute = attribute, values = v,
                       threshold = .rule_threshold(threshold, kind, direction),
                       printed_threshold = threshold, direction = direction),
       distance = .rule_distance(v, threshold, kind, direction, layers))
}
