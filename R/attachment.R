# =============================================================================
# v0.9.344 -- attachment: the degree of one class, and the choice of a name
# among several classes that may each attach the soil.
#
# Some systems are not keys: every class is tested and a soil may attach to
# one, two or none of them, simply or imperfectly (the French Referentiel
# pedologique 2008 is the first; a CPCS intergrade could use it later). Two
# generic pieces, used by no system yet:
#   * .evaluate_degree(): the degree of one class, from two evaluations of its
#     test block, one strict and one tolerant (R/tolerance.R).
#   * .attach_combine(): from every class with its degree, the main name (one
#     class, or a listed double or multiple), the order of the candidates, the
#     classes that make the name provisional, and the nearest classes when
#     nothing attaches.
# Hugo's decisions of 2026-11-02 (Q3-Q7 of the Referentiel's questions).
# =============================================================================


.DEGREES  <- c("simple", "simple_or_imperfect", "imperfect", "undecided", "fail")
.ATTACHED <- c("simple", "simple_or_imperfect", "imperfect")
.DEGREE_RANK <- c(simple = 1L, simple_or_imperfect = 2L, imperfect = 3L,
                  undecided = 4L, fail = 5L)


# Run `expr` with the rule engine in the given mode (for `system`), restoring
# the mode before.
.with_rule_ctx <- function(tolerant, table, expr, system = NULL) {
  old <- list(tolerant = .rule_ctx$tolerant, negated = .rule_ctx$negated,
              table = .rule_ctx$table, system = .rule_ctx$system)
  on.exit({
    .rule_ctx$tolerant <- old$tolerant
    .rule_ctx$negated  <- old$negated
    .rule_ctx$table    <- old$table
    .rule_ctx$system   <- old$system
  }, add = TRUE)
  .rule_ctx$tolerant <- tolerant
  .rule_ctx$negated  <- FALSE
  .rule_ctx$table    <- table
  .rule_ctx$system   <- system
  force(expr)
}


# A test named with its arguments, for the lists of criteria:
# "attribute_threshold(attribute=ph, threshold=5.5)".
.test_label <- function(spec) {
  nm <- names(spec)[1L]
  a <- spec[[1L]]
  if (!is.list(a) || !length(a)) return(nm)
  keys <- names(a) %||% rep("", length(a))
  vals <- vapply(a, function(v) paste(as.character(unlist(v)), collapse = "|"), "")
  args <- ifelse(nzchar(keys), paste0(keys, "=", vals), vals)
  sprintf("%s(%s)", nm, paste(args, collapse = ", "))
}

# A three-valued verdict read for the class: negated inside a none_of block.
.for_class <- function(p, neg) {
  p <- as.logical(p)[1L]
  if (neg) !p else p
}

.sum_or_na <- function(x) {
  x <- as.numeric(unlist(x))
  x <- x[!is.na(x)]
  if (length(x)) sum(x) else NA_real_
}

# Which criteria of a block fail the class, pass it only within tolerance,
# or leave it undecided, from the strict and the tolerant results of the same
# block. `neg`: the block is read inside a none_of (its verdict counts
# negated). In an "and" (all_of; none_of over its negated tests) every test
# counts; in an "or" (any_of) a test met strictly leaves nothing to name, else
# the first met within tolerance names its criteria, and when every
# alternative fails they are one criterion, "one of {a | b}", whose distance
# is the nearest alternative's.
.degree_explain <- function(tests, s, t, neg = FALSE) {
  out <- list(failed = character(0), imperfect = character(0),
              undecided = character(0), distance = NA_real_)
  if (!is.null(tests$not_implemented_v01)) {
    out$undecided <- paste0("diagnostic_", tests$not_implemented_v01)
    return(out)
  }
  if (isTRUE(tests$default)) return(out)
  cmb <- intersect(c("any_of", "all_of", "none_of"), names(tests))[1L]
  if (is.na(cmb)) {
    out$undecided <- "<malformed test block>"
    return(out)
  }
  child_neg <- if (cmb == "none_of") !neg else neg
  is_and <- xor(cmb != "any_of", neg)
  specs <- tests[[cmb]]
  subs <- vector("list", length(specs))
  o_s <- o_t <- rep(NA, length(specs))
  for (i in seq_along(specs)) {
    spec <- specs[[i]]
    cs <- s$evidence[[i]]
    ct <- t$evidence[[i]]
    o_s[i] <- .for_class(cs$passed, child_neg)
    o_t[i] <- .for_class(ct$passed, child_neg)
    nm <- if (is.list(spec) && length(spec)) names(spec)[1L] else NULL
    if (!is.null(nm) && nm %in% .RULE_COMBINATORS) {
      subs[[i]] <- .degree_explain(spec[1L], cs, ct, child_neg)
    } else {
      lab <- if (is.null(nm)) "<malformed>" else .test_label(spec)
      subs[[i]] <- list(
        failed    = if (isFALSE(o_t[i])) lab else character(0),
        imperfect = if (isTRUE(o_t[i]) && !isTRUE(o_s[i])) lab else character(0),
        undecided = if (is.na(o_t[i])) lab else character(0),
        distance  = if (isFALSE(o_t[i])) .sum_or_na(ct$distance %||% NA_real_) else NA_real_)
    }
  }
  pick <- function(field, which) unique(unlist(lapply(subs[which], `[[`, field)))
  if (is_and) {
    f <- o_t %in% FALSE
    out$failed    <- pick("failed", f)
    out$imperfect <- pick("imperfect", o_t %in% TRUE)
    out$undecided <- pick("undecided", is.na(o_t))
    out$distance  <- .sum_or_na(lapply(subs[f], `[[`, "distance"))
  } else if (any(o_t %in% TRUE)) {
    if (!any(o_s %in% TRUE)) out$imperfect <- subs[[which(o_t %in% TRUE)[1L]]]$imperfect
  } else if (anyNA(o_t)) {
    out$undecided <- pick("undecided", is.na(o_t))
  } else if (length(subs)) {
    alt <- vapply(subs, function(x) paste(x$failed, collapse = " + "), "")
    out$failed <- if (length(alt) == 1L) subs[[1L]]$failed
                  else sprintf("one of {%s}", paste(alt, collapse = " | "))
    d <- vapply(subs, function(x) as.numeric(x$distance)[1L], 0)
    out$distance <- if (all(is.na(d))) NA_real_ else min(d, na.rm = TRUE)
  }
  out
}


# The degree of a class from its strict and tolerant verdicts. A class that
# passes strictly is simple whatever the tolerant pass says (the tolerances
# only widen a test; a tolerant verdict that is not TRUE then comes from a
# predicate that does not widen monotonically, and .evaluate_degree() notes it).
.degree_of <- function(strict, tolerant) {
  if (isTRUE(strict)) "simple"
  else if (isFALSE(tolerant)) "fail"
  else if (is.na(tolerant)) "undecided"
  else if (isFALSE(strict)) "imperfect"
  else "simple_or_imperfect"
}

# The best degree a class can still reach: an undecided class that fails
# strictly can at best be imperfect; any other undecided or
# simple_or_imperfect class can be simple.
.best_degree <- function(degree, strict) {
  if (degree == "undecided" && isFALSE(strict)) "imperfect"
  else if (degree %in% c("undecided", "simple_or_imperfect")) "simple"
  else degree
}


#' The degree of one class: its tests evaluated strictly, then tolerantly
#'
#' The test block (the rule engine's \code{all_of} / \code{any_of} /
#' \code{none_of} / \code{default}) is evaluated twice: strictly, as every
#' system evaluates it, then with the tolerances of \code{tolerances} in
#' force for \code{system} (\code{.tolerance_table()}): the predicates of
#' that system that read their thresholds through \code{.rule_threshold()}
#' or \code{.rule_check()} see each listed threshold moved by its tolerance
#' in the direction that favours the class, the direction flipped inside a
#' \code{none_of} block. A predicate that does not read them gives the same
#' verdict twice.
#'
#' | strict | tolerant | degree |
#' |---|---|---|
#' | TRUE | any | simple (a note when the tolerant verdict is not TRUE) |
#' | FALSE | TRUE | imperfect |
#' | NA | TRUE | simple_or_imperfect |
#' | FALSE or NA | NA | undecided |
#' | FALSE or NA | FALSE | fail |
#'
#' With no tolerance table the tolerant pass is the strict one (the degree is
#' simple, undecided or fail).
#'
#' @param pedon A \code{PedonRecord} (or what the predicates read).
#' @param tests A test block.
#' @param tolerances A tolerance table, or what \code{.tolerance_table()}
#'   reads, or \code{NULL}; always checked.
#' @param system The id of the system whose predicates read the tolerances
#'   (their own \code{system} argument); \code{NULL} in the tests.
#' @return A list: \code{degree}; \code{strict} and \code{tolerant}, the two
#'   verdicts; \code{best}, the best degree the class can still reach;
#'   \code{failed}, the criteria that fail the class even within tolerance;
#'   \code{imperfect_by}, those met only within tolerance;
#'   \code{undecided_by}, those that leave the degree undecided (for
#'   \code{simple_or_imperfect}, those that leave the strict pass undecided);
#'   \code{distance}, the size of the failures in tolerance units (NA when
#'   unknown); \code{missing}, the attributes missing in either pass;
#'   \code{notes}; and \code{evidence}, the two evaluations. Criteria are
#'   named by their test and arguments.
#' @md
#' @noRd
.evaluate_degree <- function(pedon, tests, tolerances = NULL, system = NULL) {
  tab <- .tolerance_table(tolerances)
  strict <- .with_rule_ctx(FALSE, NULL, evaluate_rsg_tests(pedon, tests))
  tolerant <- if (is.null(tab) || !nrow(tab)) strict
              else .with_rule_ctx(TRUE, tab, evaluate_rsg_tests(pedon, tests), system)
  deg <- .degree_of(strict$passed, tolerant$passed)
  notes <- character(0)
  if (isTRUE(strict$passed) && !isTRUE(tolerant$passed))
    notes <- sprintf(paste("The class passes strictly but the tolerant pass gave %s;",
                           "a tolerance can only widen a test, so the class is simple and a",
                           "predicate should be checked."), format(tolerant$passed))
  ex <- .degree_explain(tests, strict, tolerant)
  und <- if (deg == "simple_or_imperfect") .degree_explain(tests, strict, strict)$undecided
         else if (deg == "undecided") ex$undecided else character(0)
  list(degree = deg,
       strict = strict$passed,
       tolerant = tolerant$passed,
       best = .best_degree(deg, strict$passed),
       failed = if (deg == "fail") ex$failed else character(0),
       imperfect_by = if (deg %in% c("imperfect", "simple_or_imperfect")) ex$imperfect else character(0),
       undecided_by = und,
       distance = if (deg == "fail") ex$distance else NA_real_,
       missing = unique(c(as.character(strict$missing), as.character(tolerant$missing))),
       notes = notes,
       evidence = list(strict = strict, tolerant = tolerant))
}


# --- the combiner -------------------------------------------------------------

# Rule rows (precedence pairs, exclusions) from a data.frame or a list of
# entries, with `when` (TRUE by default; FALSE drops the row, NA leaves it
# undecided) and `note` (a page, a reason). A single entry may be unwrapped.
.as_rule_rows <- function(x, cols) {
  if (is.null(x) || !length(x)) {
    df <- as.data.frame(stats::setNames(replicate(length(cols), character(0), simplify = FALSE), cols),
                        stringsAsFactors = FALSE)
    df$when <- logical(0); df$note <- character(0)
    return(df)
  }
  if (!is.data.frame(x)) {
    if (!is.null(names(x)) && all(cols %in% names(x))) x <- list(x)
    x <- do.call(rbind, lapply(x, function(e) {
      e <- as.list(e)
      as.data.frame(c(lapply(stats::setNames(cols, cols), function(k) as.character(e[[k]] %||% NA_character_)[1L]),
                      list(when = as.logical(e[["when"]] %||% TRUE)[1L],
                           note = as.character(e[["note"]] %||% NA_character_)[1L])),
                    stringsAsFactors = FALSE)
    }))
  }
  if (!all(cols %in% names(x)))
    rlang::abort(sprintf("Rule rows need the columns %s.", paste(cols, collapse = ", ")))
  out <- data.frame(lapply(x[cols], as.character), stringsAsFactors = FALSE)
  out$when <- if (is.null(x[["when"]])) TRUE else as.logical(x[["when"]])
  out$note <- if (is.null(x[["note"]])) NA_character_ else as.character(x[["note"]])
  out
}

# The entries of the combiner from a list of lists or a data.frame (one entry
# may be given unwrapped).
.as_attach_entries <- function(entries) {
  if (is.data.frame(entries))
    entries <- lapply(seq_len(nrow(entries)), function(i) lapply(entries[i, , drop = FALSE], function(v) if (is.list(v)) v[[1L]] else v))
  if (!is.list(entries))
    rlang::abort("The candidates must be a list of entries (lists with code and degree) or a data.frame.")
  if (!is.null(names(entries)) && "code" %in% names(entries)) entries <- list(entries)
  lapply(seq_along(entries), function(i) {
    e <- entries[[i]]
    if (!is.list(e)) rlang::abort(sprintf("Candidate %d is not a list with code and degree.", i))
    code <- as.character(e[["code"]] %||% NA_character_)[1L]
    if (is.na(code) || !nzchar(code)) rlang::abort(sprintf("Candidate %d has no code.", i))
    deg <- as.character(e[["degree"]] %||% NA_character_)[1L]
    if (is.na(deg) || !deg %in% .DEGREES)
      rlang::abort(sprintf("Candidate %s: degree must be one of %s.", code, paste(.DEGREES, collapse = ", ")))
    nm <- as.character(e[["name"]] %||% code)[1L]
    strict <- as.logical(e[["strict"]] %||% NA)[1L]
    chr <- function(v) { v <- as.character(unlist(v)); v[!is.na(v) & nzchar(v)] }
    list(code = code, name = nm, label = as.character(e[["label"]] %||% nm)[1L],
         group = as.character(e[["group"]] %||% NA_character_)[1L], degree = deg,
         strict = strict, best = .best_degree(deg, strict),
         failed = chr(e[["failed"]]), imperfect_by = chr(e[["imperfect_by"]]),
         undecided_by = chr(e[["undecided_by"]]), excluded_by = chr(e[["excluded_by"]]),
         distance = suppressWarnings(as.numeric(e[["distance"]] %||% NA_real_))[1L])
  })
}

# The groups that may be written together (doubles, multiples): a list of
# code vectors, or of lists with `codes`, `when` and `note`; each of two or
# more distinct codes.
.as_attach_groups <- function(groups) {
  lapply(seq_along(groups %||% list()), function(i) {
    g <- groups[[i]]
    out <- if (is.list(g)) list(codes = as.character(unlist(g[["codes"]])), when = as.logical(g[["when"]] %||% TRUE)[1L],
                                note = as.character(g[["note"]] %||% NA_character_)[1L])
           else list(codes = as.character(g), when = TRUE, note = NA_character_)
    if (length(out$codes) < 2L || anyNA(out$codes) || anyDuplicated(out$codes))
      rlang::abort(sprintf("Group %d must list two or more distinct codes (%s).", i,
                           paste(out$codes, collapse = ", ")))
    out
  })
}

# Reachability in a directed graph given by two vectors of codes (from, to).
.reaches <- function(a, b, from, to) {
  seen <- character(0); todo <- a
  while (length(todo)) {
    nx <- setdiff(to[from %in% todo], seen)
    if (b %in% nx) return(TRUE)
    seen <- c(seen, nx); todo <- nx
  }
  FALSE
}


#' Choose a name among classes that may attach a soil
#'
#' The generic attachment of a soil to the classes of a system that is not a
#' key (every class tested, several may attach): from every class with its
#' degree (\code{.evaluate_degree()}), the main name, its degree, the
#' candidates and what makes the name provisional.
#'
#' 1. **Exclusions.** A class whose \code{excluded_by} is set is not a
#'    candidate. A row of \code{exclusions} excludes \code{code} when
#'    \code{by} attaches, is not excluded itself and \code{when} is TRUE; the
#'    exclusions are resolved together (a class excluded by an excluded class
#'    stays). Classes that exclude each other (a cycle) are all kept, with a
#'    note, and the degree and the order decide between them. An exclusion by
#'    an undecided class, or with \code{when} NA, keeps the class but marks it
#'    "(undecided)" and makes the name provisional when the class is in it
#'    (with an "exclusion:<code>" entry when the excluding class is in the
#'    name too).
#' 2. **Candidates**: every class attached (simple, simple_or_imperfect,
#'    imperfect) or undecided, and not excluded.
#' 3. **Precedence**: the explicit pairs of \code{precedence} first
#'    (\code{first} before \code{second}, when both are candidates and
#'    \code{when} is TRUE; a pair with \code{when} NA is not applied and makes
#'    the name provisional if its \code{first} could take it), then the degree
#'    (simple, simple_or_imperfect, imperfect, undecided), then the position
#'    in \code{order} (classes not in it after, in the order given). Pairs on
#'    a cycle are dropped, with a note. When the pairs and the degrees
#'    disagree (a class ranked after one of a weaker degree, with no pair
#'    putting that one first), a note says the order is a convention. The
#'    main class is the first attached candidate.
#' 4. **Doubles and multiples**: only a group of \code{groups} that holds the
#'    main class and whose members all attach (\code{when} TRUE) is written
#'    together, in the group's own order, joined with \code{sep}; the longest
#'    such group first, then the best ranked. Two classes attaching outside a
#'    listed group: the second stays a candidate.
#' 5. **Nothing attaches**: the name is \code{none_label}, and the
#'    \code{n_nearest} failed classes nearest to attaching (fewest failed
#'    criteria, then the smallest distance in tolerance units, then the order
#'    given) join the candidates, marked \code{nearest}.
#' 6. **Provisional**: a candidate that could take the name once its data are
#'    known (an undecided or simple_or_imperfect class read at the best degree
#'    it can still reach, while the main class is read at its worst; a main
#'    class read as imperfect; a listed partner left undecided; an undecided
#'    exclusion or precedence pair) is listed in \code{provisional_because}.
#'
#' Codes of \code{exclusions}, \code{precedence} or \code{groups} that name no
#' entry are reported in \code{notes}.
#'
#' @param entries The classes: a list of lists (or a data.frame) with
#'   \code{code}, \code{degree} (one of simple, simple_or_imperfect,
#'   imperfect, undecided, fail) and optionally \code{name}, \code{label} (the
#'   form written in the name; default \code{name}), \code{group} (a grouping
#'   of classes, shown in the table), \code{strict} (the strict verdict: an
#'   undecided class that fails strictly can at best be imperfect),
#'   \code{failed}, \code{imperfect_by}, \code{undecided_by},
#'   \code{excluded_by} (character vectors) and \code{distance} (number), as
#'   \code{.evaluate_degree()} gives them.
#' @param precedence Pairs \code{first}, \code{second}, optional \code{when}
#'   and \code{note} (data.frame or list of lists).
#' @param order Class codes in the order that breaks the remaining ties.
#' @param groups Codes that may be written together: a list of code vectors,
#'   or of lists with \code{codes}, \code{when} and \code{note}; two or more
#'   distinct codes each.
#' @param exclusions Rows \code{code}, \code{by}, optional \code{when} and
#'   \code{note}.
#' @param n_nearest How many nearest classes to list when nothing attaches
#'   (0 or more).
#' @param none_label The name when nothing attaches.
#' @param sep The separator of a double or multiple name.
#' @return A list: \code{name}; \code{codes}, the classes in the name, in its
#'   order; \code{degree}, the main class's degree (for a double or multiple
#'   the weakest of its members'), NA when nothing attaches;
#'   \code{attachment}: simple, simple_or_imperfect, imperfect, double,
#'   multiple or not_attached; \code{candidates}, a data.frame (\code{code},
#'   \code{name}, \code{group}, \code{degree}, \code{rank}, \code{in_name},
#'   \code{double_with}, \code{double_possible}, \code{n_failed},
#'   \code{distance}, \code{failed}, \code{imperfect_by},
#'   \code{undecided_by}, \code{excluded_by}, \code{nearest}; the lists of
#'   criteria joined with "; "), the ranked candidates first, then the
#'   excluded classes, then the nearest; \code{provisional_because}, codes
#'   (and "exclusion:<code>" entries); \code{notes}.
#' @md
#' @noRd
.attach_combine <- function(entries, precedence = NULL, order = NULL, groups = NULL,
                            exclusions = NULL, n_nearest = 3L,
                            none_label = "[not attached]", sep = "-") {
  if (!is.numeric(n_nearest) || length(n_nearest) != 1L || is.na(n_nearest) || n_nearest < 0)
    rlang::abort("'n_nearest' must be a single number, 0 or more.")
  E <- .as_attach_entries(entries)
  codes <- vapply(E, `[[`, "", "code")
  if (anyDuplicated(codes))
    rlang::abort(sprintf("Candidate codes repeat: %s.", paste(unique(codes[duplicated(codes)]), collapse = ", ")))
  names(E) <- codes
  deg <- stats::setNames(vapply(E, `[[`, "", "degree"), codes)
  best <- stats::setNames(vapply(E, `[[`, "", "best"), codes)
  idx <- stats::setNames(seq_along(codes), codes)
  order <- as.character(order %||% character(0))
  ordpos <- stats::setNames(match(codes, order), codes)
  ordpos[is.na(ordpos)] <- length(order) + 1L
  notes <- character(0)
  unknown <- function(what, x) {
    u <- setdiff(unique(x[!is.na(x)]), codes)
    if (length(u)) notes <<- c(notes, sprintf("%s name codes that are no candidate: %s.", what,
                                              paste(u, collapse = ", ")))
  }
  G <- .as_attach_groups(groups)
  unknown("Groups", unlist(lapply(G, `[[`, "codes")))

  # 1. exclusions, resolved together: only a class that is not excluded
  # excludes; classes on a cycle of exclusions are all kept
  excl <- lapply(E, `[[`, "excluded_by")
  own <- lengths(excl) > 0L
  maybe <- stats::setNames(rep(list(character(0)), length(codes)), codes)
  ex <- .as_rule_rows(exclusions, c("code", "by"))
  unknown("Exclusions", c(ex$code, ex$by))
  ex <- ex[ex$code %in% codes & ex$by %in% codes & ex$code != ex$by & !(ex$when %in% FALSE), , drop = FALSE]
  def <- ex[ex$when %in% TRUE & deg[ex$by] %in% .ATTACHED & !own[ex$by], , drop = FALSE]
  on_cyc <- vapply(seq_len(nrow(def)), function(k) .reaches(def$code[k], def$by[k], def$by, def$code), NA)
  if (any(on_cyc)) {
    cy <- unique(c(def$code[on_cyc], def$by[on_cyc]))
    notes <- c(notes, sprintf(paste("Some classes exclude each other (%s): none of them is excluded by",
                                    "the others, and the degree and the order decide between them."),
                              paste(cy, collapse = ", ")))
  }
  def <- def[!on_cyc, , drop = FALSE]
  state <- stats::setNames(rep(NA, length(codes)), codes)
  resolve <- function(cd) {
    if (!is.na(state[[cd]])) return(state[[cd]])
    by <- def$by[def$code == cd]
    v <- own[[cd]] || (length(by) > 0L && any(!vapply(by, resolve, NA)))
    state[[cd]] <<- v
    v
  }
  for (cd in codes) resolve(cd)
  excluded <- state
  for (k in seq_len(nrow(def))) {
    if (excluded[[def$code[k]]] && !own[[def$code[k]]] && !excluded[[def$by[k]]])
      excl[[def$code[k]]] <- c(excl[[def$code[k]]], def$by[k])
  }
  for (k in seq_len(nrow(ex))) {
    cd <- ex$code[k]; by <- ex$by[k]; w <- ex$when[k]
    if (excluded[[cd]] || excluded[[by]] || own[[by]]) next
    if ((deg[[by]] %in% .ATTACHED && is.na(w)) || deg[[by]] == "undecided")
      maybe[[cd]] <- c(maybe[[cd]], by)
  }
  pool <- codes[deg %in% c(.ATTACHED, "undecided") & !excluded]

  # 3. precedence
  pr <- .as_rule_rows(precedence, c("first", "second"))
  unknown("Precedence pairs", c(pr$first, pr$second))
  pr <- pr[pr$first %in% pool & pr$second %in% pool & !(pr$when %in% FALSE), , drop = FALSE]
  pr_na <- pr[is.na(pr$when), , drop = FALSE]
  pr <- pr[pr$when %in% TRUE & pr$first != pr$second, , drop = FALSE]
  # a pair on a cycle (its second also comes before its first, through the
  # other pairs) is dropped: the degree and the order decide between them
  on_cycle <- vapply(seq_len(nrow(pr)), function(k) .reaches(pr$second[k], pr$first[k], pr$first, pr$second), NA)
  cycle <- any(on_cycle)
  pr <- pr[!on_cycle, , drop = FALSE]
  rank_pool <- function(dg) {
    rem <- pool; out <- character(0)
    while (length(rem)) {
      avail <- rem[!(rem %in% pr$second[pr$first %in% rem])]
      o <- order(.DEGREE_RANK[dg[avail]], ordpos[avail], idx[avail])
      out <- c(out, avail[o[1L]])
      rem <- setdiff(rem, avail[o[1L]])
    }
    out
  }
  main_of <- function(dg) {
    r <- rank_pool(dg)
    a <- r[dg[r] %in% .ATTACHED]
    if (length(a)) a[1L] else NA_character_
  }
  deg_pool <- deg[pool]
  ranked <- rank_pool(deg_pool)
  if (cycle) notes <- c(notes, "Some explicit precedence pairs form a cycle; between those classes the degree and the order decide.")
  if (nrow(pr_na)) notes <- c(notes, sprintf("Precedence pair(s) undecided, not applied: %s.",
                                             paste(pr_na$first, pr_na$second, sep = " before ", collapse = "; ")))
  # the pairs and the degrees disagree: a class ranked after one of a weaker
  # degree that no chain of pairs puts first
  if (nrow(pr) && length(ranked) > 1L) {
    clash <- NULL
    for (i in seq_len(length(ranked) - 1L)) {
      for (j in (i + 1L):length(ranked)) {
        a <- ranked[i]; b <- ranked[j]
        if (.DEGREE_RANK[[deg[[b]]]] < .DEGREE_RANK[[deg[[a]]]] && !.reaches(a, b, pr$first, pr$second)) {
          clash <- c(a, b); break
        }
      }
      if (!is.null(clash)) break
    }
    if (!is.null(clash))
      notes <- c(notes, sprintf(paste("The precedence pairs and the degrees disagree (%s ranks before %s,",
                                      "which has the better degree, through the pairs on other classes);",
                                      "the order chosen is a convention of soilKey."), clash[1L], clash[2L]))
  }
  M <- main_of(deg_pool)

  # 4. doubles and multiples; 6. provisional
  members <- if (is.na(M)) character(0) else M
  prov <- character(0)
  markers <- character(0)
  mine <- list()
  if (!is.na(M)) {
    mine <- Filter(function(g) M %in% g$codes && !isFALSE(g$when), G)
    full <- Filter(function(g) isTRUE(g$when) && all(g$codes %in% pool) &&
                     all(deg_pool[g$codes] %in% .ATTACHED), mine)
    if (length(full)) {
      len <- vapply(full, function(g) length(g$codes), 0L)
      rk <- vapply(full, function(g) min(match(setdiff(g$codes, M), ranked)), 0L)
      members <- full[[order(-len, rk)[1L]]]$codes
    }
    for (g in mine) {
      if (all(g$codes %in% members)) next
      oth <- setdiff(g$codes, M)
      if (!all(oth %in% pool)) next
      if (is.na(g$when)) {
        prov <- c(prov, oth)
        notes <- c(notes, sprintf("Group %s undecided, not applied.", paste(g$codes, collapse = sep)))
      } else prov <- c(prov, oth[deg_pool[oth] %in% c("undecided", "simple_or_imperfect")])
    }
    worst <- function(d) if (d == "simple_or_imperfect") "imperfect" else d
    for (X in setdiff(pool[deg_pool %in% c("undecided", "simple_or_imperfect")], M)) {
      dg <- deg_pool; dg[[X]] <- best[[X]]; dg[[M]] <- worst(dg[[M]])
      if (!identical(main_of(dg), M)) prov <- c(prov, X)
    }
    if (deg_pool[[M]] == "simple_or_imperfect") {
      dg <- deg_pool; dg[[M]] <- "imperfect"
      Y <- main_of(dg)
      if (!identical(Y, M)) prov <- c(prov, Y)
    }
    for (m in members) {
      prov <- c(prov, maybe[[m]])
      if (any(maybe[[m]] %in% members)) markers <- c(markers, paste0("exclusion:", m))
    }
    prov <- c(prov, pr_na$first[pr_na$second %in% members])
  } else {
    prov <- pool[deg_pool == "undecided"]
  }
  prov <- c(setdiff(unique(prov), members), unique(markers))

  # the candidates table
  nearest <- character(0)
  if (is.na(M)) {
    fl <- codes[deg == "fail" & !excluded]
    nf <- vapply(E[fl], function(e) length(e$failed), 0L)
    dd <- vapply(E[fl], function(e) e$distance, 0)
    nearest <- utils::head(fl[order(nf, is.na(dd), dd, idx[fl])], n_nearest)
  }
  rows <- c(ranked, codes[excluded], nearest)
  j <- function(x) paste(x, collapse = "; ")
  # may the class be written with the main class? (in a listed group with it)
  dposs <- function(r) {
    if (is.na(M)) return(FALSE)
    if (r %in% members && length(members) > 1L) return(TRUE)
    if (r == M) return(any(vapply(mine, function(g) any(setdiff(g$codes, M) %in% pool), NA)))
    any(vapply(mine, function(g) r %in% g$codes, NA))
  }
  cand <- data.frame(
    code = rows,
    name = vapply(E[rows], `[[`, "", "name", USE.NAMES = FALSE),
    group = vapply(E[rows], `[[`, "", "group", USE.NAMES = FALSE),
    degree = unname(deg[rows]),
    rank = match(rows, ranked),
    in_name = rows %in% members,
    double_with = vapply(rows, function(r) if (r %in% members) j(setdiff(members, r)) else "", "", USE.NAMES = FALSE),
    double_possible = vapply(rows, dposs, NA, USE.NAMES = FALSE),
    n_failed = vapply(E[rows], function(e) length(e$failed), 0L, USE.NAMES = FALSE),
    distance = vapply(E[rows], `[[`, 0, "distance", USE.NAMES = FALSE),
    failed = vapply(E[rows], function(e) j(e$failed), "", USE.NAMES = FALSE),
    imperfect_by = vapply(E[rows], function(e) j(e$imperfect_by), "", USE.NAMES = FALSE),
    undecided_by = vapply(E[rows], function(e) j(e$undecided_by), "", USE.NAMES = FALSE),
    excluded_by = vapply(rows, function(r) j(c(excl[[r]], if (length(maybe[[r]])) paste(maybe[[r]], "(undecided)"))),
                         "", USE.NAMES = FALSE),
    nearest = rows %in% nearest,
    stringsAsFactors = FALSE, row.names = NULL)

  if (is.na(M)) {
    name <- none_label; d <- NA_character_; att <- "not_attached"
  } else {
    name <- paste(vapply(E[members], `[[`, "", "label", USE.NAMES = FALSE), collapse = sep)
    d <- names(.DEGREE_RANK)[max(.DEGREE_RANK[deg[members]])]
    att <- if (length(members) == 1L) deg[[M]] else if (length(members) == 2L) "double" else "multiple"
  }
  list(name = name, codes = members, degree = d, attachment = att,
       candidates = cand, provisional_because = prov, notes = notes)
}
