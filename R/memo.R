# =============================================================================
# v0.9.400 -- caches that make the classifiers faster without changing any
# result.
#
# 1. Rule sets. load_rules() reads the YAML files of a system once per session
#    (.rules_cache, keyed on the system and the package); the WRB qualifier
#    lists (qualifiers.yaml) likewise. A rule set is an R list, a value: a
#    caller that changes its copy does not change the cache.
#
# 2. Per-classification memo. A classifier opens a scope for the length of its
#    call (.memo_open() / .memo_close()). Inside it, the pedon-level
#    diagnostics that the keys call again and again (each USDA order calls the
#    orders before it; the WRB limiting layers are read by many qualifiers) are
#    computed once per pedon object, argument set and soilKey.* options
#    (.memo()). The scope keeps each pedon it has seen with the horizon table
#    and the site it had: a horizon table or site replaced since (a pedon
#    modified) starts the memo of that pedon afresh. When the call returns the
#    scope is dropped, so a pedon modified after a classification is read
#    anew by the next one. Outside a scope (a diagnostic called on its own)
#    nothing is memoised. A memoised R6 result (a DiagnosticResult) is
#    returned as a fresh clone each time, as the function made a new object at
#    each call before; the warnings and messages of the first call are given
#    again at each later one.
#
# 3. Designation readings. .desg_master(), .desg_old_notation() and
#    .desg_declared_notation() are pure functions of their arguments; their
#    results are kept in a bounded session cache, each entry checked with
#    identical() against the inputs it was computed from.
# =============================================================================

# ---- 1. rule sets ------------------------------------------------------------

.rules_cache <- new.env(parent = emptyenv())

# Clear the cached rule sets (after the YAML files of a source tree changed
# within one session).
.rules_cache_clear <- function() {
  rm(list = ls(.rules_cache, all.names = TRUE), envir = .rules_cache)
  invisible(NULL)
}


# ---- 2. per-classification memo ----------------------------------------------

.memo_state <- new.env(parent = emptyenv())
.memo_state$slots <- NULL

# Pedon objects kept by one scope (a classification also memoises the copies
# it makes, a gap-filled or truncated pedon); the oldest is dropped beyond it.
.MEMO_MAX_SLOTS <- 64L

# Open a memo scope unless one is open; TRUE when this call opened it, and the
# caller then closes it on exit:
#   if (.memo_open()) on.exit(.memo_close(), add = TRUE)
.memo_open <- function() {
  if (!is.null(.memo_state$slots)) return(FALSE)
  .memo_state$slots <- list()
  TRUE
}

.memo_close <- function() {
  .memo_state$slots <- NULL
  invisible(NULL)
}

# The memo of one pedon in the open scope, or NULL (no scope, or not a
# PedonRecord). A pedon whose horizon table or site is not the one it had when
# first seen gets an empty memo. The slot keeps a copy of the horizon table,
# so a table changed in place (data.table::set(), :=) is seen by its content.
.memo_slot <- function(pedon) {
  slots <- .memo_state$slots
  if (is.null(slots) || !inherits(pedon, "PedonRecord")) return(NULL)
  h <- pedon$horizons
  site <- pedon$site
  for (s in slots) {
    if (identical(s$pedon, pedon)) {
      if (!identical(s$h, .memo_content(h)) || !identical(s$site, site)) {
        s$h <- .memo_snapshot(h)
        s$site <- site
        s$memo <- new.env(parent = emptyenv())
      }
      return(s)
    }
  }
  s <- new.env(parent = emptyenv())
  s$pedon <- pedon
  s$h <- .memo_snapshot(h)
  s$site <- site
  s$memo <- new.env(parent = emptyenv())
  if (length(slots) >= .MEMO_MAX_SLOTS) slots <- slots[-1L]
  .memo_state$slots <- c(slots, list(s))
  s
}

# The soilKey.* options in force: a result is reused only under the same ones
# (a key may set one for part of its work).
.memo_options <- function() {
  o <- .Options
  o[startsWith(names(o), "soilKey.")]
}

# The content of a horizon table, to compare with identical(): its columns
# and its class, in a copy that an edit in place of the pedon's own table does
# not reach. The other attributes are left out: a data.table keeps a pointer
# to itself (.internal.selfref) that differs between a table and its copy, and
# is empty in a table read from a file.
.memo_snapshot <- function(h) {
  if (!is.list(h)) return(h)
  if (data.table::is.data.table(h)) h <- data.table::copy(h)
  .memo_content(h)
}

.memo_content <- function(h) {
  if (!is.list(h)) return(h)
  cls <- class(h)
  nm <- names(h)
  attributes(h) <- NULL
  list(class = cls, names = nm, columns = h)
}

# A copy for the caller: a new R6 object for an R6 result, the value
# otherwise. The R6 objects inside it (the DiagnosticResults a diagnostic
# keeps in its evidence, in lists at any depth) are copied too, so a caller
# that changes a nested result changes only its own copy. An object met twice
# (the USDA orders keep the same earlier orders in their evidence) is copied
# once, so the copy keeps the shape of the original.
.memo_copy <- function(v, seen = new.env(parent = emptyenv())) {
  if (inherits(v, "R6")) {
    key <- data.table::address(v)
    done <- seen[[key]]
    if (!is.null(done)) return(done)
    out <- v$clone()
    assign(key, out, envir = seen)
    if ("evidence" %in% names(out) && is.list(out$evidence))
      out$evidence <- .memo_copy(out$evidence, seen)
    return(out)
  }
  if (is.list(v) && !is.data.frame(v)) {
    for (i in seq_along(v)) {
      x <- v[[i]]
      if (inherits(x, "R6") || (is.list(x) && !is.data.frame(x))) {
        y <- .memo_copy(x, seen)
        if (!identical(x, y)) v[[i]] <- y
      }
    }
  }
  v
}

# The value of `expr` (a promise, evaluated only when needed) for `pedon`,
# memoised in the open scope under `id` and the argument list `args`.
.memo <- function(pedon, id, args, expr) {
  s <- .memo_slot(pedon)
  if (is.null(s)) return(expr)
  opts <- .memo_options()
  entries <- s$memo[[id]]
  for (e in entries) {
    if (identical(e$args, args) && identical(e$opts, opts)) {
      for (cnd in e$conds) {
        if (inherits(cnd, "warning")) warning(cnd) else message(cnd)
      }
      return(.memo_copy(e$value))
    }
  }
  conds <- list()
  value <- withCallingHandlers(
    expr,
    warning = function(w) conds[[length(conds) + 1L]] <<- w,
    message = function(m) conds[[length(conds) + 1L]] <<- m
  )
  s$memo[[id]] <- c(entries, list(list(args = args, opts = opts,
                                       value = .memo_copy(value), conds = conds)))
  value
}


# ---- 3. designation readings ---------------------------------------------------

.desg_cache <- new.env(parent = emptyenv())
.desg_cache$n <- 0L
.DESG_CACHE_MAX <- 20000L

# Inputs that can serve as a key: character vectors of valid UTF-8 text in the
# native encoding (all the designations soilKey meets). Others are computed
# every time.
.desg_cacheable <- function(x) {
  is.character(x) && all(Encoding(x) == "unknown") && all(validUTF8(x))
}

# The cached value of compute() for `inputs` (a list of character vectors and
# flags) under `kind`, checked with identical() against the inputs it was
# computed from.
.desg_memo <- function(kind, inputs, compute) {
  for (x in inputs) if (!.desg_cacheable(x)) return(compute())
  key <- paste(c(kind, vapply(inputs, function(x) paste(c(length(x), x), collapse = "\r"), ""),
                 recursive = TRUE), collapse = "\n")
  e <- .desg_cache[[key]]
  if (!is.null(e) && identical(e$inputs, inputs)) return(e$value)
  value <- compute()
  if (.desg_cache$n >= .DESG_CACHE_MAX) {
    rm(list = ls(.desg_cache, all.names = TRUE), envir = .desg_cache)
    .desg_cache$n <- 0L
  }
  if (is.null(e)) .desg_cache$n <- .desg_cache$n + 1L
  assign(key, list(inputs = inputs, value = value), envir = .desg_cache)
  value
}
