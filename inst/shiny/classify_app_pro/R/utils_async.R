# =============================================================================
# soilKey Pro -- slow work off the Shiny process.
#
# A Cloud Run instance runs this app as ONE R process. While a handler computes,
# that process answers no one, so every other session on the instance freezes
# until it is done. Measured on 2026-10-07 (on a laptop; Cloud Run's single vCPU
# is slower):
#
#   classification with SoilGrids gap-fill     78 s   (network)
#   Map point: class prior from SoilGrids      49 s   (network)
#   uncertainty, 50 runs + sensitivity         15 s
#   uncertainty over a group of 12 points      15 s
#   Map batch, 12 profiles                      4 s
#
# Those handlers are shiny::ExtendedTask objects whose work runs in background R
# processes (mirai daemons): the Shiny process hands the job over, keeps serving
# every session, and renders the result when it arrives. Without mirai (a local
# install that lacks it, the test suite, options(soilKey.app_workers = 0)) the
# job runs in this process as before, and the task still gets a promise, so the
# modules have one code path.
# =============================================================================

.sk_async_env <- new.env(parent = emptyenv())

# Start the background workers, once per R process. Two are enough for an
# instance whose CPU is a single vCPU: the point is that the Shiny process stays
# free, not parallel speed-up. Each idle worker holds about 90 MB.
.sk_async_start <- function(n = getOption("soilKey.app_workers", 2L)) {
  if (isTRUE(.sk_async_env$started)) return(invisible(TRUE))
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 1L ||
      !requireNamespace("mirai", quietly = TRUE) || !.sk_async_child_ok())
    return(invisible(FALSE))
  ok <- tryCatch({ mirai::daemons(n); TRUE }, error = function(e) FALSE)
  .sk_async_env$started  <- ok
  .sk_async_env$verified <- FALSE
  if (!ok) return(invisible(FALSE))
  # Jobs run here until a worker has connected, and if none has within 30 s
  # the workers are dropped. Checked from the event loop: nothing waits, and
  # app start-up is not delayed.
  waited <- 0
  check <- function() {
    if (!isTRUE(.sk_async_env$started)) return(invisible())
    up <- tryCatch(mirai::status()$connections, error = function(e) 0L)
    if (isTRUE(up >= 1L)) return(invisible(.sk_async_env$verified <- TRUE))
    waited <<- waited + 0.25
    if (waited >= 30) return(.sk_async_stop())
    later::later(check, 0.25)
  }
  check()
  invisible(TRUE)
}

# mirai's workers (and its dispatcher) are new R processes: they see the library
# paths of a fresh R session, not this session's .libPaths(). Where mirai is not
# on those paths, mirai::daemons() waits for them for ever, so ask a fresh R
# first (a fraction of a second, once per process).
.sk_async_child_ok <- function() {
  rscript <- file.path(R.home("bin"), "Rscript")
  code <- "quit(status = if (requireNamespace('mirai', quietly = TRUE)) 0 else 1)"
  isTRUE(tryCatch(
    system2(rscript, c("-e", shQuote(code)), stdout = FALSE, stderr = FALSE) == 0L,
    error = function(e) FALSE))
}

.sk_async_stop <- function() {
  if (isTRUE(.sk_async_env$started))
    try(mirai::daemons(0), silent = TRUE)
  .sk_async_env$started  <- FALSE
  .sk_async_env$verified <- FALSE
  invisible(NULL)
}

.sk_async_ready <- function() {
  isTRUE(.sk_async_env$started) && isTRUE(.sk_async_env$verified)
}

# The environment a job function runs in: global, plus the app's `%||%` (base R
# only has it from 4.4.0) and any app helpers the job names. Helpers travel the
# same way as the job, without their own environment, so they too may use only
# their arguments, package functions and each other.
.sk_job_env <- function(helpers = list()) {
  e  <- new.env(parent = globalenv())
  or <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
  environment(or) <- baseenv()
  assign("%||%", or, envir = e)
  for (n in names(helpers)) {
    h <- helpers[[n]]
    if (is.function(h)) environment(h) <- e      # constants travel as they are
    assign(n, h, envir = e)
  }
  e
}

# The soilKey options this session chose on the Settings tab (the diagnostic
# engine, strict mode), or an empty list outside a session. They live in the
# session, not in options(): R options are per PROCESS, and a Cloud Run
# instance serves several visitors from one process -- until v0.9.211 one
# visitor's engine or strict-mode switch changed every other visitor's results.
.sk_session_opts <- function() {
  s <- if (requireNamespace("shiny", quietly = TRUE)) shiny::getDefaultReactiveDomain()
  o <- if (!is.null(s)) tryCatch(s$userData$sk_opts, error = function(e) NULL)
  if (is.list(o)) o else list()
}

# Evaluate `expr` -- a classification in this process -- under this session's
# options, restored afterwards. Safe because R runs one session's handler at a
# time and these calls do not yield to the event loop.
.sk_with_session_opts <- function(expr) {
  o <- .sk_session_opts()
  if (!length(o)) return(force(expr))
  old <- options(o)
  on.exit(options(old), add = TRUE)
  force(expr)
}

# Run fun(<args>) in a worker; returns a promise of its value. An error inside
# the job comes back as the condition object, so a failed job renders the way a
# failed synchronous call did (the modules already show error objects).
#
# `fun` travels WITHOUT its enclosing environment (unless it belongs to a
# package): it may use only its arguments, package functions (soilKey::...) and
# `%||%`, never other app helpers or reactives, or serialising it would drag the
# Shiny session along. The soilKey.* options travel with the job, this
# session's Settings (.sk_session_opts()) over the process defaults: a worker
# would not see either.
.sk_async <- function(fun, args = list(), helpers = list()) {
  if (!isNamespace(environment(fun))) environment(fun) <- .sk_job_env(helpers)
  opts <- utils::modifyList(options()[grepl("^soilKey\\.", names(options()))],
                           .sk_session_opts())
  run <- function(fun, args, opts) {
    old <- options(opts)
    on.exit(options(old), add = TRUE)
    tryCatch(do.call(fun, args), error = function(e) e)
  }
  environment(run) <- globalenv()
  if (!.sk_async_ready())
    return(promises::promise_resolve(run(fun, args, opts)))
  mirai::mirai(run(fun, args, opts), run = run, fun = fun, args = args,
               opts = opts)
}

# A task's value for an output. While the task runs (or before its first run)
# result() raises Shiny's silent error, which must reach Shiny untouched so the
# output waits; any other failure (a worker that died, say) becomes a condition
# object the modules render as an error.
.sk_task_value <- function(task) {
  # One handler, re-signalling from it: with a separate shiny.silent.error
  # handler the outer error handler caught the re-signal and returned it.
  tryCatch(task$result(), error = function(e) {
    if (inherits(e, "shiny.silent.error")) stop(e)
    e
  })
}
