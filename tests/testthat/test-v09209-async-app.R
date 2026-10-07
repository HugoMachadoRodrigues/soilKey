# v0.9.209: the Pro app's slow handlers run off the Shiny process.
#
# A Cloud Run instance runs the app as one R process, so while one user's
# classification read SoilGrids for over a minute, or a Monte-Carlo run took
# 15-20 s, every other session on the instance froze. Those handlers are now
# shiny::ExtendedTask objects whose jobs run in background R processes (mirai
# daemons) through .sk_async() (inst/shiny/classify_app_pro/R/utils_async.R).
# These tests pin the contract of that helper and of the jobs it runs.

.async_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

.await <- function(p, timeout = 30) {
  out <- NULL; done <- FALSE
  promises::then(p, function(v) { out <<- v; done <<- TRUE },
                 function(e) { out <<- e; done <<- TRUE })
  t0 <- Sys.time()
  while (!done && difftime(Sys.time(), t0, units = "secs") < timeout)
    later::run_now(0.05)
  out
}


test_that("without workers a job runs here and still hands back a promise", {
  skip_if_not_installed("shiny")
  e <- .async_env()
  expect_false(e$.sk_async_ready())
  p <- e$.sk_async(function(x) x * 2, list(x = 21))
  expect_true(promises::is.promise(p))
  expect_identical(.await(p), 42)
  # an error comes back as the condition, so modules render it as before
  err <- .await(e$.sk_async(function() stop("no SoilGrids today")))
  expect_s3_class(err, "error")
  expect_identical(conditionMessage(err), "no SoilGrids today")
})

test_that("a job carries its arguments and the soilKey options, not its closure", {
  skip_if_not_installed("shiny")
  e <- .async_env()
  secret <- "captured"            # a local the job must not see
  job <- function() list(
    seen   = exists("secret", inherits = TRUE),
    engine = getOption("soilKey.diagnostic_engine"),
    or     = NULL %||% "app %||% is there")
  withr::local_options(soilKey.diagnostic_engine = "aqp")
  out <- .await(e$.sk_async(job))
  expect_false(out$seen)                     # no Shiny session shipped along
  expect_identical(out$engine, "aqp")        # Settings travel with the job
  expect_identical(out$or, "app %||% is there")
  # package functions keep their namespace
  f <- .await(e$.sk_async(soilKey::make_ferralsol_canonical))
  expect_s3_class(f, "PedonRecord")
})

test_that("an output waits while a task runs and shows other failures", {
  skip_if_not_installed("shiny")
  e <- .async_env()
  running <- list(result = function() shiny::req(FALSE))
  expect_identical(tryCatch(e$.sk_task_value(running),
                            shiny.silent.error = function(c) "waits"), "waits")
  died <- list(result = function() stop("worker died"))
  v <- e$.sk_task_value(died)
  expect_s3_class(v, "error")
  expect_identical(conditionMessage(v), "worker died")
  done <- list(result = function() "ok")
  expect_identical(e$.sk_task_value(done), "ok")
})

test_that("the jobs use only their arguments and package functions", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .async_env()
  # each job runs with nothing of the app around it, as in a worker
  ped <- make_ferralsol_canonical()
  res <- .await(e$.sk_async(e$.classify_job, list(
    pedon = ped, systems = "wrb2022", on_missing = "silent",
    include_familia = FALSE, include_family = FALSE, specifiers = FALSE,
    gapfill_methods = character(0))))
  expect_match(res$wrb$name, "Ferralsol")
  # a gap-fill that cannot run: classified as-is, with the reason attached
  res <- .await(e$.sk_async(e$.classify_job, list(
    pedon = ped, systems = "wrb2022", on_missing = "silent",
    include_familia = FALSE, include_family = FALSE, specifiers = FALSE,
    gapfill_methods = "no-such-method")))
  # classify_all() reports a failed gap-fill as a warning and a NULL system;
  # the app's fallback never saw it, and the card came back empty
  expect_match(res$wrb$name, "Ferralsol")
  expect_match(attr(res, "gapfill_error"), "unknown gapfill method", fixed = TRUE)
  g <- .await(e$.sk_async(e$.uncert_group_job, list(
    pedons = list(ped, make_luvisol_canonical()), n = 10,
    system = "wrb2022", level = "rsg")))
  expect_s3_class(g, "data.frame")
  expect_equal(nrow(g), 2L)
  b <- .await(e$.sk_async(e$.batch_classify, list(
    pedons = e$.batch_demo_pedons(2L), on_missing = "silent")))
  expect_s3_class(b, "data.frame")
  expect_true(all(c("wrb_name", "sibcs_name", "usda_name") %in% names(b)))
})

test_that("until a worker has connected, jobs run here", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("mirai")
  e <- .async_env()
  # stand-in for mirai's daemons: launched, none connected yet
  e$.sk_async_env$started <- TRUE
  e$.sk_async_env$verified <- FALSE
  expect_false(e$.sk_async_ready())
  expect_identical(.await(e$.sk_async(function() Sys.getpid())), Sys.getpid())
})

test_that("with workers, a slow job leaves this process free", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("mirai")
  e <- .async_env()
  withr::local_options(soilKey.app_workers = 1L)
  # a fresh R must find mirai, or its workers never start (and used to hang)
  skip_if_not(e$.sk_async_child_ok(), "mirai is not on a fresh R process's library path")
  expect_true(e$.sk_async_start())
  on.exit(e$.sk_async_stop(), add = TRUE)
  # jobs stay in this process until a worker has connected
  t0 <- Sys.time()
  while (!e$.sk_async_ready() && isTRUE(e$.sk_async_env$started) &&
         difftime(Sys.time(), t0, units = "secs") < 40)
    later::run_now(0.1)
  skip_if_not(e$.sk_async_ready(),
              "mirai workers did not start (is mirai on a child R process's library path?)")
  t0 <- Sys.time()
  p <- e$.sk_async(function(s) { Sys.sleep(s); Sys.getpid() }, list(s = 2))
  handed_over <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  expect_lt(handed_over, 1)                  # the caller is not held for 2 s
  pid <- .await(p)
  expect_true(is.numeric(pid))
  expect_false(identical(pid, Sys.getpid()))  # it ran in another process
})
