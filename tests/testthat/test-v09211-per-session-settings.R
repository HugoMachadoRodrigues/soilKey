# v0.9.211: the Settings tab's engine and strict mode belong to the session.
#
# They were written with options(), and R options are per process: on the
# hosted app (several visitors per R process) one visitor switching the
# diagnostic engine to "aqp", or turning strict mode on, changed the
# classifications every other visitor on that instance got. They now live in
# session$userData and are applied around each classification the session
# runs: background jobs (.sk_async) and calls in the Shiny process
# (.sk_with_session_opts).

.pss_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

# A profile whose class depends on the engine: CEC ~18 cmolc per kg clay sits
# between the soilkey (16) and aqp (20) limits of the ferralic horizon.
.engine_sensitive_pedon <- function() {
  p <- make_ferralsol_canonical()
  h <- p$horizons
  h$cec_cmol <- round(h$clay_pct * 0.18, 2)
  PedonRecord$new(site = p$site, horizons = h)
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


test_that("Settings keeps engine and strict mode in the session, not in options()", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("shinyWidgets")
  e <- .pss_env()
  withr::local_options(soilKey.diagnostic_engine = NULL, soilKey.rsg_strict = NULL)
  rv <- shiny::reactiveValues(pedon = NULL, include_family = FALSE, specifiers = FALSE)
  shiny::testServer(e$settings_server, args = list(rv = rv), {
    session$setInputs(engine = "aqp", strict = TRUE, on_missing = "silent",
                      include_familia = TRUE, include_family = FALSE,
                      specifiers = FALSE)
    expect_identical(session$userData$sk_opts$soilKey.diagnostic_engine, "aqp")
    expect_true(session$userData$sk_opts$soilKey.rsg_strict)
  })
  # nothing leaked into the process
  expect_null(getOption("soilKey.diagnostic_engine"))
  expect_null(getOption("soilKey.rsg_strict"))
})

test_that("two sessions with different engines classify the same profile differently", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  e <- .pss_env()
  withr::local_options(soilKey.diagnostic_engine = NULL)
  ped <- .engine_sensitive_pedon()
  settings <- shiny::reactive(list(engine = "soilkey", strict = FALSE,
                                   on_missing = "silent", include_familia = TRUE,
                                   include_family = FALSE, specifiers = FALSE))
  classify_in <- function(engine) {
    out <- NULL
    rv <- shiny::reactiveValues(pedon = ped, include_family = FALSE, specifiers = FALSE)
    shiny::testServer(e$classify_server, args = list(rv = rv, settings = settings), {
      # what settings_server writes for this session
      session$userData$sk_opts <- list(soilKey.diagnostic_engine = engine)
      session$setInputs(systems = c("wrb2022", "sibcs", "usda"), run = 1)
      .settle(session)
      out <<- session$returned()
    })
    out
  }
  a <- classify_in("aqp")
  b <- classify_in("soilkey")
  expect_identical(a$wrb$rsg_or_order, "Ferralsols")
  # v0.9.220: Cambisols, not Nitisols -- the profile has no Fe-ox and no shiny
  # ped faces, which the nitic horizon needs (WRB 2022 Ch 3.1.22)
  expect_identical(b$wrb$rsg_or_order, "Cambisols")
  expect_false(identical(a$usda$name, b$usda$name))
  expect_null(getOption("soilKey.diagnostic_engine"))   # neither touched it
})

test_that("background jobs and in-process calls use their own session's settings", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  e <- .pss_env()
  withr::local_options(soilKey.diagnostic_engine = "soilkey")
  a <- shiny::MockShinySession$new()
  a$userData$sk_opts <- list(soilKey.diagnostic_engine = "aqp")
  b <- shiny::MockShinySession$new()          # no Settings choice: the default
  job <- function() getOption("soilKey.diagnostic_engine")
  pa <- shiny::withReactiveDomain(a, e$.sk_async(job))
  pb <- shiny::withReactiveDomain(b, e$.sk_async(job))
  expect_identical(.await(pa), "aqp")
  expect_identical(.await(pb), "soilkey")
  # a classification in this process (the Assistant's context, the reports)
  ped <- .engine_sensitive_pedon()
  ctx_a <- shiny::withReactiveDomain(a, e$.chat_pedon_context(ped, NULL))
  ctx_b <- shiny::withReactiveDomain(b, e$.chat_pedon_context(ped, NULL))
  expect_identical(ctx_a$results$wrb$rsg_or_order, "Ferralsols")
  expect_identical(ctx_b$results$wrb$rsg_or_order, "Cambisols")   # v0.9.220
  expect_identical(getOption("soilKey.diagnostic_engine"), "soilkey")  # restored
})

test_that("nothing in the app writes the engine or strict mode with options()", {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  src <- unlist(lapply(c(list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE),
                         file.path(d, "app.R")), readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  expect_false(any(grepl("options\\(soilKey\\.(diagnostic_engine|rsg_strict)\\s*=", src)))
})
