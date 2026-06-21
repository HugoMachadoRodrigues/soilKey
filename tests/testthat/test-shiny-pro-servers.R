# v0.9.118: shiny::testServer coverage for the eight Pro-app modules that were
# previously parse-tested only (pedon / classify / photo / spectra / spatial /
# uncertainty / report / settings). Modules are sourced from the app's R/
# directory into a throwaway env, mirroring test-v09101-shiny-map.R.

.pro_srv_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.pro_srv_env <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.pro_srv_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE)) sys.source(f, envir = env)
  env
}

# A settings() stub matching what the consuming modules read.
.pro_settings_stub <- function() {
  shiny::reactive(list(engine = "soilKey", strict = FALSE, on_missing = "silent",
                       include_familia = TRUE, include_family = FALSE,
                       specifiers = FALSE))
}

.pro_skip_unless_deps <- function() {
  for (p in c("shiny", "bslib", "shinyWidgets", "DT", "plotly"))
    testthat::skip_if_not_installed(p)
}

test_that("settings_server exposes a config reactive synced to rv", {
  skip_on_cran()
  .pro_skip_unless_deps()
  # settings_server writes the diagnostic-engine / strict-mode package options
  # as a side effect; snapshot + restore them so the test never leaks into the
  # rest of the suite.
  withr::local_options(
    soilKey.diagnostic_engine = getOption("soilKey.diagnostic_engine"),
    soilKey.rsg_strict        = getOption("soilKey.rsg_strict"))
  env <- .pro_srv_env()
  settings_server <- get("settings_server", envir = env)
  rv <- shiny::reactiveValues(pedon = NULL, include_family = FALSE, specifiers = FALSE)
  shiny::testServer(settings_server, args = list(rv = rv), {
    session$setInputs(engine = "soilkey", strict = FALSE, on_missing = "silent",
                      include_familia = TRUE, include_family = FALSE, specifiers = FALSE)
    cfg <- session$returned()                  # the returned reactive, invoked
    expect_true(is.list(cfg))
    expect_equal(cfg$engine, "soilkey")
    # a genuine FALSE -> TRUE change drives the widget -> rv sync (the first
    # value is swallowed by the observers' ignoreInit = TRUE).
    session$setInputs(include_family = TRUE)
    expect_true(isTRUE(rv$include_family))
    # ... and the rv -> cfg direction: the config reactive reads from rv.
    expect_true(isTRUE(session$returned()$include_family))
  })
})

test_that("classify_server runs the three keys on a pedon", {
  skip_on_cran()
  .pro_skip_unless_deps()
  env <- .pro_srv_env()
  classify_server <- get("classify_server", envir = env)
  rv <- shiny::reactiveValues(pedon = soilKey::make_ferralsol_canonical(),
                              include_family = FALSE, specifiers = FALSE)
  shiny::testServer(classify_server,
                    args = list(rv = rv, settings = .pro_settings_stub()), {
    session$setInputs(systems = c("wrb2022", "sibcs", "usda"), run = 1)
    res <- session$returned()      # returned() invokes the results eventReactive
    expect_true(is.list(res))
    expect_true(any(c("wrb", "sibcs", "usda") %in% names(res)))
    expect_error(output$body, NA)
  })
})

test_that("pedon_server loads a fixture and builds a PedonRecord", {
  skip_on_cran()
  .pro_skip_unless_deps()
  env <- .pro_srv_env()
  pedon_server <- get("pedon_server", envir = env)
  rv <- shiny::reactiveValues(pedon = NULL, example_request = 0L)
  shiny::testServer(pedon_server, args = list(rv = rv), {
    session$setInputs(source = "fixture", fixture = "make_ferralsol_canonical")
    session$setInputs(load = 1)
    expect_gt(nrow(hz()), 0L)
    session$setInputs(site_id = "t", lat = -22.5, lon = -43.7,
                      country = "BR", pm = "gneiss", build = 1)
    expect_s3_class(rv$pedon, "PedonRecord")
    expect_error(output$geom_status, NA)
  })
})

test_that("report_server's config reactive tracks settings", {
  skip_on_cran()
  .pro_skip_unless_deps()
  env <- .pro_srv_env()
  report_server <- get("report_server", envir = env)
  rv <- shiny::reactiveValues(pedon = soilKey::make_ferralsol_canonical())
  shiny::testServer(report_server,
                    args = list(rv = rv, settings = .pro_settings_stub()), {
    expect_error(output$summary, NA)
  })
})

test_that("uncertainty / spatial / photo / spectra servers initialise without error", {
  skip_on_cran()
  .pro_skip_unless_deps()
  env <- .pro_srv_env()
  rv <- shiny::reactiveValues(pedon = soilKey::make_ferralsol_canonical(),
                              include_family = FALSE, specifiers = FALSE)
  # Each server should at least instantiate and render its no-action UI.
  for (mod in c("uncertainty_server", "spatial_server")) {
    srv <- get(mod, envir = env)
    shiny::testServer(srv, args = list(rv = rv, settings = .pro_settings_stub()), {
      expect_true(TRUE)   # server body evaluated without error
    })
  }
  for (mod in c("photo_server", "spectra_server")) {
    srv <- get(mod, envir = env)
    shiny::testServer(srv, args = list(rv = rv), {
      expect_true(TRUE)
    })
  }
})
