# Tests for the bundled Photo / Spectra demo assets and their wiring (v0.9.167).
# The demo assets let the Photo and Spectra tabs be exercised with no user data:
# an illustrative profile image and a Vis-NIR spectrum matching the example
# Ferralsol's 5 horizons.

.pro_app_www <- function()
  system.file("shiny", "classify_app_pro", "www", package = "soilKey")

test_that("the demo assets are bundled", {
  www <- .pro_app_www()
  skip_if(www == "", "app not installed")
  expect_true(file.exists(file.path(www, "demo_profile.jpg")))
  expect_true(file.exists(file.path(www, "demo_spectrum.csv")))
})

test_that("the demo spectrum matches the example pedon and is numeric", {
  www <- .pro_app_www()
  skip_if(www == "", "app not installed")
  m <- as.matrix(utils::read.csv(file.path(www, "demo_spectrum.csv"),
                                 check.names = FALSE))
  storage.mode(m) <- "double"
  # one row per horizon of the example Ferralsol
  expect_equal(nrow(m), nrow(make_ferralsol_canonical()$horizons))
  expect_false(anyNA(m))
  # column names are wavelengths in the Vis-NIR-SWIR range
  wl <- suppressWarnings(as.numeric(colnames(m)))
  expect_false(anyNA(wl))
  expect_true(min(wl) >= 350 && max(wl) <= 2500)
})

test_that("spectra_server attaches the demo spectrum on demand", {
  skip_on_cran()
  for (pkg in c("shiny", "bslib", "DT", "plotly"))
    skip_if_not_installed(pkg)
  www <- .pro_app_www()
  skip_if(www == "", "app not installed")

  env <- new.env(parent = globalenv())
  app_r <- system.file("shiny", "classify_app_pro", "R", package = "soilKey")
  for (f in list.files(app_r, pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = env)
  srv <- get("spectra_server", envir = env)
  environment(srv) <- env  # resolve i18n() / .pro_demo_asset()

  rv <- shiny::reactiveValues(pedon = make_ferralsol_canonical())
  shiny::testServer(srv, args = list(rv = rv), {
    expect_null(rv$pedon$spectra$vnir)
    session$setInputs(demo_spectrum = 1L)
    expect_false(is.null(rv$pedon$spectra$vnir))
    expect_equal(nrow(rv$pedon$spectra$vnir),
                 nrow(rv$pedon$horizons))
  })
})

test_that("the demo spectrum sizes to the pedon's horizon count (v0.9.170)", {
  # Regression for "Spectrum has 5 rows but the pedon has 7 horizons": the
  # bundled demo is 5 rows, but a pedon can have any count (e.g. after a photo
  # extraction appends horizons). .pro_demo_spectrum() recycles rows to match.
  www <- .pro_app_www()
  skip_if(www == "", "app not installed")
  env <- new.env(parent = globalenv())
  app_r <- system.file("shiny", "classify_app_pro", "R", package = "soilKey")
  for (f in list.files(app_r, pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = env)
  ds <- get(".pro_demo_spectrum", envir = env)
  for (n in c(1L, 3L, 5L, 7L, 12L)) {
    m <- ds(n)
    expect_equal(nrow(m), n)
    expect_gt(ncol(m), 100L)
    expect_false(anyNA(m))
  }
})
