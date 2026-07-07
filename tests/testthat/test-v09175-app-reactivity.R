# Regression guards for v0.9.175 app fixes.
#
# The headline bug: PedonRecord is R6 (a reference), so the app's
# `rv$pedon$<mutate>; rv$pedon <- rv$pedon` idiom self-assigned the SAME
# environment, which reactiveValues suppresses as identical -> downstream
# outputs (spectrum plot, status) never re-rendered. The fix clones to a fresh
# reference. These tests drive the real spectra module and assert the reactive
# actually fires after the demo attach.

.appfix_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.appfix_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.appfix_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE))
    sys.source(f, envir = env)
  env
}


test_that("spectra demo attach fires the reactive (R6 self-assign regression)", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("plotly")

  env            <- .appfix_source_modules()
  spectra_server <- get("spectra_server", envir = env)

  pr <- soilKey::PedonRecord$new(
    site = list(id = "spec-test", lat = -22.5, lon = -43.7, crs = 4326),
    horizons = data.frame(
      designation = c("A", "AB", "B"),
      top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 100)))
  rv <- shiny::reactiveValues(pedon = pr)

  shiny::testServer(spectra_server, args = list(rv = rv), {
    # before: no spectra attached
    expect_null(rv$pedon$spectra)
    session$setInputs(demo_spectrum = 1)
    # the demo matrix is attached to a FRESH pedon reference...
    expect_false(is.null(rv$pedon$spectra))
    expect_false(is.null(rv$pedon$spectra$vnir))
    expect_equal(nrow(rv$pedon$spectra$vnir), 3L)   # one row per horizon
    # ...and the status output re-rendered to reflect it (the regression:
    # with the self-assign it stayed on the "none" string).
    expect_match(output$status, "attached|anexad|matrix|matriz", ignore.case = TRUE)
    # the has_pedon flag drives the plot's conditionalPanel
    expect_true(output$has_pedon)
  })
})


test_that("the map click handler updates the pedon coordinate reactively", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")

  env        <- .appfix_source_modules()
  map_server <- get("map_server", envir = env)
  pr <- soilKey::PedonRecord$new(
    site = list(id = "click-test", lat = -22.5, lon = -43.7, crs = 4326))
  rv       <- shiny::reactiveValues(pedon = pr)
  settings <- shiny::reactive(list(on_missing = "silent"))

  shiny::testServer(map_server, args = list(rv = rv, settings = settings), {
    session$setInputs(mode = "point", show_soilgrids = FALSE)
    session$setInputs(map_click = list(lat = -15.6, lng = -47.7))
    # the clone-based handler must actually move the point (a self-assign of the
    # shared R6 ref would leave coords_r() unchanged)
    expect_equal(coords_r()$lat, -15.6)
    expect_equal(coords_r()$lon, -47.7)
    expect_equal(rv$pedon$site$lat, -15.6)
  })
})
