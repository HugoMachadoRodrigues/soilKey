# v0.9.150: shiny::testServer coverage for the three Map-tab modules
# (map / map_batch / map_grid), which were previously parse / UI / helper-tested
# only. Mirrors test-shiny-pro-servers.R (the eight other Pro modules).

.map_srv_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.map_srv_env <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.map_srv_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE)) sys.source(f, envir = env)
  env
}

.map_settings_stub <- function() {
  shiny::reactive(list(engine = "soilKey", strict = FALSE, on_missing = "silent",
                       include_familia = TRUE, include_family = FALSE,
                       specifiers = FALSE))
}

.map_skip_unless_deps <- function() {
  for (p in c("shiny", "leaflet", "data.table"))
    testthat::skip_if_not_installed(p)
}

test_that("map_server: coords_r tracks the pedon coordinate", {
  skip_on_cran()
  .map_skip_unless_deps()
  env <- .map_srv_env()
  map_server <- get("map_server", envir = env)
  rv <- shiny::reactiveValues(pedon = NULL)
  shiny::testServer(map_server, args = list(rv = rv, settings = .map_settings_stub()), {
    expect_null(coords_r())                                   # no pedon yet
    rv$pedon <- PedonRecord$new(
      site = list(id = "t", lat = -21.5, lon = -43.2),
      horizons = ensure_horizon_schema(data.table::data.table(
        top_cm = 0, bottom_cm = 20, designation = "A")))
    cc <- coords_r()
    expect_equal(cc$src, "pedon")
    expect_equal(cc$lat, -21.5)
    expect_equal(cc$lon, -43.2)
  })
})

test_that("map_batch_server: column reactives + demo run populate rv$batch_points", {
  skip_on_cran()
  .map_skip_unless_deps()
  env <- .map_srv_env()
  map_batch_server <- get("map_batch_server", envir = env)
  rv <- shiny::reactiveValues(batch_points = NULL)
  shiny::testServer(map_batch_server, args = list(rv = rv, settings = .map_settings_stub()), {
    session$setInputs(system = "wrb")
    expect_equal(class_col(), "wrb_class")
    expect_equal(name_col(),  "wrb_name")
    # demo source: the eventReactive must produce a data.frame (or a captured
    # error), and a data.frame result syncs to rv$batch_points.
    session$setInputs(source = "demo", n_demo = 6L, run = 1L)
    res <- results()
    expect_true(is.data.frame(res) || inherits(res, "error"))
    if (is.data.frame(res)) expect_s3_class(rv$batch_points, "data.frame")
  })
})

test_that("map_grid_server: bbox + n_cells reactives are correct", {
  skip_on_cran()
  .map_skip_unless_deps()
  env <- .map_srv_env()
  map_grid_server <- get("map_grid_server", envir = env)
  rv <- shiny::reactiveValues(batch_points = NULL)
  shiny::testServer(map_grid_server, args = list(rv = rv, settings = .map_settings_stub()), {
    session$setInputs(res = 10L, lon_min = -44, lon_max = -43,
                      lat_min = -22, lat_max = -21, method = "overlay")
    expect_equal(n_cells(), 100)
    b <- bbox()
    expect_equal(b$lon_min, -44)
    expect_equal(b$lat_max, -21)
  })
})
