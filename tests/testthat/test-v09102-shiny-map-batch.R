# Tests for the v0.9.102 batch classification map module
# (classify_app_pro / mod_map_batch.R). Phase 2 of the mapping roadmap.
#
# We source the app modules into a throwaway env (as in the Phase-1 test) and
# exercise the pure helpers (.batch_parse_csv, .batch_classify) plus map_ui /
# map_server via testServer. No network is involved -- demo points come from
# canonical fixtures.

.mb_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d))
    d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.mb_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.mb_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE)) {
    sys.source(f, envir = env)
  }
  env
}


test_that("mod_map_batch.R ships and parses", {
  skip_on_cran()
  app_dir <- .mb_app_dir()
  expect_true(file.exists(file.path(app_dir, "R", "mod_map_batch.R")))
  expect_silent(parse(file.path(app_dir, "R", "mod_map_batch.R")))
})


test_that("map_batch_ui() builds a valid Shiny tag", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinyWidgets")
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")

  env <- .mb_source_modules()
  ui <- get("map_batch_ui", envir = env)("t")
  expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
})


test_that(".batch_parse_csv groups a long-format table into PedonRecords", {
  skip_on_cran()
  env   <- .mb_source_modules()
  parse_csv <- get(".batch_parse_csv", envir = env)

  df <- data.frame(
    profile_id  = c("A", "A", "B", "B"),
    lat         = c(-22.5, -22.5, -15.6, -15.6),
    lon         = c(-43.7, -43.7, -47.7, -47.7),
    top_cm      = c(0, 20, 0, 30),
    bottom_cm   = c(20, 60, 30, 80),
    designation = c("A", "Bw", "A", "Bt"),
    clay_pct    = c(45, 55, 20, 35),
    stringsAsFactors = FALSE
  )
  peds <- parse_csv(df)
  expect_length(peds, 2L)
  expect_s3_class(peds[[1]], "PedonRecord")
  expect_equal(peds[[1]]$site$id, "A")
  expect_equal(peds[[1]]$site$lat, -22.5)
  expect_equal(nrow(peds[[1]]$horizons), 2L)
  # B has two horizons too
  expect_equal(nrow(peds[[2]]$horizons), 2L)
})


test_that(".batch_parse_csv errors clearly on a missing id / coords", {
  skip_on_cran()
  env   <- .mb_source_modules()
  parse_csv <- get(".batch_parse_csv", envir = env)
  expect_error(parse_csv(data.frame(top_cm = 0, bottom_cm = 10)),
               "profile-id")
  expect_error(parse_csv(data.frame(id = "A", top_cm = 0, bottom_cm = 10)),
               "lat/lon")
})


test_that(".batch_classify returns one mappable row per demo pedon", {
  skip_on_cran()
  env <- .mb_source_modules()
  demo     <- get(".batch_demo_pedons", envir = env)
  classify <- get(".batch_classify", envir = env)

  peds <- demo(4L)
  expect_length(peds, 4L)
  df <- classify(peds, on_missing = "silent")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4L)
  expect_true(all(c("id", "lat", "lon", "wrb_class", "wrb_name",
                    "sibcs_class", "usda_class") %in% names(df)))
  # demo points carry valid coordinates
  expect_true(all(is.finite(df$lat) & is.finite(df$lon)))
  # at least one system produced a class for the first point
  expect_true(any(!is.na(c(df$wrb_class[1], df$sibcs_class[1],
                           df$usda_class[1]))))
})


test_that("map_batch_server classifies demo points and renders the table", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  skip_if_not_installed("leaflet")

  env <- .mb_source_modules()
  map_batch_server <- get("map_batch_server", envir = env)

  rv       <- shiny::reactiveValues(pedon = NULL)
  settings <- shiny::reactive(list(on_missing = "silent"))

  shiny::testServer(
    map_batch_server,
    args = list(rv = rv, settings = settings),
    {
      session$setInputs(source = "demo", n_demo = 4, system = "wrb")
      session$setInputs(run = 1)
      res <- results()
      expect_false(inherits(res, "error"))
      expect_equal(nrow(res), 4L)
      expect_equal(class_col(), "wrb_class")
      # force the DT output + count to render without error
      expect_error(output$table, NA)
    }
  )
})
