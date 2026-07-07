# Tests for the v0.9.178 Map-tab fixes: continuous overlay recode, a multi-class
# demo raster, and the point-mode system-toggle live re-query.

.mfx_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}
.mfx_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.mfx_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE))
    sys.source(f, envir = env)
  env
}
.mfx_skip_if_no_proj <- function() {
  testthat::skip_if_not_installed("terra")
  ok <- tryCatch({
    suppressWarnings(suppressMessages(terra::rast(
      nrows = 1, ncols = 1, ext = terra::ext(0, 1, 0, 1), crs = "EPSG:4326")))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("PROJ database not available")
}
.mfx_demo <- function() file.path(.mfx_app_dir(), "www", "soilgrids_wrb_demo.tif")


test_that(".overlay_recode maps a demo crop to contiguous ids + a tight LUT", {
  skip_on_cran(); .mfx_skip_if_no_proj()
  env    <- .mfx_source_modules()
  recode <- get(".overlay_recode", envir = env)
  r  <- terra::rast(.mfx_demo())
  rc <- terra::crop(r, terra::ext(-47, -41, -25, -19))   # window around Rio
  rr <- recode(rc)
  expect_false(is.null(rr))
  expect_true(all(c("id", "class") %in% names(rr$lut)))
  expect_gt(nrow(rr$lut), 3L)                            # several classes present
  vals <- sort(unique(stats::na.omit(terra::values(rr$raster, mat = FALSE))))
  expect_equal(vals, seq_len(nrow(rr$lut)))             # ids are contiguous 1..n
})


test_that("the demo raster is multi-class within a small buffer at the demo point", {
  skip_on_cran(); .mfx_skip_if_no_proj()
  p <- soilKey::soil_classes_at_location(
    lat = -22.5, lon = -43.7, system = "wrb2022",
    buffer_m = 5000, source_url = .mfx_demo(), top_n = 8, verbose = FALSE)
  d <- as.data.frame(p$distribution)
  expect_gt(nrow(d), 1L)                                # was 1 (CM 100%) before
  expect_true(all(d$probability >= 0 & d$probability <= 1))
  expect_equal(sum(d$probability), 1, tolerance = 1e-6)
})


test_that("point-mode prior re-queries live when the system changes after a run", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("leaflet")
  .mfx_skip_if_no_proj()
  env        <- .mfx_source_modules()
  map_server <- get("map_server", envir = env)
  pr <- soilKey::PedonRecord$new(
    site = list(id = "mfx", lat = -22.5, lon = -43.7, crs = 4326))
  rv       <- shiny::reactiveValues(pedon = pr)
  settings <- shiny::reactive(list(on_missing = "silent"))

  shiny::testServer(map_server, args = list(rv = rv, settings = settings), {
    session$setInputs(mode = "point", show_soilgrids = FALSE, system = "wrb2022",
                      sg_source = "demo", source_url = .mfx_demo(),
                      buffer = 8000, topn = 5)
    session$setInputs(run_point = 1)
    p1 <- prior()
    expect_false(inherits(p1, "error"))
    d1 <- as.data.frame(p1$distribution)
    # switch the system WITHOUT pressing the button -> prior must re-fire
    session$setInputs(system = "sibcs")
    p2 <- prior()
    expect_false(inherits(p2, "error"))
    d2 <- as.data.frame(p2$distribution)
    # the SiBCS crosswalk relabels the same pixels, so the class names differ
    expect_false(identical(d1$rsg_name, d2$rsg_name))
  })
})
