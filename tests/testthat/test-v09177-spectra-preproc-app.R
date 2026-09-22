# Tests for the v0.9.177 Spectra-tab preprocessing UI (mod_spectra.R): the live
# treated-spectrum reactive and the debounced record used by the report.

.spp_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}
.spp_source_modules <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.spp_app_dir(), "R"), pattern = "\\.R$",
                       full.names = TRUE))
    sys.source(f, envir = env)
  env
}
.spp_pedon <- function() {
  pr <- soilKey::PedonRecord$new(
    site = list(id = "spp", lat = -22.5, lon = -43.7, crs = 4326),
    horizons = data.frame(designation = c("A", "B", "C"),
                          top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120)))
  X <- matrix(runif(3 * 120, 0.2, 0.5), nrow = 3)
  colnames(X) <- seq(400, 2400, length.out = 120)
  pr$spectra <- list(vnir = X)
  pr
}


test_that("the treated-spectrum reactive applies the chosen pipeline", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("plotly")
  env            <- .spp_source_modules()
  spectra_server <- get("spectra_server", envir = env)
  rv <- shiny::reactiveValues(pedon = .spp_pedon())

  shiny::testServer(spectra_server, args = list(rv = rv), {
    session$setInputs(pp_absorbance = TRUE, pp_smooth = TRUE, pp_deriv = "1",
                      pp_window = 11, pp_poly = 2)
    tr <- treated()
    expect_false(is.null(tr))
    expect_equal(tr$steps[1], "Reflectance")
    expect_true(any(grepl("Absorbance",     tr$steps)))
    expect_true(any(grepl("SG smoothing",   tr$steps)))
    expect_true(any(grepl("1st derivative", tr$steps)))
    # two SG passes trim (w-1) each -> 120 - 20 bands, axis stays aligned
    expect_equal(ncol(tr$X), 100L)
    expect_equal(length(tr$wavelengths), ncol(tr$X))
    expect_true(all(is.finite(tr$X)))
  })
})


test_that("the pipeline is recorded in rv$spectra_pp for the report (debounced)", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("plotly")
  env            <- .spp_source_modules()
  spectra_server <- get("spectra_server", envir = env)
  rv <- shiny::reactiveValues(pedon = .spp_pedon())

  shiny::testServer(spectra_server, args = list(rv = rv), {
    session$setInputs(pp_absorbance = TRUE, pp_smooth = FALSE, pp_deriv = "2",
                      pp_window = 11, pp_poly = 2)
    session$elapse(800)                       # fire the 600 ms debounce
    expect_false(is.null(rv$spectra_pp))
    expect_true(any(grepl("2nd derivative", rv$spectra_pp$steps)))
    expect_true(isTRUE(rv$spectra_pp$opts$absorbance))
  })
})
