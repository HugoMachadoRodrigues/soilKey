# Tests for the v0.9.108 Pro-app polish: the new shared UI helpers
# (pro_spectrum_plot), the photo-confidence helpers, and the settings/Classify
# shared-state wiring (settings_server now takes an `rv`). These exercise the
# pure helpers sourced from the app's R/ directory; the interactive behaviour
# is covered by visual smoke.

.pro_app_dir_108 <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d))
    d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

# Source every app helper into a throwaway env (mirrors how Shiny auto-sources
# the R/ directory at launch).
.source_pro_app <- function() {
  app_dir <- .pro_app_dir_108()
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(app_dir, "R"), pattern = "[.]R$",
                       full.names = TRUE)) {
    sys.source(f, envir = env)
  }
  env
}

test_that("pro_spectrum_plot returns a plotly widget for a matrix and for NULL", {
  skip_if_not_installed("plotly")
  env <- .source_pro_app()
  m <- matrix(stats::runif(4 * 6), nrow = 4,
              dimnames = list(NULL, paste0(seq(400, 2400, length.out = 6), "nm")))
  p_mat  <- env$pro_spectrum_plot(m, designations = c("A", "Bw1", "Bw2", "C"))
  p_null <- env$pro_spectrum_plot(NULL)
  p_empty <- env$pro_spectrum_plot(matrix(numeric(0), nrow = 0, ncol = 0))
  expect_s3_class(p_mat,  "plotly")
  expect_s3_class(p_null, "plotly")
  expect_s3_class(p_empty, "plotly")
})

test_that("pro_spectrum_plot tolerates non-numeric column names", {
  skip_if_not_installed("plotly")
  env <- .source_pro_app()
  m <- matrix(stats::runif(2 * 3), nrow = 2,
              dimnames = list(NULL, c("band_a", "band_b", "band_c")))
  expect_s3_class(env$pro_spectrum_plot(m), "plotly")
})

test_that(".photo_confidence_grade maps confidence onto the A-E ladder", {
  env <- .source_pro_app()
  expect_equal(env$.photo_confidence_grade(0.95), "A")
  expect_equal(env$.photo_confidence_grade(0.75), "B")
  expect_equal(env$.photo_confidence_grade(0.60), "C")
  expect_equal(env$.photo_confidence_grade(0.45), "D")
  expect_equal(env$.photo_confidence_grade(0.10), "E")
  expect_true(is.na(env$.photo_confidence_grade(NA)))
  expect_true(is.na(env$.photo_confidence_grade(NULL)))
})

test_that(".photo_mean_confidence reads munsell rows from the provenance ledger", {
  env <- .source_pro_app()
  # No pedon / no provenance -> NA.
  expect_true(is.na(env$.photo_mean_confidence(NULL)))
  p <- make_ferralsol_canonical()
  expect_true(is.na(env$.photo_mean_confidence(p)))   # nothing extracted yet
  # A fake provenance ledger with two munsell VLM rows averages their conf.
  p$provenance <- data.frame(
    horizon_idx = c(1L, 1L, 2L),
    attribute   = c("munsell_hue_moist", "munsell_value_moist", "clay_pct"),
    source      = c("extracted_vlm", "extracted_vlm", "measured"),
    confidence  = c(0.6, 0.8, NA_real_),
    notes       = NA_character_,
    stringsAsFactors = FALSE
  )
  expect_equal(env$.photo_mean_confidence(p), 0.7)
})

test_that("settings_server takes a shared rv argument (v0.9.108 wiring)", {
  env <- .source_pro_app()
  expect_true("rv" %in% names(formals(env$settings_server)))
})
