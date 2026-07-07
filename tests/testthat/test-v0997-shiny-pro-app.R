# Tests for the v0.9.97 professional Shiny app (classify_app_pro) and the
# extended run_classify_app() wrapper.

# Resolve the pro app directory from an installed package or a dev checkout.
.pro_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d))
    d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

test_that("classify_app_pro directory ships app.R and its R/ modules", {
  app_dir <- .pro_app_dir()
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
  expect_true(dir.exists(file.path(app_dir, "R")))
})

test_that("every classify_app_pro source file parses without syntax errors", {
  app_dir <- .pro_app_dir()
  files <- c(file.path(app_dir, "app.R"),
             list.files(file.path(app_dir, "R"), pattern = "\\.R$",
                        full.names = TRUE))
  expect_gt(length(files), 1L)
  for (f in files) {
    expect_silent(parse(f))
  }
})

test_that("classify_app_pro ships the twelve expected modules", {
  app_dir <- .pro_app_dir()
  mods <- list.files(file.path(app_dir, "R"), pattern = "^mod_.*\\.R$")
  # v0.9.176: the Photo tab became the "Talk to soilKey Pro" chat (mod_chat.R);
  # mod_photo.R stays for its helpers (.photo_mock_munsell, the demo photo) that
  # the chat's photo->Munsell fold-in reuses -- so twelve mod_* files now.
  expect_setequal(
    mods,
    c("mod_pedon.R", "mod_classify.R", "mod_photo.R", "mod_chat.R",
      "mod_spectra.R", "mod_acknowledgements.R", "mod_map.R", "mod_map_batch.R",
      "mod_map_grid.R", "mod_uncertainty.R", "mod_report.R", "mod_settings.R")
  )
})

test_that("run_classify_app rejects an unknown ui value", {
  expect_error(run_classify_app(ui = "bogus"), "arg")
})

test_that("run_classify_app errors clearly when a pro dependency is missing", {
  pro_deps <- c("shiny", "bslib", "DT", "plotly", "shinyWidgets")
  if (all(vapply(pro_deps, requireNamespace, logical(1L), quietly = TRUE))) {
    skip("all pro dependencies installed -- can't exercise the error path")
  }
  expect_error(run_classify_app(ui = "pro"),
               "shiny|bslib|DT|plotly|shinyWidgets")
})

test_that("module UI builders produce valid Shiny tags", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinyWidgets")
  skip_if_not_installed("DT")
  skip_if_not_installed("plotly")
  skip_if_not_installed("leaflet")  # map_ui() calls leaflet::leafletOutput()

  app_dir <- .pro_app_dir()
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(app_dir, "R"), pattern = "\\.R$",
                       full.names = TRUE)) {
    sys.source(f, envir = env)
  }
  for (builder in c("pedon_ui", "classify_ui", "photo_ui", "chat_ui",
                    "spectra_ui", "map_ui", "map_batch_ui", "map_grid_ui",
                    "uncertainty_ui", "report_ui", "settings_ui",
                    "acknowledgements_ui")) {
    ui <- get(builder, envir = env)("t")
    expect_true(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
  }
})
