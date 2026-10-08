# v0.9.218: every session has the app's theme, on any instance.
#
# Shiny records the current theme (the "bootstrapTheme" option) when it serves a
# page while the app is running; a session copies the app's options when it
# opens. app.R is sourced before the app state exists, so the start-up render of
# v0.9.215 did not record it. On Cloud Run, a session opened on an instance that
# had not yet served a page had no theme: its DT tables took the default style,
# and selectize and bslib's component CSS were rendered unthemed, which
# re-pointed their resource paths to folders without the themed files (404s for
# the pages that instance served afterwards). Seen in the 0.9.218 test revision's
# request logs on 2026-10-08.

.th_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

test_that("registering the page's dependencies also makes its theme the app's theme", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  e <- .th_env()
  old <- shiny::getShinyOption("bootstrapTheme")
  on.exit(shiny::shinyOptions(bootstrapTheme = old), add = TRUE)
  shiny::shinyOptions(bootstrapTheme = NULL)
  theme <- bslib::bs_theme(version = 5, primary = "#7A5230")
  page <- bslib::page_navbar(title = "t", theme = theme,
                             bslib::nav_panel("A", shiny::selectInput("x", "x", "a")))
  expect_null(shiny::getShinyOption("bootstrapTheme"))
  e$sk_register_page_deps(page, theme)
  # outside a running app this is the process-wide option, which shiny::runApp()
  # copies into the app's options and every session copies from there
  expect_true(bslib::is_bs_theme(shiny::getShinyOption("bootstrapTheme")))
  expect_identical(bslib::bs_get_variables(shiny::getShinyOption("bootstrapTheme"), "primary"),
                   bslib::bs_get_variables(theme, "primary"))
})
