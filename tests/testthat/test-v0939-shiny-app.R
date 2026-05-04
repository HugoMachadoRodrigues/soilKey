# Tests for v0.9.39 Shiny app + run_classify_app() wrapper.

test_that("run_classify_app errors clearly when shiny is missing", {
  if (requireNamespace("shiny", quietly = TRUE)) {
    skip("shiny is installed -- can't exercise the missing-package path")
  }
  expect_error(run_classify_app(), "shiny")
})

test_that("run_classify_app errors clearly when DT is missing", {
  if (!requireNamespace("shiny", quietly = TRUE) ||
        requireNamespace("DT", quietly = TRUE)) {
    skip("DT is installed or shiny is missing -- skip this branch")
  }
  expect_error(run_classify_app(), "DT")
})

test_that("Shiny app dir exists in inst/shiny/classify_app", {
  app_dir <- system.file("shiny", "classify_app", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir))
    app_dir <- file.path("inst", "shiny", "classify_app")
  expect_true(dir.exists(app_dir))
  expect_true(file.exists(file.path(app_dir, "app.R")))
})

test_that("Shiny app.R parses without syntax errors", {
  app_dir <- system.file("shiny", "classify_app", package = "soilKey")
  if (!nzchar(app_dir) || !dir.exists(app_dir))
    app_dir <- file.path("inst", "shiny", "classify_app")
  app_path <- file.path(app_dir, "app.R")
  expect_silent(parse(app_path))
})
