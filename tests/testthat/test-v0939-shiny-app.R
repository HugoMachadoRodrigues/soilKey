# Tests for run_classify_app() and the retirement of the legacy "classic" app
# (the single-page uploader was removed in v0.9.117 -- the Pro app is the only
# interface now; ui = "classic" is accepted for back-compat but deprecated).

test_that("run_classify_app errors clearly when a Pro dependency is missing", {
  pro_deps <- c("shiny", "bslib", "DT", "plotly", "shinyWidgets", "leaflet")
  if (all(vapply(pro_deps, requireNamespace, logical(1L), quietly = TRUE))) {
    skip("all Pro dependencies installed -- can't exercise the missing-package path")
  }
  expect_error(run_classify_app(launch.browser = FALSE),
               "shiny|bslib|DT|plotly|shinyWidgets|leaflet")
})

test_that("the legacy classic app no longer ships", {
  d <- system.file("shiny", "classify_app", package = "soilKey")
  # system.file() returns "" for a missing inst/ path; the dev tree must not
  # carry the directory either.
  expect_false(nzchar(d) && dir.exists(d))
  expect_false(dir.exists(file.path("inst", "shiny", "classify_app")))
})

test_that("ui = 'classic' warns and falls back to the Pro app", {
  # The fallback still needs the Pro deps; if they're missing the dependency
  # error fires first, which is fine -- we only assert the deprecation warning.
  pro_deps <- c("shiny", "bslib", "DT", "plotly", "shinyWidgets", "leaflet")
  if (!all(vapply(pro_deps, requireNamespace, logical(1L), quietly = TRUE))) {
    skip("Pro deps missing -- the dependency error pre-empts the launch path")
  }
  # We don't actually launch a server in tests; intercept runApp.
  testthat::local_mocked_bindings(runApp = function(...) invisible("mocked"),
                                  .package = "shiny")
  expect_warning(run_classify_app(ui = "classic", launch.browser = FALSE),
                 "classic.*retired|retired.*classic")
})
