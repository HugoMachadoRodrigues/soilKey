# v0.9.215: every app instance can serve the page's scripts and styles.
#
# Shiny registers a page dependency (/jquery-3.6.0/, /bootstrap-5.3.1/,
# /selectize-0.15.2/, ...) only when it renders the page in that R process. On
# Cloud Run, with more than one instance, the asset requests of one page load
# were not all routed to the instance that rendered it, and the others answered
# 404: the page came up unstyled, without Shiny. Seen live on 2026-10-08 in the
# request logs (one page load, 200s from one instance, 404s from the other).
# app.R now registers the page's dependencies at start-up.

.sd_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

test_that("a page's dependencies are servable before any page is rendered", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("DT")
  e <- .sd_env()
  theme <- bslib::bs_theme(version = 5, primary = "#7A5230")
  page <- bslib::page_navbar(
    title = "t", theme = theme,
    bslib::nav_panel("A", shiny::selectInput("x", "x", c("a", "b")),
                     DT::DTOutput("tab")))
  want <- c("^jquery-", "^bootstrap-", "^shiny-javascript-", "^selectize-",
            "^bslib-component-css-", "^datatables-binding-")
  # start from a process that has registered none of them
  for (n in grep(paste(want, collapse = "|"), names(shiny::resourcePaths()), value = TRUE))
    shiny::removeResourcePath(n)
  expect_true(e$sk_register_page_deps(page, theme))
  rp <- shiny::resourcePaths()
  for (w in want) expect_true(any(grepl(w, names(rp))), info = w)
  # the themed ones are the themed files, as the running app's page asks for
  # them (rendered here under the theme a running app would apply)
  old <- bslib::bs_global_set(theme)
  html <- as.character(shiny:::renderPage(page))
  bslib::bs_global_set(old)
  css <- regmatches(html, regexpr("selectize-[0-9.]+/[^\"]+\\.css", html))
  expect_length(css, 1L)
  dir <- rp[[sub("/.*", "", css)]]
  expect_true(file.exists(file.path(dir, sub("^[^/]+/", "", css))))
  # bslib's global theme is left as it was
  expect_null(bslib::bs_global_get())
})

test_that("app.R registers the page's dependencies before starting", {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  src <- readLines(file.path(d, "app.R"), warn = FALSE)
  src <- src[!grepl("^\\s*#", src)]
  reg <- grep("sk_register_page_deps(", src, fixed = TRUE)
  app <- grep("shinyApp(", src, fixed = TRUE)
  expect_length(reg, 1L)
  expect_true(reg < app)
})
