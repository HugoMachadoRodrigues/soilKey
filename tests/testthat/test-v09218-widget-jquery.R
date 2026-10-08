# v0.9.218: DT and leaflet widgets no longer bring their own jQuery.
#
# The page loads Shiny's jQuery from the resource path "jquery-<version>". DT
# and leaflet widgets also carry jquerylib's jQuery, and in the container image
# (Shiny 1.8) both are 3.6.0: rendering a table or a map re-pointed
# "jquery-3.6.0" to jquerylib's folder, which has jquery-3.6.0.min.js but no
# jquery.min.js. Every page that instance served afterwards got HTTP 404 for
# its jQuery and came up broken. Seen live on 2026-10-08 in the request logs:
# one instance answered 200 for /jquery-3.6.0/jquery.min.js, then 404 after a
# session had rendered the key-trace table.

.wj_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.wj_env <- function() {
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(.wj_dir(), "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

.wj_dep_names <- function(w)
  vapply(htmltools::findDependencies(htmlwidgets:::toHTML(w, standalone = FALSE)),
         function(d) d$name, character(1))

test_that("a widget jQuery of Shiny's version takes over the page's jQuery path", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  # the mechanism, reproduced with Shiny's own version: a second "jquery"
  # dependency of the same version, from a folder without jquery.min.js
  page_jq <- shiny:::jqueryDependency()
  page_jq$src <- list(file = system.file("www", "shared", package = "shiny"))
  page_jq$package <- NULL
  prefix <- paste0("jquery-", page_jq$version)
  shiny::createWebDependency(page_jq)
  dir <- shiny::resourcePaths()[[prefix]]
  expect_true(file.exists(file.path(dir, "jquery.min.js")))
  other <- file.path(tempdir(), "wj-jquerylib")
  dir.create(other, showWarnings = FALSE)
  writeLines("/* jquerylib */", file.path(other, paste0(prefix, ".min.js")))
  shiny::createWebDependency(htmltools::htmlDependency(
    "jquery", page_jq$version, src = c(file = other),
    script = paste0(prefix, ".min.js")))
  dir <- shiny::resourcePaths()[[prefix]]
  expect_false(file.exists(file.path(dir, "jquery.min.js")))
  shiny::createWebDependency(page_jq)   # leave the process as it was
})

test_that("the app's tables and maps carry no jQuery of their own", {
  skip_on_cran()
  skip_if_not_installed("DT"); skip_if_not_installed("leaflet")
  e <- .wj_env()
  expect_true("jquery" %in% .wj_dep_names(DT::datatable(data.frame(a = 1))))
  expect_true("jquery" %in% .wj_dep_names(leaflet::leaflet()))
  dt <- e$sk_datatable(data.frame(a = 1, status = "met"), rownames = FALSE) |>
    DT::formatStyle("status", backgroundColor = DT::styleEqual("met", "var(--sk-st-met)"))
  expect_false("jquery" %in% .wj_dep_names(dt))
  m <- e$sk_leaflet() |>
    leaflet::addProviderTiles("CartoDB.Positron") |>
    leaflet::addCircleMarkers(lng = -43.7, lat = -22.5) |>
    leaflet::setView(-43.7, -22.5, zoom = 7)
  expect_false("jquery" %in% .wj_dep_names(m))
  # the widgets still have their own bindings
  expect_true("datatables-binding" %in% .wj_dep_names(dt))
  expect_true("leaflet" %in% .wj_dep_names(m))
})

test_that("the app builds every table and map through the helpers", {
  src <- unlist(lapply(list.files(file.path(.wj_dir(), "R"), pattern = "\\.R$",
                                  full.names = TRUE), readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  src <- src[!grepl("sk_drop_jquery\\(DT::datatable|sk_drop_jquery\\(leaflet::leaflet", src)]
  expect_false(any(grepl("DT::datatable(", src, fixed = TRUE)))
  expect_false(any(grepl("leaflet::leaflet(", src, fixed = TRUE)))
})
