# v0.9.224: the scripts that only a drawn widget brings are servable from
# start-up, on every app instance.
#
# Not every script of the app is in its page. A widget adds some when it is
# drawn: DT its DataTables core, plotly its plotly.js bundle, and a leaflet map
# with provider tiles the provider list and its plugin. The start-up
# registration of v0.9.215 rendered the page, so it left those out, and Shiny
# registered them only in the R process that drew the widget. On Cloud Run,
# with a second instance up, the browser's request for them could reach an
# instance that had drawn none. Seen on 2026-10-10 in the request log, on the
# live 0.9.222 and on a test revision: page and websocket on one instance,
# HTTP 404 for /leaflet-providers-2.0.0/leaflet-providers_2.0.0.js and
# /leaflet-providers-plugin-2.2.2/leaflet-providers-plugin.js on another, and a
# Map tab that was a grey box without tiles.

.wd_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.wd_env <- function() {
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(.wd_dir(), "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

.wd_prefixes <- function(w)
  vapply(htmltools::renderTags(w)$dependencies,
         function(d) paste0(d$name, "-", d$version), character(1))

test_that("what a drawn table, plot and map ask for is registered at start-up", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  skip_if_not_installed("DT"); skip_if_not_installed("plotly")
  skip_if_not_installed("leaflet")
  e <- .wd_env()
  old <- shiny::getShinyOption("bootstrapTheme")
  on.exit(shiny::shinyOptions(bootstrapTheme = old), add = TRUE)
  theme <- bslib::bs_theme(version = 5, primary = "#7A5230")
  page <- bslib::page_navbar(
    title = "t", theme = theme,
    bslib::nav_panel("A", DT::DTOutput("tab"), plotly::plotlyOutput("plot"),
                     leaflet::leafletOutput("map")))
  # what the three widgets bring when drawn under the app's theme
  old_bs <- bslib::bs_global_set(theme)
  want <- unique(c(
    .wd_prefixes(e$sk_datatable(data.frame(a = 1))),
    .wd_prefixes(plotly::plot_ly(x = 1, y = 1, type = "bar")),
    .wd_prefixes(e$sk_provider_tiles(e$sk_leaflet(), "CartoDB.Positron"))))
  bslib::bs_global_set(old_bs)
  want <- want[!grepl("^jquery-", want)]   # the page's jQuery serves them all
  # the ones the page does not have are among them
  for (k in c("^dt-core-", "^plotly-main-", "^leaflet-providers-[0-9]",
              "^leaflet-providers-plugin-"))
    expect_true(any(grepl(k, want)), info = k)
  # start from a process that has registered none of them
  for (n in intersect(want, names(shiny::resourcePaths())))
    shiny::removeResourcePath(n)
  expect_true(e$sk_register_page_deps(page, theme))
  rp <- shiny::resourcePaths()
  expect_identical(setdiff(want, names(rp)), character(0))
  # and the provider scripts are in the folders their prefixes point to
  w <- e$sk_provider_tiles(e$sk_leaflet(), "CartoDB.Positron")
  for (d in htmltools::renderTags(w)$dependencies) {
    if (!grepl("^leaflet-providers", d$name)) next
    dir <- rp[[paste0(d$name, "-", d$version)]]
    expect_true(all(file.exists(file.path(dir, unlist(d$script)))), info = d$name)
  }
  # a second call adds nothing and re-points nothing
  expect_identical(e$sk_register_widget_deps(), character(0))
  expect_identical(shiny::resourcePaths()[want], rp[want])
})

test_that("sk_provider_tiles() makes the call leaflet::addProviderTiles() makes", {
  skip_on_cran()
  skip_if_not_installed("leaflet")
  e <- .wd_env()
  ours   <- e$sk_provider_tiles(e$sk_leaflet(), "Esri.WorldImagery")
  theirs <- leaflet::addProviderTiles(e$sk_leaflet(), "Esri.WorldImagery")
  expect_identical(ours$x$calls, theirs$x$calls)
  expect_setequal(.wd_prefixes(ours), .wd_prefixes(theirs))
  expect_false(any(grepl("^jquery-", .wd_prefixes(ours))))
  expect_error(e$sk_provider_tiles(e$sk_leaflet(), "No.Such.Provider"),
               "Unknown tile provider")
})

test_that("a dependency in the temporary folder is served from a folder of its own", {
  skip_on_cran()
  skip_if_not_installed("shiny")
  # The leaflet of the container image writes the provider list to the R
  # session's temporary folder, and Shiny serves the folder a dependency is
  # in, whole: any file there could be requested under the prefix.
  e <- .wd_env()
  js <- file.path(tempdir(), "leaflet-providers_9.9.9.js")
  other <- file.path(tempdir(), "wd-not-for-serving.txt")
  writeLines("/* providers */", js); writeLines("x", other)
  on.exit(unlink(c(js, other)), add = TRUE)
  dep <- htmltools::htmlDependency("leaflet-providers", "9.9.9", src = tempdir(),
                                   script = basename(js), all_files = FALSE)
  own <- e$sk_own_dependency(dep)
  expect_false(identical(normalizePath(own$src$file), normalizePath(tempdir())))
  expect_identical(list.files(own$src$file), basename(js))
  shiny::createWebDependency(own)
  on.exit(shiny::removeResourcePath("leaflet-providers-9.9.9"), add = TRUE)
  dir <- shiny::resourcePaths()[["leaflet-providers-9.9.9"]]
  expect_true(file.exists(file.path(dir, basename(js))))
  expect_false(file.exists(file.path(dir, basename(other))))
  # a dependency that lives elsewhere is left as it is
  elsewhere <- htmltools::htmlDependency(
    "x", "1", src = system.file("www", package = "shiny"), script = "a.js")
  expect_identical(e$sk_own_dependency(elsewhere), elsewhere)
})

test_that("the app adds provider tiles only through the helper", {
  src <- unlist(lapply(list.files(file.path(.wd_dir(), "R"), pattern = "\\.R$",
                                  full.names = TRUE), readLines, warn = FALSE))
  src <- src[!grepl("^\\s*#", src)]
  # one call is left, in sk_provider_deps(), to read the list of dependencies
  expect_length(grep("leaflet::addProviderTiles(", src, fixed = TRUE), 1L)
  expect_gte(length(grep("sk_provider_tiles(", src, fixed = TRUE)), 5L)
})
