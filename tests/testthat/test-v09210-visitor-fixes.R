# v0.9.210: what a visitor of the hosted Pro app saw wrong.
#
# 1. A "Download PDF" button handed over an HTML file (the web build has no
#    LaTeX). 2. The report downloads sat in a sidebar that is often collapsed.
# 3. In Portuguese, ~150 help texts and the table controls were English, and
#    the language was process-wide: one visitor's EN/PT choice switched every
#    other visitor on the instance. 4. The Map opened on a synthetic demo raster,
#    and reading the live SoilGrids one froze the process. 5. Spectral gap-fill
#    wrote placeholder values into the profile (no OSSL library exists here).

.vf_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}
.vf_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}


# ---- 3. language per session, and no English left in the Portuguese UI -------

test_that("the language comes from the session, not from the process", {
  skip_if_not_installed("shiny")
  e <- .vf_env()
  withr::local_options(soilKey.app_lang = "en")
  expect_identical(e$.sk_lang_from_query("?lang=pt"), "pt")
  expect_null(e$.sk_lang_from_query("?lang=xx"))
  expect_identical(e$.sk_lang_for(""), "en")              # the app default
  # a page built for ?lang=pt is Portuguese, whatever the process option says
  expect_identical(e$.sk_with_lang("pt", e$i18n("nav.pedon")),
                   e$i18n("nav.pedon", lang = "pt"))
  expect_identical(e$i18n("nav.pedon"), e$i18n("nav.pedon", lang = "en"))
  # two sessions, two languages, neither changes the other
  a <- shiny::MockShinySession$new(); a$userData$sk_lang <- "pt"
  b <- shiny::MockShinySession$new(); b$userData$sk_lang <- "en"
  expect_identical(shiny::withReactiveDomain(a, e$.sk_app_lang()), "pt")
  expect_identical(shiny::withReactiveDomain(b, e$.sk_app_lang()), "en")
  expect_identical(getOption("soilKey.app_lang"), "en")
})

test_that("the selector puts the language in the URL, never in a global option", {
  app <- readLines(file.path(.vf_dir(), "app.R"), warn = FALSE)
  app <- app[!grepl("^\\s*#", app)]                     # code, not comments
  sel <- app[grep("observeEvent(input$app_lang_sel", app, fixed = TRUE):length(app)][1:12]
  expect_true(any(grepl("updateQueryString", sel, fixed = TRUE)))
  expect_false(any(grepl("options(soilKey.app_lang", sel, fixed = TRUE)))
  # tabs are switched by a fixed value: their titles are translated
  expect_false(any(grepl('nav_select("main_nav", "Pedon")', app, fixed = TRUE)))
  expect_false(any(grepl('nav_select("main_nav", "Classify")', app, fixed = TRUE)))
  expect_true(any(grepl('value = "pedon"', app, fixed = TRUE)))
})

# Walk every call in a file; report help texts written as English literals.
.literal_help <- function(file) {
  hits <- character(0)
  is_str <- function(x) is.character(x) && length(x) == 1L
  walk <- function(x) {
    if (!is.call(x)) return(invisible())
    fn <- x[[1]]
    nm <- if (is.name(fn)) as.character(fn)
          else if (is.call(fn) && identical(fn[[1]], as.name("::"))) as.character(fn[[3]])
          else ""
    args <- as.list(x)[-1]
    if (nm == "sk_label" && length(args) >= 2 && is_str(args[[2]]))
      hits <<- c(hits, paste("sk_label:", args[[2]]))
    if (nm == "sk_section" && is_str(args$desc))
      hits <<- c(hits, paste("sk_section desc:", args$desc))
    if (nm == "tooltip" && length(args) >= 2 && is_str(args[[length(args)]]))
      hits <<- c(hits, paste("tooltip:", args[[length(args)]]))
    if (nm == "helpText" && any(vapply(args, function(a) is_str(a) && grepl("[a-z] [a-z]", a),
                                      logical(1))))
      hits <<- c(hits, "helpText literal")
    for (a in args) if (!missing(a)) walk(a)
  }
  for (ex in parse(file, keep.source = FALSE)) walk(ex)
  hits
}

test_that("no help text in the mounted tabs is an English literal", {
  mounted <- c("mod_pedon.R", "mod_classify.R", "mod_photo.R", "mod_spectra.R",
               "mod_map.R", "mod_uncertainty.R", "mod_report.R", "mod_settings.R",
               "mod_chat.R", "mod_acknowledgements.R", "utils_ui.R")
  for (f in mounted) {
    hits <- .literal_help(file.path(.vf_dir(), "R", f))
    expect_identical(hits, character(0), info = f)
  }
})

test_that("every catalogue key exists in both languages with the same placeholders", {
  path <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(path)) path <- file.path("inst", "i18n", "translations.yaml")
  y <- yaml::read_yaml(path)
  expect_setequal(names(y$en), names(y$pt))
  ph <- function(s) sort(regmatches(s, gregexpr("%[sd]", s))[[1]])
  for (k in names(y$en)) expect_identical(ph(y$en[[k]]), ph(y$pt[[k]]), info = k)
  # the new texts are actually translated (a few names stay the same)
  new <- grep("^(pedon\\.(help|desc|tip)_|classify\\.(help|desc|tip)_|spectra\\.(help|desc|tip)_|settings\\.(help|desc)_|thanks\\.c_|dt\\.)",
              names(y$en), value = TRUE)
  expect_gt(length(new), 60)
  same <- new[vapply(new, function(k) identical(y$en[[k]], y$pt[[k]]), logical(1))]
  expect_identical(same, character(0))
})

test_that("tables speak the session's language", {
  skip_if_not_installed("shiny"); skip_if_not_installed("DT")
  e <- .vf_env()
  pt <- e$.sk_with_lang("pt", e$sk_datatable(data.frame(a = 1)))
  expect_identical(pt$x$options$language$search, "Buscar:")
  expect_identical(pt$x$options$language$paginate[["next"]], "Próxima")
  en <- e$.sk_with_lang("en", e$sk_datatable(data.frame(a = 1)))
  expect_null(en$x$options$language)                    # DataTables' own English
})


# ---- 1, 2. report: a PDF only where one can be made; downloads in sight --------

test_that("the HTML report prints to PDF, in the report's language", {
  f <- tempfile(fileext = ".html")
  report(make_ferralsol_canonical(), file = f, format = "html", lang = "pt")
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  expect_match(h, '<html lang="pt">', fixed = TRUE)
  expect_match(h, 'onclick="window.print()"', fixed = TRUE)
  expect_match(h, ".print-bar{display:none;}", fixed = TRUE)   # not on paper
})

test_that("the Report tab offers PDF only with LaTeX, and its downloads sit in the body", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("bslib")
  e <- .vf_env()
  ui <- as.character(e$report_ui("report"))
  expect_false(grepl("report-html", ui, fixed = TRUE))   # not in the sidebar
  rv <- shiny::reactiveValues(pedon = make_ferralsol_canonical())
  for (latex in c(FALSE, TRUE)) {
    e$.report_pdf_available <- function() latex
    shiny::testServer(e$report_server,
                      args = list(rv = rv, settings = shiny::reactive(NULL)), {
      body <- paste(as.character(output$body$html), collapse = "")
      expect_match(body, 'id="[^"]*html"')                 # the HTML download
      expect_identical(grepl('id="[^"]*pdf"', body), latex)
      expect_identical(grepl("fa-print", body, fixed = TRUE), !latex)
    })
  }
})


# ---- 5. spectra: no placeholder values written into a profile -----------------

test_that("spectral gap-fill is off without a real OSSL library, and says why", {
  skip_if_not_installed("shiny")
  e <- .vf_env()
  withr::local_options(soilKey.ossl_library = NULL, soilKey.ossl_models = NULL)
  expect_false(e$.spectra_gapfill_available())
  sec <- as.character(e$.spectra_gapfill_section(shiny::NS("spectra")))
  expect_match(sec, e$i18n("spectra.gapfill_unavailable", lang = "en"), fixed = TRUE)
  expect_false(grepl('id="spectra-fill"', sec, fixed = TRUE))
  # Classify does not offer it either: it would classify on invented values
  expect_false("spectra" %in% e$.classify_gapfill_choices())
  # with a library configured, both come back
  withr::local_options(soilKey.ossl_library = list(Xr = matrix(0, 2, 2),
                                                   Yr = data.frame(clay_pct = 1:2)))
  expect_true(e$.spectra_gapfill_available())
  sec <- as.character(e$.spectra_gapfill_section(shiny::NS("spectra")))
  expect_match(sec, 'id="spectra-fill"', fixed = TRUE)
  expect_true("spectra" %in% e$.classify_gapfill_choices())
  # and the package reads the same option, so every path sees one library
  expect_identical(formals(fill_from_spectra)$ossl_library,
                   quote(getOption("soilKey.ossl_library")))
})


# ---- 4. map: SoilGrids read off the Shiny process, live by default, credited ---

test_that("the overlay is read by a job that returns a packed raster, with fallback", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  e <- .vf_env()
  demo <- e$.map_soilgrids_source(NULL, "demo")
  run <- function(...) {
    out <- NULL; done <- FALSE
    promises::then(e$.sk_async(e$.map_overlay_job, list(...),
                               helpers = e$.map_job_helpers()),
                   function(v) { out <<- v; done <<- TRUE })
    for (i in 1:200) { if (done) break; later::run_now(0.05) }
    out
  }
  ok <- run(lat = -22.5, lon = -43.7, src = demo)
  expect_s4_class(ok$rr$raster, "PackedSpatRaster")
  expect_s4_class(e$.map_unwrap(ok$rr)$raster, "SpatRaster")
  expect_false(ok$fell_back)
  # a source that cannot be read falls back to the demo, and says so
  fb <- run(lat = -22.5, lon = -43.7, src = tempfile(fileext = ".tif"),
            fallback_src = demo)
  expect_true(fb$fell_back)
  expect_false(is.null(fb$rr))
})

test_that("a categorical raster like live SoilGrids' recodes (it fell back to the demo)", {
  skip_on_cran()
  skip_if_not_installed("shiny"); skip_if_not_installed("terra")
  e <- .vf_env()
  # like MostProbable.vrt: integer cells with a table of RSG names
  r <- terra::rast(nrows = 60, ncols = 60, xmin = -47, xmax = -40,
                   ymin = -26, ymax = -19, crs = "EPSG:4326")
  terra::values(r) <- rep(c(1L, 2L, 3L), length.out = terra::ncell(r))
  levels(r) <- data.frame(id = 1:3, RSG = c("Ferralsols", "Acrisols", "Cambisols"))
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f)
  out <- NULL; done <- FALSE
  promises::then(e$.sk_async(e$.map_overlay_job,
                             list(lat = -22.5, lon = -43.7, src = f,
                                  fallback_src = e$.map_soilgrids_source(NULL, "demo")),
                             helpers = e$.map_job_helpers()),
                 function(v) { out <<- v; done <<- TRUE })
  for (i in 1:200) { if (done) break; later::run_now(0.05) }
  expect_false(out$fell_back)                     # it was read, not replaced
  expect_setequal(out$rr$lut$class, c("FR", "AC", "CM"))
})

test_that("the map opens on live SoilGrids and credits the source", {
  e <- .vf_env()
  ui <- readLines(file.path(.vf_dir(), "R", "mod_map.R"), warn = FALSE)
  expect_true(any(grepl('selected = "live", justified = TRUE', ui, fixed = TRUE)))
  expect_match(e$i18n("map.attr_soilgrids", lang = "en"), "CC BY 4.0", fixed = TRUE)
  expect_match(e$i18n("map.attr_soilgrids", lang = "en"), "ISRIC", fixed = TRUE)
  expect_match(e$i18n("map.attr_demo", lang = "en"), "not SoilGrids", fixed = TRUE)
})
