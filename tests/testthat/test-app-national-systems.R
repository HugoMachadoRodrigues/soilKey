# The national systems (Canada, Australia, New Zealand, Hungary) in the Pro
# app's Classify tab and in the report: the report helpers of
# R/report-national.R, the app helpers of
# inst/shiny/classify_app_pro/R/utils_systems.R, the Classify module with a
# national system ticked, the session file, and the Report tab.

.nat_app_dir <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  d
}

.nat_app_env <- function() {
  env <- new.env(parent = globalenv())
  for (f in list.files(file.path(.nat_app_dir(), "R"), pattern = "[.]R$",
                       full.names = TRUE)) sys.source(f, envir = env)
  # the catalogue as the tests find it (under load_all the sourced helper's
  # own system.file() cannot see inst/i18n)
  p <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(p) || !file.exists(p)) p <- file.path("inst", "i18n", "translations.yaml")
  env$.sk_i18n_env$cat <- yaml::read_yaml(p)
  env
}

.nat_skip_unless_app_deps <- function() {
  for (p in c("shiny", "bslib", "shinyWidgets", "DT", "plotly"))
    testthat::skip_if_not_installed(p)
}

.nat_settings <- function() {
  shiny::reactive(list(engine = "soilKey", strict = FALSE, on_missing = "silent",
                       include_familia = TRUE, include_family = FALSE,
                       specifiers = FALSE))
}


# ---- the report -----------------------------------------------------------------

test_that("a report runs WRB, SiBCS and USDA by default and adds the systems asked for", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  def <- soilKey:::.normalise_results(p, pedon = p)$results
  expect_identical(names(def), c("wrb", "sibcs", "usda"))
  nat <- soilKey:::.normalise_results(
    p, pedon = p, systems = c("hscs", "wrb2022", "cssc3", "sibcs", "usda"))$results
  # registry order, aliases resolved
  expect_identical(names(nat), c("wrb", "sibcs", "usda", "cssc3", "hscs2024"))
  expect_identical(nat$cssc3$name, classify_cssc3(p, on_missing = "silent")$name)
  expect_identical(nat$wrb$name, def$wrb$name)
  only <- soilKey:::.normalise_results(p, pedon = p, systems = c("wrb2022", "nzsc4"))$results
  expect_identical(names(only), c("wrb", "nzsc4"))
})

test_that("WRB, SiBCS and USDA cards get no national section", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  for (r in list(classify_wrb2022(p, on_missing = "silent"), classify_sibcs(p),
                 classify_usda(p))) {
    expect_identical(soilKey:::.html_national_sections(r), "")
    expect_identical(soilKey:::.rmd_national_sections(r), "")
    expect_false(soilKey:::.result_is_experimental(r))
  }
})

test_that("a national card shows the levels, the provisional classes and the experimental note", {
  skip_on_cran()
  r <- classify_cssc3(make_ferralsol_canonical(), on_missing = "silent")
  expect_true(soilKey:::.result_is_experimental(r))
  pd <- soilKey:::.provisional_detail(r)
  expect_s3_class(pd, "data.frame")
  expect_identical(nrow(pd), length(r$provisional_because))
  expect_identical(paste0(pd$level, ":", pd$code), r$provisional_because)
  # every undecided class is found in the trace, with its name
  expect_true(all(nzchar(pd$name)))
  html <- withr::with_options(list(soilKey.report_lang = "en"),
                              soilKey:::.html_national_sections(r))
  expect_match(html, "Class at each level", fixed = TRUE)
  expect_match(html, "Experimental key", fixed = TRUE)
  expect_match(html, "Provisional", fixed = TRUE)
  for (nm in r$levels$name) expect_match(html, nm, fixed = TRUE)
  rmd <- withr::with_options(list(soilKey.report_lang = "en"),
                             soilKey:::.rmd_national_sections(r))
  expect_match(rmd, "| Order | J |", fixed = TRUE)
  # no undecided class, no provisional section
  r0 <- classify_cssc3(make_cssc3_example("chernozemic"), on_missing = "silent")
  if (!length(r0$provisional_because)) {
    expect_null(soilKey:::.provisional_detail(r0))
    expect_false(grepl("Provisional", soilKey:::.html_national_sections(r0), fixed = TRUE))
  }
})

test_that("the Hungarian table lists the properties named and those undecided", {
  skip_on_cran()
  r <- classify_hscs2024(make_hscs2024_example("steppe"), on_missing = "silent")
  hp <- soilKey:::.hscs_property_table(r)
  expect_s3_class(hp, "data.frame")
  named <- hp[hp$status == "named", ]
  expect_identical(named$label[named$level == "subtype"], r$qualifiers$subtypes)
  expect_identical(named$label[named$level == "variety"], r$qualifiers$varieties)
  und <- hp[hp$status == "undecided", ]
  expect_gt(nrow(und), 0L)
  expect_true(all(nzchar(und$missing)))
  st <- r$qualifiers$status
  expect_identical(nrow(hp), sum(st$named | is.na(st$passed)))
  expect_null(soilKey:::.hscs_property_table(classify_cssc3(make_cssc3_example(),
                                                             on_missing = "silent")))
})

test_that("report() takes the national systems and writes their sections, in both languages", {
  skip_on_cran()
  p <- make_hscs2024_example("steppe")
  f_en <- withr::local_tempfile(fileext = ".html")
  f_pt <- withr::local_tempfile(fileext = ".html")
  report(p, file = f_en, pedon = p, systems = c("wrb2022", "sibcs", "usda", "cssc3", "hscs2024"))
  report(p, file = f_pt, pedon = p, systems = c("wrb", "hscs"), lang = "pt")
  en <- paste(readLines(f_en, encoding = "UTF-8"), collapse = "\n")
  pt <- paste(readLines(f_pt, encoding = "UTF-8"), collapse = "\n")
  hu <- classify_hscs2024(p, on_missing = "silent")$name
  expect_match(en, hu, fixed = TRUE)
  expect_match(en, classify_cssc3(p, on_missing = "silent")$name, fixed = TRUE)
  expect_match(en, "Subtype and variety properties", fixed = TRUE)
  expect_match(pt, hu, fixed = TRUE)
  expect_match(pt, "Propriedades de subtipo e de variedade", fixed = TRUE)
  expect_false(grepl("Subtype and variety properties", pt, fixed = TRUE))
  # the default report has none of it
  f0 <- withr::local_tempfile(fileext = ".html")
  report(p, file = f0, pedon = p)
  d0 <- paste(readLines(f0, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl(hu, d0, fixed = TRUE))
  expect_false(grepl("Class at each level", d0, fixed = TRUE))
})


# ---- the app helpers ---------------------------------------------------------------

test_that("the Classify tab offers the registry's national systems with their editions", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  nat <- env$.sk_national_systems()
  reg <- soilkey_systems()
  expect_identical(nat$id, reg$id[!reg$default])
  expect_true(all(c("cssc3", "asc3", "nzsc4", "hscs2024") %in% nat$id))
  expect_identical(env$.SK_DEFAULT_SYSTEMS, c("wrb2022", "sibcs", "usda"))
  ch <- withr::with_options(list(soilKey.app_lang = "en"), env$.sk_national_choices())
  expect_identical(ch$values, nat$id)
  html <- paste(vapply(ch$names, as.character, ""), collapse = "")
  for (i in seq_len(nrow(nat))) expect_match(html, nat$edition[i], fixed = TRUE)
})

test_that("the catalogue's English editions are the registry's (else pt shows the registry's)", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  cat <- env$.sk_i18n_env$cat
  nat <- env$.sk_national_systems()
  for (i in seq_len(nrow(nat))) {
    key <- paste0("classify.edition_", nat$id[i])
    expect_identical(cat$en[[key]], nat$edition[i], info = key)
    expect_true(nzchar(cat$en[[paste0("classify.sys_", nat$id[i])]] %||% ""))
  }
  withr::with_options(list(soilKey.app_lang = "pt"), {
    expect_identical(env$.sk_system_edition("cssc3", "3rd edition (1998)"),
                     cat$pt[["classify.edition_cssc3"]])
    # an edition changed in the registry is shown as the registry has it
    expect_identical(env$.sk_system_edition("cssc3", "4th edition (2030)"),
                     "4th edition (2030)")
    expect_identical(env$.sk_system_label("nosuch9", "Some system"), "Some system")
  })
})

test_that("system ids and result slots are read as the app names them", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  expect_identical(env$.sk_result_slot(c("wrb2022", "sibcs", "usda", "cssc3", "hscs2024")),
                   c("wrb", "sibcs", "usda", "cssc3", "hscs2024"))
  expect_identical(env$.sk_app_system_ids(c("hscs", "sibcs5", "wrb", "bogus", "")),
                   c("wrb2022", "sibcs", "hscs2024"))
  expect_identical(env$.sk_app_system_ids(list()), character(0))
})

test_that("the session file keeps the systems as an array and reads them back", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  expect_null(env$.sk_session_classify(NULL))
  for (sys in list(c("wrb2022", "cssc3"), "hscs2024", character(0))) {
    f <- withr::local_tempfile(fileext = ".json")
    jsonlite::write_json(list(site = list(id = "x"), horizons = list(),
                              classify = env$.sk_session_classify(sys)),
                         f, auto_unbox = TRUE, null = "null", na = "null")
    expect_match(paste(readLines(f), collapse = ""), "\"systems\":[", fixed = TRUE)
    parsed <- jsonlite::fromJSON(f, simplifyVector = TRUE, simplifyDataFrame = TRUE)
    expect_identical(env$.sk_session_systems(parsed), sys)
  }
  # a file saved before has none: the ticks stay as they are
  expect_null(env$.sk_session_systems(list(site = list(id = "x"))))
})

test_that("a national card and the Hungarian table render", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  withr::local_options(soilKey.app_lang = "en")
  r <- classify_nzsc4(make_ferralsol_canonical(), on_missing = "silent")
  html <- as.character(env$pro_national_card(r, "New Zealand (NZSC)", edition = "4th edition"))
  expect_match(html, r$name, fixed = TRUE)
  expect_match(html, "sk-exp-badge", fixed = TRUE)
  expect_match(html, "New Zealand (NZSC)", fixed = TRUE)
  if (length(r$provisional_because)) expect_match(html, "sk-provisional", fixed = TRUE)
  err <- as.character(env$pro_national_card(simpleError("boom"), "Canada (CSSC)"))
  expect_match(err, "boom", fixed = TRUE)
  h <- classify_hscs2024(make_hscs2024_example("steppe"), on_missing = "silent")
  d <- env$.sk_hscs_props_display(h)
  expect_identical(names(d), c("level", "property", "gloss", "status", "missing"))
  expect_true(all(d$status %in% c("in the name", "undecided")))
  expect_null(env$.sk_hscs_props_display(r))
})


# ---- the modules ----------------------------------------------------------------------

test_that("the classify job returns the national systems, also when gap-fill fails", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  args <- list(pedon = make_ferralsol_canonical(), systems = c("wrb2022", "cssc3"),
               on_missing = "silent", include_familia = TRUE, include_family = FALSE,
               specifiers = FALSE, gapfill_methods = character(0))
  res <- do.call(env$.classify_job, args)
  expect_false(is.null(res$cssc3))
  expect_null(attr(res, "failures"))
  # a gap-fill that cannot run: every system asked for comes back without it
  args$gapfill_methods <- "no-such-method"
  res <- do.call(env$.classify_job, args)
  expect_false(is.null(res$wrb))
  expect_false(is.null(res$cssc3))
  expect_match(attr(res, "gapfill_error"), "unknown gapfill method", fixed = TRUE)
})

test_that("classify_server runs and shows a ticked national system", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  rv <- shiny::reactiveValues(pedon = make_hscs2024_example("steppe"),
                              include_family = FALSE, specifiers = FALSE)
  shiny::testServer(env$classify_server, args = list(rv = rv, settings = .nat_settings()), {
    session$setInputs(systems = c("wrb2022", "sibcs", "usda"),
                      national = c("cssc3", "hscs2024"), run = 1)
    .settle(session)
    res <- session$returned()
    expect_true(all(c("wrb", "sibcs", "usda", "cssc3", "hscs2024") %in% names(res)))
    expect_identical(rv$classify_systems,
                     c("wrb2022", "sibcs", "usda", "cssc3", "hscs2024"))
    expect_identical(rv$classified_systems, rv$classify_systems)
    html <- output$body$html
    expect_match(html, "sk-national-card", fixed = TRUE)
    expect_match(html, res$hscs2024$name, fixed = TRUE)
    expect_match(html, res$cssc3$name, fixed = TRUE)
    expect_match(html, "hscs_props", fixed = TRUE)
    session$setInputs(trace_sys = "cssc3")
    expect_match(as.character(output$trace_table), res$cssc3$levels$code[1], fixed = TRUE)
    expect_error(output$hscs_props, NA)
    expect_error(output$missing, NA)
    # a reopened session's request is taken without error
    rv$classify_restore <- list(systems = c("wrb", "nzsc"), n = 1L)
    session$flushReact()
    # no national system ticked: no national block
    session$setInputs(national = character(0), run = 2)
    .settle(session)
    expect_false(grepl("sk-national-card", output$body$html, fixed = TRUE))
  })
})

test_that("pedon_server passes a session file's systems to the Classify tab", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  p <- make_cssc3_example()
  h <- as.data.frame(p$horizons)
  h <- h[, vapply(h, function(v) !all(is.na(v)), logical(1)), drop = FALSE]
  f <- withr::local_tempfile(fileext = ".json")
  jsonlite::write_json(list(site = p$site,
                            horizons = lapply(seq_len(nrow(h)), function(i) as.list(h[i, , drop = FALSE])),
                            classify = list(systems = I(c("wrb2022", "cssc3")))),
                       f, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  rv <- shiny::reactiveValues(pedon = NULL, example_request = 0L,
                              classify_systems = c("wrb2022", "sibcs", "usda"))
  shiny::testServer(env$pedon_server, args = list(rv = rv), {
    session$setInputs(session_file = data.frame(name = "s.json", size = file.size(f),
                                                type = "application/json", datapath = f,
                                                stringsAsFactors = FALSE))
    expect_identical(rv$classify_restore$systems, c("wrb2022", "cssc3"))
    expect_identical(rv$classify_restore$n, 1L)
    expect_s3_class(rv$pedon, "PedonRecord")
  })
})

test_that("the Report tab includes the national systems of the last run", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  withr::local_options(soilKey.app_lang = "en")
  rv <- shiny::reactiveValues(pedon = make_ferralsol_canonical(),
                              classified_systems = NULL)
  shiny::testServer(env$report_server, args = list(rv = rv, settings = .nat_settings()), {
    expect_match(output$national$html, "National systems: none", fixed = TRUE)
    rv$classified_systems <- c("wrb2022", "nzsc4", "hscs2024")
    session$flushReact()
    expect_match(output$national$html, "New Zealand (NZSC), Hungary (HSCS)", fixed = TRUE)
  })
})

# ---- the national examples on the Pedon tab (v0.9.317) -----------------------------

test_that("the Pedon tab offers one example per class of each national system, each keying to it", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  cat <- env$pro_fixture_catalog()
  fun <- c(cssc3 = "classify_cssc3", asc3 = "classify_asc3", nzsc4 = "classify_nzsc4",
           hscs2024 = "classify_hscs2024")
  for (id in names(fun)) {
    grp <- cat[[env$.sk_system_label(id)]]
    expect_true(length(grp) >= 10L, info = id)
    for (k in seq_along(grp)) {
      expect_identical(env$pro_fixture_system(grp[[k]]), id)
      p <- env$pro_load_fixture(grp[[k]])
      expect_true(inherits(p, "PedonRecord"))
      r <- get(fun[[id]], asNamespace("soilKey"))(p, on_missing = "silent")
      expect_identical(r$levels$name[1], names(grp)[k], info = grp[[k]])
    }
  }
  # the WRB and SiBCS fixtures load as before and name no national system
  expect_true(is.na(env$pro_fixture_system("make_ferralsol_canonical")))
  expect_true(inherits(env$pro_load_fixture("make_ferralsol_canonical"), "PedonRecord"))
})

test_that("loading a national example ticks its system and keeps the site details its key reads", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  rv <- shiny::reactiveValues(pedon = NULL, example_request = 0L,
                              classify_systems = c("wrb2022", "sibcs", "usda"))
  shiny::testServer(env$pedon_server, args = list(rv = rv), {
    session$setInputs(source = "fixture", fixture = "make_hscs2024_example:steppe")
    session$setInputs(load = 1)
    expect_identical(rv$classify_restore$systems, c("wrb2022", "sibcs", "usda", "hscs2024"))
    expect_identical(data_source()$designation_system, "fao2006")
    # no WoSIS line for site details
    expect_null(output$data_source_note$html %||% NULL)
    # a WRB fixture afterwards clears them and asks for no change of systems
    n <- rv$classify_restore$n
    session$setInputs(fixture = "make_ferralsol_canonical")
    session$setInputs(load = 2)
    expect_null(data_source())
    expect_identical(rv$classify_restore$n, n)
  })
})

test_that("a loaded example without coordinates clears those of the profile before it", {
  skip_on_cran()
  .nat_skip_unless_app_deps()
  env <- .nat_app_env()
  sent <- list()
  sess <- new.env()
  sess$sendInputMessage <- function(id, msg) sent[[id]] <<- msg
  sess$ns <- function(x) x
  class(sess) <- "ShinySession"
  # a Canadian example with coordinates, then a Hungarian one with none
  env$.sk_fill_site_inputs(sess, make_cssc3_example("chernozemic")$site)
  expect_false(is.na(as.numeric(sent$lat$value)))
  env$.sk_fill_site_inputs(sess, make_hscs2024_example("peat")$site)
  expect_identical(sent$lat$value, "NA")
  expect_identical(sent$lon$value, "NA")
  expect_identical(sent$pm$value, "")
  expect_identical(sent$country$value, "HU")
  # a WRB example keeps its own
  env$.sk_fill_site_inputs(sess, make_ferralsol_canonical()$site)
  expect_false(is.na(suppressWarnings(as.numeric(sent$lat$value))))
  expect_true(nzchar(sent$pm$value))
})
