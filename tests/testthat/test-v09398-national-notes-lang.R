# v0.9.398: the FAO edition line and the CST correlation in the report
# language. In English the lines are those of before, byte for byte; in
# Portuguese the words around the codes and class names are translated, and so
# are the conditions of the crosswalk.

test_that("the FAO edition lines are unchanged in English", {
  skip_on_cran()
  withr::local_options(soilKey.report_lang = "en")
  l74 <- .fao_edition_lines(classify_fao1974(make_fao1974_example("F"), on_missing = "silent"))
  expect_identical(l74, "Revised Legend (1988): FRu Humic Ferralsols (amended, App. 4 p. 125).")
  l88 <- .fao_edition_lines(classify_fao1988(make_fao1988_example("FR"), on_missing = "silent"))
  expect_identical(l88, "Legend of 1974: Fo Orthic Ferralsols (renamed, App. 4 p. 125).")
  lx <- .fao_edition_lines(classify_fao1988(make_fao1988_example("LX"), on_missing = "silent"))
  expect_identical(lx, paste0("Legend of 1974: new in 1988, no class of 1974 (App. 4 p. 122); ",
                              "[some La Albic Luvisols, an argic B with a CEC below 24 cmol(+)/kg clay]."))
})

test_that("the FAO edition lines are in Portuguese in a Portuguese report", {
  skip_on_cran()
  withr::local_options(soilKey.report_lang = "pt")
  l74 <- .fao_edition_lines(classify_fao1974(make_fao1974_example("F"), on_missing = "silent"))
  expect_identical(l74, "Legenda Revisada (1988): FRu Humic Ferralsols (defini\u00e7\u00e3o alterada, Ap. 4 p. 125).")
  lx <- .fao_edition_lines(classify_fao1988(make_fao1988_example("LX"), on_missing = "silent"))
  expect_identical(lx, paste0("Legenda de 1974: nova em 1988, sem classe de 1974 (Ap. 4 p. 122); ",
                              "[parte dos La Albic Luvisols, um B \u00e1rgico com CTC abaixo de 24 cmol(+)/kg de argila]."))
})

test_that("every condition of the crosswalk has its Portuguese", {
  skip_on_cran()
  cw <- .fao_cw_read()
  w <- unique(unlist(lapply(c(cw$groups, cw$units, cw$new_groups, cw$new_units),
                            function(e) unlist(e$also_when))))
  pt <- yaml::read_yaml(system.file("i18n", "fao_crosswalk_pt.yaml", package = "soilKey"))
  expect_true(length(w) > 0L)
  expect_setequal(names(pt), w)
})

test_that("the CST correlation lines are unchanged in English and translated in Portuguese", {
  skip_on_cran()
  r <- classify_cst2001(make_cst2001_example("L4"), on_missing = "silent")
  withr::local_options(soilKey.report_lang = "en")
  expect_identical(.cst_correlation_lines(r), c(
    "Argosols, US Soil Taxonomy (1999), Table 4.2: mostly Alfisols; partly Ultisols, Mollisols.",
    "Argosols, World Reference Base (1998), Table 4.3: mostly Luvisols; partly Alisols, Chernozems, Kastanozems, Phaeozems, Calcisols, Albeluvisols, Planosols."))
  withr::local_options(soilKey.report_lang = "pt")
  expect_identical(.cst_correlation_lines(r)[1],
                   "Argosols, Soil Taxonomy dos EUA (1999), Tabela 4.2: na maior parte Alfisols; em parte Ultisols, Mollisols.")
})

test_that("the result itself stays in English whatever the report language", {
  skip_on_cran()
  withr::local_options(soilKey.report_lang = "pt")
  r <- classify_cst2001(make_cst2001_example("L4"), on_missing = "silent")
  expect_true(any(grepl("Correlation (a note, never a class): Argosols, US Soil Taxonomy (1999), Table 4.2: mostly",
                        r$warnings, fixed = TRUE)))
  f <- classify_fao1988(make_fao1988_example("FR"), on_missing = "silent")
  msg <- character(0)
  std <- capture.output(msg <- capture.output(print(f), type = "message"))
  out <- paste(c(std, msg), collapse = "\n")
  expect_match(out, "Legend of 1974: Fo Orthic Ferralsols (renamed, App. 4 p. 125).", fixed = TRUE)
  expect_identical(getOption("soilKey.report_lang"), "pt")
})

test_that("every registry system has a short citation", {
  skip_on_cran()
  for (id in soilkey_systems()$id) {
    v <- .system_get(id)$citation_short
    expect_true(is.character(v) && length(v) == 1L && !is.na(v) && nzchar(trimws(v)), info = id)
    expect_false(grepl("\n", v, fixed = TRUE), info = id)
  }
})

test_that("a report does not repeat the CST correlation as a warning", {
  skip_on_cran()
  p <- make_cst2001_example("L4")
  r <- classify_cst2001(p, on_missing = "silent")
  # the result keeps its warning, in English
  expect_true(any(startsWith(r$warnings, .CST_CORRELATION_NOTE)))
  expect_false(any(startsWith(.report_warnings(r), .CST_CORRELATION_NOTE)))
  expect_identical(.report_warnings(r), setdiff(r$warnings, r$warnings[startsWith(r$warnings, .CST_CORRELATION_NOTE)]))
  for (lang in c("en", "pt")) {
    f <- withr::local_tempfile(fileext = ".html")
    report_html(p, file = f, lang = lang, systems = c("wrb2022", "cst2001"))
    h <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_false(grepl("Correlation (a note, never a class)", h, fixed = TRUE), info = lang)
    expect_match(h, if (lang == "en") "Table 4.2: mostly Alfisols" else "Tabela 4.2: na maior parte Alfisols",
                 fixed = TRUE)
  }
  # a result of another system keeps every warning
  w <- classify_ka5(make_ka5_example("BB"), on_missing = "silent")
  expect_identical(.report_warnings(w), as.character(w$warnings %||% character()))
})
