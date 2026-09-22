# Tests for v0.9.117 bilingual report(): the .report_msg() catalogue and the
# `lang` argument on report() / report_html() / report_pdf().

test_that("the report label catalogue is complete (every en key has pt)", {
  cat <- soilKey:::.report_catalog()
  expect_true(all(c("en", "pt") %in% names(cat)))
  expect_equal(sort(names(cat$en)), sort(names(cat$pt)))
  expect_gt(length(cat$en), 40L)
  # sprintf placeholders must match between languages
  ph <- function(s) sort(regmatches(s, gregexpr("%[-0-9.]*[sdfg]", s))[[1]])
  for (k in names(cat$en))
    expect_identical(ph(cat$en[[k]]), ph(cat$pt[[k]]), info = k)
})

test_that(".report_msg resolves, falls back, and clamps", {
  withr::with_options(list(soilKey.report_lang = "en"),
                      expect_equal(soilKey:::.report_msg("report.key_trace"), "Key trace"))
  withr::with_options(list(soilKey.report_lang = "pt"),
                      expect_equal(soilKey:::.report_msg("report.key_trace"), "Rastro da chave"))
  withr::with_options(list(soilKey.report_lang = "xx"),     # unknown -> en
                      expect_equal(soilKey:::.report_msg("report.key_trace"), "Key trace"))
  expect_equal(soilKey:::.report_msg("report.does_not_exist"), "report.does_not_exist")
})

test_that("an HTML report defaults to English and translates to Portuguese", {
  p   <- make_ferralsol_canonical()
  out_en <- withr::local_tempfile(fileext = ".html")
  out_pt <- withr::local_tempfile(fileext = ".html")
  report(p, file = out_en, pedon = p)               # default lang = "en"
  report(p, file = out_pt, pedon = p, lang = "pt")
  en <- paste(readLines(out_en), collapse = "\n")
  pt <- paste(readLines(out_pt), collapse = "\n")

  expect_true(grepl("Classification results", en, fixed = TRUE))
  expect_true(grepl("Key trace", en, fixed = TRUE))

  expect_true(grepl("Resultados da classifica", pt))
  expect_true(grepl("Rastro da chave", pt, fixed = TRUE))
  # the Portuguese report must not leak the English section headers
  expect_false(grepl("Classification results", pt, fixed = TRUE))
  expect_false(grepl(">Key trace<", pt, fixed = TRUE))
})

test_that("report_html restores the report-language option after rendering", {
  before <- getOption("soilKey.report_lang")
  p <- make_ferralsol_canonical()
  report(p, file = withr::local_tempfile(fileext = ".html"), pedon = p, lang = "pt")
  expect_identical(getOption("soilKey.report_lang"), before)
})
