# Tests for the report() generic + report_html() / report_pdf().
# The HTML path has no hard dependencies (we just emit strings), so it
# runs everywhere; the PDF path requires rmarkdown + a LaTeX engine
# and is skipped on CI.


# ---- helpers ---------------------------------------------------------------


make_minimal_result <- function(system = "WRB 2022",
                                  name   = "Rhodic Ferralsol",
                                  grade  = "B",
                                  trace  = NULL) {
  if (is.null(trace)) {
    trace <- list(
      list(code = "HS", name = "Histosols",  passed = FALSE,
           missing = character(0)),
      list(code = "FR", name = "Ferralsols", passed = TRUE,
           missing = character(0))
    )
  }
  ClassificationResult$new(
    system         = system,
    name           = name,
    rsg_or_order   = "Ferralsols",
    qualifiers     = list(principal     = c("Rhodic", "Chromic"),
                          supplementary = c("Clayic", "Humic")),
    trace          = trace,
    ambiguities    = list(list(rsg_code = "AC",
                                 reason   = "argic features absent")),
    missing_data   = c("ECEC", "fe_dcb_pct"),
    evidence_grade = grade,
    warnings       = c("Andic + ferralic both passed: review")
  )
}


# ---- generic dispatch ------------------------------------------------------


test_that("report() infers HTML from .html extension", {
  res  <- make_minimal_result()
  out  <- tempfile(fileext = ".html")
  path <- report(res, file = out)
  expect_true(file.exists(out))
  expect_equal(normalizePath(out), normalizePath(path))
  unlink(out)
})

test_that("report() rejects an extension it cannot infer from", {
  res <- make_minimal_result()
  out <- tempfile(fileext = ".xyz")
  expect_error(report(res, file = out), "format")
  if (file.exists(out)) unlink(out)
})

test_that("report() requires a non-empty file argument", {
  res <- make_minimal_result()
  expect_error(report(res, file = ""), "file")
  expect_error(report(res, file = NULL), "file")
})


# ---- HTML output -----------------------------------------------------------


test_that("report_html() emits a self-contained HTML document with the result name", {
  res <- make_minimal_result(name = "Rhodic Ferralsol (Clayic, Humic)")
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  report_html(res, file = out)

  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, "<!DOCTYPE html>", fixed = TRUE)
  expect_match(body, "Rhodic Ferralsol", fixed = TRUE)
  expect_match(body, "Evidence grade", fixed = TRUE)
  # Inline CSS is embedded -- no link to external stylesheet.
  expect_false(grepl("<link[^>]*href", body, ignore.case = TRUE))
})

test_that("report_html() handles a list of results with cross-system summary", {
  results <- list(
    make_minimal_result(system = "WRB 2022",   name = "Rhodic Ferralsol"),
    make_minimal_result(system = "SiBCS 5",    name = "Latossolo Vermelho"),
    make_minimal_result(system = "USDA ST 13", name = "Rhodic Hapludox")
  )
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  report_html(results, file = out)

  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, "Cross-system summary", fixed = TRUE)
  expect_match(body, "Rhodic Ferralsol",     fixed = TRUE)
  expect_match(body, "Latossolo Vermelho",   fixed = TRUE)
  expect_match(body, "Rhodic Hapludox",      fixed = TRUE)
})

test_that("report_html() escapes HTML-significant characters in names", {
  res <- make_minimal_result(name = "Some <weird> & \"quoted\" name")
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  report_html(res, file = out)
  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_false(grepl("<weird>",  body, fixed = TRUE))
  expect_match( body, "&lt;weird&gt;",  fixed = TRUE)
  expect_match( body, "&amp;",          fixed = TRUE)
  expect_match( body, "&quot;quoted&quot;", fixed = TRUE)
})

test_that("report_html() shows trace, ambiguities, missing data and warnings", {
  res <- make_minimal_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  report_html(res, file = out)
  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, "Key trace",    fixed = TRUE)
  expect_match(body, "Ambiguities",  fixed = TRUE)
  expect_match(body, "argic features absent",       fixed = TRUE)
  expect_match(body, "ECEC",         fixed = TRUE)
  expect_match(body, "Andic + ferralic both passed", fixed = TRUE)
})

test_that("report_html() includes horizons table when a PedonRecord is supplied", {
  pedon <- PedonRecord$new(
    site = list(id = "test-pedon", lat = -22.5, lon = -43.7,
                country = "BR", parent_material = "gneiss"),
    horizons = data.frame(
      top_cm    = c(0,  15, 65),
      bottom_cm = c(15, 65, 130),
      designation = c("A", "Bw1", "Bw2"),
      clay_pct  = c(50, 60, 65),
      ph_h2o    = c(4.8, 4.7, 4.7)
    )
  )
  res <- make_minimal_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  report_html(res, file = out, pedon = pedon)
  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, ">Horizons<",    fixed = TRUE)
  expect_match(body, "Bw1",           fixed = TRUE)
  expect_match(body, "test-pedon",    fixed = TRUE)
})


# ---- R6 method delegation --------------------------------------------------


test_that("ClassificationResult$report() delegates to the generic", {
  res <- make_minimal_result()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  res$report(out)
  expect_true(file.exists(out))
  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, "Rhodic Ferralsol", fixed = TRUE)
})


# ---- PDF output (skipped without LaTeX) -----------------------------------


test_that("report_pdf() errors actionably when rmarkdown is missing", {
  skip_if(requireNamespace("rmarkdown", quietly = TRUE),
          "rmarkdown is installed; cannot exercise the missing-package path")
  res <- make_minimal_result()
  expect_error(
    report_pdf(res, file = tempfile(fileext = ".pdf")),
    regexp = "rmarkdown",
    ignore.case = TRUE
  )
})

test_that("report_pdf() round-trips through rmarkdown when available", {
  skip_if_not_installed("rmarkdown")
  skip_on_cran()
  # Skip on CI -- LaTeX is rarely installed and the test is slow.
  skip_if(!nzchar(Sys.which("pdflatex")) && !nzchar(Sys.which("xelatex")),
          "no LaTeX engine on PATH")

  res <- make_minimal_result()
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)
  report_pdf(res, file = out)
  expect_true(file.exists(out))
  expect_gt(file.info(out)$size, 1000)
})
