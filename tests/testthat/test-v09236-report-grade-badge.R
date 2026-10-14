# v0.9.236: the evidence-grade badge of the HTML report has its class again.
#
# The report colours the evidence grade with a class of its own ("grade
# grade-a" ... "grade grade-d"), given by an internal .grade_class(). v0.9.231
# added an internal function of the same name, to read the grade of a soil
# structure, in a file that is loaded later; it replaced the report's, and
# every badge was written with class="NA", without its colour. The reader of
# field grades is now .field_grade_class(), and no name is defined twice.

test_that("the report gives the evidence grade its class", {
  expect_equal(.grade_class("A"), "grade grade-a")
  expect_equal(.grade_class("B"), "grade grade-b")
  expect_equal(.grade_class("C"), "grade grade-c")
  expect_equal(.grade_class("D"), "grade grade-d")
  expect_equal(.grade_class(NA), "grade grade-na")
  expect_equal(.grade_class(NULL), "grade grade-na")
})

test_that("the grade of a structure or of clay films is read by its own function", {
  expect_equal(.field_grade_class(c("Fraca", "moderate", "Forte", "Fraca a moderada")),
               c(1L, 2L, 3L, 1L))
  # the two do not answer for each other
  expect_true(is.na(.field_grade_class("A")))
  expect_equal(.grade_class("moderate"), "grade grade-na")
})

test_that("a generated report carries the class and no class=\"NA\"", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  res <- list(classify_wrb2022(p, on_missing = "silent"),
              classify_sibcs(p, on_missing = "silent"),
              classify_usda(p, on_missing = "silent"))
  f <- tempfile(fileext = ".html"); on.exit(unlink(f), add = TRUE)
  report_html(res, file = f, pedon = p)
  html <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  expect_false(grepl("class=\"NA\"", html, fixed = TRUE))
  badges <- regmatches(html, gregexpr("<span class=\"grade grade-(a|b|c|d|na)\">", html))[[1]]
  # one badge in each system card and one in each row of the summary table
  expect_gte(length(badges), 2L * length(res))
})

test_that("no name is defined twice in the package sources", {
  skip_on_cran()
  # Files are loaded in alphabetical order, and a second definition replaces
  # the first without a word. Read from the sources, so only where they are.
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src) && length(list.files(src, pattern = "[.][Rr]$")) > 50L,
              "package sources not available")
  nm <- character(0); fl <- character(0)
  for (f in list.files(src, pattern = "[.][Rr]$", full.names = TRUE)) {
    for (e in parse(f, keep.source = FALSE)) {
      if (!is.call(e) || !is.name(e[[1]])) next
      if (!(as.character(e[[1]]) %in% c("<-", "="))) next
      if (!is.name(e[[2]])) next
      nm <- c(nm, as.character(e[[2]])); fl <- c(fl, basename(f))
    }
  }
  dup <- unique(nm[duplicated(nm)])
  info <- vapply(dup, function(n) paste0(n, ": ", paste(fl[nm == n], collapse = ", ")), "")
  expect_identical(unname(info), character(0))
})
