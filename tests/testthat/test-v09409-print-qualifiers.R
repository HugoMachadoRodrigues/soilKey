# v0.9.409: print() of a result lists only the names of its qualifiers.

.printed_qualifier_line <- function(res) {
  buf <- character()
  out <- utils::capture.output(withCallingHandlers(
    print(res),
    message = function(m) {
      buf <<- c(buf, conditionMessage(m))
      invokeRestart("muffleMessage")
    }))
  lines <- unlist(strsplit(c(out, buf), "\n"))
  first <- grep("^Qualifiers:", lines)
  if (length(first) != 1L) return("")
  last <- first
  while (last < length(lines) && !grepl("^(Evidence grade|Prior check|\u2500\u2500)", lines[last + 1L]) &&
         nzchar(lines[last + 1L]))
    last <- last + 1L
  paste(lines[first:last], collapse = " ")
}

test_that("a WRB 2022 result prints its qualifiers and not their trace", {
  res <- classify_wrb2022(make_ferralsol_canonical())
  expect_type(res$qualifiers$trace, "list")
  line <- gsub("\\s+", " ", .printed_qualifier_line(res))
  expect_identical(line, "Qualifiers: Geric, Rhodic, Clayic, Epic, Eutric, Humic")
})

test_that("an HSCS result prints its subtypes and varieties, not the status table", {
  res <- classify_hscs2024(make_hscs2024_example())
  expect_s3_class(res$qualifiers$status, "data.frame")
  line <- .printed_qualifier_line(res)
  expect_true(startsWith(line, "Qualifiers:"))
  expect_false(grepl("subtype, subtype|variety, variety", line))
  expect_true(grepl(res$qualifiers$subtypes[1], line, fixed = TRUE))
})
