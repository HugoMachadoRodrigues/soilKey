# Tests for classify_from_documents() -- the high-level VLM entry
# point. We use MockVLMProvider so no network or local LLM is needed.


test_that("classify_from_documents() requires at least one input source", {
  expect_error(
    classify_from_documents(),
    regexp = "supply at least one"
  )
})


test_that("classify_from_documents() accepts an existing pedon and runs the keys", {
  pr <- make_ferralsol_canonical()
  res <- classify_from_documents(
    pedon    = pr,
    provider = MockVLMProvider$new(responses = list()),  # never called
    verbose  = FALSE
  )
  expect_type(res, "list")
  expect_named(res, c("pedon", "classifications", "report", "provider"),
                 ignore.order = TRUE)
  expect_s3_class(res$pedon, "PedonRecord")
  expect_true("wrb" %in% names(res$classifications))
  expect_s3_class(res$classifications$wrb, "ClassificationResult")
  expect_equal(res$classifications$wrb$rsg_or_order, "Ferralsols")
  expect_null(res$report)
})


test_that("classify_from_documents() lets the caller pick a subset of systems", {
  pr <- make_luvisol_canonical()
  res <- classify_from_documents(
    pedon    = pr,
    provider = MockVLMProvider$new(responses = list()),
    systems  = c("wrb", "sibcs"),
    verbose  = FALSE
  )
  expect_named(res$classifications, c("wrb", "sibcs"),
                 ignore.order = TRUE)
  expect_false("usda" %in% names(res$classifications))
})


test_that("classify_from_documents() rejects a bad provider type", {
  expect_error(
    classify_from_documents(
      pedon    = make_ferralsol_canonical(),
      provider = 1234L,
      verbose  = FALSE
    ),
    regexp = "provider"
  )
})


test_that("classify_from_documents() writes a report when asked", {
  pr <- make_ferralsol_canonical()
  out <- tempfile(fileext = ".html")
  on.exit(unlink(out), add = TRUE)
  res <- classify_from_documents(
    pedon    = pr,
    provider = MockVLMProvider$new(responses = list()),
    report   = out,
    verbose  = FALSE
  )
  expect_true(file.exists(out))
  expect_equal(normalizePath(res$report),
                 normalizePath(out))
  body <- paste(readLines(out, warn = FALSE), collapse = "\n")
  expect_match(body, "Ferralsol", fixed = TRUE)
  expect_match(body, "<!DOCTYPE html>", fixed = TRUE)
})


test_that("classify_from_documents() errors clearly on missing PDF / image", {
  expect_error(
    classify_from_documents(
      pdf      = "/no/such/file.pdf",
      provider = MockVLMProvider$new(responses = list()),
      verbose  = FALSE
    ),
    regexp = "PDF not found"
  )
  expect_error(
    classify_from_documents(
      image    = "/no/such/file.jpg",
      provider = MockVLMProvider$new(responses = list()),
      verbose  = FALSE
    ),
    regexp = "Image not found"
  )
})
