# v0.9.265: classify_hierarchical(), the generic classifier of ordered keys
# that the national systems will use, and the new fields of
# ClassificationResult (system_id, levels, provisional_because).

# Building blocks on the Ferralsol fixture: entisol_usda() always passes,
# gelisol_usda() fails, and an unknown diagnostic is indeterminate (NA).
.T <- list(all_of = list(list(entisol_usda = list())))
.F <- list(all_of = list(list(gelisol_usda = list())))
.N <- list(all_of = list(list(no_such_diagnostic_v09265 = list())))
.toy <- function(...) list(...)
.taxon <- function(code, tests) list(code = code, name = paste("Class", code),
                                     tests = tests)

test_that("it reproduces classify_usda on the canonical fixtures", {
  skip_on_cran()
  rules <- load_rules("usda")
  for (f in c("make_ferralsol_canonical", "make_podzol_canonical",
              "make_vertisol_canonical", "make_histosol_canonical",
              "make_luvisol_canonical", "make_solonetz_canonical",
              "make_andosol_canonical", "make_chernozem_canonical")) {
    p <- get(f, envir = asNamespace("soilKey"))()
    a <- classify_usda(p, rules = rules, on_missing = "silent")
    b <- soilKey:::classify_hierarchical("usda", p, rules = rules,
                                         on_missing = "silent")
    expect_identical(b$name, a$name, info = f)
    expect_identical(b$rsg_or_order, a$rsg_or_order, info = f)
    expect_identical(b$evidence_grade, a$evidence_grade, info = f)
    expect_identical(b$system_id, "usda")
    expect_identical(b$levels$name[nrow(b$levels)], a$name, info = f)
  }
})

test_that("a class is assigned only when its tests pass", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  rules <- list(orders = .toy(.taxon("A", .F), .taxon("B", .T)),
                suborders = list(B = .toy(.taxon("BA", .F), .taxon("BB", .F))))
  r <- soilKey:::classify_hierarchical("usda", p, rules = rules,
                                       on_missing = "silent")
  # B passes; neither suborder passes, so the name stops at the order
  expect_identical(r$levels$code, "B")
  expect_identical(r$name, "Class B")
  expect_named(r$trace, c("orders", "suborders"))
  expect_length(r$provisional_because, 0L)

  # nothing passes at the top: not classified, with a warning
  rules$orders <- .toy(.taxon("A", .F), .taxon("B", .F))
  r <- soilKey:::classify_hierarchical("usda", p, rules = rules,
                                       on_missing = "silent")
  expect_true(is.na(r$name))
  expect_identical(nrow(r$levels), 0L)
  expect_named(r$levels, c("level", "code", "name"))
  expect_true(any(grepl("not classified", r$warnings)))

  # nothing passes below the top: the name stops there, with a warning
  rules$orders <- .toy(.taxon("A", .F), .taxon("B", .T))
  r <- soilKey:::classify_hierarchical("usda", p, rules = rules,
                                       on_missing = "silent")
  expect_true(any(grepl("No class of the suborders list under B passed",
                        r$warnings)))
})

test_that("undecided classes are counted by position; codes are strings", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  # a code listed twice in one level (which the validator rejects) cannot
  # hide the undecided class tested between its two entries
  rules <- list(orders = .toy(.taxon("X", .F), .taxon("A", .N),
                              .taxon("X", .T)))
  r <- soilKey:::classify_hierarchical("usda", p, rules = rules,
                                       on_missing = "silent")
  expect_identical(r$provisional_because, "orders:A")
  # numeric codes are read as strings; a class without a code is an error
  rules <- list(orders = .toy(list(code = 11L, name = "Eleven", tests = .F),
                              list(code = 12L, name = "Twelve", tests = .T)))
  r <- soilKey:::classify_hierarchical("usda", p, rules = rules,
                                       on_missing = "silent")
  expect_identical(r$levels$code, "12")
  rules <- list(orders = .toy(list(name = "No code", tests = .T)))
  expect_error(soilKey:::classify_hierarchical("usda", p, rules = rules,
                                               on_missing = "silent"),
               "Class 1 of the orders list has no code")
})

test_that("an undecided class before the assigned one makes it provisional", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  rules <- list(orders = .toy(.taxon("A", .N), .taxon("B", .F),
                              .taxon("C", list(default = TRUE))),
                suborders = list(C = .toy(.taxon("CA", .N),
                                          .taxon("CB", .T))))
  r <- soilKey:::classify_hierarchical("usda", p, rules = rules,
                                       on_missing = "silent")
  expect_identical(r$levels$code, c("C", "CB"))
  expect_identical(r$provisional_because, c("orders:A", "suborders:CA"))
  expect_true(any(grepl("^Provisional", r$warnings)))
  expect_true("diagnostic_no_such_diagnostic_v09265" %in%
                unlist(lapply(r$trace$orders, `[[`, "missing")))
  out <- utils::capture.output(print(r), type = "message")
  expect_true(any(grepl("classes tested", out)))
})

test_that("the three classifiers report their system_id", {
  skip_on_cran()
  p <- make_ferralsol_canonical()
  res <- classify_all(p, on_missing = "silent")
  expect_identical(res$wrb$system_id, "wrb2022")
  expect_identical(res$sibcs$system_id, "sibcs5")
  expect_identical(res$usda$system_id, "usda")
  expect_null(res$wrb$levels)
  expect_identical(res$wrb$provisional_because, character(0))
  out <- utils::capture.output(print(res$wrb), type = "message")
  expect_true(any(grepl("RSGs tested", out)))
})
