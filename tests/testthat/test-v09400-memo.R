# v0.9.400: the caches of R/memo.R make the classifiers faster and must never
# give a stale result: a pedon modified after a classification is read anew,
# a declared notation that changes changes the reading, a memoised result is
# a fresh object, and the warnings of a memoised call are given again.

.plain_result <- function(res) {
  keep <- c("system", "name", "rsg_or_order", "qualifiers", "trace", "ambiguities",
            "missing_data", "evidence_grade", "warnings", "provisional_because", "levels")
  lapply(stats::setNames(keep, keep), function(k) res[[k]])
}

test_that("a pedon modified after a classification is classified anew", {
  skip_on_cran()
  p <- make_luvisol_canonical()
  other <- make_ferralsol_canonical()
  for (fn in list(classify_wrb2022, classify_usda, classify_sibcs)) {
    q <- p$clone(deep = TRUE)
    first <- suppressWarnings(fn(q, on_missing = "silent"))
    q$horizons <- data.table::copy(other$horizons)
    again <- suppressWarnings(fn(q, on_missing = "silent"))
    fresh <- suppressWarnings(fn(PedonRecord$new(site = p$site, horizons = data.table::copy(other$horizons)),
                                 on_missing = "silent"))
    expect_false(identical(first$name, again$name))
    expect_identical(.plain_result(again), .plain_result(fresh))
  }
})

test_that("one horizon changed after a classification changes the result", {
  skip_on_cran()
  p <- make_luvisol_canonical()
  before <- suppressWarnings(classify_usda(p, on_missing = "silent"))
  h <- data.table::copy(p$horizons)
  h$designation <- "R"
  h$top_cm[1] <- 0
  p$horizons <- h
  after <- suppressWarnings(classify_usda(p, on_missing = "silent"))
  fresh <- suppressWarnings(classify_usda(PedonRecord$new(site = p$site, horizons = h),
                                          on_missing = "silent"))
  expect_identical(.plain_result(after), .plain_result(fresh))
  expect_false(identical(before$name, after$name))
})

test_that("a changed declared notation changes the reading of the designations", {
  ns <- asNamespace("soilKey")
  d <- c("A1", "A2", "Bt")
  expect_identical(ns$.desg_master(d, site = list(designation_system = "usda1951")), c("A1", "E", "Bt"))
  expect_identical(ns$.desg_master(d, site = list(designation_system = "fao2006")), c("A1", "A2", "Bt"))
  expect_identical(ns$.desg_master(d, site = list(designation_system = "usda1951")), c("A1", "E", "Bt"))
  # no declaration: the marks decide (B21 is the older notation)
  expect_identical(ns$.desg_master(d, profile = c(d, "B21"), site = NULL), c("A1", "E", "Bt"))
  expect_identical(ns$.desg_master(d, site = NULL), c("A1", "A2", "Bt"))
  expect_identical(ns$.desg_master(c(NA, "NA", " 2Bt "), site = NULL), c(NA, "NA", "Bt"))
  expect_true(ns$.desg_old_notation(c("A", "B21"), site = NULL))
  expect_false(ns$.desg_old_notation(c("A", "B21"), site = list(designation_system = "fao")))
})

test_that("a pedon whose declared notation changes is classified anew", {
  skip_on_cran()
  p <- make_luvisol_canonical()
  h <- data.table::copy(p$horizons)
  h$designation[2] <- "A2"
  p$horizons <- h
  p$site$designation_system <- "usda1951"
  old <- suppressWarnings(classify_wrb2022(p, on_missing = "silent"))
  p$site$designation_system <- "fao2006"
  present <- suppressWarnings(classify_wrb2022(p, on_missing = "silent"))
  fresh <- suppressWarnings(classify_wrb2022(PedonRecord$new(site = p$site, horizons = h),
                                             on_missing = "silent"))
  expect_identical(.plain_result(present), .plain_result(fresh))
  p$site$designation_system <- "usda1951"
  expect_identical(.plain_result(suppressWarnings(classify_wrb2022(p, on_missing = "silent"))),
                   .plain_result(old))
})

test_that("the memo is held only inside a classification and follows the pedon", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- make_podzol_canonical()
  expect_null(ns$.memo_state$slots)
  n <- 0L
  f <- function() { n <<- n + 1L; n }
  expect_identical(ns$.memo(p, "test", list(), f()), 1L)
  expect_identical(ns$.memo(p, "test", list(), f()), 2L)   # no scope, no memo
  expect_true(ns$.memo_open())
  expect_false(ns$.memo_open())                            # one scope at a time
  expect_identical(ns$.memo(p, "test", list(), f()), 3L)
  expect_identical(ns$.memo(p, "test", list(), f()), 3L)
  expect_identical(ns$.memo(p, "test", list(1), f()), 4L)  # other arguments
  withr::with_options(list(soilKey.memo_test = TRUE),
                      expect_identical(ns$.memo(p, "test", list(), f()), 5L))
  q <- p$clone(deep = TRUE)
  expect_identical(ns$.memo(q, "test", list(), f()), 6L)   # another pedon object
  h <- data.table::copy(p$horizons)
  h$clay_pct[1] <- 99
  p$horizons <- h                                          # the pedon modified
  expect_identical(ns$.memo(p, "test", list(), f()), 7L)
  p$site$id <- "changed"
  expect_identical(ns$.memo(p, "test", list(), f()), 8L)
  expect_identical(ns$.memo(p, "test", list(), f()), 8L)
  ns$.memo_close()
  expect_null(ns$.memo_state$slots)
  expect_identical(ns$.memo(p, "test", list(), f()), 9L)
})

test_that("a memoised diagnostic returns a new object and gives its warnings again", {
  ns <- asNamespace("soilKey")
  p <- make_podzol_canonical()
  expect_true(ns$.memo_open())
  on.exit(ns$.memo_close(), add = TRUE)
  a <- spodic(p)
  a$passed <- "changed"
  b <- spodic(p)
  expect_false(identical(a, b))
  expect_identical(b$passed, ns$.spodic_uncached(p)$passed)
  w <- function() { warning("first call"); message("a note"); 1 }
  expect_warning(expect_message(ns$.memo(p, "warns", list(), w()), "a note"), "first call")
  expect_warning(expect_message(ns$.memo(p, "warns", list(), w()), "a note"), "first call")
})

test_that("rule sets are read once and a changed copy does not change them", {
  ns <- asNamespace("soilKey")
  r1 <- load_rules("wrb2022")
  expect_identical(r1, ns$.load_rules_read("wrb2022", "soilKey"))
  r1$rsgs <- NULL
  expect_false(is.null(load_rules("wrb2022")$rsgs))
  expect_identical(load_rules("wrb"), load_rules("wrb2022"))
  expect_error(load_rules("wrb2022", package = "stats"), "Rule file not found")
})

test_that("a horizon table changed in place inside a scope is seen", {
  ns <- asNamespace("soilKey")
  p <- make_podzol_canonical()
  expect_true(ns$.memo_open())
  on.exit(ns$.memo_close(), add = TRUE)
  before <- spodic(p)
  data.table::set(p$horizons, j = "ph_h2o", value = rep(8.5, nrow(p$horizons)))
  after <- spodic(p)
  expect_identical(after$passed, ns$.spodic_uncached(p)$passed)
  expect_false(identical(before$passed, after$passed))
})

test_that("a nested result changed by a caller does not reach the next call", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- make_luvisol_canonical()
  expect_true(ns$.memo_open())
  on.exit(ns$.memo_close(), add = TRUE)
  r1 <- alfisol_usda(p)
  nested <- Filter(function(x) inherits(x, "R6"), r1$evidence)
  expect_true(length(nested) > 0L)
  k <- names(nested)[1L]
  original <- r1$evidence[[k]]$passed
  r1$evidence[[k]]$passed <- "changed"
  r2 <- alfisol_usda(p)
  expect_identical(r2$evidence[[k]]$passed, original)
  # nested lists of results are copied as well
  v <- list(a = list(b = DiagnosticResult$new(name = "x", passed = TRUE)), n = 1)
  w <- ns$.memo_copy(v)
  w$a$b$passed <- FALSE
  expect_true(v$a$b$passed)
  expect_identical(ns$.memo_copy(list(1, "a")), list(1, "a"))
})
