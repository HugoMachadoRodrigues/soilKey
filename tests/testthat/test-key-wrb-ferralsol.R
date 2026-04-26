test_that("classify_wrb2022 assigns Ferralsols to canonical Ferralsol fixture", {
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr)
  expect_s3_class(res, "ClassificationResult")
  expect_equal(res$rsg_or_order, "Ferralsols")
  expect_equal(res$evidence_grade, "A")
})

test_that("trace evaluates RSGs before Ferralsols without short-circuiting", {
  # In v0.1 the RSGs before FR were all not_implemented_v01 stubs
  # returning NA. In v0.3 they are all wired with real diagnostics;
  # for the canonical Ferralsol fixture they correctly fail (FALSE)
  # or, where a required attribute is missing, return NA. The key
  # property we test is that none short-circuits with passed=TRUE
  # and that FR PASSES.
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr, on_missing = "silent")

  pre_fr <- res$trace[1:13]
  pre_passed <- vapply(pre_fr, function(t) isTRUE(t$passed), logical(1))
  expect_false(any(pre_passed))

  expect_true(isTRUE(res$trace$FR$passed))
})

test_that("Luvisol fixture classifies as Luvisols (v0.2 wired)", {
  # In v0.1 this fixture landed at the RG catch-all because no argic-
  # derived RSG was wired. In v0.2 LV is wired and the fixture is a
  # canonical Luvisol (high CEC, low Al sat).
  pr <- make_luvisol_canonical()
  res <- classify_wrb2022(pr)
  expect_equal(res$rsg_or_order, "Luvisols")
})

test_that("Chernozem fixture classifies as Chernozems (v0.2 wired)", {
  # In v0.1 this fixture landed at the RG catch-all. In v0.2 CH is
  # wired with the chernozem() RSG diagnostic (mollic + carbonates +
  # chroma <= 2 in upper 20 cm).
  pr <- make_chernozem_canonical()
  res <- classify_wrb2022(pr)
  expect_equal(res$rsg_or_order, "Chernozems")
})

test_that("evidence_grade B when an attribute is predicted_spectra", {
  pr <- make_ferralsol_canonical()
  pr$add_measurement(4, "clay_pct", 60, "predicted_spectra",
                       confidence = 0.85, overwrite = TRUE)
  res <- classify_wrb2022(pr)
  expect_equal(res$evidence_grade, "B")
})

test_that("evidence_grade D when an attribute is extracted_vlm", {
  pr <- make_ferralsol_canonical()
  pr$add_measurement(1, "clay_pct", 50, "extracted_vlm",
                       confidence = 0.7, overwrite = TRUE)
  res <- classify_wrb2022(pr)
  expect_equal(res$evidence_grade, "D")
})

test_that("on_missing = 'error' aborts when attributes are missing", {
  pr <- make_ferralsol_canonical()
  pr$horizons$cec_cmol <- NA_real_
  expect_error(classify_wrb2022(pr, on_missing = "error"))
})

test_that("on_missing = 'silent' suppresses missing-data warnings", {
  pr <- make_ferralsol_canonical()
  pr$horizons$ph_kcl <- NA_real_   # not used by ferralic; missing but ignorable
  res <- classify_wrb2022(pr, on_missing = "silent")
  has_missing_warn <- any(grepl("missing", res$warnings, ignore.case = TRUE))
  expect_false(has_missing_warn)
})

test_that("rule engine handles a custom rule set", {
  pr <- make_ferralsol_canonical()
  custom <- list(
    rsgs = list(
      list(code = "X1", name = "Hypothetical-1", tests = list()),
      list(code = "X2", name = "Hypothetical-2",
           tests = list(default = TRUE))
    )
  )
  res <- classify_wrb2022(pr, rules = custom)
  expect_equal(res$rsg_or_order, "Hypothetical-2")
})

test_that("load_rules returns a parsed rule set with 32 RSGs", {
  rules <- load_rules("wrb2022")
  expect_true("rsgs" %in% names(rules))
  expect_equal(length(rules$rsgs), 32L)
  codes <- vapply(rules$rsgs, function(r) r$code, character(1))
  expect_equal(codes[1], "HS")
  expect_equal(codes[14], "FR")
  expect_equal(codes[length(codes)], "RG")
})

test_that("HS is no longer a stub (v0.3 wiring)", {
  # In v0.1 / v0.2 HS used the not_implemented_v01 marker which set a
  # 'scheduled' note. In v0.3 HS is wired with histic_horizon and
  # the trace reports a real evaluation -- no scheduled note.
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr, on_missing = "silent")
  expect_false(is.na(res$trace$HS$passed))
})

test_that("ClassificationResult prints without error", {
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr)
  # cli writes through its own handler, which bypasses sink() and
  # capture.output(); so we test only that print() runs without error
  # and returns the object invisibly.
  expect_no_error(invisible(print(res)))
  expect_invisible(print(res))
})
