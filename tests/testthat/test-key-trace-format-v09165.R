# Tests for key_trace_table() / .flatten_key_trace() (v0.9.165).
#
# Regression guard for the "$ operator is invalid for atomic vectors" crash:
# the decision trace is flat for WRB but a nested list of phases for SiBCS and
# USDA (candidate steps, assigned records, FamilyAttribute R6 objects, bare
# atomic labels, NULLs). The flattener must normalise every shape without
# crashing and without emitting garbled empty-code rows.

.trace_cols <- c("phase", "code", "name", "status", "missing", "n_missing")

test_that("key_trace_table returns the documented columns for every system", {
  p <- make_ferralsol_canonical()
  for (sys in c("wrb", "sibcs", "usda")) {
    res <- classify_all(p, systems = sys, on_missing = "silent")[[sys]]
    tt  <- key_trace_table(res)
    expect_s3_class(tt, "data.frame")
    expect_identical(names(tt), .trace_cols)
    expect_true(nrow(tt) > 0L)
    # status is always one of the five canonical values
    expect_true(all(tt$status %in%
                    c("passed", "failed", "indeterminate", "selected", "info")))
    # n_missing agrees with the comma-joined missing string
    expect_equal(tt$n_missing,
                 ifelse(nzchar(tt$missing),
                        lengths(strsplit(tt$missing, ", ", fixed = TRUE)), 0L))
  }
})

test_that("the nested SiBCS/USDA trace never crashes and is not garbled", {
  fx <- c("make_ferralsol_canonical", "make_gleysol_canonical",
          "make_histosol_canonical", "make_vertisol_canonical",
          "make_arenosol_canonical", "make_planosol_canonical")
  for (f in fx) {
    p <- get(f)()
    for (sys in c("sibcs", "usda")) {
      res <- classify_all(p, systems = sys, on_missing = "silent")[[sys]]
      tt  <- expect_no_error(key_trace_table(res))
      # hierarchical systems fill the phase/level column
      expect_true(any(nzchar(tt$phase)))
      # no "garbled group" rows: empty code + empty name + indeterminate
      garbled <- !nzchar(tt$code) & !nzchar(tt$name) & tt$status == "indeterminate"
      expect_false(any(garbled))
      # each system assigns a taxon -> at least one selected row
      expect_true(any(tt$status == "selected"))
    }
  }
})

test_that("WRB flat trace has an empty phase and real step codes", {
  res <- classify_wrb2022(make_ferralsol_canonical())
  tt  <- key_trace_table(res)
  expect_true(all(!nzchar(tt$phase)))            # flat trace -> no level column
  expect_true(all(nzchar(tt$code)))              # every WRB step has a code
  expect_true(any(tt$status == "passed"))        # Ferralsol passes FR
  expect_false(any(tt$status %in% c("selected", "info")))  # WRB has neither
})

test_that("a synthetic trace exercises every node shape deterministically", {
  # This is the crash reproducer in miniature: a flat step, a group of
  # candidate steps, an assigned record (no `passed`, has `tests`), a bare
  # atomic label (the "$ operator is invalid for atomic vectors" trigger), a
  # data.frame (ignored as a step), and NULL phases.
  trace <- list(
    # an UNNAMED flat step, as in the WRB trace -> phase stays blank
    list(code = "A", name = "Alpha", passed = TRUE, missing = character(0)),
    ordens = list(
      O = list(code = "O", name = "Org", passed = FALSE, missing = character(0)),
      L = list(code = "L", name = "Lat", passed = NA,
               missing = c("clay_pct", "oc_pct"))
    ),
    subordem_assigned = list(code = "LV", name = "Lat Verm",
                             tests = list("t1")),
    familia_label      = "argilosa",
    color_undetermined = NULL,
    a_data_frame       = data.frame(x = 1)
  )
  tt <- expect_no_error(key_trace_table(trace))

  # flat step -> phase blank, passed
  expect_equal(tt$status[tt$code == "A"], "passed")
  expect_equal(tt$phase[tt$code == "A"], "")
  # group children inherit the phase and keep their own status
  expect_equal(tt$phase[tt$code == "O"], "ordens")
  expect_equal(tt$status[tt$code == "O"], "failed")
  expect_equal(tt$status[tt$code == "L"], "indeterminate")
  expect_equal(tt$n_missing[tt$code == "L"], 2L)
  expect_equal(tt$missing[tt$code == "L"], "clay_pct, oc_pct")
  # assigned record -> selected
  expect_equal(tt$status[tt$code == "LV"], "selected")
  # the atomic label -> a single info row carrying the value, NO crash
  lab <- tt[tt$status == "info", , drop = FALSE]
  expect_equal(nrow(lab), 1L)
  expect_equal(lab$name, "argilosa")
  expect_equal(lab$phase, "familia_label")
})

test_that("real FamilyAttribute rows surface as 'info' when the family level runs", {
  res  <- classify_sibcs(make_ferralsol_canonical(), include_familia = TRUE)
  tt   <- key_trace_table(res)
  info <- tt[tt$status == "info", , drop = FALSE]
  expect_true(nrow(info) > 0L)                 # family attributes + label
  expect_true(all(nzchar(info$name)))          # each carries its value
  expect_true("argilosa" %in% info$name)       # the textural class value
})

test_that("key_trace_table accepts a raw trace list, not only a result", {
  res <- classify_sibcs(make_ferralsol_canonical())
  expect_identical(key_trace_table(res$trace), key_trace_table(res))
})

test_that("empty and NULL traces yield a zero-row frame with the right columns", {
  for (tr in list(NULL, list())) {
    tt <- key_trace_table(tr)
    expect_s3_class(tt, "data.frame")
    expect_identical(names(tt), .trace_cols)
    expect_equal(nrow(tt), 0L)
  }
})
