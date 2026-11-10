# v0.9.395 -- the notation declared in site$designation_system decides how the
# older notation (A2 = E, B3 = BC, (B) = Bi) is read, before the marks of the
# designations (Hugo's decisions Q1 to Q9, 2026-11-10).

test_that("declared values are read as old, present or undeclared", {
  skip_on_cran()
  dn <- function(v) .desg_declared_notation(list(designation_system = v))
  for (v in c("asc", "ASC", "australian", "Field Handbook", "cpcs", "CPCS 1967", "french",
              "fr", "fao1977", "FAO 1977", "usda1951", "ssm1951", "usda1962", "br_pre1988",
              "BR pre-1988"))
    expect_identical(dn(v), "old", info = v)
  for (v in c("fao", "fao2006", "FAO 2006", "wrb", "WRB 2022", "wrb2014", "usda", "kst",
              "sibcs", "cssc", "nzsc", "New Zealand", "scsj", "za", "ka5", "KA 6", "kidpr",
              "ru", "rp2008", "afes", "dbg-wrb"))
    expect_identical(dn(v), "present", info = v)
  for (v in list(NULL, NA, NA_character_, "", "NA", "none", "unknown", "SA", "RSA",
                 "nz_legacy", "NZ legacy", "kanadisch", "xyz"))
    expect_true(is.na(dn(v)), info = paste(v))
  expect_true(is.na(.desg_declared_notation(NULL)))
  expect_true(is.na(.desg_declared_notation("asc")))
})

test_that("a declared older notation reads A2, B3 and (B) without marks", {
  skip_on_cran()
  asc <- list(designation_system = "asc")
  # "A1, A2, B, C" has no mark of the older notation (a B3 is one)
  d <- c("A1", "A2", "B", "C")
  expect_false(.desg_old_notation(d))
  expect_true(.desg_old_notation(d, site = asc))
  expect_identical(.desg_master(d), d)
  expect_identical(.desg_master(d, site = asc), c("A1", "E", "B", "C"))
  expect_identical(.desg_master(c("A1", "(B)", "C"), site = list(designation_system = "cpcs")),
                   c("A1", "Bi", "C"))
  expect_identical(.desg_master(c("A1", "A2", "B3"), site = list(designation_system = "br_pre1988")),
                   c("A1", "E", "BC"))
})

test_that("a declared present notation reads no A2, B3 or (B), whatever the marks", {
  skip_on_cran()
  fao <- list(designation_system = "fao2006")
  d <- c("Ah", "A2", "B1", "B2", "B3", "B4")
  expect_true(.desg_old_notation(d))
  expect_false(.desg_old_notation(d, site = fao))
  expect_identical(.desg_master(d, site = fao), d)
  expect_identical(.desg_master(d)[c(2L, 5L)], c("E", "BC"))
  # (B) is read Bi only in an older or undeclared notation
  p <- c("Ah", "Bw", "(Bw)R", "3(Bb)C")
  expect_identical(.desg_master(p, site = fao), p)
  expect_identical(.desg_master(p)[3:4], c("BiwR", "BibC"))
  expect_identical(.desg_master(c("A", "(B)", "C"), site = list(designation_system = "sibcs")),
                   c("A", "(B)", "C"))
  # the material number is still taken off
  expect_identical(.desg_master(c("2Bt1", "IIBt"), site = fao), c("Bt1", "Bt"))
})

test_that("no declaration, nz_legacy or an unknown value: the marks decide", {
  skip_on_cran()
  d_old <- c("A1", "A2", "B21t", "B3", "C")
  d_new <- c("A1", "A2", "Bw1", "Bw2", "C")
  for (s in list(NULL, list(), list(designation_system = NA), list(designation_system = "nz_legacy"),
                 list(designation_system = "kanadisch"))) {
    expect_identical(.desg_master(d_old, site = s), c("A1", "E", "B21t", "BC", "C"))
    expect_identical(.desg_master(d_new, site = s), d_new)
  }
})

test_that("validate() notes a declared notation soilKey does not know", {
  skip_on_cran()
  hz <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bw"))
  w_of <- function(ds) {
    p <- PedonRecord$new(site = list(id = "n", designation_system = ds), horizons = hz)
    p$validate(verbose = FALSE)$warnings
  }
  expect_true(any(grepl("kanadisch", w_of("kanadisch"))))
  for (ds in c("fao2006", "asc", "nz_legacy", "none"))
    expect_false(any(grepl("designation_system", w_of(ds))), info = ds)
  expect_true(is.na(.desg_notation_note(list(designation_system = "fao2006"))))
  expect_true(is.na(.desg_notation_note(list())))
  expect_match(.desg_notation_note(list(designation_system = "xyz")), "not a notation soilKey knows")
})

test_that("the readers of one designation at a time take the profile and the site", {
  skip_on_cran()
  # the reading of one designation follows the notation of its profile
  expect_identical(.desg_master("A2"), "A2")
  expect_identical(.desg_master("A2", profile = c("A1", "A2", "B21t")), "E")
  expect_identical(.desg_master("A2", profile = c("A1", "A2", "B21t"),
                                site = list(designation_system = "fao2006")), "A2")
  expect_identical(.desg_master("A2", site = list(designation_system = "asc")), "E")
  # the plough reading (qualifiers of the WRB) and the cemented reading
  fao <- list(designation_system = "fao2006")
  expect_true(.wrb_plough_designation("(B)p"))
  expect_false(.wrb_plough_designation("(B)p", site = fao))
  expect_identical(.wrb_plough_designation(c("Ap", "A2"), profile = c("Ap", "A2", "B21"), site = fao),
                   c(TRUE, FALSE))
  h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("Ah", "(B)km"))
  expect_identical(.wrb_cemented_limiting(h), c(FALSE, TRUE))
  expect_identical(.wrb_cemented_limiting(h, site = fao), c(FALSE, FALSE))
  # a profile whose single A2 is read with the B21 of another horizon
  h2 <- data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                   designation = c("A1", "A2", "B21km"))
  expect_identical(.wrb_cemented_limiting(h2), c(FALSE, FALSE, TRUE))
})

test_that("the NSD reader declares nz_legacy, read by the NZSC key as New Zealand", {
  skip_on_cran()
  expect_true(.nzsc_notation(c("A11", "B21"), list(designation_system = "nz_legacy"))$nz)
  expect_true(.nzsc_notation(c("A", "Bw"), list(designation_system = "nzsc"))$nz)
  expect_false(.scsj_notation(list(designation_system = "nz_legacy"))$ok)
  expect_identical(.frozen_designation_pattern(list(designation_system = "nz_legacy")), "perma")
  # the reader's own output is checked in test-v09334-nsd-reader.R
  expect_true(is.na(.desg_declared_notation(list(designation_system = "nz_legacy"))))
})
