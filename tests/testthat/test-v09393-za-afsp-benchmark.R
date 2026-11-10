# v0.9.393: reading South African labels (.za_match_label()) for the offline
# AfSP benchmark (inst/benchmarks/za1991_afsp.R). Synthetic labels and
# profiles only, built from the names of the rule files and the examples;
# no AfSP profile or label is shipped.

.m393 <- function(x, ...) soilKey:::.za_match_label(x, ...)
# the benchmark script: installed (R CMD check), else the source tree
.script393 <- function() {
  f <- system.file("benchmarks", "za1991_afsp.R", package = "soilKey")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "benchmarks", "za1991_afsp.R")
  skip_if_not(file.exists(f))
  e <- new.env()
  sys.source(f, envir = e)
  attr(e, "file") <- f
  e
}

test_that("every form name is read in the three shapes, with its era", {
  skip_on_cran()
  f <- soilKey:::.za_label_tables()$forms
  expect_identical(nrow(f), 74L)
  m <- .m393(paste(f$name, "Xseries"))
  expect_identical(m$form_code, f$code)
  expect_true(all(m$era == "1977" & m$series == "Xseries" & m$matched_by == "name"))
  m <- .m393(paste0(f$name, ", Xseries"))
  expect_identical(m$form_code, f$code)
  expect_true(all(m$era == "1977"))
  m <- .m393(toupper(f$name))
  expect_identical(m$form_code, f$code)
  expect_true(all(m$era == "form_only"))
  expect_identical(.m393(c("Hutton,", "Hutton Form"))$era, c("form_only", "form_only"))
  expect_identical(soilKey:::.za_form_name(c("Hu", "[Ia]", "Zz")), c("Hutton", "Inanda", NA))
})

test_that("every family is read from a 1991 label of its form, and only of its form", {
  skip_on_cran()
  tab <- soilKey:::.za_label_tables()
  fam <- tab$families
  expect_identical(nrow(fam), 404L)
  fn <- tab$forms$name[match(fam$form, tab$forms$code)]
  m <- .m393(sprintf("%s Form, %s Family", fn, fam$name))
  expect_identical(m$family_code, fam$code)
  expect_identical(m$form_code, fam$form)
  expect_true(all(m$era == "1991" & is.na(m$family_unread)))
  # a family of another form is not read, but reported
  m <- .m393("Hutton Form, Glenariff Family")
  expect_identical(m$form_code, "Hu")
  expect_true(is.na(m$family_code))
  expect_identical(m$family_unread, "Glenariff")
  m <- .m393("Inanda Form, Glenariff Family")
  expect_identical(m$family_code, "[Ia]2200")
})

test_that("exact names only; the misspellings are tried only on an unread label, flagged", {
  skip_on_cran()
  m <- .m393(c("Huton Xseries", "Hutton-Xseries", "", NA, "Avalon/Xseries"))
  expect_true(all(is.na(m$form_code)))
  expect_identical(m$era[1], "1977")
  expect_identical(m$matched_by, rep(NA_character_, 5))
  env <- .script393()
  v <- env$.za_afsp_variants
  expect_identical(unname(v), c("Avalon", "Houwhoek", "Houwhoek", "Kranskop"))
  m <- .m393(c("Avanlon Xseries", "Houhoek", "Houwbroek, Xseries", "Kraanskop", "Kraanskop, Xseries",
               "Kranskop Xseries", "Huton Xseries"), variants = v)
  expect_identical(m$form_code, c("Av", "Hh", "Hh", "Kp", "Kp", "Kp", NA))
  expect_identical(m$matched_by, c(rep("variant", 5), "name", NA))
  expect_identical(m$era, c("1977", "form_only", "1977", "form_only", "1977", "1977", "1977"))
  expect_identical(nrow(.m393(character(0))), 0L)
  # the misspellings are tried on the label with its spaces normalised
  m <- .m393(c("  Kraanskop   Xseries ", "Avanlon\tXseries"), variants = v)
  expect_identical(m$form_code, c("Kp", "Av"))
  expect_identical(m$matched_by, c("variant", "variant"))
})

test_that("a rest starting with Form is no 1977 series: the label is not read", {
  skip_on_cran()
  m <- .m393(c("Hutton Form Lillieburn Family", "Avalon Form, Woodburn", "Avalon Form Woodburn",
               "Hutton Formosa", "Hutton  Form,  Lillieburn  Family"))
  expect_identical(m$era, c(NA, NA, NA, "1977", "1991"))
  expect_identical(m$form_code, c(NA, NA, NA, "Hu", "Hu"))
  expect_identical(m$series[4], "Formosa")
  expect_identical(m$family_code[5], "Hu1100")
})

test_that("the benchmark script parses and keeps the third-party data out", {
  skip_on_cran()
  env <- .script393()
  f <- attr(env, "file")
  expect_true(is.function(env$za1991_afsp_benchmark))
  expect_equal(env$.za_afsp_wilson(0, 0), c(NA_real_, NA_real_))
  ci <- env$.za_afsp_wilson(50, 100)
  expect_true(ci[1] < 0.5 && ci[2] > 0.5)
  expect_equal(env$.za_afsp_wilson(122, 641), c(0.162, 0.223), tolerance = 0.002)
  # the baseline keeps the random state of the caller and is reproducible
  set.seed(42)
  before <- .Random.seed
  cand <- list(c("Hu", "Cv"), "Gf", c("Ms", "Hu"), "Cv")
  b1 <- env$.za_afsp_cand_baseline(c("Hu", "Gf", "Ms", "Cv"), cand, B = 200L)
  expect_identical(.Random.seed, before)
  expect_identical(env$.za_afsp_cand_baseline(c("Hu", "Gf", "Ms", "Cv"), cand, B = 200L), b1)
  expect_true(b1[["lo"]] <= b1[["mean"]] && b1[["mean"]] <= b1[["hi"]])
  expect_true(all(is.na(env$.za_afsp_cand_baseline(character(0), list()))))
  expect_identical(names(env$.za_afsp_family_notes), c("Glengazi", "Cleveland", "Greendal"))
  old <- Sys.getenv("SOILKEY_AFSP_DIR")
  on.exit(Sys.setenv(SOILKEY_AFSP_DIR = old), add = TRUE)
  Sys.setenv(SOILKEY_AFSP_DIR = "")
  op <- options(soilKey.afsp_dir = NULL)
  on.exit(options(op), add = TRUE)
  nodir <- file.path(tempdir(), "no-such-afsp-dir")
  expect_message(expect_null(env$za1991_afsp_dir(nodir, must = FALSE)), "downloads nothing")
  expect_error(env$za1991_afsp_dir(nodir), "downloads nothing")
  txt <- readLines(f)
  expect_false(any(grepl("download\\.file|httr|curl::|url\\(", txt)))
})

test_that("the result reader of the script", {
  skip_on_cran()
  env <- .script393()
  r <- classify_za1991(make_za1991_example("hutton"), on_missing = "silent")
  s <- env$.za_afsp_result(r)
  expect_identical(c(s$sk_form, s$sk_family, s$outcome), c("Hu", "Hu1100", "decided"))
  expect_false(s$prov_form)
  r <- classify_za1991(make_za1991_example("undecided"), on_missing = "silent")
  s <- env$.za_afsp_result(r)
  expect_identical(s$outcome, "undecided")
  expect_true(nzchar(s$candidates))
  r <- classify_za1991(make_za1991_example("lithosol"), on_missing = "silent")
  expect_identical(env$.za_afsp_result(r)$outcome, "land_class")
  expect_true(is.na(env$.za_afsp_result(NULL)$sk_form))
})

test_that("the benchmark runs end to end on synthetic profiles and labels", {
  skip_on_cran()
  env <- .script393()
  ex <- c("hutton", "clovelly", "undecided", "lithosol", "inanda")
  peds <- lapply(seq_along(ex), function(i) {
    p <- make_za1991_example(ex[i])
    p$site$id <- paste0("S", i)
    p
  })
  und <- classify_za1991(peds[[3]], on_missing = "silent")
  cand <- sub("^forms:", "", grep("^forms:", und$provisional_because, value = TRUE))
  labels <- data.frame(
    id = c("S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8"),
    label = c("Hutton Form, Lillieburn Family", "Clovelly Xseries", paste(soilKey:::.za_form_name(cand[1]), "Xseries"),
              "Mispah", "Hutton, Xseries", "Hutton Xseries", "Huton Xseries", "Avalon Form, Xfamily"),
    year = c(1995L, 1980L, 1980L, NA, 1999L, 1980L, 1980L, 1980L), stringsAsFactors = FALSE)
  out <- tempfile(fileext = ".md")
  b <- env$za1991_afsp_benchmark(pedons = peds, labels = labels, out = out, verbose = FALSE)
  st <- b$status
  expect_identical(c(st$profiles, st$labelled, st$read, st$unmatched, st$no_pedon, st$scored),
                   c(8L, 8L, 6L, 2L, 1L, 5L))
  # every labelled profile is in one row of the shapes
  expect_identical(sum(as.matrix(b$by_era)), st$labelled)
  expect_identical(as.integer(b$by_era["unrecognised", ]), c(0L, 1L))
  cd <- b$candidates
  expect_identical(c(cd$n, cd$hit, sum(cd$by_size$n), sum(cd$by_size$hit)), c(1L, 1L, 1L, 1L))
  expect_identical(cd$by_size$size, c("1-5", "6-20", "21-40", "over 40"))
  sc <- b$score
  a <- sc[sc$group == "All", ]
  # S1 and S2 agree; S3 undecided with its label among the candidates;
  # S4 the land class; S5 Inanda against a Hutton label
  expect_identical(c(a$n, a$agree, a$decided, a$undecided, a$cand_hit, a$land_class),
                   c(5L, 2L, 3L, 1L, 1L, 1L))
  expect_equal(a$pct, 40)
  expect_true(a$lo < 40 && a$hi > 40)
  expect_identical(sc$group, c("All", "1977 binomial (Form Series)", "1991 (Form, Family)", "Form named alone",
                               "Read by a listed misspelling"))
  expect_identical(sc$n[2:4], c(3L, 1L, 1L))
  fm <- b$family
  expect_identical(c(fm$labels_1991, fm$family_read, fm$sk_family, fm$agree), c(1L, 1L, 1L, 1L))
  expect_true(any(b$confusion$label == "Hu" & b$confusion$soilKey == "[Ia]"))
  md <- readLines(out)
  for (h in c("^## Labels read", "^## Form agreement", "^## Family agreement", "^## By form of the label",
              "^## Confusions"))
    expect_true(any(grepl(h, md)), info = h)
  expect_identical(env$.za_afsp_report(b), md)
  expect_true(any(grepl("^Baseline: the candidate lists", md)))
  expect_true(any(grepl("^Not in the rate:", md)))
  expect_true(any(grepl("Shape not recognised", md)))
  expect_true(any(grepl("No form at all", md)))
  # S1 is in the 1991 shape and surveyed in 1995: no claim of conversion
  expect_true(any(grepl("^Of the 1 labels in the 1991 shape, 1 have a survey year and 0 of them", md)))
  expect_false(any(grepl("\u2014", md)))
})
