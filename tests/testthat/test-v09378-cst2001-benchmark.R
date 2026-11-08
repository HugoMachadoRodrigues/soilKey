# v0.9.378: the CST label matcher (.cst_match_label) and the offline ISIS
# benchmark of the Chinese Soil Taxonomy (inst/benchmarks/cst2001_isis.R).
# The labels are synthetic, built from the book's CST names.

.m378 <- function(x, ...) soilKey:::.cst_match_label(x, ...)
.script378 <- function() {
  e <- new.env()
  sys.source(system.file("benchmarks", "cst2001_isis.R", package = "soilKey"), envir = e)
  e
}

test_that("every name and alias of the key matches its own taxon; the bracketed residuals are left out", {
  skip_on_cran()
  tab <- soilKey:::.cst_label_table()
  lv <- tapply(tab$code, tab$level, function(z) length(unique(z)))
  expect_identical(as.integer(lv[c("orders", "suborders", "groups", "subgroups")]), c(14L, 39L, 138L, 588L))
  expect_identical(length(unique(tab$code)), 779L)
  expect_false(any(grepl("^\\[(E2|A1\\.4|A2\\.5)\\]$", tab$code)))
  expect_true("[A1.3.x]" %in% tab$code)   # listed in Appendix 1, not a residual
  expect_false(any(duplicated(tab$key)))
  m <- .m378(tab$form)
  expect_identical(m$code, tab$code)
  expect_true(all(m$matched_by %in% c("name", "alias")))
  expect_true(is.na(.m378("Other Ferralosols")$code))
  expect_true(is.na(.m378("[Other Ferralosols]")$code))
})

test_that("singular and plural, hyphens and case are the same name", {
  skip_on_cran()
  m <- .m378(c("Hapli-Udic Ferrosols", "Hapli-Udic Ferrosol", "hapli-udic  ferrosol",
               "Hapli Udic Ferrosol", "Ferrosol", "Udic Ferrosols"))
  expect_identical(m$code, c("K3.5", "K3.5", "K3.5", "K3.5", "K", "K3"))
  expect_identical(m$level, c("groups", "groups", "groups", "groups", "orders", "suborders"))
  expect_identical(m$name[1:2], rep("Hapli-Udic Ferrosols", 2))
})

test_that("the printed aliases are matched, flagged as aliases", {
  skip_on_cran()
  # a hyphenation variant is the name itself; a printed form that differs in
  # its letters is an alias
  m <- .m378(c("Hi-weatheri-Udic Ferrosols", "Hiweatheri-Udic Ferrosol", "Udic-Vitric Andosols",
               "Hapli-Udic Ferrosols; Udic-Vitric Andosol"))
  expect_identical(m$code, c("K3.2", "K3.2", "D2.2", "K3.5"))
  expect_identical(m$matched_by, c("name", "name", "alias", "name"))
  expect_identical(m$name[1:3], c("Hiweatheri-Udic Ferrosols", "Hiweatheri-Udic Ferrosols", "Udi-Vitric Andosols"))
})

test_that("misspellings match only through the explicit variant list, and an exact match wins", {
  skip_on_cran()
  v <- .script378()$.cst_isis_variants
  labs <- c("Some genetic name; Acidi-Undic Cambosol", "Some genetic name; Isohumisol",
            "Some genetic name; Isohumisols")
  # no generic tolerance: one letter off is unmatched without the list
  expect_true(all(is.na(.m378(labs)$code)))
  expect_true(is.na(.m378("Some genetic name; Acidi-Udik Cambosol", variants = v)$code))
  m <- .m378(labs, variants = v)
  expect_identical(m$code, c("M5.6", "J", "J"))
  expect_identical(m$level, c("groups", "orders", "orders"))
  expect_identical(m$matched_by, rep("variant", 3))
  expect_identical(m$part[1], "Acidi-Undic Cambosol")
  # an exact match beats a variant, whatever the depth
  m2 <- .m378("Isohumosols; Acidi-Undic Cambosol", variants = v)
  expect_identical(m2$code, "J")
  expect_identical(m2$matched_by, "name")
})

test_that("label forms: the CST part, a missing part, a genetic name only, a subgroup", {
  skip_on_cran()
  m <- .m378(c("Some genetic name; Hapli-Udic Ferrosol",
               "Earth-cumuli-Orthic Anthrosol;Old local classification: y",
               "Some genetic name;",
               "Some genetic name",
               "Some genetic name; Panic Sali-Orthic Aridosol",
               "Ferri-Udic Cambosol",
               NA, ""))
  expect_identical(m$code, c("K3.5", "B2.4", NA, NA, "G2.2.4", "M5.5", NA, NA))
  expect_identical(m$level[5], "subgroups")
  expect_identical(m$name[5], "Panic Sali-Orthic Aridosols")
  expect_identical(m$part[1], "Hapli-Udic Ferrosol")
  expect_true(is.na(m$part[3]))
})

test_that(".cst_code_at gives the code at each level, NA below the label", {
  skip_on_cran()
  f <- soilKey:::.cst_code_at
  expect_identical(f(c("K3.1.2", "K3.1", "K3", "K", NA), "orders"), c("K", "K", "K", "K", NA))
  expect_identical(f(c("K3.1.2", "K3.1", "K3", "K"), "suborders"), c("K3", "K3", "K3", NA))
  expect_identical(f(c("K3.1.2", "K3.1", "K3"), "groups"), c("K3.1", "K3.1", NA))
  expect_identical(f(c("K3.1.2", "K3.1"), "subgroups"), c("K3.1.2", NA))
})

test_that("the benchmark script stops clearly without a cache", {
  skip_on_cran()
  e <- .script378()
  old <- Sys.getenv("SOILKEY_ISIS_CACHE", NA)
  on.exit(if (is.na(old)) Sys.unsetenv("SOILKEY_ISIS_CACHE") else Sys.setenv(SOILKEY_ISIS_CACHE = old), add = TRUE)
  Sys.setenv(SOILKEY_ISIS_CACHE = tempfile(fileext = ".rds"))
  skip_if(file.exists(file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds")))
  expect_error(e$cst2001_isis_benchmark(cache = tempfile(fileext = ".rds"), verbose = FALSE), "No ISIS cache found")
  expect_identical(e$.cst_isis_wilson(0, 0), c(NA_real_, NA_real_))
  ci <- e$.cst_isis_wilson(26, 50)
  expect_equal(round(100 * ci, 1), c(38.5, 65.2))
})

test_that("the benchmark runs end to end on the ISIS cache (skipped without it)", {
  skip_on_cran()
  cache <- Sys.getenv("SOILKEY_ISIS_CACHE", "")
  if (!nzchar(cache)) cache <- file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds")
  skip_if_not(file.exists(cache))
  e <- .script378()
  out <- tempfile(fileext = ".md")
  b <- e$cst2001_isis_benchmark(cache = cache, out = out, verbose = FALSE)
  expect_identical(b$n_china, 51L)
  expect_identical(nrow(b$profiles), 51L)
  expect_identical(b$score$level, c("orders", "suborders", "groups", "subgroups"))
  # the denominators follow the labels: 50 matched, CN049 at order level, CN014 at subgroup level
  expect_identical(b$score$n, c(50L, 49L, 49L, 1L))
  expect_identical(b$unmatched$profile, "CN043")
  expect_setequal(b$variants$profile, c("CN008", "CN049"))
  expect_false(any(b$profiles$error))
  expect_true(all(b$score$agree <= b$score$n))
  expect_true(all(b$score$residual + b$score$stopped_above <= b$score$n - b$score$agree))
  expect_identical(sum(b$confusion), 50L)
  md <- readLines(out)
  expect_true(any(grepl("^## Agreement", md)))
  expect_true(any(grepl("^## Order confusion", md)))
  expect_true(any(grepl("CN043", md)))
})
