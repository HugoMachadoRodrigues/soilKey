# v0.9.379: older German soil names read as KA5 codes (.ka5_match_label,
# inst/rules/ka5/legacy_names.yaml) and the offline ISIS benchmark of the KA5
# (inst/benchmarks/ka5_isis.R). The labels are synthetic, built from the names
# of the key and of the legacy file.

.m379 <- function(x, ...) soilKey:::.ka5_match_label(x, ...)
.script379 <- function() {
  e <- new.env()
  sys.source(system.file("benchmarks", "ka5_isis.R", package = "soilKey"), envir = e)
  e
}

test_that("every legacy row resolves to an existing KA5 code and carries a source and a page", {
  skip_on_cran()
  lg <- yaml::read_yaml(system.file("rules", "ka5", "legacy_names.yaml", package = "soilKey"))
  codes <- soilKey:::.ka5_code_table()
  expect_identical(as.integer(table(codes$level)[c("classes", "types", "subtypes")]), c(21L, 54L, 223L))
  expect_true(length(lg$names) >= 50L)
  for (e in lg$names) {
    expect_true(e$code %in% codes$code, info = e$name)
    expect_true(nzchar(e$source %||% "") && nzchar(e$page %||% ""), info = e$name)
    if (!is.null(e$variety)) expect_true(e$variety %in% c("v", "dy"), info = e$name)
    if (isTRUE(e$part)) expect_identical(codes$level[match(e$code, codes$code)], "types", info = e$name)
    # each row reads as its own code
    m <- .m379(e$name)
    expect_identical(m$code, e$code, info = e$name)
    expect_identical(m$matched_by, if (e$name %in% codes$name) "name" else "legacy", info = e$name)
    expect_identical(m$reading, identical(e$reading, "soilKey"), info = e$name)
  }
  expect_setequal(vapply(lg$modifiers, `[[`, "", "effect"), c("variety", "norm", "drop"))
  # no two older names share a key with different codes
  tab <- soilKey:::.ka5_label_table()
  expect_false(any(tapply(tab$code, tab$key, function(z) length(unique(z))) > 1L))
})

test_that("every name of the key reads as its own code; bracketed codes are left out", {
  skip_on_cran()
  codes <- soilKey:::.ka5_code_table()
  expect_false(any(grepl("^\\[", codes$code)))
  m <- .m379(codes$name)
  expect_identical(m$code, codes$code)
  expect_true(all(m$matched_by == "name"))
  expect_identical(m$level, codes$level)
  expect_identical(.m379("Gley-Podsol")[c("level", "code", "type", "class")],
                   data.frame(level = "subtypes", code = "GG-PP", type = "PP", class = "P", stringsAsFactors = FALSE))
})

test_that("case, umlaut spelling, the sharp s, accents and hyphen spacing are the same name", {
  skip_on_cran()
  m <- .m379(c("haftpseudogley", "HAFTPSEUDOGLEY", "Braunerde - Haftpseudogley", "Nassgley", "Naßgley",
               "Normhaftnässemarsch", "Normhaftnaessemarsch", "Lessivés", "Lessives", "Terra fusca-Parabraunerde"))
  expect_identical(m$code, c("SH", "SH", "BB-SH", "GN", "GN", "MHn", "MHn", "L", "L", "CF-LL"))
})

test_that("older type names read alone and inside joined names", {
  skip_on_cran()
  m <- .m379(c("Haftnässepseudogley", "Gley-Haftnässepseudogley", "Kalkschwarzerde", "Schwarzerde-Gley",
               "Chernozem-Pseudogley", "Braunauenboden", "Gley-Auenregosol", "Tiefumbruchboden"))
  expect_identical(m$code, c("SH", "GG-SH", "TC", "TT-GG", "TT-SS", "AB", "GG-AQ", "YU"))
  expect_identical(m$matched_by, rep("legacy", 8))
  expect_identical(m$reading, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
  # a joined name the key does not print: the type of the last part (p. 191)
  t <- .m379(c("Podsol-Rendzina", "Kalkschwarzerde-Gley", "Rendzina-Chernozem"))
  expect_identical(t$code, c("RR", "GG", "TT"))
  expect_identical(t$level, rep("types", 3))
  expect_identical(t$matched_by, rep("transition", 3))
  expect_identical(t$reading, c(FALSE, FALSE, TRUE))
  # every part must be a type name
  expect_true(all(is.na(.m379(c("Gley-Irgendwas", "Moder-Gley"))$code)))
})

test_that("the modifiers: Acker- gives the variety, Typischer the Norm subtype, Primaerer only before a Pseudogley", {
  skip_on_cran()
  m <- .m379(c("Acker-Gley", "(Acker)Gley", "Ackergley", "Acker Gley-Podsol", "Acker-Schwarzerde-Gley",
               "(Acker)Rosterde"))
  expect_identical(m$code, c("GG", "GG", "GG", "GG-PP", "TT-GG", "BB-PP"))
  expect_identical(m$variety, rep("v", 6))   # one v, not "v;v"
  expect_identical(m$modifier, rep("Acker", 6))
  n <- .m379(c("Typischer Pseudogley", "Typische Rambla", "Typisches Kolluvisol", "Typischer Haftnässepseudogley",
               "Typische Schwarzerde", "Typischer Gley-Podsol", "Typischer Moder", "Typischer",
               "Typische Chernozem-Braunerde", "Typischer Auenpelosol", "Typische Borowina", "Typischer Normgley"))
  expect_identical(n$code, c("SSn", "AOn", "YKn", "SHn", "TTn", rep(NA, 7)))
  expect_identical(n$level[1:5], rep("subtypes", 5))
  p <- .m379(c("Primärer Pseudogley-Gley", "Primäre Rendzina", "Primärer Gley",
               "Primärer Gley-Pseudogley", "Primaerer Braunerde-Pseudogley"))
  expect_identical(p$code, c(NA, NA, NA, "GG-SS", "BB-SS"))
  expect_identical(p$reading[4:5], c(FALSE, FALSE))
  expect_identical(p$modifier[4:5], rep("Primärer", 2))
  # an older name printed as such wins over the modifier rule; the KA5's own exceptions
  k <- .m379(c("Typische Kleimarsch", "Typische Flussmarsch", "Typische Kalkmarsch", "Typische Seemarsch",
               "Übergangsbrackmarsch", "Übergangs-Brackmarsch"))
  expect_identical(k$code, c("MN", "MNf", "MCn", "M", "M", "M"))
  expect_identical(k$level, c("types", "subtypes", "subtypes", "classes", "classes", "classes"))
  expect_identical(k$matched_by, rep("legacy", 6))
})

test_that("Rosterde, Latosole, and the names left to the script's variants", {
  skip_on_cran()
  m <- .m379(c("Rosterde; Kartiereinheit", "basenarme Braunerde", "Latosole", "Plastosole"))
  expect_identical(m$code, c("BB-PP", "BB", "V", "V"))
  expect_identical(m$variety, c("v", "dy", NA, NA))
  expect_identical(m$part[1], "Rosterde")
  v <- .script379()$.ka5_isis_variants
  labs <- c("Braunerde mit geringem Basengehalt", "Braunerde geringen Basengehalts",
            "Schwarzerdeähnlicher Boden", "Schwarzerdeaehnliche Boden")
  expect_true(all(is.na(.m379(labs)$code)))
  w <- .m379(labs, variants = v)
  expect_identical(w$code, c("BB", "BB", "T", NA))
  expect_identical(w$variety, c("dy", "dy", NA, NA))
  expect_identical(w$level[1:3], c("types", "types", "classes"))
  expect_identical(w$matched_by[1:3], rep("variant", 3))
})

test_that("no generic tolerance: misspellings are unmatched unless the explicit variant list reads them", {
  skip_on_cran()
  v <- .script379()$.ka5_isis_variants
  expect_true(all(is.na(.m379(c("Parabraunerd", "Parabraunerde x", "Braunerde geringem Basengehalt",
                                 NA, "", ";"))$code)))
  m <- .m379("Braunerde geringem Basengehalt", variants = v)
  expect_identical(m$code, "BB")
  expect_identical(m$variety, "dy")
  expect_identical(m$matched_by, "variant")
  # an exact match beats a variant
  m2 <- .m379("Podsol | Gley-Podsol", variants = v)
  expect_identical(m2$code, "GG-PP")
  expect_identical(m2$matched_by, "name")
  expect_identical(m2$part, "Gley-Podsol")
})

test_that("punctuation, digits, a space for a hyphen and three-part names are not read (review D1, D8)", {
  skip_on_cran()
  bad <- c("Braunerde (Pseudogley)", "Parabraunerde/Pseudogley", "Parabraunerde, Pseudogley", "Podsol, Braunerde",
           "Braunerde 2", "Braunerde.", "Gley Podsol", "Gley-Braunerde-Podsol", "Acker-Gley-Braunerde-Podsol",
           "Braunerde-", "-Braunerde", "(Gley)Podsol", "Gley--Podsol x")
  expect_true(all(is.na(.m379(bad)$code)))
  ok <- .m379(c("Gley--Podsol", "Gley - Podsol", "  Gley-Podsol  ", "Terra  fusca"))
  expect_identical(ok$code, c("GG-PP", "GG-PP", "GG-PP", "CF"))
})

test_that("Amphigley, printed for two classes, is excluded (review D3)", {
  skip_on_cran()
  lg <- yaml::read_yaml(system.file("rules", "ka5", "legacy_names.yaml", package = "soilKey"))
  expect_true("Amphigley" %in% vapply(lg$excluded, `[[`, "", "name"))
  expect_false("Amphigley" %in% vapply(lg$names, `[[`, "", "name"))
  expect_true(is.na(.m379("Amphigley")$code))
})

test_that("labels of several parts: the deepest when the others are its ancestors, else the common ancestor (review D4)", {
  skip_on_cran()
  m <- .m379(c("Podsol | Gley-Podsol", "Podsole; Gley-Podsol", "Braunerde | Gley-Podsol", "Gley | Nassgley",
               "Gley-Podsol | Braunerde-Podsol", "Kalkmarsch | Kleimarsch", "Gley-Podsol | Unbekannt"))
  expect_identical(m$code, c("GG-PP", "GG-PP", NA, "G", "PP", "M", "GG-PP"))
  expect_identical(m$matched_by, c("name", "name", NA, "ancestor", "ancestor", "ancestor", "name"))
  expect_identical(m$part[5], "Gley-Podsol | Braunerde-Podsol")
})

test_that(".ka5_code_at gives the code at each level, NA below the label", {
  skip_on_cran()
  f <- soilKey:::.ka5_code_at
  x <- c("GG-PP", "PP", "P", "GGn", NA)
  expect_identical(f(x, "classes"), c("P", "P", "P", "G", NA))
  expect_identical(f(x, "types"), c("PP", "PP", NA, "GG", NA))
  expect_identical(f(x, "subtypes"), c("GG-PP", NA, NA, "GGn", NA))
})

test_that("the readings of this part are recorded", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "ka5", "readings.yaml", package = "soilKey"))$readings
  tp <- vapply(rd, `[[`, "", "topic")
  expect_identical(sum(startsWith(tp, "Older German names")), 8L)
  expect_true(all(grepl("2026-11-08", vapply(rd[startsWith(tp, "Older German names")], `[[`, "", "reading"))))
})

test_that("the source of the matcher is ASCII", {
  skip_on_cran()
  src <- testthat::test_path("..", "..", "R", "ka5-benchmark.R")
  skip_if_not(file.exists(src), "package sources not available")
  expect_false(any(grepl("[^\\x01-\\x7f]", readLines(src, warn = FALSE), perl = TRUE)))
})

test_that("the benchmark script stops clearly without a cache", {
  skip_on_cran()
  e <- .script379()
  old <- Sys.getenv("SOILKEY_ISIS_CACHE", NA)
  on.exit(if (is.na(old)) Sys.unsetenv("SOILKEY_ISIS_CACHE") else Sys.setenv(SOILKEY_ISIS_CACHE = old), add = TRUE)
  Sys.setenv(SOILKEY_ISIS_CACHE = tempfile(fileext = ".rds"))
  skip_if(file.exists(file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds")))
  expect_error(e$ka5_isis_benchmark(cache = tempfile(fileext = ".rds"), verbose = FALSE), "No ISIS cache found")
  expect_identical(e$.ka5_isis_wilson(0, 0), c(NA_real_, NA_real_))
  ci <- e$.ka5_isis_wilson(1, 15)
  expect_equal(round(100 * ci, 1), c(1.2, 29.8))
})

test_that("the benchmark runs end to end on the ISIS cache (skipped without it)", {
  skip_on_cran()
  cache <- Sys.getenv("SOILKEY_ISIS_CACHE", "")
  if (!nzchar(cache)) cache <- file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds")
  skip_if_not(file.exists(cache))
  e <- .script379()
  out <- tempfile(fileext = ".md")
  b <- e$ka5_isis_benchmark(cache = cache, out = out, verbose = FALSE)
  expect_identical(b$n_germany, 24L)
  expect_identical(nrow(b$profiles), 24L)
  expect_identical(b$score$level, c("classes", "types", "subtypes"))
  # 16 labels, all read: one at class level, nine at type level, six at subtype level
  expect_identical(b$score$labelled, c(16L, 15L, 6L))
  expect_identical(nrow(b$unmatched), 0L)
  expect_length(b$unlabelled, 8L)
  expect_identical(b$variants$profile, c("DE008", "DE011"))
  expect_identical(b$profiles$lab_types[b$profiles$profile == "DE004"], "PP")
  expect_true(all(b$score$agree <= b$score$n))
  expect_true(all(b$score$n <= b$score$labelled))
  md <- readLines(out)
  expect_true(any(grepl("^## Agreement", md)))
  expect_true(any(grepl("^## Labels read through a soilKey reading", md)))
})
