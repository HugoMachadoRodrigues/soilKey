# v0.9.392: reading FAO-Unesco labels (.fao_match_label()) for the offline
# ISIS benchmark (inst/benchmarks/fao_isis.R). Synthetic labels only, built
# from the names of the rule files; no third-party label is shipped.

.m392 <- function(x, ed, ...) soilKey:::.fao_match_label(x, ed, ...)
.sing392 <- function(x) sub("s$", "", x)
# the benchmark script: installed (R CMD check), else the source tree
.script392 <- function() {
  f <- system.file("benchmarks", "fao_isis.R", package = "soilKey")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "benchmarks", "fao_isis.R")
  skip_if_not(file.exists(f))
  e <- new.env()
  sys.source(f, envir = e)
  attr(e, "file") <- f
  e
}

test_that("the edition is read from a code or a publication year", {
  skip_on_cran()
  expect_identical(soilKey:::.fao_label_edition(c("1974", "1997", "1988", "1990", "74", "2006", NA)),
                   c("1974", "1988", "1988", "1988", "1974", NA, NA))
  m <- .m392("Haplic Luvisol", 2006)
  expect_true(is.na(m$group_code))
})

test_that("every unit name of both editions is read back in the singular and the plural", {
  skip_on_cran()
  for (ed in c("1974", "1988")) {
    tab <- soilKey:::.fao_label_tables(ed)
    u <- tab$units
    sing <- vapply(strsplit(u$name, " ", fixed = TRUE), function(w) {
      w[length(w)] <- .sing392(w[length(w)]); paste(w, collapse = " ")
    }, "")
    m <- .m392(sing, ed)
    expect_identical(m$unit_code, u$code, info = ed)
    expect_identical(m$group_code, u$group, info = ed)
    expect_identical(.m392(u$name, ed)$unit_code, u$code, info = ed)
    expect_true(all(m$matched_by == "name"))
  }
  # the single-unit groups of 1974 are their own unit
  m <- .m392(c("Lithosol", "Ranker", "Rendzinas"), "1974")
  expect_identical(m$unit_code, c("I", "U", "E"))
  expect_true(all(grepl("single_unit_group", m$flags)))
  # a 1988 grouping named alone has no unit
  m <- .m392("Leptosol", "1988")
  expect_identical(m$group_code, "LP")
  expect_true(is.na(m$unit_code))
  expect_identical(.m392("Solonetz", "1988")$group_code, "SN")
})

test_that("the subunit prefix is cut off and reported, never scored", {
  skip_on_cran()
  m <- .m392(c("Ferri-Haplic Acrisol", "Pelli-Eutric Vertisol", "Haplic Acrisol"), "1997")
  expect_identical(m$subunit, c("Ferri", "Pelli", NA))
  expect_identical(m$unit_code, c("ACh", "VRe", "ACh"))
  m <- .m392("Humi-Dystri-Gleyic Cambisol", "1988")
  expect_identical(m$subunit, "Humi-Dystri")
  expect_identical(m$unit_code, "CMg")
})

test_that("Nitosol and Nitisol are read in either edition, flagged", {
  skip_on_cran()
  m <- .m392(c("Eutric Nitosol", "Haplic Nitisol"), c("1974", "1988"))
  expect_identical(m$unit_code, c("Ne", "NTh"))
  expect_identical(m$flags, c("", ""))
  m <- .m392(c("Humic Nitisol", "Rhodic Nitosol"), c("1974", "1997"))
  expect_identical(m$unit_code, c("Nh", "NTr"))
  expect_true(all(m$flags == "nitosol_nitisol"))
})

test_that("phases are read per edition, other words reported apart", {
  skip_on_cran()
  m <- .m392("Chromic Luvisol petric stony", "1974")
  expect_identical(m$phases, "petric, stony")
  expect_identical(m$phases_unread, "")
  m <- .m392("Haplic Luvisol skeletic petric", "1988")
  expect_identical(m$phases, "skeletic")
  expect_identical(m$phases_unread, "petric")
  m <- .m392("Dystric Cambisol lithic", "1997")
  expect_identical(m$phases, "lithic")
  for (ed in c("1974", "1988")) {
    ids <- soilKey:::.fao_label_tables(ed)$phases
    m <- .m392(paste("Haplic Luvisol", ids), ed)
    expect_identical(m$phases, ids, info = ed)
  }
})

test_that("exact names only; a variant list is tried only on an unread label", {
  skip_on_cran()
  m <- .m392(c("Orthic Luvisol", "Haplik Luvisol", "Luvisoil", NA, ""), "1974")
  expect_identical(m$unit_code, c("Lo", NA, NA, NA, NA))
  expect_identical(m$group_code, c("L", "L", NA, NA, NA))
  # Orthic is not a 1988 unit name
  expect_true(is.na(.m392("Orthic Luvisol", "1988")$unit_code))
  v <- c("Haplik" = "Haplic")
  m <- .m392(c("Haplik Luvisol", "Haplic Luvisol"), "1988", variants = v)
  expect_identical(m$unit_code, c("LVh", "LVh"))
  expect_identical(m$matched_by, c("variant", "name"))
})

test_that("the benchmark script parses and keeps the third-party data out", {
  skip_on_cran()
  env <- .script392()
  f <- attr(env, "file")
  expect_true(is.function(env$fao_isis_benchmark))
  expect_length(env$.fao_isis_variants, 0L)
  expect_equal(env$.fao_isis_wilson(0, 0), c(NA_real_, NA_real_))
  ci <- env$.fao_isis_wilson(50, 100)
  expect_true(ci[1] < 0.5 && ci[2] > 0.5)
  # no cache: a message and NULL, never a download
  old <- Sys.getenv("SOILKEY_ISIS_CACHE")
  on.exit(Sys.setenv(SOILKEY_ISIS_CACHE = old), add = TRUE)
  Sys.setenv(SOILKEY_ISIS_CACHE = "")
  op <- options(soilKey.isis_cache = NULL)
  on.exit(options(op), add = TRUE)
  nofile <- file.path(tempdir(), "no-such-isis-cache.rds")
  if (!file.exists(file.path(tools::R_user_dir("soilKey", "cache"), "isis", "profiles.rds"))) {
    expect_message(expect_null(env$fao_isis_cache(nofile, must = FALSE)), "downloads nothing")
    expect_error(env$fao_isis_cache(nofile), "downloads nothing")
  }
  txt <- readLines(f)
  expect_false(any(grepl("download\\.file|httr|curl::|url\\(", txt)))
})

test_that("the stand-in finder and the result reader of the script", {
  skip_on_cran()
  env <- .script392()
  expect_false(env$.fao_isis_stand_in(list(list(evidence = list(stand_in = character(0))))))
  expect_true(env$.fao_isis_stand_in(list(list(evidence = list(stand_in = "argillic_usda")))))
  r <- classify_fao(make_fao_example("LVh"), on_missing = "silent")
  s <- env$.fao_isis_result(r)
  expect_identical(c(s$sk_group, s$sk_unit, s$fall_through), c("LV", "LVh", "no"))
  r <- classify_fao(make_fao_example("I", edition = "1974"), edition = "1974", on_missing = "silent")
  expect_identical(env$.fao_isis_result(r)$sk_unit, "I")
  expect_true(is.na(env$.fao_isis_result(NULL)$sk_group))
})

# A small synthetic ISIS-like cache (no third-party data): three profiles
# with two horizons each and soilKey-made labels, and one without layers.
.cache392 <- function() {
  lay <- function(top, bot, nm, clay, ...) c(list(upperDepth = top, lowerDepth = bot, layerName = nm,
                                                 clay = list(as.character(clay))), list(...))
  prof <- function(code, labels, layers = list(lay(0L, 20L, "A", 20), lay(20L, 100L, "B", 30))) list(
    profileCode = code, countryCode = "XX", profileId = 1L,
    layers = layers,
    faoClassifications = lapply(labels, function(l) list(publicationYear = l[1], faoClassification = l[2])))
  list(prof("XX001", list(c("1974", "Orthic Luvisol"), c("1997", "Ferri-Haplic Luvisol"))),
       prof("XX002", list(c("1974", "Eutric Cambisol petric"), c("1997", "Eutric Cambisol skeletic"))),
       prof("XX003", list(c("1997", "Haplic Nitisol"))),
       prof("XX004", list(c("1974", "Dystric Regosol")), layers = list()))
}

test_that("the benchmark runs end to end on a synthetic cache", {
  skip_on_cran()
  env <- .script392()
  f <- tempfile(fileext = ".rds")
  saveRDS(.cache392(), f)
  out <- tempfile(fileext = ".md")
  b <- env$fao_isis_benchmark(cache = f, out = out, verbose = FALSE)
  expect_identical(b$n_profiles, 4L)
  expect_identical(nrow(b$records), 6L)
  expect_identical(b$status$edition, c("1974", "1988"))
  expect_identical(b$status$labels, c(3L, 3L))
  expect_identical(b$status$read_unit, c(3L, 3L))
  expect_identical(b$status$with_subunit, c(0L, 1L))
  expect_identical(b$status$error, c(1L, 0L))
  sc <- b$score
  expect_identical(unique(sc$level), c("grouping", "unit", "unit | grouping agrees"))
  expect_identical(sc$n[sc$level == "grouping"], c(2L, 3L))
  expect_true(all(sc$agree <= sc$n & sc$n_firm <= sc$n))
  # firm, provisional and stand-in count only labels soilKey classifies
  for (e in c("1974", "1988")) {
    x <- b$records[b$records$edition == e & !b$records$error, ]
    g <- sc[sc$edition == e & sc$level == "grouping", ]
    expect_identical(g$fall_through, sum(x$fall_through != "no"))
    expect_true(g$n_firm + g$provisional <= g$n - g$fall_through + g$stand_in)
    expect_identical(g$provisional, sum(x$fall_through == "no" & x$prov_group))
  }
  expect_true(all(c("petric", "skeletic") %in% b$phases$phase))
  md <- readLines(out)
  for (h in c("^## Labels read", "^## Agreement", "^## Phases", "^## Grouping confusion, 1974"))
    expect_true(any(grepl(h, md)), info = h)
  expect_identical(env$.fao_isis_report(b), md)
})
