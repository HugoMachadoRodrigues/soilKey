# v0.9.204: soilKey ships no third-party data, and these tests keep it so.
#
# Two reference datasets used to be vendored and are GPL-3, which soilKey's MIT
# licence cannot carry: the SoilTaxonomy objects are now read from the installed
# package, and the SoilKnowledgeBase JSON files are downloaded on first use,
# pinned to one commit and checked by MD5. The benchmark reports, computed from
# third-party databases, are no longer part of the package either.

.local_cache <- function(env = parent.frame()) {
  d <- file.path(tempfile("sk-cache-"))
  dir.create(d)
  old <- Sys.getenv("R_USER_CACHE_DIR", unset = NA)
  Sys.setenv(R_USER_CACHE_DIR = d)
  withr_restore <- function() {
    if (is.na(old)) Sys.unsetenv("R_USER_CACHE_DIR") else Sys.setenv(R_USER_CACHE_DIR = old)
    unlink(d, recursive = TRUE)
  }
  do.call(on.exit, list(substitute(withr_restore()), add = TRUE), envir = env)
  soilKey::clear_kst13_cache()
  d
}


test_that("none of the formerly vendored files is shipped", {
  for (f in c("WRB_4th_2022.rda", "ST_criteria_13th.rda", "ST_features.rda"))
    expect_identical(system.file("extdata", "canonical", f, package = "soilKey"), "")
  for (f in c("2022_KST_codes.json", "2022_KST_criteria_EN.json"))
    expect_identical(system.file("rules", "usda", "canonical", f, package = "soilKey"), "")
})

test_that("no benchmark report travels with the package", {
  d <- system.file("benchmarks", "reports", package = "soilKey")
  if (nzchar(d)) {
    shipped <- setdiff(list.files(d, all.files = TRUE, no.. = TRUE), ".gitkeep")
    expect_length(shipped, 0L)
  } else succeed("no reports directory installed")
})

test_that("without SoilTaxonomy the loader explains itself, naming the licence", {
  local_mocked_bindings(.has_soiltaxonomy = function() FALSE)
  err <- tryCatch(canonical_reference("WRB_4th_2022"), error = conditionMessage)
  expect_match(err, "SoilTaxonomy", fixed = TRUE)
  expect_match(err, "GPL-3", fixed = TRUE)
  expect_match(err, "install.packages", fixed = TRUE)
})

test_that("with SoilTaxonomy the data have the documented shape", {
  skip_if_no_soiltaxonomy()
  w <- canonical_reference("WRB_4th_2022")
  expect_named(w, c("rsg", "pq", "sq"))
  expect_equal(c(nrow(w$rsg), nrow(w$pq), nrow(w$sq)), c(118L, 661L, 1167L))
  expect_equal(nrow(st_features_canonical()), 84L)
})

test_that("the KST pins are two well-formed MD5 digests", {
  pins <- soilKey:::.KST13_MD5
  expect_setequal(names(pins), c("2022_KST_codes.json", "2022_KST_criteria_EN.json"))
  expect_true(all(grepl("^[0-9a-f]{32}$", pins)))
  expect_match(soilKey:::.KST13_SOURCE_URL, soilKey:::.KST13_SOURCE_COMMIT, fixed = TRUE)
})

test_that("offline with an empty cache, the KST loader says where the file comes from", {
  .local_cache()
  local_mocked_bindings(.kst13_download = function(url, dest) stop("offline"))
  err <- tryCatch(soilKey:::.kst13_path("2022_KST_codes.json"), error = conditionMessage)
  expect_match(err, "SoilKnowledgeBase", fixed = TRUE)
  expect_match(err, "GPL-3", fixed = TRUE)
  expect_match(err, "soilKey.kst13_dir", fixed = TRUE)
})

test_that("a corrupted cached file is refused rather than used", {
  d <- .local_cache()
  cache <- file.path(tools::R_user_dir("soilKey", "cache"), "kst13")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  writeLines("{\"not\": \"the real file\"}", file.path(cache, "2022_KST_codes.json"))
  local_mocked_bindings(.kst13_download = function(url, dest) stop("offline"))
  expect_error(soilKey:::.kst13_path("2022_KST_codes.json"), "Could not obtain")
})

test_that("a verified local copy is used with no network at all", {
  skip_if_no_kst13()                       # needs one real copy to start from
  good <- soilKey:::.kst13_path("2022_KST_codes.json")
  local_dir <- tempfile("kst-local-"); dir.create(local_dir)
  file.copy(good, file.path(local_dir, "2022_KST_codes.json"))
  .local_cache()
  withr_opt <- options(soilKey.kst13_dir = local_dir); on.exit(options(withr_opt), add = TRUE)
  local_mocked_bindings(.kst13_download = function(url, dest) stop("must not be called"))
  expect_identical(soilKey:::.kst13_path("2022_KST_codes.json"),
                   file.path(local_dir, "2022_KST_codes.json"))
})

test_that("the downloaded KST table is the one soilKey was validated against", {
  skip_if_no_kst13()
  codes <- kst13_codes()
  expect_equal(nrow(codes), 3153L)
  expect_true(all(c("code", "name") %in% names(codes)))
})

test_that("the app footer states the licence the package actually has", {
  y <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(y)) y <- file.path("inst", "i18n", "translations.yaml")
  lines <- grep("app.footer_license", readLines(y, warn = FALSE), value = TRUE)
  expect_length(lines, 2L)                 # en and pt
  expect_true(all(grepl("MIT", lines, fixed = TRUE)))
  expect_false(any(grepl("GPL", lines, fixed = TRUE)))
})
