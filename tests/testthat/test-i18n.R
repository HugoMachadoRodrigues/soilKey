# Tests for the v0.9.114 Pro-app internationalization: the translation
# catalogue (inst/i18n/translations.yaml) and the i18n() helper
# (inst/shiny/classify_app_pro/R/i18n.R).

.i18n_catalog_path <- function() {
  p <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(p) || !file.exists(p)) p <- file.path("inst", "i18n", "translations.yaml")
  p
}

.source_i18n_helper <- function(env = new.env(parent = baseenv())) {
  f <- system.file("shiny", "classify_app_pro", "R", "i18n.R", package = "soilKey")
  if (!nzchar(f) || !file.exists(f))
    f <- file.path("inst", "shiny", "classify_app_pro", "R", "i18n.R")
  sys.source(f, envir = env)
  # Under load_all the helper's own system.file() (base, un-shimmed in this
  # sourced env) can't see inst/i18n; pre-seed its cache with the catalogue the
  # test resolves via the pkgload shim, so i18n() exercises the real lookup.
  env$.sk_i18n_env$cat <- yaml::read_yaml(.i18n_catalog_path())
  env
}

test_that("the translation catalogue ships and parses with en + pt sections", {
  p <- .i18n_catalog_path()
  expect_true(file.exists(p))
  cat <- yaml::read_yaml(p)
  expect_true(all(c("en", "pt") %in% names(cat)))
  expect_gt(length(cat$en), 300L)        # ~352 UI strings
})

test_that("every English key has a Portuguese translation (no gaps)", {
  cat <- yaml::read_yaml(.i18n_catalog_path())
  missing_pt <- setdiff(names(cat$en), names(cat$pt))
  expect_equal(missing_pt, character(0))
  extra_pt <- setdiff(names(cat$pt), names(cat$en))
  expect_equal(extra_pt, character(0))
  # no empty / NA values either side
  expect_false(any(vapply(cat$en, function(x) !nzchar(trimws(x %||% "")), logical(1))))
  expect_false(any(vapply(cat$pt, function(x) !nzchar(trimws(x %||% "")), logical(1))))
})

test_that("sprintf placeholders match between en and pt", {
  cat <- yaml::read_yaml(.i18n_catalog_path())
  ph <- function(s) {
    m <- regmatches(s, gregexpr("%[-0-9.]*[sdfg]", s))[[1]]
    sort(m)
  }
  for (k in names(cat$en)) {
    expect_identical(ph(cat$en[[k]]), ph(cat$pt[[k]]),
                     info = paste("placeholder mismatch for key", k))
  }
})

test_that("i18n() resolves, falls back, and formats", {
  env <- .source_i18n_helper()
  i18n <- get("i18n", envir = env)
  withr::with_options(list(soilKey.app_lang = "en"), {
    expect_equal(i18n("nav.classify"), "Classify")
  })
  withr::with_options(list(soilKey.app_lang = "pt"), {
    expect_equal(i18n("nav.classify"), "Classificar")
    # missing pt -> would fall back to en; here a missing key falls back to the key
    expect_equal(i18n("totally.absent.key"), "totally.absent.key")
  })
  # an unknown language clamps to en
  withr::with_options(list(soilKey.app_lang = "xx"), {
    expect_equal(i18n("nav.classify"), "Classify")
  })
  # sprintf args
  withr::with_options(list(soilKey.app_lang = "en"), {
    expect_match(i18n("pedon.loaded_n", 5L), "5")
  })
})
