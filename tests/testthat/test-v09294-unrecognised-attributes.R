# v0.9.294: horizon columns and site properties that no key reads are named
# in a warning (PedonRecord$new() from outside the package, read_pedon_csv()),
# listed by unrecognised_attributes(), and documented: site_attribute_spec()
# and the site part of inst/ATTRIBUTES.md.

# Run `expr` as user code would: from an environment that is not soilKey's.
.as_user <- function(expr) {
  eval(substitute(expr), new.env(parent = globalenv()))
}

test_that("site_attribute_spec documents each site property once", {
  skip_on_cran()
  s <- site_attribute_spec()
  expect_named(s, c("name", "type", "section", "unit", "description",
                    "allowed", "systems"))
  expect_false(anyDuplicated(s$name) > 0L)
  expect_true(all(s$type %in% c("numeric", "character", "logical", "list")))
  expect_true(all(nzchar(s$section) & nzchar(s$unit) & nzchar(s$description)))
  expect_true(all(c("id", "lat", "lon", "country", "parent_material",
                    "drainage_class", "soil_moisture_regime",
                    "soil_temperature_regime", "designation_system",
                    "technic", "geomembrane", "reference_wrb") %in% s$name))
  sys <- unlist(strsplit(s$systems[nzchar(s$systems)], ", ", fixed = TRUE))
  expect_true(all(sys %in% soilkey_systems()$id))
})

test_that("every site property the code reads is documented", {
  skip_on_cran()
  src <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(src), "package sources not available")
  code <- unlist(lapply(list.files(src, pattern = "\\.R$", full.names = TRUE),
                        readLines, warn = FALSE))
  code <- code[!grepl("^\\s*#", code)]   # code, not comments
  used <- unique(c(
    sub("^site\\$", "", unlist(regmatches(code,
      gregexpr("site\\$[A-Za-z][A-Za-z0-9_]*", code)))),
    gsub("^site\\[\\[\"|\"\\]\\]$", "", unlist(regmatches(code,
      gregexpr("site\\[\\[\"[A-Za-z][A-Za-z0-9_]*\"\\]\\]", code))))))
  # `reference_` is the prefix of a pasted name (reference_<system>).
  used <- setdiff(used, "reference_")
  expect_identical(setdiff(used, site_attribute_spec()$name), character(0))
})

test_that("PedonRecord$new() from user code warns, with suggestions", {
  skip_on_cran()
  w <- NULL
  p <- withCallingHandlers(
    .as_user(soilKey::PedonRecord$new(
      site = list(id = "p1", moisture_regime = "udic"),
      horizons = data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
                            Clay = c(30, 45), organic_carbon_gkg = c(11, 4),
                            soil_moisture_regime = "udic", zzqx = 1))),
    soilKey_unrecognised_attributes = function(cnd) {
      w <<- conditionMessage(cnd)
      invokeRestart("muffleWarning")
    })
  expect_false(is.null(w))
  expect_match(w, "Pedon 'p1'", fixed = TRUE)
  expect_match(w, "organic_carbon_gkg (did you mean oc_pct?)", fixed = TRUE)
  expect_match(w, "soil_moisture_regime (a site property: site$soil_moisture_regime)",
               fixed = TRUE)
  expect_match(w, "zzqx", fixed = TRUE)
  expect_match(w, "moisture_regime (did you mean soil_moisture_regime", fixed = TRUE)
  expect_match(w, "options(soilKey.warn_unrecognised = FALSE)", fixed = TRUE)
  # the alias is renamed, not reported, and the pedon is built as before
  expect_false(grepl("Clay", w, fixed = TRUE))
  expect_identical(p$horizons$clay_pct, c(30, 45))
  expect_true("organic_carbon_gkg" %in% names(p$horizons))
})

test_that("recognised names, aliases and case variants do not warn", {
  skip_on_cran()
  expect_no_warning(.as_user(soilKey::PedonRecord$new(
    site = list(id = "p2", lat = -3.1, lon = 39.8, country = "KE",
                soil_moisture_regime = "ustic", drainage_class = "well drained"),
    horizons = data.frame(Top_cm = 0, BOTTOM_CM = 10, SOC = 1.1,
                          pH.H2O = 6.5, designation = "Ah"))))
})

test_that("soilKey's own constructions and the option stay quiet", {
  skip_on_cran()
  expect_no_warning(make_ferralsol_canonical())
  inside <- function() PedonRecord$new(site = list(id = "x", foo = 1),
                                       horizons = data.frame(top_cm = 0,
                                                             bottom_cm = 10,
                                                             loader_col = 1))
  environment(inside) <- topenv(environment(site_attribute_spec))
  expect_no_warning(inside())
  op <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(op), add = TRUE)
  expect_no_warning(.as_user(soilKey::PedonRecord$new(
    site = list(id = "x", foo = 1),
    horizons = data.frame(top_cm = 0, bottom_cm = 10, bar = 1))))
})

test_that("unrecognised_attributes() lists them as a table", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                  clay_pct = c(30, 45), clay = c(31, 46),
                  organic_carbon_gkg = c(11, 4), soil_moisture_regime = "ustic",
                  zzqx = 1)
  tab <- unrecognised_attributes(h, site = list(id = "p", drainge_class = "well",
                                                clay_pct = 3, qqzz = 2))
  expect_named(tab, c("where", "name", "suggestion", "note"))
  row <- function(n) tab[tab$name == n, , drop = FALSE]
  expect_identical(row("organic_carbon_gkg")$suggestion, "oc_pct")
  expect_identical(row("clay")$note, "already given")
  expect_identical(row("clay")$suggestion, "clay_pct")
  expect_identical(row("soil_moisture_regime")$note, "site property")
  expect_identical(row("zzqx")$suggestion, "")
  expect_identical(row("drainge_class")$where, "site")
  expect_identical(row("drainge_class")$suggestion, "drainage_class")
  expect_identical(row("clay_pct")$note, "horizon attribute")
  expect_identical(row("qqzz")$suggestion, "")
  expect_identical(nrow(unrecognised_attributes(make_ferralsol_canonical())), 0L)
})

test_that("read_pedon_csv() recognises aliases and warns about the rest", {
  skip_on_cran()
  f <- tempfile(fileext = ".csv")
  on.exit(unlink(f), add = TRUE)
  utils::write.csv(data.frame(Top = c(0, 15), Bottom = c(15, 40),
                              Clay = c(20, 35), SOC = c(1.2, 0.6),
                              organic_matter_x = c(2, 1)),
                   f, row.names = FALSE)
  expect_warning(p <- read_pedon_csv(f), class = "soilKey_unrecognised_attributes")
  expect_equal(p$horizons$top_cm, c(0, 15))
  expect_equal(p$horizons$clay_pct, c(20, 35))
  expect_equal(p$horizons$oc_pct, c(1.2, 0.6))
})

test_that("the app keeps recognised session site properties and lists the rest", {
  skip_on_cran()
  dir <- system.file("shiny", "classify_app_pro", "R", package = "soilKey")
  if (!nzchar(dir)) dir <- file.path("inst", "shiny", "classify_app_pro", "R")
  skip_if_not(file.exists(file.path(dir, "mod_pedon.R")))
  env <- new.env(parent = globalenv())
  sys.source(file.path(dir, "i18n.R"), envir = env)
  cat_path <- system.file("i18n", "translations.yaml", package = "soilKey")
  if (!nzchar(cat_path)) cat_path <- file.path("inst", "i18n", "translations.yaml")
  env$.sk_i18n_env$cat <- yaml::read_yaml(cat_path)
  if (!exists("%||%", envir = env)) assign("%||%", function(a, b) if (is.null(a)) b else a, envir = env)
  sys.source(file.path(dir, "mod_pedon.R"), envir = env)

  site <- list(id = "KE009", lat = -3.1, lon = 39.8, country = "KE",
               parent_material = "gneiss", technic = "unknown",
               soil_moisture_regime = "ustic", drainage_class = NULL,
               moisture = "udic")
  extra <- env$.sk_session_site_extra(site)
  expect_identical(names(extra), "soil_moisture_regime")

  notice <- env$.sk_unrecognised_notice(
    data.frame(top_cm = 0, bottom_cm = 10, organic_carbon_gkg = 11), site)
  expect_match(notice, "organic_carbon_gkg → oc_pct", fixed = TRUE)
  expect_match(notice, "moisture", fixed = TRUE)
  expect_null(env$.sk_unrecognised_notice(data.frame(top_cm = 0, bottom_cm = 10)))
  expect_no_warning(env$.sk_new_pedon(site, data.frame(top_cm = 0, bottom_cm = 10,
                                                       foo = 1)))
})
