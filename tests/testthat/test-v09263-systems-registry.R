# v0.9.263: the system registry. Every classification system is a folder
# inst/rules/<id>/ with a system.yaml entry; load_rules(), .validate_rules()
# and classify_all() read the registry instead of repeating the three ids.

test_that("the registry lists the three systems in run order", {
  s <- soilkey_systems()
  expect_s3_class(s, "data.frame")
  expect_identical(s$id, c("wrb2022", "sibcs5", "usda"))
  expect_identical(s$classifier,
                   c("classify_wrb2022", "classify_sibcs", "classify_usda"))
  expect_true(all(s$default))
  expect_true(all(nzchar(s$name) & nzchar(s$edition) & nzchar(s$levels)))
  expect_identical(names(s), c("id", "name", "edition", "status",
                               "classifier", "levels", "aliases", "default"))
})

test_that("every registered system has a classifier and a key file", {
  for (id in soilkey_systems()$id) {
    sys <- soilKey:::.system_get(id)
    expect_true(exists(sys$classifier, envir = asNamespace("soilKey"),
                       mode = "function", inherits = FALSE), info = id)
    expect_true(nzchar(system.file("rules", id, "key.yaml",
                                   package = "soilKey")), info = id)
    expect_identical(sys$top_level, sys$levels[1L], info = id)
  }
})

test_that("ids and aliases resolve exactly; legacy prefixes still work", {
  res <- soilKey:::.system_resolve
  expect_identical(res("wrb2022"), "wrb2022")
  expect_identical(res("wrb"), "wrb2022")
  expect_identical(res("sibcs"), "sibcs5")
  expect_identical(res("sibcs5"), "sibcs5")
  expect_identical(res("usda"), "usda")
  # the unique partial matches match.arg() used to accept
  expect_identical(res("w"), "wrb2022")
  expect_identical(res("sib"), "sibcs5")
  expect_identical(res("us"), "usda")
  expect_error(res("bogus"), "should be one of")
  expect_error(res(c("wrb", "usda")), "length 1")
  # several.ok drops unknown names as long as one resolves, as before
  expect_identical(res(c("wrb", "bogus", "usda"), several.ok = TRUE),
                   c("wrb2022", "usda"))
  expect_error(res(c("bogus", "nope"), several.ok = TRUE), "should be one of")
  # match.arg() also gave the first choice for NULL, and the first element
  # when handed load_rules()'s old default vector
  expect_identical(res(NULL), "wrb2022")
  expect_identical(res(NULL, several.ok = TRUE), "wrb2022")
  expect_identical(res(c("wrb2022", "usda", "sibcs5")), "wrb2022")
})

test_that("a malformed system.yaml is refused with its path", {
  dir <- withr::local_tempdir()
  sysdir <- file.path(dir, "rules", "toy")
  dir.create(sysdir, recursive = TRUE)
  good <- list(id = "toy", order = 9, classifier = "classify_toy",
               top_level = "orders", levels = list("orders"),
               display = list(en = "Toy"), edition = "1st",
               citation = "Toy.", status = "experimental")
  yaml::write_yaml(good, file.path(sysdir, "system.yaml"))
  expect_identical(names(soilKey:::.read_system_registry(root = file.path(dir, "rules"))), "toy")
  bad <- good; bad$edition <- 2
  yaml::write_yaml(bad, file.path(sysdir, "system.yaml"))
  expect_error(soilKey:::.read_system_registry(root = file.path(dir, "rules")), "'edition' must be")
  bad <- good; bad$display <- list(pt = "Brinquedo")
  yaml::write_yaml(bad, file.path(sysdir, "system.yaml"))
  expect_error(soilKey:::.read_system_registry(root = file.path(dir, "rules")), "display: en")
  bad <- good; bad$levels <- list("groups")
  yaml::write_yaml(bad, file.path(sysdir, "system.yaml"))
  expect_error(soilKey:::.read_system_registry(root = file.path(dir, "rules")), "must start with")
})

test_that("load_rules reads the same rules through an id or an alias", {
  skip_on_cran()
  expect_identical(load_rules("sibcs"), load_rules("sibcs5"))
  expect_identical(load_rules(), load_rules("wrb2022"))
  expect_identical(load_rules(NULL), load_rules("wrb2022"))
  expect_identical(load_rules(c("wrb2022", "usda", "sibcs5")),
                   load_rules("wrb2022"))
  expect_error(load_rules("bogus"), "should be one of")
})

test_that("classify_all keeps its shape through the registry", {
  skip_on_cran()
  pr <- make_ferralsol_canonical()
  res <- classify_all(pr, systems = c("sibcs5", "wrb"), on_missing = "silent")
  expect_named(res, c("wrb", "sibcs", "usda", "summary"))
  expect_null(res$usda)
  expect_s3_class(res$sibcs, "ClassificationResult")
  expect_named(res$summary, c("wrb", "sibcs", "usda"))
  expect_true(is.na(res$summary$usda))
})
