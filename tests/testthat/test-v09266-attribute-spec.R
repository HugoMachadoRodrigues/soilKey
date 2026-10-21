# v0.9.266: the metadata of the horizon attributes live in
# inst/schemas/horizon-attributes.yaml; horizon_attribute_spec() joins them
# with horizon_column_spec() and the aliases, and inst/ATTRIBUTES.md is
# generated from it.

test_that("horizon_attribute_spec has one documented row per column", {
  a <- horizon_attribute_spec()
  expect_identical(a$name, names(horizon_column_spec()))
  expect_identical(a$type, unlist(horizon_column_spec(), use.names = FALSE))
  expect_named(a, c("name", "type", "section", "unit", "description",
                    "aliases", "systems", "allowed", "min", "max"))
  expect_true(all(nzchar(a$section) & nzchar(a$unit) & nzchar(a$description)))
  expect_identical(a$unit[a$name == "cec_cmol"], "cmol(+)/kg")
  expect_match(a$aliases[a$name == "clay_pct"], "clay")
})

test_that("every alias points to a documented column", {
  al <- soilKey:::.horizon_name_aliases()
  expect_true(all(unname(al) %in% horizon_attribute_spec()$name))
})

test_that("inst/ATTRIBUTES.md is up to date with the YAML", {
  path <- system.file("ATTRIBUTES.md", package = "soilKey")
  skip_if(!nzchar(path), "ATTRIBUTES.md not installed")
  drop_version <- function(x) x[!grepl("v[0-9]+\\.[0-9]+\\.[0-9]+", x)]
  expect_identical(drop_version(readLines(path, encoding = "UTF-8")),
                   drop_version(soilKey:::.attributes_markdown()),
                   info = "run Rscript data-raw/generate_attributes_doc.R")
})
