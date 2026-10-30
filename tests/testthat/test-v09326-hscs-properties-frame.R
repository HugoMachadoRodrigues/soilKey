# v0.9.316: the frame of the HSCS 2024 subtype and variety properties
# (Micheli et al. 2024, 8.5-8.7): the tables, the name and the modifiers.

test_that("every type of the key has its table, and every listed property is in the catalogue", {
  skip_on_cran()
  pr <- soilKey:::.hscs_properties_rules()
  key <- yaml::read_yaml(system.file("rules", "hscs2024", "key.yaml", package = "soilKey"))
  codes <- vapply(key$types %||% key[[1]], function(t) t$code %||% NA_character_, "")
  codes <- codes[!is.na(codes)]
  expect_setequal(names(pr$types), codes)
  ids <- unique(unlist(lapply(pr$types, function(t) unlist(c(t$subtypes, t$varieties)))))
  expect_true(all(ids %in% names(pr$properties)))
  expect_true(all(vapply(pr$properties, function(p) p$kind %in% c("layer", "surface", "soil", "site"), NA)))
})

test_that("the name follows p. 7", {
  skip_on_cran()
  nm <- soilKey:::.hscs_full_name("Agyagbemos\u00f3d\u00e1sos talaj", c("savany\u00fa", "humuszszeg\u00e9ny"),
                                  "sek\u00e9lyen k\u0151zett\u00f6rmel\u00e9kes")
  expect_identical(nm, "Savany\u00fa, humuszszeg\u00e9ny Agyagbemos\u00f3d\u00e1sos talaj (sek\u00e9lyen k\u0151zett\u00f6rmel\u00e9kes)")
  expect_identical(soilKey:::.hscs_full_name("Barnaf\u00f6ld", character(0), character(0)), "Barnaf\u00f6ld")
})

test_that("the depth modifiers of 8.7", {
  skip_on_cran()
  x <- list(surface = 0)
  w <- function(t) soilKey:::.hscs_depth_word(x, t)
  expect_identical(w(0), "felsz\u00ednt\u0151l")
  expect_identical(w(20), "sek\u00e9lyen")
  expect_identical(w(50), "k\u00f6z\u00e9pm\u00e9lyen")
  expect_identical(w(100), "m\u00e9lyben")
  expect_true(is.na(w(NA)))
})

test_that("a property soilKey does not read is undecided and leaves the name as it is", {
  skip_on_cran()
  p <- make_hscs2024_example("clay_illuvial")
  st <- soilKey:::.hscs_type_properties(p, "AGY")$status
  expect_true(all(c("level", "row", "id", "passed", "top", "label", "named") %in% names(st)))
  expect_true(all(st$level %in% c("subtype", "variety")))
})
