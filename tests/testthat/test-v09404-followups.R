# v0.9.404 -- follow-ups of the 0.9.403 release.

test_that("a word that only begins with ru does not declare the Russian notation", {
  skip_on_cran()
  for (ds in c("Russian 2008", "RU-2008", "ru", "rus", "RUS 2004", "Russia",
               "kidpr 2008", "rfg2008", "rsc2004"))
    expect_true(soilKey:::.kidpr_is_declared(ds), info = ds)
  for (ds in c("rural", "Rubefied", "run 1", "ruler", "Rusty", "rp", "fao"))
    expect_false(soilKey:::.kidpr_is_declared(ds), info = ds)
})

test_that("the tests never query ISRIC: .wosis_post is the offline stand-in", {
  skip_on_cran()
  skip_if(isTRUE(as.logical(Sys.getenv("SOILKEY_TEST_WOSIS_LIVE", "false"))))
  r <- soilKey:::.wosis_post("{ x }")
  expect_null(r$data)
  expect_match(r$error, "not queried in the tests", fixed = TRUE)
  p <- read_wosis_profiles_graphql(country = "Brazil", n_max = 1)
  expect_s3_class(p, "data.frame")
  expect_equal(nrow(p), 0L)
})
