# v0.9.30 bundled WoSIS South-America sample -- REMOVED in v0.9.199.
#
# soilKey ships the code that reads soil databases, never the observations
# (see inst/DATA-PROVENANCE.md). These tests therefore no longer assert that a
# snapshot exists. They assert the two things that still matter:
#
#   * when no snapshot is present, the loader explains where the data comes
#     from and how to obtain it, rather than failing with a missing-file error;
#   * when a user HAS obtained a snapshot and placed it in a development
#     checkout, the loader still returns the documented shape.

.wosis_sample_present <- function() {
  p <- system.file("extdata", "wosis_sa_sample.rds", package = "soilKey")
  if (nzchar(p) && file.exists(p)) return(TRUE)
  file.exists(file.path("inst", "extdata", "wosis_sa_sample.rds"))
}


test_that("without a snapshot the loader names the provider, not a file path", {
  skip_if(.wosis_sample_present(),
          "a snapshot is present in this checkout -- see the shape test below")
  err <- tryCatch(load_wosis_sample(), error = conditionMessage)
  expect_true(is.character(err))
  # The message has to be actionable: who owns the data and how to get it.
  expect_match(err, "does not distribute|No WoSIS sample is bundled")
  expect_match(err, "read_wosis_profiles_graphql", fixed = TRUE)
})

test_that("a user-supplied snapshot still loads with the documented shape", {
  skip_if_not(.wosis_sample_present(), "no WoSIS snapshot in this checkout")
  s <- load_wosis_sample()
  expect_named(s, c("profiles_raw", "pedons", "pulled_on",
                    "endpoint", "filter", "n_pulled"))
  expect_length(s$pedons, s$n_pulled)
  expect_true(all(vapply(s$pedons, inherits, logical(1), "PedonRecord")))
})

test_that("a user-supplied snapshot classifies offline", {
  skip_if_not(.wosis_sample_present(), "no WoSIS snapshot in this checkout")
  s <- load_wosis_sample()
  res <- classify_wrb2022(s$pedons[[1]], on_missing = "silent")
  expect_s3_class(res, "ClassificationResult")
  expect_true(nzchar(res$name %||% ""))
})

test_that("the package itself carries no WoSIS observations", {
  # The point of v0.9.199: nothing ISRIC owns is inside an installed soilKey.
  p <- system.file("extdata", "wosis_sa_sample.rds", package = "soilKey")
  expect_false(nzchar(p) && file.exists(p))
})
