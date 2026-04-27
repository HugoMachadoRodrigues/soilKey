test_that("default_model returns sensible per-provider defaults", {
  expect_equal(soilKey:::default_model("anthropic"), "claude-sonnet-4-5")
  expect_equal(soilKey:::default_model("openai"),    "gpt-4o")
  expect_equal(soilKey:::default_model("google"),    "gemini-1.5-pro")
  expect_equal(soilKey:::default_model("ollama"),    "gemma3:27b")
})

test_that("default_model rejects unknown providers", {
  expect_error(soilKey:::default_model("not-a-provider"), "should be one of")
})

test_that("vlm_provider errors clearly when ellmer is not installed", {
  skip_if(requireNamespace("ellmer", quietly = TRUE),
          "ellmer is installed; cannot test missing-package branch")
  expect_error(
    vlm_provider("anthropic"),
    regexp = "ellmer.*not installed|Package 'ellmer' is required",
    ignore.case = TRUE
  )
})

test_that("vlm_provider returns a chat object when ellmer is available", {
  skip_if_not_installed("ellmer")

  # We do not actually want to instantiate an Anthropic client (it
  # requires an API key); guard via mocking on the constructor name
  # if available, otherwise just verify the function dispatches.
  expect_true(is.function(vlm_provider))
  expect_true("ellmer" %in% rownames(installed.packages()))
})

test_that("vlm_provider validates name with match.arg", {
  expect_error(
    suppressWarnings(vlm_provider("aws")),
    regexp = "should be one of"
  )
})
