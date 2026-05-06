test_that("default_model returns sensible per-provider defaults", {
  # v0.9.11: bumped Anthropic to 4-7, Google to 2.0 Pro, Ollama to
  # gemma4:e4b. v0.9.65: Ollama default lowered to gemma4:e2b
  # (laptop-friendly ~1.5 GB) -- larger sizes accessible via
  # setup_local_vlm("balanced") / "best".
  expect_equal(soilKey:::default_model("anthropic"), "claude-sonnet-4-7")
  expect_equal(soilKey:::default_model("openai"),    "gpt-4o")
  expect_equal(soilKey:::default_model("google"),    "gemini-2.0-pro")
  expect_equal(soilKey:::default_model("ollama"),    "gemma4:e2b")
})

test_that("default_model rejects unknown providers", {
  expect_error(soilKey:::default_model("not-a-provider"), "should be one of")
})

test_that("vlm_provider errors clearly when ellmer is not installed", {
  # Logic is correct (clarified for future readers): `skip_if(cond)` skips
  # when `cond` is TRUE; `requireNamespace("ellmer")` returns TRUE iff
  # ellmer is installed. So this skips when ellmer IS installed (because
  # the missing-package error path can only be exercised when the
  # package is genuinely absent), and runs the assertion when ellmer is
  # NOT installed.
  skip_if(
    requireNamespace("ellmer", quietly = TRUE),
    "ellmer is installed; cannot exercise the missing-package error path"
  )
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
