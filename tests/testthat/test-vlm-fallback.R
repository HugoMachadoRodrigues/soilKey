# Tests for the v0.9.15 VLM fallback chain:
#   - ollama_is_running() probes a localhost URL with a short timeout
#   - vlm_pick_provider() prefers Ollama, then API-keyed cloud providers
#   - provider = "auto" cascades correctly in classify_from_documents()


test_that("ollama_is_running() returns FALSE when probing a guaranteed-dead URL", {
  expect_false(ollama_is_running(url = "http://127.0.0.1:1/api/tags",
                                    timeout_s = 0.3))
})


test_that("vlm_pick_provider() respects an env-key cascade when Ollama is down", {
  withr::local_options(soilKey.ollama_url = "http://127.0.0.1:1/api/tags")
  withr::local_envvar(c(ANTHROPIC_API_KEY = "sk-test-anthropic",
                          OPENAI_API_KEY    = "",
                          GOOGLE_API_KEY    = "",
                          GEMINI_API_KEY    = ""))
  expect_equal(vlm_pick_provider(verbose = FALSE), "anthropic")

  withr::local_envvar(c(ANTHROPIC_API_KEY = "",
                          OPENAI_API_KEY    = "sk-test-openai"))
  expect_equal(vlm_pick_provider(verbose = FALSE), "openai")

  withr::local_envvar(c(ANTHROPIC_API_KEY = "",
                          OPENAI_API_KEY    = "",
                          GOOGLE_API_KEY    = "test-google"))
  expect_equal(vlm_pick_provider(verbose = FALSE), "google")
})


test_that("vlm_pick_provider() errors with actionable hints when nothing is reachable", {
  withr::local_options(soilKey.ollama_url = "http://127.0.0.1:1/api/tags")
  withr::local_envvar(c(ANTHROPIC_API_KEY = "",
                          OPENAI_API_KEY    = "",
                          GOOGLE_API_KEY    = "",
                          GEMINI_API_KEY    = ""))
  expect_error(vlm_pick_provider(verbose = FALSE),
                 "No VLM provider is reachable")
})


test_that("default_model('auto') resolves via the picker", {
  skip_if_not_installed("withr")
  withr::local_options(soilKey.ollama_url = "http://127.0.0.1:1/api/tags")
  withr::local_envvar(c(ANTHROPIC_API_KEY = "sk-test",
                          OPENAI_API_KEY    = "",
                          GOOGLE_API_KEY    = "",
                          GEMINI_API_KEY    = ""))
  # Without an OPENAI / GOOGLE key, default_model('auto') should
  # resolve to the Anthropic default.
  expect_equal(default_model("auto"), "claude-sonnet-4-7")
})
