# v0.9.207: the Groq model is chosen from Groq's own list, not a fixed name.
#
# Fixed names broke the Pro app twice. In August 2026 Groq retired
# meta-llama/llama-4-scout-17b-16e-instruct (reported from ISRIC); by October it
# had retired qwen/qwen3.6-27b and llama-3.3-70b-versatile, which took down the
# Photo tab (HTTP 404) and the Assistant together, while the Assistant's status
# still read "Live model connected". These tests pin the run-time choice, the
# retry on retirement and the honest status. Model lists are synthetic except in
# the live test at the end.

.groq_env <- function() {
  d <- system.file("shiny", "classify_app_pro", package = "soilKey")
  if (!nzchar(d) || !dir.exists(d)) d <- file.path("inst", "shiny", "classify_app_pro")
  e <- new.env(parent = globalenv())
  for (f in list.files(file.path(d, "R"), pattern = "\\.R$", full.names = TRUE))
    sys.source(f, envir = e)
  e
}

# What Groq listed on 2026-10-07, after the retirements.
.oct_2026 <- c("allam-2-7b", "canopylabs/orpheus-v1-english",
               "meta-llama/llama-prompt-guard-2-86m", "openai/gpt-oss-120b",
               "openai/gpt-oss-20b", "openai/gpt-oss-safeguard-20b",
               "qwen/qwen3.8-27b", "whisper-large-v3")


test_that("versions are compared as numbers, newest first", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  x <- c("qwen/qwen3.6-27b", "qwen/qwen3.10-27b", "qwen/qwen3.8-27b")
  expect_identical(e$.groq_newest_first(x)[1], "qwen/qwen3.10-27b")
})

test_that("today's list gives a vision and a text model that exist", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  expect_identical(e$.groq_pick(.oct_2026, e$.GROQ_PREFS$vision), "qwen/qwen3.8-27b")
  # Qwen for the Assistant too: it kept to the evidence where gpt-oss-120b did not
  expect_identical(e$.groq_pick(.oct_2026, e$.GROQ_PREFS$text), "qwen/qwen3.8-27b")
  # guard, speech and safety models are never chosen for chat
  expect_false(e$.groq_pick(.oct_2026, e$.GROQ_PREFS$text) %in%
                 c("meta-llama/llama-prompt-guard-2-86m", "whisper-large-v3"))
})

test_that("a routine version bump is followed without a code change", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  later <- c(setdiff(.oct_2026, "qwen/qwen3.8-27b"), "qwen/qwen3.9-27b")
  expect_identical(e$.groq_pick(later, e$.GROQ_PREFS$vision), "qwen/qwen3.9-27b")
})

test_that("nothing suitable on offer gives NA, not a made-up name", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  expect_true(is.na(e$.groq_pick(c("whisper-large-v3", "allam-2-7b"), e$.GROQ_PREFS$vision)))
})

test_that("an explicit choice wins only while Groq still offers it", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  e$.groq_available_models <- function(key, ttl = 3600) .oct_2026
  withr::local_options(soilKey.groq_vision_model = "openai/gpt-oss-20b")
  expect_identical(e$.groq_model("vision", key = "k"), "openai/gpt-oss-20b")
  # the retired name from August is ignored instead of breaking the tab again
  withr::local_options(soilKey.groq_vision_model = "qwen/qwen3.6-27b")
  expect_identical(e$.groq_model("vision", key = "k"), "qwen/qwen3.8-27b")
  withr::local_envvar(GROQ_TEXT_MODEL = "llama-3.3-70b-versatile")
  expect_identical(e$.groq_model("text", key = "k"), "qwen/qwen3.8-27b")
})

test_that("a model at its limit can be set aside for the next in line", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  e$.groq_available_models <- function(key, ttl = 3600) .oct_2026
  expect_identical(e$.groq_model("text", "k", exclude = "qwen/qwen3.8-27b"),
                   "openai/gpt-oss-120b")
  expect_identical(e$.groq_model("text", "k",
                                 exclude = c("qwen/qwen3.8-27b", "openai/gpt-oss-120b")),
                   "openai/gpt-oss-20b")
  # an explicit choice that is set aside does not come back
  withr::local_options(soilKey.groq_text_model = "openai/gpt-oss-120b")
  expect_identical(e$.groq_model("text", "k"), "openai/gpt-oss-120b")
  expect_identical(e$.groq_model("text", "k", exclude = "openai/gpt-oss-120b"),
                   "qwen/qwen3.8-27b")
})

test_that("if the list cannot be read, the explicit choice or last good default is used", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  e$.groq_available_models <- function(key, ttl = 3600) NULL
  withr::local_options(soilKey.groq_vision_model = NULL)
  withr::local_envvar(GROQ_VISION_MODEL = "")
  expect_identical(e$.groq_model("vision", key = "k"), e$.GROQ_FALLBACK$vision)
  withr::local_options(soilKey.groq_vision_model = "my/model")
  expect_identical(e$.groq_model("vision", key = "k"), "my/model")
  # the text defaults come in order, so one can stand in for the other
  withr::local_options(soilKey.groq_text_model = NULL)
  withr::local_envvar(GROQ_TEXT_MODEL = "")
  expect_identical(e$.groq_model("text", key = "k"), "qwen/qwen3.8-27b")
  expect_identical(e$.groq_model("text", key = "k", exclude = "qwen/qwen3.8-27b"),
                   "openai/gpt-oss-120b")
  expect_true(is.na(e$.groq_model("text", key = "k", exclude = e$.GROQ_FALLBACK$text)))
})

test_that("Groq's retirement error is recognised; other errors are not", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  # the exact message the Photo tab showed on 2026-10-07
  gone <- simpleError(paste0("VLM provider call failed on attempt 1: HTTP 404 Not Found. ",
                             "The model `qwen/qwen3.6-27b` does not exist or you do not have access to it."))
  expect_true(e$.groq_model_gone(gone))
  expect_true(e$.groq_model_gone("The model has been decommissioned"))
  expect_false(e$.groq_model_gone(simpleError("HTTP 429: Rate limit reached for model")))
  expect_false(e$.groq_model_gone(simpleError("Request too large")))
})

test_that("Qwen reasons not at all, gpt-oss little, other families get nothing", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  expect_identical(e$.groq_api_args("qwen/qwen3.8-27b"), list(reasoning_effort = "none"))
  expect_identical(e$.groq_api_args("openai/gpt-oss-120b"), list(reasoning_effort = "low"))
  expect_identical(e$.groq_api_args("meta-llama/llama-4-maverick"), list())
})

test_that("Groq's per-minute limits are recognised, in the words Groq used", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  # verbatim (organisation id removed) from 2026-10-07
  itpm <- simpleError(paste(
    "HTTP 429 Too Many Requests.",
    "Rate limit reached for model `qwen/qwen3.8-27b` in organization `org` service",
    "tier `on_demand` on input tokens per minute (ITPM): Limit 7000, Used 3911,",
    "Requested 4415. Please try again in 11.365714285s."))
  otpm <- paste(
    "Request too large for model `qwen/qwen3.8-27b` in organization `org` service",
    "tier `on_demand` on output tokens per minute (OTPM): Limit 1000, Requested 1430.")
  expect_true(e$.groq_rate_limited(itpm))
  expect_true(e$.groq_rate_limited(otpm))
  expect_false(e$.groq_model_gone(itpm))     # a busy model is not a retired one
  expect_false(e$.groq_rate_limited(simpleError(
    "HTTP 404 Not Found. The model `qwen/qwen3.6-27b` does not exist")))
})

test_that("the Assistant samples cool and caps its output", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  q <- e$.groq_text_params("qwen/qwen3.8-27b")
  expect_equal(q$temperature, 0.2)
  expect_lte(q$max_tokens, 1000L)              # Qwen's output allowance a minute
  # gpt-oss reasons first, and that counts against the cap
  expect_gt(e$.groq_text_params("openai/gpt-oss-120b")$max_tokens, q$max_tokens)
})

test_that("the model list is read once an hour, and again after a retirement", {
  skip_if_not_installed("shiny"); skip_if_not_installed("httr")
  e <- .groq_env()
  calls <- 0L
  fake <- function(url, ...) {
    calls <<- calls + 1L
    structure(list(status_code = 200L), class = "response")
  }
  local_mocked_bindings(
    GET = fake,
    status_code = function(r) 200L,
    content = function(r, ...) list(data = list(
      list(id = "qwen/qwen3.8-27b", active = TRUE),
      list(id = "old/retired", active = FALSE))),
    .package = "httr")
  e$.groq_forget_models()
  ids <- e$.groq_available_models("k")
  expect_identical(ids, "qwen/qwen3.8-27b")          # inactive models dropped
  e$.groq_available_models("k")
  expect_equal(calls, 1L)                             # cached
  e$.groq_forget_models()
  e$.groq_available_models("k")
  expect_equal(calls, 2L)                             # read again
})

test_that("a failed listing is not cached", {
  skip_if_not_installed("shiny"); skip_if_not_installed("httr")
  e <- .groq_env()
  local_mocked_bindings(GET = function(...) stop("offline"), .package = "httr")
  e$.groq_forget_models()
  expect_null(e$.groq_available_models("k"))
  expect_null(e$.groq_cache$ids)
})

test_that("the status names the model in use", {
  skip_if_not_installed("shiny")
  e <- .groq_env()
  expect_match(e$i18n("chat.backend_groq", "openai/gpt-oss-120b", lang = "en"),
               "openai/gpt-oss-120b", fixed = TRUE)
  expect_match(e$i18n("chat.backend_groq", "openai/gpt-oss-120b", lang = "pt"),
               "openai/gpt-oss-120b", fixed = TRUE)
  expect_false(identical(e$i18n("chat.backend_unavailable", lang = "en"),
                         "chat.backend_unavailable"))
})

test_that("LIVE: both models resolve to something Groq lists today", {
  skip_on_cran()
  skip_if_not(isTRUE(as.logical(Sys.getenv("SOILKEY_TEST_GROQ_LIVE", "false"))),
              "set SOILKEY_TEST_GROQ_LIVE=true and GROQ_API_KEY to query Groq")
  key <- Sys.getenv("GROQ_API_KEY", "")
  skip_if_not(nzchar(key), "no GROQ_API_KEY")
  e <- .groq_env()
  e$.groq_forget_models()
  ids <- e$.groq_available_models(key)
  expect_true(length(ids) > 0L)
  expect_true(e$.groq_model("vision", key) %in% ids)
  expect_true(e$.groq_model("text", key) %in% ids)
})
