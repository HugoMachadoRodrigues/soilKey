# v0.9.193: the VLM extractor must survive a REASONING model.
#
# Groq retired meta-llama/llama-4-scout-17b-16e-instruct (every vision call
# returned HTTP 404 -- reported by Jelle Janssen, ISRIC). The replacement
# available on the same key, qwen/qwen3.6-27b, is a reasoning model: it emits a
# <think>...</think> block before the JSON. strip_code_fence() only removed ```
# fences, so a perfectly good extraction would have failed the JSON parse.

test_that("a <think> block is stripped before the JSON", {
  txt <- "<think>The top layer looks dark.</think>\n{\"horizons\":[]}"
  expect_equal(strip_code_fence(txt), '{"horizons":[]}')
})

test_that("a reasoning block wrapped in code fences is also handled", {
  txt <- "<think>reasoning here</think>\n```json\n{\"a\":1}\n```"
  expect_equal(strip_code_fence(txt), '{"a":1}')
})

test_that("multiple and multiline <think> blocks are removed", {
  txt <- "<think>one\nspans lines</think>{\"a\":1}<think>two</think>"
  expect_equal(strip_code_fence(txt), '{"a":1}')
})

test_that("an unterminated <think> (token cap hit) does not leave a tag", {
  txt <- "{\"a\":1}<think>truncated mid-thought and never closed"
  expect_equal(strip_code_fence(txt), '{"a":1}')
})

test_that("plain JSON and fenced JSON are untouched (no regression)", {
  expect_equal(strip_code_fence('{"a":1}'), '{"a":1}')
  expect_equal(strip_code_fence("```json\n{\"a\":1}\n```"), '{"a":1}')
  expect_equal(strip_code_fence("```\n{\"a\":1}\n```"), '{"a":1}')
})

test_that("extract_json_object() rescues an object wrapped in prose", {
  expect_equal(extract_json_object('Here it is: {"a":1} hope that helps!'),
               '{"a":1}')
  # nested braces: must take the LAST closing brace, not the first
  expect_equal(extract_json_object('x {"a":{"b":2}} y'), '{"a":{"b":2}}')
  # nothing to rescue -> unchanged
  expect_equal(extract_json_object("no json here"), "no json here")
  expect_equal(extract_json_object('{"a":1}'), '{"a":1}')
})

test_that("validate_or_retry() accepts a reasoning model's first response", {
  # The end-to-end regression: a provider that answers exactly the way
  # qwen3.6 does must validate on attempt 1, not burn retries.
  reasoning_answer <- paste0(
    "<think>Three bands are visible. The top is very dark.</think>\n",
    '{"horizons":[{"top_cm":0,"bottom_cm":30,"designation":"A",',
    '"munsell_moist":{"hue":"10YR","value":2,"chroma":1,',
    '"confidence":0.6,"source_quote":"dark top layer"}}]}')
  provider <- MockVLMProvider$new(responses = list(reasoning_answer))
  res <- validate_or_retry(provider, "prompt", "horizon")
  expect_equal(res$attempts, 1L)
  expect_length(res$data$horizons, 1L)
  expect_equal(res$data$horizons[[1]]$munsell_moist$hue, "10YR")
})

test_that("validate_or_retry() still rejects genuinely invalid output", {
  # Robustness must not become permissiveness: prose with no JSON at all
  # exhausts the retries and throws, as before.
  provider <- MockVLMProvider$new(
    responses = rep(list("I cannot read this image."), 4L))
  expect_error(validate_or_retry(provider, "prompt", "horizon", max_retries = 1L))
})
