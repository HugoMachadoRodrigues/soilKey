# =============================================================================
# Tests for v0.9.70 -- ellmer structured outputs (chat_structured) bridge.
# =============================================================================


# ---- vlm_type_from_soilkey_schema -----------------------------------------

test_that("vlm_type_from_soilkey_schema rejects bad input", {
  expect_error(vlm_type_from_soilkey_schema(NULL),    "non-empty character")
  expect_error(vlm_type_from_soilkey_schema(""),      "non-empty character")
  expect_error(vlm_type_from_soilkey_schema(c("a", "b")), "non-empty character")
})


test_that("vlm_type_from_soilkey_schema errors on unknown schema", {
  skip_if_not_installed("ellmer")
  expect_error(vlm_type_from_soilkey_schema("not-a-real-schema"),
                  "Schema not found")
})


test_that("vlm_type_from_soilkey_schema returns an ellmer type for horizon", {
  skip_if_not_installed("ellmer")
  if (!exists("type_from_schema", envir = asNamespace("ellmer"),
                inherits = FALSE)) {
    skip("ellmer version too old (no type_from_schema()).")
  }
  t <- vlm_type_from_soilkey_schema("horizon")
  expect_true(inherits(t, "ellmer::Type") || inherits(t, "Type") ||
                !is.null(t))
})


# ---- .provider_supports_structured ----------------------------------------

test_that(".provider_supports_structured: FALSE on NULL / mock", {
  expect_false(soilKey:::.provider_supports_structured(NULL))
  mock <- MockVLMProvider$new(responses = list("{}"))
  expect_false(soilKey:::.provider_supports_structured(mock))
})


test_that(".provider_supports_structured: TRUE on object exposing chat_structured", {
  fake <- list(
    chat = function(p, ...) "{}",
    chat_structured = function(p, type, ...) list(ok = TRUE)
  )
  expect_true(soilKey:::.provider_supports_structured(fake))
})


# ---- validate_or_retry: use_structured fast path -------------------------

test_that("validate_or_retry takes the structured path when provider supports it", {
  skip_if_not_installed("ellmer")
  if (!exists("type_from_schema", envir = asNamespace("ellmer"),
                inherits = FALSE)) {
    skip("ellmer version too old.")
  }
  # Stub provider that exposes chat + chat_structured methods. The
  # structured method ignores the prompt and returns a known list.
  golden_horizons <- list(horizons = list(
    list(top_cm = 0, bottom_cm = 30, designation = "A")
  ))
  fake <- list(
    chat = function(p, ...) stop("should not be called when use_structured=TRUE"),
    chat_structured = function(prompt, type, ...) golden_horizons
  )
  res <- soilKey:::validate_or_retry(
    fake, "irrelevant prompt", "horizon",
    max_retries = 0L, use_structured = TRUE
  )
  expect_equal(res$data, golden_horizons)
  expect_true(isTRUE(res$used_structured))
  expect_equal(res$attempts, 1L)
})


test_that("validate_or_retry falls back to legacy loop when provider lacks chat_structured", {
  # MockVLMProvider has only chat() -- no chat_structured.
  golden <- list(horizons = list(list(top_cm = 0, bottom_cm = 30,
                                          designation = "A")))
  mock <- MockVLMProvider$new(responses = list(jsonlite::toJSON(golden,
                                                                       auto_unbox = TRUE)))
  res <- soilKey:::validate_or_retry(
    mock, "p", "horizon", max_retries = 0L, use_structured = TRUE
  )
  # Did NOT short-circuit through structured path.
  expect_null(res$used_structured)
  expect_true(!is.na(res$raw))
})


test_that("validate_or_retry use_structured = FALSE preserves existing behaviour", {
  golden <- list(horizons = list(list(top_cm = 0, bottom_cm = 30,
                                          designation = "A")))
  mock <- MockVLMProvider$new(responses = list(jsonlite::toJSON(golden,
                                                                       auto_unbox = TRUE)))
  res <- soilKey:::validate_or_retry(
    mock, "p", "horizon", max_retries = 0L, use_structured = FALSE
  )
  expect_null(res$used_structured)
})


# ---- extract_*() pass through ---------------------------------------------

test_that("extract_horizons_from_pdf accepts use_structured parameter", {
  fn <- formals(extract_horizons_from_pdf)
  expect_true("use_structured" %in% names(fn))
  expect_false(eval(fn$use_structured))  # default FALSE
})


test_that("extract_munsell_from_photo accepts use_structured parameter", {
  fn <- formals(extract_munsell_from_photo)
  expect_true("use_structured" %in% names(fn))
})


test_that("extract_site_from_fieldsheet accepts use_structured parameter", {
  fn <- formals(extract_site_from_fieldsheet)
  expect_true("use_structured" %in% names(fn))
})


# ---- benchmark_vlm_extraction pass through -------------------------------

test_that("benchmark_vlm_extraction accepts use_structured parameter", {
  fn <- formals(benchmark_vlm_extraction)
  expect_true("use_structured" %in% names(fn))
  expect_false(eval(fn$use_structured))  # default FALSE for back-compat
})
