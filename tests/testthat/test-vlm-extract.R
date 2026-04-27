## Module 2 -- VLM extraction tests
##
## All tests use MockVLMProvider so the suite never depends on API
## keys, network access, or real PDFs / images. We test:
##   - happy path: extracted_vlm provenance is recorded
##   - retry path: schema-validation failure triggers a re-prompt
##   - authority order: extracted_vlm cannot overwrite measured

skip_if_not_installed("jsonvalidate")
skip_if_not_installed("jsonlite")


# Tiny JSON corresponding to a 2-horizon profile, well-formed against
# the horizon schema.
canned_horizons_json <- function() {
  '{
    "horizons": [
      {
        "top_cm": 0,
        "bottom_cm": 30,
        "designation": "A",
        "clay_pct": {
          "value": 18.5,
          "confidence": 0.85,
          "source_quote": "Argila 18.5% no horizonte A"
        },
        "ph_h2o": {
          "value": 5.2,
          "confidence": 0.9,
          "source_quote": "pH em agua 5.2"
        },
        "munsell_moist": {
          "hue": "10YR",
          "value": 4,
          "chroma": 3,
          "confidence": 0.7,
          "source_quote": "10YR 4/3 umido"
        }
      },
      {
        "top_cm": 30,
        "bottom_cm": 80,
        "designation": "Bt",
        "clay_pct": {
          "value": 42,
          "confidence": 0.92,
          "source_quote": "Argila 42% no Bt"
        },
        "ph_h2o": {
          "value": 5.5,
          "confidence": 0.9,
          "source_quote": "pH em agua 5.5"
        }
      }
    ]
  }'
}


canned_site_json <- function() {
  '{
    "site": {
      "id": "P-test-01",
      "lat": {
        "value": -22.5,
        "confidence": 0.95,
        "source_quote": "Latitude 22 30 S"
      },
      "lon": {
        "value": -43.7,
        "confidence": 0.95,
        "source_quote": "Longitude 43 42 W"
      },
      "country": {
        "value": "BR",
        "confidence": 1.0,
        "source_quote": "Brasil"
      },
      "parent_material": {
        "value": "gneiss",
        "confidence": 0.8,
        "source_quote": "material de origem: gnaisse"
      }
    }
  }'
}


# A trivial empty PDF stand-in: we monkey-patch pdftools::pdf_text via
# a temporary text file when pdftools is not installed. To keep the
# tests provider-agnostic, we exercise the apply_horizons_extraction
# layer directly when we can't read PDFs.

test_that("MockVLMProvider returns canned responses in order", {
  mock <- MockVLMProvider$new(responses = list("first", "second"))
  expect_equal(mock$chat("p1"), "first")
  expect_equal(mock$chat("p2"), "second")
  expect_equal(mock$call_count, 2L)
  expect_length(mock$prompts_received, 2L)
  expect_equal(mock$prompts_received[[1]], "p1")
})


test_that("MockVLMProvider with validation_error_at returns bad JSON on that call", {
  mock <- MockVLMProvider$new(
    responses           = list("OK1", "OK2", "OK3"),
    validation_error_at = 2L
  )
  # Cache each response so we don't re-call the mock from inside
  # expect_*; testthat occasionally evaluates the labelled expression
  # twice when an assertion fails or builds a diff.
  r1 <- mock$chat("p")
  r2 <- mock$chat("p")
  r3 <- mock$chat("p")
  expect_equal(r1, "OK1")
  expect_match(r2, "this is not valid json")
  expect_equal(r3, "OK3")
  expect_equal(mock$call_count, 3L)
})


test_that("MockVLMProvider errors when its queue is exhausted", {
  mock <- MockVLMProvider$new(responses = list("only one"))
  mock$chat("p")
  expect_error(mock$chat("p"), "exhausted")
})


test_that("validate_or_retry returns parsed JSON on first valid response", {
  mock <- MockVLMProvider$new(responses = list(canned_horizons_json()))
  res <- soilKey:::validate_or_retry(
    provider    = mock,
    prompt      = "irrelevant",
    schema      = "horizon",
    max_retries = 0L
  )
  expect_equal(res$attempts, 1L)
  expect_length(res$data$horizons, 2L)
  expect_equal(res$data$horizons[[1]]$top_cm, 0)
})


test_that("validate_or_retry retries on schema-validation failure", {
  mock <- MockVLMProvider$new(
    responses           = list(canned_horizons_json(), canned_horizons_json()),
    validation_error_at = 1L
  )
  res <- soilKey:::validate_or_retry(
    provider    = mock,
    prompt      = "irrelevant",
    schema      = "horizon",
    max_retries = 3L
  )
  expect_equal(res$attempts, 2L)
  # Retry prompt must include the previous validation error.
  expect_match(mock$prompts_received[[2]], "previous response failed")
})


test_that("validate_or_retry aborts after exhausting retries", {
  # Always-bad mock: every call returns malformed JSON.
  bad_provider <- list(
    chat = function(prompt, ...) "{ still not json"
  )
  expect_error(
    soilKey:::validate_or_retry(
      provider    = bad_provider,
      prompt      = "irrelevant",
      schema      = "horizon",
      max_retries = 2L
    ),
    "extraction failed after"
  )
})


test_that("apply_horizons_extraction tags every value with extracted_vlm", {
  pedon <- PedonRecord$new()
  parsed <- jsonlite::fromJSON(canned_horizons_json(), simplifyVector = FALSE)
  added  <- soilKey:::apply_horizons_extraction(pedon, parsed)

  expect_gt(added, 0L)
  expect_equal(nrow(pedon$horizons), 2L)
  expect_true(all(pedon$provenance$source == "extracted_vlm"))
  # Spot-check value pass-through.
  expect_equal(pedon$horizons$clay_pct[1], 18.5)
  expect_equal(pedon$horizons$clay_pct[2], 42)
  expect_equal(pedon$horizons$munsell_hue_moist[1], "10YR")
  expect_equal(pedon$horizons$munsell_value_moist[1], 4)
})


test_that("apply_horizons_extraction does NOT overwrite measured values", {
  # Build a pedon with one existing measured horizon.
  pedon <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  pedon$add_measurement(1L, "clay_pct", 25.0, source = "measured")
  expect_equal(pedon$horizons$clay_pct[1], 25.0)

  # VLM tries to assert a different clay_pct for the same depth band.
  parsed <- jsonlite::fromJSON(canned_horizons_json(), simplifyVector = FALSE)
  soilKey:::apply_horizons_extraction(pedon, parsed)

  # The measured value (25) must survive; VLM had 18.5.
  expect_equal(pedon$horizons$clay_pct[1], 25.0)

  # But fields with no prior measurement do get filled by the VLM.
  expect_equal(pedon$horizons$ph_h2o[1], 5.2)
  ph_prov <- pedon$provenance[
    pedon$provenance$attribute   == "ph_h2o" &
    pedon$provenance$horizon_idx == 1L, ]
  expect_equal(nrow(ph_prov), 1L)
  expect_equal(ph_prov$source, "extracted_vlm")

  # The measured row keeps its measured-source provenance.
  measured_rows <- pedon$provenance[
    pedon$provenance$source == "measured", ]
  expect_gte(nrow(measured_rows), 1L)
})


test_that("apply_horizons_extraction respects overwrite = TRUE", {
  pedon <- PedonRecord$new(
    horizons = data.frame(top_cm = 0, bottom_cm = 30)
  )
  pedon$add_measurement(1L, "clay_pct", 25.0, source = "measured")

  parsed <- jsonlite::fromJSON(canned_horizons_json(), simplifyVector = FALSE)
  soilKey:::apply_horizons_extraction(pedon, parsed, overwrite = TRUE)

  # With overwrite, the VLM value wins.
  expect_equal(pedon$horizons$clay_pct[1], 18.5)
})


test_that("apply_horizons_extraction matches existing depth bands within tolerance", {
  pedon <- PedonRecord$new(
    horizons = data.frame(top_cm = c(0, 30),
                            bottom_cm = c(30, 80))
  )
  parsed <- jsonlite::fromJSON(canned_horizons_json(), simplifyVector = FALSE)
  soilKey:::apply_horizons_extraction(pedon, parsed)

  # Should still be 2 rows -- no spurious appends.
  expect_equal(nrow(pedon$horizons), 2L)
  expect_equal(pedon$horizons$clay_pct[1], 18.5)
  expect_equal(pedon$horizons$clay_pct[2], 42)
})


test_that("apply_site_extraction fills empty site fields", {
  pedon <- PedonRecord$new()
  parsed <- jsonlite::fromJSON(canned_site_json(), simplifyVector = FALSE)
  added  <- soilKey:::apply_site_extraction(pedon, parsed)

  expect_gt(added, 0L)
  expect_equal(pedon$site$id,  "P-test-01")
  expect_equal(pedon$site$lat, -22.5)
  expect_equal(pedon$site$country, "BR")
  expect_equal(pedon$site$parent_material, "gneiss")
})


test_that("apply_site_extraction preserves existing site fields", {
  pedon <- PedonRecord$new(site = list(country = "PT",
                                          parent_material = "schist"))
  parsed <- jsonlite::fromJSON(canned_site_json(), simplifyVector = FALSE)
  soilKey:::apply_site_extraction(pedon, parsed, overwrite = FALSE)

  # Existing fields untouched.
  expect_equal(pedon$site$country, "PT")
  expect_equal(pedon$site$parent_material, "schist")
  # Empty fields filled.
  expect_equal(pedon$site$lat, -22.5)
})


test_that("apply_site_extraction with overwrite = TRUE clobbers existing fields", {
  pedon <- PedonRecord$new(site = list(country = "PT"))
  parsed <- jsonlite::fromJSON(canned_site_json(), simplifyVector = FALSE)
  soilKey:::apply_site_extraction(pedon, parsed, overwrite = TRUE)
  expect_equal(pedon$site$country, "BR")
})


test_that("extract_horizons_from_pdf goes end-to-end with a mock provider", {
  skip_if_not_installed("pdftools")

  # Build a minimal PDF on the fly for the test. pdftools::pdf_text
  # works on real PDFs; we generate one via a tiny pdf via grDevices.
  tmp_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_pdf, width = 4, height = 4)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Soil profile description: A 0-30cm; Bt 30-80cm.")
  grDevices::dev.off()

  mock <- MockVLMProvider$new(responses = list(canned_horizons_json()))
  pedon <- PedonRecord$new()
  result <- extract_horizons_from_pdf(
    pedon       = pedon,
    pdf_path    = tmp_pdf,
    provider    = mock,
    max_retries = 1L
  )

  expect_s3_class(result, "PedonRecord")
  expect_equal(nrow(result$horizons), 2L)
  expect_true(all(result$provenance$source == "extracted_vlm"))
  expect_length(result$documents, 1L)
  expect_equal(result$documents[[1]]$type, "pdf")

  unlink(tmp_pdf)
})


test_that("extract_horizons_from_pdf retries when validation fails first", {
  skip_if_not_installed("pdftools")

  tmp_pdf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tmp_pdf, width = 4, height = 4)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Soil profile description")
  grDevices::dev.off()

  mock <- MockVLMProvider$new(
    responses           = list(canned_horizons_json(), canned_horizons_json()),
    validation_error_at = 1L
  )
  pedon <- PedonRecord$new()
  result <- extract_horizons_from_pdf(
    pedon       = pedon,
    pdf_path    = tmp_pdf,
    provider    = mock,
    max_retries = 3L
  )
  attrs <- attr(result, "vlm_extraction")
  expect_equal(attrs$attempts, 2L)
  expect_match(mock$prompts_received[[2]], "previous response failed")

  unlink(tmp_pdf)
})


test_that("extract_horizons_from_pdf rejects non-PedonRecord input", {
  expect_error(
    extract_horizons_from_pdf(pedon = list(), pdf_path = "x", provider = NULL),
    "must be a PedonRecord"
  )
})


test_that("extract_horizons_from_pdf errors on missing file", {
  expect_error(
    extract_horizons_from_pdf(
      pedon = PedonRecord$new(),
      pdf_path = "/nonexistent/path.pdf",
      provider = MockVLMProvider$new(responses = list("{}"))
    ),
    "PDF not found"
  )
})


test_that("schema and prompt loaders find the packaged files", {
  schema_text <- soilKey:::load_schema("horizon")
  expect_match(schema_text, "json-schema")
  expect_match(schema_text, "horizons")

  site_text <- soilKey:::load_schema("site")
  expect_match(site_text, "json-schema")
  expect_match(site_text, "lat")

  prompt_text <- soilKey:::load_prompt(
    "extract_horizons",
    vars = list(schema_json = "<<S>>", document_text = "<<D>>")
  )
  expect_match(prompt_text, "<<S>>", fixed = TRUE)
  expect_match(prompt_text, "<<D>>", fixed = TRUE)
})
