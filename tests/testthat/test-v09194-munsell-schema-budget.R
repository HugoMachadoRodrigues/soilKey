# v0.9.194: the photo -> Munsell route asks for colour only.
#
# extract_munsell_from_photo() injected the FULL horizon schema (35 properties)
# into the prompt and then threw all but five fields away. On Groq's free tier
# that dead weight was ~3,200 of the 8,000-tokens-per-minute budget, and a real
# photo could not fit beside it -- the call was rejected before the model ran
# (HTTP 413, after the HTTP 404 of the retired model was fixed in v0.9.193).
#
# munsell.json is DERIVED from horizon.json by data-raw/generate_munsell_schema.R,
# so these tests pin the subset relationship rather than a copy.

.munsell_fields <- c("top_cm", "bottom_cm", "designation",
                     "munsell_moist", "munsell_dry")

test_that("the slim munsell schema ships and carries only the colour fields", {
  sch <- jsonlite::fromJSON(load_schema("munsell"), simplifyVector = FALSE)
  props <- sch$properties$horizons$items$properties
  expect_setequal(names(props), .munsell_fields)
})

test_that("munsell.json is a faithful SUBSET of horizon.json", {
  # Every kept field must be byte-identical to its canonical definition, so the
  # slim schema can never drift into validating something different.
  slim <- jsonlite::fromJSON(load_schema("munsell"), simplifyVector = FALSE)
  full <- jsonlite::fromJSON(load_schema("horizon"), simplifyVector = FALSE)
  sp <- slim$properties$horizons$items$properties
  fp <- full$properties$horizons$items$properties
  for (f in .munsell_fields) expect_identical(sp[[f]], fp[[f]])
  # and the envelope is unchanged
  expect_identical(slim$required, full$required)
  expect_identical(slim$properties$horizons$items$required,
                   full$properties$horizons$items$required)
})

test_that("the slim schema is materially smaller than the full one", {
  # The whole point: the prompt payload has to shrink. Guard the direction and
  # a conservative magnitude, not an exact byte count.
  expect_lt(nchar(load_schema("munsell")), nchar(load_schema("horizon")) / 3)
})

test_that("a colour-only response validates against the slim schema", {
  ok <- paste0(
    '{"horizons":[{"top_cm":0,"bottom_cm":30,"designation":"A",',
    '"munsell_moist":{"hue":"10YR","value":2,"chroma":1,"confidence":0.6}}]}')
  expect_true(validate_against_schema(ok, "munsell")$valid)
})

test_that("the slim schema still accepts a response carrying extra fields", {
  # horizon.json does not set additionalProperties: false, and neither does the
  # subset -- a model that volunteers clay_pct must not be rejected for it.
  rich <- paste0(
    '{"horizons":[{"top_cm":0,"bottom_cm":30,"designation":"A","clay_pct":22,',
    '"munsell_moist":{"hue":"10YR","value":2,"chroma":1,"confidence":0.6}}]}')
  expect_true(validate_against_schema(rich, "munsell")$valid)
})

test_that("extract_munsell_from_photo() defaults to the slim schema", {
  expect_identical(formals(extract_munsell_from_photo)$schema_name, "munsell")
})

test_that("the Munsell extraction still works end to end on the slim schema", {
  # Mock provider returning exactly what a vision model gives us: the pipeline
  # must validate and land the colours on the pedon.
  pr <- PedonRecord$new(
    site = list(id = "slim"),
    horizons = data.frame(designation = c("A", "Bw"),
                          top_cm = c(0, 20), bottom_cm = c(20, 80)))
  answer <- paste0(
    '{"horizons":[',
    '{"top_cm":0,"bottom_cm":20,"designation":"A",',
    '"munsell_moist":{"hue":"10YR","value":3,"chroma":2,"confidence":0.7}},',
    '{"top_cm":20,"bottom_cm":80,"designation":"Bw",',
    '"munsell_moist":{"hue":"7.5YR","value":4,"chroma":4,"confidence":0.7}}]}')
  img <- tempfile(fileext = ".png")
  grDevices::png(img, width = 20, height = 40); graphics::par(mar = c(0,0,0,0))
  graphics::plot.new(); grDevices::dev.off()
  res <- extract_munsell_from_photo(
    pr, img, MockVLMProvider$new(responses = list(answer)))
  expect_equal(attr(res, "vlm_extraction")$attempts, 1L)
  expect_true("10YR" %in% res$horizons$munsell_hue_moist)
  expect_true("7.5YR" %in% res$horizons$munsell_hue_moist)
})
