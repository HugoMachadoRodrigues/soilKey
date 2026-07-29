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


# ---- site route: the {value, confidence, source_quote} envelope -------------
#
# 12 of the 14 site fields use that envelope, so a model wraps `id` and `crs`
# too. The schema declared those two flat, so the site route failed validation
# on the FIRST attempt every time -- and on a rate-limited tier the retry never
# landed, which is how it reached the ISRIC user as a hard failure.

test_that("the site schema accepts id/crs both flat and wrapped", {
  flat <- '{"site":{"id":"P1","crs":4326}}'
  expect_true(validate_against_schema(flat, "site")$valid)
  wrapped <- paste0(
    '{"site":{"id":{"value":"P1","confidence":1,"source_quote":"ID: P1"},',
    '"crs":{"value":4326,"confidence":1,"source_quote":"WGS84"}}}')
  expect_true(validate_against_schema(wrapped, "site")$valid)
})

test_that("a wrapped id lands on the pedon as a plain value, not a list", {
  pr <- PedonRecord$new(site = list())
  parsed <- jsonlite::fromJSON(paste0(
    '{"site":{"id":{"value":"ISRIC-01","confidence":1,"source_quote":"q"},',
    '"lat":{"value":-22.74,"confidence":1,"source_quote":"q"}}}'),
    simplifyVector = FALSE)
  apply_site_extraction(pr, parsed)
  expect_identical(pr$site$id, "ISRIC-01")
  expect_true(is.character(pr$site$id))
  expect_equal(pr$site$lat, -22.74)
})

test_that("a flat id still works (no regression)", {
  pr <- PedonRecord$new(site = list())
  parsed <- jsonlite::fromJSON('{"site":{"id":"P1","crs":4326}}',
                               simplifyVector = FALSE)
  apply_site_extraction(pr, parsed)
  expect_identical(pr$site$id, "P1")
  expect_identical(pr$site$crs, 4326L)
})
