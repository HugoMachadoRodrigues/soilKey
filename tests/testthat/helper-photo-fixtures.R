# Test fixtures for the v0.9.99 photo-only classification pipeline.
# Sourced automatically by testthat before the test files run.

# Canned, horizon-schema-valid Munsell extraction response (three
# horizons, low-chroma red B -- Ferralsol-like).
photo_munsell_json <- function() {
  paste0(
    '{"horizons":[',
    '{"top_cm":0,"bottom_cm":20,"designation":"A",',
    '"munsell_moist":{"hue":"2.5YR","value":3,"chroma":4,',
    '"confidence":0.55,"source_quote":"uppermost ~20 cm next to card"}},',
    '{"top_cm":20,"bottom_cm":70,"designation":"Bw",',
    '"munsell_moist":{"hue":"2.5YR","value":3,"chroma":6,',
    '"confidence":0.6,"source_quote":"mid profile"}},',
    '{"top_cm":70,"bottom_cm":150,"designation":"BC",',
    '"munsell_moist":{"hue":"10R","value":3,"chroma":6,',
    '"confidence":0.5,"source_quote":"lower profile"}}',
    ']}'
  )
}

# Canned, site-schema-valid field-sheet extraction response.
photo_site_json <- function() {
  paste0(
    '{"lat":{"value":-22.74,"confidence":0.7,"source_quote":"GPS"},',
    '"lon":{"value":-43.68,"confidence":0.7,"source_quote":"GPS"}}'
  )
}

# A MockVLMProvider pre-loaded with `n` copies of the Munsell response
# (extract_*_from_photo may retry, so queue a few).
photo_mock <- function(json = photo_munsell_json(), n = 4L) {
  soilKey::MockVLMProvider$new(responses = rep(list(json), n))
}

# Write a small, valid PNG to a temp path and return it. Requires magick;
# callers must skip_if_not_installed("magick") first.
photo_test_image <- function(colour = "tan") {
  path <- tempfile(fileext = ".png")
  magick::image_write(magick::image_blank(24, 24, colour), path,
                      format = "png")
  path
}

# Six-slice SoilGrids depth profiles for a clayey, acidic, low-CEC soil
# (Ferralsol-like) -- used to exercise the depth-prior path offline.
photo_depth_profiles <- function() {
  list(
    clay_pct = c(45, 50, 55, 58, 60, 60),
    sand_pct = c(35, 33, 32, 32, 32, 32),
    silt_pct = c(20, 17, 13, 10,  8,  8),
    ph_h2o   = c(4.8, 4.7, 4.7, 4.8, 4.9, 4.9),
    oc_pct   = c(2.0, 1.2, 0.6, 0.3, 0.2, 0.2),
    cec_cmol = c(8.0, 7.0, 6.0, 5.0, 5.0, 5.0)
  )
}
