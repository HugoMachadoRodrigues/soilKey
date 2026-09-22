# =============================================================================
# Tests for v0.9.61 -- diagnostic gaps from the v0.9.60 BDsolos benchmark.
#
# Three fixes shipped, three test groups:
#
#   1. .bdsolos_mosqueado_to_pct() -- ordinal -> percent for mottle quantity
#   2. test_gleyic_features Munsell-hue path -- gleyic via N / 5GY / 10G hues
#   3. B_latossolico clay-films guard -- argic excludes Latossolos only when
#      clay films are common+ (per SiBCS Cap 18)
# =============================================================================


# ---- 1. .bdsolos_mosqueado_to_pct -------------------------------------

test_that(".bdsolos_mosqueado_to_pct maps the three Embrapa ordinal classes", {
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("pouco"),     1)
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("comum"),    10)
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("abundante"),30)
})


test_that(".bdsolos_mosqueado_to_pct accepts plural + diacritic variants", {
  # Plural
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("poucos"),     1)
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("comuns"),    10)
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("abundantes"),30)
  # Title case
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("Pouco"),     1)
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("Comum"),    10)
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct("Abundante"),30)
  # With trailing whitespace
  expect_equal(soilKey:::.bdsolos_mosqueado_to_pct(" pouco "),    1)
})


test_that(".bdsolos_mosqueado_to_pct returns NA for empty/unknown", {
  expect_true(is.na(soilKey:::.bdsolos_mosqueado_to_pct("")))
  expect_true(is.na(soilKey:::.bdsolos_mosqueado_to_pct(NA)))
  expect_true(is.na(soilKey:::.bdsolos_mosqueado_to_pct("ausente")))
  expect_true(is.na(soilKey:::.bdsolos_mosqueado_to_pct("foo bar")))
})


test_that(".bdsolos_mosqueado_to_pct vectorises", {
  out <- soilKey:::.bdsolos_mosqueado_to_pct(
    c("pouco", "comum", "abundante", "", NA))
  expect_equal(out, c(1, 10, 30, NA, NA))
})


# ---- 2. test_gleyic_features Munsell-hue path -------------------------

.make_hz <- function(hue, value, chroma, redox = NA_real_, top = 0,
                       bottom = 30) {
  data.frame(
    designation = "Bg",
    top_cm = top, bottom_cm = bottom,
    munsell_hue_moist    = hue,
    munsell_value_moist  = value,
    munsell_chroma_moist = chroma,
    redoximorphic_features_pct = redox,
    stringsAsFactors = FALSE
  )
}


test_that("test_gleyic_features fires on gleyic hues with low chroma", {
  expect_true(isTRUE(test_gleyic_features(.make_hz("5GY",  5, 1))$passed))
  expect_true(isTRUE(test_gleyic_features(.make_hz("10G",  6, 2))$passed))
  expect_true(isTRUE(test_gleyic_features(.make_hz("5BG",  4, 1))$passed))
  expect_true(isTRUE(test_gleyic_features(.make_hz("N",    5, 0))$passed))
  expect_true(isTRUE(test_gleyic_features(.make_hz("10Y",  6, 2))$passed))
})


test_that("test_gleyic_features rejects oxidized hues", {
  expect_false(isTRUE(test_gleyic_features(.make_hz("10YR", 4, 6))$passed))
  expect_false(isTRUE(test_gleyic_features(.make_hz("5YR",  4, 6))$passed))
  expect_false(isTRUE(test_gleyic_features(.make_hz("2.5YR", 3, 5))$passed))
})


test_that("test_gleyic_features rejects gleyic hue with high chroma", {
  # 5GY with chroma 4 is not gleyic per WRB Ch 3.1.13
  expect_false(isTRUE(test_gleyic_features(.make_hz("5GY", 5, 4))$passed))
})


test_that("test_gleyic_features uses redox percent when populated", {
  # Even with non-gleyic hue, high redox percent qualifies
  expect_true(isTRUE(test_gleyic_features(
    .make_hz("10YR", 4, 4, redox = 15))$passed))
  # Redox below threshold + non-gleyic hue -> no
  expect_false(isTRUE(test_gleyic_features(
    .make_hz("10YR", 4, 4, redox = 2))$passed))
})


test_that("test_gleyic_features returns NA when both paths unobservable", {
  expect_true(is.na(test_gleyic_features(.make_hz(NA, NA, NA))$passed))
})


# ---- 3. B_latossolico clay-films guard --------------------------------

# Build a synthetic Latossolo: ferralic-passing B horizon, low CEC/clay,
# thick (>= 50 cm), clay >= 25%. Optionally toss an argic-passing
# clay increase, with controllable clay_films_amount.
.make_latossolo_pedon <- function(clay_films = NA_character_,
                                     argic_strong = FALSE) {
  if (argic_strong) {
    # Strong argic: clay 20 -> 50, ratio 2.5
    clay_a <- 20; clay_b <- 50
  } else {
    # Marginal argic: clay 25 -> 35, ratio 1.4 (just barely passes)
    clay_a <- 25; clay_b <- 35
  }
  hz <- data.table::data.table(
    designation          = c("A", "Bw1", "Bw2"),
    top_cm               = c(0,  20, 80),
    bottom_cm            = c(20, 80, 150),
    munsell_hue_moist    = c("10YR", "2.5YR", "2.5YR"),
    munsell_value_moist  = c(4, 4, 4),
    munsell_chroma_moist = c(3, 6, 6),
    clay_pct             = c(clay_a, clay_b, clay_b),
    silt_pct             = c(15, 15, 15),
    sand_pct             = c(60, 35, 35),
    ph_h2o               = c(5.5, 5.0, 4.8),
    oc_pct               = c(2.0, 0.5, 0.3),
    cec_cmol             = c(8, 5, 4),       # CEC/clay ~14-16, < 17
    base_saturation_pct  = c(40, 25, 20),
    clay_films_amount    = c(NA_character_, clay_films, clay_films),
    stringsAsFactors = FALSE
  )
  PedonRecord$new(
    site = list(id = "lat-test", lat = -19.5, lon = -43.9, country = "BR"),
    horizons = hz)
}


test_that("B_latossolico passes when clay films are weak/absent (Latossolo)", {
  p <- .make_latossolo_pedon(clay_films = NA, argic_strong = FALSE)
  expect_true(isTRUE(B_latossolico(p)$passed))
  p2 <- .make_latossolo_pedon(clay_films = "Pouca", argic_strong = FALSE)
  expect_true(isTRUE(B_latossolico(p2)$passed))
})


test_that("B_latossolico fails when clay films are common+ (Argissolo)", {
  p <- .make_latossolo_pedon(clay_films = "Comum", argic_strong = FALSE)
  expect_false(isTRUE(B_latossolico(p)$passed))
  p2 <- .make_latossolo_pedon(clay_films = "Abundante", argic_strong = FALSE)
  expect_false(isTRUE(B_latossolico(p2)$passed))
})


test_that("B_latossolico evidence captures argic for traceability", {
  p <- .make_latossolo_pedon(clay_films = "Pouca", argic_strong = FALSE)
  res <- B_latossolico(p)
  # The argic info is preserved even when not used to exclude
  expect_true("argic_concurrent" %in% names(res$evidence))
  expect_true("ferralic" %in% names(res$evidence))
})


test_that("B_latossolico still excludes plinthic / gleyic / nitic layers", {
  # Build a pedon that would pass ferralic but ALSO trigger gleyic
  hz <- data.table::data.table(
    designation = c("A", "Bg"),
    top_cm = c(0, 20), bottom_cm = c(20, 100),
    munsell_hue_moist = c("10YR", "5GY"),  # 5GY = gleyic
    munsell_value_moist = c(4, 5),
    munsell_chroma_moist = c(3, 1),         # chroma 1 = gleyic
    clay_pct = c(25, 30),
    silt_pct = c(15, 15),
    sand_pct = c(60, 55),
    ph_h2o = c(5.5, 5.0),
    cec_cmol = c(8, 4),
    base_saturation_pct = c(40, 25),
    stringsAsFactors = FALSE
  )
  p <- PedonRecord$new(site = list(id = "g-test", country = "BR"),
                         horizons = hz)
  expect_false(isTRUE(B_latossolico(p)$passed))   # gleyic excludes
})
