# v0.9.255: item (a) of the B espodico.
#
# SiBCS 2018, Cap 2, p. 65: the B espodico has an "acumulacao iluvial de
# compostos organometalicos" and "a) Um horizonte E (albico ou nao)
# sobrejacente e cores umidas de acordo com um dos itens a seguir: 1) Matiz
# 5YR ou mais vermelho; 2) Matiz 7,5YR com valor 5 ou menor e croma 4 ou
# menor; 3) Matiz 10YR, com valor e croma 3 ou menor; 4) Cores neutras com
# valor 3 ou menor (N 3/)".
#
# B_espodico() was the WRB spodic horizon, which asks for aluminium and iron
# by oxalate or for a Bh or Bs under an albic horizon: the items of the book
# were not tested, and of the 81 Espodossolos of complete profile in the
# BDsolos export the default mode found 4. Decided on 2026-10-13: item (a),
# the illuvial accumulation being read from the organic carbon (0.5% or more
# and at least twice that of the E above) in a B with no cerosidade.

.esp_profile <- function(designation = c("A", "E", "B", "C"),
                         top = c(0, 20, 60, 90), bottom = c(20, 60, 90, 150),
                         oc = c(1.2, 0.1, 1.4, 0.1),
                         hue = c("10YR", "10YR", "10YR", "10YR"),
                         value = c(3, 7, 3, 6), chroma = c(1, 1, 2, 3),
                         clay = c(4, 2, 5, 3), films = NA_character_) {
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    oc_pct = oc, clay_pct = clay, silt_pct = 4, sand_pct = 100 - clay - 4,
    munsell_hue_moist = hue, munsell_value_moist = value,
    munsell_chroma_moist = chroma, ph_h2o = 4.5)
  hz <- ensure_horizon_schema(hz)
  hz$clay_films_amount <- films
  PedonRecord$new(site = list(id = "esp", country = "BR"), horizons = hz)
}

test_that("a B below an E, with the colour and the carbon, is a B espodico", {
  p <- .esp_profile()
  expect_false(isTRUE(spodic(p)$passed))        # no oxalate, no Bh or Bs
  r <- B_espodico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3L)
  expect_equal(r$evidence$item_a$e_above, 2L)
  expect_equal(r$evidence$item_a$oc_pct, 1.4)
  expect_equal(r$evidence$item_a$oc_e_pct, 0.1)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Espodossolos")
})

test_that("the organic carbon of the B: 0.5% or more and twice that of the E", {
  # twice the E, under 0.5%
  expect_false(isTRUE(B_espodico(.esp_profile(oc = c(1.2, 0.1, 0.4, 0.1)))$passed))
  # 0.5% or more, under twice the E
  expect_false(isTRUE(B_espodico(.esp_profile(oc = c(1.2, 0.4, 0.7, 0.1)))$passed))
  # the two limits
  expect_true(isTRUE(B_espodico(.esp_profile(oc = c(1.2, 0.25, 0.5, 0.1)))$passed))
  # not known in the B, or in the E: not known whether it accumulates
  expect_false(isTRUE(B_espodico(.esp_profile(oc = c(1.2, 0.1, NA, 0.1)))$passed))
  expect_false(isTRUE(B_espodico(.esp_profile(oc = c(1.2, NA, 1.4, 0.1)))$passed))
})

test_that("the carbon is compared with that of the nearest E above the B", {
  p <- .esp_profile(designation = c("A", "E1", "E2", "B", "C"),
                    top = c(0, 20, 40, 60, 90), bottom = c(20, 40, 60, 90, 150),
                    oc = c(1.2, 0.4, 0.1, 0.6, 0.1), hue = rep("10YR", 5),
                    value = c(3, 6, 7, 3, 6), chroma = c(1, 1, 1, 2, 3),
                    clay = c(4, 2, 2, 5, 3))
  r <- B_espodico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$item_a$e_above, 3L)
})

test_that("the moist colours are those of the item", {
  col <- function(hue, value, chroma) {
    hp <- .munsell_hue_parts(hue)
    .b_espodico_colour_a(hp$number, hp$family, value, chroma)
  }
  # 1) 5YR or redder, whatever the value and the chroma
  expect_true(col("5YR", 5, 8)); expect_true(col("2.5YR", 4, 6)); expect_true(col("10R", 3, 6))
  # 2) 7.5YR with value 5 or less and chroma 4 or less
  expect_true(col("7.5YR", 5, 4)); expect_false(col("7.5YR", 6, 4)); expect_false(col("7.5YR", 5, 6))
  # 3) 10YR with value and chroma 3 or less
  expect_true(col("10YR", 3, 3)); expect_false(col("10YR", 4, 3)); expect_false(col("10YR", 3, 4))
  # 4) neutral with value 3 or less
  expect_true(col("N", 3, NA)); expect_false(col("N", 4, NA))
  # yellower than 10YR, or not recorded: none of them
  expect_false(col("2.5Y", 3, 2)); expect_false(col(NA, 3, 2)); expect_false(col("10YR", NA, 2))
  # as it is written on the sheets
  expect_true(col("10yr", 2, 2)); expect_true(col("7,5YR", 4, 4))
  # in the horizon
  expect_true(isTRUE(B_espodico(.esp_profile(hue = rep("7.5YR", 4), value = c(3, 7, 4, 6), chroma = c(1, 1, 4, 3)))$passed))
  expect_false(isTRUE(B_espodico(.esp_profile(value = c(3, 7, 5, 6), chroma = c(1, 1, 6, 3)))$passed))
})

test_that("a B with cerosidade is no B espodico by this item", {
  # the Bt of an Argissolo abruptico below its E: red, with more carbon than
  # the E, and with clay films (as BDsolos MT 11249, SC 5444)
  p <- .esp_profile(designation = c("A", "E", "Bt1", "Bt2"),
                    oc = c(1.7, 0.2, 0.6, 0.2), hue = c("7.5YR", "5YR", "5YR", "5YR"),
                    value = c(3, 4, 4, 5), chroma = c(2, 4, 6, 8),
                    clay = c(8, 9, 41, 47), films = c(NA, NA, "Comum", "Comum"))
  r <- B_espodico(p)
  expect_false(isTRUE(r$passed))
  expect_false(identical(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Espodossolos"))
  # the same B with no cerosidade recorded passes the item
  q <- .esp_profile(designation = c("A", "E", "Bt1", "Bt2"),
                    oc = c(1.7, 0.2, 0.6, 0.2), hue = c("7.5YR", "5YR", "5YR", "5YR"),
                    value = c(3, 4, 4, 5), chroma = c(2, 4, 6, 8), clay = c(8, 9, 41, 47))
  expect_true(isTRUE(B_espodico(q)$passed))
})

test_that("no E above, a B under 2.5 cm, or a transition to the C: not the item", {
  expect_false(isTRUE(B_espodico(.esp_profile(designation = c("A1", "A2", "B", "C")))$passed))
  thin <- .esp_profile(top = c(0, 20, 60, 62), bottom = c(20, 60, 62, 150))
  expect_false(isTRUE(B_espodico(thin)$passed))
  expect_false(isTRUE(B_espodico(.esp_profile(designation = c("A", "E", "BC", "C")))$passed))
  # an E below the B is not above it
  below <- .esp_profile(designation = c("A", "B", "E", "C"), oc = c(1.2, 1.4, 0.1, 0.1),
                        value = c(3, 3, 7, 6), chroma = c(1, 2, 1, 3))
  expect_false(isTRUE(B_espodico(below)$passed))
})

test_that("the item is read in both modes, and the canonical profiles keep their horizons", {
  p <- .esp_profile()
  withr::with_options(list(soilKey.morphological_inference = TRUE), {
    expect_true(isTRUE(B_espodico(p)$passed))
    expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Espodossolos")
  })
  expect_true(isTRUE(B_espodico(make_espodossolo_canonical())$passed))
  expect_false(isTRUE(B_espodico(make_argissolo_canonical())$passed))
  expect_false(isTRUE(B_espodico(make_latossolo_canonical())$passed))
  expect_false(isTRUE(B_espodico(make_planossolo_canonical())$passed))
})
