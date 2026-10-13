# v0.9.239: the glei horizon is read by the colours of the book, with the
# drainage class of the site as the record of its saturation, and the steps of
# the key that use it follow the depths of the book.
#
# SiBCS 2018, Cap 2, p. 70-71: the horizonte glei is mineral, 15 cm thick or
# more, with less than 15% of plinthite, "saturado com agua por influencia do
# lencol freatico durante algum periodo ou o ano todo", with colours of
# requirement (a), items 1 to 4, or (b), a variegated layer with one colour of
# those items. Until v0.9.238 it was the WRB test of gleyic properties: 5% or
# more of redoximorphic features within 50 cm, whatever the colour. A mottled
# Argissolo was a Gleissolo, and a grey Cg with no mottle amount was not.
#
# The saturation is read from the drainage class (Anexo B, p. 315: in a "mal
# drenado" soil "o lencol freatico comumente esta a superficie ou proximo
# dela"). Where no class is recorded, the layer has to show it: mottles, a
# neutral, bluish or greenish colour, or a variegated colour. Key, Cap 4: Gleissolos have the glei horizon within 50 cm, or
# between 50 and 150 cm "desde que imediatamente subjacente a horizontes A ou
# E" (p. 112-113); Neossolos have no glei horizon "dentro de 50 cm a partir
# da superficie, exceto no caso de solos de textura areia e areia franca"
# (p. 111).

.gl_morph <- function(on) options(soilKey.morphological_inference = on)

.gl_profile <- function(designation, top, bottom, hue, value, chroma, drainage = NA_character_,
                        clay = 30, silt = 30, ...) {
  n <- length(designation)
  h <- make_empty_horizons(n)
  h$designation <- designation; h$top_cm <- top; h$bottom_cm <- bottom
  h$munsell_hue_moist <- hue; h$munsell_value_moist <- value; h$munsell_chroma_moist <- chroma
  h$clay_pct <- rep_len(clay, n); h$silt_pct <- rep_len(silt, n); h$sand_pct <- 100 - h$clay_pct - h$silt_pct
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  PedonRecord$new(site = list(id = "glei-test", drainage = drainage), horizons = h)
}

test_that("the drainage class is read in both languages, a range by the end asked for", {
  pt <- c("Excessivamente drenado", "Fortemente drenado", "Acentuadamente drenado", "Bem drenado",
          "Moderadamente drenado", "Imperfeitamente drenado", "Mal drenado", "Muito mal drenado")
  expect_equal(vapply(pt, .drainage_class, 0L, USE.NAMES = FALSE), 1:8)
  en <- c("excessively drained", "somewhat excessively drained", "well drained",
          "moderately well drained", "somewhat poorly drained", "imperfectly drained",
          "poorly drained", "very poorly drained", "poorly drained (high groundwater)")
  expect_equal(vapply(en, .drainage_class, 0L, USE.NAMES = FALSE), c(1L, 2L, 4L, 5L, 6L, 6L, 7L, 8L, 7L))
  # a range: the drier end by default, the wetter one when asked
  expect_equal(.drainage_class("Imperfeitamente a mal drenado"), 6L)
  expect_equal(.drainage_class("Imperfeitamente a mal drenado", end = "wetter"), 7L)
  expect_equal(.drainage_class("Mal drenado a muito mal drenado"), 7L)
  expect_equal(.drainage_class("Mal drenado a muito mal drenado", end = "wetter"), 8L)
  expect_equal(.drainage_class("Imperfeitamente a moderadamente drenado", end = "wetter"), 6L)
  expect_true(is.na(.drainage_class(NA_character_)))
  expect_true(is.na(.drainage_class("")))
  expect_true(is.na(.drainage_class("lixo")))
})

test_that("the items of requirement (a)", {
  it <- function(hue, value, chroma, ...) {
    p <- .munsell_hue_parts(hue)
    .glei_colour_item(p$number, p$family, value, chroma, ...)
  }
  # 1) neutral, or bluer than 10Y
  expect_equal(it("N", 5, NA), "a1")
  expect_equal(it("5GY", 5, 1), "a1")
  expect_equal(it("5BG", 4, 1), "a1")
  # 2) redder than 5YR, value 4 or more: chroma 1 or less
  expect_equal(it("2.5YR", 5, 1), "a2")
  expect_true(is.na(it("2.5YR", 5, 2)))
  expect_true(is.na(it("2.5YR", 3, 1)))
  # 3) 5YR or yellower, value 4 or more: chroma 2 or less
  expect_equal(it("10YR", 6, 2), "a3")
  expect_equal(it("5YR", 4, 2), "a3")
  expect_equal(it("2.5Y", 5, 1), "a3")
  expect_true(is.na(it("10YR", 6, 3)))
  expect_true(is.na(it("10YR", 3, 2)))
  #    chroma 3 in a hue 10YR or yellower, when it decreases in the horizon below
  expect_equal(it("10YR", 6, 3, chroma_below = 2), "a3")
  expect_true(is.na(it("10YR", 6, 3, chroma_below = 3)))
  expect_true(is.na(it("7.5YR", 6, 3, chroma_below = 1)))
  # 4) any hue and value, chroma 2 or less, with mottles
  expect_equal(it("10YR", 3, 2, mottled = TRUE), "a4")
  expect_equal(it("2.5YR", 3, 2, mottled = TRUE), "a4")
  expect_true(is.na(it("10YR", 3, 3, mottled = TRUE)))
  # a colour that is not recorded
  expect_true(is.na(.glei_colour_item(NA_real_, NA_character_, 5, 1)))
  expect_true(is.na(it("10YR", 5, NA)))
})

test_that("the canonical Gleissolo has a glei horizon by its colours and its drainage", {
  p <- make_gleissolo_canonical()
  g <- horizonte_glei(p)
  expect_true(isTRUE(g$passed))
  expect_true(g$evidence$drainage$saturated)
  expect_equal(g$evidence$drainage$class, 7L)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Gleissolos")
})

test_that("the colours count when the site is mal or muito mal drenado", {
  grey <- function(drainage) .gl_profile(c("A", "Cg1", "Cg2"), c(0, 20, 60), c(20, 60, 120),
                                         c("10YR", "2.5Y", "5Y"), c(3, 6, 6), c(2, 1, 1), drainage)
  for (d in c("Mal drenado", "Muito mal drenado", "Imperfeitamente a mal drenado",
              "Mal drenado a muito mal drenado", "poorly drained"))
    expect_true(isTRUE(horizonte_glei(grey(d))$passed), info = d)
  for (d in c("Imperfeitamente drenado", "Imperfeitamente a moderadamente drenado",
              "Moderadamente drenado", "Bem drenado", "well drained"))
    expect_false(isTRUE(horizonte_glei(grey(d))$passed), info = d)
  # the layers and their items are reported either way
  g <- horizonte_glei(grey("Bem drenado"))
  expect_equal(g$evidence$colour_layers, 2:3)
  expect_equal(g$evidence$colour_items, c("a3", "a3"))
  expect_false(g$evidence$drainage$saturated)
})

test_that("with no drainage class recorded, the layer itself has to show the saturation", {
  mk <- function(hue = "2.5Y", chroma = 1, ...)
    .gl_profile(c("A", "C1", "C2"), c(0, 20, 60), c(20, 60, 120), c("10YR", hue, hue),
                c(3, 6, 6), c(3, chroma, chroma), NA_character_, ...)
  # grey by item (a3), and nothing else: a pale layer of any soil
  g <- horizonte_glei(mk())
  expect_false(isTRUE(g$passed))
  expect_true(is.na(g$evidence$drainage$saturated))
  expect_equal(g$evidence$colour_layers, 2:3)
  # the same site-level class recorded as poor makes it a glei horizon
  p <- mk(); p$site$drainage <- "Mal drenado"
  expect_true(isTRUE(horizonte_glei(p)$passed))
  # mottles in the layer, by their amount or by their colours
  expect_true(isTRUE(horizonte_glei(mk(redoximorphic_features_pct = c(NA, 10, 10)))$passed))
  expect_true(isTRUE(horizonte_glei(mk(mottle_colors = c(NA, "7.5YR 5/8", "7.5YR 5/8")))$passed))
  # a neutral, bluish or greenish colour (item a1)
  expect_true(isTRUE(horizonte_glei(mk(hue = "5GY"))$passed))
  expect_true(isTRUE(horizonte_glei(mk(hue = "N", chroma = NA_real_))$passed))
  # a variegated layer with one colour of the items (item b)
  v <- .gl_profile(c("A", "C1"), c(0, 20), c(20, 120), c("10YR", NA), c(3, NA), c(3, NA), NA_character_,
                   variegated_colors = c(NA, "2.5Y 7/1; 10YR 6/8"))
  expect_true(isTRUE(horizonte_glei(v)$passed))
  # the field is read under either of its names
  q <- mk(); q$site$drainage <- NULL; q$site$drainage_class <- "poorly drained"
  expect_true(isTRUE(horizonte_glei(q)$passed))
  q$site$drainage_class <- "well drained"
  expect_false(isTRUE(horizonte_glei(q)$passed))
})

test_that("mottles without the colours of reduction are no glei horizon", {
  # 30% of mottles in a red Bt: a glei horizon until v0.9.238
  p <- .gl_profile(c("A", "Bt1", "Bt2"), c(0, 20, 60), c(20, 60, 150), c("10YR", "5YR", "2.5YR"),
                   c(3, 4, 4), c(3, 6, 6), "Mal drenado",
                   redoximorphic_features_pct = c(NA, 30, 30))
  expect_false(isTRUE(horizonte_glei(p)$passed))
  # with a chroma of 2 the mottles make it item 4, whatever the hue and the value
  p$horizons$munsell_chroma_moist[2:3] <- 2
  p$horizons$munsell_value_moist[2:3] <- 3
  g <- horizonte_glei(p)
  expect_true(isTRUE(g$passed)); expect_equal(g$evidence$colour_items, c("a4", "a4"))
  # and without the mottles a dark layer of chroma 2 is none
  p$horizons$redoximorphic_features_pct <- NA_real_
  expect_false(isTRUE(horizonte_glei(p)$passed))
})

test_that("a pale E of a well drained soil is no glei horizon", {
  p <- .gl_profile(c("A", "E", "Bt"), c(0, 20, 60), c(20, 60, 150), c("10YR", "10YR", "5YR"),
                   c(4, 6, 4), c(3, 2, 6), "Bem drenado")
  expect_false(isTRUE(horizonte_glei(p)$passed))
})

test_that("a variegated layer counts by one colour of the items; plinthite and organic layers do not", {
  p <- .gl_profile(c("A", "Bg", "C"), c(0, 20, 70), c(20, 70, 150), c("10YR", NA, "5YR"),
                   c(3, NA, 4), c(2, NA, 6), "Mal drenado",
                   variegated_colors = c(NA, "2.5Y 7/1; 10YR 6/8", NA))
  g <- horizonte_glei(p)
  expect_true(isTRUE(g$passed)); expect_equal(g$evidence$colour_items, "b")
  p$horizons$variegated_colors[2] <- "10YR 6/8; 2.5YR 4/6"
  expect_false(isTRUE(horizonte_glei(p)$passed))
  # 15% of plinthite or more: not a glei layer
  q <- .gl_profile(c("A", "Cg"), c(0, 20), c(20, 100), c("10YR", "2.5Y"), c(3, 6), c(2, 1), "Mal drenado",
                   plinthite_pct = c(NA, 20))
  expect_false(isTRUE(horizonte_glei(q)$passed))
  # an organic layer is not a mineral horizon
  r <- .gl_profile(c("H", "C"), c(0, 30), c(30, 100), c("N", "5YR"), c(2, 4), c(NA, 6), "Muito mal drenado")
  expect_false(isTRUE(horizonte_glei(r)$passed))
})

test_that("a glei layer is 15 cm thick or more", {
  thin <- .gl_profile(c("A", "Cg", "C"), c(0, 20, 30), c(20, 30, 120), c("10YR", "2.5Y", "5YR"),
                      c(3, 6, 4), c(3, 1, 6), "Mal drenado")
  g <- horizonte_glei(thin)
  expect_equal(g$evidence$colour_layers, 2L)
  expect_false(isTRUE(g$passed))
  thin$horizons$bottom_cm[2] <- 35; thin$horizons$top_cm[3] <- 35
  expect_true(isTRUE(horizonte_glei(thin)$passed))
})

test_that("a chroma of 3 counts in a hue 10YR or yellower when the horizon below has less", {
  # "admitindo-se, para solos de matiz dominante 10YR ou mais amarelo, croma
  # 3, que devera diminuir no horizonte subjacente"
  p <- .gl_profile(c("A", "Cg"), c(0, 25), c(25, 120), c("10YR", "2.5Y"), c(5, 6), c(3, 1), "Mal drenado")
  g <- horizonte_glei(p)
  expect_equal(g$evidence$colour_layers, 1:2)
  p$horizons$munsell_chroma_moist[2] <- 3
  expect_equal(horizonte_glei(p)$evidence$colour_layers, integer(0))
})

test_that("in the morphological mode the 'g' of a designation stands for the colours and the saturation", {
  old <- options(soilKey.morphological_inference = FALSE); on.exit(options(old), add = TRUE)
  p <- .gl_profile(c("A", "Cg1", "Cg2"), c(0, 20, 60), c(20, 60, 120), rep(NA_character_, 3),
                   rep(NA_real_, 3), rep(NA_real_, 3), NA_character_)
  g <- horizonte_glei(p)
  expect_false(isTRUE(g$passed))
  expect_equal(g$missing, "munsell_hue_moist")
  .gl_morph(TRUE)
  g <- horizonte_glei(p)
  expect_true(isTRUE(g$passed)); expect_equal(g$evidence$designation_g, 2:3)
  # with the number of a material in front too
  p$horizons$designation <- c("A", "2Cg1", "IICg2")
  expect_true(isTRUE(horizonte_glei(p)$passed))
})

test_that("Gleissolos: the glei horizon within 50 cm, or to 150 cm right below an A or an E", {
  mk <- function(des, top, bottom) {
    n <- length(des); grey <- grepl("g", des)
    .gl_profile(des, top, bottom, ifelse(grey, "2.5Y", "7.5YR"), ifelse(grey, 6, 4), ifelse(grey, 1, 6),
                "Mal drenado", clay = 35)
  }
  expect_true(isTRUE(gleissolo(mk(c("A", "Cg1", "Cg2"), c(0, 30, 80), c(30, 80, 150)))$passed))
  # at 70 cm right below an A, or an E
  expect_true(isTRUE(gleissolo(mk(c("A1", "A2", "Cg"), c(0, 30, 70), c(30, 70, 150)))$passed))
  expect_true(isTRUE(gleissolo(mk(c("A", "E", "Btg"), c(0, 30, 70), c(30, 70, 150)))$passed))
  # at 70 cm below a B or a C: not a Gleissolo
  expect_false(isTRUE(gleissolo(mk(c("A", "Bw", "Cg"), c(0, 30, 70), c(30, 70, 150)))$passed))
  expect_false(isTRUE(gleissolo(mk(c("A", "C1", "Cg"), c(0, 30, 70), c(30, 70, 150)))$passed))
  # below 150 cm
  expect_false(isTRUE(gleissolo(mk(c("A1", "A2", "Cg"), c(0, 60, 160), c(60, 160, 220)))$passed))
})

test_that("Neossolos: no glei horizon within 50 cm, unless the texture is areia or areia franca", {
  mk <- function(top_g, clay, silt) .gl_profile(c("A", "C", "Cg"), c(0, 20, top_g), c(20, top_g, 200),
                                                c("10YR", "10YR", "2.5Y"), c(4, 5, 6), c(3, 4, 1),
                                                "Mal drenado", clay = clay, silt = silt)
  # a glei horizon at 30 cm excludes the Neossolos; at 80 cm it does not
  expect_false(isTRUE(neossolo(mk(30, 30, 30))$passed))
  expect_true(isTRUE(neossolo(mk(80, 30, 30))$passed))
  expect_true(neossolo(mk(30, 30, 30))$evidence$glei_dentro_50)
  expect_false(neossolo(mk(80, 30, 30))$evidence$glei_dentro_50)
  # areia (silt + 2 clay < 30): the glei horizon at 30 cm does not exclude it
  expect_true(isTRUE(neossolo(mk(30, 4, 5))$passed))
  expect_true(.textura_areia_ou_areia_franca(mk(30, 4, 5)$horizons))
  expect_true(.textura_areia_ou_areia_franca(mk(30, 8, 10)$horizons))     # areia franca
  expect_false(.textura_areia_ou_areia_franca(mk(30, 12, 10)$horizons))   # franco-arenosa
  # no texture recorded: no exception
  h <- mk(30, 4, 5)$horizons; h$clay_pct <- NA_real_
  expect_false(.textura_areia_ou_areia_franca(h))
})

test_that("a mottled Argissolo with good drainage is keyed Argissolos again", {
  p <- make_argissolo_canonical()
  p$horizons$redoximorphic_features_pct <- c(NA, 10, 30, 30)
  expect_false(isTRUE(horizonte_glei(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Argissolos")
})
