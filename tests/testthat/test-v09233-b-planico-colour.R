# v0.9.233: the colour of the B planico is the one the book lists.
#
# SiBCS 2018 (Cap 2, p. 65-66), requirement (a): "Matiz 10YR ou mais amarelo,
# cromas <= 3" or "Matizes 7,5YR ou 5YR, cromas <= 2". B_planico() tested the
# first with the pattern "10YR|2\\.5Y|5Y", which "5YR", "7.5YR" and "2.5YR"
# also match, because all three contain "5Y". A B horizon of hue 5YR or 7.5YR
# passed with chroma 3, and one of hue 2.5YR passed at all.

.planico_colour <- function(hue, chroma) {
  p <- make_planossolo_canonical()
  b <- which(grepl("^B", p$horizons$designation))
  p$horizons$munsell_hue_moist[b] <- hue
  p$horizons$munsell_chroma_moist[b] <- chroma
  isTRUE(B_planico(p)$passed)
}

test_that(".munsell_hue_parts() reads the number and the family of a hue", {
  expect_equal(.munsell_hue_parts("7.5YR"), list(number = 7.5, family = "YR"))
  expect_equal(.munsell_hue_parts("7,5YR"), list(number = 7.5, family = "YR"))
  expect_equal(.munsell_hue_parts(" 2.5 y "), list(number = 2.5, family = "Y"))
  expect_equal(.munsell_hue_parts("10R"), list(number = 10, family = "R"))
  expect_equal(.munsell_hue_parts("5GY"), list(number = 5, family = "GY"))
  expect_equal(.munsell_hue_parts("N"), list(number = NA_real_, family = "N"))
  for (not_a_hue in list("10", "2,5", "", NA, "amarelo"))
    expect_true(is.na(.munsell_hue_parts(not_a_hue)$family))
})

test_that("10YR or yellower passes with chroma 3 or less", {
  skip_on_cran()
  expect_true(isTRUE(B_planico(make_planossolo_canonical())$passed))
  for (hue in c("10YR", "2.5Y", "2,5Y", "5Y", "10Y"))
    for (chroma in c(1, 2, 3)) expect_true(.planico_colour(hue, chroma), info = paste(hue, chroma))
  for (hue in c("10YR", "2.5Y", "5Y")) expect_false(.planico_colour(hue, 4), info = hue)
})

test_that("7.5YR and 5YR pass with chroma 2 or less, and no longer with 3", {
  skip_on_cran()
  for (hue in c("7.5YR", "7,5YR", "5YR")) {
    expect_true(.planico_colour(hue, 2), info = hue)
    expect_true(.planico_colour(hue, 1), info = hue)
    expect_false(.planico_colour(hue, 3), info = hue)
  }
})

test_that("a hue redder than 5YR does not pass", {
  skip_on_cran()
  for (hue in c("2.5YR", "2,5YR", "10R", "5R"))
    for (chroma in c(1, 2, 3)) expect_false(.planico_colour(hue, chroma), info = paste(hue, chroma))
})

test_that("a colour that was not recorded, or is no hue of the list, does not pass", {
  skip_on_cran()
  expect_false(.planico_colour(NA_character_, 2))
  expect_false(.planico_colour("10YR", NA_real_))
  expect_false(.planico_colour("10", 2))
  # neutral and gley hues are not among the requirements the book lists
  expect_false(.planico_colour("N", 0))
  expect_false(.planico_colour("5GY", 1))
})
