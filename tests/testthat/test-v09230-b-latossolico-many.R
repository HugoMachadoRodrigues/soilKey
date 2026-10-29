# v0.9.230: the clay-film gate of the B latossolico reads "many".
#
# A B latossolico gives way to a B textural when the layers with the clay
# increase have cerosidade "comum" or "abundante" (SiBCS Cap 18, wired into
# B_latossolico() since v0.9.61). The gate read "comum", "abundante", "common"
# and "abundant", but not "many": the term the English field sheets (KST, FAO)
# use for the class above "common", and the one the Redape loader writes. A
# profile whose films were "many" kept its B latossolico and keyed out as
# Latossolos: four of the 94 Redape profiles, all with reference Argissolos.
#
# v0.9.241: B_latossolico() no longer uses that gate. What these tests show
# holds by the book: with cerosidade common or more, a clayey B of blocky
# structure of moderate or strong grade is a B textural (item i3), and a
# layer with more than "pouca e fraca" is no B latossolico (item h, p. 59).
# .argic_strong_films_match() and the audit are kept as tools.

.films_profile <- function(films) {
  # the canonical Latossolo with the clay increase of a B textural (B/A above
  # 1.5, so that it is no B nitico) and the given cerosidade in its B
  p <- make_latossolo_canonical()
  p$horizons$clay_pct <- c(35, 38, 52, 58, 58)
  p$horizons$clay_films_amount <- as.character(c(NA, NA, films, films, films))
  p
}

test_that("'many' is a strong amount of clay films, as 'common' and 'abundant' are", {
  skip_on_cran()
  for (strong in c("many", "Many", "MANY", "very many", "many (60%)",
                   "common", "abundant", "comum", "Abundante"))
    expect_true(.argic_strong_films_match(strong), info = strong)
  expect_true(.argic_strong_films_match(c("few", "many")))
  for (weak in c("few", "very few", "weak", "pouca", "fraca", "absent", "none", ""))
    expect_false(.argic_strong_films_match(weak), info = weak)
  # the word, not four letters inside another one
  expect_false(.argic_strong_films_match("germany"))
  expect_false(.argic_strong_films_match(NA_character_))
})

test_that("clay films 'many' in the B textural exclude the B latossolico", {
  skip_on_cran()
  few <- .films_profile("few")
  # the clay increase as the SiBCS B textural reads it (v0.9.310: the WRB argic
  # asks for a transition of <= 15 cm, criterion 2.a.iii, and the AB is 20 cm)
  expect_true(isTRUE(.argic_impl(few, lithic_discontinuity = FALSE)$passed))
  expect_false(isTRUE(argic_with_strong_clay_films(few)$passed))
  expect_true(isTRUE(B_latossolico(few)$passed))
  expect_equal(classify_sibcs(few, on_missing = "silent")$rsg_or_order, "Latossolos")
  for (strong in c("many", "common", "abundant", "Comum", "Abundante")) {
    p <- .films_profile(strong)
    expect_false(isTRUE(B_nitico(p)$passed), info = strong)   # the gate decides, not B nitico
    expect_true(isTRUE(argic_with_strong_clay_films(p)$passed), info = strong)
    expect_false(isTRUE(B_latossolico(p)$passed), info = strong)
    expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Argissolos",
                 info = strong)
  }
  # no films recorded: nothing to exclude it
  none <- .films_profile(NA_character_)
  expect_true(isTRUE(B_latossolico(none)$passed))
})

test_that("the audit of the gate counts 'many' too", {
  skip_on_cran()
  p <- .films_profile("many")
  p$site$reference_sibcs <- "ARGISSOLO VERMELHO Distrófico típico"
  a <- audit_argic_strong_films(list(p))
  expect_true(a$strong_films_at_argic)
})
