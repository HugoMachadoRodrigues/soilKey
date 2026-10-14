# v0.9.238: the B planico is tested for the structure the book describes and
# for the two colour requirements that need more than the matrix colour.
#
# SiBCS 2018, Cap 2, p. 65-66. The B planico "Apresenta estrutura prismatica,
# colunar ou em blocos angulares e subangulares grandes ou medios e, as vezes,
# estrutura macica", and its colours "devem atender a pelo menos um dos
# seguintes requisitos":
#   a) the matrix colour: "Matiz 10YR ou mais amarelo, cromas <= 3" or
#      "Matizes 7,5YR ou 5YR, cromas <= 2";
#   b) "Coloracao variegada com pelo menos uma cor apresentando matiz e croma
#      conforme especificado no item (a)";
#   c) "matiz 10YR ou mais amarelo, cromas >= 4, combinado com mosqueado de
#      croma conforme especificado no item (a)".
# The key tested item (a) and no structure. A B horizon with an abrupt textural
# change and the right colour was a B planico whatever its structure, and a
# variegated B planico, which has no matrix colour, was none.

.bp <- function(...) {
  # the canonical Planossolo (A, E, Btn, C) with fields of its B set
  p <- make_planossolo_canonical()
  set <- list(...)
  b <- which(grepl("^B", p$horizons$designation))
  for (k in names(set)) p$horizons[[k]][b] <- set[[k]]
  p
}
.no_matrix <- list(munsell_hue_moist = NA_character_, munsell_value_moist = NA_real_,
                   munsell_chroma_moist = NA_real_)

test_that("the size of the structural units is read in both languages, a range by its lower end", {
  expect_equal(.structure_size_class(c("Muito pequena", "Pequena", "Media", "Grande", "Muito grande",
                                       "very fine", "fine", "medium", "coarse", "very coarse")),
               c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L))
  expect_equal(.structure_size_class(c("Média", "MEDIA", "Ultrapequena")), c(3L, 3L, 0L))
  expect_equal(.structure_size_class(c("Pequena a media", "Media a grande", "Muito pequena a pequena",
                                       "Grande a muito grande")), c(2L, 3L, 1L, 4L))
  expect_true(all(is.na(.structure_size_class(c(NA, "", "lixo")))))
})

test_that("the structures the definition describes", {
  ok <- function(type, size = NA_character_) .b_planico_structure_ok(type, size)
  expect_true(all(ok(c("Prismática", "Colunar", "Maciça", "prismatic", "columnar", "massive",
                       "Prismática que se desfaz"))))
  # blocks: medium or larger
  expect_true(all(ok(c("Blocos angulares", "Blocos subangulares", "subangular blocky", "angular blocky"),
                     c("Media", "Media a grande", "coarse", "medium"))))
  expect_false(any(ok(c("Blocos angulares", "Blocos subangulares", "subangular blocky"),
                      c("Pequena", "Pequena a media", "fine"))))
  # a size that was not recorded does not fail the blocks
  expect_true(ok("Blocos subangulares", NA_character_))
  # other recorded structures
  expect_false(any(ok(c("Granular", "Grãos simples", "Laminar", "granular", "platy", "single grain"))))
  # a structure that was not recorded passes
  expect_true(all(ok(c(NA_character_, ""))))
})

test_that("item (a) of the colours of the B planico", {
  a <- function(n, f, c) .b_planico_item_a(n, f, c)
  expect_true(all(a(c(10, 2.5, 5, 7.5, 5), c("YR", "Y", "Y", "YR", "YR"), c(3, 3, 1, 2, 2))))
  expect_false(any(a(c(7.5, 5, 10, 2.5, 10), c("YR", "YR", "YR", "YR", "R"), c(3, 3, 4, 2, 1))))
  # neutral and gley hues are not listed; a colour that is not recorded does not satisfy
  expect_false(any(a(c(NA, 5, 10, NA), c("N", "GY", "YR", NA), c(0, 1, NA, 2))))
})

test_that("the canonical Planossolo has a B planico by its matrix colour and its structure", {
  skip_on_cran()
  p <- make_planossolo_canonical()
  r <- B_planico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$cor_item, "a")
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Planossolos")
})

test_that("a B with small blocks, or with a structure that is not of the list, is no B planico", {
  skip_on_cran()
  for (s in list(c("Blocos subangulares", "Pequena a media"), c("Blocos angulares", "Pequena"),
                 c("subangular blocky", "fine"), c("granular", NA))) {
    p <- .bp(structure_type = s[1], structure_size = s[2])
    r <- B_planico(p)
    expect_false(isTRUE(r$passed), info = paste(s, collapse = " "))
    # the colour still qualifies; it is the structure that does not
    expect_true(length(r$evidence$cor_ok) > 0L)
    expect_false(identical(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Planossolos"))
  }
  for (s in list(c("Blocos subangulares", "Media a grande"), c("Blocos angulares", NA),
                 c("Colunar", "Pequena"), c("Maciça", NA), c(NA, NA))) {
    p <- .bp(structure_type = s[1], structure_size = s[2])
    expect_true(isTRUE(B_planico(p)$passed), info = paste(s, collapse = " "))
  }
})

test_that("item (b): a variegated B counts when one of its colours satisfies item (a)", {
  skip_on_cran()
  for (ok in c("10YR 6/3; 2.5YR 4/6", "2.5Y 7/1; 10YR 6/8", "5YR 5/2", "7.5YR 5/6; 10YR 5/1")) {
    r <- B_planico(do.call(.bp, c(.no_matrix, list(variegated_colors = ok))))
    expect_true(isTRUE(r$passed), info = ok)
    expect_equal(r$evidence$cor_item, "b", info = ok)
  }
  for (no in c("2.5YR 4/6; 5YR 5/6", "10YR 6/6; 10R 4/6", "7.5YR 5/4", "10YR 6/", "N 6/")) {
    r <- B_planico(do.call(.bp, c(.no_matrix, list(variegated_colors = no))))
    expect_false(isTRUE(r$passed), info = no)
  }
  # with no colour at all the B is no B planico, as before
  expect_false(isTRUE(B_planico(do.call(.bp, .no_matrix))$passed))
})

test_that("item (c): a yellowish matrix of chroma 4 or more, with a mottle that satisfies item (a)", {
  skip_on_cran()
  yellow <- function(mottles, hue = "10YR", chroma = 6)
    .bp(munsell_hue_moist = hue, munsell_value_moist = 6, munsell_chroma_moist = chroma,
        mottle_colors = mottles)
  r <- B_planico(yellow("10YR 6/1"))
  expect_true(isTRUE(r$passed)); expect_equal(r$evidence$cor_item, "c")
  expect_true(isTRUE(B_planico(yellow("10R 3/3; 2.5Y 7/2", hue = "2.5Y"))$passed))
  expect_true(isTRUE(B_planico(yellow("5YR 5/2", chroma = 4))$passed))
  # a mottle that does not satisfy item (a), or no mottle colour recorded
  expect_false(isTRUE(B_planico(yellow("5YR 6/8"))$passed))
  expect_false(isTRUE(B_planico(yellow("2.5YR 4/2"))$passed))
  expect_false(isTRUE(B_planico(yellow(NA_character_))$passed))
  # a matrix that is not 10YR or yellower
  expect_false(isTRUE(B_planico(yellow("10YR 6/1", hue = "7.5YR"))$passed))
  # a matrix that satisfies item (a) by itself is counted by item (a)
  r <- B_planico(yellow("10YR 6/1", chroma = 2))
  expect_equal(r$evidence$cor_item, "a")
})

test_that("the structure is asked of the variegated and of the mottled B too", {
  p <- do.call(.bp, c(.no_matrix, list(variegated_colors = "10YR 6/3; 2.5YR 4/6",
                                       structure_type = "Blocos subangulares",
                                       structure_size = "Pequena a media")))
  expect_false(isTRUE(B_planico(p)$passed))
  p$horizons$structure_size[grepl("^B", p$horizons$designation)] <- "Media"
  expect_true(isTRUE(B_planico(p)$passed))
})
