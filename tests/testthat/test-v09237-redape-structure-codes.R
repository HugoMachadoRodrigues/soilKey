# v0.9.237: the structure codes of the GeoTab files and of the SmartSolos
# service are read on the scales the service documents.
#
# The SmartSolos Expert API describes its fields
# (https://www.agroapi.cnptia.embrapa.br/store/api-docs/agroapi/SmartSolosExpert/v1):
#   ESTRUTURA_GRAU     "de ausente (1) a forte (4)"
#   ESTRUTURA_TAMANHO  "de muito pequeno (1) a extremamente grande (6)"
#   ESTRUTURA_TIPO     "laminar (1), prismatica (2), colunar (3), blocos
#                      angulares (4), blocos subangulares (5), granular (6),
#                      grumosa (7)"
# Since v0.9.189 the Redape loader read the grade on a scale of three ("weak"
# for 1) and the type in another order ("granular" for 1, "columnar" for 5): a
# weak structure in subangular blocks (2, 5) reached the key as a moderate
# columnar one, and a strong grade (4) was dropped. The encoders that send a
# profile to the service made the same two mistakes the other way round. The
# profiles that the BDsolos export also has agree with the documented scales,
# horizon by horizon.

.rs_hz <- function(...) .redape_horizon_to_soilkey(
  c(list(SIMB_HORIZ = "Bt", LIMITE_SUP = 40, LIMITE_INF = 90), list(...)))

test_that("the type of structure is decoded in the documented order", {
  type <- vapply(1:7, function(k) .rs_hz(ESTRUTURA_TIPO = k)$structure_type, "")
  expect_equal(type, c("platy", "prismatic", "columnar", "angular blocky",
                       "subangular blocky", "granular", "crumb"))
  expect_true(is.na(.rs_hz(ESTRUTURA_TIPO = 8L)$structure_type))
  expect_true(is.na(.rs_hz()$structure_type))
})

test_that("the grade is decoded from absent (1) to strong (4)", {
  grade <- vapply(1:4, function(k) .rs_hz(ESTRUTURA_GRAU = k)$structure_grade, "")
  expect_equal(grade, c("structureless", "weak", "moderate", "strong"))
  expect_true(is.na(.rs_hz(ESTRUTURA_GRAU = 5L)$structure_grade))
  # the text "undefined", which one file has, is no grade
  expect_true(is.na(.rs_hz(ESTRUTURA_GRAU = "undefined")$structure_grade))
})

test_that("the size is decoded up to extremely coarse (6)", {
  size <- vapply(1:6, function(k) .rs_hz(ESTRUTURA_TAMANHO = k)$structure_size, "")
  expect_equal(size, c("very fine", "fine", "medium", "coarse", "very coarse",
                       "extremely coarse"))
})

test_that("the clay films are decoded as before", {
  # CEROSIDADE_GRAU stays on its scale of three: the files hold 1, 2 and 3
  # only, and 1 comes with an amount, which "ausente" would not
  expect_equal(vapply(1:3, function(k) .rs_hz(CEROSIDADE_GRAU = k)$clay_films_strength, ""),
               c("weak", "moderate", "strong"))
  expect_equal(vapply(1:3, function(k) .rs_hz(CEROSIDADE_QUANTIDADE = k)$clay_films_amount, ""),
               c("few", "common", "many"))
})

test_that("what the loader decodes, the encoder sends back as the same code", {
  for (k in 1:7) expect_equal(.smartsolos_struct_type(.rs_hz(ESTRUTURA_TIPO = k)$structure_type), k)
  for (k in 1:4) expect_equal(.smartsolos_struct_grade(.rs_hz(ESTRUTURA_GRAU = k)$structure_grade), k)
  for (k in 1:6) expect_equal(.smartsolos_struct_size(.rs_hz(ESTRUTURA_TAMANHO = k)$structure_size), k)
  # the Portuguese terms of a BDsolos profile go to the same codes
  expect_equal(.smartsolos_struct_type("Blocos subangulares"), 5L)
  expect_equal(.smartsolos_struct_type("Blocos angulares"), 4L)
  expect_equal(.smartsolos_struct_type("Prismática"), 2L)
  expect_equal(.smartsolos_struct_type("Laminar"), 1L)
  expect_equal(.smartsolos_struct_type("Grumosa"), 7L)
})

test_that("a structure recorded as absent is a grade below weak, not an unrecorded one", {
  expect_equal(.field_grade_class(c("structureless", "absent", "Ausente", "Sem estrutura")),
               rep(0L, 4L))
  expect_true(is.na(.field_grade_class(NA_character_)))
  # in the B nitico, an unrecorded grade does not fail the layer; an absent one does
  p <- make_nitossolo_canonical()
  b <- which(grepl("^B", p$horizons$designation))
  q <- p$clone(deep = TRUE); q$horizons$structure_grade[b] <- NA_character_
  expect_true(B_nitico(q)$evidence$structure_ok)
  q$horizons$structure_grade[b] <- "structureless"
  expect_false(B_nitico(q)$evidence$structure_ok)
})

test_that("a weak structure in subangular blocks is read as that", {
  # codes 2 and 5: until v0.9.236 this horizon was "moderate" and "columnar"
  h <- .rs_hz(ESTRUTURA_GRAU = 2L, ESTRUTURA_TIPO = 5L, ESTRUTURA_TAMANHO = 2L)
  expect_equal(h$structure_grade, "weak")
  expect_equal(h$structure_type, "subangular blocky")
  expect_equal(h$structure_size, "fine")
})
