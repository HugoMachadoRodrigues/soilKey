# v0.9.227: B_nitico() reads the amount of cerosidade and the type of structure
# in the language of the field sheet.
#
# SiBCS 2018 (Cap 2, p. 61-62) asks of a B nitico "cerosidade em quantidade no
# minimo comum e com grau forte ou moderado" and "estrutura em blocos ou
# prismatica". The test compared `clay_films_amount` with the literal English
# terms c("common", "many", "abundant", "strong"), and matched the type of
# structure against "prismatica" without its accent. The BDsolos export words
# them as the Brazilian field sheets do ("Comum", "Abundante", "Prismática que
# se desfaz"), so the amount passed in none of its profiles: of the 7,694
# labelled profiles of the national export the key returned Nitossolos for
# none, although 145 of the 347 with that reference order record cerosidade
# "comum" or more and "moderada" or more.

.pt_terms <- function(p) {
  # the fixture's English terms, worded as a Brazilian field sheet words them
  pt <- function(x, map) {
    y <- as.character(x); k <- match(tolower(y), names(map))
    ifelse(is.na(k), y, unname(map[k]))
  }
  amount <- c(few = "Pouca", common = "Comum", many = "Abundante")
  grade  <- c(weak = "Fraca", moderate = "Moderada", strong = "Forte")
  type   <- c("subangular blocky" = "Blocos subangulares",
              "angular blocky" = "Blocos angulares",
              prismatic = "Prismática que se desfaz", granular = "Granular",
              massive = "Maciça", platy = "Laminar")
  q <- p$clone(deep = TRUE); h <- q$horizons
  if ("clay_films_amount" %in% names(h))   h$clay_films_amount   <- pt(h$clay_films_amount, amount)
  if ("clay_films_strength" %in% names(h)) h$clay_films_strength <- pt(h$clay_films_strength, grade)
  if ("structure_grade" %in% names(h))     h$structure_grade     <- pt(h$structure_grade, grade)
  if ("structure_type" %in% names(h))      h$structure_type      <- pt(h$structure_type, type)
  q$horizons <- h
  q
}

.with_amount <- function(p, amount, layers = NULL) {
  q <- p$clone(deep = TRUE)
  i <- if (is.null(layers)) which(!is.na(q$horizons$clay_films_amount)) else layers
  q$horizons$clay_films_amount[i] <- amount
  q
}

test_that("the readers of field terms know both languages", {
  expect_equal(.field_term(c("Prismática que se desfaz", "BLOCOS Subangulares",
                             "  Poliédrica ", NA)),
               c("prismatica que se desfaz", "blocos subangulares", "poliedrica", NA))
  amount <- c("Pouca", "Comum", "Abundante", "few", "common", "many", "abundant",
              "COMUM", "Muita", NA, "")
  expect_equal(.clay_films_class(.field_term(amount)),
               c(2L, 3L, 4L, 2L, 3L, 4L, 4L, 3L, 4L, NA, NA))
  # a range is read by its lower end
  expect_equal(.clay_films_class(.field_term(c("Pouca a comum", "Comum a abundante"))),
               c(2L, 3L))
})

test_that("a B nitico described in Portuguese is a B nitico", {
  en <- make_nitossolo_canonical()
  pt <- .pt_terms(en)
  expect_true(all(c("Comum", "Abundante") %in% pt$horizons$clay_films_amount))
  expect_true(any(grepl("Prismática|Blocos", pt$horizons$structure_type)))
  expect_true(isTRUE(B_nitico(en)$passed))
  b <- B_nitico(pt)
  expect_true(isTRUE(b$passed))
  expect_true(b$evidence$cerosidade_ok)
  expect_true(b$evidence$structure_ok)
  expect_equal(b$layers, B_nitico(en)$layers)
  expect_equal(classify_sibcs(pt, on_missing = "silent")$rsg_or_order, "Nitossolos")
})

test_that("case and accents do not matter, the amount does", {
  p <- make_nitossolo_canonical()
  for (ok in c("COMUM", "comum", "Abundante", "Common", "MANY", "abundant",
               "Comum a abundante", "Muita"))
    expect_true(isTRUE(B_nitico(.with_amount(p, ok))$passed), info = ok)
  # below "comum": the criterion is not met, in either language
  for (low in c("Pouca", "pouca", "few", "Pouca a comum", "Muito pouca", "Ausente"))
    expect_false(isTRUE(B_nitico(.with_amount(p, low))$evidence$cerosidade_ok), info = low)
  # not recorded, or a word that says nothing of the amount
  expect_false(isTRUE(B_nitico(.with_amount(p, NA_character_))$evidence$cerosidade_ok))
  expect_false(isTRUE(B_nitico(.with_amount(p, "presente"))$evidence$cerosidade_ok))
})

test_that("the type of structure is matched with or without its accent", {
  p <- make_nitossolo_canonical()
  b <- which(grepl("^B", p$horizons$designation))
  for (type in c("Prismática", "Prismática que se desfaz", "prismatica",
                 "Blocos subangulares", "BLOCOS ANGULARES", "Poliédrica", "prismatic")) {
    q <- p$clone(deep = TRUE); q$horizons$structure_type[b] <- type
    expect_true(isTRUE(B_nitico(q)$evidence$structure_ok), info = type)
  }
  for (type in c("Granular", "Maciça", "Grãos simples", "Colunar")) {
    q <- p$clone(deep = TRUE); q$horizons$structure_type[b] <- type
    expect_false(isTRUE(B_nitico(q)$evidence$structure_ok), info = type)
  }
})

test_that("`min_cerosidade` sets the threshold by its lowest term", {
  p <- make_nitossolo_canonical()
  comum <- .with_amount(p, "Comum"); abund <- .with_amount(p, "Abundante")
  # the default threshold is "common"
  expect_true(B_nitico(comum)$evidence$cerosidade_ok)
  # a stricter list: "many" and above
  expect_false(B_nitico(comum, min_cerosidade = c("many", "abundant"))$evidence$cerosidade_ok)
  expect_true(B_nitico(abund, min_cerosidade = c("many", "abundant"))$evidence$cerosidade_ok)
  # a looser one
  expect_true(B_nitico(.with_amount(p, "Pouca"), min_cerosidade = "few")$evidence$cerosidade_ok)
  # a term the scale does not know passes only when it is listed, as before
  expect_true(B_nitico(.with_amount(p, "strong"))$evidence$cerosidade_ok)
  expect_false(B_nitico(.with_amount(p, "strong"),
                        min_cerosidade = "many")$evidence$cerosidade_ok)
  expect_false(B_nitico(comum, min_cerosidade = "strong")$evidence$cerosidade_ok)
})

test_that("the order of a canonical profile does not depend on the language of its terms", {
  skip_on_cran()
  fx <- grep("^make_.*_canonical$", ls(asNamespace("soilKey")), value = TRUE)
  expect_gte(length(fx), 44L)
  for (f in fx) {
    p <- get(f, envir = asNamespace("soilKey"))()
    expect_equal(classify_sibcs(.pt_terms(p), on_missing = "silent")$rsg_or_order,
                 classify_sibcs(p, on_missing = "silent")$rsg_or_order, info = f)
  }
})
