# v0.9.251: the cerosidade written in the text of a horizon description.
#
# The BDsolos export has the cerosidade in two structured columns (amount and
# grade) and, as the survey wrote it, in the text of the horizon ("Descricao
# Original"). In 142 horizons the two columns are empty and the text names
# it. SiBCS asks for both: the B textural by its cerosidade (Cap 2, p. 56,
# item i), the B nitico "em quantidade no minimo comum e com grau forte ou
# moderado" (p. 62), the B latossolico "no maximo pouca e fraca" (p. 59).
# The loader reads them from the text where the cells are empty, by the rule
# decided on 2026-10-13 for the slickensides: what is written beside the
# term.

test_that("the amount and the grade are those written beside the term", {
  rd <- function(x) { r <- .bdsolos_films_reads(x); c(r$amount, r$grade) }
  expect_equal(rd("argila; cerosidade comum e moderada; firme"), c("Comum", "Moderada"))
  expect_equal(rd("cerosidade fraca e pouca"), c("Pouca", "Fraca"))
  expect_equal(rd("cerosidade pouca e fraca"), c("Pouca", "Fraca"))
  expect_equal(rd("cerosidade abundante e forte"), c("Abundante", "Forte"))
  expect_equal(rd("cerosidade forte e abundante"), c("Abundante", "Forte"))
  expect_equal(rd("cerosidade muita e moderada"), c("Abundante", "Moderada"))
  expect_equal(rd("cerosidade moderada comum em pontos localizados"), c("Comum", "Moderada"))
  expect_equal(rd("cerosidade comum"), c("Comum", NA))
  expect_equal(rd("cerosidade fraca"), c(NA, "Fraca"))
  expect_equal(rd("pouca cerosidade"), c("Pouca", NA))
})

test_that("two amounts or two grades are a range", {
  rd <- function(x) { r <- .bdsolos_films_reads(x); c(r$amount, r$grade) }
  expect_equal(rd("cerosidade fraca a moderada e comum"), c("Comum", "Fraca a moderada"))
  expect_equal(rd("cerosidade moderada e forte nas superfícies verticais"), c(NA, "Moderada a forte"))
  expect_equal(rd("cerosidade pouca a comum e fraca"), c("Pouca a comum", "Fraca"))
})

test_that("'muito' qualifies the word after it, and counts alone as an amount", {
  rd <- function(x) { r <- .bdsolos_films_reads(x); c(r$amount, r$grade) }
  expect_equal(rd("cerosidade muito pouca e fraca"), c("Pouca", "Fraca"))
  expect_equal(rd("cerosidade muito forte e comum"), c("Comum", "Forte"))
  # the "muito" of the consistence that follows is none of it
  expect_equal(rd("cerosidade comum e moderada muito firme"), c("Comum", "Moderada"))
})

test_that("a bare mention, a denial and words of something else give nothing", {
  rd <- function(x) { r <- .bdsolos_films_reads(x); c(r$amount, r$grade) }
  none <- c(NA_character_, NA_character_)
  expect_equal(rd("presença de cerosidade"), none)
  expect_equal(rd("cerosidade incipiente"), none)
  expect_equal(rd("estrutura fraca média blocos subangulares sem cerosidade e muito poroso"), none)
  expect_equal(rd("observação: não foi observada cerosidade"), none)
  # the amount of the mottles, after a parenthesis
  expect_equal(rd("bruno-amarelado (10YR 5/4) cor da cerosidade (10YR 4/4) com mosqueado pouco"), none)
  expect_equal(rd("mosqueado (proveniente de cerosidade) abundante"), none)
  expect_equal(rd("cerosidade em algumas partes do subhorizonte"), none)
  expect_equal(rd("argila; forte grande prismática"), none)
  expect_equal(rd(NA_character_), none)
})

test_that("the spellings of the export are read, and the rows are independent", {
  r <- .bdsolos_films_reads(c("serosidade comum e fraca", "crosidade forte e abundante",
                              NA, "argila", "cerosidade pouca"))
  expect_equal(r$amount, c("Comum", "Abundante", NA, NA, "Pouca"))
  expect_equal(r$grade, c("Fraca", "Forte", NA, NA, NA))
})

test_that("the loader fills each cell that is empty, and keeps the cells that are filled", {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior", "Descrição Original",
           "Cerosidade - Grau de desenvolvimento", "Cerosidade - Quantidade",
           "Composição Granulométrica da terra fina - Argila (g/Kg)")
  rows <- c(
    q("91", "PR", "NITOSSOLO", "A", "0", "20", "argila; forte média granular.", "", "", "550"),
    q("91", "PR", "NITOSSOLO", "Bt1", "20", "60", "argila; forte média blocos; cerosidade comum e moderada.", "", "", "620"),
    q("91", "PR", "NITOSSOLO", "Bt2", "60", "120", "argila; cerosidade abundante e forte.", "Fraca", "Pouca", "640"),
    q("91", "PR", "NITOSSOLO", "Bt3", "120", "170", "argila; cerosidade pouca e fraca.", "", "Comum", "640"),
    q("91", "PR", "NITOSSOLO", "BC", "170", "200", "argila; presença de cerosidade.", "", "", "600"))
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  h <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))[[1]]$horizons
  # from the text where both cells are empty; the cells where they are
  # filled; the grade from the text where only the amount is in its cell
  expect_equal(h$clay_films_amount, c(NA, "Comum", "Pouca", "Comum", NA))
  expect_equal(h$clay_films_strength, c(NA, "Moderada", "Fraca", "Fraca", NA))
  expect_false("description_raw" %in% names(h))
})

test_that("the cerosidade read from the text reaches the horizons that ask for it", {
  # a clayey B with a textural ratio under item (h): a B textural by its
  # cerosidade, common and moderate (item i), and no B latossolico (item h
  # of that horizon: at most few and weak)
  mk <- function(amount, grade) {
    hz <- data.table::data.table(
      designation = c("A", "Bt1", "Bt2", "C"), top_cm = c(0, 20, 70, 130),
      bottom_cm = c(20, 70, 130, 180), clay_pct = c(45, 55, 58, 50),
      silt_pct = 15, sand_pct = c(40, 30, 27, 35), cec_cmol = c(8, 5, 5, 5),
      structure_type = "Blocos subangulares", structure_grade = "Moderada",
      clay_films_amount = c(NA, amount, amount, NA),
      clay_films_strength = c(NA, grade, grade, NA))
    PedonRecord$new(site = list(id = "cf", country = "BR", lat = -23, lon = -51),
                    horizons = ensure_horizon_schema(hz))
  }
  r <- .bdsolos_films_reads("argila; cerosidade comum e moderada")
  with_films <- mk(r$amount, r$grade)
  expect_true(isTRUE(B_textural(with_films)$passed))
  expect_false(isTRUE(B_latossolico(with_films)$passed))
  without <- mk(NA_character_, NA_character_)
  expect_false(isTRUE(B_textural(without)$passed))
})
