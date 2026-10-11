# v0.9.226: the old soil names of the reference labels are read as the order
# SiBCS 5 gives them.
#
# v0.9.225 left out of the scored profiles every label whose first word is not
# an order. Most of those are names of the classifications in use before
# SiBCS: 876 of the 7,694 labels of the national BDsolos export, counting the
# 252 that begin with "Solo(s)". The 5th edition says where each old class
# went, in the "Abrangencia" paragraph of each order (chapter 3, p. 87-106)
# and in its Anexo I (p. 342). The names it gives to one order are now read
# as that order (628 labels), and Terra Roxa / Terra Bruna Estruturada as
# Nitossolos, where it puts most of them (147). The names it divides between
# orders by something the name does not say, and the names it does not list,
# stay out.

test_that("an old name with one order in SiBCS 5 is read as that order", {
  old <- c(
    "LATOSOL VERMELHO AMARELO DISTRÓFICO A proeminente textura argilosa" = "Latossolos",
    "LATOSSOL ROXO"                                      = "Latossolos",
    "LATOSOL VARIAÇÃO UNA DISTRÓFICO"                    = "Latossolos",
    "PLANOSOL SOLÓDICO EUTRÓFICO Ta A fraco"             = "Planossolos",
    "SOLONETZ SOLODIZADO."                               = "Planossolos",
    "BRUNIZEM AVERMELHADO textura média/argilosa."       = "Chernossolos",
    "BRUNIZEN AVERMELHADO"                               = "Chernossolos",
    "RENDZINA textura argilosa fase caatinga"            = "Chernossolos",
    "REGOSSOLO EUTRÓFICO A fraco textura arenosa."       = "Neossolos",
    "REGOSOL DISTRÓFICO"                                 = "Neossolos",
    "LITOSOL fase substrato gnaisse."                    = "Neossolos",
    "LITÓLICO EUTRÓFICO substrato rocha básica."         = "Neossolos",
    "VERTISOL A chernozêmico"                            = "Vertissolos",
    "CAMBISOL EUTRÓFICO latossólico"                     = "Cambissolos",
    "CONCRECIONÁRIO LATERÍTICO com cascalho"             = "Plintossolos",
    "RUBROZEM textura argilosa"                          = "Argissolos")
  expect_equal(normalise_febr_sibcs(names(old)), unname(old))
})

test_that("a misspelt name is read as the name", {
  typo <- c(
    "PLANOSOLO Ta EUTRÓFICO" = "Planossolos", "PLANOSSO NÁTRICO" = "Planossolos",
    "PLANOSSOL NÁTRICO Ta"   = "Planossolos", "BRUIZEM AVERMELHADO" = "Chernossolos",
    "CHENOSSOLO EBÂNICO"     = "Chernossolos",
    "ARE1A QUARTZOSA DISTRÓFICA" = "Neossolos", "NEO0SSOLO FLÚVICO" = "Neossolos",
    "Neossoli flúvico Ta"    = "Neossolos",  "NEOSSSOLO QUARTZARÊNICO" = "Neossolos",
    "VERTISOLO A chernozêmico" = "Vertissolos", "CAMBISOLO EUTRÓFICO" = "Cambissolos",
    "LATERIA HIDROMÓRFICA"   = "Plintossolos", "LATERIRA HIDROMÓRFICA ÁLICA" = "Plintossolos",
    "PLITOSSOLO Tb ÁLICO"    = "Plintossolos",
    "POZDÓLICO VERMELHO-AMARELO" = "Argissolos", "POZÓLICO VERMELHO-AMARELO" = "Argissolos",
    "PODZÓLCIO VERMELHO-AMARELO" = "Argissolos", "PODZÓLICA VERMELHO AMARELO" = "Argissolos",
    "PODZÓUCO VERMELHO AMARELO"  = "Argissolos", "ARGISSLO VERMELHO AMARELO" = "Argissolos",
    "ARGILOSSOLO VERMELHO-AMARELO" = "Argissolos",
    "NITSSOLO HÁPLICO"       = "Nitossolos", "GELISSOLO MELÂNICO Alítico típico" = "Gleissolos")
  expect_equal(normalise_febr_sibcs(names(typo)), unname(typo))
})

test_that("'Solo(s)' in front is not the name: the word after it is", {
  solo <- c(
    "SOLO LITÓLICO EUTRÓFICO A moderado textura média cascalhenta" = "Neossolos",
    "SOLOS LITÓLICOS ÁLICOS"                 = "Neossolos",
    "SOLO LÍTICO EUTRÓFICO Tb"               = "Neossolos",
    "SOLO ALUVIAL EUTRÓFICO Ta"              = "Neossolos",
    "Solos Aluviais."                        = "Neossolos",
    "SOLO ORGÂNICO ÁLICO A turfoso"          = "Organossolos",
    "Solos Orgânicos."                       = "Organossolos",
    "Solos Gley Pouco Húmico (distrófico)"   = "Gleissolos",
    "SOLOS CONCRECIONÁRIOS DISTRÓFICOS"      = "Plintossolos",
    "Solo Concrecionário Laterítico Epiálico com B textural" = "Plintossolos",
    "Solos Indiscriminados Concrecionários Tropicais fase cerrado" = "Plintossolos",
    "Solos Brunos Não Cálcicos."             = "Luvissolos")
  expect_equal(normalise_febr_sibcs(names(solo)), unname(solo))
  # what follows must be a name: "Solos" alone, or with a word that is none,
  # stays out as before
  out <- c("SOLOS", "Solo.", "SOLOS HALOMÓRFICOS", "SOLOS HIDROMÓRFICOS",
           "SOLO COLUVIAL DISTRÓFICO Tb", "Solos Brunos Ácidos (similares)",
           "SOLO GLEYZADO.", "SOLO INDISCRIMINADO DE MANGUE.",
           "SOLO TIOMÓRFICO EUTRÓFICO A turfoso", "Solos Podzolizados de Alpinópolis.")
  expect_true(all(is.na(normalise_febr_sibcs(out))))
})

test_that("the names that take two words are read by both", {
  two <- c(
    "TERRA ROXA ESTRUTURADA SIMILAR EUTRÓFICA A moderado" = "Nitossolos",
    "TERRA BRUNA ESTRUTURADA ÁLICA A proeminente"         = "Nitossolos",
    "Terra Roxa Estruturada eutrófica latossólica"        = "Nitossolos",
    "BRUNO NÃO CÁLCICO litólico fase pedregosa"           = "Luvissolos",
    "Bruno Não-Cálcico A moderado"                        = "Luvissolos",
    "BRUNO-NÃO -CÁLCICO A moderado"                       = "Luvissolos",
    "Humic Glei."                                         = "Gleissolos",
    "Low-Humic Glei."                                     = "Gleissolos")
  expect_equal(normalise_febr_sibcs(names(two)), unname(two))
  # one of the two words is not the name
  expect_true(all(is.na(normalise_febr_sibcs(
    c("TERRA PRETA", "TERRA", "BRUNO ÁCIDO", "BRUNO", "HUMIC", "LOW")))))
})

test_that("a hyphen separates the words of a name as a space does", {
  expect_equal(
    normalise_febr_sibcs(c("LATOSSOLO-VERMELHO EUTRÓFICO A moderado",
                           "LATOSSOLO-VERMELHO-ESCURO DISTRÓFICO",
                           "LATOSSOLO-AMARELO DISTRÓFICO",
                           "SOLONETZ-SOLODIZADO Ta EUTRÓFICO",
                           "SOLONETZ - SOLODIZADO EUTRÓFICO Tb")),
    c("Latossolos", "Latossolos", "Latossolos", "Planossolos", "Planossolos"))
  expect_equal(normalise_febr_sibcs("LATOSSOLO-VERMELHO EUTRÓFICO", level = "subordem"),
               "Latossolos Vermelhos")
})

test_that("names SiBCS 5 divides between orders, or does not list, stay out", {
  out <- c(
    # Gleissolos without an abrupt textural change, Planossolos with one
    "HIDROMÓRFICO CINZENTO EUTRÓFICO Tb A moderado textura arenosa/média",
    "HIDROMORFO CINZENTO ÁLICO Tb",
    # Gleissolos only with a glei horizon
    "SOLONCHAK SOLONÉTZICO Ta", "Solonchak-Sódico* Ta eutrófico",
    # an order of the 1st edition, divided when it was dropped
    "Alissolo Crômico Argilúvico plíntico",
    # not in the correlation of the 5th edition
    "Laterítico Bruno Avermelhado Eutrófico", "Latérico Bruno Avermelhado",
    "MEDITERRÂNEO VERMELHO-AMARELO (similar).",
    # not a class
    "aterro com calhaus a partir de 20cm de profundidade",
    "Typic Haplorthox ou Latossolo Vermelho Amarelo (Unidade Cristalino)",
    "Tropaquods ou Solos Orgânicos.",
    "*MUDAR*--- Solos Litossólicos com baixo teor de cálcio",
    "*CORRIGIR* PODZÓLICO VERMELHO-AMARELO ÁLICO Tb",
    "PAD - ARGISSOLO AMARELO Distrófico típico")
  for (level in c("order", "subordem"))
    expect_true(all(is.na(normalise_febr_sibcs(out, level = level))), info = level)
  # "Lateritico" is not "Laterita": the misspellings of one are not the other
  expect_equal(normalise_febr_sibcs("LATERITA HIDROMÓRFICA"), "Plintossolos")
})

test_that("a label the first word already places is read as before", {
  now <- c("ARGISSOLO VERMELHO-AMARELO Distrófico típico", "LATOSSOLO VERMELHO",
           "PODZÓLICO VERMELHO-AMARELO", "GLEI POUCO HÚMICO", "ALUVIAL EUTRÓFICO",
           "AREIA QUARTZOSA", "PODZOL HIDROMÓRFICO", "ESPODOSSOLO.")
  expect_equal(normalise_febr_sibcs(now),
               c("Argissolos", "Latossolos", "Argissolos", "Gleissolos", "Neossolos",
                 "Neossolos", "Espodossolos", "Espodossolos"))
  expect_equal(normalise_febr_sibcs(now[1:3], level = "subordem"),
               c("Argissolos Vermelho-amarelos", "Latossolos Vermelhos",
                 "Argissolos Vermelho-amarelos"))
})

test_that("both maps point to orders of SiBCS 5, in the form the function looks up", {
  one <- .SIBCS_LEGACY_ORDER_MAP
  expect_true(all(is.na(one) | one %in% .SIBCS_ORDERS))
  expect_true(all(.SIBCS_LEGACY_TWO_WORD_MAP %in% .SIBCS_ORDERS))
  expect_false(anyDuplicated(names(one)) > 0L)
  expect_false(anyDuplicated(names(.SIBCS_LEGACY_TWO_WORD_MAP)) > 0L)
  # no entry hides an order, and the two-word keys are two words, upper case
  expect_length(intersect(names(one), .SIBCS_ORDERS), 0L)
  expect_true(all(grepl("^[A-Z0-9]+ [A-Z0-9]+$", names(.SIBCS_LEGACY_TWO_WORD_MAP))))
})

test_that("a profile with an old name is scored against its order", {
  skip_on_cran()
  mk <- function(f, ref) {
    p <- get(f, envir = asNamespace("soilKey"))()
    p$site$reference_sibcs <- ref
    p
  }
  peds <- list(
    mk("make_latossolo_canonical",   "LATOSOL VERMELHO ESCURO DISTRÓFICO A moderado"),
    mk("make_vertissolo_canonical",  "VERTISOL A chernozêmico fase floresta subcaducifólia"),
    mk("make_luvissolo_canonical",   "BRUNO NÃO CÁLCICO A moderado textura média/argilosa"),
    mk("make_nitossolo_canonical",   "TERRA ROXA ESTRUTURADA EUTRÓFICA A moderado"),
    mk("make_espodossolo_canonical", "HIDROMÓRFICO CINZENTO ÁLICO Tb"))
  r <- benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)$per_system$sibcs
  expect_equal(r$n_compared, 4L)
  expect_equal(r$n_correct, 4L)
  expect_equal(sum(r$confusion["<unmapped>", ]), 1L)
})
