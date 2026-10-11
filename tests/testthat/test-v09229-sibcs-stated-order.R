# v0.9.229: a SiBCS label is read by what it states, when that places it in
# another order than its name does.
#
# The label map read every "Podzolico" as Argissolos and every "Cambissolo" as
# Cambissolos. Anexo I of SiBCS 2018 (p. 342) is finer: the "Podzolicos
# Vermelho-Amarelos Eutroficos Ta" and the "Podzolicos Vermelho-Escuros
# Eutroficos Ta" are Luvissolos, and the Cambissolos are Cambissolos "exceto
# os Cambissolos Eutroficos com horizonte A chernozemico e com argila de
# atividade alta", which are Chernossolos. In the national BDsolos export 31
# and 11 labels state exactly that, and none of those profiles was counted as
# agreeing. Another 36 labels begin with "Argissolo" and state Eutrofico and
# high-activity clay: a B textural with both is a Luvissolo by the book
# (p. 87 and 95), and the label is read by what it states.

test_that("a Podzolico Vermelho-Amarelo or Vermelho-Escuro, Eutrofico and Ta, is a Luvissolo", {
  luv <- c(
    "PODZÓLICO VERMELHO-AMARELO Eutrófico abrúptico argila de atividade alta A moderada textura argilosa.",
    "PODZÓLICO VERMELHO AMARELO EUTRÓFICO argila atividade alta A fraco textura argilosa. Ultic Haplustalf.",
    "Podzólico Vermelho-Amarelo Eutrófico Ta A moderado textura argilosa fase Floresta Tropical Perenifólia",
    "PODZÓLICO VERMELHO AMARELO EQUIVALENTE EUTRÓFICO Ta A moderado textura argilosa",
    "PODZÓLICO VERMELHO-ESCURO Ta EUTRÓFICO abrúptico A fraco textura média",
    "PODZÓLICO VERMELHO-AMARELADO EUTRÓFICO Ta A moderado textura média/argilosa",
    "Podzólico Vermelho- Amarelo eutrófico, Ta, A moderado",
    "podzólico vermelho-amarelo eutrófico argila de atividade  alta",   # typed with two spaces
    "SOLO PODZÓLICO VERMELHO-AMARELO EUTRÓFICO Ta",
    "POZDÓLICO VERMELHO-AMARELO EUTRÓFICO Ta")
  expect_equal(normalise_febr_sibcs(luv), rep("Luvissolos", length(luv)))
})

test_that("a Podzolico that does not state both stays an Argissolo", {
  arg <- c(
    "PODZÓLICO VERMELHO-AMARELO",
    "PODZÓLICO VERMELHO-AMARELO Tb EUTRÓFICO A moderado textura argilosa",          # Tb
    "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO argila de atividade baixa",
    "PODZÓLICO VERMELHO-AMARELO DISTRÓFICO Ta A moderado",                          # not eutrofico
    "PODZÓLICO VERMELHO-AMARELO ÁLICO argila de atividade alta",
    "PODZÓLICO VERMELHO-AMARELO EPIEUTRÓFICO Ta pouco profundo A moderado",         # only the surface
    "PODZÓLICO VERMELHO-AMARELO ENDOEUTRÓFICO Ta",
    "Podzólico Vermelho Amarelo Equivalente Eutrófico textura muito argilosa.",     # activity not stated
    "Podzólico Vermelho Amarelo Eutrófico latossólico A fraco textura média",
    "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO Ta e Tb",                                 # both
    "PODZÓLICO AMARELO EUTRÓFICO Ta A moderado",          # Podzolicos Amarelos: Argissolos in Anexo I
    "PODZÓLICO ACINZENTADO EUTRÓFICO Ta",
    "PODZÓLICO BRUNO-ACINZENTADO EUTRÓFICO epidistrófico Tb A proeminente",
    "PODZÓLICO VERMELHO-ESCURO Tb EUTRÓFICO A moderado")
  expect_equal(normalise_febr_sibcs(arg), rep("Argissolos", length(arg)))
})

test_that("'Ta' is the word, not two letters of another one", {
  # TEXTURA, ESTACA, ATALAIA, "A moderado" ...: none of them states the activity
  expect_equal(normalise_febr_sibcs(c(
    "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO A moderado textura argilosa fase floresta",
    "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO (Unidade Atalaia) textura média",
    "ARGISSOLO VERMELHO-AMARELO Eutrófico típico, textura argilosa, A moderado, hipereutrófico")),
    rep("Argissolos", 3L))
  # and it is found between punctuation
  expect_equal(normalise_febr_sibcs(c(
    "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO (Ta) A moderado",
    "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO, Ta, A moderado",
    "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO Ta.")),
    rep("Luvissolos", 3L))
})

test_that("a label that begins with 'Argissolo' and states Eutrofico and Ta is read as a Luvissolo", {
  luv <- c(
    "ARGISSOLO VERMELHO-AMARELO Eutrófico argila de atividade alta A moderado textura argilosa.",
    "ARGISSOLO VERMELHO-AMARELO EUTRÓFICO Ta A moderado textura média fase floresta equatorial",
    "ARGISSOLO AMARELO Ta Eutrófico plíntico, câmbico, abrúptico, textura média/argilosa, A moderado",
    "Argissolo Vermelho Eutrófico Ta",
    "ARGISSOLO VERMELHO AMARELO EQUIVALENTE EUTRÓFICO Ta abrúptico")
  expect_equal(normalise_febr_sibcs(luv), rep("Luvissolos", length(luv)))
  arg <- c(
    "ARGISSOLO VERMELHO-AMARELO Eutrófico típico",                       # a class of SiBCS as it is
    "ARGISSOLO VERMELHO-AMARELO Eutrófico Tb A moderado",
    "ARGISSOLO VERMELHO-AMARELO Ta Distrófico típico",
    "ARGISSOLO VERMELHO-AMARELO Alítico típico, textura argilosa",
    "ARGISSOLO VERMELHO AMARELO EQUIVALENTE EUTRÓFICO abrúptico",
    "ARGISSOLO VERMELHO Eutrófico Ta e Tb")
  expect_equal(normalise_febr_sibcs(arg), rep("Argissolos", length(arg)))
})

test_that("a Cambissolo in the old form, Eutrofico, Ta and with A chernozemico, is a Chernossolo", {
  che <- c(
    "CAMBISSOLO EUTRÓFICO Ta A chernozêmico textura argilosa fase floresta/caatinga",
    "CAMBISSOLO Ta EUTRÓFICO A chernozêmico textura média fase floresta subcaducifólia",
    "Cambissolo eutrófico argila de atividade alta A chernozêmico textura argilosa C carbonático",
    "CAMBISSOLO EUTRÓFICO Ta C Carbonático A chernozêmico textura média",
    "CAMBISOL EUTRÓFICO Ta A chernozêmico")
  expect_equal(normalise_febr_sibcs(che), rep("Chernossolos", length(che)))
  cam <- c(
    "CAMBISSOLO HÁPLICO Eutrófico Ta A chernozêmico, textura argilosa",   # written in SiBCS
    "CAMBISSOLO HÚMICO Eutrófico Ta A chernozêmico",
    "CAMBISSOLO EUTRÓFICO Tb A chernozêmico textura argilosa",            # Tb
    "CAMBISSOLO EUTRÓFICO Ta A moderado textura argilosa",                # no A chernozemico
    "CAMBISOLO EUTRÓFICO com A chernozêmico textura argilosa pedregosa",  # activity not stated
    "CAMBISSOLO DISTRÓFICO Ta A chernozêmico",                            # not eutrofico
    "CAMBISSOLO ÁLICO Tb A moderado",
    "CAMBISSOLO")
  expect_equal(normalise_febr_sibcs(cam), rep("Cambissolos", length(cam)))
})

test_that("the other orders are read as before, whatever they state", {
  same <- c(
    "LUVISSOLO CRÔMICO Órtico típico"               = "Luvissolos",
    "NITOSSOLO VERMELHO Eutrófico Ta"               = "Nitossolos",
    "LATOSSOLO VERMELHO Eutrófico argila de atividade alta" = "Latossolos",
    "PLANOSSOLO HÁPLICO Eutrófico Ta A chernozêmico" = "Planossolos",
    "BRUNIZEM AVERMELHADO Ta eutrófico"             = "Chernossolos",
    "BRUNO NÃO CÁLCICO Ta eutrófico"                = "Luvissolos",
    "TERRA ROXA ESTRUTURADA EUTRÓFICA Ta"           = "Nitossolos",
    "PODZOL HIDROMÓRFICO EUTRÓFICO Ta"              = "Espodossolos",
    "GLEISSOLO HÁPLICO Ta Eutrófico"                = "Gleissolos")
  expect_equal(normalise_febr_sibcs(names(same)), unname(same))
  # labels that are no class are still none
  expect_true(all(is.na(normalise_febr_sibcs(
    c("Typic Tropudalf ou Podzólico Vermelho-Amarelo Eutrófico Ta",
      "*CORRIGIR* PODZÓLICO VERMELHO-AMARELO EUTRÓFICO Ta", "aterro eutrófico Ta")))))
  expect_identical(.sibcs_stated_order(NA_character_, "PODZOLICO EUTROFICO TA"), NA_character_)
})

test_that("at the suborder level the stated order is used too", {
  expect_equal(normalise_febr_sibcs("PODZÓLICO VERMELHO-AMARELO EUTRÓFICO Ta", level = "subordem"),
               "Luvissolos Vermelho-amarelos")
  expect_equal(normalise_febr_sibcs("PODZÓLICO VERMELHO-AMARELO Tb", level = "subordem"),
               "Argissolos Vermelho-amarelos")
})

test_that("a profile with such a label is scored against the order it states", {
  skip_on_cran()
  mk <- function(f, ref) {
    p <- get(f, envir = asNamespace("soilKey"))()
    p$site$reference_sibcs <- ref
    p
  }
  peds <- list(
    mk("make_luvissolo_canonical",   "PODZÓLICO VERMELHO-AMARELO EUTRÓFICO Ta A moderado textura média/argilosa"),
    mk("make_luvissolo_canonical",   "ARGISSOLO VERMELHO-AMARELO Eutrófico argila de atividade alta A moderado"),
    mk("make_chernossolo_canonical", "CAMBISSOLO EUTRÓFICO Ta A chernozêmico textura argilosa"),
    mk("make_argissolo_canonical",   "PODZÓLICO VERMELHO-AMARELO Tb DISTRÓFICO A moderado"))
  r <- benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)$per_system$sibcs
  expect_equal(r$n_compared, 4L)
  expect_equal(r$n_correct, 4L)
  expect_setequal(rownames(r$confusion)[rowSums(r$confusion) > 0],
                  c("Luvissolos", "Chernossolos", "Argissolos"))
})
