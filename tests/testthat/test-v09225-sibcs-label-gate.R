# v0.9.225: a SiBCS reference label is a class only if it is an order of SiBCS 5.
#
# normalise_febr_sibcs() reads a label by its first word. Until v0.9.224 that
# word came out whatever it was: "CLASSIFICACAO ? CAMBISSOLO HAPLICO ..." was
# the class "Classificacaos", "ALISSOLO CROMICO" was "Alissolos", "LATOSOL
# AMARELO" was "Latosois" and "ESPODOSSOLO." was "Espodossolo.s". No prediction
# can equal those, yet the benchmarks counted them as scored profiles and as
# errors: 631 of the 7,694 labels of the national BDsolos export, in 79 forms,
# and 3 of the 787 of the BDsolos panel of the EJSS article, whose supplement
# says such labels are removed before scoring.

test_that("punctuation around the first word is not part of the name", {
  expect_equal(normalise_febr_sibcs("ESPODOSSOLO."), "Espodossolos")
  expect_equal(normalise_febr_sibcs("Vertissolo."), "Vertissolos")
  expect_equal(normalise_febr_sibcs("PLANOSSOLO(???) Eutrófico"), "Planossolos")
  expect_equal(normalise_febr_sibcs("  CHERNOSSOLO.  "), "Chernossolos")
})

test_that("a first word that is not an order of SiBCS 5 gives NA", {
  not_a_class <- c(
    "CLASSIFICAÇÃO ? CAMBISSOLO HÁPLICO bd Petroplíntico",  # text, not a class
    "Alissolo Crômico Argilúvico plíntico",   # an order of the 1999 edition
    "HIDROMÓRFICO CINZENTO ÁLICO",             # an old name without an entry in the map
    "Laterítico Bruno Avermelhado",            # (LATOSOL and Terra Roxa have one since v0.9.226)
    "Typic Umbriorthox ou LATOSSOLO Vermelho Amarelo",   # another system first
    "Pastagem natural.", "*MUDAR*--- Solos Litossólicos", "...", "-", "")
  for (level in c("order", "subordem"))
    expect_true(all(is.na(normalise_febr_sibcs(not_a_class, level = level))), info = level)
  expect_true(is.na(normalise_febr_sibcs(NA_character_)))
})

test_that("every order of SiBCS 5 and every entry of the legacy map still gets through", {
  expect_length(.SIBCS_ORDERS, 13L)
  singular <- toupper(sub("s$", "", .SIBCS_ORDERS))
  expect_equal(normalise_febr_sibcs(singular), .SIBCS_ORDERS)
  expect_equal(normalise_febr_sibcs(toupper(.SIBCS_ORDERS)), .SIBCS_ORDERS)
  expect_equal(normalise_febr_sibcs(paste(singular, "HAPLICO"), level = "subordem"),
               paste(.SIBCS_ORDERS, "Haplicos"))
  # what the legacy map points to is an order, or NA
  target <- unname(.SIBCS_LEGACY_ORDER_MAP)
  expect_true(all(is.na(target) | target %in% .SIBCS_ORDERS))
  expect_equal(normalise_febr_sibcs(c("PODZOLICO VERMELHO-AMARELO", "GLEI HUMICO",
                                      "AREIA QUARTZOSA", "LATOSOLO AMARELO")),
               c("Argissolos", "Gleissolos", "Neossolos", "Latossolos"))
})

test_that(".SIBCS_ORDERS is what the key returns", {
  skip_on_cran()
  fx <- grep("^make_.*_canonical$", ls(asNamespace("soilKey")), value = TRUE)
  got <- unique(vapply(fx, function(f)
    classify_sibcs(get(f, envir = asNamespace("soilKey"))(),
                   on_missing = "silent")$rsg_or_order, character(1)))
  expect_setequal(got, .SIBCS_ORDERS)
})

test_that("a label that is not a class is left out of the scored profiles", {
  skip_on_cran()
  mk <- function(f, ref) {
    p <- get(f, envir = asNamespace("soilKey"))()
    p$site$reference_sibcs <- ref
    p
  }
  peds <- list(
    mk("make_ferralsol_canonical", "LATOSSOLO VERMELHO Distrófico típico"),
    mk("make_podzol_canonical",    "ESPODOSSOLO."),
    mk("make_luvisol_canonical",   "CLASSIFICAÇÃO ? CAMBISSOLO HÁPLICO"),
    mk("make_vertisol_canonical",  "Alissolo Crômico Argilúvico"))
  r <- benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)$per_system$sibcs
  # two profiles have a class to compare with; the other two are not scored
  expect_equal(r$n_compared, 2L)
  expect_equal(r$n_correct, 2L)
  expect_equal(sum(r$confusion["<unmapped>", ]), 2L)
  expect_false(any(c("Classificacaos", "Alissolos", "Espodossolo.s") %in% rownames(r$confusion)))
})
