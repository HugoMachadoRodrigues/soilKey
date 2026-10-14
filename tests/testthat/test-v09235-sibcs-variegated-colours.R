# v0.9.235: the variegated colours and the colours of the mottles are carried
# by a profile, and the depth rule of Plintossolos reads them.
#
# A plinthic horizon that starts below 40 cm makes a Plintossolo when it lies
# immediately below a horizon "que apresente cores palidas, variegadas ou com
# mosqueados em quantidade abundante" (SiBCS 2018, Cap 3, p. 104), and the
# book says of each:
#   - "Quando precedidos de horizontes ou camadas de coloracao variegada, pelo
#     menos uma das cores deve satisfazer aos itens (a) e (b)";
#   - "Quando precedidos de horizontes ou camadas com mosqueados, estes
#     deverao ocorrer em quantidade abundante (> 20% em volume) numa matriz de
#     coloracao avermelhada ou amarelada e deverao apresentar matizes e cromas
#     conforme os itens (a) e (b)";
#   - "a) Matiz 5Y; ou b) Matizes 7,5YR, 10YR ou 2,5Y com croma menor ou igual
#     a 4".
# A profile held one colour per horizon, so a variegated horizon, which has no
# single matrix colour, could not be read at all, and the mottles counted by
# their amount whatever their colour. Two columns now hold the Munsell
# notations the sheets list, `variegated_colors` and `mottle_colors`.

test_that("the two columns are part of the profile and of its JSON schema", {
  spec <- horizon_column_spec()
  expect_equal(unname(spec[c("variegated_colors", "mottle_colors")]),
               list("character", "character"))
  h <- make_empty_horizons(2)
  expect_true(all(is.na(h$variegated_colors)) && all(is.na(h$mottle_colors)))
  s <- pedon_json_schema(as = "list")
  expect_true(all(c("variegated_colors", "mottle_colors") %in%
                    names(s$properties$horizons$items$properties)))
})

test_that("a list of colours is written as Munsell notations", {
  sp <- function(x) strsplit(x, ":", fixed = TRUE)[[1]]
  expect_equal(.munsell_notation_list(sp("10YR:5YR"), sp("6:5"), sp("3:6")),
               "10YR 6/3; 5YR 5/6")
  # a decimal comma, a space in the hue, lower case
  expect_equal(.munsell_notation_list(c("7,5 YR", "2,5y"), c("5", "6,5"), c("4", "2")),
               "7.5YR 5/4; 2.5Y 6.5/2")
  # a value or a chroma that is missing leaves its place empty
  expect_equal(.munsell_notation_list(sp("10YR:7,5YR"), sp("7:5"), sp(":8")),
               "10YR 7/; 7.5YR 5/8")
  expect_equal(.munsell_notation_list("10YR", NA, NA), "10YR /")
  # a colour with no hue is left out; with none left, NA
  expect_equal(.munsell_notation_list(c("", "10YR"), c("5", "6"), c("3", "2")), "10YR 6/2")
  expect_identical(.munsell_notation_list(character(0), "5", "3"), NA_character_)
  expect_identical(.munsell_notation_list(NA, NA, NA), NA_character_)
})

test_that("the notations are read back, one row per colour", {
  x <- .munsell_notations("10YR 6/3; 2.5YR 4/6; N 6/; 7.5 yr 5,5/4")
  expect_equal(x$number, c(10, 2.5, NA, 7.5))
  expect_equal(x$family, c("YR", "YR", "N", "YR"))
  expect_equal(x$value,  c(6, 4, 6, 5.5))
  expect_equal(x$chroma, c(3, 6, NA, 4))
  # a missing value or chroma
  y <- .munsell_notations("10YR 7/; 10YR /8")
  expect_equal(y$value, c(7, NA)); expect_equal(y$chroma, c(NA, 8))
  # what is not a notation is a row of NA; nothing recorded is no row
  z <- .munsell_notations("10 6/4; lixo")
  expect_equal(nrow(z), 2L); expect_true(all(is.na(z$family)))
  expect_equal(nrow(.munsell_notations(NA_character_)), 0L)
  expect_equal(nrow(.munsell_notations("  ")), 0L)
  # written and read back
  w <- .munsell_notations(.munsell_notation_list(c("5Y", "10YR"), c(6, 7), c(2, 1)))
  expect_equal(w$family, c("Y", "YR")); expect_equal(w$chroma, c(2, 1))
})

test_that("items (a) and (b) of the definition of Plintossolos", {
  ab <- function(n, f, c) .plintossolo_items_a_b(n, f, c)
  # a) hue 5Y, whatever the chroma
  expect_true(all(ab(c(5, 5, 5), c("Y", "Y", "Y"), c(1, 8, NA))))
  # b) 7.5YR, 10YR or 2.5Y with chroma 4 or less
  expect_true(all(ab(c(7.5, 10, 2.5, 10), c("YR", "YR", "Y", "YR"), c(4, 1, 3, 4))))
  expect_false(any(ab(c(7.5, 10, 2.5), c("YR", "YR", "Y"), c(5, 6, 8))))
  # other hues, and a chroma that is not recorded
  expect_false(any(ab(c(5, 2.5, 10, 10, NA), c("YR", "YR", "R", "YR", "N"), c(2, 2, 2, NA, 0))))
  expect_false(any(ab(c(NA, 10), c("YR", NA), c(2, 2))))
})

.vc_csv <- function() {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior",
           "Cor da Amostra Úmida - Matiz", "Cor da Amostra Úmida - Valor",
           "Cor da Amostra Úmida - Croma",
           "Variegado - Nomenclatura", "Variegado - Matiz", "Variegado - Valor",
           "Variegado - Croma",
           "Mosqueado - Nomenclatura", "Mosqueado - Quantidade", "Mosqueado - Matiz",
           "Mosqueado - Valor", "Mosqueado - Croma")
  ref <- "PLINTOSSOLO ARGILÚVICO Distrófico típico"
  rows <- c(
    q("801", "AM", ref, "A",    "0",   "20",  "10YR", "4", "2", "", "", "", "", "", "", "", "", ""),
    q("801", "AM", ref, "Bt",   "20",  "60",  "7,5YR", "5", "8", "", "", "", "",
      "Vermelho", "Abundante:Pouca", "2,5YR:10YR", "4:6", "6:2"),
    q("801", "AM", ref, "Btg",  "60",  "100", "", "", "",
      "Cinzento-claro:Amarelo-brunado", "2,5Y:10YR", "7:6", "1:8", "", "", "", "", ""),
    q("801", "AM", ref, "Btf",  "100", "180", "", "", "",
      "Cinzento-claro", "10YR:7,5YR", "7:5", ":8", "", "Comum", "5YR", "5", "8"))
  f <- tempfile(fileext = ".csv")
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  f
}

test_that("the BDsolos loader joins the three cells of each list of colours", {
  f <- .vc_csv(); on.exit(unlink(f), add = TRUE)
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))
  expect_length(peds, 1L)
  h <- peds[[1]]$horizons
  expect_equal(h$designation, c("A", "Bt", "Btg", "Btf"))
  expect_equal(h$variegated_colors,
               c(NA, NA, "2.5Y 7/1; 10YR 6/8", "10YR 7/; 7.5YR 5/8"))
  expect_equal(h$mottle_colors, c(NA, "2.5YR 4/6; 10YR 6/2", NA, "5YR 5/8"))
  # the amount of the mottles is read as before, and the matrix colour too
  expect_equal(h$redoximorphic_features_pct, c(NA, 30, NA, 10))
  expect_equal(h$munsell_hue_moist, c("10YR", "7.5YR", NA, NA))
  # the helper, cell by cell
  expect_equal(.bdsolos_colour_list(c("10YR:5YR", NA, ""), c("6:5", NA, "4"), c("3:6", NA, "2")),
               c("10YR 6/3; 5YR 5/6", NA, NA))
  expect_equal(.bdsolos_colour_list("5Y", NULL, NULL), "5Y /")
})

test_that("the Redape loader reads COR_MOSQ_* as variegated colours or as mottle colours", {
  hz <- function(...) .redape_horizon_to_soilkey(list(LIMITE_SUP = 75, LIMITE_INF = 93,
                                                      SIMB_HORIZ = "2Bt", ...))
  # no matrix colour: the horizon is variegated
  v <- hz(COR_MOSQ_MATIZ_1 = "10YR", COR_MOSQ_VALOR_1 = 5, COR_MOSQ_CROMA_1 = 3,
          COR_MOSQ_MATIZ_2 = "2.5YR", COR_MOSQ_VALOR_2 = 4, COR_MOSQ_CROMA_2 = 6)
  expect_equal(v$variegated_colors, "10YR 5/3; 2.5YR 4/6")
  expect_true(is.na(v$mottle_colors))
  # beside a matrix colour: they are the colours of the mottles
  m <- hz(COR_UMIDA_MATIZ = "10YR", COR_UMIDA_VALOR = 5, COR_UMIDA_CROMA = 8,
          COR_MOSQ_MATIZ_1 = "10YR", COR_MOSQ_VALOR_1 = 5, COR_MOSQ_CROMA_1 = 2)
  expect_equal(m$mottle_colors, "10YR 5/2")
  expect_true(is.na(m$variegated_colors))
  # neither
  n <- hz(COR_UMIDA_MATIZ = "5YR", COR_UMIDA_VALOR = 4, COR_UMIDA_CROMA = 6)
  expect_true(is.na(n$variegated_colors) && is.na(n$mottle_colors))
})

.deep_plinthite_under <- function(above = list()) {
  # the canonical Plintossolo with its plinthite only in the last horizon,
  # which starts at 80 cm; `above` sets fields of the horizon over it
  p <- make_plintossolo_canonical()
  p$horizons$plinthite_pct <- c(0, 0, 25)
  for (k in names(above)) p$horizons[[k]][2] <- above[[k]]
  p
}

.variegated <- function(colours) {
  # a variegated horizon has no matrix colour
  .deep_plinthite_under(list(munsell_hue_moist = NA_character_, munsell_value_moist = NA_real_,
                             munsell_chroma_moist = NA_real_, variegated_colors = colours))
}

test_that("a variegated horizon above the plinthic one counts when one of its colours satisfies (a) or (b)", {
  skip_on_cran()
  expect_false(isTRUE(plintossolo(.deep_plinthite_under())$passed))
  for (ok in c("10YR 5/3; 2.5YR 4/6",          # Redape GeoTab_PE_048_Goiana
               "2.5Y 7/1; 10YR 6/8",           # BDsolos AM 10006
               "5YR 4/6; 10YR 6/4",
               "2.5YR 4/8; 5Y 6/6",            # 5Y, whatever its chroma
               "7.5YR 5/4"))
    expect_true(isTRUE(plintossolo(.variegated(ok))$passed), info = ok)
  for (no in c("2.5YR 4/6; 5YR 5/6; 7.5YR 6/8", # none in the list with chroma <= 4
               "10R 4/4; 10YR 6/6",
               "5YR 3/2; 2.5YR 4/6",
               "10YR 6/",                       # the chroma is not recorded
               "lixo"))
    expect_false(isTRUE(plintossolo(.variegated(no))$passed), info = no)
  # hue and chroma only, as the book writes: the value of 5 or more that is
  # asked of a pale matrix colour is not asked of a variegated one
  expect_true(isTRUE(plintossolo(.variegated("10YR 3/2; 2.5YR 4/6"))$passed))
  pale_dark <- .deep_plinthite_under(list(munsell_hue_moist = "10YR", munsell_value_moist = 3,
                                          munsell_chroma_moist = 2))
  expect_false(isTRUE(plintossolo(pale_dark)$passed))
})

.argissolo_deep_plinthite <- function(variegated = NA_character_) {
  # the canonical Argissolo (A, BA, Bt, BC) with plinthite in its BC, which
  # starts at 120 cm under the Bt: plinthite deep in a B textural
  p <- make_argissolo_canonical()
  k <- nrow(p$horizons)
  p$horizons$plinthite_pct <- c(rep(0, k - 1L), 25)
  if (!is.na(variegated)) {
    p$horizons$munsell_hue_moist[k - 1L] <- NA_character_
    p$horizons$munsell_value_moist[k - 1L] <- NA_real_
    p$horizons$munsell_chroma_moist[k - 1L] <- NA_real_
    p$horizons$variegated_colors[k - 1L] <- variegated
  }
  p
}

test_that("an Argissolo with deep plinthite is a Plintossolo when the horizon above is so coloured", {
  skip_on_cran()
  key <- function(p) classify_sibcs(p, on_missing = "silent")$rsg_or_order
  plain <- .argissolo_deep_plinthite()
  expect_true(isTRUE(horizonte_plintico(plain)$passed))
  expect_equal(key(plain), "Argissolos")
  expect_equal(key(.argissolo_deep_plinthite("10YR 5/3; 2.5YR 4/6")), "Plintossolos")
  expect_equal(key(.argissolo_deep_plinthite("2.5YR 4/6; 5YR 5/6")), "Argissolos")
  # the variegated colours of another horizon do not count
  other <- .argissolo_deep_plinthite()
  other$horizons$variegated_colors[1] <- "10YR 5/3; 2.5YR 4/6"
  expect_equal(key(other), "Argissolos")
})

test_that("abundant mottles count by their colour, when the sheet records it", {
  skip_on_cran()
  mott <- function(pct, colours = NA_character_)
    .deep_plinthite_under(list(redoximorphic_features_pct = pct, mottle_colors = colours))
  # no colour recorded: the amount decides, as before
  expect_true(isTRUE(plintossolo(mott(30))$passed))
  expect_false(isTRUE(plintossolo(mott(10))$passed))
  # a colour that satisfies (a) or (b)
  expect_true(isTRUE(plintossolo(mott(30, "10YR 5/3"))$passed))
  expect_true(isTRUE(plintossolo(mott(30, "5YR 6/8; 2.5Y 6/2"))$passed))
  # abundant mottles of another colour (BDsolos PA 9609: 5YR 6/8)
  expect_false(isTRUE(plintossolo(mott(30, "5YR 6/8"))$passed))
  expect_false(isTRUE(plintossolo(mott(30, "7.5YR 6/8; 10YR 7/8"))$passed))
  # the colour without the amount is not enough
  expect_false(isTRUE(plintossolo(mott(10, "10YR 5/3"))$passed))
  expect_false(isTRUE(plintossolo(mott(NA_real_, "10YR 5/3"))$passed))
  # a pale matrix still counts by itself, whatever the mottles
  pale <- .deep_plinthite_under(list(munsell_hue_moist = "10YR", munsell_value_moist = 6,
                                     munsell_chroma_moist = 2,
                                     redoximorphic_features_pct = 30, mottle_colors = "5YR 5/8"))
  expect_true(isTRUE(plintossolo(pale)$passed))
})

test_that("the canonical fixtures carry neither column and are read as before", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  for (f in c("make_plintossolo_canonical", "make_argissolo_canonical", "make_gleissolo_canonical")) {
    p <- get(f, envir = ns)()
    expect_true(all(is.na(p$horizons$variegated_colors)), info = f)
    expect_true(all(is.na(p$horizons$mottle_colors)), info = f)
  }
  expect_equal(classify_sibcs(make_plintossolo_canonical(), on_missing = "silent")$rsg_or_order,
               "Plintossolos")
})
