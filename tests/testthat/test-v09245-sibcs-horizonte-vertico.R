# v0.9.245: the vertic horizon by the features of the book, read also from
# the text of the horizon description, and the Vertissolos step.
#
# SiBCS 2018, Cap 2, p. 73: the vertic horizon "apresenta feicoes pedologicas
# tipicas, que sao as superficies de friccao (slickensides) em quantidade no
# minimo comum, e/ou unidades estruturais cuneiformes e/ou paralelepipedicas
# [...] Para ser diagnostico, este horizonte deve apresentar uma espessura
# minima de 20 cm"; it "tem precedencia diagnostica sobre os horizontes B
# incipiente, B nitico e glei". Cap 4, p. 112: Vertissolos are "Outros solos
# com relacao textural insuficiente para identificar um B textural e que
# apresentam horizonte vertico".
#
# The horizon was that of WRB: slickensides only, and 20 cm of one layer. The
# surveys older than the structured sheet describe the slickensides and the
# structure in the text of the horizon ("Descricao Original" of the BDsolos
# export), which the loader did not read. And the Vertissolos step did not
# ask about the B textural.

.vt <- function(designation, top, bottom, clay, slickensides = NA_character_,
                structure = NA_character_, ...) {
  n <- length(designation)
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    clay_pct = clay, silt_pct = 25, sand_pct = 75 - clay,
    slickensides = rep_len(slickensides, n),
    structure_type = rep_len(structure, n), ...)
  PedonRecord$new(site = list(id = "vt", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

# ---- what the text of a description says -------------------------------------

test_that("the amount of slickensides is the one written beside the term", {
  rd <- function(x) .bdsolos_description_reads(x)$slickensides
  expect_equal(rd("Bruno-escuro (10YR 3/3); argila; forte grande prismática; 'slickenside' comum e moderado; muito duro."), "Comum")
  expect_equal(rd("argila; superfícies de fricção abundantes e fortes; plástico"), "Abundante")
  expect_equal(rd("argila; slickensides fraco e pouco; firme"), "Pouca")
  expect_equal(rd("argila; abundantes superfícies de fricção; firme"), "Abundante")
  expect_equal(rd("argila; muitos slickensides"), "Abundante")
  expect_equal(rd("argila; presença de superfície de fricção comum"), "Comum")
})

test_that("a mention with no amount, or a denial, gives none", {
  rd <- function(x) .bdsolos_description_reads(x)$slickensides
  expect_true(is.na(rd("argila; forte grande prismática; slickensides; muito duro")))
  # the "muito" of a grade is no amount
  expect_true(is.na(rd("argila; slickensides, muito forte grande prismática")))
  expect_true(is.na(rd("argila; muito duro, muito firme; slickensides")))
  expect_true(is.na(rd("argila; sem slickensides; firme")))
  expect_true(is.na(rd("argila; não apresenta superfícies de fricção")))
  # the amount of another feature, in another phrase
  expect_true(is.na(rd("cerosidade comum e moderada; slickensides; poros comuns")))
  expect_true(is.na(rd("argila; forte grande prismática")))
  expect_true(is.na(rd(NA_character_)))
  expect_equal(.bdsolos_description_reads(c("slickensides comuns", NA, "argila"))$slickensides,
               c("Comum", NA, NA))
})

test_that("an amount is read only where it stands beside the term", {
  rd <- function(x) .bdsolos_description_reads(x)$slickensides
  # the amount of another feature, before the term with words between
  expect_true(is.na(rd("argila; cerosidade forte e abundante apresentando ''slickensides'', não poroso")))
  expect_true(is.na(rd("maciça muito coerente quebrando-se ao longo das superfícies de deslizamento-''slickenside''")))
  # the amount directly before the term prevails over one further back
  expect_equal(rd("superfícies de compressão comuns entre os prismas e poucas superfícies de fricção"), "Pouca")
  expect_equal(rd("poucas superfícies de compressão e 'slickensides' comuns"), "Comum")
  expect_equal(rd("de coloração variegada e com presença comum de superfícies de fricção"), "Comum")
  # a comparison states no amount
  expect_true(is.na(rd("apresenta ''slickensides'' mais abundantes que o horizonte superior")))
  expect_equal(rd("apresenta ''slickensides'' em abundância"), "Abundante")
  expect_true(is.na(rd("algumas superfícies de fricção")))
  expect_true(is.na(rd("''slickenside'' médio e moderado")))
})

test_that("the terms are read in the spellings of the export", {
  rd <- function(x) .bdsolos_description_reads(x)$slickensides
  expect_equal(rd("''slikenside'' muito e forte"), "Abundante")
  expect_equal(rd("''slikeside'' comum e moderada"), "Comum")
  expect_equal(rd("stickenside poucos"), "Pouca")
  expect_equal(rd("comuns suoerficies de fricção (''slickensides'')"), "Comum")
  expect_equal(rd("slickenside bundante e forte"), "Abundante")
  expect_true(is.na(rd("presença de ''silkensides''")))
  expect_equal(.bdsolos_description_reads("forte média a grande paralelepédica")$structure,
               "paralelepipedica")
})

test_that("the wedge-shaped and the parallelepipedic structure are read from the text", {
  rd <- function(x) .bdsolos_description_reads(x)$structure
  expect_equal(rd("argila; moderada média paralelepipédica composta de blocos; duro"), "paralelepipedica")
  expect_equal(rd("argila; forte grande cuneiforme; duro"), "cuneiforme")
  expect_equal(rd("estrutura cuneiforme e paralelepipédica"), "cuneiforme e paralelepipedica")
  expect_equal(rd("maciça fraturada em grandes blocos paralelepipédicos"), "paralelepipedica")
  expect_equal(rd("blocos com forma de paralelepípedo devido à presença de ''silkensides''"), "paralelepipedica")
  expect_equal(rd("forte média e grande blocos angulares com partes com estrutura cuneiforme"), "cuneiforme")
  # a tendency to the shape is not the shape; nor is one the text denies
  expect_true(is.na(rd("moderada grande prismática com tendência a paralelepipédica, composta de blocos angulares")))
  expect_true(is.na(rd("moderada grande prismática (com tendência paralelepipédica) composta de blocos")))
  expect_true(is.na(rd("blocos angulares com tendência para estrutura paralelepipédica")))
  expect_true(is.na(rd("prismática; sem estrutura cuneiforme")))
  expect_true(is.na(rd("argila; forte grande prismática")))
  expect_true(is.na(rd(NA_character_)))
})

# ---- the loader -------------------------------------------------------------------

test_that("the loader fills from the text what the structured columns leave empty", {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior", "Descrição Original",
           "Forma - 1ª Ocorrência", "Superfície de fricção - Quantidade",
           "Composição Granulométrica da terra fina - Argila (g/Kg)")
  rows <- c(
    q("77", "BA", "VERTISSOLO", "A1", "0", "20",
      "Cinzento muito escuro; argila; forte média blocos; duro.", "Blocos angulares", "", "450"),
    q("77", "BA", "VERTISSOLO", "C1", "20", "60",
      "argila; forte grande prismática; 'slickenside' comum e moderado.", "Prismática", "", "520"),
    q("77", "BA", "VERTISSOLO", "C2", "60", "100",
      "argila; moderada média paralelepipédica; superfícies de fricção abundantes.", "", "Pouca", "540"),
    q("77", "BA", "VERTISSOLO", "C3", "100", "140",
      "argila; maciça; slickensides.", "Maciça", "", "500"))
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  p <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))[[1]]
  h <- p$horizons
  # from the text where the cell is empty; the cell where it is filled; a
  # mention with no amount stays empty
  expect_equal(h$slickensides, c(NA, "Comum", "Pouca", NA))
  # the structure of the text is added to that of the sheet
  expect_equal(h$structure_type[2], "Prismática")
  expect_equal(h$structure_type[3], "paralelepipedica")
  # the text itself is not kept
  expect_false("description_raw" %in% names(h))
  expect_false(any(grepl("Cinzento muito escuro", unlist(lapply(h, as.character)))))
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  expect_equal(horizonte_vertico(p)$layers, 2:3)
})

test_that("a file with no description column loads as before", {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior",
           "Forma - 1ª Ocorrência", "Superfície de fricção - Quantidade")
  rows <- c(q("78", "BA", "VERTISSOLO", "A", "0", "20", "Granular", ""),
            q("78", "BA", "VERTISSOLO", "C", "20", "80", "Prismática", "Comum"))
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  h <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))[[1]]$horizons
  expect_equal(h$slickensides, c(NA, "Comum"))
  expect_equal(h$structure_type, c("Granular", "Prismática"))
})

# ---- the vertic horizon ----------------------------------------------------------

test_that("slickensides in amount of common or more make the horizon; few do not", {
  p <- .vt(c("A", "C1", "C2"), c(0, 20, 60), c(20, 60, 100), c(40, 45, 45),
           slickensides = c(NA, "Comum", "Abundante"))
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  few <- .vt(c("A", "C1", "C2"), c(0, 20, 60), c(20, 60, 100), c(40, 45, 45),
             slickensides = c(NA, "Pouca", "Pouca"))
  expect_false(isTRUE(horizonte_vertico(few)$passed))
})

test_that("wedge-shaped or parallelepipedic structural units make it too", {
  for (s in c("Cuneiforme", "paralelepipedica", "Prismática; paralelepipedica", "wedge-shaped"))
    expect_true(isTRUE(horizonte_vertico(
      .vt(c("A", "C1"), c(0, 20), c(20, 60), c(40, 45), structure = c("Granular", s)))$passed),
      info = s)
  expect_false(isTRUE(horizonte_vertico(
    .vt(c("A", "C1"), c(0, 20), c(20, 60), c(40, 45), structure = c("Granular", "Prismática")))$passed))
})

test_that("it has 300 g/kg of clay or more, and cracks of 1 cm where their width is recorded", {
  expect_false(isTRUE(horizonte_vertico(
    .vt(c("A", "C1"), c(0, 20), c(20, 60), c(25, 29), structure = c("Granular", "Cuneiforme")))$passed))
  expect_true(isTRUE(horizonte_vertico(
    .vt(c("A", "C1"), c(0, 20), c(20, 60), c(25, 30), structure = c("Granular", "Cuneiforme")))$passed))
  narrow <- .vt(c("A", "C1"), c(0, 20), c(20, 60), c(40, 45),
                structure = c("Granular", "Cuneiforme"), cracks_width_cm = c(NA, 0.5))
  expect_false(isTRUE(horizonte_vertico(narrow)$passed))
  wide <- .vt(c("A", "C1"), c(0, 20), c(20, 60), c(40, 45),
              structure = c("Granular", "Cuneiforme"), cracks_width_cm = c(NA, 1))
  expect_true(isTRUE(horizonte_vertico(wide)$passed))
})

test_that("its 20 cm are those of the horizon, made of adjoining layers", {
  # two layers of 12 cm: vertic_horizon() asks 20 cm of one layer
  p <- .vt(c("A", "C1", "C2", "C3"), c(0, 20, 32, 44), c(20, 32, 44, 90), c(40, 45, 45, 45),
           slickensides = c(NA, "Comum", "Comum", NA))
  expect_false(isTRUE(vertic_horizon(p, min_thickness = 20, min_crack_width_cm = 1)$passed))
  r <- horizonte_vertico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  # 12 cm, and another 12 cm that do not adjoin
  q <- .vt(c("A", "C1", "C2", "C3"), c(0, 20, 32, 60), c(20, 32, 60, 72), c(40, 45, 45, 45),
           slickensides = c(NA, "Comum", NA, "Comum"))
  expect_false(isTRUE(horizonte_vertico(q)$passed))
})

test_that("the layers of the two readings are joined", {
  # C1 by its structure, C2 by its slickensides: one horizon from 20 cm
  p <- .vt(c("A", "C1", "C2"), c(0, 20, 50), c(20, 50, 90), c(40, 45, 45),
           slickensides = c(NA, "Pouca", "Comum"),
           structure = c("Granular", "Prismática; paralelepipedica", "Maciça"))
  expect_equal(horizonte_vertico(p)$layers, 2:3)
  expect_equal(horizonte_vertico(p)$evidence$sibcs$structure, 2L)
})

# ---- precedence over the B incipiente ----------------------------------------------

test_that("a vertic layer is no B incipiente", {
  p <- .vt(c("A", "Bi1", "Bi2"), c(0, 20, 60), c(20, 60, 110), c(40, 45, 45),
           structure = c("Granular", "Blocos angulares; cuneiforme", "Blocos angulares; cuneiforme"),
           munsell_hue_moist = "10YR", munsell_value_moist = 4, munsell_chroma_moist = 3,
           cec_cmol = 30)
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  expect_false(isTRUE(B_incipiente(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Vertissolos")
})

# ---- the Vertissolos step ----------------------------------------------------------

test_that("a textural ratio that identifies a B textural keeps the profile out", {
  # A 32%, Bt 60%: 1.88, over the 1.70 of item (h)
  p <- .vt(c("A", "Btv1", "Btv2"), c(0, 20, 60), c(20, 60, 110), c(32, 60, 60),
           slickensides = c(NA, "Comum", "Comum"),
           munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 6,
           boundary_distinctness = c("gradual", "diffuse", NA))
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  expect_true(isTRUE(B_textural(p)$evidence$item_h))
  v <- vertissolo(p)
  expect_false(isTRUE(v$passed))
  expect_false(v$evidence$no_b_textural)
  expect_false(classify_sibcs(p, on_missing = "silent")$rsg_or_order == "Vertissolos")
})

test_that("a B textural identified by its cerosidade, with a ratio below the limit, does not", {
  # A 54%, B 64 and 81%: 1.34, under the 1.50 of item (h); cerosidade common
  # and moderate in blocks
  p <- .vt(c("Ap", "Bt", "Bv"), c(0, 20, 50), c(20, 50, 120), c(54, 64, 81),
           slickensides = c(NA, NA, "Comum"),
           structure = c("Blocos angulares", "Blocos angulares", "Prismática"),
           clay_films_amount = c(NA, "Comum", NA), clay_films_strength = c(NA, "Moderada", NA),
           structure_grade = "Moderada")
  bt <- B_textural(p)
  expect_true(isTRUE(bt$passed))
  expect_false(bt$evidence$item_h)
  v <- vertissolo(p)
  expect_true(v$evidence$no_b_textural)
  expect_true(isTRUE(v$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Vertissolos")
})

test_that("where the ratio cannot be computed, the B textural keeps the profile out", {
  # no clay in the A: the ratio is not known to be insufficient
  p <- .vt(c("A1", "IIB21t", "IIB22t"), c(0, 20, 40), c(20, 40, 60), c(NA, 49, 49),
           slickensides = c(NA, NA, "Abundante"),
           structure = "Blocos angulares", structure_grade = "Moderada",
           clay_films_amount = c(NA, "Abundante", NA), clay_films_strength = c(NA, "Moderada", NA))
  expect_true(isTRUE(B_textural(p)$passed))
  v <- vertissolo(p)
  expect_true(is.na(v$evidence$textural_ratio))
  expect_false(v$evidence$no_b_textural)
  expect_false(isTRUE(v$passed))
})

test_that("the clay of the upper 20 cm is that of the mixed material", {
  # A of 12 cm with 16% and 8 cm of a B with 44%: 27.2%, under 300 g/kg; the
  # plain mean of the two layers, 30%, was tested
  p <- .vt(c("A1", "Cv1", "Cv2"), c(0, 12, 20), c(12, 20, 60), c(16, 44, 47),
           slickensides = c(NA, "Comum", "Comum"))
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  v <- vertissolo(p)
  expect_equal(round(v$evidence$surface_clay_pct, 1), 27.2)
  expect_false(isTRUE(v$passed))
  # A of 6 cm with 17% and 14 cm with 37%: 31%
  q <- .vt(c("A", "Cv"), c(0, 6), c(6, 60), c(17, 37), slickensides = c(NA, "Comum"))
  expect_equal(round(vertissolo(q)$evidence$surface_clay_pct, 1), 31)
  expect_true(isTRUE(vertissolo(q)$passed))
  # a layer that begins at 20 cm is no part of the upper 20 cm; with no clay
  # recorded above it the item is not met
  r <- .vt(c("A1", "Cv1", "Cv2"), c(0, 20, 40), c(20, 40, 80), c(NA, 49, 49),
           slickensides = c(NA, "Comum", "Comum"))
  expect_true(is.na(vertissolo(r)$evidence$surface_clay_pct))
  expect_false(isTRUE(vertissolo(r)$passed))
})

test_that("the canonical Vertissolo keeps its class", {
  p <- make_vertissolo_canonical()
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  expect_true(isTRUE(vertissolo(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Vertissolos")
})
