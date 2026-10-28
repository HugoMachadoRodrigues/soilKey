# v0.9.296: the SiBCS family label (5th level) uses the wording of the
# book (SiBCS 2018, Cap 18): "textura muito argilosa", "A moderado",
# not the internal codes ("muito_argilosa, moderado"). The codes in
# FamilyAttribute$value do not change.

.fa <- function(n, v) FamilyAttribute$new(name = n, value = v)

test_that("texture comes with 'textura', gravel and esquelética without a comma", {
  skip_on_cran()
  lab <- familia_label(list(
    .fa("grupamento_textural", "muito_argilosa"),
    .fa("distribuicao_cascalhos", "pouco_cascalhenta"),
    .fa("constituicao_esqueletica", "esquelética"),
    .fa("tipo_horizonte_superficial", "moderado"),
    .fa("saturacao_aluminio", "epiálico"),
    .fa("mineralogia_areia", "micácea"),
    .fa("atividade_argila", "Tmob"),
    .fa("oxidos_ferro", "mesoférrico")))
  expect_identical(lab, paste0(
    "textura muito argilosa pouco cascalhenta esquelética (micácea), ",
    "A moderado, epiálico, Tmob, mesoférrico"))
})

test_that("subgroupings, surface horizons and Organossolos read as in the book", {
  skip_on_cran()
  expect_identical(familia_label(list(.fa("subgrupamento_textural", "media-arenosa"))),
                   "textura média-arenosa")
  expect_identical(familia_label(list(.fa("subgrupamento_textural", "arenosa-media"))),
                   "textura arenosa-média")
  expect_identical(familia_label(list(.fa("grupamento_textural", "média"),
                                      .fa("tipo_horizonte_superficial", "húmico"))),
                   "textura média, A húmico")
  # Hugo's decision, 2026-10-28: the histic horizon reads "horizonte histico"
  expect_identical(familia_label(list(.fa("tipo_horizonte_superficial", "hístico"))),
                   "horizonte hístico")
  expect_identical(familia_label(list(
    .fa("material_subjacente", "muito_argiloso"),
    .fa("espessura_organica", "espesso"),
    .fa("lenhosidade", "extremamente_lenhoso"))),
    "material subjacente muito argiloso, espesso, extremamente lenhoso")
  expect_identical(familia_label(list(.fa("material_subjacente", "medio"))),
                   "material subjacente médio")
  # a gravel class with no texture still shows
  expect_identical(familia_label(list(.fa("grupamento_textural", NULL),
                                      .fa("distribuicao_cascalhos", "cascalhenta"))),
                   "cascalhenta")
  # an unknown dimension shows its value, underscores read as spaces
  expect_identical(familia_label(list(.fa("x", "algo_novo"))), "algo novo")
})

test_that("classified names carry the book's terms and no internal codes", {
  skip_on_cran()
  r <- classify_sibcs(make_ferralsol_canonical(), on_missing = "silent",
                      include_familia = TRUE)
  expect_match(r$name, ", textura argilosa, A moderado$")
  expect_identical(r$trace$familia_label, "textura argilosa, A moderado")
  # the codes are unchanged
  expect_identical(r$trace$familia$grupamento_textural$value, "argilosa")
  fx <- grep("^make_.*_canonical$", getNamespaceExports("soilKey"), value = TRUE)
  for (f in fx) {
    p <- get(f)()
    if (!inherits(p, "PedonRecord") || !nrow(p$horizons)) next
    nm <- classify_sibcs(p, on_missing = "silent", include_familia = TRUE)$name
    expect_false(grepl("_", nm, fixed = TRUE), info = paste(f, nm))
  }
})
