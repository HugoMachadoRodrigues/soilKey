# v0.9.297: the SiBCS family does not repeat what the class already says
# (Eutroficos / Distroficos -> no base saturation; Aluminicos -> no
# "alico"), the Organossolos give the texture only through the underlying
# material, and H/O horizons stay out of the texture averages in every
# order (Hugo's decisions, 2026-10-28).

test_that("a class that already states the base saturation does not repeat it", {
  skip_on_cran()
  r <- classify_sibcs(make_gleissolo_canonical(), on_missing = "silent",
                      include_familia = TRUE)
  expect_match(r$name, "Eutróficos")
  expect_false(grepl("eutrófico,|eutrófico$", r$trace$familia_label))
  expect_null(r$trace$familia$saturacao_bases)
  # without the class name the dimension is still there (the old call)
  f <- classify_sibcs_familia(make_gleissolo_canonical(), ordem_code = "G")
  expect_false(is.null(f$saturacao_bases))
  f <- classify_sibcs_familia(make_gleissolo_canonical(), ordem_code = "G",
                              nome_superior = "Gleissolos Tiomórficos Órticos típicos")
  expect_false(is.null(f$saturacao_bases))
})

test_that("a class that already states the aluminic character takes no 'alico'", {
  skip_on_cran()
  p <- make_espodossolo_canonical()
  f <- classify_sibcs_familia(p, ordem_code = "E")
  expect_identical(f$saturacao_aluminio$value, "epiálico")
  f <- classify_sibcs_familia(p, ordem_code = "E",
                              nome_superior = "Espodossolos Humilúvicos Alumínicos típicos")
  expect_null(f$saturacao_aluminio)
  f <- classify_sibcs_familia(p, ordem_code = "E",
                              nome_superior = "Espodossolos Ferrilúvicos Distróficos típicos")
  expect_null(f$saturacao_bases)
  expect_false(is.null(f$saturacao_aluminio))
})

test_that("Organossolos give the texture only through the underlying material", {
  skip_on_cran()
  for (fx in c("make_organossolo_canonical", "make_histosol_canonical")) {
    p <- get(fx)()
    f <- classify_sibcs_familia(p, ordem_code = "O")
    expect_null(f$grupamento_textural)
    expect_null(f$subgrupamento_textural)
    expect_null(f$distribuicao_cascalhos)
    expect_false(is.null(f$material_subjacente))
    r <- classify_sibcs(p, on_missing = "silent", include_familia = TRUE)
    expect_false(grepl("textura", r$name, fixed = TRUE), info = r$name)
    expect_match(r$name, "material subjacente", fixed = TRUE)
  }
})

test_that("H and O horizons stay out of the texture averages", {
  skip_on_cran()
  p <- make_espodossolo_canonical()
  h <- p$horizons
  h$clay_pct[1] <- 70                         # the Oa horizon, 0-5 cm
  h$coarse_fragments_pct[1] <- 90
  q <- PedonRecord$new(site = p$site, horizons = h)
  mineral <- h[h$designation != "Oa", ]
  w <- pmin(mineral$bottom_cm, 200) - mineral$top_cm
  all_w <- pmin(h$bottom_cm, 200) - h$top_cm
  # the organic horizon would have moved the average
  expect_false(isTRUE(all.equal(sum(h$clay_pct * all_w) / sum(all_w),
                                sum(mineral$clay_pct * w) / sum(w))))
  sg <- familia_subgrupamento_textural(q)
  expect_equal(sg$evidence$clay_pct_avg, sum(mineral$clay_pct * w) / sum(w))
  g <- familia_grupamento_textural(q)
  expect_equal(g$evidence$clay_pct_avg, sum(mineral$clay_pct * w) / sum(w))
  cf <- familia_distribuicao_cascalhos(q)
  expect_equal(cf$evidence$coarse_fragments_pct_avg,
               sum(mineral$coarse_fragments_pct * w) / sum(w))
  expect_null(familia_constituicao_esqueletica(q)$value)
})
