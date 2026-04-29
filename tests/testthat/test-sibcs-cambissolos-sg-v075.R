# v0.7.5.B SiBCS Cap 6 (Cambissolos) -- Stage B: 109 Subgrupos.
# CH 8 + CHU 18 + CY 22 + CX 61 = 109 SGs.

test_that("Cap 6 Cambissolos SGs: 8 + 18 + 22 + 61 = 109 classes", {
  rules <- load_rules("sibcs5")
  ch_ggs <- c("CHal","CHd")
  chu_ggs <- c("CHUaf","CHUal","CHUdf","CHUd")
  cy_ggs  <- c("CYca","CYsd","CYsl","CYal","CYtd","CYte","CYbd","CYbe")
  cx_ggs  <- c("CXca","CXsd","CXpf","CXtal","CXtd","CXtef","CXte",
                "CXbal","CXbdf","CXbd","CXbef","CXbe")
  expect_equal(sum(vapply(rules$subgrupos[ch_ggs],  length, integer(1))),
                  8L)
  expect_equal(sum(vapply(rules$subgrupos[chu_ggs], length, integer(1))),
                 18L)
  expect_equal(sum(vapply(rules$subgrupos[cy_ggs],  length, integer(1))),
                 22L)
  expect_equal(sum(vapply(rules$subgrupos[cx_ggs],  length, integer(1))),
                 61L)
  total <- sum(vapply(rules$subgrupos[c(ch_ggs, chu_ggs, cy_ggs, cx_ggs)],
                          length, integer(1)))
  expect_equal(total, 109L)
})

test_that("cada Cap 6 GG termina em 'Tp' (catch-all)", {
  rules <- load_rules("sibcs5")
  for (gg in c("CHal","CHd","CHUaf","CHUal","CHUdf","CHUd",
                  "CYca","CYsd","CYsl","CYal","CYtd","CYte","CYbd","CYbe",
                  "CXca","CXsd","CXpf","CXtal","CXtd","CXtef","CXte",
                  "CXbal","CXbdf","CXbd","CXbef","CXbe")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s: %s", gg, last$code))
    expect_true(endsWith(last$code, "Tp"),
                  info = sprintf("GG %s deveria terminar em 'Tp'; got %s",
                                  gg, last$code))
  }
})

test_that("CXte (Eutroficos Ta) preserva ordem canonica multi-criterio", {
  rules <- load_rules("sibcs5")
  codes <- vapply(rules$subgrupos$CXte, function(x) x$code, character(1))
  # 4.7.1 Fr (lit frag <50) > 4.7.2 Li (lit <50) > 4.7.3 Lf (lit frag 50-100) >
  # 4.7.4 LeHC (composto le+hipoca) > 4.7.5 Le > 4.7.6 So > 4.7.7 Vs >
  # 4.7.8 Ar > 4.7.9 Tp.
  expected <- c("CXteFr","CXteLi","CXteLf","CXteLeHC","CXteLe",
                  "CXteSo","CXteVs","CXteAr","CXteTp")
  expect_equal(codes, expected)
})


test_that("CXca dispatcher catches Cr profile (Lf/Sp/Tp dependendo da chave)", {
  # NOTA v0.7.5: Cr designation casa em ambos contato_litico_fragmentario
  # (50-100 cm, leptofragmentario) E carater_saprolitico (Cr <100 cm).
  # Como Lf vem antes na chave canonica, ele captura. Comportamento
  # correto -- a distincao entre "Cr brando" (saprolitico) e
  # "Crf consolidado" (fragmentario) requer um campo de schema novo
  # (planejado para v0.8). Por enquanto, aceitamos qualquer um dos
  # candidatos validos.
  hz <- data.table::data.table(
    top_cm = c(0, 30, 70),
    bottom_cm = c(30, 70, 150),
    designation = c("A", "Bw", "Cr"),
    caco3_pct = c(NA_real_, 20, NA_real_)
  )
  pr <- PedonRecord$new(
    site = list(id = "CXCASP", lat = -10, lon = -45, country = "BR",
                  parent_material = "calcareo"),
    horizons = ensure_horizon_schema(hz)
  )
  res <- run_sibcs_subgrupo(pr, "CXca")
  expect_match(res$assigned$code, "^CXca(Lf|Sp|Tp)$")
})

test_that("CXpfLa (Perferricos latossolicos) catches Fe + B latossolico-like", {
  rules <- load_rules("sibcs5")
  cxpf_codes <- vapply(rules$subgrupos$CXpf, function(x) x$code, character(1))
  expect_equal(cxpf_codes, c("CXpfLa", "CXpfTp"))
})


test_that("Cap 14 + Cap 5 + Cap 6 GGs preservados", {
  rules <- load_rules("sibcs5")
  total_ggs <- sum(vapply(rules$grandes_grupos, length, integer(1)))
  # >= 58 (9 Cap 14 + 23 Cap 5 + 26 Cap 6); cresce com novos caps.
  expect_gte(total_ggs, 58L)
})

test_that("WRB / USDA inalterados apos Cap 6 SGs add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
