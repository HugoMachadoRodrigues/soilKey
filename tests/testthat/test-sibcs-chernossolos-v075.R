# v0.7.5.C SiBCS Cap 7 (Chernossolos) -- 11 GGs + 34 SGs end-to-end.
# Cap 7 NAO requer novos diagnosticos -- reusa todos existentes.

test_that("Cap 7 Chernossolos GGs: 3+2+3+3 = 11 classes em 4 subordens", {
  rules <- load_rules("sibcs5")
  for (sub in c("MD", "ME", "MT", "MX")) {
    expect_true(sub %in% names(rules$grandes_grupos),
                  info = sprintf("Subordem %s ausente", sub))
  }
  expect_equal(length(rules$grandes_grupos$MD), 3L)
  expect_equal(length(rules$grandes_grupos$ME), 2L)
  expect_equal(length(rules$grandes_grupos$MT), 3L)
  expect_equal(length(rules$grandes_grupos$MX), 3L)
  total <- sum(vapply(rules$grandes_grupos[c("MD","ME","MT","MX")],
                          length, integer(1)))
  expect_equal(total, 11L)
})

test_that("Cap 7 Chernossolos SGs: 5+4+15+10 = 34 classes em 11 GGs", {
  rules <- load_rules("sibcs5")
  md_ggs <- c("MDpc","MDli","MDo")
  me_ggs <- c("MEca","MEo")
  mt_ggs <- c("MTfe","MTca","MTo")
  mx_ggs <- c("MXfe","MXca","MXo")
  expect_equal(sum(vapply(rules$subgrupos[md_ggs], length, integer(1))), 5L)
  expect_equal(sum(vapply(rules$subgrupos[me_ggs], length, integer(1))), 4L)
  expect_equal(sum(vapply(rules$subgrupos[mt_ggs], length, integer(1))), 15L)
  expect_equal(sum(vapply(rules$subgrupos[mx_ggs], length, integer(1))), 10L)
  total <- sum(vapply(rules$subgrupos[c(md_ggs, me_ggs, mt_ggs, mx_ggs)],
                          length, integer(1)))
  expect_equal(total, 34L)
})

test_that("cada Cap 7 GG termina em 'Tp' OR e a-priori tipico-only", {
  rules <- load_rules("sibcs5")
  for (gg in c("MDpc","MDli","MDo","MEca","MEo",
                  "MTfe","MTca","MTo","MXfe","MXca","MXo")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s: %s", gg, last$code))
  }
})

test_that("MTo (Argiluvicos Orticos) preserva ordem canonica 9 SGs", {
  rules <- load_rules("sibcs5")
  codes <- vapply(rules$subgrupos$MTo, function(x) x$code, character(1))
  # Ordem: Lf > Le > Sp > So > Ab > Vs > EpRx > EnRx > Tp
  expected <- c("MToLf","MToLe","MToSp","MToSo","MToAb",
                  "MToVs","MToEpRx","MToEnRx","MToTp")
  expect_equal(codes, expected)
})

test_that("MEcaVs (Ebanicos Carbonaticos vertissolicos) usa carater_vertissolico", {
  rules <- load_rules("sibcs5")
  meca <- rules$subgrupos$MEca[[1]]
  expect_equal(meca$code, "MEcaVs")
  expect_true("carater_vertissolico" %in% names(meca$tests$all_of[[1]]))
})

test_that("Cap 14 + Cap 5 + Cap 6 + Cap 7 GGs preservados", {
  rules <- load_rules("sibcs5")
  total_ggs <- sum(vapply(rules$grandes_grupos, length, integer(1)))
  # >= 69 (9 + 23 + 26 + 11); cresce com novos caps.
  expect_gte(total_ggs, 69L)
})

test_that("WRB / USDA inalterados apos Cap 7 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
