test_that("load_rules le o conjunto SiBCS", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$ordens), 13L)
  codes <- vapply(rules$ordens, function(o) o$code, character(1))
  expect_equal(codes[length(codes)], "R")  # Neossolos catch-all
})

test_that("classify_sibcs atribui Latossolos ao Ferralsol canonico", {
  pr <- make_ferralsol_canonical()
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_s3_class(res, "ClassificationResult")
  expect_equal(res$rsg_or_order, "Latossolos")
  expect_equal(res$system, "SiBCS 5a edicao")
})

test_that("classify_sibcs atribui Argissolos ao Luvisol canonico", {
  pr <- make_luvisol_canonical()
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Argissolos")
})

test_that("classify_sibcs cai em Neossolos (default) para perfis sem path implementado", {
  for (fix_fn in list(make_chernozem_canonical, make_solonchak_canonical,
                        make_cambisol_canonical, make_vertisol_canonical)) {
    res <- classify_sibcs(fix_fn(), on_missing = "silent")
    expect_equal(res$rsg_or_order, "Neossolos")
    expect_true(any(grepl("v0\\.7", res$warnings)))
  }
})

test_that("B_latossolico delega ao ferralic", {
  pr <- make_ferralsol_canonical()
  fer <- ferralic(pr)
  bl  <- B_latossolico(pr)
  expect_identical(fer$passed, bl$passed)
  expect_identical(fer$layers, bl$layers)
  expect_match(bl$reference, "Embrapa")
})

test_that("B_textural delega ao argic", {
  pr <- make_luvisol_canonical()
  arg <- argic(pr)
  bt  <- B_textural(pr)
  expect_identical(arg$passed, bt$passed)
  expect_identical(arg$layers, bt$layers)
})
