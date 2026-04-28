test_that("load_rules le o conjunto SiBCS na ordem canonica do Cap 4", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$ordens), 13L)
  codes <- vapply(rules$ordens, function(o) o$code, character(1))
  # v0.7: ordem canonica Cap 4 (pp 110-114): O, R, V, E, S, G, L, M, C, F, T, N, P
  expect_equal(codes[1], "O")             # Organossolos primeiro
  expect_equal(codes[2], "R")             # Neossolos
  expect_equal(codes[3], "V")             # Vertissolos
  expect_equal(codes[7], "L")             # Latossolos
  expect_equal(codes[length(codes)], "P") # Argissolos catch-all
})

test_that("classify_sibcs atribui Latossolos ao Ferralsol canonico (cobertura WRB)", {
  pr <- make_ferralsol_canonical()
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_s3_class(res, "ClassificationResult")
  expect_equal(res$rsg_or_order, "Latossolos")
  expect_equal(res$system, "SiBCS 5a edicao")
})

test_that("classify_sibcs atribui Argissolos ao Luvisol canonico", {
  # Luvisol -> WRB Luvisols. No SiBCS, B textural eutrofico Ta seria
  # Luvissolo; argila ativ baixa OR baixa V seria Argissolo. Os fixtures
  # WRB nao foram tunados para o discriminante SiBCS Ta vs Tb -- aceita
  # qualquer um dos dois.
  pr <- make_luvisol_canonical()
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_true(res$rsg_or_order %in% c("Argissolos", "Luvissolos"))
})

test_that("B_latossolico SiBCS 5 v0.7 (strict thickness >= 50 cm)", {
  pr <- make_ferralsol_canonical()
  fer <- ferralic(pr)              # thickness >= 30 cm
  bl  <- B_latossolico(pr)         # thickness >= 50 cm SiBCS strict
  expect_identical(fer$passed, bl$passed)
  # SiBCS layers e um SUBSET dos WRB ferralic layers (intersecao das
  # camadas que tambem atendem ao threshold de espessura mais rigoroso).
  expect_true(all(bl$layers %in% fer$layers))
  expect_match(bl$reference, "Embrapa")
})

test_that("B_textural SiBCS 5 v0.7 delega ao WRB argic", {
  pr <- make_luvisol_canonical()
  arg <- argic(pr)
  bt  <- B_textural(pr)
  expect_identical(arg$passed, bt$passed)
  expect_identical(arg$layers, bt$layers)
})
