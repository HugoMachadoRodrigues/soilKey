# v0.7 SiBCS 5a ed.: testes do modulo 6 completo. Verifica que cada
# fixture canonica classifica para sua ordem alvo na chave do 1o nivel.

# ---- horizontes diagnosticos individuais -----------------------------------

test_that("horizonte_histico catches H >= 60 cm com OC alta", {
  pr <- make_organossolo_canonical()
  res <- horizonte_histico(pr)
  expect_true(isTRUE(res$passed))
})

test_that("horizonte_A_chernozemico catches dark + V alto + OC", {
  pr <- make_chernossolo_canonical()
  res <- horizonte_A_chernozemico(pr)
  expect_true(isTRUE(res$passed))
})

test_that("B_nitico catches clay-rich profile com B/A <= 1.5", {
  pr <- make_nitossolo_canonical()
  res <- B_nitico(pr)
  expect_true(isTRUE(res$passed))
  # Argissolo nao deve passar B_nitico (gradiente B/A > 1.5)
  pr2 <- make_argissolo_canonical()
  res2 <- B_nitico(pr2)
  expect_false(isTRUE(res2$passed))
})

test_that("B_planico catches mudanca textural abrupta + cores neutras", {
  pr <- make_planossolo_canonical()
  res <- B_planico(pr)
  expect_true(isTRUE(res$passed))
})


# ---- atributos diagnosticos -----------------------------------------------

test_that("atividade_argila_alta classifica Luvissolo como Ta", {
  pr <- make_luvissolo_canonical()
  res <- atividade_argila_alta(pr)
  expect_true(isTRUE(res$passed))
})

test_that("atividade_argila_alta classifica Nitossolo como Tb (baixa)", {
  pr <- make_nitossolo_canonical()
  res <- atividade_argila_alta(pr)
  expect_false(isTRUE(res$passed))
})

test_that("eutrofico catches V >= 50% no horizonte B/C", {
  pr <- make_luvissolo_canonical()
  res <- eutrofico(pr)
  expect_true(isTRUE(res$passed))
  # Argissolo distrofico
  pr2 <- make_argissolo_canonical()
  res2 <- eutrofico(pr2)
  expect_false(isTRUE(res2$passed))
})

test_that("carater_carbonatico detecta CaCO3 >= 15%", {
  pr <- make_chernossolo_canonical()
  pr$horizons$caco3_pct[4] <- 20    # turn Bk into carbonatico
  res <- carater_carbonatico(pr)
  expect_true(isTRUE(res$passed))
})


# ---- gates RSG-level (Cap 4) ----------------------------------------------

test_that("organossolo passa para fixture Organossolo", {
  pr <- make_organossolo_canonical()
  res <- organossolo(pr)
  expect_true(isTRUE(res$passed))
})

test_that("neossolo passa para fixture Neossolo (raso, sem B)", {
  pr <- make_neossolo_canonical()
  res <- neossolo(pr)
  expect_true(isTRUE(res$passed))
})

test_that("vertissolo passa para fixture Vertissolo", {
  pr <- make_vertissolo_canonical()
  res <- vertissolo(pr)
  expect_true(isTRUE(res$passed))
})

test_that("latossolo passa para fixture Latossolo (Ferralsol BR)", {
  pr <- make_latossolo_canonical()
  res <- latossolo(pr)
  expect_true(isTRUE(res$passed))
})

test_that("planossolo passa para fixture Planossolo", {
  pr <- make_planossolo_canonical()
  res <- planossolo(pr)
  expect_true(isTRUE(res$passed))
})

test_that("nitossolo passa para fixture Nitossolo", {
  pr <- make_nitossolo_canonical()
  res <- nitossolo(pr)
  expect_true(isTRUE(res$passed))
})

test_that("argissolo passa para fixture Argissolo (catch-all)", {
  pr <- make_argissolo_canonical()
  res <- argissolo(pr)
  expect_true(isTRUE(res$passed))
})


# ---- end-to-end: classify_sibcs com cada fixture --------------------------

test_that("classify_sibcs atribui cada fixture canonica a sua ordem alvo", {
  expected <- list(
    O = "Organossolos",
    R = "Neossolos",
    V = "Vertissolos",
    E = "Espodossolos",
    S = "Planossolos",
    G = "Gleissolos",
    L = "Latossolos",
    M = "Chernossolos",
    C = "Cambissolos",
    F = "Plintossolos",
    T = "Luvissolos",
    N = "Nitossolos",
    P = "Argissolos"
  )
  fixfns <- list(
    O = make_organossolo_canonical,
    R = make_neossolo_canonical,
    V = make_vertissolo_canonical,
    E = make_espodossolo_canonical,
    S = make_planossolo_canonical,
    G = make_gleissolo_canonical,
    L = make_latossolo_canonical,
    M = make_chernossolo_canonical,
    C = make_cambissolo_canonical,
    F = make_plintossolo_canonical,
    T = make_luvissolo_canonical,
    N = make_nitossolo_canonical,
    P = make_argissolo_canonical
  )
  for (code in names(fixfns)) {
    pr <- fixfns[[code]]()
    res <- classify_sibcs(pr, on_missing = "silent")
    expect_equal(res$rsg_or_order, expected[[code]],
                 info = sprintf("fixture %s expected %s, got %s",
                                  code, expected[[code]], res$rsg_or_order))
  }
})
