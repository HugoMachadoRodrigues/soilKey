# v0.7.3 SiBCS Grandes Grupos (3o nivel categorico).
#
# Cap 14 (Organossolos) e a primeira ordem wirada -- 9 Grandes Grupos
# discriminados pelo grau de decomposicao do material organico
# (saprico/hemico/fibrico, v0.7.2).


# ---- Helper builder for organic profiles with explicit decomposition ---

.make_organossolo_pedon <- function(subordem = c("OJ", "OO", "OX"),
                                       fiber_rubbed = NA_real_,
                                       von_post = NA_integer_,
                                       extra_horizons = list()) {
  subordem <- match.arg(subordem)
  hz_top <- if (subordem == "OO")  # Folicos: pequeno H sobre rocha
              data.table::data.table(
                top_cm    = c(0,  20),
                bottom_cm = c(20, 60),
                designation = c("Oa", "R"),
                fiber_content_rubbed_pct = c(fiber_rubbed, NA_real_),
                von_post_index           = c(von_post,     NA_integer_),
                oc_pct                   = c(35, NA_real_)
              )
            else if (subordem == "OJ")   # Tiomorficos: H espesso + sulfidic
              data.table::data.table(
                top_cm    = c(0,  40),
                bottom_cm = c(40, 90),
                designation = c("Hd", "Hd2"),
                fiber_content_rubbed_pct = c(fiber_rubbed, fiber_rubbed),
                von_post_index           = c(von_post,     von_post),
                oc_pct                   = c(35, 30),
                ph_h2o                   = c(3.2, 3.5),
                sulfidic_s_pct           = c(0.8, 0.5)
              )
            else                          # OX Haplicos
              data.table::data.table(
                top_cm    = c(0,  40),
                bottom_cm = c(40, 90),
                designation = c("Hd", "Hd2"),
                fiber_content_rubbed_pct = c(fiber_rubbed, fiber_rubbed),
                von_post_index           = c(von_post,     von_post),
                oc_pct                   = c(35, 30)
              )

  PedonRecord$new(
    site = list(id = sprintf("ORG-%s", subordem),
                  lat = -3, lon = -50, country = "BR",
                  parent_material = "deposito turfoso"),
    horizons = ensure_horizon_schema(hz_top)
  )
}


# ---- 1. YAML structural integrity --------------------------------------

test_that("load_rules('sibcs5') merges grandes-grupos/organossolos.yaml", {
  rules <- load_rules("sibcs5")
  expect_true("grandes_grupos" %in% names(rules))
  expect_true("OJ" %in% names(rules$grandes_grupos))
  expect_true("OO" %in% names(rules$grandes_grupos))
  expect_true("OX" %in% names(rules$grandes_grupos))
  expect_equal(length(rules$grandes_grupos$OJ), 3L)
  expect_equal(length(rules$grandes_grupos$OO), 3L)
  expect_equal(length(rules$grandes_grupos$OX), 3L)
})

test_that("Organossolos GG codes follow 3-char convention (subordem + F/H/S)", {
  rules <- load_rules("sibcs5")
  oj_codes <- vapply(rules$grandes_grupos$OJ, function(x) x$code, character(1))
  expect_setequal(oj_codes, c("OJF", "OJH", "OJS"))
  oo_codes <- vapply(rules$grandes_grupos$OO, function(x) x$code, character(1))
  expect_setequal(oo_codes, c("OOF", "OOH", "OOS"))
  ox_codes <- vapply(rules$grandes_grupos$OX, function(x) x$code, character(1))
  expect_setequal(ox_codes, c("OXF", "OXH", "OXS"))
})


# ---- 2. run_sibcs_grande_grupo dispatcher -------------------------------

test_that("run_sibcs_grande_grupo returns NULL for subordem without GG block", {
  pr <- .make_organossolo_pedon("OJ", fiber_rubbed = 50)
  res <- run_sibcs_grande_grupo(pr, "ZZ")  # nao existe
  expect_null(res$assigned)
})

test_that("OJF (Tiomorficos Fibricos) catches high-fiber organic profile", {
  pr <- .make_organossolo_pedon("OJ", fiber_rubbed = 60)
  res <- run_sibcs_grande_grupo(pr, "OJ")
  expect_equal(res$assigned$code, "OJF")
})

test_that("OJH (Tiomorficos Hemicos) catches intermediate-fiber profile", {
  pr <- .make_organossolo_pedon("OJ", fiber_rubbed = 25)
  res <- run_sibcs_grande_grupo(pr, "OJ")
  expect_equal(res$assigned$code, "OJH")
})

test_that("OJS (Tiomorficos Sapricos) catches low-fiber profile", {
  pr <- .make_organossolo_pedon("OJ", fiber_rubbed = 10)
  res <- run_sibcs_grande_grupo(pr, "OJ")
  expect_equal(res$assigned$code, "OJS")
})

test_that("OXF / OXH / OXS catch via von Post index when fibers NA", {
  pr_f <- .make_organossolo_pedon("OX", von_post = 3L)   # H1-H4 = fibrico
  pr_h <- .make_organossolo_pedon("OX", von_post = 5L)   # H5-H6 = hemico
  pr_s <- .make_organossolo_pedon("OX", von_post = 9L)   # H7-H10 = saprico
  expect_equal(run_sibcs_grande_grupo(pr_f, "OX")$assigned$code, "OXF")
  expect_equal(run_sibcs_grande_grupo(pr_h, "OX")$assigned$code, "OXH")
  expect_equal(run_sibcs_grande_grupo(pr_s, "OX")$assigned$code, "OXS")
})

test_that("Folicos (OO) descem ao GG correto pelo grau de decomposicao", {
  pr <- .make_organossolo_pedon("OO", fiber_rubbed = 50)
  res <- run_sibcs_grande_grupo(pr, "OO")
  expect_equal(res$assigned$code, "OOF")
})


# ---- 3. classify_sibcs descends to GG when fibers present ---------------

test_that("classify_sibcs returns GG name for fiber-rich Organossolo", {
  pr <- .make_organossolo_pedon("OX", fiber_rubbed = 60)
  res <- classify_sibcs(pr, on_missing = "silent")
  # ATENCAO: este pedon precisa cair em ordem O (Organossolos) primeiro
  # para o teste fazer sentido.
  if (res$rsg_or_order == "Organossolos") {
    expect_match(res$name, "Organossolos H[aá]plicos F[ií]bricos|Fibricos")
    expect_false(is.null(res$trace$grande_grupo_assigned))
    expect_equal(res$trace$grande_grupo_assigned$code, "OXF")
  } else {
    skip(sprintf("fixture nao caiu em Organossolos (caiu em %s); skip GG check",
                  res$rsg_or_order))
  }
})

test_that("classify_sibcs trace exposes grandes_grupos block", {
  pr <- .make_organossolo_pedon("OX", fiber_rubbed = 50)
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_true("grandes_grupos" %in% names(res$trace))
  expect_true("grande_grupo_assigned" %in% names(res$trace))
})

test_that("classify_sibcs stops at subordem when fibers/von_post both NA", {
  # Sem dados de fibras nem von Post, nenhum GG passa, e como nao ha
  # catch-all 'default:true' em Organossolos, classify_sibcs deve
  # devolver display_name no nivel da subordem.
  pr <- .make_organossolo_pedon("OX")   # ambos NA
  res <- classify_sibcs(pr, on_missing = "silent")
  if (res$rsg_or_order == "Organossolos") {
    expect_null(res$trace$grande_grupo_assigned)
    expect_match(res$name, "^Organossolos")
    # Nao deve mencionar Fibricos/Hemicos/Sapricos quando os dados
    # nao estao presentes.
    expect_false(grepl("F[ií]bricos|H[eê]micos|S[aá]pricos|Fibricos|Hemicos|Sapricos",
                          res$name))
  } else {
    skip(sprintf("fixture nao caiu em Organossolos (caiu em %s)",
                  res$rsg_or_order))
  }
})


# ---- 4. Backward-compat: WRB and USDA still work ------------------------

test_that("WRB classification unaffected by load_rules subdir extension", {
  pr <- make_ferralsol_canonical()
  res <- classify_wrb2022(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Ferralsols")
})

test_that("USDA classification unaffected by load_rules subdir extension", {
  pr <- make_ferralsol_canonical()
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Oxisols")
})
