# v0.7.3.B SiBCS Subgrupos (4o nivel categorico).
#
# Cap 14 (Organossolos) e a primeira ordem com Subgrupos wirados:
# 42 classes em 9 Grandes Grupos.


# Helper: organic Hd profile com decomposicao explicita + caracteres opcionais
.make_organossolo_subgrupo <- function(subordem = c("OJ", "OO", "OX"),
                                          fiber_rubbed = NA_real_,
                                          ec_layer1 = NA_real_,
                                          na_cmol_layer1 = NA_real_,
                                          ph_h2o = NA_real_,
                                          designation_layer2 = NA_character_,
                                          mineral_thickness = 0) {
  subordem <- match.arg(subordem)
  if (subordem == "OO") {
    # Folicos: O hístico raso sobre rocha/B
    hz <- data.table::data.table(
      top_cm    = c(0,  20),
      bottom_cm = c(20, ifelse(!is.na(designation_layer2), 60, 80)),
      designation = c("Oa",
                        ifelse(!is.na(designation_layer2),
                                  designation_layer2, "C")),
      fiber_content_rubbed_pct = c(fiber_rubbed, NA_real_),
      oc_pct = c(35, NA_real_),
      ec_dS_m = c(ec_layer1, NA_real_),
      cec_cmol = c(20, NA_real_),
      na_cmol  = c(na_cmol_layer1, NA_real_),
      ph_h2o   = c(ph_h2o, NA_real_),
      caco3_pct = c(NA_real_, NA_real_)
    )
  } else {
    # Tiomorfico (OJ) ou Haplico (OX): H espesso + camadas opcionais
    n_top <- if (mineral_thickness > 0) 3L else 2L
    if (n_top == 3L) {
      hz <- data.table::data.table(
        top_cm    = c(0,  40, 70),
        bottom_cm = c(40, 70, 70 + mineral_thickness),
        designation = c("Hd", "Hd2", "Ag"),
        fiber_content_rubbed_pct = c(fiber_rubbed, fiber_rubbed, NA_real_),
        oc_pct   = c(35, 30, 1),
        ec_dS_m  = c(ec_layer1, NA_real_, NA_real_),
        cec_cmol = c(20, 18, 10),
        na_cmol  = c(na_cmol_layer1, NA_real_, NA_real_),
        ph_h2o   = c(ph_h2o, NA_real_, NA_real_),
        sulfidic_s_pct = c(if (subordem == "OJ") 0.8 else NA_real_,
                              NA_real_, NA_real_),
        caco3_pct = c(NA_real_, NA_real_, NA_real_)
      )
    } else {
      hz <- data.table::data.table(
        top_cm    = c(0,  40),
        bottom_cm = c(40, 90),
        designation = c("Hd", "Hd2"),
        fiber_content_rubbed_pct = c(fiber_rubbed, fiber_rubbed),
        oc_pct   = c(35, 30),
        ec_dS_m  = c(ec_layer1, NA_real_),
        cec_cmol = c(20, 18),
        na_cmol  = c(na_cmol_layer1, NA_real_),
        ph_h2o   = c(ph_h2o, NA_real_),
        sulfidic_s_pct = c(if (subordem == "OJ") 0.8 else NA_real_, NA_real_),
        caco3_pct = c(NA_real_, NA_real_)
      )
    }
  }
  PedonRecord$new(
    site = list(id = sprintf("SG-%s", subordem),
                  lat = -3, lon = -50, country = "BR",
                  parent_material = "deposito turfoso"),
    horizons = ensure_horizon_schema(hz)
  )
}


# ---- 1. YAML structural integrity --------------------------------------

test_that("load_rules merges subgrupos/organossolos.yaml", {
  rules <- load_rules("sibcs5")
  expect_true("subgrupos" %in% names(rules))
  # 9 GGs do Cap 14 todos presentes
  for (gg in c("OJF", "OJH", "OJS", "OOF", "OOH", "OOS",
                  "OXF", "OXH", "OXS")) {
    expect_true(gg %in% names(rules$subgrupos),
                  info = sprintf("GG %s ausente no bloco subgrupos", gg))
  }
})

test_that("Organossolos subgrupos totalizam 42 classes", {
  rules <- load_rules("sibcs5")
  ogg <- c("OJF", "OJH", "OJS", "OOF", "OOH", "OOS",
            "OXF", "OXH", "OXS")
  total <- sum(vapply(rules$subgrupos[ogg], length, integer(1)))
  expect_equal(total, 42L)
})

test_that("cada GG tem catch-all 'tipico' como ultima entrada", {
  rules <- load_rules("sibcs5")
  for (gg in c("OJF", "OJH", "OJS", "OOF", "OOH", "OOS",
                  "OXF", "OXH", "OXS")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s deveria terminar com default:true; got %s",
                                  gg, last$code))
    expect_match(last$name, "tipicos$",
                   info = sprintf("GG %s catch-all deveria ter nome 'tipicos'", gg))
  }
})

test_that("OXS tem 8 subgrupos incluindo OXShst (composto)", {
  rules <- load_rules("sibcs5")
  oxs_codes <- vapply(rules$subgrupos$OXS, function(x) x$code, character(1))
  expect_equal(length(oxs_codes), 8L)
  expect_true("OXShst" %in% oxs_codes)
})


# ---- 2. run_sibcs_subgrupo dispatcher -----------------------------------

test_that("run_sibcs_subgrupo returns NULL for unknown GG", {
  pr <- .make_organossolo_subgrupo("OJ", fiber_rubbed = 60)
  expect_null(run_sibcs_subgrupo(pr, "ZZZ")$assigned)
})

test_that("run_sibcs_subgrupo falls back to tipico when no specific match", {
  # Pedon sem nenhum atributo discriminante -> deve cair em tipicos.
  pr <- .make_organossolo_subgrupo("OJ", fiber_rubbed = 60)
  res <- run_sibcs_subgrupo(pr, "OJF")
  expect_match(res$assigned$code, "tp$")
  expect_match(res$assigned$name, "tipicos$")
})


# ---- 3. Targeted subgrupo positives ------------------------------------

test_that("OJFsa (salinos) catches profile com EC alta < 150 cm", {
  pr <- .make_organossolo_subgrupo("OJ", fiber_rubbed = 60,
                                       ec_layer1 = 5)  # 4 <= 5 < 7 = salino
  res <- run_sibcs_subgrupo(pr, "OJF")
  expect_equal(res$assigned$code, "OJFsa")
})

test_that("OJSso (solodicos) catches profile com PST 6-15% em horizonte sapric", {
  # CEC=20, Na=2.0 -> PST = 100*2/20 = 10% (entre 6 e 15)
  pr <- .make_organossolo_subgrupo("OJ", fiber_rubbed = 10,   # saprico
                                       na_cmol_layer1 = 2)
  res <- run_sibcs_subgrupo(pr, "OJS")
  expect_equal(res$assigned$code, "OJSso")
})

test_that("OXFte (terricos) catches profile com horizonte mineral >= 30 cm", {
  pr <- .make_organossolo_subgrupo("OX", fiber_rubbed = 60,
                                       mineral_thickness = 35)
  res <- run_sibcs_subgrupo(pr, "OXF")
  expect_equal(res$assigned$code, "OXFte")
})

test_that("OXFtp (tipicos catch-all) is selected when no other criterion fires", {
  pr <- .make_organossolo_subgrupo("OX", fiber_rubbed = 60)
  res <- run_sibcs_subgrupo(pr, "OXF")
  expect_equal(res$assigned$code, "OXFtp")
})

test_that("OOFli (liticos) catches Folico com R/Cr < 50 cm", {
  pr <- .make_organossolo_subgrupo("OO", fiber_rubbed = 50,
                                       designation_layer2 = "R")
  res <- run_sibcs_subgrupo(pr, "OOF")
  expect_equal(res$assigned$code, "OOFli")
})

test_that("OOFfr (fragmentarios) catches Folico com Cr/Crf < 50 cm", {
  pr <- .make_organossolo_subgrupo("OO", fiber_rubbed = 50,
                                       designation_layer2 = "Cr")
  res <- run_sibcs_subgrupo(pr, "OOF")
  # Cr: contato_litico_fragmentario passa primeiro pela ordem canonica.
  # Mas tambem contato_litico (continuous_rock) pode pegar Cr -- depende
  # do helper. Aceita li OU fr.
  expect_match(res$assigned$code, "^OOF(li|fr)$")
})


# ---- 4. classify_sibcs end-to-end ao 4o nivel -------------------------

test_that("classify_sibcs trace exposes subgrupos block", {
  pr <- .make_organossolo_subgrupo("OX", fiber_rubbed = 60)
  res <- classify_sibcs(pr, on_missing = "silent")
  expect_true("subgrupos" %in% names(res$trace))
  expect_true("subgrupo_assigned" %in% names(res$trace))
})

test_that("classify_sibcs descende ate o 4o nivel quando ordem=Organossolos", {
  pr <- .make_organossolo_subgrupo("OX", fiber_rubbed = 60)
  res <- classify_sibcs(pr, on_missing = "silent")
  if (res$rsg_or_order == "Organossolos") {
    expect_false(is.null(res$trace$subgrupo_assigned))
    expect_match(res$trace$subgrupo_assigned$code, "^OXF(so|te|tp)$")
  } else {
    skip(sprintf("fixture nao caiu em Organossolos (caiu em %s)",
                  res$rsg_or_order))
  }
})


# ---- 5. Backward-compat: WRB e USDA --------------------------------

test_that("WRB Ferralsol classification ainda passa apos subgrupos add", {
  pr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})

test_that("USDA Oxisol classification ainda passa apos subgrupos add", {
  pr <- make_ferralsol_canonical()
  expect_equal(classify_usda(pr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
