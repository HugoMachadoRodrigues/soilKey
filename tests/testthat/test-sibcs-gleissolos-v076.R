# v0.7.6 SiBCS Cap 9 (Gleissolos) -- 20 GGs + 128 SGs end-to-end.
# 1 diagnostico novo: carater_tionico.

test_that("carater_tionico passes via sulfidic_s_pct in [100, 150] cm", {
  hz <- data.table::data.table(
    top_cm    = c(0,    30,  120),
    bottom_cm = c(30,  120,  200),
    designation = c("A", "Bg", "Cg"),
    sulfidic_s_pct = c(NA_real_, NA_real_, 0.8)   # in tionic window
  )
  pr <- PedonRecord$new(
    site = list(id = "TN", lat = -1, lon = -48, country = "BR",
                  parent_material = "manguezal"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_tionico(pr)$passed))
})

test_that("carater_tionico FAILS quando sulfidic <100 cm (eh Tiomorfico)", {
  hz <- data.table::data.table(
    top_cm    = c(0,    30,  90),
    bottom_cm = c(30,   90, 200),
    designation = c("A", "Bg", "Bj"),
    sulfidic_s_pct = c(NA_real_, NA_real_, 0.8)   # < 100 cm
  )
  pr <- PedonRecord$new(
    site = list(id = "TN-shallow", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_tionico(pr)$passed))
})

test_that("Cap 9 Gleissolos GGs: 2+2+8+8 = 20 classes em 4 subordens", {
  rules <- load_rules("sibcs5")
  for (sub in c("GJ","GZ","GM","GX")) {
    expect_true(sub %in% names(rules$grandes_grupos))
  }
  expect_equal(length(rules$grandes_grupos$GJ),  2L)
  expect_equal(length(rules$grandes_grupos$GZ),  2L)
  expect_equal(length(rules$grandes_grupos$GM),  8L)
  expect_equal(length(rules$grandes_grupos$GX),  8L)
  total <- sum(vapply(rules$grandes_grupos[c("GJ","GZ","GM","GX")],
                          length, integer(1)))
  expect_equal(total, 20L)
})

test_that("Cap 9 Gleissolos SGs: 5+4+5+3+...+9 = 128 classes em 20 GGs", {
  rules <- load_rules("sibcs5")
  gj_ggs <- c("GJh","GJo")
  gz_ggs <- c("GZsd","GZo")
  gm_ggs <- c("GMca","GMsd","GMtal","GMtd","GMte","GMbal","GMbd","GMbe")
  gx_ggs <- c("GXca","GXsd","GXtal","GXtd","GXte","GXbal","GXbd","GXbe")
  expect_equal(sum(vapply(rules$subgrupos[gj_ggs], length, integer(1))),  9L)
  expect_equal(sum(vapply(rules$subgrupos[gz_ggs], length, integer(1))),  8L)
  expect_equal(sum(vapply(rules$subgrupos[gm_ggs], length, integer(1))), 60L)
  expect_equal(sum(vapply(rules$subgrupos[gx_ggs], length, integer(1))), 51L)
  total <- sum(vapply(rules$subgrupos[c(gj_ggs,gz_ggs,gm_ggs,gx_ggs)],
                          length, integer(1)))
  expect_equal(total, 128L)
})

test_that("cada Cap 9 GG termina em 'Tp' (catch-all default:true)", {
  rules <- load_rules("sibcs5")
  for (gg in c("GJh","GJo","GZsd","GZo",
                  "GMca","GMsd","GMtal","GMtd","GMte","GMbal","GMbd","GMbe",
                  "GXca","GXsd","GXtal","GXtd","GXte","GXbal","GXbd","GXbe")) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s: %s", gg, last$code))
    expect_true(endsWith(last$code, "Tp"))
  }
})

test_that("Cap 14+5+6+7+8+9 GGs preservados", {
  rules <- load_rules("sibcs5")
  total_ggs <- sum(vapply(rules$grandes_grupos, length, integer(1)))
  expect_gte(total_ggs, 81L + 20L)   # >= 101
})

test_that("WRB / USDA inalterados apos Cap 9 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
