# v0.7.7 SiBCS Cap 10 (Latossolos) -- 25 GGs + 99 SGs end-to-end.
# 2 diagnosticos novos: carater_rubrico, carater_psamitico.

test_that("carater_rubrico passes for B com hue 2.5YR + chroma 6", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_hue_moist = c("5YR", "2.5YR"),
    munsell_chroma_moist = c(4, 6)
  )
  pr <- PedonRecord$new(
    site = list(id = "RU", lat = -28, lon = -52, country = "BR",
                  parent_material = "basalto"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_rubrico(pr)$passed))
})

test_that("carater_rubrico FAILS para hue 7.5YR (mais amarelo)", {
  hz <- data.table::data.table(
    top_cm = c(0, 30),
    bottom_cm = c(30, 100),
    designation = c("A", "Bw"),
    munsell_hue_moist = c("10YR", "7.5YR"),
    munsell_chroma_moist = c(4, 6)
  )
  pr <- PedonRecord$new(
    site = list(id = "RUF", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_rubrico(pr)$passed))
})

test_that("carater_psamitico passes para clay < 20% nos primeiros 150 cm", {
  hz <- data.table::data.table(
    top_cm    = c(0,   50, 120),
    bottom_cm = c(50, 120, 200),
    designation = c("A", "AB", "Bw"),
    # Weighted: (10*50 + 12*70 + 18*30) / 150 = 12.93 < 20
    clay_pct  = c(10, 12, 18)
  )
  pr <- PedonRecord$new(
    site = list(id = "PS", lat = 0, lon = 0, country = "TEST",
                  parent_material = "areia"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_psamitico(pr)$passed))
})

test_that("carater_psamitico FAILS para clay >= 20% no 0-150 cm", {
  hz <- data.table::data.table(
    top_cm    = c(0, 30),
    bottom_cm = c(30, 200),
    designation = c("A", "Bw"),
    clay_pct  = c(25, 40)
  )
  pr <- PedonRecord$new(
    site = list(id = "PSF", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_psamitico(pr)$passed))
})

test_that("Cap 10 GGs: 4+7+8+6 = 25 classes em 4 subordens", {
  rules <- load_rules("sibcs5")
  for (sub in c("LB","LA","LV","LVA")) {
    expect_true(sub %in% names(rules$grandes_grupos))
  }
  expect_equal(length(rules$grandes_grupos$LB),  4L)
  expect_equal(length(rules$grandes_grupos$LA),  7L)
  expect_equal(length(rules$grandes_grupos$LV),  8L)
  expect_equal(length(rules$grandes_grupos$LVA), 6L)
  total <- sum(vapply(rules$grandes_grupos[c("LB","LA","LV","LVA")],
                          length, integer(1)))
  expect_equal(total, 25L)
})

test_that("Cap 10 SGs: 10+31+36+22 = 99 classes em 25 GGs", {
  rules <- load_rules("sibcs5")
  lb_ggs <- c("LBaf","LBal","LBdf","LBd")
  la_ggs <- c("LAaf","LAac","LAal","LAdf","LAdc","LAd","LAe")
  lv_ggs <- c("LVpf","LVaf","LVac","LValf","LVdf","LVd","LVef","LVe")
  lva_ggs <- c("LVAaf","LVAac","LVAal","LVAdf","LVAd","LVAe")
  expect_equal(sum(vapply(rules$subgrupos[lb_ggs],  length, integer(1))), 10L)
  expect_equal(sum(vapply(rules$subgrupos[la_ggs],  length, integer(1))), 31L)
  expect_equal(sum(vapply(rules$subgrupos[lv_ggs],  length, integer(1))), 36L)
  expect_equal(sum(vapply(rules$subgrupos[lva_ggs], length, integer(1))), 22L)
  total <- sum(vapply(rules$subgrupos[c(lb_ggs,la_ggs,lv_ggs,lva_ggs)],
                          length, integer(1)))
  expect_equal(total, 99L)
})

test_that("cada Cap 10 GG termina em 'Tp' default:true", {
  rules <- load_rules("sibcs5")
  all_la_ggs <- c("LBaf","LBal","LBdf","LBd",
                    "LAaf","LAac","LAal","LAdf","LAdc","LAd","LAe",
                    "LVpf","LVaf","LVac","LValf","LVdf","LVd","LVef","LVe",
                    "LVAaf","LVAac","LVAal","LVAdf","LVAd","LVAe")
  for (gg in all_la_ggs) {
    last <- rules$subgrupos[[gg]][[length(rules$subgrupos[[gg]])]]
    expect_true(isTRUE(last$tests$default),
                  info = sprintf("GG %s: %s", gg, last$code))
  }
})

test_that("WRB / USDA inalterados apos Cap 10 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
  expect_equal(classify_usda(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Oxisols")
})
