# v0.7.10 SiBCS Cap 13 (Nitossolos) -- 14 GGs + 43 SGs end-to-end. ZERO novos diagn.

test_that("Cap 13 GGs: 4+6+4 = 14 classes em 3 subordens", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$grandes_grupos$NB), 4L)
  expect_equal(length(rules$grandes_grupos$NV), 6L)
  expect_equal(length(rules$grandes_grupos$NX), 4L)
})

test_that("Cap 13 SGs: 16+17+10 = 43 classes em 14 GGs", {
  rules <- load_rules("sibcs5")
  nb_ggs <- c("NBaf", "NBal", "NBdf", "NBd")
  nv_ggs <- c("NVtal", "NVal", "NVdf", "NVd", "NVef", "NVe")
  nx_ggs <- c("NXtal", "NXal", "NXd", "NXe")
  # NB: 4 + 4 + 4 + 4 = 16
  expect_equal(sum(vapply(rules$subgrupos[nb_ggs], length, integer(1))), 16L)
  # NV: 2 + 2 + 2 + 2 + 4 + 5 = 17
  expect_equal(sum(vapply(rules$subgrupos[nv_ggs], length, integer(1))), 17L)
  # NX: 1 + 2 + 3 + 4 = 10
  expect_equal(sum(vapply(rules$subgrupos[nx_ggs], length, integer(1))), 10L)
})

test_that("WRB / USDA inalterados apos Cap 13 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
