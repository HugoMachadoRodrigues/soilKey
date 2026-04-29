# v0.7.13 SiBCS Cap 17 (Vertissolos) -- 11 GGs + 42 SGs end-to-end.
# ZERO novos diagnosticos.

test_that("Cap 17 GGs: 4+3+4 = 11 classes em 3 subordens", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$grandes_grupos$VG), 4L)
  expect_equal(length(rules$grandes_grupos$VE), 3L)
  expect_equal(length(rules$grandes_grupos$VX), 4L)
})

test_that("Cap 17 SGs: 9+8+25 = 42 classes em 11 GGs", {
  rules <- load_rules("sibcs5")
  vg_ggs <- c("VGca", "VGsd", "VGsl", "VGo")
  ve_ggs <- c("VEca", "VEsd", "VEo")
  vx_ggs <- c("VXca", "VXsd", "VXsl", "VXo")
  # VG: 2 + 2 + 2 + 3 = 9
  expect_equal(sum(vapply(rules$subgrupos[vg_ggs], length, integer(1))), 9L)
  # VE: 2 + 2 + 4 = 8
  expect_equal(sum(vapply(rules$subgrupos[ve_ggs], length, integer(1))), 8L)
  # VX: 6 + 5 + 5 + 9 = 25
  expect_equal(sum(vapply(rules$subgrupos[vx_ggs], length, integer(1))), 25L)
  total <- sum(vapply(rules$subgrupos[c(vg_ggs, ve_ggs, vx_ggs)],
                          length, integer(1)))
  expect_equal(total, 42L)
})

test_that("WRB / USDA inalterados apos Cap 17 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
