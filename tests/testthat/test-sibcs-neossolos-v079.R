# v0.7.9 SiBCS Cap 12 (Neossolos) -- 19 GGs + 75 SGs end-to-end. ZERO novos diagn.

test_that("Cap 12 GGs: 6+7+4+2 = 19 classes em 4 subordens", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$grandes_grupos$RL), 6L)
  expect_equal(length(rules$grandes_grupos$RY), 7L)
  expect_equal(length(rules$grandes_grupos$RR), 4L)
  expect_equal(length(rules$grandes_grupos$RQ), 2L)
})

test_that("Cap 12 SGs: 11+23+23+18 = 75 classes em 19 GGs", {
  rules <- load_rules("sibcs5")
  rl_ggs <- c("RLh","RLhu","RLca","RLch","RLd","RLe")
  ry_ggs <- c("RYca","RYsd","RYsl","RYps","RYte","RYbd","RYbe")
  rr_ggs <- c("RRps","RRhu","RRd","RRe")
  rq_ggs <- c("RQhm","RQo")
  expect_equal(sum(vapply(rules$subgrupos[rl_ggs], length, integer(1))), 11L)
  expect_equal(sum(vapply(rules$subgrupos[ry_ggs], length, integer(1))), 23L)
  expect_equal(sum(vapply(rules$subgrupos[rr_ggs], length, integer(1))), 23L)
  expect_equal(sum(vapply(rules$subgrupos[rq_ggs], length, integer(1))), 18L)
})

test_that("WRB / USDA inalterados apos Cap 12 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
