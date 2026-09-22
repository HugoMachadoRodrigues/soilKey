# v0.7.8 SiBCS Cap 11 (Luvissolos) -- 5 GGs + 33 SGs end-to-end.
# 1 diagnostico novo: carater_palico (solum > 80 cm).

test_that("carater_palico passes para solum > 80 cm", {
  hz <- data.table::data.table(
    top_cm    = c(0,   30,  100, 200),
    bottom_cm = c(30, 100, 200, 250),
    designation = c("A", "Bt", "Bt2", "BC")    # solum = 0-200 = 200 cm > 80
  )
  pr <- PedonRecord$new(
    site = list(id = "PA", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_true(isTRUE(carater_palico(pr)$passed))
})

test_that("carater_palico FAILS para solum <= 80 cm", {
  hz <- data.table::data.table(
    top_cm    = c(0,  30, 60),
    bottom_cm = c(30, 60, 150),
    designation = c("A", "Bt", "BC")    # solum (A+B) = 0-60 = 60 cm < 80
  )
  pr <- PedonRecord$new(
    site = list(id = "PA-FAIL", lat = 0, lon = 0, country = "TEST",
                  parent_material = "test"),
    horizons = ensure_horizon_schema(hz)
  )
  expect_false(isTRUE(carater_palico(pr)$passed))
})

test_that("Cap 11 GGs: 3+2 = 5 classes em 2 subordens", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$grandes_grupos$TC), 3L)
  expect_equal(length(rules$grandes_grupos$TX), 2L)
})

test_that("Cap 11 SGs: 3+9+10+8+3 = 33 classes em 5 GGs", {
  rules <- load_rules("sibcs5")
  expect_equal(length(rules$subgrupos$TCca),  3L)
  expect_equal(length(rules$subgrupos$TCp),   9L)
  expect_equal(length(rules$subgrupos$TCo),  10L)
  expect_equal(length(rules$subgrupos$TXp),   8L)
  expect_equal(length(rules$subgrupos$TXo),   3L)
  total <- sum(vapply(rules$subgrupos[c("TCca","TCp","TCo","TXp","TXo")],
                          length, integer(1)))
  expect_equal(total, 33L)
})

test_that("WRB / USDA inalterados apos Cap 11 add", {
  pr_fr <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(pr_fr, on_missing = "silent")$rsg_or_order,
                 "Ferralsols")
})
