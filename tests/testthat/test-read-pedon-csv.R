# Tests for the simple CSV entry point: read_pedon_csv() / classify_csv().

test_that("read_pedon_csv() reads the bundled template into a PedonRecord", {
  f <- system.file("extdata", "perfil_exemplo.csv", package = "soilKey")
  skip_if(f == "" || !file.exists(f))
  p <- read_pedon_csv(f)
  expect_s3_class(p, "PedonRecord")
  expect_equal(nrow(p$horizons), 4L)
  expect_true(all(c("top_cm", "bottom_cm", "clay_pct") %in% names(p$horizons)))
  expect_equal(p$site$id, "perfil_exemplo")          # id from the file name
})

test_that("classify_csv() returns a one-row WRB/SiBCS/USDA data.frame", {
  f <- system.file("extdata", "perfil_exemplo.csv", package = "soilKey")
  skip_if(f == "" || !file.exists(f))
  out <- classify_csv(f)
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true(all(c("wrb", "sibcs", "usda") %in% names(out)))
  expect_match(out$wrb, "Ferralsol")                 # a red Latossolo
  expect_match(out$sibcs, "Latossolos")
  expect_false(is.null(attr(out, "results")))        # full results attached
  expect_s3_class(attr(out, "pedon"), "PedonRecord")
})

test_that("read_pedon_csv() works on an in-memory CSV without site + honours a site", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(
    top_cm = c(0, 20), bottom_cm = c(20, 90),
    designation = c("A", "Bw"),
    clay_pct = c(22, 30), silt_pct = c(30, 28), sand_pct = c(48, 42),
    ph_h2o = c(5.5, 5.6), bs_pct = c(60, 58)
  ), tmp, row.names = FALSE)

  p <- read_pedon_csv(tmp)
  expect_s3_class(p, "PedonRecord")
  expect_equal(nrow(p$horizons), 2L)

  p2 <- read_pedon_csv(tmp, site = list(id = "meu", country = "BR"))
  expect_equal(p2$site$id, "meu")
  expect_equal(p2$site$country, "BR")
})

test_that("read_pedon_csv() errors clearly when no canonical columns are present", {
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(foo = 1, bar = 2), tmp, row.names = FALSE)
  expect_error(read_pedon_csv(tmp), "no recognised horizon columns")
})

test_that("read_pedon_csv() errors on a missing file", {
  expect_error(read_pedon_csv("does-not-exist-42.csv"), "existing")
})
