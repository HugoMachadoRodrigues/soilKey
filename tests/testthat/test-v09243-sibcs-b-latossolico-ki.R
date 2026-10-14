# v0.9.243: the Ki index of the B latossolico.
#
# SiBCS 2018 (Cap 2, p. 59, item e) asks of the B latossolico a "Relacao
# molecular SiO2/Al2O3 (Ki) igual ou inferior a 2,2, sendo normalmente menor
# que 2,0", and tells the B incipiente from it by a Ki "maior que 2,2" (p. 61).
# The item was not tested: the profile schema has the silica and the alumina
# of the sulfuric attack, but the BDsolos loader did not read them, although
# the export records them in 71% of its rows.

.ki_profile <- function(ki, designation = c("A", "Bw1", "Bw2", "Bw3"),
                        top = c(0, 20, 45, 80), bottom = c(20, 45, 80, 115)) {
  # low-activity clay throughout; alumina of 20%, silica set by the Ki wanted
  n <- length(designation)
  al <- rep(20, n)
  si <- rep_len(ki, n) * al * 60.08 / 101.96
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    clay_pct = c(45, rep(50, n - 1L)), silt_pct = 15, sand_pct = c(40, rep(35, n - 1L)),
    cec_cmol = c(8, rep(4, n - 1L)),
    sio2_sulfuric_pct = si, al2o3_sulfuric_pct = al)
  PedonRecord$new(site = list(id = "ki", country = "BR", lat = -20, lon = -44),
                  horizons = ensure_horizon_schema(hz))
}

test_that("the Ki is computed from the silica and the alumina of the sulfuric attack", {
  p <- .ki_profile(c(2.5, 1.8, 1.9, 2.0))
  expect_equal(round(compute_ki(p$horizons$sio2_sulfuric_pct, p$horizons$al2o3_sulfuric_pct), 2),
               c(2.5, 1.8, 1.9, 2.0))
})

test_that("a B with Ki of 2.2 or less is a B latossolico; above it, it is none", {
  skip_on_cran()
  ok <- .ki_profile(c(2.6, 1.8, 1.9, 2.0))       # the A is not asked for its Ki
  r <- B_latossolico(ok)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:4)
  expect_equal(r$evidence$ki_above_2_2, 1L)
  expect_equal(classify_sibcs(ok, on_missing = "silent")$rsg_or_order, "Latossolos")
  over <- .ki_profile(c(2.6, 2.5, 2.4, 2.6))
  r <- B_latossolico(over)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$ki_above_2_2, 1:4)
  # the limit: "igual ou inferior a 2,2"
  expect_true(isTRUE(B_latossolico(.ki_profile(c(2.0, 2.2, 2.2, 2.2)))$passed))
  expect_false(isTRUE(B_latossolico(.ki_profile(c(2.0, 2.21, 2.21, 2.21)))$passed))
})

test_that("a sub-horizon above 2.2 is no part of the horizon, which then has to keep its 50 cm", {
  # B1 25 cm, B21 35 cm, B22 35 cm: with the B21 out, no 50 cm adjoin
  broken <- .ki_profile(c(2.0, 2.0, 2.3, 2.0))
  r <- B_latossolico(broken)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$ki_above_2_2, 3L)
  # with the first sub-horizon out, the two below make 70 cm
  upper <- .ki_profile(c(2.0, 2.3, 2.0, 2.0))
  r <- B_latossolico(upper)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3:4)
})

test_that("where the sulfuric attack is not recorded the horizon is tested as before", {
  skip_on_cran()
  p <- .ki_profile(c(2.6, 2.5, 2.4, 2.6))
  p$horizons$sio2_sulfuric_pct <- NA_real_
  r <- B_latossolico(p)
  expect_true(isTRUE(r$passed))
  expect_length(r$evidence$ki_above_2_2, 0L)
  # one of the two alone gives no Ki
  q <- .ki_profile(c(2.6, 2.5, 2.4, 2.6)); q$horizons$al2o3_sulfuric_pct <- NA_real_
  expect_true(isTRUE(B_latossolico(q)$passed))
  expect_true(isTRUE(B_latossolico(make_latossolo_canonical())$passed))
})

test_that("a Bw with Ki above 2.2 is the B incipiente the book tells from the B latossolico", {
  skip_on_cran()
  p <- .ki_profile(c(2.6, 2.5, 2.4, 2.6))
  expect_false(isTRUE(B_latossolico(p)$passed))
  expect_true(isTRUE(B_incipiente(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Cambissolos")
})

test_that("the BDsolos loader reads the silica and the alumina, in per cent", {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior",
           "Ataque sulfúrico - SiO2", "Ataque sulfúrico - Al2O3",
           "Ataque sulfúrico - Fe2O3", "Ataque sulfúrico - SiO2 / Al2O3 (Ki)")
  rows <- c(q("31", "MG", "LATOSSOLO VERMELHO", "A",  "0",  "20", "150", "180", "90", "1,42"),
            q("31", "MG", "LATOSSOLO VERMELHO", "Bw", "20", "120", "165", "212", "110", "1,32"),
            q("31", "MG", "LATOSSOLO VERMELHO", "C",  "120", "160", "", "", "", ""))
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  h <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))[[1]]$horizons
  expect_equal(h$sio2_sulfuric_pct, c(15, 16.5, NA))
  expect_equal(h$al2o3_sulfuric_pct, c(18, 21.2, NA))
  expect_equal(h$fe2o3_sulfuric_pct, c(9, 11, NA))
  # the index computed is the one the export gives
  expect_equal(round(compute_ki(h$sio2_sulfuric_pct, h$al2o3_sulfuric_pct), 2),
               c(1.41, 1.32, NA))
})
