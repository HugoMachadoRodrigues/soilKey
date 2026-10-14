# v0.9.257: the activity of the clay and the base saturation "na maior parte
# dos primeiros 100 cm do horizonte B (inclusive BA)".
#
# The key (SiBCS 2018, Cap 4, p. 113-114) gives the Luvissolos a "horizonte B
# textural com argila de atividade alta e saturacao por bases alta na maior
# parte dos primeiros 100 cm do horizonte B (inclusive BA)", and asks the same
# section of the Nitossolos; the definitions of the classes of the lower
# levels repeat it. Cap 1 (p. 30) takes the activity "no horizonte B
# (inclusive BA e exclusive BC) ou no horizonte C (inclusive CA), quando nao
# existe B", and does not apply it to the textural classes areia and areia
# franca.
#
# atividade_argila_alta() and eutrofico() passed with one layer of B or of C.
# The C of a profile, with little clay and a high ratio of CEC to clay, made a
# Luvissolo of an Argissolo and kept a B nitico from being one.

.mp_profile <- function(designation, top, bottom, clay, cec, bs, silt = 20) {
  hz <- data.table::data.table(designation = designation, top_cm = top,
                               bottom_cm = bottom, clay_pct = clay,
                               silt_pct = silt, sand_pct = 100 - clay - silt,
                               cec_cmol = cec, bs_pct = bs)
  PedonRecord$new(site = list(id = "mp", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("the section is the first 100 cm of the B, with the BA and without the BC", {
  p <- .mp_profile(c("A", "BA", "Bt1", "Bt2", "BC", "C"), c(0, 20, 40, 80, 150, 200),
                   c(20, 40, 80, 150, 200, 250), 30, 5, 40)
  s <- .sibcs_b_first_100(p$horizons)
  expect_equal(s$layers, 2:4)
  expect_equal(s$top_cm, 20)
  expect_equal(s$limit_cm, 120)
  expect_equal(s$thickness_cm, c(20, 40, 40))       # the Bt2 counts to 120 cm
  # a B that begins below the section is no part of it
  q <- .mp_profile(c("A", "Bt1", "C", "2Bt2"), c(0, 20, 60, 140), c(20, 60, 140, 200), 30, 5, 40)
  expect_equal(.sibcs_b_first_100(q$horizons)$layers, 2L)
  # the older notation: B1 is the BA of today, B3 the BC
  o <- .mp_profile(c("A1", "B1", "B21", "B22", "B3"), c(0, 15, 40, 80, 130),
                   c(15, 40, 80, 130, 180), 30, 5, 40)
  expect_equal(.sibcs_b_first_100(o$horizons)$layers, 2:4)
  # no B
  expect_null(.sibcs_b_first_100(.mp_profile(c("A", "C1", "C2"), c(0, 20, 60), c(20, 60, 120), 30, 5, 40)$horizons))
})

test_that("'a maior parte' is more than half of the thickness that says", {
  sec <- list(thickness_cm = c(20, 40, 40))
  expect_true(.sibcs_maior_parte(sec, c(FALSE, TRUE, TRUE))$passed)
  expect_false(.sibcs_maior_parte(sec, c(TRUE, FALSE, FALSE))$passed)
  expect_false(.sibcs_maior_parte(list(thickness_cm = c(50, 50)), c(TRUE, FALSE))$passed)   # half is not the larger part
  # a layer that does not say is left out of the two sides
  r <- .sibcs_maior_parte(sec, c(TRUE, NA, FALSE))
  expect_false(r$passed); expect_equal(r$known_cm, 60); expect_equal(r$holding_cm, 20)
  expect_true(.sibcs_maior_parte(sec, c(NA, TRUE, NA))$passed)
  expect_true(is.na(.sibcs_maior_parte(sec, c(NA, NA, NA))$passed))
})

test_that("the activity of the clay is that of most of the first 100 cm of the B", {
  skip_on_cran()
  # Tb in the whole B; the C, with less clay, has a high ratio
  p <- .mp_profile(c("A", "Bt1", "Bt2", "C"), c(0, 20, 60, 120), c(20, 60, 120, 180),
                   clay = c(20, 40, 45, 18), cec = c(6, 6, 6, 8), bs = 40)
  r <- atividade_argila_alta(p)
  expect_false(isTRUE(r$passed))
  expect_length(r$layers, 0L)
  expect_equal(r$evidence$thickness_known_cm, 100)
  # Ta in the first 40 cm of the B, Tb in the 60 cm below: not the larger part
  q <- .mp_profile(c("A", "Bt1", "Bt2", "C"), c(0, 20, 60, 120), c(20, 60, 120, 180),
                   clay = c(20, 40, 45, 18), cec = c(6, 12, 6, 8), bs = 40)
  r <- atividade_argila_alta(q)
  expect_false(isTRUE(r$passed))
  expect_equal(r$layers, 2L)
  expect_equal(r$evidence$thickness_high_cm, 40)
  # Ta in the 60 cm below: the larger part
  s <- .mp_profile(c("A", "Bt1", "Bt2", "C"), c(0, 20, 60, 120), c(20, 60, 120, 180),
                   clay = c(20, 40, 45, 18), cec = c(6, 6, 13, 3), bs = 40)
  expect_true(isTRUE(atividade_argila_alta(s)$passed))
  # the limit: 27 cmolc per kg of clay
  expect_true(isTRUE(atividade_argila_alta(.mp_profile(c("A", "Bt"), c(0, 20), c(20, 120), c(20, 40), c(6, 10.8), 40))$passed))
  expect_false(isTRUE(atividade_argila_alta(.mp_profile(c("A", "Bt"), c(0, 20), c(20, 120), c(20, 40), c(6, 10.7), 40))$passed))
  # the BC is no part of the B
  b <- .mp_profile(c("A", "Bt", "BC"), c(0, 20, 50), c(20, 50, 120), clay = c(20, 40, 20),
                   cec = c(6, 6, 9), bs = 40)
  expect_false(isTRUE(atividade_argila_alta(b)$passed))
})

test_that("a layer of areia or areia franca is left out; the franco-arenosa is not", {
  # Bt1 areia franca (5% of clay, 5% of silt) with a high ratio, Bt2 Tb
  p <- .mp_profile(c("A", "Bt1", "Bt2"), c(0, 20, 70), c(20, 70, 120), clay = c(4, 5, 30),
                   cec = c(2, 3, 5), bs = 40, silt = 5)
  r <- atividade_argila_alta(p)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$sandy, 2L)
  expect_equal(r$evidence$thickness_known_cm, 50)
  # a B of areia franca alone: the criterion does not apply
  s <- .mp_profile(c("A", "Bt"), c(0, 20), c(20, 120), clay = c(4, 5), cec = c(2, 3), bs = 40, silt = 5)
  expect_true(is.na(atividade_argila_alta(s)$passed))
  # franco-arenosa with 12% of clay and 20% of silt: the criterion applies
  f <- .mp_profile(c("A", "Bi"), c(0, 20), c(20, 80), clay = c(10, 12), cec = c(6, 7), bs = 80, silt = 20)
  r <- atividade_argila_alta(f)
  expect_true(isTRUE(r$passed))
  expect_length(r$evidence$sandy, 0L)
})

test_that("the base saturation is that of most of the first 100 cm of the B", {
  skip_on_cran()
  # dystrophic B with an eutrophic first sub-horizon and an eutrophic C
  p <- .mp_profile(c("A", "BA", "Bt1", "Bt2", "C"), c(0, 20, 35, 70, 130), c(20, 35, 70, 130, 180),
                   clay = 35, cec = 6, bs = c(70, 60, 40, 30, 80))
  r <- eutrofico(p)
  expect_false(isTRUE(r$passed))
  expect_equal(r$layers, 2L)
  expect_equal(r$evidence$thickness_high_cm, 15)
  expect_equal(r$evidence$thickness_known_cm, 100)
  expect_true(isTRUE(distrofico(p)$passed))
  # eutrophic in the larger part
  q <- .mp_profile(c("A", "BA", "Bt1", "Bt2", "C"), c(0, 20, 35, 70, 130), c(20, 35, 70, 130, 180),
                   clay = 35, cec = 6, bs = c(30, 40, 60, 70, 20))
  expect_true(isTRUE(eutrofico(q)$passed))
  expect_false(isTRUE(distrofico(q)$passed))
  # the 65% of the A chernozemico, by the same section
  expect_false(isTRUE(eutrofico(q, min_v = 65)$passed))
  # V recorded in one sub-horizon only: that one says
  u <- .mp_profile(c("A", "Bt1", "Bt2"), c(0, 20, 60), c(20, 60, 120), 35, 6, c(30, NA, 70))
  expect_true(isTRUE(eutrofico(u)$passed))
  # in none: not known
  n <- .mp_profile(c("A", "Bt1", "Bt2"), c(0, 20, 60), c(20, 60, 120), 35, 6, c(30, NA, NA))
  expect_true(is.na(eutrofico(n)$passed))
  expect_equal(eutrofico(n)$missing, "bs_pct")
})

test_that("a profile with no B is read in its C, as before", {
  p <- .mp_profile(c("A", "C1", "C2"), c(0, 20, 60), c(20, 60, 120), clay = c(20, 25, 25),
                   cec = c(6, 4, 9), bs = c(40, 30, 60))
  expect_true(isTRUE(atividade_argila_alta(p)$passed))
  expect_true(isTRUE(eutrofico(p)$passed))
  expect_null(atividade_argila_alta(p)$evidence$section)
})

test_that("an Argissolo with a Ta and eutrophic C is no Luvissolo", {
  skip_on_cran()
  p <- make_luvissolo_canonical()                       # A, BA, Bt, C
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Luvissolos")
  h <- data.table::copy(p$horizons)
  h$cec_cmol <- c(6, 6, 8, 20); h$bs_pct <- c(40, 35, 30, 95)   # Tb and dystrophic B
  q <- p$clone(deep = TRUE); q$horizons <- h
  expect_true(isTRUE(B_textural(q)$passed))
  expect_false(isTRUE(atividade_argila_alta(q)$passed))
  expect_false(isTRUE(eutrofico(q)$passed))
  expect_false(isTRUE(luvissolo(q)$passed))
  expect_equal(classify_sibcs(q, on_missing = "silent")$rsg_or_order, "Argissolos")
  # Ta in the B, V high in the C only: no Luvissolo either
  h2 <- data.table::copy(p$horizons); h2$bs_pct <- c(40, 35, 30, 95)
  q2 <- p$clone(deep = TRUE); q2$horizons <- h2
  expect_true(isTRUE(atividade_argila_alta(q2)$passed))
  expect_false(isTRUE(luvissolo(q2)$passed))
})

test_that("a Nitossolo keeps its B nitico when a layer below the B has a high ratio", {
  skip_on_cran()
  p <- make_nitossolo_canonical()                       # A, BA, Bt, BC
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Nitossolos")
  h <- data.table::copy(p$horizons)
  h$cec_cmol[4] <- 20                                   # BC: 36 cmolc per kg of clay
  q <- p$clone(deep = TRUE); q$horizons <- h
  expect_false(isTRUE(atividade_argila_alta(q)$passed))
  expect_true(isTRUE(B_nitico(q)$passed))
  expect_equal(classify_sibcs(q, on_missing = "silent")$rsg_or_order, "Nitossolos")
})

test_that("the canonical profiles keep their names", {
  skip_on_cran()
  expect_equal(classify_sibcs(make_luvissolo_canonical(), on_missing = "silent")$name,
               "Luvissolos Crômicos Órticos típicos")
  expect_equal(classify_sibcs(make_argissolo_canonical(), on_missing = "silent")$name,
               "Argissolos Vermelhos Distróficos típicos")
  expect_equal(classify_sibcs(make_cambissolo_canonical(), on_missing = "silent")$name,
               "Cambissolos Háplicos Ta Eutróficos típicos")
  expect_equal(classify_sibcs(make_latossolo_canonical(), on_missing = "silent")$name,
               "Latossolos Vermelhos Distróficos típicos")
})
