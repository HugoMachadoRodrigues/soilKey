# v0.9.246: the minimum thickness of a diagnostic horizon is that of the
# horizon, made of its adjoining sub-horizons.
#
# SiBCS 2018 states each minimum of the horizon: the B incipiente "deve ter no
# minimo 10 cm de espessura" (p. 60), the B nitico "espessura de 30 cm ou
# mais" (p. 62), the horizonte glei "com espessura de 15 cm ou mais" (p. 69),
# the concrecionario "no minimo 30 cm de espessura" (p. 68), the sulfurico
# "15 cm ou mais de espessura" (p. 72). Each was asked of one layer of the
# description: a horizon described in two sub-horizons thinner than the
# minimum did not pass, whatever the two made together. The B latossolico is
# read this way since v0.9.241, the vertic horizon since v0.9.245.

.rows <- function(p, idx, designation, top, bottom) {
  # the layers `idx` of a profile, in that order, with new names and depths
  h <- data.table::copy(p$horizons)[idx]
  h$designation <- designation; h$top_cm <- top; h$bottom_cm <- bottom
  q <- p$clone(deep = TRUE); q$horizons <- h
  q
}

test_that("B incipiente: two adjoining sub-horizons of 6 cm make its 10 cm", {
  p <- make_cambissolo_canonical()                      # A, Bw, BC, C
  q <- .rows(p, c(1, 2, 2, 3, 4), c("A", "Bw1", "Bw2", "BC", "C"),
             c(0, 15, 21, 27, 100), c(15, 21, 27, 100, 150))
  r <- B_incipiente(q)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_equal(classify_sibcs(q, on_missing = "silent")$rsg_or_order, "Cambissolos")
  # one of 6 cm alone is under the minimum
  one <- .rows(p, c(1, 2, 3, 4), c("A", "Bw", "BC", "C"),
               c(0, 15, 21, 100), c(15, 21, 100, 150))
  expect_false(isTRUE(B_incipiente(one)$passed))
  # two of 6 cm with another layer between them do not adjoin
  apart <- .rows(p, c(1, 2, 4, 2, 4), c("A", "Bw1", "C1", "Bw2", "C2"),
                 c(0, 15, 21, 60, 66), c(15, 21, 60, 66, 150))
  expect_false(isTRUE(B_incipiente(apart)$passed))
})

test_that("B nitico: two adjoining sub-horizons of 18 cm make its 30 cm", {
  p <- make_nitossolo_canonical()                       # A, BA, Bt, BC
  h <- data.table::copy(p$horizons)
  h$designation <- c("A", "Bt1", "Bt2", "C")
  h$top_cm <- c(0, 20, 38, 56); h$bottom_cm <- c(20, 38, 56, 200)
  h$clay_pct[4] <- 20; h$clay_films_amount[4] <- NA
  q <- p$clone(deep = TRUE); q$horizons <- h
  r <- B_nitico(q)
  expect_true(isTRUE(r$passed))
  expect_true(all(2:3 %in% r$layers))
  # 18 cm alone
  h1 <- data.table::copy(h); h1$clay_pct[3] <- 20; h1$clay_films_amount[3] <- NA
  q1 <- p$clone(deep = TRUE); q1$horizons <- h1
  expect_false(isTRUE(B_nitico(q1)$passed))
})

test_that("horizonte glei: two adjoining sub-horizons of 8 cm make its 15 cm", {
  mk <- function(designation, top, bottom, glei) {
    n <- length(designation)
    hz <- data.table::data.table(
      designation = designation, top_cm = top, bottom_cm = bottom,
      clay_pct = 30, silt_pct = 30, sand_pct = 40,
      munsell_hue_moist = ifelse(glei, "2.5Y", "10YR"),
      munsell_value_moist = 5, munsell_chroma_moist = ifelse(glei, 1, 6))
    PedonRecord$new(site = list(id = "g", country = "BR", drainage = "Mal drenado"),
                    horizons = ensure_horizon_schema(hz))
  }
  q <- mk(c("A", "Cg1", "Cg2", "C"), c(0, 20, 28, 36), c(20, 28, 36, 120),
          c(FALSE, TRUE, TRUE, FALSE))
  r <- horizonte_glei(q)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  one <- mk(c("A", "Cg", "C"), c(0, 20, 28), c(20, 28, 120), c(FALSE, TRUE, FALSE))
  expect_false(isTRUE(horizonte_glei(one)$passed))
  apart <- mk(c("A", "Cg1", "C1", "Cg2", "C2"), c(0, 20, 28, 60, 68), c(20, 28, 60, 68, 120),
              c(FALSE, TRUE, FALSE, TRUE, FALSE))
  expect_false(isTRUE(horizonte_glei(apart)$passed))
})

test_that("horizonte concrecionario: two adjoining sub-horizons of 16 cm make its 30 cm", {
  mk <- function(designation, top, bottom, pp) {
    hz <- data.table::data.table(designation = designation, top_cm = top,
                                 bottom_cm = bottom, clay_pct = 30, plinthite_pct = pp)
    PedonRecord$new(site = list(id = "c", country = "BR"),
                    horizons = ensure_horizon_schema(hz))
  }
  q <- mk(c("A", "Bc1", "Bc2", "C"), c(0, 20, 36, 52), c(20, 36, 52, 120), c(0, 60, 60, 0))
  r <- horizonte_concrecionario(q)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_false(isTRUE(horizonte_concrecionario(
    mk(c("A", "Bc", "C"), c(0, 20, 36), c(20, 36, 120), c(0, 60, 0)))$passed))
})

test_that("horizonte sulfurico with jarosite: two adjoining sub-horizons of 8 cm make its 15 cm", {
  mk <- function(designation, top, bottom, ph, jar) {
    hz <- data.table::data.table(designation = designation, top_cm = top,
                                 bottom_cm = bottom, clay_pct = 30, ph_h2o = ph)
    hz <- ensure_horizon_schema(hz)
    hz$jarosite_present <- jar
    PedonRecord$new(site = list(id = "s", country = "BR"), horizons = hz)
  }
  q <- mk(c("A", "Bj1", "Bj2", "C"), c(0, 20, 28, 36), c(20, 28, 36, 120),
          c(5, 3.2, 3.3, 5), c(FALSE, TRUE, TRUE, FALSE))
  r <- horizonte_sulfurico(q)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_false(isTRUE(horizonte_sulfurico(
    mk(c("A", "Bj", "C"), c(0, 20, 28), c(20, 28, 120), c(5, 3.2, 5), c(FALSE, TRUE, FALSE)))$passed))
})

test_that("the canonical profiles keep their horizons", {
  expect_true(isTRUE(B_incipiente(make_cambissolo_canonical())$passed))
  expect_true(isTRUE(B_nitico(make_nitossolo_canonical())$passed))
  expect_true(isTRUE(horizonte_glei(make_gleissolo_canonical())$passed))
})
