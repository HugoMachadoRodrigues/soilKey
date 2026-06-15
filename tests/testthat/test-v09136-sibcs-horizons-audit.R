# v0.9.136 -- SiBCS diagnostic-HORIZON audit slice (Embrapa 2018 Cap 2,
# verbatim). Four confirmed divergences fixed, each refine-when-present so
# data lacking the field stays byte-identical to the pre-v0.9.136 result.
#
# Verbatim grounding (printed pages):
#   A chernozemico p.50 (a) structure "grau ... moderado ou forte";
#                   p.51 (e) thickness >=10/over-rock | >=18 & >1/3 solum if
#                   solum<75 | >=25 if solum>=75.
#   A humico       p.51 "valor e croma (cor do solo umido) <= 4".
#   A antropico    p.53 artefacts of "presenca obrigatoria" + (>=20cm "e" P>=30).

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))
prh <- function(hz) PedonRecord$new(site = list(id = "t"), horizons = hz)


# ---- A humico colour gate (value & chroma moist <= 4) ---------------------

test_that("v0.9.136: A humico rejects a light-coloured A (value/chroma moist > 4)", {
  base <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
                     designation = c("A1", "A2"), clay_pct = c(20, 20),
                     oc_pct = c(5, 4), bs_pct = c(40, 40),
                     munsell_value_moist = c(5, 5), munsell_chroma_moist = c(3, 3))
  # CO inequation + V<65 + thickness are all met; only the colour fails.
  expect_false(isTRUE(horizonte_A_humico(prh(mkh(base)))$passed))
  dark <- base; dark$munsell_value_moist <- c(3, 3)
  expect_true(isTRUE(horizonte_A_humico(prh(mkh(dark)))$passed))
})

test_that("v0.9.136: A humico is byte-identical when colour is absent (NA)", {
  base <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
                     designation = c("A1", "A2"), clay_pct = c(20, 20),
                     oc_pct = c(5, 4), bs_pct = c(40, 40),
                     munsell_value_moist = NA_real_, munsell_chroma_moist = NA_real_)
  expect_true(isTRUE(horizonte_A_humico(prh(mkh(base)))$passed))
})


# ---- A chernozemico structure gate (grade moderate/strong) ----------------

test_that("v0.9.136: A chernozemico rejects WEAK structure grade", {
  ch <- data.frame(top_cm = 0, bottom_cm = 30, designation = "A",
                   oc_pct = 2, bs_pct = 80, munsell_value_moist = 2,
                   munsell_chroma_moist = 1, munsell_value_dry = 3,
                   structure_grade = "weak")
  expect_false(isTRUE(horizonte_A_chernozemico(prh(mkh(ch)))$passed))
  ch$structure_grade <- "moderate"
  expect_true(isTRUE(horizonte_A_chernozemico(prh(mkh(ch)))$passed))
})

test_that("v0.9.136: A chernozemico byte-identical when grade is NA", {
  ch <- data.frame(top_cm = 0, bottom_cm = 30, designation = "A",
                   oc_pct = 2, bs_pct = 80, munsell_value_moist = 2,
                   munsell_chroma_moist = 1, munsell_value_dry = 3,
                   structure_grade = NA_character_)
  expect_true(isTRUE(horizonte_A_chernozemico(prh(mkh(ch)))$passed))
})


# ---- A chernozemico thickness conditional (10 / 18+1/3 / 25 by solum) ------

test_that("v0.9.136: a 20 cm A over a deep solum (>=75) needs 25 cm (FALSE)", {
  deep <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 120),
                     designation = c("A", "Bt"), oc_pct = c(2, 0.5),
                     bs_pct = c(80, 80), munsell_value_moist = c(2, 4),
                     munsell_chroma_moist = c(1, 3), munsell_value_dry = c(3, 5),
                     structure_grade = c("strong", "moderate"))
  expect_false(isTRUE(horizonte_A_chernozemico(prh(mkh(deep)))$passed))
})

test_that("v0.9.136: a 12 cm A directly over rock qualifies (>= 10 cm)", {
  rock <- data.frame(top_cm = c(0, 12), bottom_cm = c(12, 40),
                     designation = c("A", "R"), oc_pct = c(2, NA),
                     bs_pct = c(80, NA), munsell_value_moist = c(2, NA),
                     munsell_chroma_moist = c(1, NA), munsell_value_dry = c(3, NA),
                     structure_grade = c("strong", NA))
  expect_true(isTRUE(horizonte_A_chernozemico(prh(mkh(rock)))$passed))
})

test_that("v0.9.136: a thick A (100 cm) over a deep solum still qualifies", {
  # guards the canonical chernozem fixture geometry (Ah 100 cm / Bk-Ck solum).
  thick <- data.frame(top_cm = c(0, 30, 60, 100), bottom_cm = c(30, 60, 100, 140),
                      designation = c("Ah1", "Ah2", "AB", "Bk"),
                      oc_pct = c(4, 2.5, 1.5, 0.8), bs_pct = c(89, 87, 86, 97),
                      munsell_value_moist = c(2, 2, 3, 4),
                      munsell_chroma_moist = c(1, 1, 2, 3),
                      munsell_value_dry = c(3, 3, 4, 6),
                      structure_grade = c("strong", "strong", "moderate", "weak"))
  expect_true(isTRUE(horizonte_A_chernozemico(prh(mkh(thick)))$passed))
})


# ---- A antropico artefacts gate (mandatory, refine-when-present) -----------

test_that("v0.9.136: A antropico requires artefacts when they are RECORDED", {
  # passes hortic (thick>=20, oc>=0.6, P>=30, surface) -- only artefacts differ.
  base <- data.frame(top_cm = 0, bottom_cm = 30, designation = "Ap",
                     oc_pct = 2, p_mehlich3_mg_kg = 120, munsell_value_moist = 3)
  a0 <- base; a0$artefacts_pct <- 0
  expect_false(isTRUE(horizonte_A_antropico(prh(mkh(a0)))$passed))
  a8 <- base; a8$artefacts_pct <- 8
  expect_true(isTRUE(horizonte_A_antropico(prh(mkh(a8)))$passed))
})

test_that("v0.9.136: A antropico byte-identical to hortic when artefacts absent", {
  base <- data.frame(top_cm = 0, bottom_cm = 30, designation = "Ap",
                     oc_pct = 2, p_mehlich3_mg_kg = 120, munsell_value_moist = 3)
  # artefacts_pct not provided -> NA -> defer to hortic (which passes here).
  expect_true(isTRUE(horizonte_A_antropico(prh(mkh(base)))$passed))
})
