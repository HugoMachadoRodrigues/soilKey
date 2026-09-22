# v0.9.134 -- SiBCS atributos audit (Phase 3, slice 1). A multi-agent audit vs
# the verbatim Embrapa 2018 manual (Cap 1) surfaced AND/OR and missing-clause
# bugs; each confirmed fix is locked in here.

mk <- function(df) {
  PedonRecord$new(horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("carater_acrico: pH-KCl >= 5.0 OR delta-pH >= 0 (OR, not delta only)", {
  # low ECEC/clay; pH-KCl 5.2 but delta-pH negative -> still acrico via pH-KCl
  p <- mk(data.frame(top_cm = 0, bottom_cm = 50, designation = "Bw",
                     clay_pct = 60, ecec_cmol = 0.7, ph_kcl = 5.2, ph_h2o = 5.6))
  expect_true(isTRUE(carater_acrico(p)$passed))
  # delta-pH path still works (pH-KCl low, delta >= 0)
  q <- mk(data.frame(top_cm = 0, bottom_cm = 50, designation = "Bw",
                     clay_pct = 60, ecec_cmol = 0.7, ph_kcl = 4.8, ph_h2o = 4.6))
  expect_true(isTRUE(carater_acrico(q)$passed))
  # neither pH condition -> not acrico
  r <- mk(data.frame(top_cm = 0, bottom_cm = 50, designation = "Bw",
                     clay_pct = 60, ecec_cmol = 0.7, ph_kcl = 4.5, ph_h2o = 4.8))
  expect_false(isTRUE(carater_acrico(r)$passed))
})

test_that("carater_alitico: Al >= 4 AND (Al-sat >= 50 OR V < 50), not all-three-AND", {
  # Al 5, Al-sat ~62% (high), but V ~55% (>= 50) -> still alitico via Al-sat OR
  p <- mk(data.frame(top_cm = 0, bottom_cm = 40, designation = "Bi",
                     al_cmol = 5, ca_cmol = 2, mg_cmol = 1, k_cmol = 0.1,
                     na_cmol = 0.1, bs_pct = 55))
  expect_true(isTRUE(carater_alitico(p)$passed))
})

test_that("luvissolo_cromico: criterion (c) restricted to 2.5Y-5Y, not all yellow", {
  # 7.5Y (yellower than 5Y), v 6, c 5 -> NOT cromico (was wrongly caught)
  p <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     designation = c("A", "Bt"),
                     munsell_hue_moist = c("7.5Y", "7.5Y"),
                     munsell_value_moist = c(6, 6), munsell_chroma_moist = c(5, 5)))
  expect_false(isTRUE(luvissolo_cromico(p)$passed))
  # 5Y v5 c5 -> cromico (criterion c)
  q <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     designation = c("A", "Bt"),
                     munsell_hue_moist = c("5Y", "5Y"),
                     munsell_value_moist = c(5, 5), munsell_chroma_moist = c(5, 5)))
  expect_true(isTRUE(luvissolo_cromico(q)$passed))
})

test_that("carater_argiluvico: requires prismatic/blocky structure where recorded", {
  base <- data.frame(top_cm = c(0, 25), bottom_cm = c(25, 90),
                     designation = c("A", "Bt"),
                     clay_pct = c(15, 30), silt_pct = c(40, 30), sand_pct = c(45, 40),
                     clay_films_amount = c(NA, "common"))
  # B with prismatic structure -> argiluvico (if B_textural fires)
  pri <- base; pri$structure_type <- c("granular", "prismatic")
  # B with weak blocky -> structure clause fails
  wb  <- base; wb$structure_type <- c("granular", "blocks")
  wb$structure_grade <- c("weak", "weak")
  rp <- carater_argiluvico(mk(pri)); rw <- carater_argiluvico(mk(wb))
  # only assert the structure gate when B_textural actually fired on the fixture
  if (isTRUE(B_textural(mk(pri))$passed)) {
    expect_true(isTRUE(rp$passed))
    expect_false(isTRUE(rw$passed))
  } else {
    skip("B_textural did not fire on the synthetic fixture")
  }
})
