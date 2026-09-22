# v0.9.132 -- WRB 2022 qualifier audit (Fix D slice 4): a multi-agent audit vs
# the verbatim WRB 2022 Ch 5 PDF surfaced threshold/depth/criterion bugs in 11
# qualifiers; each fix is locked in here.

mk <- function(df) {
  PedonRecord$new(horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("Geric: (bases+Al) < 6 cmol/kg CLAY, no spurious delta-pH path", {
  # clay 50%, ECEC 2.0 cmol/kg soil -> 4.0 cmol/kg clay < 6 -> Geric
  p <- mk(data.frame(top_cm = 0, bottom_cm = 50, clay_pct = 50,
                     ca_cmol = 1.0, mg_cmol = 0.5, k_cmol = 0.1, na_cmol = 0.1,
                     al_cmol = 0.3))
  expect_true(qual_geric(p)$passed)
  # ECEC 5.0 cmol/kg soil / 0.50 clay = 10 cmol/kg clay >= 6 -> not Geric
  q <- mk(data.frame(top_cm = 0, bottom_cm = 50, clay_pct = 50,
                     ca_cmol = 3, mg_cmol = 1.5, k_cmol = 0.3, na_cmol = 0.2,
                     al_cmol = 0))
  expect_false(isTRUE(qual_geric(q)$passed))
  # a positive delta-pH alone (Posic) must NOT make a high-ECEC soil Geric
  r <- mk(data.frame(top_cm = 0, bottom_cm = 50, clay_pct = 50,
                     ca_cmol = 8, mg_cmol = 2, k_cmol = 0.3, na_cmol = 0.2,
                     al_cmol = 0, ph_h2o = 5.0, ph_kcl = 5.5))
  expect_false(isTRUE(qual_geric(r)$passed))
})

test_that("Pellic: value <= 3 AND chroma <= 2 (was value <= 4)", {
  p3 <- mk(data.frame(top_cm = 0, bottom_cm = 25, munsell_value_moist = 3,
                      munsell_chroma_moist = 2))
  expect_true(qual_pellic(p3)$passed)
  p4 <- mk(data.frame(top_cm = 0, bottom_cm = 25, munsell_value_moist = 4,
                      munsell_chroma_moist = 2))
  expect_false(isTRUE(qual_pellic(p4)$passed))
})

test_that("Sodic: needs >= 15% (Na+Mg) AND >= 6% Na", {
  # ESP 8% but (Na+Mg) only 10% -> not Sodic
  low <- mk(data.frame(top_cm = 0, bottom_cm = 30, cec_cmol = 100,
                       na_cmol = 8, mg_cmol = 2))
  expect_false(isTRUE(qual_sodic(low)$passed))
  # ESP 8% and (Na+Mg) 20% -> Sodic
  hi <- mk(data.frame(top_cm = 0, bottom_cm = 30, cec_cmol = 100,
                      na_cmol = 8, mg_cmol = 12))
  expect_true(qual_sodic(hi)$passed)
})

test_that("Magnesic needs a >= 30 cm thick Ca/Mg < 1 layer", {
  thin <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 40),
                        ca_cmol = c(2, 1), mg_cmol = c(1, 3)))  # only 20 cm < 1
  expect_false(isTRUE(qual_magnesic(thin)$passed))
  thick <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                         ca_cmol = c(1, 1), mg_cmol = c(3, 3)))  # 60 cm < 1
  expect_true(qual_magnesic(thick)$passed)
})

test_that("Aceric: 3.5 <= pH < 5 (lower bound added)", {
  ok <- mk(data.frame(top_cm = 0, bottom_cm = 30, ph_h2o = 4.2))
  expect_true(qual_aceric(ok)$passed)
  toolow <- mk(data.frame(top_cm = 0, bottom_cm = 30, ph_h2o = 3.0))
  expect_false(isTRUE(qual_aceric(toolow)$passed))
})

test_that("Columnic: columnar only (not prismatic) + >= 15 cm", {
  col <- mk(data.frame(top_cm = 0, bottom_cm = 30, structure_type = "columnar"))
  expect_true(qual_columnic(col)$passed)
  pri <- mk(data.frame(top_cm = 0, bottom_cm = 30, structure_type = "prismatic"))
  expect_false(isTRUE(qual_columnic(pri)$passed))
})

test_that("Carbonic: >= 5% OC (was 6) in a layer >= 10 cm", {
  ok <- mk(data.frame(top_cm = 0, bottom_cm = 30, oc_pct = 5.2))
  expect_true(qual_carbonic(ok)$passed)
  thin <- mk(data.frame(top_cm = 0, bottom_cm = 8, oc_pct = 5.2))  # 8 cm < 10
  expect_false(isTRUE(qual_carbonic(thin)$passed))
})

test_that("Placic: Fe-cementation at least weakly (was strongly/indurated)", {
  weak <- mk(data.frame(top_cm = c(0, 30), bottom_cm = c(30, 31),
                        cementation_class = c("none", "weakly")))
  expect_true(qual_placic(weak)$passed)
})
