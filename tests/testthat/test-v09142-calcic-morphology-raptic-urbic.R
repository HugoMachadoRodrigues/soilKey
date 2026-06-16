# v0.9.142 -- schema field secondary_carbonates_pct unblocks the WRB/USDA calcic
# enrichment (protocalcic / by-volume OR-path), and the verbatim Raptic
# (layer_origin exclusion) + Urbic (>= 20 cm) clauses. All refine-when-present:
# absent data -> byte-identical.

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))
prh <- function(hz) PedonRecord$new(site = list(id = "t"), horizons = hz)


# ---- calcic() core enrichment (refine-when-present, protocalcic OR-path) ---

test_that("v0.9.142: calcic core is byte-identical when secondary_carbonates absent", {
  uni <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "C1", "C2"), caco3_pct = c(20, 20, 20)))
  expect_true(isTRUE(calcic(prh(uni))$passed))     # no morphology -> not disproven
})

test_that("v0.9.142: calcic core drops a uniform profile when 2a AND 2b disproven", {
  # every >= 15% layer must be disproven (no +5% enrichment AND secondary < 5%);
  # a single NA-secondary layer stays indeterminate and keeps the calcic.
  uni <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "C1", "C2"), caco3_pct = c(20, 20, 20),
                        secondary_carbonates_pct = c(2, 2, 2)))
  expect_false(isTRUE(calcic(prh(uni))$passed))    # all layers: no +5% AND secondary < 5
})

test_that("v0.9.142: by-volume secondary carbonates (>=5%) rescue a calcic (protocalcic)", {
  uni <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "C1", "C2"), caco3_pct = c(20, 20, 20),
                        secondary_carbonates_pct = c(NA, 8, 8)))
  expect_true(isTRUE(calcic(prh(uni))$passed))
})

test_that("v0.9.142: an enriched calcic still passes (CaCO3 +5% path)", {
  enr <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "Bk", "C"), caco3_pct = c(5, 30, 10)))
  expect_true(isTRUE(calcic(prh(enr))$passed))
})


# ---- SiBCS horizonte_calcico by-volume alternative -------------------------

test_that("v0.9.142: SiBCS calcico accepts the by-volume secondary-carbonate path", {
  # uniform CaCO3 (no +50) but >= 5% by-volume secondary carbonate -> calcico
  uni <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "Bk", "C"), caco3_pct = c(20, 20, 20),
                        secondary_carbonates_pct = c(NA, 8, NA)))
  expect_true(isTRUE(horizonte_calcico(prh(uni))$passed))
  # without the by-volume datum the same uniform profile is NOT calcico (SiBCS +50)
  uni2 <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                         designation = c("A", "Bk", "C"), caco3_pct = c(20, 20, 20)))
  expect_false(isTRUE(horizonte_calcico(prh(uni2))$passed))
})


# ---- Raptic layer_origin exclusion -----------------------------------------

test_that("v0.9.142: Raptic excludes a fluvic/aeolic/tephric/solimovic discontinuity", {
  rp <- mkh(data.frame(top_cm = c(0, 40), bottom_cm = c(40, 90),
                       designation = c("A", "2C"),
                       stratification_pattern = c(NA, "lithologic_break"),
                       layer_origin = c(NA, "fluvic")))
  expect_false(isTRUE(qual_raptic(prh(rp))$passed))
  rp$layer_origin <- c(NA, "residual")
  expect_true(isTRUE(qual_raptic(prh(rp))$passed))   # non-excluded origin
})


# ---- Urbic >= 20 cm thickness ----------------------------------------------

test_that("v0.9.142: Urbic requires a >= 20 cm qualifying layer", {
  thin <- mkh(data.frame(top_cm = 0, bottom_cm = 10, designation = "Cu",
                         artefacts_urbic_pct = 30))
  expect_false(isTRUE(qual_urbic(prh(thin))$passed))
  thick <- mkh(data.frame(top_cm = 0, bottom_cm = 30, designation = "Cu",
                          artefacts_urbic_pct = 30))
  expect_true(isTRUE(qual_urbic(prh(thick))$passed))
})
