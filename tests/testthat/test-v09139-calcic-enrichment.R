# v0.9.139 -- the SiBCS calcic horizon (horizonte_calcico) now requires the
# verbatim secondary-carbonate ENRICHMENT (Embrapa 2018 Cap 2 p.71: >= 50 g/kg
# more CaCO3 than the subjacent layer), via the new test_caco3_enrichment.
#
# IMPORTANT: the enrichment is enforced ONLY in the SiBCS wrapper, NOT in the
# shared calcic() core. WRB/USDA calcic has a protocalcic / by-volume-secondary-
# carbonate morphological OR-alternative that the schema cannot measure; a
# measured KSSL n=34,755 test showed a caco3-only enrichment in the core drops
# 10 genuine Aridisols (protocalcic calciargids/petrocalcids) -- not 0-worsened.
# So calcic() stays absolute-only (byte-identical for WRB/USDA), and only SiBCS
# (which has no protocalcic OR) gets the +50 enrichment.

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))
prh <- function(hz) PedonRecord$new(site = list(id = "t"), horizons = hz)


# ---- the shared calcic() core stays absolute-only (byte-identical) --------

test_that("v0.9.139: calcic() core is byte-identical when morphology is absent", {
  # a uniform calcareous profile is still calcic at the WRB/USDA core level: the
  # protocalcic OR-path (secondary_carbonates_pct, v0.9.142) is indeterminate and
  # so the enrichment criterion is not disproven (refine-when-present).
  uni <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "C1", "C2"), caco3_pct = c(20, 20, 20)))
  expect_true(isTRUE(calcic(prh(uni))$passed))
  # v0.9.142: the core now carries the refine-when-present enrichment sub-test.
  expect_named(calcic(make_calcisol_canonical())$evidence,
               c("caco3", "enrichment", "thickness"))
})


# ---- SiBCS horizonte_calcico enforces the +50 enrichment ------------------

test_that("v0.9.139: SiBCS calcico rejects a uniform calcareous profile", {
  uni <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "C1", "C2"), caco3_pct = c(20, 20, 20)))
  expect_false(isTRUE(horizonte_calcico(prh(uni))$passed))
})

test_that("v0.9.139: SiBCS calcico accepts an enriched Bk over lower-carbonate C", {
  enr <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "Bk", "C"), caco3_pct = c(5, 30, 10)))
  expect_true(isTRUE(horizonte_calcico(prh(enr))$passed))
})

test_that("v0.9.139: SiBCS calcico exempts a calcic over a >=40% substrate", {
  mar <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                        designation = c("A", "Bk", "2C"), caco3_pct = c(5, 30, 45)))
  expect_true(isTRUE(horizonte_calcico(prh(mar))$passed))
})

test_that("v0.9.139: SiBCS calcico is NA when CaCO3 absent (byte-identical)", {
  noc <- mkh(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 40),
                        designation = c("A", "Bw"), clay_pct = c(20, 25)))
  expect_true(is.na(horizonte_calcico(prh(noc))$passed))
})

test_that("v0.9.139: the canonical Calcisol fixture stays calcic under SiBCS", {
  expect_true(isTRUE(horizonte_calcico(make_calcisol_canonical())$passed))
})


# ---- the enrichment helper -----------------------------------------------

test_that("v0.9.139: test_caco3_enrichment thresholds and exemptions", {
  h <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                      designation = c("A", "Bk", "C"), caco3_pct = c(5, 30, 10)))
  # Bk (idx 2) exceeds C (10) by 20 -> passes
  expect_true(2L %in% test_caco3_enrichment(h, candidate_layers = c(2L, 3L))$layers)
  # the deepest C (idx 3) has no underlying layer -> dropped
  expect_false(3L %in% test_caco3_enrichment(h, candidate_layers = c(2L, 3L))$layers)
  # empty candidates -> NA (preserves no-data semantics)
  expect_true(is.na(test_caco3_enrichment(h, candidate_layers = integer(0))$passed))
})
