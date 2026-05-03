# Tests for v0.9.26 argic / argillic per-system clay-increase thresholds
#
# WRB 2022 (Ch 3.1.3 p 36) and KST 13ed (Ch 3 p 4) use the same structural
# rule but DIFFERENT thresholds:
#                    eluvial clay   WRB     KST 13ed
#                       < 15 %      +6 pp   +3 pp
#                       15-X %      1.4x    1.2x  (X=50 WRB; 40 KST)
#                       >= X %      +20 pp  +8 pp
#
# v0.9.26 introduces the system switch on test_clay_increase_argic() and
# argic(); argillic_usda() now passes system = "usda" through.

mk_h <- function(...) ensure_horizon_schema(data.table::data.table(...))

# ---- KST-only-passing band (above < 15 %, +3 to <+6 pp) --------------------

test_that("clay-increase: 8.6 -> 12.3 (3.7 pp) passes KST but fails WRB", {
  h <- mk_h(top_cm = c(0, 5), bottom_cm = c(5, 25),
              designation = c("E", "Bt"),
              clay_pct = c(8.6, 12.3))
  res_wrb <- test_clay_increase_argic(h, system = "wrb2022")
  res_kst <- test_clay_increase_argic(h, system = "usda")
  expect_false(isTRUE(res_wrb$passed))
  expect_true(isTRUE(res_kst$passed))
  expect_equal(res_kst$layers, 2L)
})

# ---- KST-only-passing band (15-40 %, ratio 1.2-1.4) ------------------------

test_that("clay-increase: 28.3 -> 39.4 (ratio 1.39) passes KST but fails WRB", {
  h <- mk_h(top_cm = c(0, 5), bottom_cm = c(5, 25),
              designation = c("Ap", "Bt"),
              clay_pct = c(28.3, 39.4))
  res_wrb <- test_clay_increase_argic(h, system = "wrb2022")
  res_kst <- test_clay_increase_argic(h, system = "usda")
  expect_false(isTRUE(res_wrb$passed))
  expect_true(isTRUE(res_kst$passed))
})

# ---- KST-only-passing band (>= 40 %, +8 to <+20 pp) ------------------------

test_that("clay-increase: 45 -> 58 (+13 pp) passes KST but fails WRB", {
  h <- mk_h(top_cm = c(0, 5), bottom_cm = c(5, 25),
              designation = c("Ap", "Bt"),
              clay_pct = c(45, 58))
  res_wrb <- test_clay_increase_argic(h, system = "wrb2022")
  res_kst <- test_clay_increase_argic(h, system = "usda")
  expect_false(isTRUE(res_wrb$passed))
  expect_true(isTRUE(res_kst$passed))
})

# ---- Both-passing band (well above either threshold) -----------------------

test_that("clay-increase: 13 -> 31 passes BOTH WRB and KST (canonical case)", {
  h <- mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
              designation = c("A", "Bt"),
              clay_pct = c(13, 31))
  expect_true(isTRUE(test_clay_increase_argic(h, system = "wrb2022")$passed))
  expect_true(isTRUE(test_clay_increase_argic(h, system = "usda")$passed))
})

# ---- Both-failing band -----------------------------------------------------

test_that("clay-increase: 30 -> 32 (ratio 1.07) fails BOTH systems", {
  h <- mk_h(top_cm = c(0, 30), bottom_cm = c(30, 60),
              designation = c("Ap", "Bw"),
              clay_pct = c(30, 32))
  expect_false(isTRUE(test_clay_increase_argic(h, system = "wrb2022")$passed))
  expect_false(isTRUE(test_clay_increase_argic(h, system = "usda")$passed))
})

# ---- Default system is WRB (back-compat) ------------------------------------

test_that("default system is wrb2022 (back-compat)", {
  h <- mk_h(top_cm = c(0, 5), bottom_cm = c(5, 25),
              designation = c("E", "Bt"),
              clay_pct = c(8.6, 12.3))
  res_default <- test_clay_increase_argic(h)
  res_wrb     <- test_clay_increase_argic(h, system = "wrb2022")
  expect_identical(res_default$passed, res_wrb$passed)
})

# ---- argillic_usda design choice -------------------------------------------
# v0.9.27 wired clay-films-test (NASIS pediagfeatures + phpvsf).
# v0.9.28 added the designation 't'-suffix proxy.
# argillic_usda now uses a two-tier strategy: KST 13ed thresholds when
# clay-films evidence is present (any of the three sources), WRB
# stricter thresholds (proxy) when not.

test_that("argillic_usda falls back to WRB thresholds when no evidence at all", {
  # +3.7 pp band: WRB rejects (needs +6), KST accepts (>=3).
  # Use designation WITHOUT 't' suffix and no NASIS data so the
  # clay-films-test cannot fire -> WRB tier.
  p <- PedonRecord$new(
    site = list(id = "kst-arg-test", lat = 0, lon = 0, country = "TEST"),
    horizons = mk_h(
      top_cm      = c(0,  10, 30),
      bottom_cm   = c(10, 30, 60),
      designation = c("A", "E", "Bw"),     # no 't' suffix
      clay_pct    = c(10, 8.6, 12.3),
      silt_pct    = c(40, 35, 30),
      sand_pct    = c(50, 56.4, 57.7)
    )
  )
  res <- argillic_usda(p)
  expect_false(isTRUE(res$passed))
  expect_equal(res$evidence$argillic_tier$threshold_system, "wrb2022")
})

test_that("argillic_usda accepts the canonical strong-argillic profile (Luvisol)", {
  # Use the canonical Luvisol fixture (oc, bs structured to avoid the
  # fluvic-pattern exclusion in argillic_usda).
  pr <- make_luvisol_canonical()
  res <- argillic_usda(pr)
  expect_true(isTRUE(res$passed))
})
