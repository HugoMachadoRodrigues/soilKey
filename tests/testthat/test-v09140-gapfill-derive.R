# v0.9.140 -- definitional-closure gap-fill (gapfill_derive_horizon) + the
# method dispatcher of the classifiers' `gapfill=` argument (interp / derive /
# soilgrids). Off by default -> classification stays byte-identical.

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))
prh <- function(hz) PedonRecord$new(site = list(id = "t"), horizons = hz)


test_that("v0.9.140: texture closure fills the third fraction (100 - other two)", {
  skip_on_cran()
  h <- mkh(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
                      clay_pct = c(NA, 40), silt_pct = c(30, 30), sand_pct = c(50, 30)))
  p <- prh(h); gapfill_derive_horizon(p)
  expect_equal(p$horizons$clay_pct[1], 20)         # 100 - 30 - 50
  expect_equal(p$horizons$clay_pct[2], 40)         # measured, untouched
})

test_that("v0.9.140: al_sat / ecec / bs closures from the exchange complex", {
  skip_on_cran()
  h <- mkh(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
                      ca_cmol = c(2, 1), mg_cmol = c(1, 0.5), k_cmol = c(0.2, 0.1),
                      na_cmol = c(0.1, 0.1), al_cmol = c(0.5, 3), cec_cmol = c(8, 10)))
  p <- prh(h); gapfill_derive_horizon(p)
  # ecec = bases + al; layer 2: (1+0.5+0.1+0.1) + 3 = 4.7
  expect_equal(round(p$horizons$ecec_cmol[2], 1), 4.7)
  # al_sat = 100*al/ecec; layer 2: 100*3/4.7
  expect_equal(round(p$horizons$al_sat_pct[2], 1), round(100 * 3 / 4.7, 1))
  # bs = 100*bases/cec; layer 1: 100*3.3/8
  expect_equal(round(p$horizons$bs_pct[1], 1), round(100 * 3.3 / 8, 1))
})

test_that("v0.9.140: derive never overwrites a measured value (authority order)", {
  skip_on_cran()
  h <- mkh(data.frame(top_cm = 0, bottom_cm = 30, clay_pct = 25,
                      silt_pct = 30, sand_pct = 50))
  p <- prh(h); gapfill_derive_horizon(p)
  expect_equal(p$horizons$clay_pct[1], 25)         # measured 25 kept, not 100-30-50=20
})

test_that("v0.9.140: derive requires Ca+Mg present (no bases-from-thin-air)", {
  skip_on_cran()
  h <- mkh(data.frame(top_cm = 0, bottom_cm = 30, al_cmol = 3, cec_cmol = 10))
  p <- prh(h); gapfill_derive_horizon(p)
  expect_true(is.na(p$horizons$al_sat_pct[1]))     # no Ca/Mg -> bases untrusted
  expect_true(is.na(p$horizons$bs_pct[1]))
})


# ---- classifier dispatcher (interp / derive / soilgrids) ------------------

test_that("v0.9.140: gapfill=FALSE is byte-identical (no fill, no mutation)", {
  skip_on_cran()
  pr <- make_argissolo_canonical()
  base <- classify_sibcs(pr, on_missing = "silent")$rsg_or_order
  with <- classify_sibcs(pr, gapfill = FALSE, on_missing = "silent")$rsg_or_order
  expect_identical(base, with)
})

test_that("v0.9.140: gapfill=list(method='derive') fills on a deep copy", {
  skip_on_cran()
  h <- mkh(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 50),
                      clay_pct = c(15, 40), al_cmol = c(0.5, 3),
                      ca_cmol = c(2, 1), mg_cmol = c(1, 0.5), cec_cmol = c(8, 10)))
  pr <- prh(h)
  q <- soilKey:::.classify_apply_gapfill(pr, list(method = "derive"))
  expect_equal(sum(!is.na(q$horizons$al_sat_pct)), 2L)
  expect_true(all(is.na(pr$horizons$al_sat_pct)))  # caller's pedon untouched
})

test_that("v0.9.140: gapfill=list(method='soilgrids') is reachable offline", {
  skip_on_cran()
  pr <- prh(mkh(data.frame(top_cm = 0, bottom_cm = 30, clay_pct = NA_real_)))
  q <- soilKey:::.classify_apply_gapfill(
    pr, list(method = "soilgrids", attrs = "clay_pct",
             depth_profiles = list(clay_pct = c(18, 20, 24, 28, 30, 30))))
  expect_false(is.na(q$horizons$clay_pct[1]))
})

test_that("v0.9.140: back-compat -- gapfill=TRUE and character still do interp", {
  skip_on_cran()
  h <- mkh(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60),
                      clay_pct = c(15, NA, 35)))
  q1 <- soilKey:::.classify_apply_gapfill(prh(h), TRUE)
  q2 <- soilKey:::.classify_apply_gapfill(prh(h), "clay_pct")
  expect_false(is.na(q1$horizons$clay_pct[2]))     # interior interpolated
  expect_false(is.na(q2$horizons$clay_pct[2]))
})

test_that("v0.9.140: an unknown method errors", {
  skip_on_cran()
  expect_error(
    soilKey:::.classify_apply_gapfill(make_argissolo_canonical(), list(method = "bogus")),
    "unknown gapfill method")
})
