# v0.9.138 -- SiBCS B textural relacao-textural (Embrapa 2018 Cap 2 p.56 item h)
# implemented as test_ratio_textural_sibcs and UNIONed into B_textural.
#
# Measured finding: item (h) is almost entirely a SUBSET of the WRB argic
# absolute clay-increase -- it only adds B-textural cases for very sandy A
# horizons (clay < ~7.5%), where the ratio (>1.80) is a smaller absolute jump
# than argic's +6 pp. The union therefore can only ADD a B_textural pass, never
# remove one; on FEBR/Redape/BDsolos it is benchmark-neutral.

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))
prh <- function(hz) PedonRecord$new(site = list(id = "t"), horizons = hz)
rt  <- function(df) test_ratio_textural_sibcs(mkh(df))


# ---- the ratio thresholds keyed on A-horizon clay (p.56 h.1/h.2/h.3) ------

test_that("v0.9.138: relacao-textural thresholds are 1.50 / 1.70 / 1.80 by A clay", {
  # A clay > 40% -> threshold 1.50
  cy <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 70),
                   designation = c("A", "Bt"), clay_pct = c(50, 80))
  expect_equal(rt(cy)$details$threshold, 1.50)
  expect_true(rt(cy)$passed)                      # 80/50 = 1.6 > 1.50

  # A clay 15-40% -> threshold 1.70 (stricter than argic's 1.4)
  md <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 70),
                   designation = c("A", "Bt"), clay_pct = c(25, 40))
  expect_equal(rt(md)$details$threshold, 1.70)
  expect_false(rt(md)$passed)                     # 40/25 = 1.6 < 1.70

  # A clay < 15% -> threshold 1.80
  sd <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 70),
                   designation = c("A", "Bt"), clay_pct = c(5, 10))
  expect_equal(rt(sd)$details$threshold, 1.80)
  expect_true(rt(sd)$passed)                      # 10/5 = 2.0 > 1.80
})


# ---- footnote-4 control section ------------------------------------------

test_that("v0.9.138: a thin A (<15cm) uses a 30cm B control window", {
  # A is 10cm thick -> B mean computed over the first 30cm of B only, so the
  # deep low-clay Bt2 (40-100cm) does NOT dilute the ratio.
  win <- data.frame(top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 100),
                    designation = c("A", "Bt1", "Bt2"), clay_pct = c(5, 12, 3))
  d <- rt(win)$details
  expect_equal(d$control_window_cm, 30)
  expect_equal(round(d$b_mean_clay, 1), 12)       # only Bt1 (10-40) counts
  expect_true(rt(win)$passed)                     # 12/5 = 2.4 > 1.80
})

test_that("v0.9.138: a thick A (>=15cm) uses a 2x-A-thickness B window", {
  win <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 70),
                    designation = c("A", "Bt"), clay_pct = c(10, 22))
  expect_equal(rt(win)$details$control_window_cm, 40)   # 2 x 20cm A
})


# ---- the UNION into B_textural -------------------------------------------

test_that("v0.9.138: B_textural fires via (h) on a sandy A that argic misses", {
  # A 5%, B 10%: argic (+6 pp absolute) fails (only +5); (h) ratio 2.0 > 1.80.
  sd <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 70),
                   designation = c("A", "Bt"), clay_pct = c(5, 10))
  expect_false(isTRUE(argic(prh(mkh(sd)))$passed))
  expect_true(isTRUE(B_textural(prh(mkh(sd)))$passed))
})

test_that("v0.9.138: the union never removes an argic pass (canonical Argissolo)", {
  pr <- make_argissolo_canonical()
  expect_true(isTRUE(argic(pr)$passed))
  expect_true(isTRUE(B_textural(pr)$passed))
  # and the order classification is unchanged
  expect_equal(classify_sibcs(pr, on_missing = "silent")$rsg_or_order, "Argissolos")
})
