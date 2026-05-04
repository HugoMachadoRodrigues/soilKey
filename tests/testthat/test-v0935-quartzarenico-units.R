# Tests for v0.9.35 -- units fix on neossolo_quartzarenico (Cap 12 SiBCS).
#
# SiBCS Cap 1 defines textural classes in g/kg (areia >= 700 g/kg, clay <
# 150-200 g/kg). soilKey's schema stores sand_pct / clay_pct in PERCENT
# (0-100). The pre-v0.9.35 code used the g/kg thresholds directly on %
# data, which never fired and caused 9 FEBR Quartzarenicos to be
# misrouted to Regoliticos.

mk_h <- function(...) ensure_horizon_schema(data.table::data.table(...))


test_that("neossolo_quartzarenico fires on canonical sandy profile", {
  # 80 % sand, 5 % clay -- well within Areia / Areia Franca per SiBCS.
  p <- PedonRecord$new(
    site = list(id = "qa-1", lat = -15, lon = -47, country = "BR"),
    horizons = mk_h(top_cm    = c(0, 30, 80),
                      bottom_cm = c(30, 80, 150),
                      designation = c("A", "C1", "C2"),
                      sand_pct = c(85, 88, 90),
                      clay_pct = c(5, 4, 3),
                      silt_pct = c(10, 8, 7))
  )
  res <- neossolo_quartzarenico(p)
  expect_true(isTRUE(res$passed))
})

test_that("neossolo_quartzarenico fires on areia-franca (clay 18 %, sand 75 %)", {
  # Areia franca per SiBCS: sand >= 700 g/kg AND clay < 200 g/kg
  # (i.e. sand >= 70 % AND clay < 20 % in our schema).
  p <- PedonRecord$new(
    site = list(id = "qa-2", lat = -15, lon = -47, country = "BR"),
    horizons = mk_h(top_cm    = c(0, 30),
                      bottom_cm = c(30, 100),
                      designation = c("A", "C"),
                      sand_pct = c(75, 78),
                      clay_pct = c(18, 16),
                      silt_pct = c(7, 6))
  )
  expect_true(isTRUE(neossolo_quartzarenico(p)$passed))
})

test_that("neossolo_quartzarenico does NOT fire on loamy profile (clay 25 %)", {
  p <- PedonRecord$new(
    site = list(id = "loamy", lat = -15, lon = -47, country = "BR"),
    horizons = mk_h(top_cm    = c(0, 30),
                      bottom_cm = c(30, 100),
                      designation = c("A", "B"),
                      sand_pct = c(60, 55),
                      clay_pct = c(25, 28),
                      silt_pct = c(15, 17))
  )
  expect_false(isTRUE(neossolo_quartzarenico(p)$passed))
})

test_that("neossolo_quartzarenico does NOT fire if any layer fails", {
  # All layers must satisfy the sandy criteria; one clay layer breaks it.
  p <- PedonRecord$new(
    site = list(id = "mixed", lat = -15, lon = -47, country = "BR"),
    horizons = mk_h(top_cm    = c(0, 30, 80),
                      bottom_cm = c(30, 80, 150),
                      designation = c("A", "Bw", "C"),
                      sand_pct = c(80, 50, 85),
                      clay_pct = c(8, 30, 5),
                      silt_pct = c(12, 20, 10))
  )
  expect_false(isTRUE(neossolo_quartzarenico(p)$passed))
})

test_that("classify_sibcs returns 'Neossolos Quartzarenicos' on sandy single-A profile", {
  # End-to-end: shallow sandy profile that should NOT route to Litolico
  # (deep) NOR to Fluvico (no fluvic) NOR to Regolitico (catch-all if
  # all else fails). Per SiBCS Cap 12 priority, Quartzarenico must fire
  # before Regolitico when sand criteria are met.
  p <- PedonRecord$new(
    site = list(id = "febr-style-qa", lat = -15, lon = -47, country = "BR",
                  parent_material = "areias quartzosas"),
    horizons = mk_h(top_cm    = c(0, 30, 80, 130),
                      bottom_cm = c(30, 80, 130, 200),
                      designation = c("A", "C1", "C2", "C3"),
                      sand_pct = c(88, 90, 91, 92),
                      clay_pct = c(5, 4, 3, 3),
                      silt_pct = c(7, 6, 6, 5),
                      ph_h2o   = c(5.2, 5.0, 4.9, 5.0),
                      oc_pct   = c(0.5, 0.2, 0.1, 0.05),
                      bs_pct   = c(20, 15, 12, 10),
                      cec_cmol = c(3, 2, 1.5, 1))
  )
  res <- classify_sibcs(p, on_missing = "silent")
  expect_match(res$rsg_or_order %||% "", "Neossolos")
  expect_match(res$name %||% "", "Quartzarenicos")
})
