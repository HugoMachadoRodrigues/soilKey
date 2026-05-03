# Tests for v0.9.29 Neossolo Litolico shallow-profile heuristic.
#
# SiBCS Cap 12 (p 219) defines Neossolos Litolicos by lithic contact
# within 50 cm. In FEBR / BDsolos snapshots, surveyors often document
# this implicitly by stopping the profile description at the rock
# boundary rather than entering a pseudo-R horizon. v0.9.29 adds a
# heuristic that detects this implicit pattern: max profile depth
# <= 50 cm AND no B horizon described AND no explicit non-rock
# material deeper than 50 cm.

mk_h <- function(...) ensure_horizon_schema(data.table::data.table(...))

# ---- positive: shallow profile, no B horizon, no R designation -------------

test_that("neossolo_litolico fires on FEBR-style shallow single-A profile", {
  # Surveyor described one A horizon ending at 20 cm with no further
  # records -- the canonical FEBR Neossolo Litolico pattern.
  p <- PedonRecord$new(
    site = list(id = "litolico-1", lat = -22, lon = -43, country = "BR",
                  parent_material = "rocha"),
    horizons = mk_h(top_cm = 0, bottom_cm = 20,
                      designation = "A",
                      clay_pct = 25, sand_pct = 50, silt_pct = 25)
  )
  res <- neossolo_litolico(p)
  expect_true(isTRUE(res$passed))
  expect_true(isTRUE(res$evidence$shallow_no_B_proxy))
})

test_that("neossolo_litolico fires when designation is NA across all horizons", {
  # FEBR snapshot frequently has NA designation; the depth is the
  # implicit signal.
  p <- PedonRecord$new(
    site = list(id = "litolico-2", lat = -22, lon = -43, country = "BR",
                  parent_material = "rocha"),
    horizons = mk_h(top_cm = 0, bottom_cm = 18,
                      designation = NA_character_,
                      clay_pct = 20, sand_pct = 60, silt_pct = 20)
  )
  res <- neossolo_litolico(p)
  expect_true(isTRUE(res$passed))
})

test_that("neossolo_litolico fires when explicit R designation present (direct path)", {
  p <- PedonRecord$new(
    site = list(id = "litolico-3", lat = -22, lon = -43, country = "BR",
                  parent_material = "rocha"),
    horizons = mk_h(top_cm = c(0, 25), bottom_cm = c(25, 40),
                      designation = c("A", "R"),
                      clay_pct = c(25, NA))
  )
  res <- neossolo_litolico(p)
  expect_true(isTRUE(res$passed))
  expect_true(isTRUE(res$evidence$direct_pass))
})

# ---- negative: deep profile with B horizon ---------------------------------

test_that("neossolo_litolico does NOT fire on deep profile with Bt", {
  p <- PedonRecord$new(
    site = list(id = "argissolo", lat = -22, lon = -43, country = "BR"),
    horizons = mk_h(
      top_cm    = c(0,  25, 60, 100),
      bottom_cm = c(25, 60, 100, 150),
      designation = c("A", "Bt1", "Bt2", "BC"),
      clay_pct  = c(20, 35, 40, 35)
    )
  )
  res <- neossolo_litolico(p)
  expect_false(isTRUE(res$passed))
})

test_that("neossolo_litolico does NOT fire on shallow profile with B horizon", {
  # Shallow but the surveyor described a B -- this is a Cambissolo
  # raso, not a Neossolo Litolico.
  p <- PedonRecord$new(
    site = list(id = "cambissolo", lat = -22, lon = -43, country = "BR"),
    horizons = mk_h(top_cm = c(0, 20), bottom_cm = c(20, 45),
                      designation = c("A", "Bw"),
                      clay_pct = c(25, 30))
  )
  res <- neossolo_litolico(p)
  expect_false(isTRUE(res$passed))
  expect_true(isTRUE(res$evidence$has_B_designation))
})

test_that("neossolo_litolico does NOT fire on deep profile without B (but max > 50)", {
  p <- PedonRecord$new(
    site = list(id = "deep-A-only", lat = -22, lon = -43, country = "BR"),
    horizons = mk_h(top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 150),
                      designation = c("A1", "A2", "C"),
                      clay_pct = c(15, 18, 20))
  )
  res <- neossolo_litolico(p)
  expect_false(isTRUE(res$passed))
})

test_that("neossolo_litolico FALSE when contato_litico explicitly returned FALSE", {
  # contato_litico returns FALSE when surveyor described non-rock
  # material below 50 cm -- e.g. a horizontal series of A horizons
  # going to 100 cm with explicit non-R designation. The heuristic
  # should NOT fire because the surveyor explicitly contradicts the
  # implicit rock-contact signal.
  p <- PedonRecord$new(
    site = list(id = "deep-A-only", lat = -22, lon = -43, country = "BR"),
    horizons = mk_h(top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 150),
                      designation = c("A1", "A2", "C1"),
                      clay_pct = c(15, 18, 20))
  )
  cl <- contato_litico(p)
  # contato_litico should be FALSE (no R designation; profile deep)
  # so the heuristic gate does not fire.
  res <- neossolo_litolico(p)
  expect_false(isTRUE(res$passed))
})

# ---- regression: classify_sibcs end-to-end on FEBR-style input -------------

test_that("classify_sibcs returns 'Neossolos Litolicos' on FEBR-style shallow single-A", {
  p <- PedonRecord$new(
    site = list(id = "febr-style", lat = -22, lon = -43, country = "BR",
                  parent_material = "rocha gnaissica"),
    horizons = mk_h(top_cm = 0, bottom_cm = 20,
                      designation = "A",
                      clay_pct = 22, sand_pct = 55, silt_pct = 23,
                      ph_h2o = 5.5, oc_pct = 1.5,
                      bs_pct = 35, cec_cmol = 8)
  )
  res <- classify_sibcs(p, on_missing = "silent")
  expect_match(res$rsg_or_order %||% "", "Neossolos")
  expect_match(res$name %||% "", "Litolicos|Litolico")
})
