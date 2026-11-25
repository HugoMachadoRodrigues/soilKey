# v0.9.438: the diagnostics and key of the FAO-Unesco Revised Legend (1988)
# as the audit of the FAO legends read them (Hugo, 2026-11-24, option (a) of
# R6 to R9, R11 and R15, with the defects C4 and C5 and the G1 gap of
# 1988): depth windows read over the union of overlapping layers (C4); the
# field texture class where clay and silt are not measured (C5, R15); soft
# powdery lime (R6) and plinthite in the Gleysols gate (R7) as field
# features; the clay increase across a lithological discontinuity with clay
# skins (R8); the colour route for a mottled E (R9); boundaries not
# recorded at the Nitisols gate (R11); the stand-ins of the key-level
# predicates (G1).

.f438 <- function(p, ed = "1988") suppressWarnings(classify_fao(p, edition = ed, on_missing = "silent"))
.ex438 <- function(code, nm = FALSE) make_fao_example(code, edition = "1988", near_miss = nm)
# the horizons of a pedon, some rows kept (or added) and some columns set
.ped438 <- function(p, rows = NULL, ...) {
  h <- as.data.frame(p$horizons)
  if (!is.null(rows)) h <- h[rows, , drop = FALSE]
  rownames(h) <- NULL
  v <- list(...)
  for (k in names(v)) h[[k]] <- v[[k]]
  suppressWarnings(PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(h))))
}
.grp438 <- function(r) { lv <- r$levels; if (is.data.frame(lv) && nrow(lv)) lv$code[lv$level == "major_groups"][1L] else NA_character_ }

test_that("C4: a layer test reads a window the union of overlapping layers fills", {
  skip_on_cran()
  # 0-30 and 20-60 overlap; 20-50 lies inside the second; 60-130 below
  h <- data.frame(top_cm = c(0, 20, 20, 60), bottom_cm = c(30, 60, 50, 130),
                  clay_pct = c(10, 20, 22, 25))
  w <- soilKey:::.fao_window(h, 0, 125)
  expect_true(w$overlap)
  expect_true(w$filled)
  expect_false(w$covered)
  a <- soilKey:::.fao_in_window(h, rep(TRUE, 4), 0, 125, "throughout")
  expect_true(isTRUE(a$value))
  b <- soilKey:::.fao_in_window(h, c(FALSE, FALSE, TRUE, FALSE), 0, 125, "some")
  expect_true(isTRUE(b$value))
  b <- soilKey:::.fao_in_window(h, rep(FALSE, 4), 0, 125, "some")
  expect_false(b$value)
  s <- soilKey:::.fao_within(h, rep(FALSE, 4), 100)
  expect_false(s$value)
  # a mean over overlapping layers stays undecided (the reading of 2026-11-09)
  m <- soilKey:::.fao_weighted(h, "clay_pct", 0, 50)
  expect_true(is.na(m$value))
  expect_true("overlapping_layers" %in% m$flags)
})

test_that("C4: a gap stays a gap, even where overlaps hide it in the sum of thicknesses", {
  skip_on_cran()
  # 0-60 twice and 80-130: the thicknesses add up to more than 100 cm, the
  # union leaves 60-80 undescribed
  h <- data.frame(top_cm = c(0, 0, 80), bottom_cm = c(60, 60, 130))
  w <- soilKey:::.fao_window(h, 0, 100)
  expect_true(w$gap)
  expect_false(w$filled)
  a <- soilKey:::.fao_in_window(h, rep(TRUE, 3), 0, 100, "throughout")
  expect_true(is.na(a$value))
  expect_true(all(c("top_cm", "bottom_cm") %in% a$missing))
  expect_true(is.na(soilKey:::.fao_within(h, rep(FALSE, 3), 90)$value))
})

test_that("C4: the Phaeozems read base saturation over overlapping layers", {
  skip_on_cran()
  p <- .ex438("PH")
  h <- as.data.frame(p$horizons)
  # a second record of the second layer, shifted by 5 cm, with the same data
  extra <- h[2, , drop = FALSE]
  extra$top_cm <- extra$top_cm + 5; extra$bottom_cm <- extra$bottom_cm + 5
  extra$designation <- NA_character_
  h2 <- rbind(h, extra)
  q <- suppressWarnings(PedonRecord$new(site = p$site,
                                        horizons = ensure_horizon_schema(data.table::as.data.table(h2))))
  expect_true(soilKey:::.fao_window(as.data.frame(q$horizons), 0, 125)$overlap)
  bs <- soilKey:::base_saturation_fao88(q)
  expect_true(isTRUE(bs$passed))
})

test_that("C5 and R15: the field texture class decides where clay and silt are not measured", {
  skip_on_cran()
  h <- data.frame(clay_pct = c(4, NA, NA, NA), silt_pct = c(6, NA, NA, NA), sand_pct = c(90, NA, NA, NA),
                  texture_class_usda = c("C", "LS", "SL", NA))
  v <- soilKey:::.fao_sandy_loam_or_finer(h)
  # laboratory data first: the field class of the first layer is not read
  expect_identical(v, c(FALSE, FALSE, TRUE, NA))
  expect_identical(soilKey:::.fao_texture_field_flags(h),
                   c(NA, "texture_from_field_class", "texture_from_field_class", NA))
  expect_identical(soilKey:::.fao_coarser_than_sandy_loam(h), c(TRUE, TRUE, FALSE, NA))
  # the oxic B of 1974 reads the laboratory data only
  expect_identical(soilKey:::.fao_sandy_loam_or_finer(h, field_class = FALSE), c(FALSE, NA, NA, NA))
  # the Arenosols on the field class alone
  p <- .ex438("AR")
  n <- nrow(p$horizons)
  q <- .ped438(p, clay_pct = rep(NA_real_, n), silt_pct = rep(NA_real_, n), sand_pct = rep(NA_real_, n),
               texture_class_usda = rep("LS", n))
  ct <- soilKey:::coarse_texture_fao88(q)
  expect_true(isTRUE(ct$passed))
  expect_true("texture_from_field_class" %in% ct$evidence$flags)
  q2 <- .ped438(p, clay_pct = rep(NA_real_, n), silt_pct = rep(NA_real_, n), sand_pct = rep(NA_real_, n),
                texture_class_usda = rep("SL", n))
  expect_false(soilKey:::coarse_texture_fao88(q2)$passed)
  q3 <- .ped438(p, clay_pct = rep(NA_real_, n), silt_pct = rep(NA_real_, n), sand_pct = rep(NA_real_, n))
  expect_true(is.na(soilKey:::coarse_texture_fao88(q3)$passed))
})

test_that("C5: the argic B reads the field class where silt and sand are not measured", {
  skip_on_cran()
  p <- .ex438("LV")
  n <- nrow(p$horizons)
  # weatherable minerals recorded, so that the silt / clay ratio is not
  # needed to rule out a ferralic B
  q <- .ped438(p, silt_pct = rep(NA_real_, n), sand_pct = rep(NA_real_, n),
               weatherable_minerals_pct = rep(20, n),
               texture_class_usda = c("L", "L", "CL", "CL", "CL"))
  ar <- soilKey:::argic_b_fao88(q)
  expect_true(isTRUE(ar$passed))
  expect_true("texture_from_field_class" %in% ar$evidence$flags)
  q0 <- .ped438(p, silt_pct = rep(NA_real_, n), sand_pct = rep(NA_real_, n),
                weatherable_minerals_pct = rep(20, n))
  expect_true(is.na(soilKey:::argic_b_fao88(q0)$passed))
})

test_that("R6: no soft powdery lime below the deepest layer described", {
  skip_on_cran()
  p <- .ex438("LV")
  # described to 110 cm, no lime recorded
  q <- .ped438(p, rows = 1:4)
  r <- soilKey:::soft_powdery_lime_fao88(q, max_top_cm = 125)
  expect_false(r$passed)
  expect_match(r$notes, "absent_below_the_description", fixed = TRUE)
  # a gap in the depths still leaves it undecided
  g <- .ped438(p, rows = c(1, 2, 4))
  expect_true(is.na(soilKey:::soft_powdery_lime_fao88(g, max_top_cm = 125)$passed))
})

test_that("R7: the Gleysols gate reads plinthite as a field feature", {
  skip_on_cran()
  p <- .ex438("GL")
  # the description stops at 100 cm
  q <- .ped438(p, bottom_cm = c(20, 45, 100))
  expect_true(is.na(soilKey:::plinthite_fao88(q, max_top_cm = 125)$passed))
  pl <- soilKey:::plinthite_fao88(q, max_top_cm = 125, field = TRUE)
  expect_false(pl$passed)
  expect_match(pl$notes, "absent_below_the_description", fixed = TRUE)
  r <- .f438(q)
  expect_identical(.grp438(r), "GL")
  expect_false("GL" %in% vapply(r$ambiguities, function(a) a$rsg_code %||% "", ""))
})

test_that("R8: a clay increase across a lithological discontinuity counts with clay skins below", {
  skip_on_cran()
  p <- .ex438("LV")
  q <- .ped438(p, designation = c("A", "E", "2Bt1", "2Bt2", "2BC"))
  ar <- soilKey:::argic_b_fao88(q)
  expect_true(isTRUE(ar$passed))
  expect_true("clay_increase_across_discontinuity_with_clay_skins" %in% ar$evidence$flags)
  expect_identical(.f438(q)$name, .f438(p)$name)
  # without clay skins the pair stays undecided, and item 6, which asks
  # for clay skins over a discontinuity, fails the B as before
  q0 <- .ped438(p, designation = c("A", "E", "2Bw1", "2Bw2", "2BC"),
                clay_films_amount = rep(NA_character_, 5))
  expect_false(soilKey:::argic_b_fao88(q0)$passed)
  # the reader: compared only for a layer with skins
  h <- as.data.frame(q$horizons)
  expect_true(is.na(soilKey:::.fao_clay_increase(h)$value[3]))
  expect_true(isTRUE(soilKey:::.fao_clay_increase(h, skins = c(FALSE, FALSE, TRUE, TRUE, TRUE))$value[3]))
  expect_true(is.na(soilKey:::.fao_clay_increase(h, skins = rep(FALSE, 5))$value[3]))
})

test_that("R9: a mottled E takes the colour route; a plainly bleached E does not", {
  skip_on_cran()
  p <- make_planosol_canonical()
  st <- soilKey:::stagnic_properties_fao88(p, max_top_cm = 125)
  it <- st$evidence$items
  expect_true(isTRUE(it$reduction[2]))
  expect_match(st$notes, "reduction_from_colour_pattern_mottled_e", fixed = TRUE)
  r <- .f438(p)
  expect_identical(r$name, "Eutric Planosols")
  # the same E with no mottles: excluded, as on 2026-11-09
  q <- .ped438(p, redoximorphic_features_pct = c(0, 0, 20, 5))
  it0 <- soilKey:::stagnic_properties_fao88(q, max_top_cm = 125)$evidence$items
  expect_true(is.na(it0$reduction[2]))
  # mottles in a matrix of chroma 3: no colour route
  q3 <- .ped438(p, munsell_chroma_moist = c(3, 3, 2, 3))
  it3 <- soilKey:::stagnic_properties_fao88(q3, max_top_cm = 125)$evidence$items
  expect_true(is.na(it3$reduction[2]))
})

test_that("R11: a boundary not recorded leaves the Nitisols gate undecided", {
  skip_on_cran()
  p <- .ex438("NT")
  expect_true(isTRUE(soilKey:::nitisols_gate_fao88(p)$passed))
  q <- .ped438(p, boundary_distinctness = c("gradual", NA, "diffuse", "diffuse", NA))
  g <- soilKey:::nitisols_gate_fao88(q)
  expect_true(is.na(g$passed))
  expect_true("boundary_distinctness" %in% g$missing)
  q2 <- .ped438(p, boundary_distinctness = c("gradual", "clear", "diffuse", "diffuse", NA))
  expect_false(soilKey:::nitisols_gate_fao88(q2)$passed)
  # the abrupt textural change keeps reading a boundary not recorded as absent
  ab <- soilKey:::abrupt_textural_change_fao88(q)
  expect_false(isTRUE(ab$passed))
  # the canonical Nitisol: still Haplic Alisols, now past an undecided NT
  r <- .f438(make_nitisol_canonical())
  expect_identical(r$name, "Haplic Alisols")
  expect_identical(r$evidence_grade, "C")
  expect_true("NT" %in% vapply(r$ambiguities, function(a) a$rsg_code %||% "", ""))
})

test_that("G1: the key-level predicates of 1988 carry the stand-ins they read", {
  skip_on_cran()
  p <- make_podzol_canonical()
  sp <- soilKey:::spodic_b_fao88(p)
  expect_true("spodic_horizon_usda" %in% sp$evidence$stand_in)
  # a gate that fails on a spodic B decided by a stand-in carries it
  oh <- soilKey:::only_horizons_fao88(p, allowed = c("ochric_a", "albic_e", "histic_h"))
  expect_false(oh$passed)
  expect_true("spodic_horizon_usda" %in% oh$evidence$stand_in)
  expect_true(isTRUE(oh$evidence$lower_grade))
  # a gate that reads no stand-in carries none
  lv <- soilKey:::only_horizons_fao88(.ex438("LV"), allowed = c("ochric_a", "albic_e", "argic_b"))
  expect_length(lv$evidence$stand_in, 0L)
  expect_false(isTRUE(lv$evidence$lower_grade))
  # the builder
  x <- soilKey:::.f88_ctx(p)
  k <- soilKey:::.f88_key_result(x, "test", TRUE, "1", stand_in = c("spodic", NA, ""))
  expect_identical(k$evidence$stand_in, "spodic")
  expect_true(k$evidence$lower_grade)
  r <- .f438(p)
  expect_identical(r$evidence_grade, "C")
  expect_true(any(grepl("spodic_horizon_usda()", r$warnings, fixed = TRUE)))
})
