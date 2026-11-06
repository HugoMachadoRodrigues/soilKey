# v0.9.356: the South African soil classification of 1991, part 2: the five
# topsoil horizons and the eight apedal and structured B horizons on the
# recorded data (R/diagnostics-za-horizons.R), the colour readers of Hugo's
# decisions Q9-Q12 and the thresholds of inst/rules/za1991/thresholds.yaml.
# One passing layer, a failing twin and an undecided one (an attribute
# removed) per horizon; the stand-in flags in the trace.

# --- builders --------------------------------------------------------------------

# One layer: a plain, structureless, brown, non-calcareous mineral layer.
.za_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_moist = "10YR", munsell_value_moist = 5,
            munsell_chroma_moist = 4, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 20, sand_pct = 60, oc_pct = 0.3, effervescence_class = "none",
            clay_films_amount = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.za_P <- function(..., site = list()) {
  s <- list(id = "za", lat = -29, lon = 30)
  s[names(site)] <- site
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = s, horizons = h)
}
# An orthic A (weak, brown, 1 % OC, bases low) over a layer B.
.za_A <- function(...) .za_L(0, 25, designation = "A", structure_type = "subangular blocky",
                             structure_grade = "weak", oc_pct = 1, ca_cmol = 1, mg_cmol = 0.5,
                             k_cmol = 0.2, na_cmol = 0.1, ...)
.za_t <- function(f, p, i) f(soilKey:::.za_x(p), i)
.zp <- function(f, p, i) .za_t(f, p, i)$passed
.has_tag <- function(r, t) any(grepl(t, r$tag, fixed = TRUE))


test_that("thresholds.yaml: every number has a source and a stand_in flag", {
  skip_on_cran()
  th <- soilKey:::.za_thresholds()
  expect_true(length(th) >= 30L)
  for (n in names(th)) {
    expect_false(is.null(th[[n]]$value), info = n)
    expect_true(is.character(th[[n]]$source) && nzchar(th[[n]]$source), info = n)
    expect_true(is.logical(th[[n]]$stand_in), info = n)
  }
  # the stand-ins of Q9-Q12 are flagged; the numbers the open pages print are not
  expect_true(all(vapply(c("grey_dry", "yellow_brown_hue", "low_chroma", "gley_hues", "wetness_signs",
                           "organic_o_saturation_days", "calcareous_caco3"), function(n) th[[n]]$stand_in, NA)))
  expect_false(any(vapply(c("red_hues", "humic_oc", "base_ratio", "plasticity_index_cup", "apedal_cec",
                            "prismacutanic_clay", "plinthic_mottles", "organic_o_oc"), function(n) th[[n]]$stand_in, NA)))
  expect_identical(unlist(th$red_hues$value), c("5YR", "2.5YR", "10R", "7.5R"))
})

test_that("the colour readers (Q9-Q12)", {
  skip_on_cran()
  one <- function(...) .za_P(.za_L(0, 30, ...))
  f <- soilKey:::.za_red
  expect_true(.zp(f, one(munsell_hue_moist = "2.5YR"), 1))
  expect_true(.zp(f, one(munsell_hue_moist = "5YR"), 1))
  expect_true(.zp(f, one(munsell_hue_moist = "7.5R"), 1))
  expect_false(.zp(f, one(munsell_hue_moist = "7.5YR"), 1))
  # yellow dry, red moist: red (S 32)
  expect_true(.zp(f, one(munsell_hue_moist = "5YR", munsell_hue_dry = "10YR"), 1))
  # the dry hue only with no moist one, flagged
  r <- .za_t(f, one(munsell_hue_moist = NA, munsell_hue_dry = "2.5YR"), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "dry_hue_fallback"))
  expect_true(is.na(.zp(f, one(munsell_hue_moist = NA, munsell_hue_dry = NA), 1)))
  # redder than 7.5R: red, flagged as soilKey's reading
  r <- .za_t(f, one(munsell_hue_moist = "5R"), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:red_redder_hues"))
  # grey dry (Q9)
  g <- soilKey:::.za_grey_dry
  r <- .za_t(g, one(munsell_value_dry = 7, munsell_chroma_dry = 2), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:grey_dry"))
  expect_true(.zp(g, one(munsell_value_dry = 5, munsell_chroma_dry = 2), 1))
  expect_false(.zp(g, one(munsell_value_dry = 6, munsell_chroma_dry = 3), 1))
  # no dry colour: a moist chroma of 4 or more is not grey, flagged (Hugo, 2026-11-05); 3, undecided
  r <- .za_t(g, one(munsell_value_dry = NA, munsell_chroma_dry = NA), 1)
  expect_false(r$passed); expect_true(.has_tag(r, "grey_dry_from_moist"))
  expect_true(is.na(.zp(g, one(munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_chroma_moist = 3), 1)))
  # low chroma (Q12)
  l <- soilKey:::.za_low_chroma
  expect_true(.zp(l, one(munsell_chroma_moist = 2), 1))
  expect_true(.zp(l, one(munsell_hue_moist = "5GY", munsell_chroma_moist = 4), 1))
  expect_false(.zp(l, one(munsell_chroma_moist = 3), 1))
  # yellow-brown (Q10)
  y <- soilKey:::.za_yellow_brown
  r <- .za_t(y, one(munsell_hue_moist = "10YR", munsell_chroma_moist = 4), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:yellow_brown_hue"))
  expect_false(.zp(y, one(munsell_hue_moist = "5YR"), 1))
  expect_false(.zp(y, one(munsell_chroma_moist = 2), 1))
  expect_false(.zp(y, one(munsell_value_dry = 7, munsell_chroma_dry = 2), 1))
  expect_true(is.na(.zp(y, one(munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_chroma_moist = 3), 1)))
  # signs of wetness: low-chroma mottles on 5 % or more
  w <- soilKey:::.za_wetness
  expect_true(.zp(w, one(mottle_colors = "10YR 6/1", redoximorphic_features_pct = 10), 1))
  expect_false(.zp(w, one(mottle_colors = "10YR 6/1", redoximorphic_features_pct = 2), 1))
  expect_false(.zp(w, one(), 1))
  expect_true(is.na(.zp(w, one(redoximorphic_features_pct = 10), 1)))
})

test_that("organic O: 10 % OC over 200 mm and saturation by evidence (Q18)", {
  skip_on_cran()
  o <- function(...) .za_L(0, 25, designation = "O", oc_pct = 30, ...)
  wet <- .za_L(25, 60, munsell_chroma_moist = 1)
  r <- .za_t(soilKey:::.za_t_OO, .za_P(o(water_saturation_days = 200), wet), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:organic_o_saturation_days"))
  # saturation from the signs of wetness below, flagged
  r <- .za_t(soilKey:::.za_t_OO, .za_P(o(), wet), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "saturation_by_evidence"))
  # not saturated for long: litter excluded
  expect_false(.zp(soilKey:::.za_t_OO, .za_P(o(water_saturation_days = 5), wet), 1))
  # too little carbon
  expect_false(.zp(soilKey:::.za_t_OO, .za_P(o(oc_pct = 4, water_saturation_days = 200), wet), 1))
  # no saturation, no evidence: undecided
  r <- .za_t(soilKey:::.za_t_OO, .za_P(o(), .za_L(25, 60)), 1)
  expect_true(is.na(r$passed)); expect_true("water_saturation_days" %in% r$missing)
  # 10 cm at 30 % over a mineral layer at 1 %: 15.5 % over 200 mm
  expect_true(.zp(soilKey:::.za_oo_oc, .za_P(.za_L(0, 10, oc_pct = 30), .za_L(10, 40, oc_pct = 1)), 1))
  res <- organic_o_za(.za_P(o(water_saturation_days = 200), wet))
  expect_true(inherits(res, "DiagnosticResult")); expect_true(res$passed); expect_identical(res$layers, 1L)
})

test_that("vertic A: slickensides decide without the plasticity index (Q17)", {
  skip_on_cran()
  v <- function(...) .za_P(.za_L(0, 40, designation = "A", structure_type = "angular blocky",
                                 structure_grade = "strong", clay_pct = 55, ...), .za_L(40, 80))
  expect_true(.zp(soilKey:::.za_t_VA, v(slickensides = "common"), 1))
  r <- .za_t(soilKey:::.za_t_VA, v(slickensides = "few"), 1)
  expect_false(r$passed); expect_true(.has_tag(r, "slickensides_decide"))
  # the plasticity index over 32 (cup, by default flagged) or 36 (cone)
  r <- .za_t(soilKey:::.za_t_VA, v(plasticity_index = 35), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "liquid_limit_default"))
  expect_false(.zp(soilKey:::.za_t_VA, v(plasticity_index = 35, liquid_limit_method = "bs_cone"), 1))
  # not strong: no vertic A
  expect_false(.zp(soilKey:::.za_t_VA, v(slickensides = "many", structure_grade = "moderate"), 1))
  # slickensides recorded with no amount, no PI: undecided
  expect_true(is.na(.zp(soilKey:::.za_t_VA, v(slickensides = TRUE), 1)))
  expect_true(is.na(.zp(soilKey:::.za_t_VA, v(slickensides = "common", structure_grade = NA), 1)))
})

test_that("melanic A against humic A: the ratio of 4 per 1 % OC", {
  skip_on_cran()
  # 2 % OC, 40 % clay: ratio = S * 100 / 40 / 2 = 1.25 S
  m <- function(...) .za_P(.za_L(0, 30, designation = "A", structure_type = "subangular blocky",
                                 structure_grade = "moderate", munsell_hue_dry = "10YR",
                                 munsell_value_dry = 3, munsell_chroma_dry = 2, oc_pct = 2,
                                 clay_pct = 40, ca_cmol = 12, mg_cmol = 5, k_cmol = 0.5, na_cmol = 0.3, ...),
                           .za_L(30, 80, designation = "Bw", munsell_hue_moist = "10YR",
                                 munsell_chroma_moist = 4, munsell_value_dry = 6, munsell_chroma_dry = 4))
  r <- .za_t(soilKey:::.za_t_MA, m(), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "slickensides_decide"))
  expect_false(.zp(soilKey:::.za_t_HA, m(), 1))
  # low bases: humic, not melanic
  low <- m(ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.2, na_cmol = 0.1)
  expect_false(.zp(soilKey:::.za_t_MA, low, 1))
  expect_true(.zp(soilKey:::.za_t_HA, low, 1))
  # never both
  for (ca in c(0.5, 3, 4.5, 8)) {
    p <- m(ca_cmol = ca, mg_cmol = 0, k_cmol = 0, na_cmol = 0)
    expect_false(isTRUE(.zp(soilKey:::.za_t_MA, p, 1)) && isTRUE(.zp(soilKey:::.za_t_HA, p, 1)), info = ca)
  }
  # melanic colour: 10YR 3/3 and 5YR excluded; no dry colour undecided (default 9)
  expect_false(.zp(soilKey:::.za_t_MA, m(munsell_chroma_dry = 3, munsell_value_dry = 3), 1))
  expect_false(.zp(soilKey:::.za_t_MA, m(munsell_hue_dry = "5YR"), 1))
  r <- .za_t(soilKey:::.za_t_MA, m(munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_hue_dry = NA), 1)
  expect_true(is.na(r$passed)); expect_true(.has_tag(r, "dry_from_moist_evidence"))
  # common slickensides: vertic, not melanic
  expect_false(.zp(soilKey:::.za_t_MA, m(slickensides = "common"), 1))
  # massive and hard dry
  expect_false(.zp(soilKey:::.za_t_MA, m(structure_type = "massive", structure_grade = "structureless",
                                         rupture_resistance = "very hard"), 1))
  # a base not recorded: a lower bound decides when it suffices, else undecided
  expect_true(.zp(soilKey:::.za_t_MA, m(na_cmol = NA), 1))
  expect_true(is.na(.zp(soilKey:::.za_t_HA, m(ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.2, na_cmol = NA), 1)))
  # humic (iii): stripped grains
  r <- .za_t(soilKey:::.za_t_HA, m(ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.2, na_cmol = 0.1, uncoated_grains_pct = 40), 1)
  expect_false(r$passed)
})

test_that("humic A, the two passes: (iv) on the layer below, then orthic", {
  skip_on_cran()
  h <- .za_L(0, 30, designation = "A", oc_pct = 3, clay_pct = 30, ca_cmol = 1, mg_cmol = 0.5,
             k_cmol = 0.1, na_cmol = 0.1, structure_type = "subangular blocky", structure_grade = "weak")
  ok <- .za_P(h, .za_L(30, 90, designation = "Bw"))
  expect_true(.zp(soilKey:::.za_t_HA, ok, 1))
  expect_true(humic_a_za(ok)$passed)
  # directly over a wet grey layer: (iv) fails, the layer is orthic
  wet <- .za_P(h, .za_L(30, 90, designation = "Bg", munsell_chroma_moist = 1))
  expect_true(.zp(function(x, i) soilKey:::.za_t_HA(x, i, 1L), wet, 1))
  expect_false(.zp(soilKey:::.za_t_HA, wet, 1))
  r <- .za_t(soilKey:::.za_t_OA, wet, 1)
  expect_true(r$passed); expect_true(any(grepl("falls to orthic", r$note)))
  expect_identical(soilKey:::.za_topsoil(wet, 1)$code, "OA")
  # over a written red structured B
  rs <- .za_P(h, .za_L(30, 90, diagnostic_za = "red structured B"))
  expect_false(.zp(soilKey:::.za_t_HA, rs, 1))
  # over a layer with no dry colour and no designation: the E undecided
  und <- .za_P(h, .za_L(30, 90, munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_chroma_moist = 3))
  expect_true(is.na(.zp(soilKey:::.za_t_HA, und, 1)))
  tp <- soilKey:::.za_topsoil(und, 1)
  expect_true(is.na(tp$code)); expect_true(all(c("HA", "OA") %in% tp$candidates))
})

test_that("orthic A: none of the other four; undecided with them", {
  skip_on_cran()
  p <- .za_P(.za_A(), .za_L(25, 80, designation = "Bw"))
  expect_true(.zp(soilKey:::.za_t_OA, p, 1))
  expect_identical(soilKey:::.za_topsoil(p, 1)$code, "OA")
  expect_true(orthic_a_za(p)$passed)
  expect_false(.zp(soilKey:::.za_t_OA, .za_P(.za_L(0, 30, designation = "A", oc_pct = 3, clay_pct = 30, ca_cmol = 1,
                                                   mg_cmol = 0.2, k_cmol = 0.1, na_cmol = 0.1),
                                             .za_L(30, 80, designation = "Bw")), 1))
  r <- .za_t(soilKey:::.za_t_OA, .za_P(.za_A(oc_pct = 3, ca_cmol = NA, mg_cmol = NA, k_cmol = NA, na_cmol = NA),
                                       .za_L(25, 80, designation = "Bw")), 1)
  expect_true(is.na(r$passed)); expect_true("HA" %in% r$value$candidates)
  # the surface run: A layers only; a B first starts none
  expect_identical(soilKey:::.za_surface_run(soilKey:::.za_x(.za_P(.za_A(), .za_L(25, 40, designation = "A2"),
                                                                      .za_L(40, 80, designation = "Bw")))), c(1L, 2L))
  expect_length(soilKey:::.za_surface_run(soilKey:::.za_x(.za_P(.za_L(0, 40, designation = "Bw")))), 0L)
})

test_that("red and yellow-brown apedal B", {
  skip_on_cran()
  b <- function(...) .za_P(.za_A(), .za_L(25, 120, designation = "B", ...))
  red <- function(...) b(munsell_hue_moist = "2.5YR", munsell_chroma_moist = 6, ...)
  expect_true(.zp(soilKey:::.za_t_RAB, red(), 2))
  expect_true(red_apedal_b_za(red())$passed)
  expect_identical(red_apedal_b_za(red())$layers, 2L)
  expect_false(.zp(soilKey:::.za_t_RAB, red(structure_type = "subangular blocky", structure_grade = "moderate"), 2))
  expect_false(.zp(soilKey:::.za_t_RAB, red(effervescence_class = "strong"), 2))
  # more than 10 % mottles: not apedal when the layer is a soft plinthic B
  # (with grey mottles); undecided when their colours are not recorded (R3,
  # 0.9.357)
  expect_false(.zp(soilKey:::.za_t_RAB, red(redoximorphic_features_pct = 20, mottle_colors = "10YR 6/1"), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_RAB, red(redoximorphic_features_pct = 20), 2)))
  expect_true(is.na(.zp(soilKey:::.za_t_RAB, red(munsell_hue_moist = NA, munsell_hue_dry = NA), 2)))
  # borderline structure: the CEC under 11 cmol(+)/kg soil (V 21)
  bl <- function(...) red(structure_type = "subangular blocky", structure_grade = "weak to moderate", ...)
  expect_true(.zp(soilKey:::.za_t_RAB, bl(cec_cmol = 6), 2))
  expect_false(.zp(soilKey:::.za_t_RAB, bl(cec_cmol = 20), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_RAB, bl(), 2)))
  r <- .za_t(soilKey:::.za_t_RAB, bl(cec_cmol = 6, cec_method = "kcl"), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "cec_method_substitute"))
  # yellow-brown: 10YR 5/6 moist, not grey dry
  yb <- function(...) b(munsell_hue_moist = "10YR", munsell_chroma_moist = 6, munsell_value_dry = 6,
                        munsell_chroma_dry = 6, ...)
  r <- .za_t(soilKey:::.za_t_YAB, yb(), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:yellow_brown_hue"))
  expect_false(.zp(soilKey:::.za_t_YAB, yb(munsell_hue_moist = "5YR"), 2))
  expect_false(.zp(soilKey:::.za_t_YAB, yb(munsell_value_dry = 7, munsell_chroma_dry = 2), 2))
  expect_false(.zp(soilKey:::.za_t_YAB, yb(variegated_colors = "10YR 5/6; 7.5YR 5/8"), 2))
  expect_true(.zp(soilKey:::.za_t_YAB, yb(munsell_value_dry = NA, munsell_chroma_dry = NA), 2))
  r <- .za_t(soilKey:::.za_t_YAB, yb(munsell_value_dry = NA, munsell_chroma_dry = NA, munsell_chroma_moist = 3), 2)
  expect_true(is.na(r$passed)); expect_true("munsell_value_dry" %in% r$missing)
})

test_that("neocutanic and neocarbonate B", {
  skip_on_cran()
  b <- function(...) .za_P(.za_A(), .za_L(25, 100, designation = "B", structure_type = "subangular blocky",
                                         structure_grade = "weak", ...))
  expect_true(.zp(soilKey:::.za_t_NCU, b(variegated_colors = "10YR 5/4; 7.5YR 5/6"), 2))
  expect_true(.zp(soilKey:::.za_t_NCU, b(clay_films_amount = "few"), 2))
  # a uniform weak structure alone: apedal, not neocutanic
  expect_false(.zp(soilKey:::.za_t_NCU, b(), 2))
  expect_false(.zp(soilKey:::.za_t_NCU, b(clay_films_amount = "few", structure_grade = "moderate"), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_NCU, b(clay_films_amount = "few", structure_grade = NA, structure_type = NA), 2)))
  # neocarbonate: effervescence, else CaCO3 1 % or more flagged (Q22)
  expect_true(.zp(soilKey:::.za_t_NCA, b(effervescence_class = "moderate"), 2))
  r <- .za_t(soilKey:::.za_t_NCA, b(effervescence_class = NA, caco3_pct = 3), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:calcareous_caco3"))
  expect_false(.zp(soilKey:::.za_t_NCA, b(), 2))
  expect_false(.zp(soilKey:::.za_t_NCA, b(effervescence_class = "strong", stratification_pattern = "continuous"), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_NCA, b(effervescence_class = "strong", structure_grade = NA, structure_type = NA), 2)))
  expect_true(neocarbonate_b_za(b(effervescence_class = "moderate"))$passed)
})

test_that("pedocutanic and red structured B", {
  skip_on_cran()
  b <- function(a = list(), ...) .za_P(do.call(.za_A, a), .za_L(25, 100, designation = "Bt",
                                                                 structure_type = "angular blocky",
                                                                 structure_grade = "strong", clay_pct = 45, ...))
  # non-red with prominent cutans
  expect_true(.zp(soilKey:::.za_t_PCU, b(clay_films_amount = "many"), 2))
  expect_false(.zp(soilKey:::.za_t_PCU, b(clay_films_amount = "few"), 2))
  expect_false(.zp(soilKey:::.za_t_PCU, b(clay_films_amount = "many", structure_grade = "weak"), 2))
  # clay films not recorded: soilKey's argic() stands in, flagged
  r <- .za_t(soilKey:::.za_t_PCU, b(clay_films_amount = NA), 2)
  expect_true(.has_tag(r, "argic"))
  # red: the abrupt transition makes it pedocutanic, a gradual one red structured
  rd <- function(bd, ...) b(a = list(boundary_distinctness = bd), munsell_hue_moist = "2.5YR",
                            munsell_chroma_moist = 6, clay_films_amount = "many", ...)
  expect_true(.zp(soilKey:::.za_t_PCU, rd("abrupt"), 2))
  expect_false(.zp(soilKey:::.za_t_RSB, rd("abrupt"), 2))
  expect_false(.zp(soilKey:::.za_t_PCU, rd("gradual"), 2))
  expect_true(.zp(soilKey:::.za_t_RSB, rd("gradual"), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_PCU, rd(NA), 2)))
  # red structured B: not red, or weak, fails; no structure grade undecided
  expect_false(.zp(soilKey:::.za_t_RSB, rd("gradual", munsell_hue_moist = "10YR"), 2))
  expect_false(.zp(soilKey:::.za_t_RSB, rd("gradual", structure_grade = "weak"), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_RSB, rd("gradual", structure_grade = NA), 2)))
  # red with cutans of another colour: pedocutanic whatever the transition
  expect_true(.zp(soilKey:::.za_t_PCU, rd("gradual", variegated_colors = "2.5YR 4/6; 10YR 4/2"), 2))
  # a B2 of the same B reads the transition from the A
  p <- .za_P(.za_A(boundary_distinctness = "gradual"),
             .za_L(25, 60, designation = "Bt1", structure_type = "angular blocky", structure_grade = "strong",
                   munsell_hue_moist = "2.5YR", munsell_chroma_moist = 6, boundary_distinctness = "diffuse"),
             .za_L(60, 100, designation = "Bt2", structure_type = "angular blocky", structure_grade = "strong",
                   munsell_hue_moist = "2.5YR", munsell_chroma_moist = 6))
  expect_identical(red_structured_b_za(p)$layers, c(2L, 3L))
})

test_that("prismacutanic B: the clay rule (Q21) and the wet case", {
  skip_on_cran()
  b <- function(ca, cb, bd = "abrupt", ...) .za_P(.za_A(clay_pct = ca, boundary_distinctness = bd),
                                               .za_L(25, 90, designation = "Btn", structure_type = "columnar",
                                                     structure_grade = "strong", clay_pct = cb, ...))
  expect_true(.zp(soilKey:::.za_t_PRC, b(10, 20), 2))
  expect_false(.zp(soilKey:::.za_t_PRC, b(10, 19), 2))
  expect_true(.zp(soilKey:::.za_t_PRC, b(25, 45), 2))
  expect_false(.zp(soilKey:::.za_t_PRC, b(25, 44), 2))
  # 20 % exactly: the +20 rule (40 % either way)
  expect_true(.zp(soilKey:::.za_t_PRC, b(20, 40), 2))
  expect_false(.zp(soilKey:::.za_t_PRC, b(20, 39), 2))
  expect_false(.zp(soilKey:::.za_t_PRC, b(10, 30, bd = "gradual"), 2))
  expect_false(.zp(soilKey:::.za_t_PRC, b(10, 30, structure_type = "angular blocky"), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_PRC, b(NA, 30), 2)))
  # wet: dark cutans needed (flagged)
  r <- .za_t(soilKey:::.za_t_PRC, b(10, 30, munsell_chroma_moist = 1, coatings_value_moist = 2), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:prismacutanic_dark_cutans"))
  expect_false(.zp(soilKey:::.za_t_PRC, b(10, 30, munsell_chroma_moist = 1, coatings_value_moist = 5), 2))
  expect_true(is.na(.zp(soilKey:::.za_t_PRC, b(10, 30, munsell_chroma_moist = 1), 2)))
  expect_true(prismacutanic_b_za(b(10, 30))$passed)
  # prismatic under a gradual transition: pedocutanic (K 53-55)
  pr <- b(25, 35, bd = "gradual", structure_type = "prismatic", clay_films_amount = "many")
  expect_false(.zp(soilKey:::.za_t_PRC, pr, 2))
  expect_true(.zp(soilKey:::.za_t_PCU, pr, 2))
})

test_that("lithocutanic B", {
  skip_on_cran()
  b <- function(...) .za_P(.za_A(), .za_L(25, 60, ...), .za_L(60, 80, designation = "R"))
  r <- .za_t(soilKey:::.za_t_LCU, b(designation = "BC", bedrock_pct = 40, clay_films_amount = "common"), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:lithocutanic_rock"))
  expect_true(.zp(soilKey:::.za_t_LCU, b(designation = "BCr"), 2))
  expect_false(.zp(soilKey:::.za_t_LCU, b(designation = "Bw"), 2))
  expect_false(.zp(soilKey:::.za_t_LCU, b(designation = "BC", bedrock_pct = 100), 2))
  expect_false(.zp(soilKey:::.za_t_LCU, b(designation = "BC", bedrock_pct = 40, pan_type = "calcrete"), 2))
  r <- .za_t(soilKey:::.za_t_LCU, b(designation = "BC"), 2)
  expect_true(is.na(r$passed)); expect_true("bedrock_pct" %in% r$missing)
  expect_true(lithocutanic_b_za(b(designation = "BC", bedrock_pct = 40))$passed)
})

test_that("the predicates over a pedon: undecided, and the flags in the trace", {
  skip_on_cran()
  p <- .za_P(.za_A(), .za_L(25, 100, designation = "B", munsell_hue_moist = "10YR", munsell_chroma_moist = 3,
                            munsell_value_dry = NA, munsell_chroma_dry = NA))
  r <- yellow_brown_apedal_b_za(p)
  expect_true(is.na(r$passed))
  expect_true("munsell_value_dry" %in% r$missing)
  expect_match(r$notes, "stand_in:yellow_brown_hue")
  expect_identical(r$reference, soilKey:::.ZA_REFERENCE)
  # a pedon with no B: FALSE, not NA
  expect_false(red_apedal_b_za(.za_P(.za_A()))$passed)
  # a lithocutanic B that the hard-rock name rules out is not required here: the
  # tests do not depend on the other layers' identification
  expect_true(all(c("organic_o_za", "humic_a_za", "vertic_a_za", "melanic_a_za", "orthic_a_za",
                    "red_apedal_b_za", "yellow_brown_apedal_b_za", "red_structured_b_za", "neocutanic_b_za",
                    "neocarbonate_b_za", "pedocutanic_b_za", "prismacutanic_b_za", "lithocutanic_b_za") %in%
                  ls(asNamespace("soilKey"))))
})


# --- the review of 2026-11-05 and Hugo's amendments ----------------------------------

test_that("D1: slickensides present with no amount and a failing PI stay undecided", {
  skip_on_cran()
  p <- .za_P(.za_L(0, 40, designation = "A", structure_type = "angular blocky", structure_grade = "strong",
                   slickensides = TRUE, plasticity_index = 30, clay_pct = 55, oc_pct = 1.5))
  r <- .za_t(soilKey:::.za_vertic_ii, p, 1)
  expect_true(is.na(r$passed)); expect_true("slickensides" %in% r$missing)
  expect_true(is.na(.zp(soilKey:::.za_ma_not_vertic, p, 1)))
  # the PI passing decides whatever the slickensides
  p2 <- .za_P(.za_L(0, 40, designation = "A", structure_type = "angular blocky", structure_grade = "strong",
                    slickensides = TRUE, plasticity_index = 40))
  expect_true(.zp(soilKey:::.za_vertic_ii, p2, 1))
})

test_that("D2: red structured and red pedocutanic share the colour-contrast reading", {
  skip_on_cran()
  # a Lusiki-like humic A over a red blocky B with common clay films, transition not recorded
  p <- .za_P(.za_L(0, 30, designation = "A", oc_pct = 3, ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.1, na_cmol = 0.1,
                   clay_pct = 30, structure_type = "granular", structure_grade = "weak", munsell_hue_moist = "7.5YR",
                   munsell_value_moist = 3, munsell_chroma_moist = 2, munsell_value_dry = 4, munsell_chroma_dry = 3),
             .za_L(30, 90, designation = "Bt", munsell_hue_moist = "2.5YR", munsell_value_moist = 3,
                   munsell_chroma_moist = 6, structure_type = "angular blocky", structure_grade = "moderate",
                   clay_films_amount = "common", clay_pct = 45))
  expect_true(is.na(.zp(soilKey:::.za_t_RSB, p, 2)))
  expect_true(is.na(.zp(soilKey:::.za_t_PCU, p, 2)))
  tp <- soilKey:::.za_topsoil(p, 1)
  expect_true(is.na(tp$code)); expect_true(all(c("HA", "OA") %in% tp$candidates))
  # coatings of another colour: pedocutanic, not red structured
  p2 <- .za_P(.za_A(), .za_L(25, 90, designation = "Bt", munsell_hue_moist = "2.5YR", munsell_value_moist = 3,
                              munsell_chroma_moist = 6, structure_type = "angular blocky", structure_grade = "moderate",
                              clay_films_amount = "common", coatings_value_moist = 5, coatings_hue_moist = "10YR"))
  expect_true(.zp(soilKey:::.za_t_PCU, p2, 2))
  expect_false(.zp(soilKey:::.za_t_RSB, p2, 2))
})

test_that("D3: the organic O bound by the carbon above", {
  skip_on_cran()
  p <- .za_P(.za_L(0, 15, designation = "A", oc_pct = 0.8, structure_type = "subangular blocky", structure_grade = "weak",
                   ca_cmol = 2, mg_cmol = 1, k_cmol = 0.2, na_cmol = 0.1),
             .za_L(15, 80, designation = "B", oc_pct = NA, munsell_hue_moist = "2.5YR", munsell_chroma_moist = 6))
  r <- .za_t(soilKey:::.za_oo_oc, p, 1)
  expect_false(r$passed); expect_true(.has_tag(r, "oc_bound_from_above"))
  expect_identical(soilKey:::.za_topsoil(p, 1)$code, "OA")
  # an A2 with no carbon under an A1 with 4 %: bounded too
  p2 <- .za_P(.za_L(0, 25, designation = "A1", oc_pct = 4), .za_L(25, 50, designation = "A2", oc_pct = NA),
              .za_L(50, 100, designation = "B"))
  expect_false(.zp(soilKey:::.za_oo_oc, p2, 2))
  # peat over an O with no carbon: still undecided
  expect_true(is.na(.zp(soilKey:::.za_oo_oc, .za_P(.za_L(0, 5, designation = "O", oc_pct = 25),
                                                     .za_L(5, 40, designation = "O2", oc_pct = NA)), 1)))
})

test_that("D4: low chroma needs a value of 4 or more outside the gley hues; 10Y is gley", {
  skip_on_cran()
  one <- function(...) .za_P(.za_L(0, 30, ...))
  l <- soilKey:::.za_low_chroma
  expect_false(.zp(l, one(munsell_hue_moist = "10YR", munsell_value_moist = 3, munsell_chroma_moist = 2), 1))
  expect_true(.zp(l, one(munsell_hue_moist = "10YR", munsell_value_moist = 4, munsell_chroma_moist = 2), 1))
  r <- .za_t(l, one(munsell_hue_moist = "10Y", munsell_value_moist = 5, munsell_chroma_moist = 3), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:low_chroma_value"))
  expect_true(.zp(l, one(munsell_hue_moist = "GY", munsell_value_moist = 5, munsell_chroma_moist = 3), 1))
  expect_false(.zp(soilKey:::.za_wetness, one(munsell_hue_moist = "10YR", munsell_value_moist = 3, munsell_chroma_moist = 2), 1))
  th <- soilKey:::.za_thresholds()
  expect_identical(th$low_chroma$source, "soilkey")
  expect_true("10Y" %in% unlist(th$gley_hues$value))
})

test_that("D5: red needs a colour that is not grey (5YR 6/1 is not red)", {
  skip_on_cran()
  p <- .za_P(.za_A(), .za_L(25, 80, designation = "B", munsell_hue_moist = "5YR", munsell_value_moist = 6,
                            munsell_chroma_moist = 1))
  r <- .za_t(soilKey:::.za_red, p, 2)
  expect_false(r$passed); expect_true(.has_tag(r, "stand_in:low_chroma"))
  expect_false(.zp(soilKey:::.za_t_RAB, p, 2))
  # a dark red 2.5YR 3/2 stays red
  expect_true(.zp(soilKey:::.za_red, .za_P(.za_L(0, 30, munsell_hue_moist = "2.5YR", munsell_value_moist = 3,
                                                 munsell_chroma_moist = 2)), 1))
  expect_false(.zp(soilKey:::.za_red, .za_P(.za_L(0, 30, munsell_hue_moist = "GY")), 1))
})

test_that("D6: no B horizon on a layer of rock", {
  skip_on_cran()
  bare <- .za_L(15, 30, designation = "R", munsell_hue_moist = NA, munsell_value_moist = NA, munsell_chroma_moist = NA,
                munsell_hue_dry = NA, munsell_value_dry = NA, munsell_chroma_dry = NA, structure_type = NA, structure_grade = NA)
  for (p in list(.za_P(.za_A(0, 15), .za_L(15, 30, designation = "R")), .za_P(.za_A(0, 15), bare),
                 .za_P(.za_A(0, 15), .za_L(15, 30, bedrock_pct = 100)))) {
    for (f in c("red_apedal_b_za", "yellow_brown_apedal_b_za", "red_structured_b_za", "neocutanic_b_za",
                "neocarbonate_b_za", "pedocutanic_b_za", "prismacutanic_b_za", "lithocutanic_b_za")) {
      expect_false(get(f, envir = asNamespace("soilKey"))(p)$passed, info = f)
    }
  }
})

test_that("D7: a borderline grade with a CEC of 11 or more is moderate, flagged", {
  skip_on_cran()
  b <- function(...) .za_P(.za_A(), .za_L(25, 80, designation = "B", munsell_hue_moist = "2.5YR", munsell_chroma_moist = 6,
                                          structure_type = "subangular blocky", structure_grade = "weak to moderate", ...))
  r <- .za_t(soilKey:::.za_blocky, b(cec_cmol = 20, cec_method = "acetate_ph7"), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "borderline_cec"))
  expect_false(.zp(soilKey:::.za_t_RAB, b(cec_cmol = 20), 2))
  expect_false(.zp(soilKey:::.za_blocky, b(cec_cmol = 6), 2))
  expect_true(is.na(.zp(soilKey:::.za_blocky, b(), 2)))
})

test_that("D8: a uniform weak layer neither red nor yellow-brown is neocutanic", {
  skip_on_cran()
  p <- .za_P(.za_A(), .za_L(25, 80, designation = "B", munsell_hue_moist = "7.5YR", munsell_value_moist = 3,
                            munsell_chroma_moist = 2, structure_type = "subangular blocky", structure_grade = "weak"))
  r <- .za_t(soilKey:::.za_t_NCU, p, 2)
  expect_true(r$passed); expect_true(.has_tag(r, "uniform_dark_brown"))
  expect_false(.zp(soilKey:::.za_t_RAB, p, 2)); expect_false(.zp(soilKey:::.za_t_YAB, p, 2))
  # red or yellow-brown and uniform: apedal, not neocutanic
  q <- .za_P(.za_A(), .za_L(25, 80, designation = "B", munsell_hue_moist = "2.5YR", munsell_chroma_moist = 6,
                            structure_type = "subangular blocky", structure_grade = "weak"))
  expect_false(.zp(soilKey:::.za_t_NCU, q, 2)); expect_true(.zp(soilKey:::.za_t_RAB, q, 2))
  # a grey low-chroma weak layer is no dark brown
  g <- .za_P(.za_A(), .za_L(25, 80, designation = "B", munsell_value_moist = 6, munsell_chroma_moist = 1,
                            structure_type = "subangular blocky", structure_grade = "weak"))
  expect_false(.zp(soilKey:::.za_t_NCU, g, 2))
})

test_that("D9: the words of effervescence; FC 16 relict nodules", {
  skip_on_cran()
  e <- function(v, ...) .zp(soilKey:::.za_calcareous, .za_P(.za_L(0, 30, effervescence_class = v, ...)), 1)
  for (v in c("none", "nil", "0", "-", "noneffervescent", "not effervescent")) expect_false(e(v), info = v)
  for (v in c("slight", "very slight", "weak", "moderate", "strong", "violent")) expect_true(e(v), info = v)
  for (v in c("distinct", "?")) expect_true(is.na(e(v)), info = v)
  expect_true(is.na(e(NA, caco3_pct = 5, carbonate_hard_pct = 3)))
  expect_true(e(NA, caco3_pct = 5))
})

test_that("the old notation: an A2 under an A1 is a candidate E", {
  skip_on_cran()
  run <- function(...) soilKey:::.za_surface_run(soilKey:::.za_x(.za_P(...)))
  expect_identical(run(.za_L(0, 20, designation = "A1"), .za_L(20, 40, designation = "A2"),
                       .za_L(40, 80, designation = "B")), c(1L, 2L))
  expect_identical(run(.za_L(0, 20, designation = "A1"),
                       .za_L(20, 40, designation = "A2", munsell_value_dry = 7, munsell_chroma_dry = 2),
                       .za_L(40, 80, designation = "B")), 1L)
  p <- .za_P(.za_A(designation = "A1"), .za_L(25, 40, designation = "A2", munsell_value_dry = 7, munsell_chroma_dry = 2),
             .za_L(40, 80, designation = "B"))
  expect_match(orthic_a_za(p)$notes, "a2_old_notation")
  # capitals: AP and AH are topsoil; AB is not
  expect_identical(run(.za_L(0, 20, designation = "AP"), .za_L(20, 40, designation = "B")), 1L)
  expect_identical(run(.za_L(0, 20, designation = "AH"), .za_L(20, 40, designation = "AB")), 1L)
})

test_that("Q6: a stratified calcareous layer stays the material", {
  skip_on_cran()
  b <- function(...) .za_P(.za_A(), .za_L(25, 80, designation = "C", effervescence_class = "strong", ...))
  expect_true(.zp(soilKey:::.za_t_NCA, b(), 2))
  expect_false(.zp(soilKey:::.za_t_NCA, b(stratification_pattern = "continuous"), 2))
  expect_false(.zp(soilKey:::.za_t_NCA, b(cross_bedding = TRUE), 2))
})

test_that("the minor points of the review", {
  skip_on_cran()
  one <- function(...) .za_P(.za_L(0, 30, ...))
  # thresholds read from the yaml
  src <- testthat::test_path("..", "..", "R", "diagnostics-za-horizons.R")
  skip_if_not(file.exists(src), "package sources not available")
  txt <- readLines(src, warn = FALSE)
  expect_true(any(grepl('"apedal_max_grade"', txt, fixed = TRUE)))
  expect_true(any(grepl('"vertic_slickensides"', txt, fixed = TRUE)))
  expect_false(any(grepl("[^\\x01-\\x7F]", txt, perl = TRUE)))
  # humic (i) over the mineral A only: a litter O does not make the A humic
  q <- .za_P(.za_L(0, 3, designation = "O", oc_pct = 35),
             .za_L(3, 40, designation = "A", oc_pct = 1.2, clay_pct = 30, ca_cmol = 0.5, mg_cmol = 0.2, k_cmol = 0.1, na_cmol = 0.1),
             .za_L(40, 100, designation = "B"))
  expect_false(.zp(soilKey:::.za_ha_oc, q, 2))
  # boundary words
  ab <- function(b) soilKey:::.za_abrupt(soilKey:::.za_x(.za_P(.za_L(0, 30, boundary_distinctness = b), .za_L(30, 60))), 1L)
  for (b in c("abrupt", "very abrupt", "sharp", "A", "a", "smooth abrupt")) expect_true(ab(b), info = b)
  for (b in c("clear", "gradual", "diffuse")) expect_false(ab(b), info = b)
  expect_true(is.na(ab("wavy")))
  # prismacutanic: a blocky B1 over a columnar B2 compares with the A
  q <- .za_P(.za_A(clay_pct = 10), .za_L(25, 40, designation = "Btn1", structure_type = "angular blocky",
                                         structure_grade = "strong", clay_pct = 40),
             .za_L(40, 80, designation = "Btn2", structure_type = "columnar", structure_grade = "strong", clay_pct = 45))
  r <- .za_t(soilKey:::.za_t_PRC, q, 3)
  expect_true(r$passed); expect_true(any(grepl("colour contrast", r$note)))
  # the organic O saturation: signs of wetness only within 50 cm below
  q <- .za_P(.za_L(0, 30, designation = "O", oc_pct = 30), .za_L(30, 140, designation = "C", oc_pct = 0.5),
             .za_L(140, 180, designation = "Cg", munsell_chroma_moist = 1))
  expect_true(is.na(.zp(soilKey:::.za_oo_saturation, q, 1)))
  q <- .za_P(.za_L(0, 30, designation = "O", oc_pct = 30), .za_L(30, 60, designation = "Cg", munsell_chroma_moist = 1))
  r <- .za_t(soilKey:::.za_oo_saturation, q, 1)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:organic_o_wetness_cm"))
  # 10.0YR 3/3 is the excluded 10YR 3/3; the moist hue stands in for a missing dry hue
  expect_false(.zp(soilKey:::.za_ma_colour, one(munsell_hue_dry = "10.0YR", munsell_value_dry = 3, munsell_chroma_dry = 3), 1))
  r <- .za_t(soilKey:::.za_ma_colour, one(munsell_hue_dry = NA, munsell_value_dry = 2, munsell_chroma_dry = 1), 1)
  expect_true(r$passed); expect_true(.has_tag(r, "dry_hue_from_moist"))
  # .za_not adds its note only when the negation holds
  r <- soilKey:::.za_not(soilKey:::.rp_r(TRUE), "only when it holds")
  expect_false("only when it holds" %in% r$note)
  expect_true("only when it holds" %in% soilKey:::.za_not(soilKey:::.rp_r(FALSE), "only when it holds")$note)
})
