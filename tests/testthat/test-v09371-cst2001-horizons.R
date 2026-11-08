# v0.9.371: the 33 diagnostic horizons of the Chinese Soil Taxonomy (2001,
# English edition), their evidences, grades and stand-ins, and the horizon
# panel .cst_identify().

.cp <- function(h, site = list()) {
  PedonRecord$new(site = c(list(id = "cst"), site), horizons = h)
}
# the full and evidence grades of a reader, per layer in depth order
.cg <- function(p, id) {
  r <- soilKey:::.cst_hz(soilKey:::.cst_ctx(p), id)
  list(full = r$full, evid = r$evid, r = r)
}
.cres <- function(p, fn, ...) get(fn, envir = asNamespace("soilKey"))(p, ...)


test_that("the table holds the 33 horizons and every reader runs on an empty profile", {
  skip_on_cran()
  t <- soilKey:::.cst_horizon_table()
  expect_equal(nrow(t), 33L)
  expect_equal(sum(t$group == "surface"), 11L)
  expect_equal(sum(t$group == "subsurface"), 20L)
  expect_equal(sum(t$group == "other"), 2L)
  expect_true(all(vapply(t$fun, exists, NA, envir = asNamespace("soilKey"))))
  # the book's evidences tied to horizons (p. 19 list, the horizon ones)
  expect_setequal(t$id[t$evidence],
                  c("histic_epipedon", "mattic_epipedon", "siltigic_epipedon", "cumulic_epipedon",
                    "fimic_epipedon", "anthrostagnic_epipedon", "glossic_horizon", "plinthic_horizon",
                    "spodic_horizon", "agric_horizon", "hydragric_horizon", "alkalic_horizon",
                    "gypsic_horizon", "calcic_horizon", "salic_horizon"))
  p0 <- .cp(data.frame(top_cm = numeric(0), bottom_cm = numeric(0)))
  for (f in t$fun) expect_false(isTRUE(.cres(p0, f)$passed))
  expect_equal(nrow(soilKey:::.cst_identify(p0)), 0L)
  thr <- soilKey:::.cst_thr("horizons", "argic", "clay_increase")
  expect_equal(c(thr$low_below_pct, thr$mid_ratio, thr$high_plus_points, thr$fine_plus_points), c(15, 1.2, 8, 8))
  r <- yaml::read_yaml(system.file("rules", "cst2001", "readings.yaml", package = "soilKey"))$readings
  rd <- vapply(r, `[[`, "", "reading")
  expect_true(sum(grepl("\\(Hugo, 2026-11-06\\)\\.$", rd)) >= 25L)
  expect_false(any(grepl("for Hugo to confirm, 2026-11-05", rd)))
})

test_that("grades: full, evidence only and at least evidence", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 60), designation = c("A", "Bv", "C"),
                  plinthite_pct = c(0, 12, 0))
  p <- .cp(h)
  r <- .cres(p, "plinthic_horizon_cst")
  expect_false(r$passed)
  expect_equal(soilKey:::.cst_grade(r), "evidence")
  expect_true(.cres(p, "plinthic_horizon_cst", grade = "evidence")$passed)
  expect_true(.cres(p, "plinthic_horizon_cst", grade = "at_least_evidence")$passed)
  expect_equal(unname(r$evidence$layer_grade), c("none", "evidence", "none"))
  h$bottom_cm[2] <- 40; h$top_cm[3] <- 40
  p <- .cp(h)
  expect_true(.cres(p, "plinthic_horizon_cst")$passed)
  expect_false(.cres(p, "plinthic_horizon_cst", grade = "evidence")$passed)
  expect_true(.cres(p, "plinthic_horizon_cst", grade = "at_least_evidence")$passed)
  # top_max: the upper boundary within a depth
  expect_true(.cres(p, "plinthic_horizon_cst", top_max = 20)$passed)
  expect_false(.cres(p, "plinthic_horizon_cst", top_max = 10)$passed)
})


# --- organic epipedons ------------------------------------------------------------

test_that("histic epipedon: peaty and littery kinds, thickness, carbon, evidence", {
  skip_on_cran()
  mk <- function(thk, oc, sat, clay = NA, site = list(climatic_moisture_regime = "udic"), sph = NA, bd = NA) {
    .cp(data.frame(top_cm = c(0, thk), bottom_cm = c(thk, thk + 50), designation = c("Oa", "C"),
                   oc_pct = c(oc, 0.5), clay_pct = c(clay, 20), water_saturation_days = c(sat, sat),
                   sphagnum_fibres_pct = c(sph, NA), bulk_density_g_cm3 = c(bd, 1.3)), site)
  }
  g <- .cg(mk(30, 30, 200), "histic_epipedon")
  expect_equal(g$full, c(TRUE, FALSE))
  expect_match(g$r$kind[1], "^peaty")
  # littery: saturated under a month, 20 % carbon, 20 cm or more
  g <- .cg(mk(25, 25, 10), "histic_epipedon")
  expect_equal(g$full[1], TRUE); expect_match(g$r$kind[1], "^littery")
  expect_false(.cg(mk(25, 15, 10), "histic_epipedon")$full[1])
  # saturation unknown: passes only when both kinds pass
  expect_true(.cg(mk(25, 25, NA), "histic_epipedon")$full[1])
  expect_true(is.na(.cg(mk(25, 14, NA, clay = 10), "histic_epipedon")$full[1]))
  # peaty carbon 12 + 0.1 x clay: 13 % at 10 % clay passes, 12.9 fails
  expect_true(.cg(mk(30, 13, 200, clay = 10), "histic_epipedon")$full[1])
  expect_false(.cg(mk(30, 12.9, 200, clay = 10), "histic_epipedon")$full[1])
  # without clay, between 12 and 18 % the carbon is undecided
  expect_true(is.na(.cg(mk(30, 15, 200), "histic_epipedon")$full[1]))
  # thicker than 40 cm without sphagnum: no histic epipedon (an organic soil);
  # 60 cm with sphagnum; sphagnum and density not recorded: undecided
  expect_false(.cg(mk(45, 30, 200, sph = 10, bd = 0.3), "histic_epipedon")$full[1])
  expect_true(.cg(mk(55, 30, 200, sph = 80), "histic_epipedon")$full[1])
  expect_false(.cg(mk(65, 30, 200, sph = 80), "histic_epipedon")$full[1])
  expect_true(is.na(.cg(mk(45, 30, 200), "histic_epipedon")$full[1]))
  # 40 cm in total within the upper 50 cm, with a mineral layer over it
  h <- data.frame(top_cm = c(0, 8, 50), bottom_cm = c(8, 50, 90), designation = c("A", "Oa", "C"),
                  oc_pct = c(2, 30, 0.5), clay_pct = c(20, NA, 20), water_saturation_days = 200)
  expect_true(.cg(.cp(h), "histic_epipedon")$full[2])
  # evidence: all but thickness, 5 cm (3 cm in an arid region)
  g <- .cg(mk(5, 30, 200), "histic_epipedon")
  expect_equal(c(g$full[1], g$evid[1]), c(FALSE, TRUE))
  expect_false(.cg(mk(4, 30, 200), "histic_epipedon")$evid[1])
  expect_true(.cg(mk(3, 30, 200, site = list(climatic_moisture_regime = "aridic")), "histic_epipedon")$evid[1])
  expect_true(is.na(.cg(mk(4, 30, 200, site = list()), "histic_epipedon")$evid[1]))
})

test_that("mattic epipedon: the root mat, its evidence, undecided density", {
  skip_on_cran()
  mk <- function(thk = 8, roots = 60, bd = 0.8, mat = TRUE) {
    .cp(data.frame(top_cm = c(0, thk), bottom_cm = c(thk, 40), designation = c("A", "B"),
                   root_mat = c(mat, NA), root_mat_pct = c(roots, NA),
                   munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5), munsell_value_dry = c(5, 6),
                   munsell_chroma_moist = c(2, 4), oc_pct = c(30, 1), n_total_pct = c(1.8, 0.1),
                   water_saturation_days = c(10, 10), bulk_density_g_cm3 = c(bd, 1.3)),
        list(soil_temperature_regime = "cryic"))
  }
  expect_equal(.cg(mk(), "mattic_epipedon")$full, c(TRUE, FALSE))
  g <- .cg(mk(roots = 30), "mattic_epipedon")
  expect_equal(c(g$full[1], g$evid[1]), c(FALSE, TRUE))
  g <- .cg(mk(thk = 4), "mattic_epipedon")
  expect_equal(c(g$full[1], g$evid[1]), c(FALSE, TRUE))
  expect_false(.cg(mk(thk = 1.5, roots = 30), "mattic_epipedon")$evid[1])
  expect_false(.cg(mk(roots = 9), "mattic_epipedon")$evid[1])
  expect_true(is.na(.cg(mk(bd = NA), "mattic_epipedon")$full[1]))
  # a recorded share stands for the mat; nothing recorded is no mat
  expect_true(.cg(mk(mat = NA), "mattic_epipedon")$full[1])
  expect_false(.cg(mk(mat = NA, roots = NA), "mattic_epipedon")$full[1])
})


# --- humic epipedons ----------------------------------------------------------------------

.humic <- function(a_bot = 30, bs = 70, vd = 4, oc = 2, st = "granular") {
  data.frame(top_cm = c(0, a_bot, 60), bottom_cm = c(a_bot, 60, 100), designation = c("A", "Bw", "C"),
             munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, 5), munsell_value_dry = c(vd, 6, 7),
             munsell_chroma_moist = c(2, 4, 4), munsell_chroma_dry = c(2, 4, 4),
             oc_pct = c(oc, 0.5, 0.2), bs_pct = c(bs, 60, 60), structure_type = c(st, "blocky", "massive"),
             clay_pct = 25, sand_pct = 30, silt_pct = 45)
}

test_that("mollic, umbric and ochric epipedons", {
  skip_on_cran()
  p <- .cp(.humic())
  expect_equal(.cg(p, "mollic_epipedon")$full, c(TRUE, FALSE, FALSE))
  expect_equal(.cg(p, "umbric_epipedon")$full, c(FALSE, FALSE, FALSE))
  expect_equal(.cg(p, "ochric_epipedon")$full, c(FALSE, FALSE, FALSE))
  # base saturation under 50: umbric
  p <- .cp(.humic(bs = 30))
  expect_false(.cg(p, "mollic_epipedon")$full[1])
  expect_true(.cg(p, "umbric_epipedon")$full[1])
  # the solum is 60 cm: more than a third (20 cm) and 18 cm
  expect_false(.cg(.cp(.humic(a_bot = 20)), "mollic_epipedon")$full[1])
  expect_true(.cg(.cp(.humic(a_bot = 21)), "mollic_epipedon")$full[1])
  # light colour: ochric
  h <- .humic(); h$munsell_value_moist[1] <- 5
  p <- .cp(h)
  expect_false(.cg(p, "mollic_epipedon")$full[1])
  expect_true(.cg(p, "ochric_epipedon")$full[1])
  # carbon not measured: undecided (no stand-in can decide it)
  p <- .cp(.humic(oc = NA))
  expect_true(is.na(.cg(p, "mollic_epipedon")$full[1]))
  expect_true(is.na(.cres(p, "mollic_epipedon_cst")$passed))
  expect_true(is.na(.cg(p, "ochric_epipedon")$full[1]))
  # massive and hard when dry: no mollic structure
  h <- .humic(st = "massive"); h$rupture_resistance <- c("hard", NA, NA)
  expect_false(.cg(.cp(h), "mollic_epipedon")$full[1])
})

test_that("mollic epipedon: the USDA mollic stands in for a missing dry colour, flagged", {
  skip_on_cran()
  p <- .cp(.humic(vd = NA))
  r <- .cres(p, "mollic_epipedon_cst")
  usda <- soilKey:::mollic_epipedon_usda(p)$passed
  if (isTRUE(usda)) {
    expect_true(r$passed)
    expect_true("mollic_epipedon_usda" %in% r$evidence$stand_in)
    expect_true(r$evidence$lower_grade)
    expect_match(r$notes, "stand_in:mollic_epipedon_usda")
  } else {
    expect_true(is.na(r$passed))
  }
})


# --- anthropic epipedons -------------------------------------------------------------------

.silt <- function(bot = c(20, 40, 60), oc = c(0.8, 0.6, 0.4), sieve = "positive", art = 1, cc = c(8, 8, 8)) {
  n <- length(bot)
  data.frame(top_cm = c(0, bot[-n]), bottom_cm = bot, designation = c("Ap", "A2", "A3")[seq_len(n)],
             clay_pct = 18, silt_pct = 60, sand_pct = 22, munsell_hue_moist = "10YR",
             munsell_value_moist = 4, munsell_chroma_moist = 3, caco3_pct = cc, oc_pct = oc,
             silting_sieve_test = c(sieve, NA, NA)[seq_len(n)], artefacts_pct = c(art, NA, NA)[seq_len(n)])
}

test_that("siltigic epipedon: 50 cm homogeneous, carbon, sieve test, artefacts; evidence 20-50 cm", {
  skip_on_cran()
  h <- rbind(.silt(), data.frame(top_cm = 60, bottom_cm = 100, designation = "C", clay_pct = 5, silt_pct = 10,
                                 sand_pct = 85, munsell_hue_moist = "2.5Y", munsell_value_moist = 6,
                                 munsell_chroma_moist = 4, caco3_pct = 2, oc_pct = 0.1,
                                 silting_sieve_test = NA, artefacts_pct = NA))
  expect_equal(.cg(.cp(h), "siltigic_epipedon")$full, c(TRUE, TRUE, TRUE, FALSE))
  # 40 cm: evidence; 20 cm still evidence; 19 cm none
  h2 <- h; h2$bottom_cm[3] <- 40; h2$top_cm[3] <- 40; h2$bottom_cm[3] <- 40
  h2 <- h2[-3, ]; h2$top_cm[3] <- 40
  g <- .cg(.cp(h2), "siltigic_epipedon")
  expect_equal(c(g$full[1], g$evid[1]), c(FALSE, TRUE))
  # carbon not decreasing: fails
  h3 <- h; h3$oc_pct[1:3] <- c(0.5, 0.7, 0.4)
  expect_false(.cg(.cp(h3), "siltigic_epipedon")$full[1])
  # no artefacts recorded: absent (the WRB irragric may stand in)
  h4 <- h; h4$artefacts_pct <- NA
  r <- .cres(.cp(h4), "siltigic_epipedon_cst")
  expect_true(isFALSE(r$passed) || "irragric" %in% r$evidence$stand_in)
  # CaCO3 not measured: the homogeneity is undecided
  h5 <- h; h5$caco3_pct[2] <- NA
  expect_true(is.na(.cg(.cp(h5), "siltigic_epipedon")$full[1]))
})

test_that("cumulic epipedon: mud and earth kinds, evidence", {
  skip_on_cran()
  h <- .silt(cc = NA)
  h$silting_sieve_test <- NULL
  h$mottle_colors <- c("7.5YR 5/6", NA, NA)
  h <- rbind(h, data.frame(top_cm = 60, bottom_cm = 90, designation = "C", clay_pct = 40, silt_pct = 30,
                           sand_pct = 30, munsell_hue_moist = "5Y", munsell_value_moist = 6,
                           munsell_chroma_moist = 1, caco3_pct = NA, oc_pct = 0.1,
                           artefacts_pct = NA, mottle_colors = NA))
  g <- .cg(.cp(h), "cumulic_epipedon")
  expect_equal(g$full, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(g$r$kind[1], "mud-cumulic")
  h2 <- h; h2$mottle_colors <- NA; h2$foreign_fragments_pct <- c(5, NA, NA, NA)
  expect_equal(.cg(.cp(h2), "cumulic_epipedon")$r$kind[1], "earth-cumulic")
  h3 <- h[-3, ]; h3$top_cm[3] <- 40
  g <- .cg(.cp(h3), "cumulic_epipedon")
  expect_equal(c(g$full[1], g$evid[1]), c(FALSE, TRUE))
  h4 <- h; h4$oc_pct[1:3] <- c(0.4, 0.4, 0.4)
  expect_false(.cg(.cp(h4), "cumulic_epipedon")$evid[1])
  h5 <- h; h5$oc_pct[2] <- NA
  expect_true(is.na(.cg(.cp(h5), "cumulic_epipedon")$full[1]))
})

test_that("fimic epipedon: the three numbers and their evidence bands", {
  skip_on_cran()
  mk <- function(p = 50, oc = 1, bot = 30, worm = 60) {
    .cp(data.frame(top_cm = c(0, 20, bot), bottom_cm = c(20, bot, bot + 40), designation = c("Ap", "AB", "B"),
                   oc_pct = c(oc, oc, 0.3), p_olsen_mg_kg = c(p, p, 5), worm_holes_pct = c(worm, NA, NA),
                   artefacts_pct = c(2, NA, NA)))
  }
  g <- function(...) { r <- .cg(mk(...), "fimic_epipedon"); c(r$full[1], r$evid[1]) }
  expect_equal(g(), c(TRUE, TRUE))
  expect_equal(g(p = 35), c(TRUE, TRUE))
  expect_equal(g(p = 34.9), c(FALSE, TRUE))
  expect_equal(g(p = 18), c(FALSE, TRUE))
  expect_equal(g(p = 17.9), c(FALSE, FALSE))
  expect_equal(g(oc = 0.6), c(TRUE, TRUE))
  expect_equal(g(oc = 0.5), c(FALSE, TRUE))
  expect_equal(g(oc = 0.44), c(FALSE, FALSE))
  expect_equal(g(bot = 25), c(TRUE, TRUE))
  expect_equal(g(bot = 22), c(FALSE, TRUE))
  # two numbers in the lower band: none
  expect_equal(g(p = 25, oc = 0.5), c(FALSE, FALSE))
  # Olsen P not measured (Mehlich-3 is not read): undecided
  expect_true(is.na(g(p = NA)[1]))
})

.paddy <- function(bd2 = 1.4, pud = NA, site = list(flood_irrigated = TRUE), mot = "7.5YR 5/6",
                   bg_bot = 60, films = 0.6, fe = 2) {
  .cp(data.frame(top_cm = c(0, 15, 22, bg_bot), bottom_cm = c(15, 22, bg_bot, bg_bot + 40),
                 designation = c("Ap1", "Ap2", "Bg", "C"),
                 munsell_hue_moist = c("10YR", "10YR", "10YR", "2.5Y"), munsell_value_moist = c(4, 4, 5, 6),
                 munsell_chroma_moist = c(1, 2, 4, 2), mottle_colors = c(mot, NA, "7.5YR 5/8", NA),
                 bulk_density_g_cm3 = c(1.2, bd2, 1.45, 1.5), puddled = c(pud, NA, NA, NA),
                 fe_dcb_pct = c(1, 1, fe, 1), humus_clay_films_mm = c(NA, NA, films, NA),
                 structure_type = c("granular", "platy", "prismatic", "massive"),
                 structure_grade = c("weak", "weak", "moderate", NA)), site)
}

test_that("anthrostagnic epipedon: regime, puddling, colour, mottles, plough pan", {
  skip_on_cran()
  g <- .cg(.paddy(), "anthrostagnic_epipedon")
  expect_equal(g$full, c(TRUE, TRUE, FALSE, FALSE))
  expect_true("puddled_from_paddy_records" %in% g$r$flags)
  expect_true("colour_not_submerged" %in% g$r$flags)
  # the plough pan 1.10 times as dense: 1.32 / 1.2 passes, 1.31 does not (evidence)
  expect_true(.cg(.paddy(bd2 = 1.32), "anthrostagnic_epipedon")$full[1])
  g <- .cg(.paddy(bd2 = 1.31), "anthrostagnic_epipedon")
  expect_equal(c(g$full[1], g$evid[1]), c(FALSE, TRUE))
  # a short paddy history: evidence
  g <- .cg(.paddy(site = list(flood_irrigated = TRUE, paddy_years_frac = 0.4)), "anthrostagnic_epipedon")
  expect_equal(c(g$full[1], g$evid[1]), c(FALSE, TRUE))
  # not puddled: none
  expect_false(.cg(.paddy(pud = FALSE), "anthrostagnic_epipedon")$evid[1])
  # pan density not measured: the full grade undecided, the evidence holds
  g <- .cg(.paddy(bd2 = NA), "anthrostagnic_epipedon")
  expect_true(is.na(g$full[1])); expect_true(g$evid[1])
})

test_that("hydragric horizon under the anthrostagnic epipedon; evidence 5-20 cm", {
  skip_on_cran()
  g <- .cg(.paddy(), "hydragric_horizon")
  expect_equal(g$full, c(FALSE, FALSE, TRUE, FALSE))
  g <- .cg(.paddy(bg_bot = 35), "hydragric_horizon")
  expect_equal(c(g$full[3], g$evid[3]), c(FALSE, TRUE))
  # no redox features, DCB iron not measured: undecided
  p <- .paddy(fe = NA)
  h <- p$horizons; h$mottle_colors[3] <- NA; h$munsell_value_moist[3] <- 4
  g <- .cg(.cp(h, p$site), "hydragric_horizon")
  expect_true(is.na(g$full[3]))
  # iron 1.5 times the cultivated layer (no mottles): 1.5 passes, 1.4 fails
  h$fe_dcb_pct[3] <- 1.5
  expect_true(.cg(.cp(h, p$site), "hydragric_horizon")$full[3])
  h$fe_dcb_pct[3] <- 1.4
  expect_false(.cg(.cp(h, p$site), "hydragric_horizon")$full[3])
  # no anthrostagnic epipedon: no hydragric horizon
  expect_false(any(.cg(.paddy(pud = FALSE), "hydragric_horizon")$full))
})

test_that("agric horizon and its phos-agric kind; evidence 5-10 cm", {
  skip_on_cran()
  mk <- function(bot = 35, pct = 8, mm = 0.6, p = c(10, 10, 5)) {
    .cp(data.frame(top_cm = c(0, 20, bot), bottom_cm = c(20, bot, bot + 40), designation = c("Ap", "B", "C"),
                   humus_clay_films_pct = c(NA, pct, NA), humus_clay_films_mm = c(NA, mm, NA),
                   p_olsen_mg_kg = p, munsell_hue_moist = "10YR", munsell_value_moist = c(4, 4, 5),
                   munsell_chroma_moist = c(3, 3, 4), ph_h2o = 7, bs_pct = 90))
  }
  expect_equal(.cg(mk(), "agric_horizon")$full, c(FALSE, TRUE, FALSE))
  g <- .cg(mk(bot = 27), "agric_horizon")
  expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  g <- .cg(mk(pct = 4), "agric_horizon")
  expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  expect_true(is.na(.cg(mk(mm = NA), "agric_horizon")$full[2]))
  # phos-agric: Olsen P 18 or more and twice the layer below
  p <- mk(pct = 0, mm = 0, p = c(60, 40, 10))
  g <- .cg(p, "agric_horizon")
  expect_true(g$full[2]); expect_equal(g$r$kind[2], "phos-agric")
  expect_true(.cres(p, "phos_agric_horizon_cst")$passed)
  expect_false(.cres(mk(), "phos_agric_horizon_cst")$passed)
  expect_false(.cg(mk(pct = 0, mm = 0, p = c(60, 19, 10)), "agric_horizon")$full[2])
  # an A in a profile written with FAO suffixes is no cultivated layer; a
  # plain A among plain letters cannot be told: undecided (D1), but only
  # where the items under it do not fail (0.9.373, Hugo, 2026-11-06)
  h <- mk()$horizons; h$designation <- c("A", "Bw", "C")
  expect_false(any(.cg(.cp(h), "agric_horizon")$full))
  h$designation <- c("A", "B", "C")
  g <- .cg(.cp(h), "agric_horizon")$full
  expect_true(is.na(g[2])); expect_false(any(g %in% TRUE))
  h$humus_clay_films_pct <- 0; h$humus_clay_films_mm <- 0; h$p_olsen_mg_kg <- c(10, 10, 10)
  expect_false(any(.cg(.cp(h), "agric_horizon")$full))
})


# --- crustic epipedons ---------------------------------------------------------------------

test_that("aridic epipedon and salic crust", {
  skip_on_cran()
  mk <- function(pav = 30, salts = 0.1) {
    .cp(data.frame(top_cm = c(0, 5), bottom_cm = c(5, 40), designation = c("A", "Bw"),
                   desert_pavement_pct = c(pav, NA), structure_type = c("platy", "blocky"),
                   soluble_salts_pct = c(salts, 0.2), clay_pct = 15, sand_pct = 60, silt_pct = 25),
        list(surface_crust_thickness_cm = 1))
  }
  expect_equal(.cg(mk(), "aridic_epipedon")$full, c(TRUE, FALSE))
  expect_false(.cg(mk(pav = NA), "aridic_epipedon")$full[1])
  # salts not measured in a top layer thick enough for a salic horizon:
  # undecided (here the gypsum under the crust, item 4)
  h <- mk(salts = NA)$horizons; h$bottom_cm[1] <- 20; h$top_cm[2] <- 20; h$caso4_pct <- c(10, 0)
  expect_true(is.na(.cg(.cp(h, list(surface_crust_thickness_cm = 1)), "aridic_epipedon")$full[1]))
  # a salic horizon from the surface excludes it
  h <- mk(salts = 3)$horizons; h$bottom_cm[1] <- 20; h$top_cm[2] <- 20; h$soluble_salts_pct[2] <- 0.1
  expect_false(.cg(.cp(h, list(surface_crust_thickness_cm = 1, climatic_moisture_regime = "aridic")),
                   "aridic_epipedon")$full[1])
  sc <- function(thk, s) .cp(data.frame(top_cm = c(0, thk), bottom_cm = c(thk, 30), soluble_salts_pct = c(s, 1)))
  expect_equal(.cg(sc(2, 10), "salic_crust")$full, c(TRUE, FALSE))
  expect_false(.cg(sc(1.5, 10), "salic_crust")$full[1])
  expect_false(.cg(sc(3, 9.9), "salic_crust")$full[1])
  expect_true(is.na(.cg(sc(3, NA), "salic_crust")$full[1]))
})


# --- subsurface horizons -------------------------------------------------------------------

test_that("albic horizon", {
  skip_on_cran()
  mk <- function(vm = 6, vd = 7, c = 2, td = NA, share = NA) {
    .cp(data.frame(top_cm = c(0, 15, 30), bottom_cm = c(15, 30, 60), designation = c("A", "E", "Bt"),
                   munsell_value_moist = c(3, vm, 4), munsell_value_dry = c(5, vd, 6),
                   munsell_chroma_moist = c(2, c, 6), tongue_depth_cm = c(NA, td, NA),
                   albic_materials_pct = c(NA, share, NA)))
  }
  g <- .cg(mk(), "albic_horizon")
  expect_equal(g$full, c(FALSE, TRUE, FALSE))
  expect_true("albic_materials_from_colour" %in% g$r$flags)
  expect_false(.cg(mk(c = 4), "albic_horizon")$full[2])
  # tongues 5 cm deep recorded on the layer below: glossic, not albic; on
  # the E itself they are the E's own and do not remove the albic (R6)
  expect_true(.cg(mk(td = 5), "albic_horizon")$full[2])
  h <- mk()$horizons; h$tongue_depth_cm <- c(NA, NA, 6)
  expect_false(.cg(.cp(h), "albic_horizon")$full[2])
  expect_false(.cg(mk(share = 80), "albic_horizon")$full[2])
  expect_true(.cg(mk(share = 85), "albic_horizon")$full[2])
  # value moist 4 without a dry value: undecided by the CST colours
  r <- .cg(mk(vm = 4, vd = NA), "albic_horizon")
  expect_true(is.na(r$full[2]) || identical(r$r$stand_in[2], "albic"))
})

test_that("glossic horizon and its evidence (Table 3.1 widths)", {
  skip_on_cran()
  mk <- function(td = 8, vol = 30, w = 12, clay = 30, silt = 40) {
    .cp(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), clay_pct = c(15, clay), silt_pct = c(50, silt),
                   sand_pct = c(35, 100 - clay - silt), tongue_depth_cm = c(NA, td), tongue_vol_pct = c(NA, vol),
                   tongue_width_mm = c(NA, w)))
  }
  expect_equal(.cg(mk(), "glossic_horizon")$full, c(FALSE, TRUE))
  g <- .cg(mk(w = 8), "glossic_horizon")
  expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  g <- .cg(mk(vol = 10), "glossic_horizon")
  expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  expect_false(.cg(mk(w = 1.5, vol = 10), "glossic_horizon")$evid[2])
  # clayey: 5 mm is enough
  expect_true(.cg(mk(w = 5, clay = 50, silt = 20), "glossic_horizon")$full[2])
  expect_true(is.na(.cg(mk(vol = NA), "glossic_horizon")$full[2]))
  expect_false(.cg(mk(td = NA), "glossic_horizon")$evid[2])
})

test_that("cambic horizon", {
  skip_on_cran()
  mk <- function(thk = 30, st = "blocky", site = list(), sand = 30, fs = NA) {
    .cp(data.frame(top_cm = c(0, 20, 20 + thk), bottom_cm = c(20, 20 + thk, 100), designation = c("A", "Bw", "C"),
                   clay_pct = c(18, 20, 15), sand_pct = c(sand, sand, sand), silt_pct = 100 - c(18, 20, 15) - sand,
                   fine_sand_pct = c(NA, fs, NA),
                   munsell_hue_moist = c("10YR", "7.5YR", "10YR"), munsell_chroma_moist = c(2, 6, 3),
                   munsell_value_moist = c(3, 5, 5), structure_type = c("granular", st, "massive"),
                   ph_h2o = 6.5, oc_pct = c(1.5, 0.5, 0.2), cec_cmol = 20, fe_dcb_pct = 0.5, fe_total_pct = 5,
                   structure_grade = c("moderate", if (is.na(st)) NA else "moderate", "structureless")), site)
  }
  expect_equal(.cg(mk(), "cambic_horizon")$full, c(FALSE, TRUE, FALSE))
  temperate <- list(soil_moisture_regime = "udic", soil_temperature_regime = "mesic")
  expect_false(.cg(mk(thk = 8, site = temperate), "cambic_horizon")$full[2])
  expect_true(.cg(mk(thk = 8, site = list(soil_moisture_regime = "aridic")), "cambic_horizon")$full[2])
  expect_true(.cg(mk(thk = 5, site = list(soil_temperature_regime = "frigid")), "cambic_horizon")$full[2])
  expect_false(.cg(mk(thk = 4, site = list(soil_moisture_regime = "aridic")), "cambic_horizon")$full[2])
  # regimes not recorded: 5-10 cm undecided
  expect_true(is.na(.cg(mk(thk = 8), "cambic_horizon")$full[2]))
  # a sand with little fine sand: not very fine sand or finer
  h <- mk()$horizons; h$clay_pct <- 5; h$sand_pct <- 90; h$silt_pct <- 5; h$fine_sand_pct <- c(NA, 30, NA)
  expect_false(.cg(.cp(h), "cambic_horizon")$full[2])
  # its colour against the C decides even with no structure recorded
  expect_true(.cg(mk(st = NA), "cambic_horizon")$full[2])
})

test_that("ferralic and LAC-ferric horizons", {
  skip_on_cran()
  mk <- function(cec = 5, k = 0.3, hue = "2.5YR", wm = NA, al = 0.5) {
    .cp(data.frame(top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 120), designation = c("A", "Bo", "C"),
                   clay_pct = c(40, 50, 50), sand_pct = 25, silt_pct = c(35, 25, 25),
                   cec_cmol = c(8, cec, cec), ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.1, na_cmol = 0.05,
                   al_kcl_cmol = al, k_total_pct = c(NA, k, NA), weatherable_minerals_pct = c(NA, wm, NA),
                   munsell_hue_moist = hue, fe_dcb_pct = 0.5, fe_total_pct = 10))
  }
  expect_equal(.cg(mk(), "ferralic_horizon")$full, c(FALSE, TRUE, FALSE))
  expect_false(.cg(mk(), "lac_ferric_horizon")$full[2])
  # CEC 16 cmol per kg clay is not under 16
  expect_true(.cg(mk(cec = 7.95), "ferralic_horizon")$full[2])
  expect_false(.cg(mk(cec = 8), "ferralic_horizon")$full[2])
  # weatherables 20-200 um stand in for total K, flagged
  g <- .cg(mk(k = NA, wm = 5), "ferralic_horizon")
  expect_true(g$full[2]); expect_true("weatherables_20_200um_for_50_200um" %in% g$r$flags)
  # LAC-ferric: CEC under 24 per kg clay, iron, not ferralic
  expect_true(.cg(mk(cec = 10), "lac_ferric_horizon")$full[2])
  expect_false(.cg(mk(cec = 12), "lac_ferric_horizon")$full[2])
  expect_false(.cg(mk(cec = 10, hue = "10YR"), "lac_ferric_horizon")$full[2])
  expect_true(is.na(.cg(mk(cec = NA), "lac_ferric_horizon")$full[2]))
})

test_that("plinthic horizon at 10 % plinthite, the WRB at 15 %", {
  skip_on_cran()
  mk <- function(pl, thk = 20, hard = NA) {
    .cp(data.frame(top_cm = c(0, 20, 20 + thk), bottom_cm = c(20, 20 + thk, 100), designation = c("A", "Bv", "C"),
                   plinthite_pct = c(0, pl, 0), plinthite_hardens = c(NA, hard, NA)))
  }
  p <- mk(12)
  g <- .cg(p, "plinthic_horizon")
  expect_equal(g$full, c(FALSE, TRUE, FALSE))
  # the hardening not recorded: absent, the WRB plinthic at 10 % stands in (D12)
  expect_true("stand_in:plinthic" %in% g$r$flags)
  expect_equal(g$r$stand_in[2], "plinthic")
  expect_false(isTRUE(soilKey:::plinthic(p)$passed))
  expect_true(.cres(p, "plinthic_horizon_cst")$passed)
  g <- .cg(mk(7), "plinthic_horizon"); expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  g <- .cg(mk(12, thk = 5), "plinthic_horizon"); expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  expect_false(.cg(mk(12, thk = 4), "plinthic_horizon")$evid[2])
  expect_false(.cg(mk(4.9), "plinthic_horizon")$evid[2])
  expect_false(.cg(mk(12, hard = FALSE), "plinthic_horizon")$full[2])
  expect_true(.cg(mk(12, hard = TRUE), "plinthic_horizon")$full[2])
  expect_true(is.na(.cg(mk(12, hard = TRUE), "plinthic_horizon")$r$stand_in[2]))
  h <- mk(12, hard = TRUE)$horizons; h$bottom_cm[2] <- NA
  expect_true(is.na(.cg(.cp(h), "plinthic_horizon")$full[2]))
})

test_that("spodic horizon and its evidence", {
  skip_on_cran()
  mk <- function(ph = 4.5, share = NA, al = 0.4) {
    .cp(data.frame(top_cm = c(0, 10, 20, 40), bottom_cm = c(10, 20, 40, 80), designation = c("A", "E", "Bhs", "C"),
                   ph_h2o = c(4.5, 4.6, ph, 5), oc_pct = c(3, 0.5, 1.2, 0.2),
                   munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"), munsell_value_moist = c(2, 6, 3, 5),
                   munsell_chroma_moist = c(1, 2, 4, 4), spodic_materials_pct = c(NA, NA, share, NA),
                   al_ox_pct = c(NA, NA, al, NA), fe_ox_pct = c(NA, NA, 0.4, NA)))
  }
  g <- .cg(mk(), "spodic_horizon")
  expect_equal(g$full, c(FALSE, FALSE, TRUE, FALSE))
  expect_true(all(c("ph_ratio_substitute", "spodic_materials_from_layer") %in% g$r$flags))
  g <- .cg(mk(share = 60), "spodic_horizon"); expect_equal(c(g$full[3], g$evid[3]), c(FALSE, TRUE))
  expect_true(.cg(mk(share = 85), "spodic_horizon")$full[3])
  expect_false(.cg(mk(share = 49), "spodic_horizon")$evid[3])
  expect_false(.cg(mk(ph = 6.0), "spodic_horizon")$full[3])
  expect_true(.cg(mk(ph = 5.9), "spodic_horizon")$full[3])
  r <- .cg(mk(ph = NA), "spodic_horizon")
  expect_true(is.na(r$full[3]) || identical(r$r$stand_in[3], "spodic"))
})


# --- the argic horizon: the CST breakpoints against the WRB 2022 ones ----------------------------

.arg <- function(e, b, fine_e = NA, fine_b = NA, bthk = 30, na = NA, cec = 20, ph = 6, films = NA,
                 desg = c("A", "B", "C")) {
  .cp(data.frame(top_cm = c(0, 20, 20 + bthk), bottom_cm = c(20, 20 + bthk, 120), designation = desg,
                 oc_pct = c(1, 0.3, 0.2), munsell_hue_moist = "10YR", munsell_chroma_moist = c(2, 4, 4),
                 clay_pct = c(e, b, b), silt_pct = 30, sand_pct = c(70 - e, 70 - b, 70 - b),
                 fine_clay_pct = c(fine_e, fine_b, NA), na_cmol = c(NA, na, NA), cec_cmol = c(NA, cec, NA),
                 ph_h2o = ph, clay_films_amount = c(NA, if (is.na(films)) NA else "common", NA),
                 clay_films_strength = c(NA, films, NA)))
}

test_that("argic horizon: the clay increase of p. 36, at each breakpoint", {
  skip_on_cran()
  a <- function(...) .cg(.arg(...), "argic_horizon")$full[2]
  # under 15 %: 3 points
  expect_true(a(12, 15)); expect_false(a(12, 14.9))
  expect_true(a(14.9, 17.9)); expect_false(a(14.9, 17.8))
  # 15-40 %: 1.2 times
  expect_true(a(15, 18)); expect_false(a(15, 17.9))
  expect_true(a(30, 36)); expect_false(a(30, 35.9))
  # 40-60 %: 8 points
  expect_true(a(40, 48)); expect_false(a(40, 47.9))
  expect_true(a(45, 53))
  # 60 % or more: 8 points of fine clay
  expect_true(a(62, 66, fine_e = 20, fine_b = 28))
  expect_false(a(62, 75, fine_e = 20, fine_b = 27))
  expect_true(is.na(a(62, 75)))
  # the C is not an argic horizon even with more clay
  expect_false(.cg(.arg(12, 20), "argic_horizon")$full[3])
})

test_that("argic horizon: the WRB 2022 argic needs more (fixtures of the differences)", {
  skip_on_cran()
  for (pair in list(c(12, 15), c(30, 36), c(45, 53))) {
    p <- .arg(pair[1], pair[2], desg = c(NA, NA, NA))
    expect_true(.cres(p, "argic_horizon_cst")$passed)
    expect_false(isTRUE(soilKey:::argic(p)$passed))
  }
})

test_that("argic horizon: thickness, films, sodic property and the natro-argic horizon", {
  skip_on_cran()
  # 7.5 cm (loamy) and a tenth of the layers above
  expect_true(.cg(.arg(12, 20, bthk = 7.5), "argic_horizon")$full[2])
  expect_false(.cg(.arg(12, 20, bthk = 7), "argic_horizon")$full[2])
  # clay films thicker than 0.5 mm on 5 % of the faces, with no clay increase
  g <- .cg(.arg(20, 21, films = "distinct"), "argic_horizon")
  expect_true(g$full[2]); expect_true("clay_films_strength_for_thickness" %in% g$r$flags)
  expect_false(.cg(.arg(20, 21, films = "faint"), "argic_horizon")$full[2])
  # the sodic property: a natro-argic horizon, argic only with natro = TRUE
  p <- .arg(12, 20, na = 8, cec = 20)
  expect_false(.cres(p, "argic_horizon_cst")$passed)
  r <- .cres(p, "argic_horizon_cst", natro = TRUE)
  expect_true(r$passed); expect_equal(r$evidence$kind, "natro-argic")
  # sodium not measured in an alkaline layer: undecided
  expect_true(is.na(.cg(.arg(12, 20, ph = 8.8), "argic_horizon")$full[2]) ||
                !is.na(.cg(.arg(12, 20, ph = 8.8), "argic_horizon")$r$stand_in[2]))
  # a recorded stratification: a sedimentary clay layer
  h <- .arg(12, 20)$horizons; h$stratification_pattern <- c(NA, "continuous", NA)
  expect_false(.cg(.cp(h), "argic_horizon")$full[2])
  # clay not measured: undecided
  expect_true(is.na(.cg(.arg(12, NA, desg = c(NA, NA, NA)), "argic_horizon")$full[2]))
})

test_that("clay-pan: firm prisms, twice the albic clay, the thin section met unless contradicted", {
  skip_on_cran()
  mk <- function(b = 35, mf = NA, st = "prismatic") {
    .cp(data.frame(top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 60), designation = c("A", "E", "Btn"),
                   clay_pct = c(20, 12, b), silt_pct = 40, sand_pct = c(40, 48, 60 - b),
                   munsell_value_moist = c(3, 6, 4), munsell_value_dry = c(5, 7, 6), munsell_chroma_moist = c(2, 2, 4),
                   structure_type = c("granular", "platy", st), consistence_moist = c(NA, NA, "firm"),
                   micromorph_features = c(NA, NA, mf)))
  }
  g <- .cg(mk(), "claypan")
  expect_equal(g$full, c(FALSE, FALSE, TRUE))
  expect_true("thin_section_assumed" %in% g$r$flags)
  expect_true(.cg(mk(b = 24), "claypan")$full[3])
  expect_false(.cg(mk(b = 23.9), "claypan")$full[3])
  expect_false(.cg(mk(mf = "pseudomorphs"), "claypan")$full[3])
  expect_true(.cg(mk(mf = "clay_stripes"), "claypan")$full[3])
  expect_true(is.na(.cg(mk(b = NA), "claypan")$full[3]))
})

test_that("alkalic horizon: ESP 30 and pH 9.0 (the WRB natric takes ESP 15)", {
  skip_on_cran()
  mk <- function(esp = 35, ph = 9.2, salts = 0.2, st = "columnar", esp_c = 20) {
    .cp(data.frame(top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 90), designation = c("A", "Btn", "C"),
                   clay_pct = c(15, 30, 25), silt_pct = 40, sand_pct = c(45, 30, 35),
                   esp_pct = c(5, esp, esp_c), ph_h2o = c(8.5, ph, 9), soluble_salts_pct = c(salts, 0.5, 0.6),
                   structure_type = c("platy", st, "massive")))
  }
  g <- .cg(mk(), "alkalic_horizon")
  expect_equal(g$full, c(FALSE, TRUE, FALSE))
  expect_false(.cg(mk(), "argic_horizon")$full[2])
  # ESP 29: evidence only; 5: evidence; 4.9: none
  g <- .cg(mk(esp = 29), "alkalic_horizon"); expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  # v0.9.376 (Hugo on the review, 2026-11-08): the evidence on its printed
  # items, the alkalic structure or an ESP of 5 or more within 40 cm
  expect_false(.cg(mk(esp = 4.9, st = "blocky", esp_c = 4), "alkalic_horizon")$evid[2])
  expect_true(.cg(mk(esp = 4.9, esp_c = 4), "alkalic_horizon")$evid[2])        # the structure
  expect_true(.cg(mk(esp = 4.9, st = "blocky"), "alkalic_horizon")$evid[2])    # ESP 20 within 40 cm
  # pH 8.9: not alkalic
  expect_false(.cg(mk(ph = 8.9), "alkalic_horizon")$full[2])
  # salts 5 g/kg in the surface layer: not alkalic
  expect_false(.cg(mk(salts = 0.5), "alkalic_horizon")$full[2])
  expect_true(is.na(.cg(mk(salts = NA), "alkalic_horizon")$full[2]))
  # the WRB natric horizon at its own ESP 15 takes an ESP of 20
  p <- mk(esp = 20)
  expect_false(.cres(p, "alkalic_horizon_cst")$passed)
  expect_true(.cres(p, "alkalic_horizon_cst", grade = "evidence")$passed)
})


# --- salts, gypsum and carbonates ---------------------------------------------------------------

.one <- function(thk = 20, top = 20, ..., below = list(), surface = list()) {
  # three layers: the layer tested between neutral layers (0, FALSE, "none")
  cols <- list(...)
  h <- data.frame(top_cm = c(0, top, top + thk), bottom_cm = c(top, top + thk, top + thk + 40))
  neutral <- function(v) if (is.numeric(v)) 0 else if (is.logical(v)) FALSE else "none"
  for (k in union(names(cols), names(below))) {
    s <- if (!is.null(surface[[k]])) surface[[k]] else if (!is.null(cols[[k]])) neutral(cols[[k]]) else NA
    b <- if (!is.null(below[[k]])) below[[k]] else if (!is.null(cols[[k]])) neutral(cols[[k]]) else NA
    h[[k]] <- c(s, if (is.null(cols[[k]])) NA else cols[[k]], b)
  }
  h
}

test_that("hypersalic horizon and salipan", {
  skip_on_cran()
  hs <- function(s, thk = 20) .cg(.cp(.one(thk, soluble_salts_pct = s)), "hypersalic_horizon")$full[2]
  expect_true(hs(55)); expect_true(hs(50)); expect_false(hs(49.9)); expect_false(hs(55, 14))
  expect_true(is.na(hs(NA)))
  sp <- function(s, cem = "strongly", thk = 6) .cg(.cp(.one(thk, soluble_salts_pct = s, cementation_class = cem)), "salipan")$full[2]
  expect_true(sp(25)); expect_false(sp(25, "none")); expect_false(sp(19)); expect_false(sp(25, thk = 4))
  expect_true(is.na(sp(NA)))
  expect_false(.cg(.cp(.one(20, soluble_salts_pct = 55, cementation_class = "strongly")), "hypersalic_horizon")$full[2])
})

test_that("gypsic and hypergypsic horizons; the gypsic evidence", {
  skip_on_cran()
  gy <- function(g, thk = 20, vis = 2, b = 0.5) {
    .cg(.cp(.one(thk, caso4_pct = g, gypsum_visible_pct = vis, below = list(caso4_pct = b, gypsum_visible_pct = 0))),
        "gypsic_horizon")
  }
  expect_true(gy(10)$full[2])
  # thickness x gypsum 1,500: 15 cm at 10 % passes, at 9.9 % not (evidence)
  expect_true(gy(10, thk = 15)$full[2])
  # 9.9 % over 15 cm: 1,485 < 1,500, and above the evidence's 49 g/kg (R8)
  r <- gy(9.9, thk = 15); expect_equal(c(r$full[2], r$evid[2]), c(FALSE, FALSE))
  r <- gy(4.9); expect_equal(c(r$full[2], r$evid[2]), c(FALSE, TRUE))
  expect_false(gy(4.95)$evid[2])
  r <- gy(3); expect_equal(c(r$full[2], r$evid[2]), c(FALSE, TRUE))
  expect_false(gy(3, b = 4)$evid[2])
  expect_false(gy(0.9)$evid[2])
  expect_false(gy(55)$full[2])
  expect_true(is.na(gy(NA)$full[2]))
  hg <- function(g) .cg(.cp(.one(20, caso4_pct = g)), "hypergypsic_horizon")$full[2]
  expect_true(hg(50)); expect_false(hg(49)); expect_true(is.na(hg(NA)))
})

test_that("calcic, hypercalcic horizons and calcipan", {
  skip_on_cran()
  ca <- function(cc, thk = 20, b = 10, sec = NA) {
    .cg(.cp(.one(thk, caco3_pct = cc, secondary_carbonates_pct = sec, clay_pct = 25, sand_pct = 40, silt_pct = 35,
                 below = list(caco3_pct = b, secondary_carbonates_pct = 0, clay_pct = 25, sand_pct = 40, silt_pct = 35),
                 surface = list(caco3_pct = cc, clay_pct = 25, sand_pct = 40, silt_pct = 35))),
        "calcic_horizon")
  }
  # (3)a: 15-50 % and 5 points more than a neighbour
  expect_true(ca(25)$full[2]); expect_true(ca(15)$full[2]); expect_false(ca(14.9, b = 2)$full[2])
  expect_false(ca(25, b = 20.1)$full[2])
  # (3)b: 5 % secondary carbonates by volume
  expect_true(ca(25, b = 24, sec = 5)$full[2])
  # evidence: 5-14 cm; 2-5 points more
  r <- ca(25, thk = 10); expect_equal(c(r$full[2], r$evid[2]), c(FALSE, TRUE))
  r <- ca(25, b = 22); expect_equal(c(r$full[2], r$evid[2]), c(FALSE, TRUE))
  expect_false(ca(25, b = 23.5)$evid[2])
  expect_true(is.na(ca(NA)$full[2]))
  hc <- function(cc, sec, b = 20) {
    .cg(.cp(.one(20, caco3_pct = cc, secondary_carbonates_pct = sec,
                 below = list(caco3_pct = b, secondary_carbonates_pct = 0))), "hypercalcic_horizon")$full[2]
  }
  expect_true(hc(60, 60)); expect_false(hc(60, 50)); expect_false(hc(45, 60)); expect_true(is.na(hc(NA, 60)))
  cp <- function(cc, thk = 10, cem = "indurated", rock = FALSE) {
    h <- .one(thk, caco3_pct = cc, cementation_class = cem)
    if (rock) h$designation <- c("A", "Bkm", "R")
    .cg(.cp(h), "calcipan")$full[2]
  }
  expect_true(cp(40)); expect_false(cp(19)); expect_true(cp(20))
  expect_false(cp(40, thk = 8)); expect_true(cp(60, thk = 4, rock = TRUE)); expect_false(cp(40, thk = 4, rock = TRUE))
  expect_true(is.na(cp(NA)))
})

test_that("phosphipan", {
  skip_on_cran()
  pp <- function(top = 15, p = 4, cc = 55, thk = 15) {
    .cg(.cp(.one(thk, top, p_total_pct = p, caco3_pct = cc, cementation_class = "strongly")), "phosphipan")$full[2]
  }
  expect_true(pp()); expect_false(pp(top = 5)); expect_false(pp(p = 2.9)); expect_false(pp(thk = 9))
  expect_true(is.na(pp(p = NA)))
})

test_that("salic horizon: arid regions, the 1:1 EC, products; the WRB salic differs", {
  skip_on_cran()
  sa <- function(s = NA, thk = 30, site = list(), ec = NA, ece = NA) {
    .cg(.cp(.one(thk, soluble_salts_pct = s, ec_1to1_dS_m = ec, ec_dS_m = ece), site), "salic_horizon")
  }
  arid <- list(climatic_moisture_regime = "aridic"); humid <- list(climatic_moisture_regime = "udic")
  expect_true(sa(2.5, site = arid)$full[2])
  # product 600: 2 % x 30 cm = 600 passes, 25 cm (500) not
  expect_true(sa(2, site = arid)$full[2])
  expect_false(sa(2, 25, site = arid)$full[2])
  expect_false(sa(1.5, 40, site = arid)$full[2])
  expect_true(sa(1.5, 40, site = humid)$full[2])
  # region unknown: above 20 g/kg passes, under 10 fails, between undecided
  expect_true(sa(2.5)$full[2]); expect_false(sa(0.9)$full[2]); expect_true(is.na(sa(1.5, 40)$full[2]))
  # 1:1 EC: 30 / 15 dS/m, product 900
  expect_true(sa(ec = 20, thk = 50, site = humid)$full[2])
  expect_false(sa(ec = 20, thk = 40, site = humid)$full[2])
  expect_false(sa(ec = 20, thk = 50, site = arid)$full[2])
  # evidence: 5 g/kg (arid), 2 g/kg (other regions)
  r <- sa(0.5, site = arid); expect_equal(c(r$full[2], r$evid[2]), c(FALSE, TRUE))
  expect_false(sa(0.4, site = arid)$evid[2])
  expect_true(sa(0.2, site = humid)$evid[2])
  # the saturated-paste EC is not read for the 1:1 EC (Q13); the WRB salic reads it
  p <- .cp(.one(30, ec_dS_m = 40))
  expect_true(is.na(.cres(p, "salic_horizon_cst")$passed))
  expect_true(isTRUE(soilKey:::salic(p)$passed))
})

test_that("sulfuric horizon", {
  skip_on_cran()
  su <- function(ph = 3.5, jar = TRUE, sulf = 0.05, sid = 0.5, mot = NA) {
    .cg(.cp(.one(20, ph_h2o = ph, jarosite_present = jar, sulfate_s_pct = sulf, mottle_colors = mot,
                 below = list(sulfidic_s_pct = sid, ph_h2o = 6.5))), "sulfuric_horizon")$full[2]
  }
  expect_true(su()); expect_false(su(ph = 4.0)); expect_false(su(sid = 0.005))
  expect_true(su(sulf = 0.0167))
  expect_true(is.na(su(sulf = 0.016)))
  h <- .one(20, ph_h2o = 3.5, jarosite_present = TRUE, sulfate_s_pct = 0.016, s_total_pct = 0.29,
            below = list(sulfidic_s_pct = 0.5, ph_h2o = 6.5))
  expect_false(.cg(.cp(h), "sulfuric_horizon")$full[2])
  expect_true(su(jar = NA, mot = "2.5Y 6/6"))
  expect_true(is.na(su(sulf = NA)))
})


# --- the horizon panel -------------------------------------------------------------------------

test_that("the horizon panel gives each layer its horizons, evidences and stand-ins", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 45, 90), bottom_cm = c(20, 45, 90, 130),
                  designation = c("Ap", "E", "Bt", "C"), clay_pct = c(12, 11, 30, 25), silt_pct = c(40, 40, 30, 35),
                  sand_pct = c(48, 49, 40, 40), oc_pct = c(1.5, 0.4, 0.3, 0.1), bs_pct = c(60, 50, 70, 80),
                  munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 4, 5),
                  munsell_chroma_moist = c(2, 2, 6, 4), munsell_value_dry = c(5, 7, 6, 7),
                  munsell_chroma_dry = c(2, 2, 6, 4), plinthite_pct = c(0, 0, 7, 0),
                  structure_type = c("granular", "platy", "blocky", "massive"))
  p <- .cp(h)
  fr <- soilKey:::.cst_identify(p)
  expect_equal(nrow(fr), 4L)
  expect_equal(fr$code[1:3], c("Ochric epipedon", "Albic horizon", "Argic horizon"))
  expect_true("Plinthic horizon evidence" %in% fr$features[[3]])
  expect_true(all(fr$route == "data"))
  expect_true(all(c("diagnostics", "evidences", "undecided", "kinds", "stand_ins", "lower_grade") %in% names(fr)))
  expect_true(is.na(fr$passed[4]))
  expect_true(all(c("Calcic horizon", "Salic horizon") %in% fr$undecided[[4]]))
  expect_true("Argic horizon: illuvial" %in% fr$kinds[[3]])
  # a layer read by a stand-in is marked
  h2 <- h; h2$clay_pct[3] <- NA; h2$designation[3] <- "Bt"
  fr2 <- soilKey:::.cst_identify(.cp(h2))
  if (length(fr2$stand_ins[[3]])) expect_true(fr2$lower_grade[3])
  # the registry entry (0.9.373): identify_horizons() reaches the panel
  expect_identical(identify_horizons(p, "cst2001")$code[1:3], fr$code[1:3])
})

test_that("the horizons leave the other systems unchanged", {
  skip_on_cran()
  p <- make_cssc3_example(eval(formals(make_cssc3_example)[[1]])[1])
  expect_no_error(soilKey:::.cst_identify(p))
})

test_that("stand-ins: flagged, lower grade, only where a descriptor is missing", {
  skip_on_cran()
  # visible gypsum not recorded: the WRB gypsic horizon stands in
  h <- data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80), caso4_pct = c(0, 10, 0),
                  gypsum_visible_pct = c(0, NA, 0))
  p <- .cp(h)
  r <- .cres(p, "gypsic_horizon_cst")
  expect_true(r$passed)
  expect_equal(r$evidence$stand_in, "gypsic")
  expect_true(r$evidence$lower_grade)
  expect_match(r$notes, "stand_in:gypsic")
  fr <- soilKey:::.cst_identify(p)
  expect_equal(fr$stand_ins[[2]], "gypsic"); expect_true(fr$lower_grade[2])
  # recorded and under 1 %: no stand-in, it fails
  h$gypsum_visible_pct[2] <- 0.5
  r <- .cres(.cp(h), "gypsic_horizon_cst")
  expect_false(r$passed); expect_length(r$evidence$stand_in, 0L)
  # weatherable minerals and total K not measured: the WRB ferralic stands in
  h <- data.frame(top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 120), designation = c("A", "Bo", "C"),
                  clay_pct = 50, sand_pct = 25, silt_pct = 25, cec_cmol = 5, ca_cmol = 1, mg_cmol = 0.5,
                  k_cmol = 0.1, na_cmol = 0.05, al_kcl_cmol = 0.5)
  r <- .cres(.cp(h), "ferralic_horizon_cst")
  expect_true(is.na(.cg(.cp(h), "ferralic_horizon")$r$full[2]) || r$evidence$lower_grade)
  if (isTRUE(soilKey:::ferralic(.cp(h))$passed)) expect_equal(r$evidence$stand_in, "ferralic")
  # the CST passing on its own is never flagged
  h$k_total_pct <- 0.3
  r <- .cres(.cp(h), "ferralic_horizon_cst")
  expect_true(r$passed); expect_false(r$evidence$lower_grade)
  # an umbric epipedon with base saturation not measured: undecided or a flagged stand-in
  r <- .cres(.cp(.humic(bs = NA)), "umbric_epipedon_cst")
  expect_true(is.na(r$passed) || r$evidence$lower_grade)
})

test_that("more evidence band edges: siltigic 20 cm, agric 5 cm, hydragric 20 cm", {
  skip_on_cran()
  base <- .silt()
  sil <- function(bot2) {
    h <- base[1:2, ]; h$bottom_cm[2] <- bot2
    h <- rbind(h, data.frame(top_cm = bot2, bottom_cm = 100, designation = "C", clay_pct = 5, silt_pct = 10,
                             sand_pct = 85, munsell_hue_moist = "2.5Y", munsell_value_moist = 6,
                             munsell_chroma_moist = 4, caco3_pct = 2, oc_pct = 0.1,
                             silting_sieve_test = NA, artefacts_pct = NA))
    h$oc_pct[2] <- 0.5
    .cg(.cp(h), "siltigic_epipedon")$evid[1]
  }
  expect_true(sil(20.5)); expect_true(sil(40))
  ag <- function(bot) {
    .cg(.cp(data.frame(top_cm = c(0, 20, bot), bottom_cm = c(20, bot, bot + 40), designation = c("Ap", "B", "C"),
                       humus_clay_films_pct = c(NA, 8, NA), humus_clay_films_mm = c(NA, 0.6, NA),
                       munsell_hue_moist = "10YR", munsell_value_moist = 4, munsell_chroma_moist = 3,
                       ph_h2o = 7, bs_pct = 90, p_olsen_mg_kg = 10)), "agric_horizon")
  }
  g <- ag(25); expect_equal(c(g$full[2], g$evid[2]), c(FALSE, TRUE))
  g <- ag(24.9); expect_equal(c(g$full[2], g$evid[2]), c(FALSE, FALSE))
  g <- ag(30); expect_equal(c(g$full[2], g$evid[2]), c(TRUE, TRUE))
  g <- .cg(.paddy(bg_bot = 42), "hydragric_horizon"); expect_equal(c(g$full[3], g$evid[3]), c(TRUE, TRUE))
  g <- .cg(.paddy(bg_bot = 41), "hydragric_horizon"); expect_equal(c(g$full[3], g$evid[3]), c(FALSE, TRUE))
  g <- .cg(.paddy(bg_bot = 26.9), "hydragric_horizon"); expect_equal(c(g$full[3], g$evid[3]), c(FALSE, FALSE))
})

test_that("alkalic horizon against the WRB natric horizon (ESP 15)", {
  skip_on_cran()
  p <- .cp(data.frame(top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 90), designation = c("A", "Btn", "C"),
                      clay_pct = c(15, 30, 25), silt_pct = 40, sand_pct = c(45, 30, 35),
                      esp_pct = c(5, 20, 20), ph_h2o = c(8.5, 9.2, 9), soluble_salts_pct = c(0.2, 0.5, 0.6),
                      structure_type = c("platy", "columnar", "massive")))
  expect_true(soilKey:::natric_horizon(p)$passed)
  expect_false(.cres(p, "alkalic_horizon_cst")$passed)
  expect_equal(soilKey:::.cst_grade(.cres(p, "alkalic_horizon_cst")), "evidence")
})


# --- the repros of the review (2026-11-06) -----------------------------------------------------

.p1 <- function(desg = c("A", "P", "W", "G"), hue1 = "2.5Y", films = 0.6, st3 = "prismatic") {
  .cp(data.frame(top_cm = c(0, 15, 25, 60), bottom_cm = c(15, 25, 60, 100), designation = desg,
                 munsell_hue_moist = c(hue1, "2.5Y", "10YR", "5GY"), munsell_value_moist = c(4, 4, 5, 5),
                 munsell_chroma_moist = c(if (hue1 == "N") 0 else 1, 2, 3, 1),
                 mottle_colors = c("7.5YR 5/6", "7.5YR 5/6", "7.5YR 5/8", NA),
                 bulk_density_g_cm3 = c(1.1, 1.4, 1.35, 1.3), puddled = c(TRUE, NA, NA, NA),
                 fe_dcb_pct = c(1, 1, 2, 0.5), humus_clay_films_mm = c(NA, NA, films, NA),
                 structure_type = c("granular", "platy", st3, NA), structure_grade = c("moderate", "moderate", "strong", NA),
                 clay_pct = c(25, 26, 28, 30), sand_pct = 30),
      list(soil_moisture_regime = "anthrostagnic", land_use = "paddy rice"))
}

test_that("D1: the older paddy notation A-P-W-G reads as Ap1-Ap2-Br-Bg", {
  skip_on_cran()
  old <- .p1(); fao <- .p1(c("Ap1", "Ap2", "Br", "Bg"))
  for (id in c("anthrostagnic_epipedon", "agric_horizon", "hydragric_horizon")) {
    expect_identical(.cg(old, id)$full, .cg(fao, id)$full)
    expect_identical(.cg(old, id)$evid, .cg(fao, id)$evid)
  }
  expect_equal(.cg(old, "anthrostagnic_epipedon")$full, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(.cg(old, "hydragric_horizon")$full, c(FALSE, FALSE, TRUE, FALSE))
  expect_equal(soilKey:::.cst_cultivated(soilKey:::.cst_ctx(old)), c(TRUE, TRUE, FALSE, FALSE))
  # no designations: undecided, never false
  h <- old$horizons; h$designation <- NA
  expect_true(all(is.na(soilKey:::.cst_cultivated(soilKey:::.cst_ctx(.cp(h))))))
  expect_true(all(is.na(.cg(.cp(h, old$site), "anthrostagnic_epipedon")$full)))
})

test_that("D2: the eluvial layer of item 2b lies within 30 cm above", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 40, 60, 80, 100), bottom_cm = c(20, 40, 60, 80, 100, 130),
                  designation = c("A", "B1", "B2", "B3", "B4", "B5"),
                  clay_pct = c(10, 11, 12, 12.5, 13, 13.5), sand_pct = 40)
  g <- .cg(.cp(h), "argic_horizon")
  expect_false(any(g$full %in% TRUE))
  expect_false(g$full[5])
})

test_that("D3, D5: ESP measured with Mg not recorded is no sodic property; no stand-in decides it", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), designation = c("A", "Bt", "C"),
                  clay_pct = c(10, 25, 20), sand_pct = 50, ph_h2o = 6.5, esp_pct = 1,
                  clay_films_amount = c(NA, "common", NA), clay_films_thickness_mm = c(NA, 1, NA))
  g <- .cg(.cp(h), "argic_horizon")
  expect_equal(g$full, c(FALSE, TRUE, FALSE)); expect_true(is.na(g$r$stand_in[2]))
  h$esp_pct <- NA
  expect_equal(.cg(.cp(h), "argic_horizon")$full, c(FALSE, TRUE, FALSE))
  # alkaline with no sodium: undecided, and the WRB argic does not settle item 2e
  h$ph_h2o <- 8.8
  g <- .cg(.cp(h), "argic_horizon")
  expect_true(is.na(g$full[2])); expect_true(is.na(g$r$stand_in[2]))
  # probes3: two Bt layers with films and ESP 1, no Mg: the CST decides alone
  h <- data.frame(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 120), designation = c("A", "Bt1", "Bt2", "C"),
                  clay_pct = c(10, 25, 26, 20), sand_pct = 50, silt_pct = c(40, 25, 24, 30), ph_h2o = 6.5, esp_pct = 1,
                  clay_films_amount = c(NA, "common", "common", NA), clay_films_thickness_mm = c(NA, 1, 1, NA))
  g <- .cg(.cp(h), "argic_horizon")
  expect_equal(g$full, c(FALSE, TRUE, TRUE, FALSE)); expect_true(all(is.na(g$r$stand_in)))
})

test_that("D4: hydragric items 4 and 5 not recorded stay undecided where the WRB fails", {
  skip_on_cran()
  p <- .p1(c("Ap1", "Ap2", "Br", "Bg"), films = NA, st3 = NA)
  expect_false(isTRUE(soilKey:::hydragric(p)$passed))
  g <- .cg(p, "hydragric_horizon")
  expect_true(is.na(g$full[3]))
  expect_false("stand_in:hydragric" %in% g$r$flags)
})

test_that("D6: the siltigic and cumulic runs stop at a C", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 100), designation = c("Ap", "Au1", "Au2", "C"),
                  oc_pct = c(1.2, 0.8, 0.6, 0.2), clay_pct = 20, sand_pct = 30, munsell_hue_moist = "10YR",
                  munsell_value_moist = 4, munsell_chroma_moist = 2, caco3_pct = 5, artefacts_pct = c(1, 1, 1, NA),
                  silting_sieve_test = c("positive", NA, NA, NA))
  expect_equal(.cg(.cp(h), "siltigic_epipedon")$full, c(TRUE, TRUE, TRUE, FALSE))
  h$oc_pct[4] <- 0.35
  expect_equal(.cg(.cp(h), "siltigic_epipedon")$full, c(TRUE, TRUE, TRUE, FALSE))
  # undesignated: the run stops at under 3 g/kg carbon or without artefacts
  h$designation <- NA; h$oc_pct[4] <- 0.2
  expect_equal(.cg(.cp(h), "siltigic_epipedon")$full, c(TRUE, TRUE, TRUE, FALSE))
  h <- data.frame(top_cm = c(0, 25, 55), bottom_cm = c(25, 55, 120), designation = c("Ap", "Au", "C"),
                  oc_pct = c(1.5, 0.8, 0.1), clay_pct = 20, sand_pct = 30, munsell_hue_moist = "10YR",
                  munsell_value_moist = 4, munsell_chroma_moist = 2, artefacts_pct = c(1, 1, NA),
                  mottle_colors = c("7.5YR 5/6", NA, NA))
  expect_equal(.cg(.cp(h), "cumulic_epipedon")$full, c(TRUE, TRUE, FALSE))
})

test_that("D7: a natro-argic layer is no cambic horizon, and the panel shows the argic only", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100), designation = c("A", "Btn", "C"),
                  clay_pct = c(10, 25, 20), sand_pct = 50, ph_h2o = 8.0, esp_pct = c(5, 40, 30), na_cmol = c(0.5, 6, 4),
                  cec_cmol = 15, ecec_cmol = 15, structure_type = c("granular", "subangular blocky", "massive"),
                  structure_grade = c("weak", "moderate", NA), clay_films_amount = c(NA, "common", NA),
                  clay_films_thickness_mm = c(NA, 1, NA), soluble_salts_pct = 0.1, k_total_pct = 2)
  p <- .cp(h, list(soil_moisture_regime = "ustic", soil_temperature_regime = "mesic"))
  expect_false(.cg(p, "cambic_horizon")$full[2])
  fr <- soilKey:::.cst_identify(p)
  expect_equal(fr$code[2], "Argic horizon"); expect_length(fr$overlay[[2]], 0L)
  expect_true("Argic horizon: natro-argic" %in% fr$kinds[[2]])
})

test_that("D8: an argic horizon exposed by erosion passes on its clay films", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 100), designation = c("Bt1", "Bt2", "C"),
                  clay_pct = c(30, 35, 20), sand_pct = 30, esp_pct = 1, na_cmol = 0.1, mg_cmol = 2, cec_cmol = 20,
                  clay_films_amount = c("many", "many", NA), clay_films_thickness_mm = c(1, 1, NA))
  expect_equal(.cg(.cp(h), "argic_horizon")$full, c(TRUE, TRUE, FALSE))
  # no films: item 2b cannot apply at the surface
  h$clay_films_amount <- NA; h$clay_films_thickness_mm <- NA
  expect_false(.cg(.cp(h), "argic_horizon")$full[1])
})

test_that("D9: secondary clayification by item c or a recorded thin section", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), designation = c("A", "Bt", "C"),
                  clay_pct = c(20, 30, 22), sand_pct = 30, munsell_hue_moist = c("10YR", "5YR", "10YR"),
                  munsell_value_moist = 4, munsell_chroma_moist = c(3, 6, 3), bulk_density_g_cm3 = c(1.2, 1.5, 1.3),
                  sio2_sulfuric_pct = 40, al2o3_sulfuric_pct = 20, micromorph_features = c(NA, "argillans", NA),
                  esp_pct = 1, ph_h2o = 7)
  g <- .cg(.cp(h), "argic_horizon")
  expect_true(g$full[2])
  expect_false("thin_section_assumed" %in% g$r$flags)
})

test_that("D10: the sulfuric horizon over sulfidic materials below its base", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 10, 20), bottom_cm = c(10, 20, 60), designation = c("Bj1", "Bj2", "Cg"),
                  jarosite_present = c(TRUE, TRUE, FALSE), sulfate_s_pct = c(0.1, 0.1, 0.01), ph_h2o = c(3.5, 3.6, 6.5),
                  sulfidic_s_pct = c(0, 0, 0.8))
  expect_equal(.cg(.cp(h), "sulfuric_horizon")$full, c(TRUE, TRUE, FALSE))
  h$sulfidic_s_pct[3] <- 0
  expect_equal(.cg(.cp(h), "sulfuric_horizon")$full, c(FALSE, FALSE, FALSE))
})

test_that("D11: the neutral hue N", {
  skip_on_cran()
  expect_equal(.cg(.p1(hue1 = "N"), "anthrostagnic_epipedon")$full, c(TRUE, TRUE, FALSE, FALSE))
  h <- .p1(c("Ap1", "Ap2", "Br", "Bg"))$horizons
  h$munsell_hue_moist[3] <- "N"; h$munsell_chroma_moist[3] <- 0; h$mottle_colors[3] <- NA; h$fe_dcb_pct[3] <- 1
  expect_true(.cg(.cp(h, list(soil_moisture_regime = "anthrostagnic")), "hydragric_horizon")$full[3])
  h <- data.frame(top_cm = c(0, 8, 30), bottom_cm = c(8, 30, 60), designation = c("Om", "A", "C"),
                  root_mat_pct = c(70, NA, NA), munsell_hue_moist = "N", munsell_value_moist = c(2, 3, 4),
                  munsell_value_dry = c(4, 5, 6), munsell_chroma_moist = 0, oc_pct = c(15, 4, 1),
                  n_total_pct = c(0.9, 0.3, 0.1), bulk_density_g_cm3 = c(0.8, 1.0, 1.3), water_saturation_days = 0)
  expect_false(.cg(.cp(h, list(soil_temperature_regime = "cryic")), "mattic_epipedon")$full[1])
  x <- soilKey:::.cst_ctx(.cp(data.frame(top_cm = 0, bottom_cm = 10, munsell_hue_moist = "N")))
  col <- soilKey:::.cst_colours(x)
  expect_false(soilKey:::.cst_hue_le(col, 15)); expect_true(soilKey:::.cst_hue_ge(col, 17.5))
  expect_false(soilKey:::.cst_hue_in(col, 17.5, 20)); expect_true(soilKey:::.cst_hue_in(col, 20, 27.5, neutral = TRUE))
})

test_that("R1: in a cryic soil 5 cm of cambic horizon waive the 25 cm base", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 8, 15), bottom_cm = c(8, 15, 40), designation = c("A", "Bw", "C"),
                  clay_pct = 15, sand_pct = 40, structure_type = c("granular", "subangular blocky", "massive"),
                  structure_grade = c("weak", "moderate", NA), munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
                  munsell_chroma_moist = c(2, 4, 3), cec_cmol = 15, ecec_cmol = 14, k_total_pct = 2, esp_pct = 1,
                  na_cmol = 0.1, mg_cmol = 1, ph_h2o = 6.5, oc_pct = c(2, 0.5, 0.2))
  expect_true(.cg(.cp(h, list(soil_temperature_regime = "cryic")), "cambic_horizon")$full[2])
  expect_false(.cg(.cp(h, list(soil_temperature_regime = "mesic", soil_moisture_regime = "udic")),
                   "cambic_horizon")$full[2])
  # as the review wrote it (no pH or carbon), the spodic reading of the Bw
  # is undecided, and so its cambic item 6
  h$ph_h2o <- NULL; h$oc_pct <- NULL
  expect_true(is.na(.cg(.cp(h, list(soil_temperature_regime = "cryic")), "cambic_horizon")$full[2]))
})

test_that("Q18 as decided: value or chroma within one unit, no hue rule", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 100), designation = c("Ap", "Au1", "Au2", "C"),
                  oc_pct = c(1.2, 0.8, 0.6, 0.2), clay_pct = 20, sand_pct = 30,
                  munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(4, 4, 6, 4),
                  munsell_chroma_moist = c(2, 2, 3, 2), caco3_pct = 5, artefacts_pct = c(1, 1, 1, NA),
                  silting_sieve_test = c("positive", NA, NA, NA))
  # value 4 -> 6 but chroma 2 -> 3: homogeneous; the hue change does not count
  expect_equal(.cg(.cp(h), "siltigic_epipedon")$full, c(TRUE, TRUE, TRUE, FALSE))
  h$munsell_chroma_moist[3] <- 4
  expect_false(.cg(.cp(h), "siltigic_epipedon")$full[1])
})

test_that("R10: the salic evidence on the 1:1 EC; the context is cached per pedon", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("Az", "C"), ec_1to1_dS_m = c(20, 2))
  g <- .cg(.cp(h), "salic_horizon")
  expect_equal(g$full, c(FALSE, FALSE)); expect_equal(g$evid, c(TRUE, FALSE))
  p <- .cp(h)
  expect_identical(soilKey:::.cst_ctx(p), soilKey:::.cst_ctx(p))
  # a changed profile gets a new context
  p2 <- .cp(transform(h, ec_1to1_dS_m = c(40, 2)))
  expect_false(identical(soilKey:::.cst_ctx(p), soilKey:::.cst_ctx(p2)))
  expect_true(.cres(p2, "salic_horizon_cst", grade = "at_least_evidence")$passed)
  # a horizon table changed in place is seen
  x1 <- soilKey:::.cst_ctx(p)
  data.table::set(p$horizons, i = 1L, j = "ec_1to1_dS_m", value = 40)
  expect_false(identical(x1, soilKey:::.cst_ctx(p)))
  expect_true(.cres(p, "salic_horizon_cst", grade = "at_least_evidence")$passed)
})
