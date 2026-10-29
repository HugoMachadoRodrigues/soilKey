# v0.9.310: the argic family as WRB 2022 writes it. The argic horizon
# (criteria 1, 2.a.iii, 2.b.ii; a subsurface horizon), the natric horizon
# (criteria 3 and 4) and the Solonetz depth, the ferralic thickness, the
# Ferralsol key, the Acrisols, Lixisols, Alisols and Luvisols of the 2022 key
# and their qualifiers, and the nitic horizon as a subsurface horizon. The
# profiles are made up.

.v09310_pedon <- function(...) {
  h <- data.frame(..., stringsAsFactors = FALSE)
  PedonRecord$new(site = list(id = "v09310"), horizons = h)
}

# ---- argic horizon -----------------------------------------------------------

test_that("criterion 2.b.ii: clay coatings common or more make an argic horizon without a clay increase", {
  skip_on_cran()
  p <- function(films) .v09310_pedon(
    designation = c("A", "Bt", "BC"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
    clay_pct = c(30, 34, 32), silt_pct = c(30, 30, 30), sand_pct = c(40, 36, 38),
    clay_films_amount = c(NA, films, NA))
  r <- argic(p("common"))
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2L)
  expect_true(isTRUE(argic(p("many"))$passed))
  # patchy cutans are "few" and do not count; unrecorded films leave 2.b unmet
  expect_false(argic(p("few"))$passed)
  expect_false(argic(p(NA_character_))$passed)
  # the USDA argillic and the SiBCS B textural keep their reading
  expect_false(isTRUE(soilKey:::.argic_impl(p("common"))$passed))
})

test_that("the argic horizon is a subsurface horizon: not the surface layer nor an A horizon", {
  skip_on_cran()
  # coatings at the surface (an eroded profile) or in an A do not count
  p <- .v09310_pedon(
    designation = c("Ap", "Ah2", "Bw"), top_cm = c(0, 15, 35), bottom_cm = c(15, 35, 80),
    clay_pct = c(10, 18, 18), silt_pct = c(30, 30, 30), sand_pct = c(60, 52, 52),
    clay_films_amount = c("common", "common", NA))
  # Ah2 has +8% clay over the Ap and clay coatings: an A horizon, so no argic
  expect_false(argic(p)$passed)
  expect_true(isTRUE(soilKey:::.argic_impl(p)$passed))   # the earlier reading
  # the same layer written as a B is argic
  p$horizons$designation[2] <- "Bt"
  expect_true(isTRUE(argic(p)$passed))
  # without designations only the surface layer is set aside
  p$horizons$designation <- NA_character_
  expect_equal(argic(p)$layers, 2:3)
})

test_that("criterion 1: an argic horizon has >= 8% clay", {
  skip_on_cran()
  p <- function(clay) .v09310_pedon(
    designation = c("A", "Bt"), top_cm = c(0, 20), bottom_cm = c(20, 60),
    clay_pct = c(1, clay), silt_pct = c(28, 28), sand_pct = c(71, 72 - clay))
  expect_false(argic(p(7))$passed)          # +6% clay, but 7% < 8%
  expect_true(isTRUE(argic(p(8))$passed))
  expect_true(isTRUE(soilKey:::.argic_impl(p(7))$passed))
  # a coated layer whose clay is not known: undecided
  q <- .v09310_pedon(
    designation = c("A", "Bt"), top_cm = c(0, 20), bottom_cm = c(20, 60),
    clay_pct = c(20, NA), silt_pct = c(30, NA), sand_pct = c(50, NA),
    clay_films_amount = c(NA, "common"))
  expect_true(is.na(argic(q)$passed))
})

test_that("criterion 2.a.iii: a transitional horizon of <= 15 cm, Bt layers being part of the argic horizon", {
  skip_on_cran()
  p <- function(ba_bottom, ba = "BA") .v09310_pedon(
    designation = c("A", ba, "Bt"), top_cm = c(0, 20, ba_bottom),
    bottom_cm = c(20, ba_bottom, ba_bottom + 40),
    clay_pct = c(10, 13, 18), silt_pct = c(30, 30, 30), sand_pct = c(60, 57, 52))
  # Bt: +8 over the A, +5 over the BA. A BA of 25 cm is too thick a transition
  expect_false(argic(p(45))$passed)
  expect_true(isTRUE(argic(p(30))$passed))           # a BA of 10 cm
  expect_true(isTRUE(argic(p(45, ba = "Bt1"))$passed)) # a Bt1 is part of the argic horizon
  expect_true(isTRUE(soilKey:::.argic_impl(p(45))$passed))
  # test_clay_increase_argic() keeps its default
  expect_equal(test_clay_increase_argic(p(45)$horizons)$layers, 3L)
  expect_length(test_clay_increase_argic(p(45)$horizons, max_transition_cm = 15)$layers, 0L)
})

# ---- Acrisols, Lixisols, Alisols, Luvisols --------------------------------

test_that("Acrisols ask for exchangeable Al > bases in half or more of 50-100 cm", {
  skip_on_cran()
  p <- make_acrisol_canonical()               # Bt: Al 0.9-0.95 > bases 0.72
  expect_true(isTRUE(acrisol(p)$passed))
  expect_equal(classify_wrb2022(p, on_missing = "silent")$rsg_or_order, "Acrisols")
  # Al below the bases: the profile is a Lixisol
  q <- p$clone(deep = TRUE); q$horizons$al_cmol[3:4] <- 0.4; q$horizons$al_sat_pct[3:4] <- 36
  expect_false(acrisol(q)$passed)
  expect_equal(classify_wrb2022(q, on_missing = "silent")$rsg_or_order, "Lixisols")
})

test_that("without the cations the Al test reads the pH by Annex 2, Table 9.3", {
  skip_on_cran()
  p <- make_acrisol_canonical()
  for (v in c("ca_cmol", "mg_cmol", "k_cmol", "na_cmol", "al_cmol", "al_sat_pct", "bs_pct"))
    p$horizons[[v]][3:4] <- NA_real_
  # pH 4.7-4.8 with < 2% organic carbon: at or above 4.6, so bases >= Al
  expect_false(acrisol(p)$passed)
  p$horizons$ph_h2o[3:4] <- c(4.4, 4.5)
  expect_true(isTRUE(acrisol(p)$passed))
  # pH 5.0 with the organic carbon unknown: 4.6, 4.9 or 5.4 by the carbon class
  p$horizons$ph_h2o[3:4] <- 5.0; p$horizons$oc_pct[3:4] <- NA_real_
  expect_true(is.na(acrisol(p)$passed))
  expect_true(any(c("al_cmol", "ph_h2o") %in% acrisol(p)$missing))
})

test_that("the depth range of the Al test: 50-100 cm or the lower half above a limiting layer", {
  skip_on_cran()
  p <- function(rock) .v09310_pedon(
    designation = c("A", "E", "Bt", if (rock) "R" else "C"),
    top_cm = c(0, 15, 30, 60), bottom_cm = c(15, 30, 60, 70),
    clay_pct = c(15, 12, 35, if (rock) NA else 30), silt_pct = c(25, 25, 20, if (rock) NA else 20),
    sand_pct = c(60, 63, 45, if (rock) NA else 50), cec_cmol = c(6, 4, 7, if (rock) NA else 6),
    al_cmol = c(1, 1, 3, if (rock) NA else 3), ca_cmol = c(0.5, 0.3, 0.5, if (rock) NA else 0.5),
    mg_cmol = c(0.2, 0.1, 0.2, if (rock) NA else 0.2),
    clay_films_amount = c(NA, NA, "common", NA))
  # rock at 60 cm: the lower half of the mineral soil above it, 30-60 cm
  expect_true(isTRUE(soilKey:::.wrb_al_dominance(p(TRUE))$al_gt))
  expect_equal(soilKey:::.wrb_al_dominance(p(TRUE))$window, c(30, 60))
  # no limiting layer: 50-100 cm, described only to 70 cm. 20 cm with Al >
  # bases and 30 cm not described: undecided
  expect_true(is.na(soilKey:::.wrb_al_dominance(p(FALSE))$al_gt))
})

test_that("Lixisols, Alisols and Luvisols: the 2022 tests in the key's order", {
  skip_on_cran()
  lx <- make_lixisol_canonical(); al <- make_alisol_canonical(); lv <- make_luvisol_canonical()
  expect_true(isTRUE(lixisol(lx)$passed))
  expect_true(isTRUE(alisol(al)$passed))
  expect_false(lixisol(al)$passed)                    # CEC >= 24 throughout
  expect_true(isTRUE(luvisol(lv)$passed))
  # the CEC of the argic horizon not recorded: undecided
  q <- lx$clone(deep = TRUE); q$horizons$cec_cmol <- NA_real_
  expect_true(is.na(lixisol(q)$passed))
  # an argic horizon starting > 100 cm is no Luvisol
  d <- lv$clone(deep = TRUE)
  d$horizons$top_cm <- c(0, 80, 105, 130, 170); d$horizons$bottom_cm <- c(80, 105, 130, 170, 210)
  expect_true(isTRUE(argic(d)$passed))
  expect_false(luvisol(d)$passed)
  expect_false(alisol(d)$passed)
})

test_that("the Acric, Lixic, Alic and Luvic qualifiers follow Chapter 5", {
  skip_on_cran()
  q <- function(f, p) get(f, envir = asNamespace("soilKey"))(p)$passed
  ac <- make_acrisol_canonical(); lx <- make_lixisol_canonical()
  al <- make_alisol_canonical(); lv <- make_luvisol_canonical()
  expect_true(isTRUE(q("qual_acric", ac))); expect_false(q("qual_lixic", ac))
  expect_true(isTRUE(q("qual_lixic", lx))); expect_false(q("qual_acric", lx))
  expect_true(isTRUE(q("qual_alic", al)));  expect_false(q("qual_luvic", al))
  expect_true(isTRUE(q("qual_luvic", lv))); expect_false(q("qual_alic", lv))
  # Alic and Luvic need CEC >= 24 throughout within 150 cm
  expect_false(q("qual_luvic", lx)); expect_false(q("qual_alic", ac))
})

# ---- natric horizon and Solonetz --------------------------------------------

test_that("natric criterion 3: columnar or prismatic, or blocky with tongues", {
  skip_on_cran()
  p <- make_solonetz_canonical()              # Btn columnar, BC massive
  expect_true(isTRUE(natric_horizon(p)$passed))
  b <- p$clone(deep = TRUE); b$horizons$structure_type[3] <- "subangular blocky"
  expect_true(is.na(natric_horizon(b)$passed))          # tongues not recorded
  expect_true("boundary_topography" %in% natric_horizon(b)$missing)
  b$horizons$boundary_topography <- c(NA, "tongued", NA, NA)
  expect_true(isTRUE(natric_horizon(b)$passed))
  b$horizons$boundary_topography <- c(NA, "smooth", NA, NA)   # no penetrations
  expect_false(natric_horizon(b)$passed)
  # a structure soilKey does not recognise (a survey code) is undecided
  k <- p$clone(deep = TRUE); k$horizons$structure_type[3] <- "C"
  expect_true(is.na(natric_horizon(k)$passed))
  m <- p$clone(deep = TRUE); m$horizons$structure_type[3] <- "massive"
  expect_false(natric_horizon(m)$passed)
  # no structure recorded in the whole horizon: criterion 3 is not tested
  n <- p$clone(deep = TRUE); n$horizons$structure_type[3:4] <- NA_character_
  expect_true(isTRUE(natric_horizon(n)$passed))
  # the USDA keys keep the earlier reading
  expect_true(isTRUE(soilKey:::natric_horizon_usda(m)$passed))
})

test_that("natric criterion 4: ESP >= 15 in the upper 40 cm, or Mg + Na > Ca + exchange acidity", {
  skip_on_cran()
  p <- make_solonetz_canonical()
  # ESP 5.6% in the Btn (30-80 cm), Mg + Na < Ca: the Btn does not start one
  lo <- p$clone(deep = TRUE); lo$horizons$na_cmol[3] <- 1
  expect_false(natric_horizon(lo)$passed)
  # 4.b: Mg + Na > Ca at pH 9.2 (no exchange acidity) and ESP 25% in the BC,
  # starting 50 cm below the upper limit of the natric horizon
  mg <- p$clone(deep = TRUE)
  mg$horizons$na_cmol[3] <- 2.5; mg$horizons$mg_cmol[3] <- 5; mg$horizons$ca_cmol[3] <- 2
  expect_true(isTRUE(natric_horizon(mg)$passed))
  # at pH 7.5 the exchange acidity is not known: undecided
  mg$horizons$ph_h2o[3] <- 7.5
  expect_true(is.na(natric_horizon(mg)$passed))
  # the natric horizon ends where its criteria stop: a Btn of ESP 28% over a
  # BC of ESP 6% starting within its upper 40 cm is natric down to the BC
  sh <- p$clone(deep = TRUE)
  sh$horizons$top_cm <- c(0, 15, 30, 55); sh$horizons$bottom_cm <- c(15, 30, 55, 150)
  sh$horizons$na_cmol[4] <- 1
  r <- natric_horizon(sh)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3L)
})

test_that("a saline natric horizon reads the ESP the laboratory reports (EC >= 4 dS/m)", {
  skip_on_cran()
  p <- make_solonetz_canonical()
  p$horizons$ec_dS_m <- c(1, 2, 6, 6)
  expect_true(is.na(natric_horizon(p)$passed))
  p$horizons$esp_pct <- c(NA, NA, 20, 18)
  expect_true(isTRUE(natric_horizon(p)$passed))
  p$horizons$esp_pct[3] <- 12
  expect_false(natric_horizon(p)$passed)
})

test_that("Solonetz: a natric horizon starting <= 100 cm", {
  skip_on_cran()
  p <- make_solonetz_canonical()
  expect_true(isTRUE(soilKey:::solonetz(p)$passed))
  d <- p$clone(deep = TRUE)
  d$horizons$top_cm <- c(0, 95, 110, 160); d$horizons$bottom_cm <- c(95, 110, 160, 230)
  expect_true(isTRUE(natric_horizon(d)$passed))
  expect_false(soilKey:::solonetz(d)$passed)
  expect_false(classify_wrb2022(d, on_missing = "silent")$rsg_or_order == "Solonetz")
})

# ---- ferralic horizon and Ferralsols ---------------------------------------

.v09310_ferralic <- function(cec_bw2 = 5) .v09310_pedon(
  designation = c("A", "Bw1", "Bw2", "C"), top_cm = c(0, 15, 35, 55),
  bottom_cm = c(15, 35, 55, 90), clay_pct = c(40, 50, 50, 45),
  silt_pct = c(20, 15, 15, 20), sand_pct = c(40, 35, 35, 35),
  cec_cmol = c(12, 5, cec_bw2, 15), oc_pct = c(2, 0.6, 0.4, 0.2))

test_that("the ferralic thickness is that of its adjoining sub-horizons", {
  skip_on_cran()
  r <- ferralic(.v09310_ferralic())
  expect_true(isTRUE(r$passed))               # Bw1 + Bw2 = 40 cm
  expect_equal(r$layers, 2:3)
  expect_false(ferralic(.v09310_ferralic(cec_bw2 = 12))$passed)   # Bw1 alone, 20 cm
  # an A of low-activity clay is not taken into the horizon
  a <- .v09310_ferralic(); a$horizons$cec_cmol[1] <- 4
  expect_equal(ferralic(a)$layers, 2:3)
})

test_that("Ferralsols: a ferralic horizon starting <= 150 cm", {
  skip_on_cran()
  p <- .v09310_pedon(
    designation = c("A", "B", "Bw"), top_cm = c(0, 20, 160), bottom_cm = c(20, 160, 220),
    clay_pct = c(40, 50, 55), silt_pct = c(20, 15, 15), sand_pct = c(40, 35, 30),
    cec_cmol = c(15, 15, 6))
  expect_true(isTRUE(ferralic(p)$passed))
  expect_false(ferralsol(p)$passed)
})

test_that("Ferralsols: an argic horizon starting at the ferralic, and its exceptions throughout the upper 30 cm", {
  skip_on_cran()
  p <- make_nitossolo_canonical()             # clay films from 20 cm; ferralic from 20 cm
  expect_equal(min(p$horizons$top_cm[argic(p)$layers]), 20)
  expect_equal(min(p$horizons$top_cm[ferralic(p)$layers]), 20)
  # SOC < 1.4, no pH KCl, no water-dispersible clay: undecided
  expect_true(is.na(ferralsol(p)$passed))
  # a Delta pH >= 0 in the BA (the argic horizon's upper 30 cm): Ferralsol
  q <- p$clone(deep = TRUE); q$horizons$ph_kcl <- q$horizons$ph_h2o + 0.1
  expect_true(isTRUE(ferralsol(q)$passed))
  # water-dispersible clay is a clay content (% of the fine earth)
  w <- p$clone(deep = TRUE); w$horizons$water_dispersible_clay_pct <- c(30, 5, 4, 4)
  expect_true(isTRUE(ferralsol(w)$passed))
  w$horizons$water_dispersible_clay_pct[2] <- 25; w$horizons$ph_kcl <- w$horizons$ph_h2o - 0.8
  expect_false(ferralsol(w)$passed)
  # an argic horizon starting below the upper limit of the ferralic does not count
  b <- make_nitossolo_canonical(); b$horizons$clay_films_amount <- c(NA, NA, "many", "common")
  expect_true(min(b$horizons$top_cm[argic(b)$layers]) > 20)
  expect_true(isTRUE(ferralsol(b)$passed))
})

# ---- nitic horizon ------------------------------------------------------------

test_that("the nitic horizon is a subsurface horizon", {
  skip_on_cran()
  p <- function(b_bottom) .v09310_pedon(
    designation = c("A", "Bt"), top_cm = c(0, 15), bottom_cm = c(15, b_bottom),
    clay_pct = c(45, 55), silt_pct = c(25, 20), sand_pct = c(30, 25),
    structure_type = c("polyhedral", "polyhedral"), structure_grade = c("strong", "strong"),
    clay_films_amount = c("common", "many"), fe_dcb_pct = c(8, 9), fe_ox_pct = c(0.6, 0.6))
  r <- nitic_horizon(p(60))
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2L)                  # the A is not part of it
  expect_false(nitic_horizon(p(35))$passed)   # 20 cm of B: the A no longer makes up the 30 cm
})
