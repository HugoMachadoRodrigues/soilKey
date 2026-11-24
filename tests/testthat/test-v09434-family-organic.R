# v0.9.434: USDA family of organic soils (Histosols and Histels): particle
# size of the Terric subgroups, mineralogy (ferrihumic, limnic, the mineral
# key for the Terric subgroups), reaction (kalkic, euic, dysic), temperature
# and the depth classes of Histosols (KST 13th ed., ch. 17, pp. 370-372),
# with the readings F16-F18 of the author and F39-F46.

.fp <- function(h, site = list(id = "o")) soilKey::PedonRecord$new(site = site, horizons = h)
.und <- function(a) soilKey:::.family_term_undecided_usda(a)

# A mineralogical analysis that keys "mixed" in section E.
.ana <- list(mineralogy_analysed = TRUE, caco3_pct = 0, caso4_pct = 0,
             fe_dcb_pct = 1, ph_naf = 7)

# A Terric Hemist: organic to 90 cm, a mineral layer 90-150 cm. `min` sets
# columns of the mineral layer, `...` columns of every layer.
.terric <- function(min = list(), ..., top_min = 90) {
  h <- data.frame(top_cm = c(0, 30, top_min), bottom_cm = c(30, top_min, 150),
                  designation = c("Oa", "Oe", "C"), oc_pct = c(35, 30, 0.5),
                  clay_pct = c(NA, NA, 20), sand_pct = c(NA, NA, 40),
                  silt_pct = c(NA, NA, 40), ph_cacl2 = c(4.0, 4.2, 4.4))
  for (k in names(min)) { if (is.null(h[[k]])) h[[k]] <- NA; h[[k]][3] <- min[[k]] }
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  .fp(h)
}
.ps <- function(p, gg = "Haplohemists", sg = "Terric Haplohemists", ord = "HI")
  family_particle_size_usda(p, ord, gg, sg)
.mn <- function(p, gg = "Haplohemists", sg = "Typic Haplohemists", ord = "HI", ...)
  family_mineralogy_usda(p, ord, gg, sg, ...)
.rx <- function(p, gg = "Haplohemists", sg = "Typic Haplohemists", ord = "HI")
  family_reaction_usda(p, ord, gg, sg)
.dp <- function(p, gg = "Haplohemists", sg = "Typic Haplohemists", ord = "HI")
  family_depth_class_usda(p, ord, gg, sg)


# ---- particle-size classes of the Terric subgroups (p. 370) -----------------

test_that("particle size: the six classes of the organic key", {
  skip_on_cran()
  expect_equal(.ps(.terric())$value, "loamy")
  expect_equal(.ps(.terric(list(clay_pct = 45, sand_pct = 20, silt_pct = 35)))$value, "clayey")
  expect_equal(.ps(.terric(list(clay_pct = 3, sand_pct = 90, silt_pct = 7,
                                very_fine_sand_pct = 5)))$value, "sandy or sandy-skeletal")
  # sandy whatever the fragments (item 2 before the skeletal items)
  expect_equal(.ps(.terric(list(clay_pct = 3, sand_pct = 90, silt_pct = 7,
                                very_fine_sand_pct = 5, coarse_fragments_pct = 60)))$value,
               "sandy or sandy-skeletal")
  expect_equal(.ps(.terric(list(coarse_fragments_pct = 50)))$value, "loamy-skeletal")
  expect_equal(.ps(.terric(list(coarse_fragments_pct = 50, clay_pct = 40, sand_pct = 30,
                                silt_pct = 30)))$value, "clayey-skeletal")
  expect_equal(.ps(.terric(list(coarse_fragments_pct = 95)))$value, "fragmental")
  # no fine-family classes (fine-loamy, coarse-silty ...) and no contrasts
  expect_false(grepl("fine|coarse|over", .ps(.terric())$value))
})

test_that("particle size: the control section of the Terric mineral layer", {
  skip_on_cran()
  # upper 30 cm of the layer (90-120) or its part within 130 cm (90-130):
  # the thicker
  cs <- .ps(.terric())$evidence$control_section
  expect_equal(c(cs$top, cs$bottom), c(90, 130))
  # a layer starting at 35 cm: 35-130
  cs2 <- .ps(.terric(top_min = 35))$evidence$control_section
  expect_equal(c(cs2$top, cs2$bottom), c(35, 130))
  # a layer at 120 cm: its upper 30 cm (120-150)
  cs3 <- .ps(.terric(top_min = 120))$evidence$control_section
  expect_equal(c(cs3$top, cs3$bottom), c(120, 150))
  # the organic layers above are not averaged in
  r <- .ps(.terric(list(clay_pct = 45, sand_pct = 20, silt_pct = 35)))
  expect_equal(r$evidence$weighted$clay, 45)
})

test_that("particle size: only the Terric subgroups; undecided without data", {
  skip_on_cran()
  ty <- .ps(.terric(), sg = "Typic Haplohemists")
  expect_null(ty$value); expect_false(.und(ty))
  expect_equal(ty$evidence$reason, "not named in this taxon")
  un <- .ps(.terric(list(clay_pct = NA, sand_pct = NA, silt_pct = NA)))
  expect_null(un$value); expect_true(.und(un))
  expect_true(any(c("clay_pct", "sand_pct") %in% un$missing))
  # a layer of unknown kind (no designation, no organic carbon) above the
  # mineral layer could move its top
  p <- .terric(); h <- p$horizons
  h$designation[2] <- NA; h$oc_pct[2] <- NA
  u2 <- .ps(.fp(as.data.frame(h)))
  expect_null(u2$value); expect_true(.und(u2))
  expect_true("oc_pct" %in% u2$missing)
})

test_that("particle size: Terric Histels read the mineral layer within 100 cm", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 40, 80), bottom_cm = c(40, 80, 150),
                  designation = c("Oi", "C", "Cf"), oc_pct = c(35, 0.5, 0.5),
                  clay_pct = c(NA, 40, 40), sand_pct = c(NA, 30, 30),
                  silt_pct = c(NA, 30, 30))
  r <- .ps(.fp(h), "Hemistels", "Terric Hemistels", "GE")
  expect_equal(r$value, "clayey")
  # control section of Histels: 25 cm below the permafrost table (105)
  expect_equal(c(r$evidence$control_section$top, r$evidence$control_section$bottom),
               c(40, 105))
  t <- .ps(.fp(h), "Hemistels", "Typic Hemistels", "GE")
  expect_null(t$value); expect_false(.und(t))
})


# ---- mineralogy classes (pp. 371-372) ---------------------------------------

test_that("mineralogy: ferrihumic, and the taxa that do not use it", {
  skip_on_cran()
  p <- .terric(ferrihumic_material = c(NA, TRUE, NA))
  expect_equal(.mn(p)$value, "ferrihumic")
  expect_equal(.mn(p, sg = "Terric Haplohemists")$value, "ferrihumic")
  for (a in list(.mn(p, "Udifolists", "Typic Udifolists"),
                 .mn(p, "Sphagnofibrists", "Typic Sphagnofibrists"),
                 .mn(p, "Cryofibrists", "Sphagnic Cryofibrists"))) {
    expect_null(a$value); expect_false(.und(a))
  }
  # Histels: never ferrihumic
  h <- .mn(p, "Hemistels", "Typic Hemistels", "GE")
  expect_null(h$value); expect_false(.und(h))
  # not recorded: absent
  expect_null(.mn(.terric())$value)
  expect_false(.und(.mn(.terric())))
})

test_that("mineralogy: coprogenous, diatomaceous and marly limnic materials", {
  skip_on_cran()
  lim <- function(des, thk = 10, origin = NA_character_) {
    h <- data.frame(top_cm = c(0, 40, 40 + thk), bottom_cm = c(40, 40 + thk, 150),
                    designation = c("Oa", des, "Oe"), oc_pct = c(35, 5, 30),
                    layer_origin = c(NA, origin, NA))
    .fp(h)
  }
  expect_equal(.mn(lim("Lco"))$value, "coprogenous")
  expect_equal(.mn(lim("Ldi"))$value, "diatomaceous")
  expect_equal(.mn(lim("Lma"))$value, "marly")
  expect_equal(.mn(lim("C", origin = "marl"))$value, "marly")
  # less than 5 cm: no class
  thin <- .mn(lim("Lco", thk = 3))
  expect_null(thin$value); expect_false(.und(thin))
  # a limnic layer of unrecorded kind: undecided
  un <- .mn(lim("L"))
  expect_null(un$value); expect_true(.und(un))
  expect_true("layer_origin" %in% un$missing)
  # also in Folists ("other Histosols"); never in Histels
  expect_equal(.mn(lim("Lco"), "Udifolists", "Typic Udifolists")$value, "coprogenous")
  hl <- .mn(lim("Lco"), "Hemistels", "Limnic Hemistels", "GE")
  expect_null(hl$value); expect_false(.und(hl))
  # ferrihumic first
  p <- lim("Lco"); h <- as.data.frame(p$horizons); h$ferrihumic_material <- c(TRUE, NA, NA)
  expect_equal(.mn(.fp(h))$value, "ferrihumic")
})

test_that("mineralogy: the Terric subgroups take the key of mineral soils", {
  skip_on_cran()
  p <- do.call(.terric, list(min = .ana))
  expect_equal(.mn(p, sg = "Terric Haplohemists")$value, "mixed")
  expect_equal(.mn(p, sg = "Terric Haplohemists")$evidence$key, "C")
  # Typic: no class
  ty <- .mn(p)
  expect_null(ty$value); expect_false(.und(ty))
  # Terric Histels (F17) take it; other Histels do not
  ph <- do.call(.terric, list(min = .ana, top_min = 60))
  expect_equal(.mn(ph, "Hemistels", "Terric Hemistels", "GE")$value, "mixed")
  expect_null(.mn(ph, "Hemistels", "Typic Hemistels", "GE")$value)
  # without an analysis the class is undecided
  un <- .mn(.terric(), sg = "Terric Haplohemists")
  expect_null(un$value); expect_true(.und(un))
  # clayey layer: section D
  cl <- do.call(.terric, list(min = c(.ana, list(clay_pct = 50, sand_pct = 20, silt_pct = 30))))
  r <- .mn(cl, sg = "Terric Haplohemists")
  expect_equal(r$value, "mixed")
  expect_match(r$evidence$sections, "D")
})


# ---- reaction classes (p. 372) -----------------------------------------------

test_that("reaction: kalkic, euic, dysic", {
  skip_on_cran()
  # dysic: every organic layer below pH 4.5 (the mineral layer is not read)
  d <- .rx(.terric(ph_cacl2 = c(4.0, 4.2, 6.5)))
  expect_equal(d$value, "dysic")
  expect_match(d$evidence$ph_note, "F16")
  # euic: one organic layer at 4.5 or more
  expect_equal(.rx(.terric(ph_cacl2 = c(4.0, 4.5, 4.0)))$value, "euic")
  # kalkic: effervescence throughout the surface tier (0-30 cm)
  k <- .terric(ph_cacl2 = c(7.2, 7.0, 7.5), effervescence_class = c("strong", NA, NA))
  expect_equal(.rx(k)$value, "kalkic")
  # Histels are never kalkic
  expect_equal(.rx(k, "Hemistels", "Typic Hemistels", "GE")$value, "euic")
  # effervescence not in the whole surface tier: euic
  k2 <- .terric(ph_cacl2 = c(7.2, 7.0, 7.5), effervescence_class = c("none", NA, NA))
  expect_equal(.rx(k2)$value, "euic")
})

test_that("reaction: undecided when data are missing (F12, F18)", {
  skip_on_cran()
  # no pH and no effervescence: kalkic and euic undecided
  u <- .rx(.terric(ph_cacl2 = NA))
  expect_null(u$value); expect_true(.und(u))
  expect_true("ph_cacl2" %in% u$missing)
  # pH missing in one organic layer: euic or dysic undecided
  u2 <- .rx(.terric(ph_cacl2 = c(4.0, NA, 4.0)))
  expect_null(u2$value); expect_true(.und(u2))
  # the description ends before the control section (130 cm): undecided
  h <- data.frame(top_cm = c(0, 40), bottom_cm = c(40, 100), designation = c("Oa", "Oe"),
                  oc_pct = c(35, 30), ph_cacl2 = c(4.0, 4.1))
  u3 <- .rx(.fp(h))
  expect_null(u3$value); expect_true(.und(u3))
  expect_true("horizons" %in% u3$missing)
  # but one organic layer of 4.5 or more decides euic
  h$ph_cacl2 <- c(4.0, 5.0)
  expect_equal(.rx(.fp(h))$value, "euic")
  # mineral soils are unchanged
  m <- family_reaction_usda(.terric(), "EN", "Udorthents", "Typic Udorthents")
  expect_false(identical(m$value, "dysic"))
})


# ---- soil depth classes of Histosols (p. 372) --------------------------------

test_that("depth: micro and shallow from the soil surface", {
  skip_on_cran()
  rock <- function(top) {
    h <- data.frame(top_cm = c(0, top), bottom_cm = c(top, top + 20),
                    designation = c("Oa", "R"), oc_pct = c(35, NA))
    .fp(h)
  }
  expect_equal(.dp(rock(15))$value, "micro")
  expect_equal(.dp(rock(30))$value, "shallow")
  expect_null(.dp(rock(60))$value)
  # the Lithic subgroups take a class too
  expect_equal(.dp(rock(30), sg = "Lithic Haplohemists")$value, "shallow")
  # Folists: micro, never shallow
  expect_equal(.dp(rock(15), "Udifolists", "Lithic Udifolists")$value, "micro")
  f <- .dp(rock(30), "Udifolists", "Lithic Udifolists")
  expect_null(f$value); expect_false(.und(f))
  # a fragmental substitute class (fine earth under 10 %)
  h <- data.frame(top_cm = c(0, 25), bottom_cm = c(25, 60), designation = c("Oa", "C"),
                  oc_pct = c(35, 0.5), coarse_fragments_pct = c(0, 95))
  expect_equal(.dp(.fp(h))$value, "shallow")
  # nothing recorded: no class, not undecided
  n <- .dp(.terric())
  expect_null(n$value); expect_false(.und(n))
})

test_that("depth: a fragipan does not limit roots in Histosols", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 80), designation = c("Oa", "Bx"),
                  oc_pct = c(35, 0.5))
  expect_null(.dp(.fp(h))$value)
  rl <- soilKey:::.root_limiting_usda(.fp(h))
  expect_equal(rl$kind, "fragipan")
})

test_that("depth: Histels keep the class of p. 369", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 30), bottom_cm = c(30, 50), designation = c("Oi", "R"),
                  oc_pct = c(35, NA))
  expect_equal(.dp(.fp(h), "Hemistels", "Typic Hemistels", "GE")$value, "shallow")
  expect_equal(.dp(.fp(h), "Hemistels", "Typic Hemistels", "GE")$reference,
               "Soil Survey Staff (2022), KST 13th ed., Ch. 17, p. 369")
})


# ---- temperature and the name ------------------------------------------------

test_that("temperature: Histosols as mineral soils, Histels as Gelisols", {
  skip_on_cran()
  p <- .terric(); p$site$soil_temperature_regime <- "frigid"
  expect_equal(family_temperature_regime_usda(p, order_code = "HI", great_group = "Haplohemists",
                                              subgroup = "Typic Haplohemists")$value, "frigid")
  expect_null(family_temperature_regime_usda(p, order_code = "HI", great_group = "Cryohemists",
                                             subgroup = "Typic Cryohemists")$value)
  p$site$mean_annual_soil_temp_C <- -5
  expect_equal(family_temperature_regime_usda(p, order_code = "GE", great_group = "Hemistels",
                                              subgroup = "Typic Hemistels")$value, "pergelic")
})

test_that("the family name of an organic soil in the order of p. 370", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 30, 90, 150), bottom_cm = c(30, 90, 150, 170),
                  designation = c("Oa", "Oe", "C", "R"), oc_pct = c(35, 30, 0.5, NA),
                  clay_pct = c(NA, NA, 20, NA), sand_pct = c(NA, NA, 40, NA),
                  silt_pct = c(NA, NA, 40, NA), ph_cacl2 = c(4.0, 4.6, 5.0, NA))
  for (k in names(.ana)) h[[k]] <- c(NA, NA, .ana[[k]], NA)
  site <- list(id = "o", soil_temperature_regime = "mesic")
  fam <- classify_usda_family(.fp(h, site), "HI", great_group = "Haplohemists",
                              subgroup = "Terric Haplohemists")
  expect_equal(soilKey:::family_label_usda(fam), "loamy, mixed, euic, mesic")
  for (k in c("human_material", "cec_activity", "rupture_resistance", "coatings", "cracks"))
    expect_false(.und(fam[[k]]))
  # the depth class comes last
  h2 <- data.frame(top_cm = c(0, 20, 30, 45), bottom_cm = c(20, 30, 45, 60),
                   designation = c("Oa", "Oe", "C", "R"), oc_pct = c(35, 30, 0.5, NA),
                   ph_cacl2 = c(4.0, 4.6, 5.0, NA))
  f2 <- classify_usda_family(.fp(h2, site), "HI", great_group = "Haplohemists",
                             subgroup = "Typic Haplohemists")
  expect_equal(soilKey:::family_label_usda(f2), "euic, mesic, shallow")
})

test_that("classify_usda: the canonical Histosol names its decided terms", {
  skip_on_cran()
  r <- suppressWarnings(classify_usda(make_histosol_canonical(), include_family = TRUE))
  expect_equal(r$name, "loamy Terric Cryohemists")
  fam <- r$trace$family
  # pH in CaCl2, effervescence and the mineral analysis are not recorded
  expect_true(.und(fam$reaction))
  expect_true(.und(fam$mineralogy))
  expect_equal(r$evidence_grade, "C")
})
