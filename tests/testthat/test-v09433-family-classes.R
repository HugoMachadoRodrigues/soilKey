# v0.9.433: USDA family cation-exchange activity, calcareous and reaction,
# soil temperature, soil depth, rupture-resistance, coatings and permanent
# cracks classes, and the family in the name (KST 13th ed., ch. 17, pp. 355
# and 366-370), with the readings F12-F15, F18 and F20 of the author (USDA
# audit, wave 5, 2026-11-23).

.fp <- function(h, site = list(id = "f")) soilKey::PedonRecord$new(site = site, horizons = h)

# A mineralogical analysis that keys "mixed" (no carbonates, no gypsum, NaF
# pH below 8.4 so isotic is ruled out).
.ana <- list(mineralogy_analysed = TRUE, caco3_pct = 0, caso4_pct = 0,
             fe_dcb_pct = 1, ph_naf = 7)
# A clayey soil (fine, 25-100 cm control section of an Inceptisol or
# Entisol); `...` overrides or adds columns.
.fine <- function(..., site = list(id = "f")) {
  h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                  designation = c("A", "Bw1", "Bw2"),
                  clay_pct = 50, sand_pct = 25, silt_pct = 25)
  extra <- utils::modifyList(.ana, list(...))
  for (k in names(extra)) h[[k]] <- extra[[k]]
  .fp(h, site)
}
.cec <- function(p, ord = "IN", gg = "Dystrudepts", sg = "Typic Dystrudepts", ...)
  family_cec_activity_usda(p, ord, gg, sg, ...)
.rx <- function(p, ord = "EN", gg = "Udorthents", sg = "Typic Udorthents", ...)
  family_reaction_usda(p, ord, gg, sg, ...)
.und <- function(a) soilKey:::.family_term_undecided_usda(a)


# ---- cation-exchange activity (pp. 366-367) ---------------------------------

test_that("CEC activity: the four classes over the control section", {
  skip_on_cran()
  expect_equal(.cec(.fine(cec_cmol = 40))$value, "superactive")   # 0.80
  expect_equal(.cec(.fine(cec_cmol = 25))$value, "active")        # 0.50
  expect_equal(.cec(.fine(cec_cmol = 15))$value, "semiactive")    # 0.30
  expect_equal(.cec(.fine(cec_cmol = 5))$value, "subactive")      # 0.10
  expect_equal(.cec(.fine(cec_cmol = 30))$value, "superactive")   # 0.60 exactly
  expect_equal(.cec(.fine(cec_cmol = 12))$value, "semiactive")    # 0.24 exactly
  # the 0-25 cm layer is outside the control section
  expect_equal(.cec(.fine(cec_cmol = c(5, 40, 40)))$value, "superactive")
})

test_that("CEC activity: no clay cut-off; coarse-loamy soils take a class", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                  designation = c("A", "Bw", "C"), clay_pct = 8, sand_pct = 60,
                  silt_pct = 32, very_fine_sand_pct = 10, cec_cmol = 6)
  for (k in names(.ana)) h[[k]] <- .ana[[k]]
  r <- .cec(.fp(h))
  expect_equal(r$value, "superactive")                            # 6 / 8 = 0.75
})

test_that("CEC activity: not used in the taxa and classes of p. 366", {
  skip_on_cran()
  p <- .fine(cec_cmol = 40)
  for (a in list(.cec(p, "OX", "Hapludox", "Typic Hapludox"),
                 .cec(p, "UT", "Kandiudults", "Typic Kandiudults"),
                 .cec(p, "AF", "Hapludalfs", "Kanhaplic Hapludalfs"),
                 .cec(p, "EN", "Quartzipsamments", "Typic Quartzipsamments"),
                 .cec(p, "EN", "Udorthents", "Psammentic Udorthents"),
                 .cec(p, "HI", "Haplohemists", "Typic Haplohemists"))) {
    expect_null(a$value)
    expect_false(.und(a))
    expect_match(a$evidence$reason, "^not used")
  }
  # smectitic mineralogy: not used
  sm <- .cec(.fine(cec_cmol = 40, smectite_pct = 70))
  expect_null(sm$value); expect_false(.und(sm))
  # a sandy particle-size class: not used
  ps <- FamilyAttribute$new("particle_size", "sandy",
                            evidence = list(control_section = list(top = 25, bottom = 100)))
  mn <- FamilyAttribute$new("mineralogy", "mixed")
  sa <- .cec(p, particle_size = ps, mineralogy = mn)
  expect_null(sa$value); expect_false(.und(sa))
})

test_that("CEC activity: mineralogy undecided leaves it undecided", {
  skip_on_cran()
  r <- .cec(.fine(cec_cmol = 40, mineralogy_analysed = NA))
  expect_null(r$value)
  expect_true(.und(r))
  expect_true(length(r$missing) > 0)
})

test_that("CEC activity: clay-size carbonate subtracted (F13)", {
  skip_on_cran()
  # CaCO3 measured above 0, carbonate clay not recorded: undecided
  u <- .cec(.fine(cec_cmol = 20, caco3_pct = 10))
  expect_null(u$value); expect_true("caco3_clay_pct" %in% u$missing)
  # recorded: 20 / (50 - 10) = 0.50
  a <- .cec(.fine(cec_cmol = 20, caco3_pct = 10, caco3_clay_pct = 10))
  expect_equal(a$value, "active")
  expect_equal(a$evidence$cec_to_clay_ratio, 0.5)
  # no carbonate: CaCO3 not measured, pH(H2O) below 7 (as N6): zero
  z <- .cec(.fine(cec_cmol = 20, caco3_pct = NA, ph_h2o = 6))
  expect_equal(z$evidence$cec_to_clay_ratio, 0.4)
})

test_that("CEC activity: the clay estimate from 1500 kPa water (F11)", {
  skip_on_cran()
  # water / clay = 0.70 >= 0.6: clay = 2.5 x (35 - 1) = 85; 40 / 85 = 0.47
  r <- .cec(.fine(cec_cmol = 40, water_content_1500kpa = 35, oc_pct = 1))
  expect_equal(r$value, "active")
  expect_true(isTRUE(r$evidence$ratio$clay_estimated))
})

test_that("CEC activity: measured parts must cover half the section (F18)", {
  skip_on_cran()
  # 25-100 cm; only 60-75 measured (20 %): undecided
  h <- data.frame(top_cm = c(0, 25, 60, 75), bottom_cm = c(25, 60, 75, 120),
                  designation = c("A", "Bw1", "Bw2", "Bw3"), clay_pct = 50,
                  sand_pct = 25, silt_pct = 25, cec_cmol = c(NA, NA, 40, NA))
  for (k in names(.ana)) h[[k]] <- .ana[[k]]
  r <- .cec(.fp(h))
  expect_null(r$value); expect_true("cec_cmol" %in% r$missing)
  h$cec_cmol <- c(NA, 40, 40, NA)           # 25-75 measured: two thirds
  expect_equal(.cec(.fp(h))$value, "superactive")
})

test_that("CEC activity: strongly contrasting families (p. 367)", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                  designation = c("A", "Bw", "C"), clay_pct = c(15, 25, 50),
                  sand_pct = 40, silt_pct = c(45, 35, 10),
                  cec_cmol = c(10, 15, 10), ph_h2o = 6)
  p <- .fp(h)
  cs <- list(top = 25, bottom = 100)
  parts <- list(upper = list(top = 0, bottom = 60, class = "loamy"),
                lower = list(top = 60, bottom = 120, class = "clayey"))
  mk_ps <- function(v) FamilyAttribute$new("particle_size", v,
    evidence = list(control_section = cs, contrast_parts = parts))
  mixed <- FamilyAttribute$new("mineralogy", "mixed")
  # both parts use a class: the more clayey (lower) names it: 10 / 50 = 0.20
  r <- .cec(p, particle_size = mk_ps("fine-loamy over clayey"), mineralogy = mixed)
  expect_equal(r$value, "subactive")
  expect_equal(r$evidence$unit, "lower")
  # one part uses a class: that part names it: 15 / 25 = 0.60
  r2 <- .cec(p, particle_size = mk_ps("fine-loamy over sandy or sandy-skeletal"),
             mineralogy = mixed)
  expect_equal(r2$value, "superactive")
  expect_equal(r2$evidence$unit, "upper")
  # the clayey part with a smectitic mineralogy: the loamy part names it
  smec <- FamilyAttribute$new("mineralogy", "mixed over smectitic", evidence = list(
    parts = list(upper = list(class = "mixed"), lower = list(class = "smectitic"))))
  r3 <- .cec(p, particle_size = mk_ps("fine-loamy over clayey"), mineralogy = smec)
  expect_equal(r3$evidence$unit, "upper")
})


# ---- calcareous and reaction classes (pp. 367-368) ---------------------------

test_that("reaction: calcareous by effervescence or CaCO3 in all parts (F12)", {
  skip_on_cran()
  # CaCO3 measured above 0 everywhere, no effervescence recorded
  expect_equal(.rx(.fine(caco3_pct = 5, ph_cacl2 = 7.5))$value, "calcareous")
  # effervescence recorded, CaCO3 not measured
  # (CaCO3 not measured leaves carbonatic open, so the mineralogy is given)
  e <- .rx(.fine(caco3_pct = NA, effervescence_class = "strong", ph_cacl2 = 7.5),
           mineralogy = FamilyAttribute$new("mineralogy", "mixed"))
  expect_equal(e$value, "calcareous")
  # neither recorded: undecided
  n <- .rx(.fine(caco3_pct = NA, ph_cacl2 = 6.5))
  expect_null(n$value); expect_true(.und(n))
  expect_true(all(c("effervescence_class", "caco3_pct") %in% n$missing))
  # one part with no data (F18, "in all parts"): undecided
  h <- data.frame(top_cm = c(0, 25, 40, 60), bottom_cm = c(25, 40, 60, 120),
                  designation = c("A", "Bw1", "Bw2", "C"), clay_pct = 50,
                  sand_pct = 25, silt_pct = 25, caco3_pct = c(5, 5, NA, 5),
                  ph_cacl2 = 7.5)
  for (k in setdiff(names(.ana), "caco3_pct")) h[[k]] <- .ana[[k]]
  expect_true(.und(.rx(.fp(h))))
  # one part without carbonates: not calcareous (nonacid)
  h$caco3_pct <- c(5, 5, 0, 5)
  expect_equal(.rx(.fp(h))$value, "nonacid")
})

test_that("reaction: acid and nonacid by pH in CaCl2 throughout", {
  skip_on_cran()
  expect_equal(.rx(.fine(ph_cacl2 = 4.6))$value, "acid")
  expect_equal(.rx(.fine(ph_cacl2 = c(4.5, 4.6, 5.2)))$value, "nonacid")
  u <- .rx(.fine(ph_cacl2 = c(4.5, 4.6, NA)))
  expect_null(u$value); expect_true("ph_cacl2" %in% u$missing)
  # pH in water is not read (the CaCl2 criterion)
  w <- .rx(.fine(ph_h2o = 4.8))
  expect_true(.und(w))
})

test_that("reaction: the taxa that use it and the exclusions", {
  skip_on_cran()
  p <- .fine(caco3_pct = 5, ph_cacl2 = 7.5)
  # Aquepts, Aquands, Gelic taxa: used
  expect_equal(.rx(p, "IN", "Endoaquepts", "Typic Endoaquepts")$value, "calcareous")
  expect_equal(.rx(p, "IN", "Haplogelepts", "Typic Haplogelepts")$value, "calcareous")
  # other Inceptisols, Mollisols: not used
  for (a in list(.rx(p, "IN", "Eutrudepts", "Typic Eutrudepts"),
                 .rx(p, "MO", "Hapludolls", "Typic Hapludolls"),
                 .rx(p, "IN", "Fragiaquepts", "Typic Fragiaquepts"),
                 .rx(p, "AD", "Duraquands", "Typic Duraquands"),
                 .rx(p, "MO", "Argiaquolls", "Typic Argiaquolls"),
                 .rx(p, "GE", "Fibristels", "Typic Fibristels"))) {
    expect_null(a$value); expect_false(.und(a))
  }
  # Aquolls take calcareous only
  expect_equal(.rx(p, "MO", "Endoaquolls", "Typic Endoaquolls")$value, "calcareous")
  na <- .rx(.fine(ph_cacl2 = 4.6), "MO", "Endoaquolls", "Typic Endoaquolls")
  expect_null(na$value); expect_false(.und(na))
  # sandy family: not used
  ps <- FamilyAttribute$new("particle_size", "sandy",
                            evidence = list(control_section = list(top = 25, bottom = 100)))
  s <- .rx(p, particle_size = ps, mineralogy = FamilyAttribute$new("mineralogy", "mixed"))
  expect_null(s$value); expect_false(.und(s))
  # carbonatic mineralogy: not used
  cb <- .rx(p, mineralogy = FamilyAttribute$new("mineralogy", "carbonatic"))
  expect_null(cb$value); expect_false(.und(cb))
  # an undecided mineralogy that may be anhydritic: undecided
  un <- .rx(.fine(caco3_pct = 5, ph_cacl2 = 7.5, mineralogy_analysed = NA))
  expect_true(.und(un))
})

test_that("reaction: the calcareous control sections (p. 368)", {
  skip_on_cran()
  # a root-limiting layer at 20 cm: the 2.5 cm above it
  h <- data.frame(top_cm = c(0, 15, 20), bottom_cm = c(15, 20, 40),
                  designation = c("A", "C", "R"), clay_pct = c(30, 30, NA),
                  sand_pct = c(30, 30, NA), silt_pct = c(40, 40, NA),
                  caco3_pct = c(0, 5, NA), ph_cacl2 = c(6, 7.5, NA))
  for (k in setdiff(names(.ana), "caco3_pct")) h[[k]] <- .ana[[k]]
  r <- .rx(.fp(h), sg = "Lithic Udorthents")
  expect_equal(r$evidence$calcareous_control_section, c(17.5, 20))
  expect_equal(r$value, "calcareous")
  # Gelic taxa: the surface to 25 cm
  g <- .rx(.fine(caco3_pct = c(5, 0, 0), ph_cacl2 = 7.5), "IN", "Haplogelepts",
           "Typic Haplogelepts")
  expect_equal(g$evidence$calcareous_control_section, c(0, 25))
  expect_equal(g$value, "calcareous")
})

test_that("reaction: allic only in Oxisols (key A)", {
  skip_on_cran()
  ox <- function(al) .rx(.fine(al_kcl_cmol = al), "OX", "Hapludox", "Typic Hapludox")
  expect_equal(ox(c(1, 3, 3))$value, "allic")                  # 25-100 cm
  n <- ox(c(3, 1, 1))
  expect_null(n$value); expect_false(.und(n))
  u <- ox(c(1, NA, 1))                                         # 35 cm unknown
  expect_true(.und(u)); expect_true("al_kcl_cmol" %in% u$missing)
  # a 20 cm layer is too thin
  h <- data.frame(top_cm = c(0, 25, 45), bottom_cm = c(25, 45, 120),
                  designation = c("A", "Bo1", "Bo2"), clay_pct = 50, sand_pct = 25,
                  silt_pct = 25, al_kcl_cmol = c(1, 3, 1))
  expect_null(.rx(.fp(h), "OX", "Hapludox", "Typic Hapludox")$value)
})


# ---- soil temperature classes (pp. 368-369) ---------------------------------

test_that("temperature: the recorded regime; frigid omitted in cryic taxa", {
  skip_on_cran()
  t <- function(reg, ord, gg, sg = NULL, mast = NULL)
    family_temperature_regime_usda(.fine(site = list(id = "t", soil_temperature_regime = reg,
                                                     mean_annual_soil_temp_C = mast)),
                                   order_code = ord, great_group = gg, subgroup = sg)
  expect_equal(t("mesic", "IN", "Dystrudepts")$value, "mesic")
  expect_equal(t("Isohyperthermic", "OX", "Hapludox")$value, "isohyperthermic")
  fr <- t("frigid", "IN", "Haplocryepts")
  expect_null(fr$value); expect_false(.und(fr))
  cr <- t("cryic", "SP", "Haplocryods")
  expect_null(cr$value); expect_false(.und(cr))
  # no cryic class: a cryic regime outside a cryic taxon is undecided
  expect_true(.und(t("cryic", "UT", "Paleudults")))
  expect_equal(t("frigid", "IN", "Dystrudepts")$value, "frigid")
  # not recorded: undecided
  expect_true(.und(t(NULL, "IN", "Dystrudepts")))
})

test_that("temperature: gelic classes by the mean annual soil temperature (F14)", {
  skip_on_cran()
  g <- function(mast, gg = "Haplorthels", ord = "GE", reg = "gelic")
    family_temperature_regime_usda(.fine(site = list(id = "g", soil_temperature_regime = reg,
                                                     mean_annual_soil_temp_C = mast)),
                                   order_code = ord, great_group = gg)
  expect_equal(g(-12)$value, "hypergelic")
  expect_equal(g(-10)$value, "hypergelic")
  expect_equal(g(-6)$value, "pergelic")
  expect_equal(g(-2)$value, "subgelic")
  expect_equal(g(-2, "Haplogelepts", "IN")$value, "subgelic")
  u <- g(NULL)
  expect_null(u$value); expect_true(.und(u))
  expect_true("site$mean_annual_soil_temp_C" %in% u$missing)
})


# ---- soil depth classes (p. 369) --------------------------------------------

test_that("depth: shallow only; Oxisols at 100 cm, others at 50 cm", {
  skip_on_cran()
  dp <- function(rl, ord = "EN", gg = "Udorthents", sg = "Typic Udorthents",
                 des = "R", extra = list()) {
    h <- data.frame(top_cm = c(0, rl), bottom_cm = c(rl, rl + 20),
                    designation = c("A", des))
    for (k in names(extra)) h[[k]] <- extra[[k]]
    family_depth_class_usda(.fp(h), ord, gg, sg)
  }
  expect_equal(dp(20)$value, "shallow")          # no "very-shallow"
  expect_equal(dp(45)$value, "shallow")
  expect_null(dp(50)$value)
  expect_equal(dp(80, "OX", "Hapludox", "Typic Hapludox")$value, "shallow")
  expect_null(dp(100, "OX", "Hapludox", "Typic Hapludox")$value)
  # not in Lithic subgroups
  li <- dp(30, sg = "Lithic Udorthents")
  expect_null(li$value); expect_false(.und(li))
  # not with a fragipan
  fp <- dp(40, "AF", "Fragiudalfs", "Typic Fragiudalfs", des = "Btx")
  expect_null(fp$value); expect_match(fp$evidence$reason, "fragipan")
  # no root-limiting layer recorded: no class, not undecided (F15)
  nr <- family_depth_class_usda(.fine(), "IN", "Dystrudepts", "Typic Dystrudepts")
  expect_null(nr$value); expect_false(.und(nr))
})

test_that("depth: ortstein is root-limiting only when continuous (F15)", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 10, 20, 35), bottom_cm = c(10, 20, 35, 100),
                  designation = c("A", "E", "Bhsm", "C"),
                  clay_pct = 5, sand_pct = 90, silt_pct = 5)
  d <- family_depth_class_usda(.fp(h), "SP", "Haplorthods", "Typic Haplorthods")
  expect_null(d$value)
  h$horizon_continuity <- "continuous"
  d2 <- family_depth_class_usda(.fp(h), "SP", "Haplorthods", "Typic Haplorthods")
  expect_equal(d2$value, "shallow")
})


# ---- rupture-resistance, coatings, cracks (pp. 369-370) ---------------------

test_that("rupture-resistance: ortstein in Spodosols only", {
  skip_on_cran()
  h <- data.frame(top_cm = c(0, 10, 20, 35), bottom_cm = c(10, 20, 35, 100),
                  designation = c("A", "E", "Bhsm", "C"),
                  clay_pct = 5, sand_pct = 90, silt_pct = 5)
  expect_equal(family_rupture_resistance_usda(.fp(h), "SP", "Haplorthods",
                                              "Typic Haplorthods")$value, "ortstein")
  expect_null(family_rupture_resistance_usda(.fp(h), "IN", "Dystrudepts",
                                             "Typic Dystrudepts")$value)
  h$designation[3] <- "Bhs"                       # not cemented: absent
  n <- family_rupture_resistance_usda(.fp(h), "SP", "Haplorthods", "Typic Haplorthods")
  expect_null(n$value); expect_false(.und(n))
  h$pan_type <- c(NA, NA, "ortstein", NA)         # recorded as ortstein
  expect_equal(family_rupture_resistance_usda(.fp(h), "SP", "Haplorthods",
                                              "Typic Haplorthods")$value, "ortstein")
})

test_that("coatings: Quartzipsamments only, silt + 2 x clay over 5", {
  skip_on_cran()
  qz <- function(silt, clay) {
    h <- data.frame(top_cm = c(0, 25), bottom_cm = c(25, 150),
                    designation = c("A", "C"), clay_pct = clay, silt_pct = silt,
                    sand_pct = 100 - silt - clay, very_fine_sand_pct = 5)
    family_coatings_usda(.fp(h), "EN", "Quartzipsamments", "Typic Quartzipsamments")
  }
  expect_equal(qz(3, 2)$value, "coated")          # 7
  expect_equal(qz(2, 1)$value, "uncoated")        # 4
  expect_equal(qz(3, 1)$value, "uncoated")        # 5: not more than 5
  u <- qz(NA, NA)
  expect_null(u$value); expect_true(.und(u))
  h <- data.frame(top_cm = 0, bottom_cm = 150, clay_pct = 2, silt_pct = 3, sand_pct = 95)
  expect_null(family_coatings_usda(.fp(h), "EN", "Udipsamments", "Typic Udipsamments")$value)
})

test_that("cracks: Fluvaquents and Humaquepts; permanence a field record", {
  skip_on_cran()
  cr <- function(perm, wd = 0.5, sp = 30, gg = "Fluvaquents", ord = "EN") {
    h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                    designation = c("Ap", "Bg1", "Bg2"), clay_pct = 40,
                    cracks_permanent = perm, cracks_width_cm = wd,
                    polygonal_cracks_spacing_cm = sp)
    family_cracks_usda(.fp(h), ord, gg, paste("Typic", gg))
  }
  expect_equal(cr(TRUE)$value, "cracked")
  expect_equal(cr(TRUE, gg = "Humaquepts", ord = "IN")$value, "cracked")
  n <- cr(NA)                                     # permanence not recorded
  expect_null(n$value); expect_false(.und(n))
  expect_null(cr(TRUE, wd = 0.1)$value)           # narrower than 2 mm
  expect_null(cr(TRUE, sp = 60)$value)            # spaced 50 cm or more
  u <- cr(TRUE, wd = NA)
  expect_true(.und(u))
  expect_null(cr(TRUE, gg = "Udifluvents")$value)
})


# ---- the family in the name (p. 355; reading F20) ---------------------------

test_that("family label: the ten classes in the order of p. 355", {
  skip_on_cran()
  f <- function(n, v) FamilyAttribute$new(n, v)
  fam <- list(cracks = f("cracks", "cracked"), coatings = f("coatings", "coated"),
              rupture_resistance = f("rupture_resistance", "ortstein"),
              depth_class = f("depth_class", "shallow"),
              temperature_regime = f("temperature_regime", "mesic"),
              reaction = f("reaction", "acid"),
              cec_activity = f("cec_activity", "active"),
              mineralogy = f("mineralogy", "mixed"),
              human_material = f("human_material", "spolic"),
              particle_size = f("particle_size", "sandy"))
  expect_equal(family_label_usda(fam), paste(
    "sandy, spolic, mixed, active, acid, mesic, shallow, ortstein, coated, cracked"))
})

test_that("classify_usda: undecided terms join $ambiguities and cap the grade (F20)", {
  skip_on_cran()
  p <- make_andosol_canonical()
  base <- suppressWarnings(classify_usda(p))
  fam <- suppressWarnings(classify_usda(p, include_family = TRUE))
  expect_equal(base$evidence_grade, "A")
  expect_equal(fam$evidence_grade, "C")
  lv <- vapply(fam$ambiguities, function(a) a$level %||% "", "")
  expect_true("family" %in% lv)
  codes <- vapply(fam$ambiguities[lv == "family"], function(a) a$rsg_code, "")
  expect_true(all(codes %in% names(fam$trace$family)))
  expect_true("cec_activity" %in% codes)
  # the name carries only the decided terms
  expect_equal(fam$name, "mesic Hydric Melanudands")
  # terms not used in the taxon are not undecided: the grade is kept
  h <- suppressWarnings(classify_usda(make_histosol_canonical(), include_family = TRUE))
  expect_equal(h$evidence_grade, "A")
  expect_false("family" %in% vapply(h$ambiguities, function(a) a$level %||% "", ""))
})

test_that("classify_usda: the family enters the name only below a subgroup (F20)", {
  skip_on_cran()
  p <- make_acrisol_canonical()
  p$site$soil_moisture_regime <- NULL            # Ultisols need a regime
  r <- suppressWarnings(classify_usda(p, include_family = TRUE))
  expect_null(r$trace$subgroup_assigned)
  expect_false(grepl(",", r$name))
  expect_false("family" %in% vapply(r$ambiguities, function(a) a$level %||% "", ""))
})
