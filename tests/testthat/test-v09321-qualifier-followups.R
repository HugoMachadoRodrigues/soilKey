# v0.9.321: WRB 2022 follow-ups read from the text: Rendzic (Ch 5, p. 145),
# calcaric material and Endocalcaric (Ch 3.3.3, p. 86; Ch 5, p. 129), and the
# texture qualifiers of one slash group used together with specifiers (Ch 2.1,
# p. 24-25; Ch 2.3.1, p. 27). Small made-up pedons; no third-party data.

.fu_ped <- function(..., site = list(id = "made-up")) {
  hz <- data.frame(..., stringsAsFactors = FALSE)
  PedonRecord$new(site = site, horizons = hz)
}

# A mollic horizon 0-30 cm (two layers) over a third layer; `caco3`, `eff`,
# `desg3` set the layers' carbonates and the designation of the third.
.fu_mollic <- function(caco3 = c(10, 12, NA), eff = NA_character_, desg3 = "Ck",
                       site = list(id = "made-up"), n = 3) {
  i <- seq_len(n)
  .fu_ped(designation = c("Ah1", "Ah2", desg3, "C2")[i],
          top_cm = c(0, 15, 30, 60)[i], bottom_cm = c(15, 30, 60, 90)[i],
          caco3_pct = caco3[i], effervescence_class = eff,
          oc_pct = c(3, 2, 0.5, 0.2)[i], clay_pct = 25, silt_pct = 40, sand_pct = 35,
          munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 5, 6)[i],
          munsell_chroma_moist = c(1, 2, 4, 4)[i], munsell_value_dry = c(3, 3, 6, 7)[i],
          bs_pct = 90, ph_h2o = 7.8, structure_grade = "moderate",
          structure_type = "granular", site = site)
}


# ---- Rendzic (Ch 5, p. 145) ---------------------------------------------------

test_that("v0.9.321 Rendzic: a mollic horizon containing or directly over >= 40% CCE", {
  skip_on_cran()
  over <- .fu_mollic(caco3 = c(10, 12, 55))
  expect_true(isTRUE(mollic(over)$passed))
  expect_true(isTRUE(qual_rendzic(over)$passed))
  expect_equal(qual_rendzic(over)$layers, 1:2)
  expect_true(isTRUE(qual_rendzic(.fu_mollic(caco3 = c(45, 12, 20)))$passed))  # contains
  # 2 to < 40% CCE is calcaric material but not enough (was TRUE: any >= 2%)
  expect_false(qual_rendzic(.fu_mollic(caco3 = c(10, 12, 20)))$passed)
  # >= 40% deeper down, not directly under the mollic horizon
  deep <- .fu_mollic(caco3 = c(10, 12, 5, 60), desg3 = "Bw", n = 4)
  expect_false(qual_rendzic(deep)$passed)
})

test_that("v0.9.321 Rendzic: undecided without the CCE, not when the HCl reaction is not visible", {
  skip_on_cran()
  # strong effervescence: calcaric material, but how much carbonate is unknown
  r <- qual_rendzic(.fu_mollic(caco3 = c(10, 12, NA), eff = c(NA, NA, "strong")))
  expect_true(is.na(r$passed))
  # slight effervescence is not calcaric material (Ch 3.3.3, p. 86)
  expect_false(qual_rendzic(.fu_mollic(caco3 = c(10, 12, NA), eff = c(NA, NA, "slight")))$passed)
  # nothing described below the mollic horizon
  expect_true(is.na(qual_rendzic(.fu_mollic(caco3 = c(10, 12), n = 2))$passed))
  # no mollic horizon
  nom <- .fu_mollic(caco3 = c(10, 12, 55))
  nom$horizons$oc_pct <- 0.2
  expect_false(isTRUE(mollic(nom)$passed))
  expect_false(isTRUE(qual_rendzic(nom)$passed))
})

test_that("v0.9.321 Rendzic: calcareous rock read from its CCE or the rock named", {
  skip_on_cran()
  lime <- .fu_mollic(caco3 = c(10, 12, NA), desg3 = "R",
                     site = list(id = "made-up", parent_material = "Jurassic limestone"))
  expect_true(isTRUE(qual_rendzic(lime)$passed))
  # a rock with no CCE and no carbonate rock named is undecided
  rock <- .fu_mollic(caco3 = c(10, 12, NA), desg3 = "R",
                     site = list(id = "made-up", parent_material = "granite"))
  expect_true(is.na(qual_rendzic(rock)$passed))
  # a measured CCE decides for the rock too
  marl <- .fu_mollic(caco3 = c(10, 12, 30), desg3 = "R",
                     site = list(id = "made-up", parent_material = "limestone"))
  expect_false(qual_rendzic(marl)$passed)
})


# ---- calcaric material and Endocalcaric (Ch 3.3.3, p. 86; Ch 5, p. 129) ---------

test_that("v0.9.321 calcaric material reads the HCl reaction where CaCO3 is not measured", {
  skip_on_cran()
  mk <- function(caco3, eff)
    .fu_ped(designation = c("A", "B", "C"), top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 110),
            caco3_pct = caco3, effervescence_class = eff, clay_pct = 20, silt_pct = 40,
            sand_pct = 40)
  r <- calcaric_material(mk(c(NA, NA, NA), c("none", "slight", "moderate")))
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3L)
  expect_false(calcaric_material(mk(c(NA, NA, NA), c("none", "slight", "none")))$passed)
  expect_true(is.na(calcaric_material(mk(c(NA, NA, NA), c("none", NA, "none")))$passed))
  # a measured CCE decides over the field reaction
  expect_false(calcaric_material(mk(c(1, 1, 1), c("strong", "strong", "strong")))$passed)
  expect_true(isTRUE(calcaric_material(mk(c(0, 0, 3), NA))$passed))
})

test_that("v0.9.321 Endocalcaric: the HCl reaction, depths from the mineral soil surface", {
  skip_on_cran()
  mk <- function(caco3, eff, top = c(0, 30, 60), bottom = c(30, 60, 110),
                 desg = c("A", "B", "C"), oc = 1)
    .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, caco3_pct = caco3,
            effervescence_class = eff, oc_pct = oc, clay_pct = 20, silt_pct = 40,
            sand_pct = 40)
  # CaCO3 measured above (no calcic horizon), the HCl reaction below
  p <- mk(c(0, 0, NA), c(NA, NA, "strong"))
  expect_false(isTRUE(calcic(p)$passed))
  expect_true(isTRUE(qual_endocalcaric(p)$passed))           # was FALSE: CaCO3 only
  expect_false(qual_endocalcaric(mk(c(0, 0, NA), c(NA, NA, "slight")))$passed)
  # a layer that might be calcaric from 30 cm leaves it undecided
  expect_true(is.na(qual_endocalcaric(mk(c(0, NA, NA), c(NA, NA, "strong")))$passed))
  # no deep layer known, one might exist: undecided, not absent
  expect_true(is.na(qual_endocalcaric(mk(c(0, 0, NA), c(NA, NA, NA)))$passed))
  # 20 cm of organic material on top: 60-110 cm starts 40 cm below the mineral
  # soil surface, so not Endo- (was TRUE, counted from 0 cm)
  org <- mk(c(NA, 0, NA), c(NA, NA, "strong"), top = c(0, 20, 60), bottom = c(20, 60, 110),
            desg = c("Oa", "A", "C"), oc = c(30, 1, 1))
  expect_false(qual_endocalcaric(org)$passed)
})
