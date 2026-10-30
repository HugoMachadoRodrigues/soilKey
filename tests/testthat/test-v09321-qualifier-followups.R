# v0.9.321: WRB 2022 follow-ups read from the text: Rendzic (Ch 5, p. 145),
# calcaric material and Endocalcaric (Ch 3.3.3, p. 86; Ch 5, p. 129), and the
# texture qualifiers of one slash group used together with specifiers (Ch 2.1,
# p. 24-25; Ch 2.3.1, p. 27). Small made-up pedons; no third-party data.

.fu_ped <- function(..., site = list(id = "made-up")) {
  hz <- data.frame(..., stringsAsFactors = FALSE)
  PedonRecord$new(site = site, horizons = hz)
}

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
