# v0.9.418: Spodosols, Histosols and Gelisols after the Soil Taxonomy audit
# (section 2.2, items S1 to S13; fix A13; readings L5 to L12).

.pd418 <- function(hz, site = list()) {
  site <- utils::modifyList(list(id = "t", lat = 55, lon = 12, country = "DK",
                                 parent_material = "sand"), site)
  PedonRecord$new(site = site,
                  horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}

# A podzol: O, albic E, spodic Bs, C
.podzol418 <- function(bs_hue = "7.5YR", bs_value = 3, bs_chroma = 4, e_dry = 7,
                       bs_odoe = NA_real_, e_odoe = NA_real_, alox = c(0.02, 0.02, 0.8, 0.1),
                       feox = c(0.02, 0.02, 0.05, 0.02), ...) {
  hz <- list(
    top_cm = c(0, 5, 30, 60), bottom_cm = c(5, 30, 60, 150),
    designation = c("Oa", "E", "Bs", "C"),
    munsell_hue_moist = c("10YR", "10YR", bs_hue, "10YR"),
    munsell_value_moist = c(2, 5, bs_value, 5),
    munsell_chroma_moist = c(1, 2, bs_chroma, 3),
    munsell_value_dry = c(3, e_dry, bs_value + 1, 6),
    ph_h2o = c(4.0, 4.2, 4.6, 5.0), oc_pct = c(30, 0.4, 1.5, 0.2),
    clay_pct = c(NA, 3, 5, 3), silt_pct = c(NA, 5, 8, 5), sand_pct = c(NA, 92, 87, 92),
    al_ox_pct = alox, fe_ox_pct = feox, odoe = c(NA, e_odoe, bs_odoe, NA))
  extra <- list(...)
  for (k in names(extra)) hz[[k]] <- extra[[k]]
  .pd418(hz)
}

test_that("spodic horizon: KST colours, albic path and undecided colours (S1)", {
  skip_on_cran()
  sp <- spodic_horizon_usda(.podzol418())
  expect_true(isTRUE(sp$passed))
  expect_equal(sp$layers, 3L)
  # 10YR 4/4 is no spodic colour (NL033's 10YR 4.5/4)
  expect_false(isTRUE(spodic_horizon_usda(.podzol418(bs_hue = "10YR", bs_value = 4))$passed))
  expect_true(isFALSE(spodic_horizon_usda(.podzol418(bs_hue = "10YR", bs_value = 4))$passed))
  # moist hue not recorded: undecided
  sp_na <- spodic_horizon_usda(.podzol418(bs_hue = NA_character_))
  expect_true(is.na(sp_na$passed))
  expect_true("munsell_hue_moist" %in% sp_na$missing)
  # 7.5YR 4/6 needs item b: the oxalate extract decides
  expect_true(isTRUE(spodic_horizon_usda(.podzol418(bs_value = 4, bs_chroma = 6))$passed))
  expect_false(isTRUE(spodic_horizon_usda(.podzol418(bs_value = 4, bs_chroma = 6,
                                                     alox = c(0.02, 0.02, 0.2, 0.1)))$passed))
})

test_that("spodic horizon: ODOE not measured is undecided (L5)", {
  skip_on_cran()
  # no albic horizon (E dry value 4), 7.5YR 4/6, oxalate too low: ODOE decides
  base <- function(...) .podzol418(e_dry = 4, bs_value = 4, bs_chroma = 6,
                                   alox = c(0.02, 0.02, 0.2, 0.1), ...)
  expect_true(is.na(spodic_horizon_usda(base())$passed))
  expect_true(isTRUE(spodic_horizon_usda(base(bs_odoe = 0.4, e_odoe = 0.05))$passed))
  expect_true(isFALSE(spodic_horizon_usda(base(bs_odoe = 0.1, e_odoe = 0.05))$passed))
})

test_that("albic horizon: a dry value below 5 rules it out without the moist colour (L8)", {
  skip_on_cran()
  pd <- .podzol418(e_dry = 4, munsell_value_moist = c(2, NA, 3, 5))
  alb <- .usda_albic_status(pd)
  expect_true(isFALSE(alb$horizon[2]))
  pd2 <- .podzol418(e_dry = 6, munsell_value_moist = c(2, NA, 3, 5))
  expect_true(is.na(.usda_albic_status(pd2)$horizon[2]))
})

test_that("Spodosols order: items C1 to C3 and the exclusions (S2)", {
  skip_on_cran()
  expect_true(isTRUE(spodosol_usda(.podzol418())$passed))
  # a spodic horizon starting at 210 cm in a loamy profile fails item b
  hz <- list(top_cm = c(0, 30, 210), bottom_cm = c(30, 210, 240),
             designation = c("A", "Bw", "Bs"),
             munsell_hue_moist = c("10YR", "10YR", "5YR"),
             munsell_value_moist = c(3, 4, 3), munsell_chroma_moist = c(2, 4, 4),
             ph_h2o = c(5, 5, 5), oc_pct = c(2, 0.5, 1),
             clay_pct = c(20, 25, 20), silt_pct = c(40, 40, 40),
             al_ox_pct = c(0.1, 0.1, 1), fe_ox_pct = c(0.1, 0.1, 0.5))
  expect_true(isFALSE(spodosol_usda(.pd418(hz))$passed))
  # a plaggen epipedon (artefacts, dark, 60 cm) excludes the order
  pl <- .podzol418()
  h <- pl$horizons
  h <- rbind(data.table::data.table(top_cm = 0, bottom_cm = 60, designation = "Ap",
                                    munsell_hue_moist = "10YR", munsell_value_moist = 2,
                                    munsell_chroma_moist = 1, munsell_value_dry = 4,
                                    oc_pct = 3, artefacts_pct = 2, ph_h2o = 5),
             within(as.data.frame(h[2:4, ]), { top_cm <- top_cm + 55; bottom_cm <- bottom_cm + 55 }),
             fill = TRUE)
  expect_true(isFALSE(spodosol_usda(.pd418(as.list(h)))$passed))
})

test_that("Spodosols order: no albic and no andic data leave item C.3.d undecided (L7)", {
  skip_on_cran()
  # no albic horizon (E dry value 4); spodic by oxalate; oxalate of the B
  # high enough that andic properties are possible
  pd <- .podzol418(e_dry = 4, alox = c(0.02, 0.02, 1.2, 0.8), feox = c(0.02, 0.02, 0.3, 0.3))
  expect_true(isTRUE(spodic_horizon_usda(pd)$passed))
  # Al + 1/2 Fe under 2 %: the Andisol test of v0.9.417 rules andic out, so
  # C.3.d(2) holds and the order is decided
  expect_true(isTRUE(spodosol_usda(pd)$passed))
  # where the Andisol test is undecided, the bound decides only when the
  # layers that cannot be andic fill 60 % of the window (reading L7)
  pd <- .podzol418(e_dry = 4, alox = c(0.02, 0.02, 2.4, 0.8), feox = c(0.02, 0.02, 0.3, 0.3))
  expect_true(is.na(soilKey:::.spodosol_no_andic_bound(pd, 5)))
})

test_that("Alorthods read oxalate iron alone, Alaquods also Al >= 3 Fe (S3)", {
  skip_on_cran()
  pd <- .podzol418(alox = c(0.02, 0.02, 1.5, 0.1), feox = c(0.02, 0.02, 0.3, 0.02))
  expect_true(isTRUE(al_rich_spodic_usda(pd)$passed))
  expect_true(isFALSE(al_rich_spodic_usda(pd, fe_only = TRUE)$passed))
  pd2 <- .podzol418(feox = c(0.02, 0.02, 0.05, 0.02))
  expect_true(isTRUE(al_rich_spodic_usda(pd2, fe_only = TRUE)$passed))
  # oxalate iron not recorded: undecided
  pd3 <- .podzol418(feox = c(0.02, 0.02, NA, 0.02))
  expect_true(is.na(al_rich_spodic_usda(pd3, fe_only = TRUE)$passed))
})

test_that("Entic subgroups follow the four KST definitions (S4)", {
  skip_on_cran()
  pd <- .podzol418()                     # sandy, 7.5YR 3/4: chroma 4 at the top
  expect_true(isTRUE(entic_subgroup_usda(pd, rule = "orthod")$passed))
  pd2 <- .podzol418(bs_chroma = 3)       # 7.5YR 3/3: neither value nor chroma 4
  expect_true(isFALSE(entic_subgroup_usda(pd2, rule = "orthod")$passed))
  expect_true(isFALSE(entic_subgroup_usda(pd, rule = "haplocryod")$passed))   # 1.5 % OC
  expect_true(isTRUE(entic_subgroup_usda(pd, rule = "alorthod")$passed))      # < 3 % OC
  expect_true(isFALSE(entic_subgroup_usda(pd, rule = "cryaquod")$passed))     # 30 cm thick
})

test_that("Aquods need aquic conditions in the albic or spodic horizon, or a histic epipedon (S5)", {
  skip_on_cran()
  wet <- .podzol418(redoximorphic_features_pct = c(0, 10, 10, 0),
                    designation = c("Oa", "Eg", "Bs", "C"))
  expect_true(isTRUE(aquod_qualifying_usda(wet)$passed))
  dry <- .podzol418(redoximorphic_features_pct = c(0, 0, 0, 0))
  expect_true(isFALSE(aquod_qualifying_usda(dry)$passed))
})

test_that("placic horizon: 1 mm or more, under 25 mm only next to spodic materials (S6)", {
  skip_on_cran()
  mk <- function(thk, spodic_above = TRUE) {
    hz <- list(top_cm = c(0, 20, 50, 50 + thk), bottom_cm = c(20, 50, 50 + thk, 120),
               designation = c("E", "Bs", "Bsm", "C"),
               munsell_hue_moist = c("10YR", if (spodic_above) "5YR" else "10YR",
                                     if (spodic_above) "5YR" else "10YR", "10YR"),
               munsell_value_moist = c(6, 3, if (spodic_above) 2 else 4, 5),
               munsell_chroma_moist = c(2, 4, if (spodic_above) 2 else 3, 4),
               munsell_value_dry = c(7, 4, 3, 6),
               ph_h2o = c(4.5, 4.8, 4.8, 5.5),
               oc_pct = c(0.4, if (spodic_above) 1.2 else 0.3, if (spodic_above) 1 else 0.3, 0.2),
               cementation_class = c(NA, NA, "strongly", NA))
    .pd418(hz)
  }
  expect_true(isTRUE(placic_horizon_usda(mk(1))$passed))
  expect_true(isFALSE(placic_horizon_usda(mk(5))$passed))
  expect_true(isTRUE(placic_horizon_usda(mk(5, spodic_above = FALSE))$passed))
})

test_that("Dur- great groups read cementation in 90 % of each pedon laterally (L6, L9)", {
  skip_on_cran()
  mk <- function(desg, cem, share = NA_real_) {
    .podzol418(designation = c("Oa", "E", desg, "C"),
               cementation_class = c(NA, NA, cem, NA), duripan_pct = c(NA, NA, share, NA))
  }
  expect_true(isTRUE(cemented_horizon_usda(mk("Bsm", "weakly"))$passed))
  expect_true(is.na(cemented_horizon_usda(mk("Bs", "weakly"))$passed))
  expect_true(isFALSE(cemented_horizon_usda(mk("Bs", "strongly", 70))$passed))
  expect_true(isFALSE(cemented_horizon_usda(mk("Bs", NA))$passed))
})

test_that("Spodic subgroups read thickness, depth, cementation and ODOE (S8)", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 10, 25), bottom_cm = c(10, 25, 100),
             designation = c("Ap", "Bw", "C"),
             al_ox_pct = c(0.05, 0.4, 0.1), fe_ox_pct = c(0.02, 0.1, 0.02),
             odoe = c(0.05, 0.05, 0.02))
  pd <- .pd418(hz)
  expect_true(isTRUE(spodic_subgroup_usda(pd)$passed))
  # the Psamments ask for 18 cm below the mineral surface and below the Ap
  expect_true(isFALSE(spodic_subgroup_usda(pd, psamments = TRUE)$passed))
  # without oxalate data the ODOE and the oxalate items are undecided
  hz$al_ox_pct <- NULL; hz$fe_ox_pct <- NULL; hz$odoe <- NULL
  expect_true(is.na(spodic_subgroup_usda(.pd418(hz))$passed))
})

test_that("Gelisols: gelic materials within 100 cm and permafrost within 200 cm (S9, S10)", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 30, 150), bottom_cm = c(30, 150, 180),
             designation = c("A", "Bgjj", "Cf"))
  pd <- .pd418(hz, list(country = "RU"))
  expect_true(isFALSE(permafrost_within_usda(pd, 100)$passed))
  expect_true(isTRUE(permafrost_within_usda(pd, 200)$passed))   # the suffix f (S10)
  expect_true(isTRUE(gelisol_usda(pd)$passed))
  hz$designation <- c("A", "Bg", "Cf")
  expect_true(isFALSE(gelisol_usda(.pd418(hz, list(country = "RU")))$passed))
})

test_that("Histosols: organic soil material by OC or O/H designation, items a-d (S11, L10, L11)", {
  skip_on_cran()
  # 2O1 and 4O1 count (the old regex missed them)
  # (oxalate recorded so that the andic exclusion of item B.1 is decided)
  hz <- list(top_cm = c(0, 45, 60), bottom_cm = c(45, 60, 140),
             designation = c("2O1", "3C", "4O1"), oc_pct = c(NA, 1, NA),
             al_ox_pct = c(0.1, 0.1, 0.1), fe_ox_pct = c(0.1, 0.1, 0.1))
  expect_true(isTRUE(histosol_qualifying_usda(.pd418(hz))$passed))
  # a measured organic carbon under 12 % overrides the O designation
  hz$oc_pct <- c(8, 1, NA)
  expect_true(isFALSE(histosol_qualifying_usda(.pd418(hz))$passed))
  # item d: organic from 20 cm, 50 cm within the control section, saturated
  hz2 <- list(top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 150),
              designation = c("A", "Oa", "C"), oc_pct = c(3, 30, 0.5),
              water_saturation_days = c(60, 200, 200))
  expect_true(isTRUE(histosol_qualifying_usda(.pd418(hz2))$passed))
  hz2$water_saturation_days <- NULL
  expect_true(is.na(histosol_qualifying_usda(.pd418(hz2))$passed))
})

test_that("Folists need fewer than 30 days of saturation and no drainage (S12)", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 60), bottom_cm = c(60, 140), designation = c("Oa", "Oe"),
             oc_pct = c(40, 35))
  expect_true(is.na(folist_qualifying_usda(.pd418(hz))$passed))
  hz$water_saturation_days <- c(5, 10)
  expect_true(isTRUE(folist_qualifying_usda(.pd418(hz))$passed))
  expect_true(isFALSE(folist_qualifying_usda(.pd418(hz, list(artificially_drained = TRUE)))$passed))
})

test_that("Fibrists and Saprists read the subsurface tier with the KST fibre classes (S13)", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 30, 90), bottom_cm = c(30, 90, 140),
             designation = c("Oa", "Oi", "Oe"), oc_pct = c(40, 40, 40),
             fiber_content_rubbed_pct = c(10, 80, 50))
  pd <- .pd418(hz)
  expect_true(isTRUE(fibric_predominant_usda(pd)$passed))
  expect_true(isFALSE(sapric_predominant_usda(pd)$passed))
  # 60 % fibre is hemic for the KST (fibric for the SiBCS classes)
  hz$fiber_content_rubbed_pct <- c(10, 60, 50)
  expect_true(isFALSE(fibric_predominant_usda(.pd418(hz))$passed))
  # kind not recorded and no suffix: undecided
  hz$fiber_content_rubbed_pct <- NULL
  hz$designation <- c("O", "O", "O")
  expect_true(is.na(fibric_predominant_usda(.pd418(hz))$passed))
})

test_that("histic epipedon: the Ap path needs saturation and mixing to 25 cm (A13)", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 100),
             designation = c("Ap", "Bw", "C"), oc_pct = c(11, 4, 0.5))
  pd <- .pd418(hz)
  # mixed to 25 cm: (11 * 15 + 4 * 10) / 25 = 8.2 %, saturation not recorded
  expect_true(is.na(histic_epipedon_usda(pd)$passed))
  hz$water_saturation_days <- c(40, 40, 40)
  expect_true(isTRUE(histic_epipedon_usda(.pd418(hz))$passed))
  hz$oc_pct <- c(9, 2, 0.5)                # mixed: 6.2 %
  expect_true(isFALSE(histic_epipedon_usda(.pd418(hz))$passed))
})

test_that("histic epipedon: 20 to 40 cm of organic soil material (60 for moss)", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 50), bottom_cm = c(50, 100), designation = c("H", "C"),
             oc_pct = c(30, 1))
  expect_true(isFALSE(histic_epipedon_usda(.pd418(hz))$passed))
  hz$bulk_density_g_cm3 <- c(0.08, 1.3)
  expect_true(isTRUE(histic_epipedon_usda(.pd418(hz))$passed))
})

test_that("drainage class is not evidence of aquic conditions (L12)", {
  skip_on_cran()
  hz <- list(top_cm = c(0, 60), bottom_cm = c(60, 140), designation = c("Oa", "Oe"),
             oc_pct = c(40, 35))
  pd <- .pd418(hz, list(drainage_class = "well drained"))
  expect_true(is.na(folist_qualifying_usda(pd)$passed))
})
