# v0.9.314: WRB 2022 readings for the weakly developed soils and the soils of
# carbonate and gypsum accumulation: the cambic horizon (Ch 3.1.5), the calcic
# horizon (Ch 3.1.4), fluvic material (Ch 3.3.6) and the Ch 4 entries of the
# Gypsisols, Calcisols, Cambisols, Fluvisols and Arenosols. Small made-up
# pedons; no third-party data.

.w14_ped <- function(..., site = list(id = "w14")) {
  hz <- ensure_horizon_schema(data.table::as.data.table(data.frame(..., stringsAsFactors = FALSE)))
  PedonRecord$new(site = site, horizons = hz)
}


# ---- cambic horizon ----------------------------------------------------------

test_that("v0.9.314 cambic: criterion 4 sets aside only the layers of the argic horizon", {
  skip_on_cran()
  # a structured BA above a Bt with a clay increase: the BA is not part of the
  # argic horizon, so it can be cambic; the Bt cannot.
  p <- .w14_ped(top_cm = c(0, 15, 45, 80), bottom_cm = c(15, 45, 80, 130),
                designation = c("A", "BA", "Bt", "C"),
                clay_pct = c(12, 14, 32, 30), silt_pct = c(30, 30, 25, 25),
                sand_pct = c(58, 56, 43, 45),
                structure_grade = c("weak", "moderate", "moderate", "weak"),
                structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"),
                clay_films_amount = c(NA, NA, "common", NA))
  expect_true(isTRUE(argic(p)$passed))
  cb <- cambic(p)
  expect_true(isTRUE(cb$passed))
  expect_equal(cb$layers, 2L)
  expect_true(3L %in% cb$evidence$not_part_of_other_horizon$details$excluded_layers)
  # the USDA reading sets the whole profile aside, as before
  expect_false(isTRUE(.cambic_impl(p)$passed))
})

test_that("v0.9.314 cambic: a subsurface layer starting above 5 cm counts; the surface layer and A horizons do not", {
  skip_on_cran()
  p <- .w14_ped(top_cm = c(0, 2, 75), bottom_cm = c(2, 75, 80), designation = c("Ah", "B", "R"),
                clay_pct = c(8, 9, NA), silt_pct = c(28, 19, NA), sand_pct = c(NA, NA, NA),
                structure_grade = c("weak", "weak", NA),
                structure_type = c("granular", "subangular blocky", NA))
  cb <- cambic(p)
  expect_true(isTRUE(cb$passed))       # silt + 2 x clay = 37: sandy loam or finer without the sand
  expect_equal(cb$layers, 2L)
  # min_top_cm, when given, is asked as before
  expect_false(isTRUE(cambic(p, min_top_cm = 5)$passed))
  # an A2 is an A horizon, not a subsurface horizon
  q <- .w14_ped(top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 90), designation = c("A1", "A2", "C"),
                clay_pct = c(15, 16, 15), silt_pct = c(30, 30, 30), sand_pct = c(55, 54, 55),
                structure_grade = c("moderate", "moderate", "structureless"),
                structure_type = c("granular", "subangular blocky", "massive"))
  expect_false(isTRUE(cambic(q)$passed))
})

test_that("v0.9.314 cambic: the 15 cm are those of the adjoining layers; undecided without texture", {
  skip_on_cran()
  mk <- function(bw2_bottom, clay = c(15, 18, 18, 15))
    .w14_ped(top_cm = c(0, 20, 30, bw2_bottom), bottom_cm = c(20, 30, bw2_bottom, 120),
             designation = c("A", "Bw1", "Bw2", "C"), clay_pct = clay,
             silt_pct = c(30, 30, 30, 30), sand_pct = 100 - clay - 30,
             structure_grade = c("weak", "weak", "weak", "structureless"),
             structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"))
  expect_true(isTRUE(cambic(mk(40))$passed))           # 10 + 10 cm
  expect_equal(cambic(mk(40))$layers, c(2L, 3L))
  one <- .w14_ped(top_cm = c(0, 20, 30), bottom_cm = c(20, 30, 120), designation = c("A", "Bw", "C"),
                  clay_pct = c(15, 18, 15), silt_pct = c(30, 30, 30), sand_pct = c(55, 52, 55),
                  structure_grade = c("weak", "weak", "structureless"),
                  structure_type = c("granular", "subangular blocky", "massive"))
  expect_false(isTRUE(cambic(one)$passed))            # 10 cm only
  unk <- mk(40, clay = c(15, NA, NA, 15))
  unk$horizons$silt_pct <- c(30, NA, NA, 30); unk$horizons$sand_pct <- c(55, NA, NA, 55)
  expect_true(is.na(cambic(unk)$passed))
})

test_that("v0.9.314 cambic: an albic horizon is set aside only over an argic horizon or in stagnic layers", {
  skip_on_cran()
  # light-coloured Bw (claric colours) in a profile with nothing below it
  p <- .w14_ped(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120), designation = c("A", "Bw", "C"),
                clay_pct = c(15, 18, 15), silt_pct = c(30, 30, 30), sand_pct = c(55, 52, 55),
                munsell_hue_moist = c("10YR", "10YR", "10YR"), munsell_value_moist = c(4, 6, 6),
                munsell_chroma_moist = c(2, 2, 3), munsell_value_dry = c(5, 7, 7),
                munsell_chroma_dry = c(2, 2, 3),
                structure_grade = c("weak", "weak", "structureless"),
                structure_type = c("granular", "subangular blocky", "massive"))
  expect_true(isTRUE(cambic(p)$passed))
  expect_equal(.wrb_albic_layers(p, 2L), integer(0))
})


# ---- Cambisols ---------------------------------------------------------------

test_that("v0.9.314 Cambisols: a cambic horizon starting <= 50 cm with its lower limit >= 25 cm", {
  skip_on_cran()
  mk <- function(top, bottom)
    .w14_ped(top_cm = c(0, top, bottom), bottom_cm = c(top, bottom, 150),
             designation = c("A", "Bw", "C"), clay_pct = c(15, 18, 15),
             silt_pct = c(30, 30, 30), sand_pct = c(55, 52, 55),
             structure_grade = c("weak", "moderate", "structureless"),
             structure_type = c("granular", "subangular blocky", "massive"))
  expect_true(isTRUE(cambisol(mk(20, 60))$passed))
  expect_false(isTRUE(cambisol(mk(60, 90))$passed))    # starts > 50 cm
  expect_false(isTRUE(cambisol(mk(5, 22))$passed))     # lower limit < 25 cm
  expect_true(isTRUE(cambic(mk(60, 90))$passed))       # the horizon is there all the same
  expect_true(isTRUE(cambisol(make_cambisol_canonical())$passed))
})

test_that("v0.9.314 Cambisols: undecided when the cambic horizon is undecided", {
  skip_on_cran()
  p <- .w14_ped(top_cm = c(0, 20), bottom_cm = c(20, 60), designation = c("A", "Bw"),
                clay_pct = c(NA, NA), structure_grade = c("weak", "moderate"),
                structure_type = c("granular", "subangular blocky"))
  expect_true(is.na(cambisol(p)$passed))
})

test_that("v0.9.314 Cambisols: a vertic horizon starting <= 100 cm is alternative 3", {
  skip_on_cran()
  v <- make_vertisol_canonical()
  skip_if_not(isTRUE(vertic_horizon(v)$passed))
  expect_true(isTRUE(cambisol(v)$evidence$alternatives$fragic_thionic_vertic))
})


# ---- calcic horizon and Calcisols --------------------------------------------

test_that("v0.9.314 calcic: criterion 2.b from the laboratory CaCO3, whatever the secondary carbonates", {
  skip_on_cran()
  p <- .w14_ped(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), designation = c("A", "Bk", "C"),
                caco3_pct = c(5, 30, 10))
  expect_true(isTRUE(calcic(p)$passed))                # Bk 20 % above the C
  expect_equal(calcic(p)$layers, 2L)
})

test_that("v0.9.314 calcic: undecided when neither 2.a nor 2.b can be told; decided by the secondary carbonates", {
  skip_on_cran()
  uni <- function(sc) .w14_ped(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                               designation = c("A", "C1", "C2"), caco3_pct = c(20, 20, 20),
                               secondary_carbonates_pct = sc)
  expect_true(is.na(calcic(uni(c(NA, NA, NA)))$passed))
  # recorded secondary carbonates < 5 %: the deepest layer has nothing below it
  # to compare with, so criterion 2.b is still undecided there
  expect_true(is.na(calcic(uni(c(2, 2, 2)))$passed))
  over <- .w14_ped(top_cm = c(0, 20, 40, 80), bottom_cm = c(20, 40, 80, 120),
                   designation = c("A", "C1", "C2", "2C"), caco3_pct = c(20, 20, 20, 5),
                   secondary_carbonates_pct = c(2, 2, 2, 0))
  expect_false(isTRUE(calcic(over)$passed))            # 2.b fails over every layer
  expect_false(is.na(calcic(over)$passed))
  expect_true(isTRUE(calcic(uni(c(NA, 8, 8)))$passed))
  # the USDA and SiBCS readings keep the layer when the morphology is unknown
  expect_true(isTRUE(.calcic_impl(uni(c(NA, NA, NA)))$passed))
})

test_that("v0.9.314 calcic: no exemption over a highly calcareous layer, no comparison across a lithic discontinuity", {
  skip_on_cran()
  over_marl <- .w14_ped(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90),
                        designation = c("A", "Bk", "C"), caco3_pct = c(5, 20, 45),
                        secondary_carbonates_pct = c(0, 2, 0))
  expect_false(isTRUE(calcic(over_marl)$passed))      # 20 % over 45 %: not 5 % higher
  disc <- .w14_ped(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90),
                   designation = c("A", "Bk", "2C"), caco3_pct = c(5, 30, 10),
                   secondary_carbonates_pct = c(0, 2, 0))
  expect_false(isTRUE(calcic(disc)$passed))           # the 2C is another material
  expect_false(isTRUE(.wrb_caco3_2b(disc$horizons, 2L)))
})

test_that("v0.9.314 calcic: a layer of a petrocalcic horizon is not calcic", {
  skip_on_cran()
  p <- .w14_ped(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90), designation = c("A", "Bkm", "C"),
                caco3_pct = c(5, 40, 10), cementation_class = c(NA, "strongly", NA),
                clay_pct = c(20, 20, 20), silt_pct = c(30, 30, 30), sand_pct = c(50, 50, 50))
  skip_if_not(isTRUE(petrocalcic(p)$passed))
  expect_false(isTRUE(calcic(p)$passed))
  expect_true(isTRUE(calcisol(p)$passed))              # the petrocalcic horizon keys the Calcisol
})

test_that("v0.9.314 Calcisols: starting <= 100 cm and no argic horizon above without secondary carbonates", {
  skip_on_cran()
  base <- list(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 130),
               designation = c("A", "Bt", "Bk", "C"),
               clay_pct = c(12, 30, 28, 25), silt_pct = c(30, 30, 30, 30),
               sand_pct = c(58, 40, 42, 45), caco3_pct = c(0, 0, 30, 10),
               structure_grade = c("weak", "moderate", "moderate", "structureless"),
               structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"),
               clay_films_amount = c(NA, "common", NA, NA),
               secondary_carbonates_pct = c(0, 0, NA, NA))
  p <- do.call(.w14_ped, base)
  expect_true(isTRUE(calcic(p)$passed))
  skip_if_not(isTRUE(argic(p)$passed))
  expect_false(isTRUE(calcisol(p)$passed))             # argic above, no secondary carbonates
  q <- p; q$horizons$secondary_carbonates_pct <- c(0, NA, NA, NA)
  q$horizons$designation[2] <- "Bt"
  expect_true(is.na(calcisol(q)$passed))               # not recorded in the Bt: undecided
  r <- p; r$horizons$designation[c(2, 4)] <- c("Btk", "Ck"); r$horizons$secondary_carbonates_pct[2] <- NA
  expect_true(isTRUE(calcisol(r)$passed))              # Btk: secondary carbonates throughout
  deep <- .w14_ped(top_cm = c(0, 30, 110), bottom_cm = c(30, 110, 150),
                   designation = c("A", "Bw", "Bk"), caco3_pct = c(0, 2, 30),
                   secondary_carbonates_pct = c(0, 0, 10))
  expect_true(isTRUE(calcic(deep)$passed))
  expect_false(isTRUE(calcisol(deep)$passed))          # starts > 100 cm
  # no clay recorded: the argic horizon is undecided, and so is the gate
  noclay <- .w14_ped(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 90),
                     designation = c("A", "Bk", "C"), caco3_pct = c(5, 30, 10))
  expect_true(isTRUE(calcic(noclay)$passed))
  if (is.na(argic(noclay)$passed)) expect_true(is.na(calcisol(noclay)$passed))
})

test_that("v0.9.314 Gypsisols: a gypsic horizon starting <= 100 cm, the argic rule with gypsum or carbonates", {
  skip_on_cran()
  expect_true(isTRUE(gypsisol(make_gypsisol_canonical())$passed))
  deep <- .w14_ped(top_cm = c(0, 30, 110), bottom_cm = c(30, 110, 150),
                   designation = c("A", "Bw", "By"), caso4_pct = c(0, 1, 30))
  expect_true(isTRUE(gypsic(deep)$passed))
  expect_false(isTRUE(gypsisol(deep)$passed))
  g <- .w14_ped(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 130),
                designation = c("A", "Bty", "By", "Cy"),
                clay_pct = c(12, 30, 28, 25), silt_pct = c(30, 30, 30, 30),
                sand_pct = c(58, 40, 42, 45), caso4_pct = c(0, 3, 30, 2),
                structure_grade = c("weak", "moderate", "moderate", "structureless"),
                structure_type = c("granular", "subangular blocky", "subangular blocky", "massive"),
                clay_films_amount = c(NA, "common", NA, NA))
  skip_if_not(isTRUE(argic(g)$passed) && isTRUE(gypsic(g)$passed))
  expect_true(isTRUE(gypsisol(g)$passed))              # Bty: secondary gypsum in the argic
  g$horizons$designation[2] <- "Bt"; g$horizons$gypsum_visible_pct <- c(0, 0, 20, 0)
  g$horizons$secondary_carbonates_pct <- c(0, 0, 0, 0)
  expect_false(isTRUE(gypsisol(g)$passed))
})


# ---- Arenosols ---------------------------------------------------------------

test_that("v0.9.314 Arenosols: weighted average texture, finer layers < 15 cm", {
  skip_on_cran()
  mk <- function(clay, silt, bottoms = c(20, 50, 60, 120))
    .w14_ped(top_cm = c(0, bottoms[-length(bottoms)]), bottom_cm = bottoms,
             designation = c("A", "C1", "C2", "C3"), clay_pct = clay, silt_pct = silt,
             sand_pct = 100 - clay - silt, coarse_fragments_pct = c(0, 0, 0, 0))
  # a 10 cm sandy loam band in sand: finer layers < 15 cm, average loamy sand
  expect_true(isTRUE(arenic_texture(mk(c(4, 3, 12, 3), c(8, 5, 20, 5)))$passed))
  # the band is 20 cm thick: finer layers >= 15 cm
  expect_false(isTRUE(arenic_texture(mk(c(4, 3, 12, 3), c(8, 5, 20, 5), c(20, 50, 70, 120)))$passed))
  # loamy sand throughout but at the limit: average silt + 2 clay >= 30
  expect_false(isTRUE(arenic_texture(mk(c(10, 10, 10, 10), c(12, 12, 12, 12)))$passed))
  # the texture of 20 cm is not recorded: undecided
  u <- mk(c(4, NA, 3, 3), c(8, NA, 5, 5), c(20, 40, 60, 120))
  expect_true(is.na(arenic_texture(u)$passed))
  # >= 15 cm of layers with >= 40 % coarse fragments
  s <- mk(c(4, 3, 3, 3), c(8, 5, 5, 5)); s$horizons$coarse_fragments_pct <- c(0, 50, 0, 0)
  expect_false(isTRUE(arenic_texture(s)$passed))
  expect_true(isTRUE(arenic_texture(make_arenosol_canonical())$passed))
})


# ---- fluvic material and Fluvisols -------------------------------------------

.w14_fluv <- function(origin = "fluviatile", oc = c(1.2, 0.5, 0.4, 0.3),
                      grade = c("weak", "structureless", "structureless", "structureless"),
                      type = c("subangular blocky", "massive", "massive", "massive"),
                      clay = c(20, 12, 30, 15))
  .w14_ped(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 130),
           designation = c("A", "C", "2C", "3C"), clay_pct = clay, silt_pct = c(30, 30, 30, 30),
           sand_pct = 100 - clay - 30, oc_pct = oc, structure_grade = grade, structure_type = type,
           rock_origin = rep(origin, 4))

test_that("v0.9.314 fluvic material: strata by texture OR by carbon, the origin and the structure", {
  skip_on_cran()
  # clay 20 / 12 / 30 / 15 reverses, SOC decreases regularly: fluvic (2.a alone)
  expect_true(isTRUE(fluvic_material(.w14_fluv())$passed))
  # the SiBCS carater fluvico keeps the older AND
  expect_false(isTRUE(.fluvic_material_impl(.w14_fluv())$passed))
  # another origin recorded
  expect_false(isTRUE(fluvic_material(.w14_fluv(origin = "colluvial"))$passed))
  # no origin recorded: undecided; the site's parent material can give it
  no <- .w14_fluv(origin = NA_character_)
  expect_true(is.na(fluvic_material(no)$passed))
  no$site$parent_material <- "Recent alluvium"
  expect_true(isTRUE(fluvic_material(no)$passed))
  # strong angular blocky structure everywhere: not fluvic material
  st <- .w14_fluv(grade = rep("strong", 4), type = rep("angular blocky", 4))
  expect_false(isTRUE(fluvic_material(st)$passed))
  # no strata: regular clay and SOC
  flat <- .w14_fluv(clay = c(20, 20, 21, 21))
  expect_false(isTRUE(fluvic_material(flat)$passed))
  # strata by SOC alone: two layers richer than the one above
  soc <- .w14_fluv(clay = c(20, 20, 21, 21), oc = c(1.0, 0.3, 0.8, 1.4))
  expect_true(isTRUE(fluvic_material(soc)$passed))
  # strata in the sand fraction: 52 / 69 / 38 % sand with the clay nearly even
  sand <- .w14_ped(top_cm = c(0, 20, 50, 80), bottom_cm = c(20, 50, 80, 130),
                   designation = c("A", "C", "2C", "3C"), clay_pct = c(10, 7, 12, 10),
                   silt_pct = c(38, 24, 50, 37), sand_pct = c(52, 69, 38, 53),
                   oc_pct = c(1.2, 0.5, 0.4, 0.3),
                   structure_grade = c("weak", "structureless", "structureless", "structureless"),
                   structure_type = c("granular", "massive", "single grain", "massive"),
                   rock_origin = rep("fluviatile", 4))
  expect_true(isTRUE(fluvic_material(sand)$passed))
  expect_false(isTRUE(fluvic_material(sand)$evidence$strata_soc))
})

test_that("v0.9.314 Fluvisols: fluvic material >= 25 cm from <= 25 cm, or below a plough layer", {
  skip_on_cran()
  expect_true(isTRUE(fluvisol(.w14_fluv())$passed))
  # fluvic material only from 50 cm: the C above is strongly structured
  deep <- .w14_fluv(grade = c("strong", "strong", "structureless", "structureless"),
                    type = c("angular blocky", "angular blocky", "massive", "massive"))
  expect_true(isTRUE(fluvic_material(deep)$passed))
  expect_false(isTRUE(fluvisol(deep)$passed))
  # below an Ap of 0-40 cm, fluvic material to >= 50 cm
  ap <- .w14_ped(top_cm = c(0, 40, 60, 90), bottom_cm = c(40, 60, 90, 130),
                 designation = c("Ap", "C", "2C", "3C"), clay_pct = c(20, 12, 30, 15),
                 silt_pct = c(30, 30, 30, 30), sand_pct = c(50, 58, 40, 55),
                 oc_pct = c(1.2, 0.5, 0.4, 0.3),
                 structure_grade = c("moderate", "structureless", "structureless", "structureless"),
                 structure_type = c("granular", "massive", "massive", "massive"),
                 rock_origin = rep("fluviatile", 4))
  expect_true(isTRUE(fluvisol(ap)$passed))
  expect_true(isTRUE(fluvisol(make_fluvisol_canonical())$passed))
})


# ---- the ISIS reader: deposition of the parent material ----------------------

test_that("v0.9.314 ISIS reader: fluvial, marine and lacustrine deposits are the rock_origin", {
  skip_on_cran()
  cd <- function(item) list(item = item, description = item)
  prof <- function(d1, d2 = NULL) list(
    profileCode = "XX002", countryCode = "XX", geog = list(latitude = 1, longitude = 1),
    lithologyParmatDeposition = if (!is.null(d1)) cd(d1), lithologyParmatDeposition2 = if (!is.null(d2)) cd(d2),
    layers = list(
      list(layerNumber = 1L, upperDepth = 0L, lowerDepth = 20L, layerName = "Ah", clay = list("20")),
      list(layerNumber = 2L, upperDepth = 20L, lowerDepth = 60L, layerName = "C", clay = list("15")),
      list(layerNumber = 3L, upperDepth = 60L, lowerDepth = 70L, layerName = "R")))
  h <- isis_profile_to_pedon(prof("F"))$horizons
  expect_equal(h$rock_origin, c("fluviatile", "fluviatile", NA))
  expect_equal(isis_profile_to_pedon(prof("M"))$horizons$rock_origin[1], "marine")
  expect_true(all(is.na(isis_profile_to_pedon(prof("X"))$horizons$rock_origin)))
  expect_true(all(is.na(isis_profile_to_pedon(prof("F", "X"))$horizons$rock_origin)))
  expect_match(.isis_profile_fields(), "lithologyParmatDeposition")
})
