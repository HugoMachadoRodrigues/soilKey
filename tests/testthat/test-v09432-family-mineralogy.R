# v0.9.432: USDA family mineralogy classes and human-altered and
# human-transported material classes (KST 13th ed., ch. 17, pp. 362-366),
# with the readings F1-F4 and F19 of the author (USDA audit, wave 5,
# 2026-11-23).

.mp <- function(h, site = list(id = "m")) soilKey::PedonRecord$new(site = site, horizons = h)

# A clayey soil whose control section (25-100 cm, an Inceptisol) is one
# material; `...` adds columns.
.clayey <- function(...) {
  h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                  designation = c("A", "Bw1", "Bw2"),
                  clay_pct = 50, sand_pct = 25, silt_pct = 25)
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  .mp(h)
}
# A loamy soil (coarse-loamy, section E).
.loamy <- function(...) {
  h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                  designation = c("A", "Bw", "C"),
                  clay_pct = 12, sand_pct = 55, silt_pct = 33, very_fine_sand_pct = 10)
  extra <- list(...)
  for (k in names(extra)) h[[k]] <- extra[[k]]
  .mp(h)
}
.min_in <- function(p, ord = "IN", gg = "Dystrudepts", sg = "Typic Dystrudepts", ...)
  family_mineralogy_usda(p, ord, gg, sg, ...)
.min_ox <- function(p) family_mineralogy_usda(p, "OX", "Hapludox", "Typic Hapludox")
# every mineral recorded or a measured zero; section C ruled out
.ana <- list(mineralogy_analysed = TRUE, caco3_pct = 0, caso4_pct = 0, fe_dcb_pct = 1)


test_that("section A (Oxisols): ferritic, gibbsitic, sesquic, ferruginous, allitic", {
  skip_on_cran()
  expect_equal(.min_ox(.clayey(fe_dcb_pct = 30))$value, "ferritic")          # 42.9 Fe2O3
  # gibbsite_pct is % of the clay: 90 x 50 / 100 = 45 of the fine earth
  expect_equal(.min_ox(.clayey(fe_dcb_pct = 5, gibbsite_pct = 90))$value, "gibbsitic")
  expect_equal(.min_ox(.clayey(fe_dcb_pct = 20, gibbsite_pct = 50))$value, "sesquic")
  expect_equal(.min_ox(.clayey(fe_dcb_pct = 20, gibbsite_pct = 10))$value, "ferruginous")
  expect_equal(.min_ox(.clayey(fe_dcb_pct = 5, gibbsite_pct = 50))$value, "allitic")
})

test_that("section A: kaolinitic, halloysitic and mixed after a mineralogical analysis", {
  skip_on_cran()
  k <- .min_ox(.clayey(fe_dcb_pct = 5, mineralogy_analysed = TRUE, kaolinite_pct = 70,
                       halloysite_pct = 10, gibbsite_pct = 10))
  expect_equal(k$value, "kaolinitic")
  hl <- .min_ox(.clayey(fe_dcb_pct = 5, mineralogy_analysed = TRUE, kaolinite_pct = 20,
                        halloysite_pct = 50))
  expect_equal(hl$value, "halloysitic")      # more halloysite than kaolinite
  mx <- .min_ox(.clayey(fe_dcb_pct = 5, mineralogy_analysed = TRUE, kaolinite_pct = 20,
                        smectite_pct = 40, illite_pct = 30))
  expect_equal(mx$value, "mixed")
})

test_that("section A covers the kandi and kanhap great groups only", {
  skip_on_cran()
  p <- .clayey(fe_dcb_pct = 20, gibbsite_pct = 10)
  expect_equal(family_mineralogy_usda(p, "UT", "Kandiudults", "Typic Kandiudults")$value,
               "ferruginous")
  r <- family_mineralogy_usda(p, "UT", "Hapludults", "Typic Hapludults")
  expect_false(identical(r$value, "ferruginous"))
  expect_false(grepl("A", r$evidence$sections[["whole"]]))
})

test_that("F1: nothing measured leaves the class undecided, listing what the section needs", {
  skip_on_cran()
  r <- .min_ox(.clayey())
  expect_null(r$value)
  expect_true(all(c("fe_dcb_pct", "gibbsite_pct") %in% r$missing))
  expect_match(r$evidence$reason, "undecided")
  # Fe2O3 alone decides ferritic
  expect_equal(.min_ox(.clayey(fe_dcb_pct = 29))$value, "ferritic")
  # a total iron below 40 % Fe2O3 rules ferritic out (Fe by dithionite is a
  # part of it), leaving gibbsite undecided
  r2 <- .min_ox(.clayey(fe_total_pct = 30))
  expect_null(r2$value)
  expect_false(isTRUE(r2$evidence$tested$whole$ferritic))
  expect_true(is.na(r2$evidence$tested$whole$gibbsitic))
})

test_that("section B: the classes of a substitute class", {
  skip_on_cran()
  run_b <- function(...) {
    p <- .clayey(...)
    U <- .min_unit_usda(.min_layers_usda(p), 25, 100)
    .min_unit_class(U, FALSE, "medial", TRUE, FALSE)
  }
  expect_equal(run_b(caso4_pct = 45)$class, "hypergypsic")
  expect_equal(run_b(mineralogy_analysed = TRUE, caso4_pct = 0, opaline_pct = 35)$class, "opaline")
  expect_equal(run_b(mineralogy_analysed = TRUE, caso4_pct = 0, si_ox_pct = 1, fe_ox_pct = 0.5)$class,
               "amorphic")                                   # 8 > 1, sum 9
  expect_equal(run_b(mineralogy_analysed = TRUE, caso4_pct = 0, si_ox_pct = 0.1, fe_ox_pct = 2.5)$class,
               "ferrihydritic")                              # 0.8 < 5, sum 5.8
  expect_equal(run_b(mineralogy_analysed = TRUE, caso4_pct = 0, si_ox_pct = 0.1, fe_ox_pct = 0.5,
                     volcanic_glass_pct = 40)$class, "glassy")
  expect_equal(run_b(mineralogy_analysed = TRUE, caso4_pct = 0, si_ox_pct = 0.1, fe_ox_pct = 0.5)$class,
               "mixed")
  # Si and Fe by oxalate not measured: undecided
  r <- run_b(mineralogy_analysed = TRUE, caso4_pct = 0)
  expect_true(is.na(r$state))
  expect_true(all(c("si_ox_pct", "fe_ox_pct") %in% r$missing))
})

test_that("section C: anhydritic, gypsic, carbonatic, ferritic, magnesic, glauconitic", {
  skip_on_cran()
  base <- function(...) do.call(.loamy, modifyList(.ana, list(...)))
  expect_equal(.min_in(base(anhydrite_pct = 20))$value, "anhydritic")
  expect_equal(.min_in(base(caso4_pct = 20))$value, "gypsic")
  expect_equal(.min_in(base(caco3_pct = 45))$value, "carbonatic")
  expect_equal(.min_in(base(fe_dcb_pct = 30))$value, "ferritic")
  expect_equal(.min_in(base(mg_silicates_pct = 45))$value, "magnesic")
  expect_equal(.min_in(base(glauconite_pct = 25))$value, "glauconitic")
  expect_equal(.min_in(base(gibbsite_pct = 100, boehmite_pct = 40))$value, "gibbsitic")
})

test_that("section C has no mixed class: without an analysis anhydritic stays undecided (F2)", {
  skip_on_cran()
  r <- .min_in(.loamy(caco3_pct = 0, caso4_pct = 0, fe_dcb_pct = 1))
  expect_null(r$value)
  expect_true(all(c("anhydrite_pct", "mineralogy_analysed") %in% r$missing))
  # mineralogy_analysed NA is the same as FALSE
  r2 <- .min_in(.loamy(caco3_pct = 0, caso4_pct = 0, fe_dcb_pct = 1, mineralogy_analysed = NA))
  expect_null(r2$value)
})

test_that("section D: parasesquic and the clay-mineral classes", {
  skip_on_cran()
  base <- function(...) do.call(.clayey, modifyList(.ana, list(...)))
  expect_equal(.min_in(base(fe_dcb_pct = 8))$value, "parasesquic")          # 11.4
  # Fe2O3 + gibbsite (fine-earth basis): 5.7 + 10 x 50 / 100 = 10.7
  expect_equal(.min_in(base(fe_dcb_pct = 4, gibbsite_pct = 10, kaolinite_pct = 60))$value,
               "parasesquic")
  expect_equal(.min_in(base(halloysite_pct = 60, kaolinite_pct = 20))$value, "halloysitic")
  expect_equal(.min_in(base(kaolinite_pct = 60, halloysite_pct = 5))$value, "kaolinitic")
  expect_equal(.min_in(base(smectite_pct = 45, kaolinite_pct = 40))$value, "smectitic")
  expect_equal(.min_in(base(illite_pct = 60, smectite_pct = 15, kaolinite_pct = 10))$value, "illitic")
  expect_equal(.min_in(base(vermiculite_pct = 40, illite_pct = 30, kaolinite_pct = 30))$value,
               "vermiculitic")
  expect_equal(.min_in(base(kaolinite_pct = 30, illite_pct = 30, smectite_pct = 30,
                            ph_naf = 9, water_content_1500kpa = 35))$value, "isotic")
  expect_equal(.min_in(base(kaolinite_pct = 30, illite_pct = 30, smectite_pct = 30,
                            ph_naf = 7, water_content_1500kpa = 20))$value, "mixed")
})

test_that("section D: the dominant clay mineral recorded as a word decides the single-kind test", {
  skip_on_cran()
  # no analysis: the smectite share is unknown, the word decides
  p <- .clayey(caco3_pct = 0, caso4_pct = 0, fe_dcb_pct = 1, anhydrite_pct = 0,
               boehmite_pct = 0, mg_silicates_pct = 0, glauconite_pct = 0,
               clay_mineralogy = "smectite", kaolinite_pct = 20, halloysite_pct = 0,
               gibbsite_pct = 0, kaolin_other_pct = 0)
  expect_equal(.min_in(p)$value, "smectitic")
})

test_that("section E: micaceous, parasesquic, isotic, siliceous, mixed", {
  skip_on_cran()
  base <- function(...) do.call(.loamy, modifyList(.ana, list(...)))
  expect_equal(.min_in(base(mica_grains_pct = 50))$value, "micaceous")
  expect_equal(.min_in(base(fe_dcb_pct = 8))$value, "parasesquic")
  expect_equal(.min_in(base(ph_naf = 9, water_content_1500kpa = 9))$value, "isotic")   # 0.75
  expect_equal(.min_in(base(ph_naf = 7, water_content_1500kpa = 4,
                            resistant_minerals_pct = 95))$value, "siliceous")
  expect_equal(.min_in(base(ph_naf = 7, water_content_1500kpa = 4,
                            resistant_minerals_pct = 70))$value, "mixed")
})

test_that("mixed is reached only after every earlier class is ruled out", {
  skip_on_cran()
  # isotic undecided (no NaF pH): no mixed
  r <- .min_in(do.call(.loamy, modifyList(.ana, list(water_content_1500kpa = 9,
                                                       resistant_minerals_pct = 70))))
  expect_null(r$value)
  expect_true("ph_naf" %in% r$missing)
  # free carbonates (effervescence) rule isotic out
  r2 <- .min_in(do.call(.loamy, modifyList(.ana, list(caco3_pct = 5, effervescence_class = "strong",
                                                        resistant_minerals_pct = 70))))
  expect_equal(r2$value, "mixed")
  # 1500 kPa water / clay below 0.6 rules isotic out
  r3 <- .min_in(do.call(.loamy, modifyList(.ana, list(water_content_1500kpa = 4,
                                                        resistant_minerals_pct = 70))))
  expect_equal(r3$value, "mixed")
})

test_that("F2: mineralogy_analysed TRUE makes an unlisted mineral a measured zero", {
  skip_on_cran()
  args <- list(caco3_pct = 0, caso4_pct = 0, fe_dcb_pct = 1, ph_naf = 7,
               water_content_1500kpa = 4)
  expect_null(.min_in(do.call(.loamy, args))$value)
  expect_equal(.min_in(do.call(.loamy, c(args, mineralogy_analysed = TRUE)))$value, "mixed")
  r <- .min_in(do.call(.loamy, c(args, mineralogy_analysed = FALSE)))
  expect_null(r$value)
  expect_true("mineralogy_analysed" %in% r$missing)
})

test_that("F3 and F4: stand-ins are an opt-in suggestion only", {
  skip_on_cran()
  p <- .loamy(caco3_pct = 0, caso4_pct = 0, fe_dcb_pct = 1, sand_mineralogy = "quartzosa",
              ph_naf = 7, water_content_1500kpa = 4)
  expect_null(.min_in(p)$value)
  s <- .min_in(p, suggest = TRUE)
  expect_equal(s$value, "siliceous")
  expect_true(isTRUE(s$evidence$suggestion))
  expect_true("resistant_minerals_pct" %in% s$missing)
  # classify_usda warns and caps the grade at C
  r <- suppressWarnings(classify_usda(make_calcisol_canonical(), include_family = TRUE,
                                      suggest_mineralogy = TRUE))
  expect_true(any(grepl("Family mineralogy class .* is a suggestion", r$warnings)))
  expect_true(r$evidence_grade %in% c("C", "D", NA))
  r0 <- suppressWarnings(classify_usda(make_calcisol_canonical(), include_family = TRUE))
  expect_false(any(grepl("Family mineralogy class", r0$warnings)))
})

test_that("Quartzipsamments and organic soils have no mineralogy term", {
  skip_on_cran()
  q <- family_mineralogy_usda(.loamy(), "EN", "Quartzipsamments", "Typic Quartzipsamments")
  expect_null(q$value)
  expect_equal(q$evidence$reason, "not named in this taxon")
  expect_length(q$missing, 0)
  hi <- family_mineralogy_usda(.loamy(), "HI", "Haplohemists", "Typic Haplohemists")
  expect_null(hi$value)
  expect_length(hi$missing, 0)
})

test_that("strongly contrasting classes: each named part, one term when they agree", {
  skip_on_cran()
  mk <- function(up, low) {
    h <- data.frame(top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120),
                    designation = c("A", "Bw", "2C"), boundary_distinctness = "abrupt",
                    clay_pct = c(45, 45, 3), sand_pct = c(25, 25, 94), silt_pct = c(30, 30, 3),
                    very_fine_sand_pct = c(5, 5, 2), mineralogy_analysed = TRUE,
                    caco3_pct = 0, caso4_pct = 0, fe_dcb_pct = 1,
                    ph_naf = 7, water_content_1500kpa = c(20, 20, 1))
    for (k in names(up)) h[[k]] <- c(up[[k]], up[[k]], 0)
    for (k in names(low)) h[[k]] <- c(h[[k]][1:2] %||% c(0, 0), low[[k]])
    .mp(h)
  }
  p <- mk(list(smectite_pct = 70), list(resistant_minerals_pct = 95))
  ps <- family_particle_size_usda(p, "IN", "Dystrudepts", "Typic Dystrudepts")
  expect_match(ps$value, "^clayey over sandy")
  m <- family_mineralogy_usda(p, "IN", "Dystrudepts", "Typic Dystrudepts", particle_size = ps)
  expect_equal(m$value, "smectitic over siliceous")
  expect_equal(unname(m$evidence$sections), c("D", "E"))   # C ruled out
  p2 <- mk(list(smectite_pct = 30, kaolinite_pct = 30, illite_pct = 30),
           list(resistant_minerals_pct = 60))
  m2 <- family_mineralogy_usda(p2, "IN", "Dystrudepts", "Typic Dystrudepts")
  expect_equal(m2$value, "mixed")              # never "mixed over mixed"
})

test_that("human-transported material classes: the use gate (p. 362)", {
  skip_on_cran()
  nat <- .loamy()
  r <- family_human_material_usda(nat, "EN", "Udorthents", "Typic Udorthents")
  expect_null(r$value)
  expect_match(r$evidence$reason, "not used")
  # 60 cm of fill from the surface: used, spolic
  fill <- .loamy(designation = c("^Au", "^C", "C"), artefacts_pct = c(5, 5, 0))
  expect_equal(family_human_material_usda(fill, "EN", "Udorthents", "Typic Udorthents")$value,
               "spolic")
  # thin fill (25 cm): used only in an anthropogenic subgroup
  thin <- .loamy(designation = c("^Au", "Bw", "C"), artefacts_pct = c(5, 0, 0),
                 araric_fragments_pct = c(5, 0, 0))
  expect_null(family_human_material_usda(thin, "EN", "Udorthents", "Typic Udorthents")$value)
  expect_equal(family_human_material_usda(thin, "IN", "Eutrudepts", "Anthraltic Eutrudepts")$value,
               "araric")
  # organic soils: not used
  expect_null(family_human_material_usda(fill, "HI", "Haplohemists", "Typic Haplohemists")$value)
})

test_that("human-transported material classes: the key A1 to A12 (p. 363)", {
  skip_on_cran()
  hm <- function(...) {
    p <- .loamy(designation = c("^Au", "^C1", "^C2"), ...)
    family_human_material_usda(p, "EN", "Udorthents", "Anthroportic Udorthents")$value
  }
  expect_equal(hm(methanogenic_evidence = c(FALSE, TRUE, FALSE)), "methanogenic")
  expect_equal(hm(artefacts_asphalt_pct = c(0, 40, 0)), "asphaltic")
  expect_equal(hm(artefacts_concrete_pct = c(0, 40, 0)), "concretic")
  expect_equal(hm(artefacts_synthetic_gypsum_pct = c(0, 45, 0)), "gypsifactic")
  expect_equal(hm(artefacts_coal_combustion_pct = c(0, 40, 0)), "combustic")
  expect_equal(hm(fly_ash_grains_pct = c(0, 20, 0)), "ashifactic")
  expect_equal(hm(pyrolysis_grains_pct = c(0, 6, 0)), "pyrocarbonic")
  expect_equal(hm(artefacts_pct = c(0, 40, 40), artefacts_cohesive_persistent = TRUE), "artifactic")
  expect_equal(hm(artefacts_pct = c(0, 20, 20), artefacts_cohesive_persistent = TRUE), "pauciartifactic")
  # not recorded as cohesive and persistent: they do not count (F19)
  expect_equal(hm(artefacts_pct = c(0, 40, 40)), "spolic")
  expect_equal(hm(htm_finely_stratified = c(FALSE, TRUE, TRUE)), "dredgic")
  expect_equal(hm(), "spolic")
  # a single 35 cm layer of 40 % artifacts is too thin for artifactic
  expect_equal(hm(artefacts_pct = c(0, 40, 0), artefacts_cohesive_persistent = TRUE), "spolic")
})

test_that("the human-transported class sits between particle size and mineralogy (p. 355)", {
  skip_on_cran()
  fam <- list(
    mineralogy = FamilyAttribute$new("mineralogy", "mixed"),
    particle_size = FamilyAttribute$new("particle_size", "fine-loamy"),
    human_material = FamilyAttribute$new("human_material", "spolic"),
    temperature_regime = FamilyAttribute$new("temperature_regime", "mesic"))
  expect_equal(family_label_usda(fam), "fine-loamy, spolic, mixed, mesic")
  r <- suppressWarnings(classify_usda(make_technosol_canonical(), include_family = TRUE))
  expect_match(r$name, "^spolic, ")
  expect_equal(r$trace$family$human_material$value, "spolic")
})
