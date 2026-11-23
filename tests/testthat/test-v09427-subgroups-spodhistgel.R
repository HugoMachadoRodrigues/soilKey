# v0.9.427: the subgroup lists of the Spodosols, Histosols and Gelisols rebuilt
# from the KST 13th edition (Ch. 14, pp. 311-320; Ch. 10, pp. 199-205; Ch. 9,
# pp. 189-198) and the tests of R/subgroups-usda-spodhistgel.R.

# ---- structural: each great group's list is the KST list, in KST order ----

.kst427 <- list(
  # Spodosols
  CAA = c("Lithic", "Placic", "Duric", "Andic", "Entic", "Typic"),
  CAB = c("Lithic", "Duric", "Histic", "Alfic Arenic", "Arenic Ultic", "Arenic Umbric",
          "Arenic", "Grossarenic", "Alfic", "Ultic", "Aeric", "Typic"),
  CAC = c("Histic", "Haploplaggic", "Argic", "Typic"),
  CAD = c("Andic", "Typic"),
  CAE = c("Histic", "Andic", "Typic"),
  CAF = c("Lithic", "Histic", "Andic", "Alfic", "Ultic", "Umbric", "Typic"),
  CAG = c("Lithic", "Histic", "Andic", "Argic", "Umbric", "Typic"),
  CBA = c("Lithic", "Andic", "Aquic", "Turbic", "Typic"),
  CBB = c("Lithic", "Andic", "Aquic", "Turbic", "Typic"),
  CCA = c("Andic", "Humic", "Typic"),
  CCB = c("Aquandic", "Andic", "Aquic", "Oxyaquic", "Humic", "Typic"),
  CCC = c("Lithic", "Aquandic", "Andic", "Folistic", "Aquic", "Oxyaquic", "Typic"),
  CCD = c("Lithic", "Aquandic", "Andic", "Folistic", "Aquic", "Oxyaquic", "Entic", "Typic"),
  CDA = c("Andic", "Typic"),
  CDB = c("Andic", "Typic"),
  CDC = c("Typic"),
  CDD = c("Lithic", "Andic", "Haploplaggic", "Typic"),
  CEA = c("Typic"),
  CEB = c("Andic", "Typic"),
  CEC = c("Aquic", "Alfic Oxyaquic", "Oxyaquic", "Haploplaggic", "Alfic", "Ultic", "Entic", "Typic"),
  CED = c("Oxyaquic", "Arenic Ultic", "Arenic", "Entic Grossarenic", "Entic", "Grossarenic",
          "Haploplaggic", "Alfic", "Ultic", "Typic"),
  CEE = c("Entic Lithic", "Lithic", "Fragiaquic", "Aqualfic", "Aquentic", "Aquic",
          "Alfic Oxyaquic", "Oxyaquic Ultic", "Fragic", "Lamellic Oxyaquic", "Lamellic",
          "Oxyaquic", "Andic", "Alfic", "Ultic", "Entic", "Typic"),
  # Histosols
  BAA = c("Lithic", "Typic"), BAB = c("Lithic", "Typic"), BAC = c("Lithic", "Typic"),
  BAD = c("Lithic", "Typic"),
  BBA = c("Terric", "Fibric", "Sapric", "Typic"),
  BBB = c("Terric", "Fibric", "Sapric", "Typic"),
  BBC = c("Sulfic", "Terric", "Fibric", "Sapric", "Typic"),
  BCA = c("Hydric", "Lithic", "Terric", "Fluvaquentic", "Sphagnic", "Typic"),
  BCB = c("Hydric", "Lithic", "Limnic", "Terric", "Fluvaquentic", "Hemic", "Typic"),
  BCC = c("Hydric", "Lithic", "Limnic", "Terric", "Fluvaquentic", "Hemic", "Typic"),
  BDA = c("Typic"),
  BDB = c("Lithic", "Terric", "Typic"),
  BDC = c("Lithic", "Limnic", "Terric", "Fluvaquentic", "Typic"),
  BDD = c("Lithic", "Limnic", "Halic Terric", "Halic", "Terric", "Fluvaquentic", "Hemic", "Typic"),
  BEA = c("Typic"), BEB = c("Terric", "Typic"), BEC = c("Typic"),
  BED = c("Hydric", "Lithic", "Terric", "Fluvaquentic", "Typic"),
  BEE = c("Hydric", "Lithic", "Limnic", "Terric", "Fluvaquentic", "Fibric", "Sapric", "Typic"),
  # Gelisols
  AAA = c("Lithic", "Glacic", "Typic"),
  AAB = c("Hemic", "Sapric", "Typic"),
  AAC = c("Lithic", "Terric", "Thapto-Humic", "Fluvaquentic", "Sphagnic", "Typic"),
  AAD = c("Lithic", "Limnic", "Terric", "Thapto-Humic", "Fluvaquentic", "Typic"),
  AAE = c("Lithic", "Limnic", "Terric", "Thapto-Humic", "Fluvaquentic", "Typic"),
  ABA = c("Lithic", "Glacic", "Ruptic", "Typic"),
  ABB = c("Lithic", "Glacic", "Sulfuric", "Ruptic-Histic", "Psammentic", "Typic"),
  ABC = c("Lithic", "Glacic", "Petrogypsic", "Gypsic", "Nitric", "Salic", "Calcic", "Typic"),
  ABD = c("Lithic", "Glacic", "Vertic", "Andic", "Vitrandic", "Folistic", "Cumulic", "Aquic", "Typic"),
  ABE = c("Lithic", "Glacic", "Vertic", "Andic", "Vitrandic", "Folistic", "Cumulic", "Aquic", "Typic"),
  ABF = c("Lithic", "Glacic", "Spodic", "Typic"),
  ABG = c("Lithic", "Glacic", "Folistic", "Aquic", "Typic"),
  ACA = c("Lithic", "Glacic", "Thapto-Humic", "Fluvaquentic", "Fluventic", "Ruptic", "Typic"),
  ACB = c("Lithic", "Glacic", "Sulfuric", "Ruptic-Histic", "Andic", "Vitrandic", "Salic",
          "Psammentic", "Thapto-Humic", "Fluvaquentic", "Typic"),
  ACC = c("Lithic", "Glacic", "Petrogypsic", "Gypsic", "Nitric", "Salic", "Calcic", "Typic"),
  ACD = c("Lithic", "Glacic", "Vertic", "Andic", "Vitrandic", "Folistic", "Cumulic", "Aquic", "Typic"),
  ACE = c("Lithic", "Glacic", "Vertic", "Andic", "Vitrandic", "Folistic", "Cumulic", "Aquic", "Typic"),
  ACF = c("Lithic", "Glacic", "Natric", "Typic"),
  ACG = c("Lithic", "Glacic", "Spodic", "Typic"),
  ACH = c("Lithic", "Glacic", "Thapto-Humic", "Fluvaquentic", "Folistic", "Aquic", "Fluventic", "Typic")
)

test_that("subgroup lists of the Spodosols, Histosols and Gelisols are the KST lists", {
  skip_on_cran()
  rules <- load_rules("usda")
  gg_name <- function(code) {
    for (sub in rules$great_groups) for (g in sub) if (identical(g$code, code)) return(g$name)
    NA_character_
  }
  for (code in names(.kst427)) {
    gg <- gg_name(code)
    expect_false(is.na(gg), info = code)
    got <- vapply(rules$subgroups[[code]], function(s) s$name, "")
    expect_equal(got, paste(.kst427[[code]], gg), info = code)
    codes <- vapply(rules$subgroups[[code]], function(s) s$code, "")
    expect_equal(codes, paste0(code, LETTERS[seq_along(codes)]), info = code)
    last <- rules$subgroups[[code]][[length(codes)]]
    expect_true(isTRUE(last$tests$default), info = code)
  }
  # every great group of the three orders is covered
  ggs <- unlist(lapply(c("CA", "CB", "CC", "CD", "CE", "BA", "BB", "BC", "BD", "BE",
                         "AA", "AB", "AC"),
                       function(s) vapply(rules$great_groups[[s]], function(g) g$code, "")))
  expect_setequal(ggs, names(.kst427))
})

test_that("no subgroup of the three orders is switched off or calls a missing test", {
  skip_on_cran()
  rules <- load_rules("usda")
  ns <- asNamespace("soilKey")
  for (code in names(.kst427)) for (s in rules$subgroups[[code]]) {
    expect_false(isFALSE(s$tests$default), info = s$name)
    for (t in s$tests$all_of) expect_true(exists(names(t)[1], envir = ns, mode = "function"),
                                          info = paste(s$name, names(t)[1]))
  }
})


# ---- small pedons ------------------------------------------------------

.pd427 <- function(hz, site = list()) {
  site <- utils::modifyList(list(id = "t427", lat = 60, lon = 10, country = "NO"), site)
  PedonRecord$new(site = site,
                  horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}

# a peat (Histosol) with an optional mineral layer and a water layer
.peat427 <- function(des = c("Oi", "Oe", "Oe", "C"), top = c(0, 30, 60, 100),
                     bot = c(30, 60, 100, 150), oc = c(40, 35, 30, 0.5), ...) {
  hz <- list(top_cm = top, bottom_cm = bot, designation = des, oc_pct = oc)
  extra <- list(...)
  for (k in names(extra)) hz[[k]] <- extra[[k]]
  .pd427(hz)
}


# ---- Histosols ---------------------------------------------------------

test_that("Terric: one mineral layer 30 cm thick starting in the control section, below the surface tier", {
  skip_on_cran()
  # C from 100 cm (in the control section, 130 cm): Terric; up to v0.9.426 the
  # layer had to start above 100 cm
  expect_true(isTRUE(sg_histosols_terric_usda(.peat427())$passed))
  # two thin mineral layers adding to 30 cm are not one layer
  pd <- .peat427(des = c("Oi", "C1", "Oe", "C2", "Oe"), top = c(0, 30, 45, 70, 85),
                 bot = c(30, 45, 70, 85, 150), oc = c(40, 1, 30, 1, 30))
  expect_true(isFALSE(sg_histosols_terric_usda(pd)$passed))
  # a mineral layer that starts below the control section is not Terric
  pd2 <- .peat427(top = c(0, 30, 60, 140), bot = c(30, 60, 140, 200))
  expect_true(isFALSE(sg_histosols_terric_usda(pd2)$passed))
  # organic or mineral not known (no designation, no carbon): undecided
  pd3 <- .peat427(des = c("Oi", "Oe", "Oe", NA), oc = c(40, 35, 30, NA))
  expect_true(is.na(sg_histosols_terric_usda(pd3)$passed))
})

test_that("Fluvaquentic Histosols: mineral layers inside the organic materials, below the surface tier", {
  skip_on_cran()
  pd <- .peat427(des = c("Oi", "Oe", "C", "Oe"), top = c(0, 30, 60, 70),
                 bot = c(30, 60, 70, 150), oc = c(40, 35, 1, 30))
  expect_true(isTRUE(sg_histosols_fluvaquentic_usda(pd)$passed))
  # the same mineral layer at the bottom (not inside the organic materials)
  expect_true(isFALSE(sg_histosols_fluvaquentic_usda(.peat427())$passed))
})

test_that("Hydric: a water layer counts only below the surface tier", {
  skip_on_cran()
  pd <- .peat427(des = c("Oi", "W", "Oe", "Oe"), oc = c(40, NA, 30, 30))
  expect_true(isTRUE(sg_histosols_hydric_usda(pd)$passed))
  pd2 <- .peat427(des = c("W", "Oi", "Oe", "Oe"), top = c(0, 20, 60, 100),
                  bot = c(20, 60, 100, 150), oc = c(NA, 40, 30, 30))
  expect_true(isFALSE(sg_histosols_hydric_usda(pd2)$passed))
})

test_that("Lithic Histosols: contact at the bottom of the control section (130 or 160 cm)", {
  skip_on_cran()
  pd <- .peat427(des = c("Oi", "Oe", "Oe", "R"), top = c(0, 30, 60, 120), bot = c(30, 60, 120, 150))
  expect_true(isTRUE(sg_histosols_lithic_usda(pd)$passed))
  # Folists: within 50 cm of the soil surface
  expect_true(isFALSE(sg_histosols_lithic_usda(pd, max_cm = 50)$passed))
  # a moss surface (three-fourths Sphagnum) makes the control section 160 cm
  pd2 <- .peat427(des = c("Oi", "Oi", "Oe", "R"), top = c(0, 30, 60, 150), bot = c(30, 60, 150, 170),
                  oc = c(45, 45, 30, NA), sphagnum_fibres_pct = c(90, 90, 10, NA))
  expect_true(isTRUE(sg_histosols_lithic_usda(pd2)$passed))
})

test_that("Hemic Sphagnofibrists and Haplofibrists count hemic and sapric materials", {
  skip_on_cran()
  pd <- .peat427(des = c("Oi", "Oi", "Oe", "Oa"), top = c(0, 30, 60, 75), bot = c(30, 60, 75, 150),
                 oc = c(45, 45, 35, 30))
  # 15 cm hemic + 55 cm sapric below the surface tier: Hemic
  expect_true(isTRUE(sg_histosols_kinds_usda(pd, kinds = c("hemic", "sapric"))$passed))
  expect_true(isFALSE(sg_histosols_kinds_usda(pd, kinds = "hemic")$passed))
  # kind not recorded: undecided
  pd2 <- .peat427(des = c("Oi", "Oi", "O", "O"), oc = c(45, 45, 35, 30))
  expect_true(is.na(sg_histosols_kinds_usda(pd2, kinds = c("hemic", "sapric"))$passed))
})

test_that("Fibric and Sapric Wassists read the predominance of the suborders", {
  skip_on_cran()
  pd <- .peat427(des = c("Oi", "Oi", "Oa", "Oi"), top = c(0, 30, 50, 60), bot = c(30, 50, 60, 150),
                 oc = c(45, 45, 30, 45))
  expect_true(isTRUE(sg_histosols_predominant_usda(pd, kind = "fibric")$passed))
  expect_true(isFALSE(sg_histosols_predominant_usda(pd, kind = "sapric")$passed))
})

test_that("Halic Haplosaprists: one layer 30 cm thick with EC 30 dS/m or more", {
  skip_on_cran()
  pd <- .peat427(des = c("Oa", "Oa", "Oa", "C"), ec_dS_m = c(10, 35, 40, 5))
  expect_true(isTRUE(sg_histosols_halic_usda(pd)$passed))
  # 20 cm measured, 30 cm not measured above it: undecided; measured low: no
  pd2 <- .peat427(des = c("Oa", "Oa", "Oa", "C"), bot = c(30, 60, 80, 150), top = c(0, 30, 60, 80),
                  ec_dS_m = c(10, NA, 40, 5))
  expect_true(is.na(sg_histosols_halic_usda(pd2)$passed))
  pd3 <- .peat427(des = c("Oa", "Oa", "Oa", "C"), bot = c(30, 60, 80, 150), top = c(0, 30, 60, 80),
                  ec_dS_m = c(10, 5, 40, 5))
  expect_true(isFALSE(sg_histosols_halic_usda(pd3)$passed))
})

test_that("Sphagnic Cryofibrists read the Sphagnum share, not the fibre content", {
  skip_on_cran()
  pd <- .peat427(des = c("Oi", "Oi", "Oe", "Oe"), fiber_content_rubbed_pct = c(90, 90, 40, 40))
  expect_true(isFALSE(sg_histosols_sphagnic_usda(pd)$passed))
  pd2 <- .peat427(des = c("Oi", "Oi", "Oe", "Oe"), sphagnum_fibres_pct = c(90, 90, NA, NA))
  expect_true(isTRUE(sg_histosols_sphagnic_usda(pd2)$passed))
})


# ---- Gelisols ----------------------------------------------------------

test_that("Fibristels and Hemistels by the kind of organic material to 50 cm (no SiBCS classes)", {
  skip_on_cran()
  hel <- function(des) .pd427(list(top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
                                    designation = des, oc_pct = c(40, 35, 30),
                                    permafrost_temp_C = c(1, -1, -2)))
  expect_true(isTRUE(sg_gelisols_histel_kind_usda(hel(c("Oi", "Oi", "Oe")), "fibric")$passed))
  expect_true(isTRUE(sg_gelisols_histel_kind_usda(hel(c("Oi", "Oe", "Oi")), "hemic")$passed))
  expect_true(is.na(sg_gelisols_histel_kind_usda(hel(c("Oi", "O", "Oe")), "fibric")$passed))
  rules <- load_rules("usda")
  gg <- unlist(lapply(rules$great_groups$AA, function(g) names(unlist(g$tests))))
  expect_false(any(grepl("fibrico|hemico|saprico", gg)))
})

test_that("Lithic Gelisols measure from the mineral soil surface outside the Histels", {
  skip_on_cran()
  pd <- .pd427(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 80),
                    designation = c("Oe", "Bw", "R"), oc_pct = c(30, 1, NA)))
  expect_true(isTRUE(sg_gelisols_lithic_usda(pd, max_cm = 50)$passed))
  expect_true(isFALSE(sg_gelisols_lithic_usda(pd, max_cm = 50, from = "soil")$passed))
})

test_that("Thapto-Humic Gelisols: buried dark layers, undecided without colour", {
  skip_on_cran()
  pd <- function(v) .pd427(list(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                                designation = c("A", "Ab", "Cf"), oc_pct = c(3, 2, 0.3),
                                munsell_value_moist = c(3, v, 4)))
  expect_true(isTRUE(sg_gelisols_thapto_humic_usda(pd(2))$passed))
  expect_true(isFALSE(sg_gelisols_thapto_humic_usda(pd(5))$passed))
  expect_true(is.na(sg_gelisols_thapto_humic_usda(pd(NA))$passed))
})


# ---- Spodosols ---------------------------------------------------------

.spod427 <- function(top = c(0, 5, 30, 90, 110), bot = c(5, 30, 90, 110, 150),
                     des = c("Oa", "A", "E", "Bs", "C"), sand = c(NA, 90, 92, 85, 90),
                     clay = c(NA, 3, 2, 5, 3), ...) {
  hz <- list(top_cm = top, bottom_cm = bot, designation = des,
             munsell_hue_moist = c("10YR", "10YR", "10YR", "7.5YR", "10YR"),
             munsell_value_moist = c(2, 3, 6, 3, 5), munsell_chroma_moist = c(1, 1, 1, 4, 3),
             munsell_value_dry = c(3, 4, 7, 4, 6),
             ph_h2o = c(4, 4.2, 4.3, 4.6, 5), oc_pct = c(30, 2, 0.3, 1.5, 0.2),
             clay_pct = clay, sand_pct = sand, silt_pct = 100 - sand - clay,
             al_ox_pct = c(NA, 0.05, 0.02, 0.8, 0.1), fe_ox_pct = c(NA, 0.05, 0.02, 0.05, 0.02))
  extra <- list(...)
  for (k in names(extra)) hz[[k]] <- extra[[k]]
  .pd427(hz)
}

test_that("Arenic Spodosols: sandy from the mineral soil surface to a spodic horizon at 75-125 cm", {
  skip_on_cran()
  pd <- .spod427()
  expect_true(isTRUE(spodic_horizon_usda(pd)$passed))
  expect_true(isTRUE(sg_spodosols_arenic_usda(pd)$passed))
  expect_true(isFALSE(sg_spodosols_grossarenic_usda(pd)$passed))
  # a texture not recorded above the spodic horizon: undecided (skipped before)
  pd2 <- .spod427(sand = c(NA, NA, 92, 85, 90), clay = c(NA, NA, 2, 5, 3))
  expect_true(is.na(sg_spodosols_arenic_usda(pd2)$passed))
  # a loamy layer above it: not Arenic
  pd3 <- .spod427(sand = c(NA, 40, 92, 85, 90), clay = c(NA, 20, 2, 5, 3))
  expect_true(isFALSE(sg_spodosols_arenic_usda(pd3)$passed))
})

test_that("Lithic Spodosols measure from the mineral soil surface", {
  skip_on_cran()
  pd <- .pd427(list(top_cm = c(0, 10, 25, 55), bottom_cm = c(10, 25, 55, 70),
                    designation = c("Oa", "E", "Bs", "R"), oc_pct = c(30, 0.5, 1.5, NA)))
  expect_true(isTRUE(sg_spodosols_lithic_usda(pd)$passed))
  expect_true(isFALSE(isTRUE(lithic_contact_usda(pd, max_top_cm = 50)$passed)))
})

test_that("Andic Spodosols: 25 cm of andic soil properties within 75 cm", {
  skip_on_cran()
  mk <- function(bd, al) .pd427(list(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100),
                                     designation = c("A", "Bs", "C"), oc_pct = c(5, 2, 0.5),
                                     bulk_density_g_cm3 = bd, phosphate_retention_pct = c(90, 90, 20),
                                     al_ox_pct = al, fe_ox_pct = c(1, 1, 0.1),
                                     volcanic_glass_pct = c(0, 0, 0)))
  expect_true(isTRUE(sg_spodosols_andic_usda(mk(c(0.7, 0.8, 1.4), c(2.5, 2.5, 0.1)))$passed))
  # only 20 cm andic: not Andic (the subgroups of the mineral orders want 18 cm)
  expect_true(isFALSE(sg_spodosols_andic_usda(mk(c(0.7, 1.2, 1.4), c(2.5, 0.3, 0.1)))$passed))
  # bulk density and NaF not recorded in the second layer: undecided
  expect_true(is.na(sg_spodosols_andic_usda(mk(c(0.7, NA, 1.4), c(2.5, 2.5, 0.1)))$passed))
})

test_that("Entic subgroups are called with the great group's rule; Haploplaggic is reachable", {
  skip_on_cran()
  rules <- load_rules("usda")
  rule_of <- function(code, name) {
    s <- Filter(function(x) x$name == name, rules$subgroups[[code]])[[1]]
    ent <- Filter(function(t) names(t)[1] == "entic_subgroup_usda", s$tests$all_of)
    ent[[1]][[1]]$rule
  }
  expect_equal(rule_of("CAA", "Entic Cryaquods"), "cryaquod")
  expect_equal(rule_of("CCD", "Entic Haplocryods"), "haplocryod")
  expect_equal(rule_of("CED", "Entic Alorthods"), "alorthod")
  expect_equal(rule_of("CED", "Entic Grossarenic Alorthods"), "alorthod")
  expect_equal(rule_of("CEC", "Entic Fragiorthods"), "orthod")
  expect_equal(rule_of("CEE", "Entic Haplorthods"), "orthod")
  expect_equal(rule_of("CEE", "Aquentic Haplorthods"), "orthod")
  for (code in c("CAC", "CDD", "CEC", "CED")) {
    s <- Filter(function(x) grepl("^Haploplaggic", x$name), rules$subgroups[[code]])[[1]]
    expect_equal(names(s$tests$all_of[[1]])[1], "haploplaggic_subgroup_usda")
  }
  for (n in c("Duric Alaquods", "Duric Cryaquods")) {
    code <- if (grepl("Alaquods", n)) "CAB" else "CAA"
    s <- Filter(function(x) x$name == n, rules$subgroups[[code]])[[1]]
    expect_equal(names(s$tests$all_of[[1]])[1], "cemented_horizon_usda")
  }
})

test_that("Turbic Gelods read gelic materials, not permafrost alone", {
  skip_on_cran()
  pd <- .pd427(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                    designation = c("E", "Bs", "Cf"), oc_pct = c(0.5, 2, 0.2),
                    permafrost_temp_C = c(2, 1, -2)))
  expect_true(isFALSE(sg_spodosols_turbic_usda(pd)$passed))
  pd2 <- .pd427(list(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                     designation = c("E", "Bsjj", "Cf"), oc_pct = c(0.5, 2, 0.2),
                     permafrost_temp_C = c(2, 1, -2)))
  expect_true(isTRUE(sg_spodosols_turbic_usda(pd2)$passed))
})
