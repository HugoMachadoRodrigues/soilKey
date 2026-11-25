# v0.9.439: the readers shared by both FAO-Unesco legends and the ISIS reader
# as the audit of the FAO legends read them (Hugo, 2026-11-24, option (a) of
# R12, R13, R14 and R16, with the defects E4 and E7): the exchangeable bases
# as a lower bound of the NH4Cl CEC and the ECEC (R12); secondary carbonate
# recorded without its form (R13); soft calcareous inclusions of ISIS as soft
# lime (E4, R14); duplicate rows of ISIS (E7); the cerrado phase from the
# ISIS text (R16). Every pedon and ISIS record below is synthetic.

.h439 <- function(...) {
  cols <- list(...)
  n <- max(lengths(cols))
  h <- make_empty_horizons(n)
  for (k in names(cols)) data.table::set(h, j = k, value = rep_len(cols[[k]], n))
  h
}
.p439 <- function(..., site = list(id = "p439")) {
  suppressWarnings(PedonRecord$new(site = site, horizons = .h439(...)))
}
.it439 <- function(x) list(item = x, description = NULL)
.isis439 <- function(layers, ...) {
  c(list(profileCode = "XX439", countryCode = NULL, layers = layers,
         wrbClassifications = list(), usdaClassifications = list()), list(...))
}

# ---- R12: the bases as a lower bound -----------------------------------------

test_that("R12: the bases bound the NH4Cl CEC only where they already fail the test", {
  skip_on_cran()
  # clay 40 %: bases 10 -> 25 per kg clay, 4 -> 10, 0.8 -> 2, 0.6 -> 1.5;
  # the last layer has its ECEC recorded (5 -> 12.5)
  h <- .h439(top_cm = c(0, 20, 40, 60, 80), bottom_cm = c(20, 40, 60, 80, 100),
             clay_pct = 40, ca_cmol = c(7, 1, 0.5, 0.3, 7), mg_cmol = c(2, 2, 0.2, 0.2, 2),
             k_cmol = c(0.5, 0.5, 0.05, 0.05, 0.5), na_cmol = c(0.5, 0.5, 0.05, 0.05, 0.5),
             ecec_cmol = c(NA, NA, NA, NA, 5))
  a <- soilKey:::.fao_cec_clay(h, "nh4cl")
  expect_equal(a$value, c(NA, NA, NA, NA, 12.5))
  b <- soilKey:::.fao_cec_clay(h, "nh4cl", bound = 24)
  expect_equal(b$value, c(25, NA, 5, 3.75, 12.5) * c(1, NA, NA, NA, 1))
  expect_identical(b$flags, c("bases_lower_bound_of_nh4cl_cec", NA, NA, NA, "ecec_for_nh4cl_cec"))
  # "1.5 or less" (the Acric Ferralsols): the bound decides above 1.5 only
  b <- soilKey:::.fao_cec_clay(h, "nh4cl", bound = 1.5, bound_op = "<=")
  expect_equal(b$value, c(25, 10, 2, NA, 12.5))
  # a "less than" limit is failed at the limit itself
  h2 <- .h439(top_cm = 0, bottom_cm = 20, clay_pct = 50, ca_cmol = 10, mg_cmol = 2,
              k_cmol = 0, na_cmol = 0)
  expect_equal(soilKey:::.fao_cec_clay(h2, "nh4cl", bound = 24)$value, 24)
  expect_true(is.na(soilKey:::.fao_cec_clay(h2, "nh4cl", bound = 24, bound_op = "<=")$value))
  # NH4OAc is never bounded
  expect_true(is.na(soilKey:::.fao_cec_clay(h2, "nh4oac")$value))
})

test_that("R12: the bases bound the ECEC of 1988 where acidity, Al and ECEC are missing", {
  skip_on_cran()
  h <- .h439(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60), clay_pct = 50,
             ca_cmol = c(5, 6.5, 5), mg_cmol = c(1, 0, 1), k_cmol = 0, na_cmol = 0,
             exch_acidity_kcl_cmol = c(NA, NA, 1))
  e <- soilKey:::.fao_ecec_clay(h, "1988")
  expect_equal(e$value, c(NA, NA, 14))
  e <- soilKey:::.fao_ecec_clay(h, "1988", bound = 12, bound_op = "<=")
  expect_equal(e$value, c(NA, 13, 14))            # 12 is not above 12
  expect_identical(e$flags, c(NA, "bases_lower_bound_of_ecec", NA))
})

test_that("R12: the ferralic, ferric and Acric tests of 1974 and the ferralic B of 1988 decide on the bound", {
  skip_on_cran()
  # a Bw of 50 % clay with no ECEC, no Al, no acidity and no NH4Cl CEC
  bw <- function(bases_ca, mg_cmol = 1, k_cmol = 0.2, na_cmol = 0.1, ...)
    .p439(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150), designation = c("A", "Bw", "C"),
          clay_pct = 50, silt_pct = 10, sand_pct = 40, ca_cmol = bases_ca, mg_cmol = mg_cmol,
          k_cmol = k_cmol, na_cmol = na_cmol, ...)
  # bases 1.8 per kg soil = 3.6 per kg clay: the Acric CEC (1.5 or less) fails
  r <- acric_cec_fao74(bw(0.5))
  expect_false(r$passed)
  expect_true("bases_lower_bound_of_nh4cl_cec" %in% r$evidence$flags)
  # bases 0.6 per kg soil = 1.2 per kg clay: still undecided
  expect_true(is.na(acric_cec_fao74(bw(0.4, mg_cmol = 0.1, k_cmol = 0.05, na_cmol = 0.05))$passed))
  # bases 13.3 = 26.6 per kg clay: the NH4Cl CEC (below 24) of the ferric
  # properties fails; with no mottles or nodules recorded, they fail
  r <- ferric_properties_fao74(.p439(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
                                     designation = c("A", "Bt", "C"), clay_pct = 50, silt_pct = 10,
                                     sand_pct = 40, ca_cmol = 12, mg_cmol = 1, k_cmol = 0.2, na_cmol = 0.1,
                                     clay_films_amount = c(NA, "common", NA), redoximorphic_features_pct = 0))
  expect_false(r$passed)
  # the ferralic B of 1988: CEC 16.2 per kg clay fails item 3 by NH4OAc;
  # bases above 12 per kg clay fail the ECEC route
  f88 <- function(ca) .p439(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 150),
                            designation = c("A", "Bw", "C"), clay_pct = c(50, 60, 60),
                            silt_pct = c(8, 6, 6), sand_pct = c(42, 34, 34),
                            cec_cmol = c(10, 9.7, 10), ca_cmol = c(1, ca, 1), mg_cmol = 1,
                            k_cmol = 0.1, na_cmol = 0.1,
                            weatherable_minerals_pct = 1, water_dispersible_clay_pct = 0,
                            al_ox_pct = 0.2, fe_ox_pct = 0.4)
  expect_false(ferralic_b_fao88(f88(7))$passed)              # bases 8.2 = 13.7 per kg clay
  expect_true(is.na(ferralic_b_fao88(f88(5))$passed))         # bases 6.2 = 10.3: undecided
})

# ---- R13: secondary carbonate without its form -------------------------------

test_that("R13: 5 % or more secondary carbonate with no form leaves soft powdery lime undecided", {
  skip_on_cran()
  one <- function(...) .p439(top_cm = c(0, 30), bottom_cm = c(30, 130), designation = c("A", "Bk"),
                             caco3_pct = c(1, 20), ...)
  r <- soft_powdery_lime_fao88(one(secondary_carbonates_pct = c(NA, 10)))
  expect_true(is.na(r$passed))
  expect_true("secondary_carbonate_without_form" %in% r$evidence$flags)
  # below 5 %, a recorded form that is not soft, or an amount of soft nodules
  # recorded: decided, as before
  expect_false(soft_powdery_lime_fao88(one(secondary_carbonates_pct = c(NA, 3)))$passed)
  expect_false(soft_powdery_lime_fao88(one(secondary_carbonates_pct = c(NA, 10),
                                           secondary_carbonates_form = c(NA, "hard concretions")))$passed)
  expect_false(soft_powdery_lime_fao88(one(secondary_carbonates_pct = c(NA, 10),
                                           carbonate_soft_pct = c(NA, 2)))$passed)
  expect_true(soft_powdery_lime_fao88(one(secondary_carbonates_pct = c(NA, 10),
                                          carbonate_soft_pct = c(NA, 10)))$passed)
  # the Legend of 1974 shares the reader
  expect_true(is.na(soft_powdery_lime_fao74(one(secondary_carbonates_pct = c(NA, 10)))$passed))
})

# ---- E4, R14: soft calcareous inclusions of ISIS -------------------------------

test_that("R14: ISIS soft calcareous inclusions are soft nodules of lime, flagged", {
  skip_on_cran()
  K <- .it439("K")
  lay <- function(top, bot, ...) c(list(upperDepth = top, lowerDepth = bot, layerName = "Bk",
                                        inclusionsComposition = K, mineralConcAbundance = .it439("C")), list(...))
  L <- list(
    list(upperDepth = 0, lowerDepth = 20, layerName = "A"),
    lay(20, 40, concretionsMineralHardness = .it439("S"), inclusionsType = .it439("N")),   # soft nodules
    lay(40, 60, concretionsMineralHardness = .it439("S"), inclusionsType = .it439("T")),   # soft crystals: no
    lay(60, 80, inclusionsType = .it439("S")),                                            # soft segregations
    lay(80, 100, concretionsMineralHardness = .it439("H"), inclusionsType = .it439("C")),  # hard: no
    lay(100, 120, inclusionsType = .it439("C")),                                          # no hardness: no
    # the second inclusion reads its own type and hardness
    lay(120, 140, concretionsMineralHardness = .it439("H"), inclusionsType = .it439("C"),
        inclusionsComposition2 = K, mineralConcAbundance2 = .it439("F"),
        concretionsMineralHardness2 = .it439("S"), inclusionsType2 = .it439("N")))
  p <- suppressWarnings(isis_profile_to_pedon(.isis439(L)))
  h <- as.data.frame(p$horizons)
  expect_equal(h$secondary_carbonates_pct, c(NA, 10, 10, 10, 10, 10, 13.5))
  expect_equal(h$carbonate_soft_pct, c(NA, 10, NA, 10, NA, NA, 3.5))
  pv <- as.data.frame(p$provenance)
  expect_true(any(pv$attribute == "carbonate_soft_pct" & grepl("R14", pv$notes)))
})

test_that("R13 and R14 together: soft inclusions decide, others of 5 % or more stay undecided", {
  skip_on_cran()
  K <- .it439("K")
  prof <- function(...) .isis439(list(
    list(upperDepth = 0, lowerDepth = 30, layerName = "A", carbonateReaction = .it439("N")),
    c(list(upperDepth = 30, lowerDepth = 130, layerName = "Bk", inclusionsComposition = K,
           mineralConcAbundance = .it439("C")), list(...))))
  sp <- function(pr) soft_powdery_lime_fao88(suppressWarnings(isis_profile_to_pedon(pr)))$passed
  expect_true(sp(prof(concretionsMineralHardness = .it439("S"), inclusionsType = .it439("N"))))
  expect_true(is.na(sp(prof(concretionsMineralHardness = .it439("H"), inclusionsType = .it439("C")))))
  # a soft inclusion below 5 % is no soft powdery lime
  few <- prof(concretionsMineralHardness = .it439("S"), inclusionsType = .it439("N"))
  few$layers[[2]]$mineralConcAbundance <- .it439("F")
  expect_false(sp(few))
})

# ---- E7: duplicate rows of ISIS ----------------------------------------------

test_that("E7: a named row with no data takes its unnamed data row of the same depths", {
  skip_on_cran()
  L <- list(
    list(upperDepth = 0, lowerDepth = 20, layerName = "Ah", clay = list("20")),
    list(upperDepth = 20, lowerDepth = 40, layerName = "E"),
    list(upperDepth = 20, lowerDepth = 40, clay = list("15"), phaq = list("5.5")),
    list(upperDepth = 40, lowerDepth = 80, layerName = "Btg"),
    list(upperDepth = 40, lowerDepth = 80, clay = list("35")),
    # not an exact pair: two unnamed data rows at the same depths stay as they are
    list(upperDepth = 80, lowerDepth = 100, layerName = "C"),
    list(upperDepth = 80, lowerDepth = 100, clay = list("30")),
    list(upperDepth = 80, lowerDepth = 100, clay = list("31")))
  g <- soilKey:::.isis_guard_layers(L)
  expect_length(g$layers, 6L)
  nm <- vapply(g$layers, function(z) as.character(z$layerName %||% NA), "")
  expect_identical(nm, c("Ah", "E", "Btg", "C", NA, NA))
  expect_equal(soilKey:::.isis_num(g$layers[[2]]$clay), 15)
  expect_true(any(grepl("moved onto its data row", g$notes)))
  p <- suppressWarnings(isis_profile_to_pedon(.isis439(L[1:5])))
  expect_identical(p$horizons$designation, c("Ah", "E", "Btg"))
  expect_equal(p$horizons$clay_pct, c(20, 15, 35))
})

# ---- R16: the cerrado phase from ISIS text -------------------------------------

test_that("R16: the site's vegetation is cerrado where ISIS names it, and the phase reads it", {
  skip_on_cran()
  L <- list(list(upperDepth = 0, lowerDepth = 20, layerName = "A", clay = list("40")),
            list(upperDepth = 20, lowerDepth = 200, layerName = "Bw", clay = list("60")))
  p <- suppressWarnings(isis_profile_to_pedon(.isis439(L,
         landuseRemarks = "VEGETATION: Cerrado (savanna) LANDUSE: none")))
  expect_identical(p$site$vegetation, "cerrado")
  p2 <- suppressWarnings(isis_profile_to_pedon(.isis439(L,
          vegetationClassification = list(item = "X", description = "Campo cerrado"))))
  expect_identical(p2$site$vegetation, "cerrado")
  p3 <- suppressWarnings(isis_profile_to_pedon(.isis439(L, landuseRemarks = "VEGETATION: forest")))
  expect_null(p3$site$vegetation)
  r <- suppressWarnings(classify_fao(p, edition = "1974", on_missing = "silent"))
  expect_identical(r$phases$status[r$phases$code == "cerrado" & r$phases$kind == "phase"], "present")
  r3 <- suppressWarnings(classify_fao(p3, edition = "1974", on_missing = "silent"))
  expect_identical(r3$phases$status[r3$phases$code == "cerrado" & r3$phases$kind == "phase"], "absent")
})

# ---- the canonical fixtures ----------------------------------------------------

test_that("the canonical fixtures that move: the Retisol, the Luvisol, the Chernozem, the Kastanozem", {
  skip_on_cran()
  f <- function(p, ed) suppressWarnings(classify_fao(p, edition = ed, on_missing = "silent"))
  # R12: the bases (above 24 per kg clay) rule out the ferric properties of
  # the argillic B, so the Nitosols gate passes (was Albic Luvisols, grade C)
  r <- f(make_retisol_canonical(), "1974")
  expect_identical(r$name, "Eutric Nitosols")
  expect_identical(r$evidence_grade, "A")
  # R12: the Ferric Luvisols are no longer undecided before the Orthic
  r <- f(make_luvisol_canonical(), "1974")
  expect_identical(r$name, "Orthic Luvisols")
  expect_false("units:Lf" %in% r$provisional_because)
  # R13: secondary carbonate with no form leaves the Chernozems (the
  # Chernozem) and the Calcisols (the Kastanozem) undecided before the name,
  # which stays, at grade C
  r <- f(make_chernozem_canonical(), "1988")
  expect_identical(r$name, "Haplic Phaeozems")
  expect_true("major_groups:CH" %in% r$provisional_because)
  expect_identical(r$evidence_grade, "C")
  r <- f(make_kastanozem_canonical(), "1988")
  expect_identical(r$name, "Eutric Cambisols")
  expect_true("major_groups:CL" %in% r$provisional_because)
  expect_identical(r$evidence_grade, "C")
})

test_that("R12, R13, R14 and R16 are recorded in the readings of both editions", {
  skip_on_cran()
  for (e in c("fao1974", "fao1988")) {
    rd <- yaml::read_yaml(system.file("rules", e, "readings.yaml", package = "soilKey"))$readings
    v <- Filter(function(z) identical(z$version, "0.9.439"), rd)
    expect_length(v, 4L)
    txt <- vapply(v, function(z) z$reading, "")
    expect_true(all(grepl("option a) (Hugo, 2026-11-24)", txt, fixed = TRUE)), info = e)
    for (k in c("R12", "R13", "R14", "R16"))
      expect_true(any(grepl(k, txt, fixed = TRUE)), info = paste(e, k))
  }
})
