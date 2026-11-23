# v0.9.422 (Hugo, 2026-11-20; audit items R3 to R6, G7, F16, F17): the
# subgroup lists of the Alfisols rebuilt from the KST 13th edition (Ch. 5,
# pp. 73-117): every great group has the KST list, in the order of its key,
# Typic last, and each subgroup reads its own criteria (R/subgroups-usda-
# alfisols.R), three-valued.

.v422_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = c(list(id = "v422", country = "XX"), site),
                  horizons = ensure_horizon_schema(hz))
}

# A loamy Alfisol: A over a Bt with clay films from 30 cm.
.v422_alf <- function(top = c(0, 15, 30, 70), bottom = c(15, 30, 70, 160),
                      desg = c("A", "E", "Bt1", "Bt2"), clay = c(12, 11, 25, 26),
                      sand = 100 - 30 - clay, ..., site = list()) {
  cols <- list(top_cm = top, bottom_cm = bottom, designation = desg,
               clay_pct = clay, silt_pct = 100 - sand - clay, sand_pct = sand,
               clay_films_amount = ifelse(grepl("t", desg), "common", NA),
               munsell_hue_moist = "10YR", munsell_value_moist = 4,
               munsell_chroma_moist = 4, bs_pct = 70, cec_cmol = 12)
  extra <- list(...)
  cols[names(extra)] <- extra
  hz <- do.call(data.table::data.table, cols)
  PedonRecord$new(site = c(list(id = "v422", country = "XX"), site),
                  horizons = ensure_horizon_schema(hz))
}

# ---- The lists are the KST lists ------------------------------------------

test_that("each Alfisol great group has the KST 13th edition subgroup list", {
  skip_on_cran()
  kst <- list(
    Cryaqualfs = c("Typic"),
    Plinthaqualfs = c("Typic"),
    Duraqualfs = c("Typic"),
    Natraqualfs = c("Vertic", "Vermic", "Albic Glossic", "Albic", "Glossic", "Mollic", "Typic"),
    Fragiaqualfs = c("Vermic", "Aeric", "Plinthic", "Humic", "Typic"),
    Kandiaqualfs = c("Arenic", "Grossarenic", "Plinthic", "Aeric Umbric", "Aeric", "Umbric", "Typic"),
    Vermaqualfs = c("Natric", "Typic"),
    Albaqualfs = c("Arenic", "Aeric Vertic", "Chromic Vertic", "Vertic", "Udollic", "Aeric", "Aquandic", "Mollic", "Umbric", "Typic"),
    Glossaqualfs = c("Histic", "Arenic", "Aeric Fragic", "Fragic", "Aeric", "Mollic", "Typic"),
    Epiaqualfs = c("Aeric Chromic Vertic", "Aeric Vertic", "Chromic Vertic", "Vertic", "Aquandic", "Aeric Fragic", "Fragic", "Arenic", "Grossarenic", "Aeric Umbric", "Udollic", "Aeric", "Mollic", "Umbric", "Typic"),
    Endoaqualfs = c("Aquandic", "Chromic Vertic", "Vertic", "Aeric Fragic", "Fragic", "Arenic", "Grossarenic", "Udollic", "Aeric Umbric", "Aeric", "Mollic", "Umbric", "Typic"),
    Palecryalfs = c("Andic", "Vitrandic", "Aquic", "Oxyaquic", "Xeric", "Ustic", "Mollic", "Umbric", "Typic"),
    Glossocryalfs = c("Lithic", "Vertic", "Andic", "Vitrandic", "Aquic", "Oxyaquic", "Fragic", "Xerollic", "Umbric Xeric", "Ustollic", "Xeric", "Ustic", "Mollic", "Umbric", "Eutric", "Typic"),
    Haplocryalfs = c("Lithic", "Vertic", "Andic", "Vitrandic", "Aquic", "Oxyaquic", "Lamellic", "Psammentic", "Inceptic", "Xerollic", "Umbric Xeric", "Ustollic", "Xeric", "Ustic", "Mollic", "Umbric", "Eutric", "Typic"),
    Durustalfs = c("Typic"),
    Plinthustalfs = c("Typic"),
    Natrustalfs = c("Salidic", "Leptic Torrertic", "Torrertic", "Aquertic", "Aridic Leptic", "Vertic", "Aquic Arenic", "Aquic", "Arenic", "Petrocalcic", "Leptic", "Haplargidic", "Aridic Glossic", "Aridic", "Mollic", "Typic"),
    Kandiustalfs = c("Grossarenic", "Aquic Arenic", "Plinthic", "Aquic", "Arenic Aridic", "Arenic", "Aridic", "Udic", "Rhodic", "Typic"),
    Kanhaplustalfs = c("Lithic", "Aquic", "Aridic", "Udic", "Rhodic", "Typic"),
    Paleustalfs = c("Aquertic", "Oxyaquic Vertic", "Udertic", "Vertic", "Aquic Arenic", "Aquic", "Oxyaquic", "Lamellic", "Psammentic", "Arenic Aridic", "Grossarenic", "Arenic", "Plinthic", "Petrocalcic", "Calcidic", "Aridic", "Kandic", "Rhodic", "Ultic", "Udic", "Typic"),
    Rhodustalfs = c("Lithic", "Kanhaplic", "Udic", "Typic"),
    Haplustalfs = c("Lithic", "Aquertic", "Oxyaquic Vertic", "Torrertic", "Udertic", "Vertic", "Aquic Arenic", "Aquultic", "Aquic", "Oxyaquic", "Vitrandic", "Lamellic", "Psammentic", "Arenic Aridic", "Arenic", "Calcidic", "Aridic", "Kanhaplic", "Inceptic", "Calcic Udic", "Ultic", "Calcic", "Udic", "Typic"),
    Durixeralfs = c("Natric", "Vertic", "Aquic", "Abruptic Haplic", "Abruptic", "Haplic", "Typic"),
    Natrixeralfs = c("Vertic", "Aquic", "Typic"),
    Fragixeralfs = c("Andic", "Vitrandic", "Mollic", "Aquic", "Inceptic", "Typic"),
    Plinthoxeralfs = c("Typic"),
    Rhodoxeralfs = c("Lithic", "Vertic", "Petrocalcic", "Calcic", "Inceptic", "Typic"),
    Palexeralfs = c("Vertic", "Aquandic", "Andic", "Vitrandic", "Fragiaquic", "Aquic", "Petrocalcic", "Lamellic", "Psammentic", "Arenic", "Natric", "Fragic", "Calcic", "Plinthic", "Ultic", "Haplic", "Mollic", "Typic"),
    Haploxeralfs = c("Lithic Mollic", "Lithic Ruptic-Inceptic", "Lithic", "Vertic", "Aquandic", "Andic", "Vitrandic", "Fragiaquic", "Aquultic", "Aquic", "Natric", "Fragic", "Lamellic", "Psammentic", "Plinthic", "Calcic", "Inceptic", "Ultic", "Mollic", "Typic"),
    Natrudalfs = c("Vertic", "Glossaquic", "Aquic", "Typic"),
    Ferrudalfs = c("Aquic", "Typic"),
    Fraglossudalfs = c("Andic", "Vitrandic", "Aquic", "Oxyaquic", "Typic"),
    Fragiudalfs = c("Andic", "Vitrandic", "Aquic", "Oxyaquic", "Typic"),
    Kandiudalfs = c("Plinthaquic", "Aquic", "Oxyaquic", "Arenic Plinthic", "Grossarenic Plinthic", "Arenic", "Grossarenic", "Plinthic", "Rhodic", "Mollic", "Typic"),
    Kanhapludalfs = c("Lithic", "Aquic", "Oxyaquic", "Rhodic", "Typic"),
    Paleudalfs = c("Vertic", "Andic", "Vitrandic", "Anthraquic", "Fragiaquic", "Plinthaquic", "Glossaquic", "Albaquic", "Aquic", "Oxyaquic", "Fragic", "Arenic Plinthic", "Grossarenic Plinthic", "Lamellic", "Psammentic", "Arenic", "Grossarenic", "Plinthic", "Glossic", "Rhodic", "Mollic", "Typic"),
    Rhodudalfs = c("Typic"),
    Glossudalfs = c("Aquertic", "Oxyaquic Vertic", "Vertic", "Aquandic", "Andic", "Vitrandic", "Fragiaquic", "Aquic Arenic", "Aquic", "Arenic Oxyaquic", "Oxyaquic", "Fragic", "Arenic", "Haplic", "Typic"),
    Hapludalfs = c("Lithic", "Aquertic Chromic", "Aquertic", "Oxyaquic Vertic", "Chromic Vertic", "Vertic", "Andic", "Vitrandic", "Fragiaquic", "Fragic Oxyaquic", "Aquic Arenic", "Arenic Oxyaquic", "Anthraquic", "Albaquultic", "Albaquic", "Glossaquic", "Aquultic", "Aquollic", "Aquic", "Mollic Oxyaquic", "Oxyaquic", "Fragic", "Lamellic", "Psammentic", "Arenic", "Glossic", "Inceptic", "Ultic", "Mollic", "Typic")
  )
  rules <- load_rules("usda")
  gl <- unlist(rules$great_groups[c("JA", "JB", "JC", "JD", "JE")], recursive = FALSE,
               use.names = FALSE)
  gg <- stats::setNames(vapply(gl, `[[`, "", "code"), vapply(gl, `[[`, "", "name"))
  expect_setequal(names(gg), names(kst))
  for (g in names(kst)) {
    sg <- rules$subgroups[[gg[[g]]]]
    got <- vapply(sg, `[[`, "", "name")
    expect_identical(got, paste(kst[[g]], g), info = g)
    expect_true(isTRUE(sg[[length(sg)]]$tests$default), info = g)
    codes <- vapply(sg, `[[`, "", "code")
    expect_false(anyDuplicated(codes) > 0, info = g)
  }
  expect_equal(sum(lengths(kst)), 356L)
})

test_that("the Alfisol subgroup names and codes are those of the KST code list", {
  skip_on_cran()
  y <- yaml::read_yaml(system.file("rules", "usda", "subgroups", "alfisols.yaml",
                                   package = "soilKey"))$subgroups
  nm <- unlist(lapply(y, function(g) vapply(g, `[[`, "", "name")))
  k <- kst13_codes()
  a <- k[substr(k$code, 1, 1) == "J" & nchar(k$code) >= 4, ]
  expect_setequal(nm, a$name)
  # the codes keep the KST letters after the great-group code
  cd <- unlist(lapply(y, function(g) vapply(g, `[[`, "", "code")))
  kc <- a$code[match(nm, a$name)]
  expect_identical(unname(substring(cd, 4)), substring(kc, 4))
})

test_that("every test the Alfisol subgroup list names is a function of soilKey", {
  skip_on_cran()
  y <- yaml::read_yaml(system.file("rules", "usda", "subgroups", "alfisols.yaml",
                                   package = "soilKey"))$subgroups
  fns <- unique(unlist(lapply(y, function(g) lapply(g, function(s)
    vapply(s$tests$all_of %||% list(), function(t) names(t)[1], "")))))
  ns <- asNamespace("soilKey")
  expect_true(all(vapply(fns, exists, NA, envir = ns, inherits = FALSE)))
})

# ---- Criteria -----------------------------------------------------------

test_that("Aquic reads the zone of the great group (KST pp. 85, 92, 99)", {
  skip_on_cran()
  p <- .v422_alf(desg = c("A", "E", "Bt1", "Btg2"))
  # a g horizon from 70 cm: within 75 cm, not within 60 cm
  expect_true(soilKey:::sg_alfisols_aquic_usda(p, "surface", 75)$passed)
  expect_false(soilKey:::sg_alfisols_aquic_usda(p, "surface", 60)$passed)
  # Hapludalfs: the argillic horizon starts at 30 cm, so its upper 25 cm
  expect_false(soilKey:::sg_alfisols_aquic_usda(p, "relative")$passed)
  # redox features in a matrix whose chroma is not recorded: undecided
  q <- .v422_alf(redoximorphic_features_pct = c(0, 0, 10, 0))
  q$horizons$munsell_chroma_moist[3] <- NA
  r <- soilKey:::sg_alfisols_aquic_usda(q, "surface", 75)
  expect_true(is.na(r$passed))
  expect_true("munsell_chroma_moist" %in% r$missing)
  # mottles of chroma 2 are depletions
  m <- .v422_alf(mottle_colors = c(NA, NA, "10YR 6/2", NA))
  expect_true(soilKey:::sg_alfisols_aquic_usda(m, "upper25")$passed)
})

test_that("Arenic reads the argillic or kandic horizon, not the spodic one (F16)", {
  skip_on_cran()
  p <- .v422_alf(top = c(0, 30, 60, 100), bottom = c(30, 60, 100, 160),
                 desg = c("A", "E", "Bt1", "Bt2"), clay = c(4, 4, 20, 22),
                 sand = c(88, 88, 50, 50))
  expect_true(soilKey:::sg_alfisols_arenic_usda(p, "argillic", 50)$passed)
  expect_true(soilKey:::sg_alfisols_arenic_usda(p, "argillic", 50, 100)$passed)
  expect_false(soilKey:::sg_alfisols_arenic_usda(p, "argillic", 100)$passed)
  # a loamy layer above it fails the texture
  q <- .v422_alf(top = c(0, 30, 60, 100), bottom = c(30, 60, 100, 160),
                 desg = c("A", "E", "Bt1", "Bt2"), clay = c(4, 18, 30, 32),
                 sand = c(88, 50, 40, 40))
  expect_false(soilKey:::sg_alfisols_arenic_usda(q, "argillic", 50)$passed)
  # the old helper looked for a spodic horizon and never passed here
  expect_false(isTRUE(soilKey:::arenic_subgroup_usda(p, 50, Inf)$passed))
})

test_that("Vertic: slickensides stand for unrecorded cracks; no crack or slickenside, no COLE: FALSE", {
  skip_on_cran()
  p <- .v422_alf(slickensides = c("absent", "absent", "common", "common"))
  r <- soilKey:::sg_alfisols_vertic_usda(p)
  expect_true(r$passed)
  expect_true(r$evidence$cracks_from_slickensides)
  q <- .v422_alf(slickensides = "absent")
  r <- soilKey:::sg_alfisols_vertic_usda(q)
  # v0.9.428: no sign of shrink and swell recorded, COLE not recorded: FALSE
  expect_false(r$passed)
  expect_false("cole_value" %in% r$missing)
  q2 <- .v422_alf(slickensides = "absent", cole_value = 0.01)
  expect_false(soilKey:::sg_alfisols_vertic_usda(q2)$passed)
  q3 <- .v422_alf(slickensides = "absent", cole_value = 0.07)
  expect_true(soilKey:::sg_alfisols_vertic_usda(q3)$passed)
  # cracks recorded as none: the slickensides alone do not pass
  q4 <- .v422_alf(slickensides = "common", cracks_width_cm = 0, cole_value = 0.01)
  expect_false(soilKey:::sg_alfisols_vertic_usda(q4)$passed)
})

test_that("Chromic mixes the Ap or the upper 18 cm", {
  skip_on_cran()
  p <- .v422_alf()
  expect_true(soilKey:::sg_alfisols_chromic_usda(p)$passed)
  d <- .v422_alf(munsell_value_moist = 3, munsell_chroma_moist = 2, munsell_value_dry = 5)
  expect_false(soilKey:::sg_alfisols_chromic_usda(d)$passed)
  d$horizons$munsell_value_dry <- NA
  expect_true(is.na(soilKey:::sg_alfisols_chromic_usda(d)$passed))
})

test_that("Aeric of the Aqualfs: colours below the A horizon, dry chroma where the text asks", {
  skip_on_cran()
  p <- .v422_alf(desg = c("A", "Eg", "Btg1", "Btg2"),
                 munsell_chroma_moist = c(2, 2, 4, 2), munsell_chroma_dry = NA_real_)
  expect_true(soilKey:::sg_alfisols_aeric_usda(p, "aqualf")$passed)
  # Epiaqualfs read the chroma moist and dry: the dry one is not recorded
  expect_true(is.na(soilKey:::sg_alfisols_aeric_usda(p, "aqualf", TRUE, TRUE)$passed))
  # Albaqualfs: chroma 3 or more in 40 % of the matrix between the A and 75 cm
  expect_true(soilKey:::sg_alfisols_aeric_usda(p, "albaqualf")$passed)
  g <- .v422_alf(desg = c("A", "Eg", "Btg1", "Btg2"), munsell_chroma_moist = 1)
  expect_false(soilKey:::sg_alfisols_aeric_usda(g, "aqualf")$passed)
  expect_false(soilKey:::sg_alfisols_aeric_usda(g, "albaqualf")$passed)
})

test_that("Fragic reads fragic soil properties (x layers), not a firm consistence", {
  skip_on_cran()
  p <- .v422_alf(desg = c("A", "E", "Bt", "Btx"))
  expect_true(soilKey:::sg_alfisols_fragic_usda(p)$passed)
  q <- .v422_alf(rupture_resistance = "firm")
  expect_false(soilKey:::sg_alfisols_fragic_usda(q)$passed)
})

test_that("Eutric Cryalfs read the base saturation to 180 cm, a short profile as an inference", {
  skip_on_cran()
  p <- .v422_alf()
  r <- soilKey:::sg_alfisols_bs_usda(p, "eutric")
  expect_true(r$passed)
  expect_true(length(r$evidence$inferences) == 1L)
  q <- .v422_alf(bs_pct = c(70, 70, 40, 60))
  expect_false(soilKey:::sg_alfisols_bs_usda(q, "eutric")$passed)
  # Ultic Hapludalfs: the base saturation at 125 cm below the argillic top
  u <- .v422_alf(top = c(0, 15, 30, 70, 150), bottom = c(15, 30, 70, 150, 200),
                 desg = c("A", "E", "Bt1", "Bt2", "BC"), clay = c(12, 11, 25, 26, 24),
                 bs_pct = c(70, 60, 60, 55, 50))
  expect_true(soilKey:::sg_alfisols_bs_usda(u, "udalf")$passed)
})

test_that("the ustic intergrades need the intergrade recorded", {
  skip_on_cran()
  mk <- function(r) .v422_alf(site = list(soil_moisture_regime = r))
  expect_true(is.na(soilKey:::sg_alfisols_regime_usda(mk("ustic"), "aridic")$passed))
  expect_true(soilKey:::sg_alfisols_regime_usda(mk("aridic ustic"), "aridic")$passed)
  expect_false(soilKey:::sg_alfisols_regime_usda(mk("udic ustic"), "aridic")$passed)
  expect_true(soilKey:::sg_alfisols_regime_usda(mk("udic ustic"), "udic")$passed)
  expect_false(soilKey:::sg_alfisols_regime_usda(mk("typic ustic"), "udic")$passed)
  # Ustic Cryalfs: 45 dry days
  expect_true(soilKey:::sg_alfisols_regime_usda(mk("xeric"), "dry45")$passed)
  expect_true(is.na(soilKey:::sg_alfisols_regime_usda(mk("udic"), "dry45")$passed))
  expect_true(is.na(soilKey:::sg_alfisols_regime_usda(mk(NA), "xeric")$passed))
})

test_that("Kanhaplic: CEC under 24 per kg clay in half the argillic horizon", {
  skip_on_cran()
  p <- .v422_alf(cec_cmol = c(8, 8, 5, 5))
  expect_true(soilKey:::sg_alfisols_kanhaplic_usda(p)$passed)
  q <- .v422_alf(cec_cmol = c(8, 8, 12, 12))
  expect_false(soilKey:::sg_alfisols_kanhaplic_usda(q)$passed)
  q$horizons$cec_cmol[4] <- NA
  expect_true(is.na(soilKey:::sg_alfisols_kanhaplic_usda(q)$passed))
})

test_that("Rhodic reads the upper 75 or 50 cm of the horizon and the dry value", {
  skip_on_cran()
  p <- .v422_alf(munsell_hue_moist = c("10YR", "10YR", "2.5YR", "2.5YR"),
                 munsell_value_moist = c(4, 4, 3, 3), munsell_value_dry = c(5, 5, 4, 4))
  expect_true(soilKey:::sg_alfisols_rhodic_usda(p, "argillic", 75)$passed)
  p$horizons$munsell_value_dry[3] <- NA
  expect_true(is.na(soilKey:::sg_alfisols_rhodic_usda(p, "argillic", 75)$passed))
  p$horizons$munsell_value_moist[4] <- 4
  expect_false(soilKey:::sg_alfisols_rhodic_usda(p, "argillic", 75)$passed)
})

test_that("compound subgroups come before their parts and are reachable", {
  skip_on_cran()
  p <- .v422_pedon(
    top_cm = c(0, 30, 60, 90), bottom_cm = c(30, 60, 90, 160),
    designation = c("A1", "A2", "Btg1", "Bt2"), clay_pct = c(5, 5, 25, 25),
    silt_pct = 10, sand_pct = c(85, 85, 65, 65),
    clay_films_amount = c(NA, NA, "common", "common"),
    munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 5, 5),
    munsell_chroma_moist = 4, munsell_value_dry = c(6, 6, 7, 7),
    munsell_chroma_dry = 4, mottle_colors = c(NA, NA, "10YR 6/2", NA),
    bs_pct = 70, cec_cmol = 12, slickensides = "absent",
    site = list(soil_moisture_regime = "udic", soil_temperature_regime = "mesic"))
  r <- classify_usda(p)
  expect_match(r$name, "Aquic Arenic Hapludalfs$")
  # the subgroups tested before it are undecided or false, never true
  tr <- r$trace$subgroups
  before <- tr[seq_len(which(names(tr) == r$trace$subgroups[[length(tr)]]$code) - 1L)]
  expect_false(any(vapply(before, function(x) isTRUE(x$passed), NA)))
})

test_that("Inceptic: a thin argillic horizon, ended by a layer that cannot be argillic", {
  skip_on_cran()
  mk <- function(d3, b2 = 120, c3 = 12) .v422_alf(top = c(0, 15, 25, 45), bottom = c(15, 25, 45, b2),
                                         desg = c("A", "E", "Bt", d3), clay = c(12, 11, 25, c3))
  expect_true(soilKey:::sg_alfisols_inceptic_usda(mk("C"))$passed)
  # a BC (no clay increase) with no clay films recorded could go on with the
  # argillic horizon
  expect_true(is.na(soilKey:::sg_alfisols_inceptic_usda(mk("BC"))$passed))
  # a thick argillic horizon
  expect_false(soilKey:::sg_alfisols_inceptic_usda(mk("Bt2", c3 = 20))$passed)
})
