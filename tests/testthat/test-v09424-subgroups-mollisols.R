# v0.9.424 (Hugo, 2026-11-22): the Mollisol subgroups rebuilt from the KST
# 13th edition (ch. 12, pp. 248-294; audit R3 to R6, E26). Every great group
# lists exactly the subgroups of the KST, in the order of its key, Typic
# last, each with the KST criteria of that great group
# (R/subgroups-usda-mollisols.R). Three-valued: field observations not
# recorded are absent, measurements not recorded undecided.

.v09424_names <- function(gg, ...) paste(c(...), gg)

.v09424_kst <- list(
  IAA = .v09424_names("Natralbolls",
    "Leptic", "Typic"),
  IAB = .v09424_names("Argialbolls",
    "Xerertic", "Vertic", "Argiaquic Xeric", "Argiaquic", "Xeric",
    "Aquandic", "Typic"),
  IBA = .v09424_names("Cryaquolls",
    "Vertic", "Histic", "Thapto-Histic", "Aquandic", "Argic", "Calcic",
    "Cumulic", "Thapto-Humic", "Typic"),
  IBB = .v09424_names("Duraquolls",
    "Natric", "Vertic", "Argic", "Typic"),
  IBC = .v09424_names("Natraquolls",
    "Petrocalcic", "Vertic", "Glossic", "Typic"),
  IBD = .v09424_names("Calciaquolls",
    "Petrocalcic", "Aeric", "Typic"),
  IBE = .v09424_names("Argiaquolls",
    "Arenic", "Grossarenic", "Vertic", "Abruptic", "Typic"),
  IBF = .v09424_names("Epiaquolls",
    "Cumulic Vertic", "Fluvaquentic Vertic", "Vertic", "Histic",
    "Thapto-Histic", "Aquandic", "Duric", "Cumulic", "Thapto-Humic",
    "Fluvaquentic", "Typic"),
  IBG = .v09424_names("Endoaquolls",
    "Lithic", "Cumulic Vertic", "Fluvaquentic Vertic", "Vertic", "Histic",
    "Thapto-Histic", "Aquandic", "Duric", "Cumulic", "Thapto-Humic",
    "Fluvaquentic", "Typic"),
  ICA = .v09424_names("Cryrendolls",
    "Lithic", "Typic"),
  ICB = .v09424_names("Haprendolls",
    "Lithic", "Vertic", "Inceptic", "Entic", "Typic"),
  IDA = .v09424_names("Haplogelolls",
    "Lithic", "Andic", "Aquic", "Oxyaquic", "Turbic", "Cumulic", "Fluventic",
    "Pachic", "Typic"),
  IEA = .v09424_names("Duricryolls",
    "Argic", "Calcic", "Typic"),
  IEB = .v09424_names("Natricryolls",
    "Typic"),
  IEC = .v09424_names("Palecryolls",
    "Aquic", "Oxyaquic", "Abruptic", "Pachic", "Ustic", "Xeric", "Typic"),
  IED = .v09424_names("Argicryolls",
    "Lithic", "Vertic", "Andic", "Vitrandic", "Abruptic", "Aquic",
    "Oxyaquic", "Calcic Pachic", "Pachic", "Calcic", "Alfic", "Ustic",
    "Xeric", "Typic"),
  IEE = .v09424_names("Calcicryolls",
    "Lithic", "Vitrandic", "Petrocalcic", "Pachic", "Ustic", "Xeric",
    "Typic"),
  IEF = .v09424_names("Haplocryolls",
    "Lithic", "Vertic", "Andic", "Vitrandic", "Aquic Cumulic", "Cumulic",
    "Thapto-Humic", "Fluvaquentic", "Aquic", "Oxyaquic", "Calcic Pachic",
    "Pachic", "Fluventic", "Calcic", "Ustic", "Xeric", "Typic"),
  IFA = .v09424_names("Durixerolls",
    "Vertic", "Vitritorrandic", "Vitrandic", "Aquic", "Paleargidic",
    "Abruptic Argiduridic", "Cambidic", "Haploduridic", "Argidic",
    "Argiduridic", "Haplic Palexerollic", "Palexerollic",
    "Haplic Haploxerollic", "Haploxerollic", "Haplic", "Typic"),
  IFB = .v09424_names("Natrixerolls",
    "Vertic", "Aquic Duric", "Aquic", "Aridic", "Duric", "Typic"),
  IFC = .v09424_names("Palexerolls",
    "Vertic", "Vitrandic", "Aquic", "Pachic", "Petrocalcidic", "Duric",
    "Aridic", "Petrocalcic", "Ultic", "Haplic", "Typic"),
  IFD = .v09424_names("Calcixerolls",
    "Aridic Lithic", "Lithic", "Vertic", "Aquic", "Oxyaquic", "Pachic",
    "Vitrandic", "Aridic", "Vermic", "Typic"),
  IFE = .v09424_names("Argixerolls",
    "Aridic Lithic", "Lithic Ultic", "Lithic", "Torrertic", "Vertic",
    "Andic", "Vitritorrandic", "Vitrandic", "Aquultic", "Aquic", "Oxyaquic",
    "Alfic", "Calcic Pachic", "Pachic Ultic", "Pachic", "Argiduridic",
    "Duric", "Calciargidic", "Aridic", "Calcic", "Ultic", "Typic"),
  IFF = .v09424_names("Haploxerolls",
    "Aridic Lithic", "Lithic Ultic", "Lithic", "Torrertic", "Vertic",
    "Andic", "Vitritorrandic", "Vitrandic", "Aquic Cumulic", "Cumulic Ultic",
    "Cumulic", "Thapto-Humic", "Fluvaquentic", "Aquic Duric", "Aquultic",
    "Aquic", "Oxyaquic", "Calcic Pachic", "Pachic Ultic", "Pachic",
    "Torrifluventic", "Duridic", "Calcidic", "Torripsammentic",
    "Torriorthentic", "Aridic", "Duric", "Psammentic", "Fluventic", "Vermic",
    "Calcic", "Entic Ultic", "Ultic", "Entic", "Typic"),
  IGA = .v09424_names("Durustolls",
    "Natric", "Haploduridic", "Argiduridic", "Entic", "Haplic", "Typic"),
  IGB = .v09424_names("Natrustolls",
    "Leptic Torrertic", "Torrertic", "Leptic Vertic", "Glossic Vertic",
    "Vertic", "Aridic Leptic", "Leptic", "Aquic", "Aridic", "Duric",
    "Glossic", "Typic"),
  IGC = .v09424_names("Calciustolls",
    "Salidic", "Lithic Petrocalcic", "Lithic", "Torrertic", "Udertic",
    "Vertic", "Petrocalcic", "Gypsic", "Aquic", "Oxyaquic", "Pachic",
    "Aridic", "Udic", "Typic"),
  IGD = .v09424_names("Paleustolls",
    "Torrertic", "Udertic", "Vertic", "Aquic", "Pachic", "Petrocalcic",
    "Calcidic", "Aridic", "Udic", "Calcic", "Entic", "Typic"),
  IGE = .v09424_names("Argiustolls",
    "Aridic Lithic", "Alfic Lithic", "Lithic", "Aquertic", "Torrertic",
    "Pachic Udertic", "Udertic", "Pachic Vertic", "Vertic", "Andic",
    "Vitritorrandic", "Vitrandic", "Aquic", "Oxyaquic", "Pachic", "Alfic",
    "Calcidic", "Aridic", "Udic", "Duric", "Typic"),
  IGF = .v09424_names("Vermustolls",
    "Lithic", "Aquic", "Pachic", "Entic", "Typic"),
  IGG = .v09424_names("Haplustolls",
    "Salidic", "Ruptic-Lithic", "Aridic Lithic", "Lithic", "Aquertic",
    "Torrertic", "Pachic Udertic", "Udertic", "Pachic Vertic", "Vertic",
    "Torroxic", "Oxic", "Andic", "Vitritorrandic", "Vitrandic",
    "Aquic Cumulic", "Cumulic", "Anthraquic", "Thapto-Humic", "Fluvaquentic",
    "Aquic", "Pachic", "Oxyaquic", "Torrifluventic", "Torriorthentic",
    "Aridic", "Fluventic", "Duric", "Udorthentic", "Udic", "Entic", "Typic"),
  IHA = .v09424_names("Natrudolls",
    "Petrocalcic", "Leptic Vertic", "Glossic Vertic", "Vertic", "Leptic",
    "Abruptic", "Glossic", "Calcic", "Typic"),
  IHB = .v09424_names("Calciudolls",
    "Lithic", "Vertic", "Anthropic Petrocalcic", "Aquic", "Thapto-Humic",
    "Fluventic", "Typic"),
  IHC = .v09424_names("Paleudolls",
    "Vertic", "Aquic Pachic", "Pachic", "Aquic", "Oxyaquic", "Calcic",
    "Typic"),
  IHD = .v09424_names("Argiudolls",
    "Lithic", "Petrocalcic", "Aquertic", "Oxyaquic Vertic", "Pachic Vertic",
    "Alfic Vertic", "Vertic", "Andic", "Vitrandic", "Aquic Pachic", "Pachic",
    "Aquic", "Oxyaquic", "Lamellic", "Psammentic", "Arenic", "Abruptic",
    "Alfic", "Oxic", "Calcic", "Typic"),
  IHE = .v09424_names("Vermudolls",
    "Lithic", "Haplic", "Typic"),
  IHF = .v09424_names("Hapludolls",
    "Lithic", "Petrocalcic", "Aquertic", "Pachic Vertic", "Vertic", "Andic",
    "Vitrandic", "Aquic Cumulic", "Cumulic", "Thapto-Humic", "Fluvaquentic",
    "Fluventic", "Aquic Pachic", "Pachic", "Aquic", "Oxyaquic", "Vermic",
    "Calcic", "Entic", "Typic"))

.v09424_yaml <- function() {
  f <- system.file("rules", "usda", "subgroups", "mollisols.yaml", package = "soilKey")
  if (!nzchar(f)) f <- testthat::test_path("..", "..", "inst", "rules", "usda", "subgroups", "mollisols.yaml")
  yaml::read_yaml(f)$subgroups
}

test_that("each Mollisol great group lists exactly the KST subgroups, in KST order", {
  y <- .v09424_yaml()
  expect_setequal(names(y), names(.v09424_kst))
  for (g in names(.v09424_kst)) {
    expect_identical(vapply(y[[g]], `[[`, "", "name"), .v09424_kst[[g]], info = g)
    last <- y[[g]][[length(y[[g]])]]
    expect_true(isTRUE(last$tests$default), info = g)
  }
  expect_equal(sum(lengths(.v09424_kst)), 391L)
  codes <- unlist(lapply(y, function(b) vapply(b, `[[`, "", "code")))
  # KST codes; past Z the KST adds a lowercase letter (IFFZa, p. 296)
  expect_true(all(grepl("^[A-Z]{4}[a-z]?$", codes)))
  expect_true(all(c("IFFZa", "IFFZi", "IGGZa", "IGGZf") %in% codes))
  expect_false(anyDuplicated(codes) > 0)
})

test_that("every test named in the Mollisol subgroup YAML is a soilKey function", {
  y <- .v09424_yaml()
  fns <- character(0)
  walk <- function(x) {
    for (t in x) {
      if (!is.null(names(t)) && names(t)[1] %in% c("all_of", "any_of", "none_of")) walk(t[[1]])
      else if (!is.null(names(t))) fns <<- c(fns, names(t)[1])
    }
  }
  for (g in y) for (e in g) if (is.null(e$tests$default)) walk(e$tests$all_of)
  fns <- unique(fns)
  ok <- vapply(fns, function(f) exists(f, envir = asNamespace("soilKey"), mode = "function"), NA)
  expect_true(all(ok), info = paste(fns[!ok], collapse = ", "))
})

.v09424_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(site = c(list(id = "v09424"), site),
                  horizons = ensure_horizon_schema(hz))
}

# A dark, base-rich loamy profile (mollic 0-60 cm); arguments replace columns.
.v09424_mollic <- function(top = c(0, 20, 40, 60, 100), bottom = c(20, 40, 60, 100, 150),
                           designation = c("Ap", "A2", "AB", "Bw", "C"),
                           vm = c(2, 2, 3, 5, 6), vd = c(3, 3, 5, 7, 7), cm = c(2, 2, 2, 4, 4),
                           oc = c(3, 2, 1.2, 0.4, 0.2), bs = c(80, 80, 80, 85, 90),
                           clay = c(25, 26, 26, 24, 20), sand = c(35, 35, 35, 40, 45),
                           st = c("granular", "granular", "subangular blocky",
                                  "subangular blocky", "massive"),
                           ca = 0, site = list(), ...) {
  .v09424_pedon(top_cm = top, bottom_cm = bottom, designation = designation,
                munsell_hue_moist = "10YR", munsell_value_moist = vm,
                munsell_value_dry = vd, munsell_chroma_moist = cm,
                oc_pct = oc, bs_pct = bs, clay_pct = clay, sand_pct = sand,
                silt_pct = 100 - clay - sand, structure_type = st,
                rupture_resistance = "slightly hard", caco3_pct = ca, ph_h2o = 7.2,
                ..., site = site)
}

.v09424_sg <- function(p, gg) soilKey:::run_usda_subgroup(p, gg)$assigned$name

test_that("Vertic: slickensides stand for cracks (L30); COLE not measured is undecided", {
  testthat::skip_on_cran()
  p <- .v09424_mollic(slickensides = c(NA, NA, "common", "common", NA))
  r <- soilKey:::sg_mollisol_vertic_usda(p)
  expect_true(r$passed)
  expect_match(r$notes, "L30")
  # the compound comes before its parts: Pachic Vertic, not Pachic or Vertic
  expect_identical(.v09424_sg(p, "IHF"), "Pachic Vertic Hapludolls")
  q <- soilKey:::sg_mollisol_vertic_usda(.v09424_mollic())
  # v0.9.428: no crack or slickenside recorded and no COLE: FALSE
  expect_false(q$passed)
  expect_false("cole_value" %in% q$missing)
  expect_false(soilKey:::sg_mollisol_vertic_usda(.v09424_mollic(cole_value = 0.01))$passed)
  expect_true(soilKey:::sg_mollisol_vertic_usda(.v09424_mollic(cole_value = 0.08))$passed)
})

test_that("Aquic: the depletions, their colour and the depth of each great group", {
  testthat::skip_on_cran()
  pa <- function(mc) .v09424_mollic(cole_value = 0.01,
                                    redoximorphic_features_pct = c(0, 0, 0, 10, 0),
                                    mottle_colors = c(NA, NA, NA, mc, NA))
  aq <- function(p, ...) soilKey:::sg_mollisol_aquic_usda(p, ...)$passed
  expect_true(aq(pa("10YR 6/2"), "udoll"))
  expect_false(aq(pa("10YR 5/8"), "udoll"))           # concentrations, matrix chroma 4
  expect_true(is.na(aq(pa(NA), "udoll")))             # redox colour not recorded
  expect_false(aq(.v09424_mollic(), "udoll"))         # no redox features recorded: absent
  expect_true(aq(pa("10YR 6/2"), "depletions", max_cm = 75))
  expect_false(aq(pa("10YR 6/2"), "depletions", max_cm = 55))
  expect_identical(.v09424_sg(pa("10YR 6/2"), "IHF"), "Aquic Pachic Hapludolls")
  expect_identical(.v09424_sg(pa("10YR 6/2"), "IGG"), "Aquic Haplustolls")
})

test_that("Cumulic: 0.3 % organic carbon at 125 cm, Aquic Cumulic before Cumulic", {
  testthat::skip_on_cran()
  pc <- .v09424_mollic(cole_value = 0.01, oc = c(3, 2, 1.2, 0.4, 0.4))
  expect_identical(.v09424_sg(pc, "IGG"), "Cumulic Haplustolls")
  expect_identical(.v09424_sg(.v09424_mollic(cole_value = 0.01), "IGG"), "Pachic Haplustolls")
  pac <- .v09424_mollic(cole_value = 0.01, oc = c(3, 2, 1.2, 0.4, 0.4),
                        redoximorphic_features_pct = c(0, 0, 0, 10, 0),
                        mottle_colors = c(NA, NA, NA, "10YR 6/2", NA))
  expect_identical(.v09424_sg(pac, "IGG"), "Aquic Cumulic Haplustolls")
  f <- soilKey:::sg_mollisol_fluventic_usda(pc, "at125")
  expect_true(f$passed)
  expect_true(soilKey:::sg_mollisol_fluventic_usda(pc, "all125")$passed)
  expect_false(soilKey:::sg_mollisol_fluventic_usda(.v09424_mollic(), "all125")$passed)  # 0.2 % at 100-150
})

test_that("moisture intergrades: the suborder's own regime does not say how dry", {
  testthat::skip_on_cran()
  m <- function(site, k) soilKey:::sg_mollisol_moisture_usda(.v09424_mollic(site = site), k)$passed
  expect_true(is.na(m(list(soil_moisture_regime = "ustic"), "aridic")))
  expect_true(m(list(soil_moisture_regime = "ustic", soil_moisture_subclass = "aridic ustic"), "aridic"))
  expect_false(m(list(soil_moisture_regime = "ustic", soil_moisture_subclass = "typic ustic"), "udic"))
  expect_false(m(list(soil_moisture_regime = "udic"), "aridic"))
  expect_true(m(list(soil_moisture_regime = "udic"), "udic"))
  expect_true(is.na(m(list(), "udic")))
  expect_true(m(list(soil_moisture_regime = "ustic"), "ustic_cryoll"))
  expect_false(m(list(soil_moisture_regime = "xeric"), "ustic_cryoll"))
  expect_true(m(list(soil_moisture_regime = "xeric"), "xeric_summer"))
})

test_that("Haplic Vermudolls: a mollic epipedon less than 75 cm thick", {
  testthat::skip_on_cran()
  expect_identical(.v09424_sg(.v09424_mollic(cole_value = 0.01), "IHE"), "Haplic Vermudolls")
  deep <- .v09424_mollic(cole_value = 0.01, vm = c(2, 2, 3, 3, 6), vd = c(3, 3, 5, 5, 7),
                         cm = c(2, 2, 2, 2, 4), oc = c(3, 2, 1.2, 1, 0.2))
  expect_identical(.v09424_sg(deep, "IHE"), "Typic Vermudolls")
})

test_that("Thapto-Humic, Ultic, Calcic, Arenic and Leptic read the Mollisol wording", {
  testthat::skip_on_cran()
  pv <- function(r) r$passed
  pt <- .v09424_mollic(designation = c("Ap", "A2", "AB", "Bw", "Ab"),
                       vm = c(2, 2, 3, 5, 2), oc = c(3, 2, 1.2, 0.4, 1.5))
  expect_true(pv(soilKey:::sg_mollisol_thapto_humic_usda(pt)))
  expect_false(pv(soilKey:::sg_mollisol_thapto_humic_usda(.v09424_mollic())))   # no buried horizon
  expect_true(pv(soilKey:::sg_mollisol_ultic_usda(.v09424_mollic(bs = c(80, 70, 80, 85, 90)))))
  expect_false(pv(soilKey:::sg_mollisol_ultic_usda(.v09424_mollic())))
  expect_true(is.na(pv(soilKey:::sg_mollisol_ultic_usda(.v09424_mollic(bs = c(80, NA, 80, 85, 90))))))
  # loamy control section: secondary carbonates within 110 cm
  expect_true(pv(soilKey:::sg_mollisol_calcic_usda(
    .v09424_mollic(designation = c("Ap", "A2", "AB", "Bw", "Bk")), "psc_secondary")))
  expect_false(pv(soilKey:::sg_mollisol_calcic_usda(
    .v09424_mollic(top = c(0, 20, 40, 60, 120), bottom = c(20, 40, 60, 120, 150),
                   designation = c("Ap", "A2", "AB", "Bw", "Bk")), "psc_secondary")))
  # Arenic needs no spodic horizon in the Mollisols (audit R5)
  pr <- .v09424_mollic(designation = c("A1", "A2", "A3", "Bt", "BC"),
                       clay = c(5, 5, 5, 30, 25), sand = c(88, 88, 88, 40, 45))
  expect_true(pv(soilKey:::sg_mollisol_arenic_usda(pr, min_cm = 50)))
  expect_false(pv(soilKey:::sg_mollisol_arenic_usda(pr, min_cm = 100)))
  expect_true(pv(soilKey:::sg_mollisol_leptic_usda(
    .v09424_mollic(designation = c("Ap", "Az", "AB", "Bw", "C")))))
  expect_false(pv(soilKey:::sg_mollisol_leptic_usda(.v09424_mollic())))
})

test_that("names the KST 13th edition does not have are gone", {
  y <- .v09424_yaml()
  nm <- unlist(lapply(y, function(b) vapply(b, `[[`, "", "name")))
  gone <- c("Aquic Natralbolls", "Aquic Argialbolls", "Lithic Cryaquolls", "Salic Natraquolls",
            "Calcic Haplustolls", "Pachic Vermudolls", "Argic Durustolls", "Lithic Palexerolls")
  expect_false(any(gone %in% nm))
})
