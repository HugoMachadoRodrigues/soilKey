# v0.9.425 (wave 3 of the Soil Taxonomy audit): the Inceptisol and Entisol
# subgroup lists are the KST 13th edition lists (Ch. 11, pp. 207-246; Ch. 8,
# pp. 165-188), in KST order, with each subgroup's own criteria
# (R/subgroups-usda-inceptent.R); three-valued throughout.

.p425 <- function(h, site = list()) {
  PedonRecord$new(site = c(list(id = "v09425"), site), horizons = h)
}
.f425 <- function(fn, p, ...) get(fn, envir = asNamespace("soilKey"))(p, ...)

# The KST 13th edition subgroup lists, the modifier of each name, per great
# group, in key order (Typic last).
.kst425 <- list(
  Cryaquepts = c("Sulfic", "Histic Lithic", "Lithic", "Vertic", "Histic",
    "Limnic", "Aquandic", "Thapto-Humic", "Fluvaquentic", "Aeric Humic",
    "Aeric", "Humic", "Typic"),
  Densiaquepts = c("Aeric", "Humic", "Typic"),
  Endoaquepts = c("Sulfic", "Lithic", "Vertic", "Aquandic", "Thapto-Humic",
    "Fluventic", "Fluvaquentic", "Fragic", "Aeric", "Humic", "Mollic", "Typic"),
  Epiaquepts = c("Vertic", "Aquandic", "Thapto-Humic", "Fluvaquentic",
    "Fragic", "Aeric", "Humic", "Mollic", "Typic"),
  Fragiaquepts = c("Aeric", "Humic", "Typic"),
  Gelaquepts = c("Lithic", "Histic", "Aquandic", "Thapto-Humic",
    "Fluvaquentic", "Humic", "Turbic", "Typic"),
  Halaquepts = c("Vertic", "Aquandic", "Duric", "Aeric", "Typic"),
  Humaquepts = c("Hydraquentic", "Histic", "Aquandic", "Thapto-Humic",
    "Cumulic", "Fluvaquentic", "Aeric", "Typic"),
  Petraquepts = c("Histic Placic", "Placic", "Plinthic", "Typic"),
  Sulfaquepts = c("Salidic", "Hydraquentic", "Typic"),
  Vermaquepts = c("Sodic", "Typic"),
  Calcicryepts = c("Lithic", "Oxyaquic", "Xeric", "Ustic", "Typic"),
  Dystrocryepts = c("Lithic", "Aquandic", "Haploxerandic", "Vitrixerandic",
    "Andic", "Vitrandic", "Thapto-Humic", "Fluvaquentic", "Folistic", "Aquic",
    "Oxyaquic", "Lamellic", "Fluventic", "Spodic", "Xeric", "Ustic", "Eutric",
    "Typic"),
  Haplocryepts = c("Lithic", "Aquandic", "Haploxerandic", "Vitrixerandic",
    "Haplustandic", "Ustivitrandic", "Andic", "Vitrandic", "Thapto-Humic",
    "Fluvaquentic", "Aquic", "Oxyaquic", "Lamellic", "Fluventic", "Calcic",
    "Xeric", "Ustic", "Typic"),
  Humicryepts = c("Lithic", "Aquandic", "Haploxerandic", "Vitrixerandic",
    "Andic", "Vitrandic", "Thapto-Humic", "Fluvaquentic", "Aquic", "Oxyaquic",
    "Lamellic", "Fluventic", "Spodic", "Xeric", "Eutric", "Typic"),
  Dystrogelepts = c("Lithic", "Andic", "Aquic", "Thapto-Humic", "Fluventic",
    "Turbic", "Typic"),
  Haplogelepts = c("Lithic", "Andic", "Aquic", "Thapto-Humic", "Fluventic",
    "Turbic", "Typic"),
  Humigelepts = c("Lithic", "Andic", "Aquic", "Oxyaquic", "Thapto-Humic",
    "Fluventic", "Turbic", "Eutric", "Typic"),
  Densiudepts = c("Aquic", "Humic", "Typic"),
  Durudepts = c("Aquandic", "Andic", "Vitrandic", "Aquic", "Typic"),
  Dystrudepts = c("Humic Lithic", "Lithic", "Vertic", "Anthrodensic",
    "Anthroportic", "Aquandic", "Andic Oxyaquic", "Andic", "Vitrandic",
    "Fragiaquic", "Thapto-Humic", "Fluvaquentic", "Aquic Humic", "Aquic",
    "Oxyaquic", "Fragic", "Lamellic", "Humic Psammentic", "Fluventic Humic",
    "Fluventic", "Spodic", "Oxic", "Ruptic-Alfic", "Ruptic-Ultic", "Humic",
    "Typic"),
  Eutrudepts = c("Humic Lithic", "Lithic", "Aquertic", "Vertic", "Andic",
    "Vitrandic", "Anthraquic", "Fragiaquic", "Thapto-Humic", "Fluvaquentic",
    "Aquic Dystric", "Aquic", "Oxyaquic", "Fragic", "Lamellic",
    "Dystric Fluventic", "Fluventic", "Arenic", "Dystric", "Rendollic",
    "Ruptic-Alfic", "Humic", "Typic"),
  Fragiudepts = c("Andic", "Vitrandic", "Aquic", "Humic", "Typic"),
  Humudepts = c("Lithic", "Vertic", "Aquandic", "Andic Oxyaquic", "Andic",
    "Vitrandic", "Thapto-Humic", "Fluvaquentic", "Aquic", "Oxyaquic",
    "Psammentic", "Oxic", "Cumulic", "Fluventic", "Pachic", "Eutric", "Entic",
    "Typic"),
  Sulfudepts = c("Typic"),
  Calciustepts = c("Lithic Petrocalcic", "Lithic", "Torrertic", "Vertic",
    "Petrocalcic", "Gypsic", "Aquic", "Aridic", "Udic", "Typic"),
  Durustepts = c("Typic"),
  Dystrustepts = c("Lithic", "Torrertic", "Vertic", "Andic", "Vitrandic",
    "Aquic", "Thapto-Humic", "Fluventic", "Aridic", "Oxic", "Humic", "Typic"),
  Haplustepts = c("Aridic Lithic", "Lithic", "Udertic", "Torrertic",
    "Vertic", "Andic", "Vitrandic", "Anthraquic", "Anthrodensic",
    "Anthroportic", "Aquic", "Oxyaquic", "Oxic", "Lamellic", "Thapto-Humic",
    "Torrifluventic", "Udifluventic", "Fluventic", "Gypsic", "Haplocalcidic",
    "Calcic Udic", "Calcic", "Aridic", "Dystric", "Udic", "Typic"),
  Humustepts = c("Lithic", "Andic", "Vitrandic", "Oxyaquic", "Oxic",
    "Aridic", "Typic"),
  Calcixerepts = c("Lithic", "Vertic", "Petrocalcic", "Sodic", "Vitrandic",
    "Aquic", "Typic"),
  Durixerepts = c("Aquandic", "Andic", "Vitrandic", "Aquic", "Entic",
    "Typic"),
  Dystroxerepts = c("Humic Lithic", "Lithic", "Aquandic", "Andic",
    "Vitrandic", "Fragiaquic", "Thapto-Humic", "Fluvaquentic", "Aquic",
    "Oxyaquic", "Fragic", "Fluventic Humic", "Fluventic", "Humic", "Typic"),
  Fragixerepts = c("Andic", "Vitrandic", "Aquic", "Humic", "Typic"),
  Haploxerepts = c("Humic Lithic", "Lithic", "Vertic", "Aquandic",
    "Andic Oxyaquic", "Andic", "Oxyaquic Vitrandic", "Vitrandic", "Gypsic",
    "Aquic", "Lamellic", "Fragic", "Thapto-Humic", "Fluventic", "Calcic",
    "Humic", "Typic"),
  Humixerepts = c("Lithic", "Aquandic", "Andic", "Vitrandic", "Aquic",
    "Oxyaquic", "Thapto-Humic", "Cumulic", "Fluventic", "Pachic", "Entic",
    "Typic"),
  Cryaquents = c("Aquandic", "Typic"),
  Endoaquents = c("Sulfic", "Lithic", "Sodic", "Aeric", "Humaqueptic",
    "Mollic", "Typic"),
  Epiaquents = c("Aeric", "Humaqueptic", "Mollic", "Typic"),
  Fluvaquents = c("Sulfic", "Vertic", "Thapto-Histic", "Thapto-Humic",
    "Aquandic", "Aeric", "Humaqueptic", "Mollic", "Typic"),
  Gelaquents = c("Typic"),
  Hydraquents = c("Sulfic", "Sodic", "Thapto-Histic", "Thapto-Humic",
    "Typic"),
  Psammaquents = c("Lithic", "Sodic", "Spodic", "Humaqueptic", "Mollic",
    "Thapto-Histic", "Thapto-Humic", "Fluventic", "Typic"),
  Sulfaquents = c("Histic-Haplic", "Haplic", "Histic", "Thapto-Histic",
    "Thapto-Humic", "Fluventic", "Typic"),
  Cryofluvents = c("Andic", "Vitrandic", "Aquic", "Oxyaquic", "Mollic",
    "Typic"),
  Gelifluvents = c("Aquic", "Typic"),
  Torrifluvents = c("Ustertic", "Vertic", "Vitrixerandic", "Vitrandic",
    "Aquic", "Oxyaquic", "Duric Xeric", "Duric", "Ustic", "Xeric",
    "Anthropic", "Typic"),
  Udifluvents = c("Aquertic", "Vertic", "Andic", "Vitrandic", "Aquic",
    "Oxyaquic", "Mollic", "Typic"),
  Ustifluvents = c("Aquertic", "Torrertic", "Vertic", "Anthraquic", "Aquic",
    "Oxyaquic", "Aridic", "Udic", "Mollic", "Typic"),
  Xerofluvents = c("Vertic", "Aquandic", "Andic", "Vitrandic", "Aquic",
    "Oxyaquic", "Durinodic", "Mollic", "Typic"),
  Cryorthents = c("Lithic", "Vitrandic", "Aquic", "Oxyaquic", "Lamellic",
    "Typic"),
  Gelorthents = c("Aquic", "Oxyaquic", "Typic"),
  Torriorthents = c("Lithic Ustic", "Lithic Xeric", "Lithic", "Xerertic",
    "Ustertic", "Vertic", "Anthraltic", "Vitrandic", "Aquic", "Oxyaquic",
    "Duric", "Ustic", "Xeric", "Typic"),
  Udorthents = c("Lithic", "Anthrodensic Sodic", "Anthrodensic", "Anthropic",
    "Anthroportic", "Vitrandic", "Aquic", "Oxyaquic", "Vermic", "Typic"),
  Ustorthents = c("Aridic Lithic", "Lithic", "Torrertic", "Vertic",
    "Anthraquic", "Anthrodensic", "Anthroportic", "Aquic", "Oxyaquic",
    "Durinodic", "Vitritorrandic", "Vitrandic", "Aridic", "Udic", "Vermic",
    "Typic"),
  Xerorthents = c("Lithic", "Anthraltic Sodic", "Anthraltic", "Vitrandic",
    "Aquic", "Oxyaquic", "Durinodic", "Dystric", "Typic"),
  Cryopsamments = c("Lithic", "Aquic", "Oxyaquic", "Vitrandic", "Spodic",
    "Lamellic", "Typic"),
  Quartzipsamments = c("Lithic", "Aquodic", "Aquic", "Oxyaquic", "Ustoxic",
    "Udoxic", "Plinthic", "Lamellic Ustic", "Lamellic", "Ustic", "Xeric",
    "Spodic", "Typic"),
  Torripsamments = c("Lithic", "Oxyaquic", "Vitrandic", "Haploduridic",
    "Ustic", "Xeric", "Rhodic", "Typic"),
  Udipsamments = c("Lithic", "Aquic", "Oxyaquic", "Spodic", "Lamellic",
    "Haploplaggic", "Typic"),
  Ustipsamments = c("Lithic", "Aquic", "Oxyaquic", "Aridic", "Lamellic",
    "Rhodic", "Typic"),
  Xeropsamments = c("Lithic", "Aquic Durinodic", "Aquic", "Oxyaquic",
    "Vitrandic", "Durinodic", "Lamellic", "Dystric", "Typic"),
  Fluviwassents = c("Sulfic", "Lithic", "Thapto-Histic", "Thapto-Humic",
    "Aeric", "Typic"),
  Frasiwassents = c("Humic Fluic", "Humic", "Fluic", "Lithic", "Psammentic",
    "Thapto-Histic", "Thapto-Humic", "Fluventic", "Aeric", "Typic"),
  Haplowassents = c("Sulfic", "Lithic", "Aeric", "Typic"),
  Hydrowassents = c("Sulfic", "Grossic", "Lithic", "Thapto-Histic",
    "Thapto-Humic", "Typic"),
  Psammowassents = c("Sulfic", "Lithic", "Thapto-Humic", "Fluventic",
    "Aeric", "Typic"),
  Sulfiwassents = c("Lithic", "Haplic", "Thapto-Histic", "Thapto-Humic",
    "Fluventic", "Aeric", "Typic")
)

.yaml425 <- function(order) {
  f <- system.file("rules", "usda", "subgroups", paste0(order, ".yaml"), package = "soilKey")
  yaml::read_yaml(f)$subgroups
}
.gg425 <- function(order) {
  f <- system.file("rules", "usda", "great-groups", paste0(order, ".yaml"), package = "soilKey")
  g <- unlist(yaml::read_yaml(f), recursive = FALSE)
  g <- unlist(lapply(g, function(x) if (!is.null(x$code)) list(x) else x), recursive = FALSE)
  setNames(vapply(g, function(x) x$name, ""), vapply(g, function(x) x$code, ""))
}

test_that("every Inceptisol and Entisol great group has the KST 13th edition list, in order", {
  skip_on_cran()
  n <- 0L
  for (o in c("inceptisols", "entisols")) {
    sg <- .yaml425(o); gg <- .gg425(o)
    expect_setequal(names(sg), names(gg))
    for (code in names(sg)) {
      ggn <- gg[[code]]
      kst <- .kst425[[ggn]]
      expect_false(is.null(kst), info = ggn)
      got <- vapply(sg[[code]], function(x) x$name, "")
      expect_identical(got, paste(kst, ggn), info = ggn)
      typic <- sg[[code]][[length(sg[[code]])]]
      expect_true(isTRUE(typic$tests$default), info = ggn)
      codes <- vapply(sg[[code]], function(x) x$code, "")
      expect_identical(codes, paste0(code, LETTERS[seq_along(codes)]), info = ggn)
      n <- n + length(got)
    }
  }
  expect_identical(n, 588L)   # 349 Inceptisol and 239 Entisol subgroups
})

test_that("every test the two lists name is a function of the package", {
  skip_on_cran()
  ns <- asNamespace("soilKey")
  walk <- function(t) {
    if (!is.null(t$all_of)) return(unlist(lapply(t$all_of, walk)))
    if (!is.null(t$any_of)) return(unlist(lapply(t$any_of, walk)))
    names(t)
  }
  for (o in c("inceptisols", "entisols")) for (gg in .yaml425(o)) for (s in gg) {
    if (isTRUE(s$tests$default)) next
    fns <- walk(s$tests)
    for (f in fns) expect_true(exists(f, envir = ns, inherits = FALSE), info = paste(s$name, f))
  }
})

test_that("Vertic: cracks with slickensides, or a linear extensibility of 6 cm; COLE not measured is undecided", {
  skip_on_cran()
  h <- data.frame(designation = c("A", "Bw", "C"), top_cm = c(0, 20, 60),
                  bottom_cm = c(20, 60, 120), stringsAsFactors = FALSE)
  expect_true(is.na(.f425("sg_incept_vertic_usda", .p425(h))$passed))
  h$cole_value <- c(0.01, 0.02, 0.01)
  expect_false(.f425("sg_incept_vertic_usda", .p425(h))$passed)
  h$cole_value <- c(0.08, 0.08, 0.08)                # 8 cm over 100 cm
  expect_true(.f425("sg_incept_vertic_usda", .p425(h))$passed)
  h$cole_value <- c(0.01, NA, 0.01)
  h$cracks_width_cm <- c(NA, 1, NA); h$cracks_depth_cm <- c(NA, 40, NA)
  h$slickensides <- c(NA, "common", NA)
  expect_true(.f425("sg_incept_vertic_usda", .p425(h))$passed)
})

test_that("Aquic: redox depletions within the depth of the great group", {
  skip_on_cran()
  h <- data.frame(designation = c("A", "Bw", "C"), top_cm = c(0, 20, 70),
                  bottom_cm = c(20, 70, 120), munsell_chroma_moist = c(2, 4, 2),
                  redoximorphic_features_pct = c(0, 0, 10), stringsAsFactors = FALSE)
  p <- .p425(h)
  expect_false(.f425("sg_incept_aquic_usda", p, max_cm = 60)$passed)   # redox at 70 cm
  expect_true(.f425("sg_incept_aquic_usda", p, max_cm = 75)$passed)
  h$munsell_chroma_moist[3] <- NA
  expect_true(is.na(.f425("sg_incept_aquic_usda", .p425(h), max_cm = 75)$passed))
  h$redoximorphic_features_pct <- NA                             # none recorded: absent
  expect_false(.f425("sg_incept_aquic_usda", .p425(h), max_cm = 75)$passed)
  h$designation[2] <- "Bg"
  expect_true(.f425("sg_incept_aquic_usda", .p425(h), max_cm = 60)$passed)
})

test_that("dark surface, buried humus and the base saturation are three-valued", {
  skip_on_cran()
  h <- data.frame(designation = c("A", "Bw", "Ab", "C"), top_cm = c(0, 20, 60, 90),
                  bottom_cm = c(20, 60, 90, 150), munsell_value_moist = c(3, 5, 2, 5),
                  munsell_value_dry = c(5, 6, 4, 6), oc_pct = c(2, 0.5, 1.5, 0.2),
                  bs_pct = c(40, 45, 60, 70), stringsAsFactors = FALSE)
  p <- .p425(h)
  expect_true(.f425("sg_incept_dark_usda", p, depth_cm = 18)$passed)
  expect_true(.f425("sg_incept_thapto_humic_usda", p)$passed)
  expect_true(.f425("sg_incept_bs_usda", p, op = "lt", value = 50, top = 0, bottom = 100, mode = "some")$passed)
  expect_false(.f425("sg_incept_bs_usda", p, op = "ge", value = 50, top = 25, bottom = 75, mode = "half")$passed)
  h2 <- h; h2$munsell_value_dry[1] <- NA
  expect_true(is.na(.f425("sg_incept_dark_usda", .p425(h2), depth_cm = 18)$passed))
  h2$oc_pct[3] <- NA
  expect_true(is.na(.f425("sg_incept_thapto_humic_usda", .p425(h2))$passed))
  h2$designation[3] <- "AB"
  expect_false(.f425("sg_incept_thapto_humic_usda", .p425(h2))$passed)
})

test_that("moisture intergrades are decided only where the regime class settles them", {
  skip_on_cran()
  h <- data.frame(designation = "A", top_cm = 0, bottom_cm = 30)
  m <- function(smr, kind, str = NULL) {
    p <- .p425(h, site = list(soil_moisture_regime = smr, soil_temperature_regime = "mesic"))
    .f425("sg_incept_moisture_usda", p, kind = kind, str = str)$passed
  }
  expect_true(is.na(m("ustic", "aridic")))
  expect_true(m("aridic", "aridic"))
  expect_true(m("udic", "udic"))
  expect_false(m("aridic", "udic"))
  expect_true(m("xeric", "dry45"))
  expect_true(is.na(m("udic", "dry45")))
  expect_true(is.na(m("aridic", "torric_xeric")))
  expect_false(m("ustic", "torric_xeric"))
  expect_false(m("aridic", "torric_xeric", str = "thermic"))   # mesic recorded
  expect_true(is.na(m(NA, "aridic")))
})

test_that("Anthroportic Udorthents is reachable; Oxyaquic Udorthents reads 150 cm", {
  skip_on_cran()
  # human-transported material 60 cm thick, no anthropic epipedon
  h <- data.frame(designation = c("^C1", "^C2", "C"), top_cm = c(0, 30, 60),
                  bottom_cm = c(30, 60, 160), oc_pct = c(0.3, 0.2, 0.1),
                  munsell_value_moist = c(5, 5, 6), munsell_value_dry = c(6, 6, 7),
                  munsell_chroma_moist = c(4, 4, 4), redoximorphic_features_pct = 0,
                  stringsAsFactors = FALSE)
  p <- .p425(h)
  expect_true(.f425("sg_incept_anthro_usda", p, kind = "anthroportic")$passed)
  expect_false(isTRUE(.f425("anthropic_epipedon_usda", p)$passed))
  r <- run_usda_subgroup(p, "LEF")
  expect_identical(r$assigned$name, "Anthroportic Udorthents")
  # saturation at 120 cm: Oxyaquic in the Udorthents (150 cm), not in the
  # Udifluvents (100 cm)
  h2 <- h; h2$designation <- c("A", "C1", "C2")
  h2$water_saturation_days <- c(0, 0, 40)
  h2$top_cm <- c(0, 30, 110); h2$bottom_cm <- c(30, 110, 160)
  p2 <- .p425(h2)
  expect_true(.f425("oxyaquic_subgroup_usda", p2, max_depth_cm = 150)$passed)
  expect_false(.f425("oxyaquic_subgroup_usda", p2, max_depth_cm = 100)$passed)
  oxy <- Filter(function(s) s$name == "Oxyaquic Udorthents", .yaml425("entisols")$LEF)[[1]]
  expect_identical(oxy$tests$all_of[[1]]$oxyaquic_subgroup_usda$max_depth_cm, 150L)
  # the canonical Technosol: anthropic epipedon first, as the KST orders it
  t <- classify_usda(make_technosol_canonical(), on_missing = "silent")
  expect_identical(t$name, "Anthropic Udorthents")
})

test_that("Fluventic Endoaquepts needs the Aeric colours; Fluvaquentic the organic carbon only", {
  skip_on_cran()
  h <- data.frame(designation = c("Ap", "Bg", "Cg"), top_cm = c(0, 20, 60),
                  bottom_cm = c(20, 60, 150), oc_pct = c(2, 0.4, 0.3),
                  munsell_hue_moist = c("10YR", "10YR", "2.5Y"),
                  munsell_value_moist = c(4, 5, 5), munsell_chroma_moist = c(2, 1, 1),
                  redoximorphic_features_pct = c(0, 20, 20), stringsAsFactors = FALSE)
  p <- .p425(h, site = list(slope_pct = 1))
  expect_true(.f425("sg_incept_fluventic_usda", p)$passed)          # 0.3 % at 125 cm
  expect_false(.f425("sg_incept_aeric_usda", p, rule = "endoaquept")$passed)
  expect_identical(run_usda_subgroup(p, "KAK")$assigned$name, "Fluvaquentic Endoaquepts")
  h$munsell_chroma_moist[2] <- 4; h$redoximorphic_features_pct[2] <- 0
  expect_identical(run_usda_subgroup(.p425(h, site = list(slope_pct = 1)), "KAK")$assigned$name,
                   "Fluventic Endoaquepts")
  expect_false(.f425("sg_incept_fluventic_usda", .p425(h, site = list(slope_pct = 30)))$passed)
})

test_that("Dystric, Rendollic and Oxic criteria", {
  skip_on_cran()
  h <- data.frame(designation = c("A", "Bw", "C"), top_cm = c(0, 20, 60),
                  bottom_cm = c(20, 60, 120), ph_h2o = c(5.5, 5.8, 6.0),
                  cec_cmol = c(8, 4, 4), clay_pct = c(20, 30, 30),
                  water_content_1500kpa = c(10, 12, 12), oc_pct = c(1.5, 0.4, 0.2),
                  stringsAsFactors = FALSE)
  p <- .p425(h)
  expect_true(.f425("sg_incept_no_carbonates_usda", p)$passed)
  expect_true(.f425("sg_incept_oxic_usda", p)$passed)                # 13 cmol/kg clay
  h$ph_h2o <- NA
  expect_true(is.na(.f425("sg_incept_no_carbonates_usda", .p425(h))$passed))
  h$caco3_pct <- c(0, 0, 5)
  expect_false(.f425("sg_incept_no_carbonates_usda", .p425(h))$passed)
  h$cec_cmol <- c(20, 20, 20)
  expect_false(.f425("sg_incept_oxic_usda", .p425(h))$passed)
})

test_that("canonical fixtures that move with the KST lists", {
  skip_on_cran()
  nm <- function(f) classify_usda(f(), on_missing = "silent")$name
  expect_identical(nm(make_cambisol_canonical), "Dystric Fluventic Eutrudepts")
  expect_identical(nm(make_fluvisol_canonical), "Mollic Udifluvents")
  expect_identical(nm(make_gleysol_canonical), "Fluvaquentic Endoaquepts")
  expect_identical(nm(make_umbrisol_canonical), "Fluventic Humudepts")
})

test_that("the Spodic Psamments and Psammaquents read the depth rule of the Psamments (p. 184)", {
  skip_on_cran()
  e <- .yaml425("entisols")
  for (nm in c("Spodic Psammaquents", "Spodic Cryopsamments", "Spodic Quartzipsamments",
               "Spodic Udipsamments")) {
    s <- Filter(function(x) x$name == nm, unlist(e, recursive = FALSE))[[1]]
    expect_identical(s$tests$all_of[[1]]$spodic_subgroup_usda$psamments, TRUE, info = nm)
  }
})
