# v0.9.426 (wave 3 of the Soil Taxonomy audit, R3 to R6): the subgroup lists
# of the Aridisols, Vertisols and Andisols rebuilt from the KST 13th edition
# (Ch. 7, pp. 137-163; Ch. 16, pp. 343-354; Ch. 6, pp. 117-136), with the
# three-valued tests of R/subgroups-usda-aridvertand.R.

# The KST 13th edition lists, great group by great group: the code, the
# great group, and the subgroup modifiers in KST order (Typic last).
.v09426_kst_aridisols <- list(
  GAA = list("Salicryids", c("Aquic", "Typic")),
  GAB = list("Petrocryids", c("Xereptic", "Duric Xeric", "Duric",
      "Petrogypsic", "Xeric", "Ustic", "Typic")),
  GAC = list("Gypsicryids", c("Calcic", "Vitrixerandic", "Vitrandic",
      "Typic")),
  GAD = list("Argicryids", c("Lithic", "Vertic", "Natric", "Vitrixerandic",
      "Vitrandic", "Xeric", "Ustic", "Typic")),
  GAE = list("Calcicryids", c("Lithic", "Vitrixerandic", "Vitrandic", "Xeric",
      "Ustic", "Typic")),
  GAF = list("Haplocryids", c("Lithic", "Vertic", "Vitrixerandic",
      "Vitrandic", "Xeric", "Ustic", "Typic")),
  GBA = list("Aquisalids", c("Anhydritic", "Gypsic", "Calcic", "Typic")),
  GBB = list("Haplosalids", c("Duric", "Petrogypsic", "Anhydritic", "Gypsic",
      "Calcic", "Typic")),
  GCA = list("Natridurids", c("Vertic", "Aquic Natrargidic", "Aquic",
      "Natrixeralfic", "Natrargidic", "Vitrixerandic", "Vitrandic", "Xeric",
      "Typic")),
  GCB = list("Argidurids", c("Vertic", "Aquic", "Abruptic Xeric", "Abruptic",
      "Haploxeralfic", "Argidic", "Vitrixerandic", "Vitrandic", "Xeric",
      "Ustic", "Typic")),
  GCC = list("Haplodurids", c("Aquicambidic", "Aquic", "Xereptic", "Cambidic",
      "Vitrixerandic", "Vitrandic", "Xeric", "Ustic", "Typic")),
  GDA = list("Petrogypsids", c("Petrocalcic", "Calcic", "Vitrixerandic",
      "Vitrandic", "Xeric", "Ustic", "Typic")),
  GDB = list("Natrigypsids", c("Lithic", "Vertic", "Petronodic",
      "Vitrixerandic", "Vitrandic", "Xeric", "Ustic", "Typic")),
  GDC = list("Argigypsids", c("Lithic", "Vertic", "Calcic", "Petronodic",
      "Vitrixerandic", "Vitrandic", "Xeric", "Ustic", "Typic")),
  GDD = list("Calcigypsids", c("Lithic", "Petronodic", "Vitrixerandic",
      "Vitrandic", "Xeric", "Ustic", "Typic")),
  GDE = list("Haplogypsids", c("Lithic", "Leptic", "Sodic", "Petronodic",
      "Vitrixerandic", "Vitrandic", "Xeric", "Ustic", "Typic")),
  GEA = list("Petroargids", c("Petrogypsic Ustic", "Petrogypsic",
      "Duric Xeric", "Duric", "Natric", "Xeric", "Ustic", "Typic")),
  GEB = list("Natrargids", c("Lithic Xeric", "Lithic Ustic", "Lithic",
      "Xerertic", "Ustertic", "Vertic", "Aquic", "Durinodic Xeric",
      "Durinodic", "Petronodic", "Glossic Ustic", "Haplic Ustic",
      "Haploxeralfic", "Haplic", "Vitrixerandic", "Vitrandic", "Xeric",
      "Ustic", "Glossic", "Typic")),
  GEC = list("Paleargids", c("Vertic", "Aquic", "Arenic Ustic", "Arenic",
      "Calcic", "Durinodic Xeric", "Durinodic", "Petronodic Ustic",
      "Petronodic", "Vitrixerandic", "Vitrandic", "Xeric", "Ustic", "Typic")),
  GED = list("Gypsiargids", c("Aquic", "Durinodic", "Vitrixerandic",
      "Vitrandic", "Xeric", "Ustic", "Typic")),
  GEE = list("Calciargids", c("Lithic", "Xerertic", "Ustertic", "Vertic",
      "Aquic", "Arenic Ustic", "Arenic", "Durinodic Xeric", "Durinodic",
      "Petronodic Xeric", "Petronodic Ustic", "Petronodic", "Vitrixerandic",
      "Vitrandic", "Xeric", "Ustic", "Typic")),
  GEF = list("Haplargids", c("Lithic Ruptic-Entic", "Lithic Xeric",
      "Lithic Ustic", "Lithic", "Xerertic", "Ustertic", "Vertic", "Aquic",
      "Arenic Ustic", "Arenic", "Durinodic Xeric", "Durinodic",
      "Petronodic Ustic", "Petronodic", "Vitrixerandic", "Vitrandic", "Xeric",
      "Ustic", "Typic")),
  GFA = list("Petrocalcids", c("Aquic", "Natric", "Xeralfic", "Ustalfic",
      "Argic", "Calcic Lithic", "Calcic", "Xeric", "Ustic", "Typic")),
  GFB = list("Haplocalcids", c("Lithic Xeric", "Lithic Ustic", "Lithic",
      "Vertic", "Aquic Durinodic", "Aquic", "Duric Xeric", "Duric",
      "Durinodic Xeric", "Durinodic", "Petronodic Xeric", "Petronodic Ustic",
      "Petronodic", "Sodic Xeric", "Sodic Ustic", "Sodic", "Vitrixerandic",
      "Vitrandic", "Xeric", "Ustic", "Typic")),
  GGA = list("Aquicambids", c("Sodic", "Durinodic Xeric", "Durinodic",
      "Petronodic", "Vitrixerandic", "Vitrandic", "Thapto-Humic", "Fluventic",
      "Xeric", "Ustic", "Typic")),
  GGB = list("Petrocambids", c("Sodic", "Vitrixerandic", "Vitrandic", "Xeric",
      "Ustic", "Typic")),
  GGC = list("Haplocambids", c("Lithic Xeric", "Lithic Ustic", "Lithic",
      "Xerertic", "Ustertic", "Vertic", "Durinodic Xeric", "Durinodic",
      "Petronodic Xeric", "Petronodic Ustic", "Petronodic", "Sodic Xeric",
      "Sodic Ustic", "Sodic", "Vitrixerandic", "Vitrandic", "Thapto-Humic",
      "Xerofluventic", "Ustifluventic", "Fluventic", "Anthropic", "Xeric",
      "Ustic", "Typic"))
)

.v09426_kst_vertisols <- list(
  FAA = list("Sulfaquerts", c("Salic", "Sulfic", "Typic")),
  FAB = list("Salaquerts", c("Aridic", "Ustic", "Leptic", "Entic", "Chromic",
      "Typic")),
  FAC = list("Duraquerts", c("Aridic", "Xeric", "Ustic", "Aeric", "Chromic",
      "Typic")),
  FAD = list("Natraquerts", c("Typic")),
  FAE = list("Calciaquerts", c("Aeric", "Typic")),
  FAF = list("Dystraquerts", c("Sulfaqueptic", "Aridic", "Ustic", "Aeric",
      "Leptic", "Entic", "Chromic", "Typic")),
  FAG = list("Epiaquerts", c("Halic", "Sodic", "Aridic", "Xeric", "Ustic",
      "Aeric", "Leptic", "Entic", "Chromic", "Typic")),
  FAH = list("Endoaquerts", c("Halic", "Sodic", "Aridic", "Xeric", "Ustic",
      "Aeric", "Leptic", "Entic", "Chromic", "Typic")),
  FBA = list("Humicryerts", c("Sodic", "Typic")),
  FBB = list("Haplocryerts", c("Sodic", "Chromic", "Typic")),
  FCA = list("Durixererts", c("Halic", "Sodic", "Aquic", "Aridic", "Udic",
      "Haplic", "Chromic", "Typic")),
  FCB = list("Calcixererts", c("Lithic", "Petrocalcic", "Aridic", "Leptic",
      "Entic", "Chromic", "Typic")),
  FCC = list("Haploxererts", c("Lithic", "Halic", "Sodic", "Aridic", "Aquic",
      "Udic", "Leptic", "Entic", "Chromic", "Typic")),
  FDA = list("Salitorrerts", c("Aquic", "Leptic", "Entic", "Chromic",
      "Typic")),
  FDB = list("Gypsitorrerts", c("Chromic", "Typic")),
  FDC = list("Calcitorrerts", c("Petrocalcic", "Leptic", "Entic", "Chromic",
      "Typic")),
  FDD = list("Haplotorrerts", c("Halic", "Sodic", "Leptic", "Entic",
      "Chromic", "Typic")),
  FEA = list("Dystrusterts", c("Lithic", "Aquic", "Aridic", "Udic", "Leptic",
      "Entic", "Chromic", "Typic")),
  FEB = list("Salusterts", c("Lithic", "Sodic", "Aquic", "Aridic", "Leptic",
      "Entic", "Chromic", "Typic")),
  FEC = list("Gypsiusterts", c("Lithic", "Halic", "Sodic", "Aridic", "Udic",
      "Leptic", "Entic", "Chromic", "Typic")),
  FED = list("Calciusterts", c("Lithic", "Halic", "Sodic", "Petrocalcic",
      "Aridic", "Udic", "Leptic", "Entic", "Chromic", "Typic")),
  FEE = list("Haplusterts", c("Lithic", "Halic", "Sodic", "Petrocalcic",
      "Gypsic", "Calcic", "Aridic Leptic", "Aridic", "Leptic Udic",
      "Entic Udic", "Chromic Udic", "Udic", "Leptic", "Entic", "Chromic",
      "Typic")),
  FFA = list("Dystruderts", c("Aquic", "Oxyaquic", "Leptic", "Entic",
      "Chromic", "Typic")),
  FFB = list("Hapluderts", c("Lithic", "Aquic", "Oxyaquic", "Leptic", "Entic",
      "Chromic", "Typic"))
)

.v09426_kst_andisols <- list(
  DAA = list("Gelaquands", c("Histic", "Turbic", "Thaptic", "Typic")),
  DAB = list("Cryaquands", c("Lithic", "Histic", "Thaptic", "Typic")),
  DAC = list("Placaquands", c("Lithic", "Duric Histic", "Duric", "Histic",
      "Thaptic", "Typic")),
  DAD = list("Duraquands", c("Histic", "Acraquoxic", "Thaptic", "Typic")),
  DAE = list("Vitraquands", c("Lithic", "Duric", "Histic", "Thaptic",
      "Typic")),
  DAF = list("Melanaquands", c("Lithic", "Acraquoxic", "Hydric Pachic",
      "Hydric", "Pachic", "Thaptic", "Typic")),
  DAG = list("Epiaquands", c("Duric", "Histic", "Alic", "Hydric", "Thaptic",
      "Typic")),
  DAH = list("Endoaquands", c("Lithic", "Duric", "Histic", "Alic", "Hydric",
      "Thaptic", "Typic")),
  DBA = list("Vitrigelands", c("Humic", "Turbic", "Typic")),
  DCA = list("Duricryands", c("Aquic", "Eutric Oxyaquic", "Oxyaquic",
      "Eutric", "Typic")),
  DCB = list("Hydrocryands", c("Lithic", "Placic", "Aquic", "Thaptic",
      "Typic")),
  DCC = list("Melanocryands", c("Lithic", "Vitric", "Typic")),
  DCD = list("Fulvicryands", c("Lithic", "Folistic", "Eutric Pachic",
      "Eutric", "Pachic", "Vitric", "Typic")),
  DCE = list("Vitricryands", c("Lithic", "Folistic", "Aquic", "Oxyaquic",
      "Spodic", "Thaptic", "Humic Xeric", "Xeric", "Ultic", "Alfic", "Humic",
      "Typic")),
  DCF = list("Haplocryands", c("Lithic", "Folistic", "Aquic", "Oxyaquic",
      "Alic", "Spodic", "Acrudoxic", "Vitric", "Thaptic", "Xeric", "Typic")),
  DDA = list("Duritorrands", c("Petrocalcic", "Vitric", "Typic")),
  DDB = list("Vitritorrands", c("Lithic", "Duric", "Aquic", "Calcic",
      "Typic")),
  DDC = list("Haplotorrands", c("Lithic", "Duric", "Calcic", "Typic")),
  DEA = list("Vitrixerands", c("Lithic", "Aquic", "Thaptic", "Alfic Humic",
      "Ultic", "Alfic", "Humic", "Typic")),
  DEB = list("Melanoxerands", c("Pachic", "Typic")),
  DEC = list("Haploxerands", c("Lithic", "Aquic", "Thaptic", "Calcic",
      "Ultic", "Alfic Humic", "Alfic", "Humic", "Typic")),
  DFA = list("Ustivitrands", c("Lithic", "Aquic", "Thaptic", "Calcic",
      "Humic", "Typic")),
  DFB = list("Udivitrands", c("Lithic", "Aquic", "Oxyaquic", "Thaptic",
      "Ultic", "Alfic", "Humic", "Typic")),
  DGA = list("Durustands", c("Aquic", "Thaptic", "Humic", "Typic")),
  DGB = list("Haplustands", c("Lithic", "Aquic", "Dystric Vitric", "Vitric",
      "Pachic", "Thaptic", "Calcic", "Dystric", "Oxic", "Ultic", "Alfic",
      "Humic", "Typic")),
  DHA = list("Placudands", c("Lithic", "Aquic", "Acrudoxic", "Hydric",
      "Typic")),
  DHB = list("Durudands", c("Aquic", "Eutric", "Acrudoxic", "Hydric",
      "Pachic", "Typic")),
  DHC = list("Melanudands", c("Lithic", "Anthraquic", "Aquic",
      "Acrudoxic Vitric", "Acrudoxic Hydric", "Acrudoxic", "Pachic Vitric",
      "Vitric", "Hydric Pachic", "Pachic", "Hydric", "Thaptic", "Ultic",
      "Eutric", "Typic")),
  DHD = list("Hydrudands", c("Lithic", "Aquic", "Acrudoxic Thaptic",
      "Acrudoxic", "Thaptic", "Eutric", "Ultic", "Typic")),
  DHE = list("Fulvudands", c("Eutric Lithic", "Lithic", "Aquic", "Oxyaquic",
      "Hydric", "Acrudoxic", "Ultic", "Eutric Pachic", "Eutric", "Pachic",
      "Thaptic", "Typic")),
  DHF = list("Hapludands", c("Lithic", "Anthraquic", "Aquic Duric", "Duric",
      "Aquic", "Oxyaquic", "Alic", "Acrudoxic Hydric", "Acrudoxic Thaptic",
      "Acrudoxic Ultic", "Acrudoxic", "Vitric", "Hydric Thaptic", "Hydric",
      "Eutric Thaptic", "Thaptic", "Eutric", "Oxic", "Ultic", "Alfic",
      "Typic"))
)

.v09426_pedon <- function(..., site = list()) {
  hz <- data.table::data.table(...)
  PedonRecord$new(
    site = utils::modifyList(list(id = "t426", lat = 0, lon = 0, country = "US"), site),
    horizons = ensure_horizon_schema(hz))
}

test_that("each great group lists exactly the KST 13th edition subgroups, in order", {
  testthat::skip_on_cran()
  rules <- load_rules("usda")
  for (lst in list(.v09426_kst_aridisols, .v09426_kst_vertisols, .v09426_kst_andisols)) {
    for (code in names(lst)) {
      gg <- lst[[code]][[1]]
      want <- paste(lst[[code]][[2]], gg)
      sg <- rules$subgroups[[code]]
      expect_identical(vapply(sg, `[[`, "", "name"), want, info = code)
      expect_identical(vapply(sg, `[[`, "", "code"),
                       paste0(code, LETTERS[seq_along(want)]), info = code)
      expect_true(isTRUE(sg[[length(sg)]]$tests$default), info = code)
      expect_true(all(vapply(sg[-length(sg)], function(x) is.null(x$tests$default), NA)),
                  info = code)
    }
  }
  # every great group of the three orders has its list
  gg <- unlist(lapply(rules$great_groups[c("GA", "GB", "GC", "GD", "GE", "GF", "GG",
                                           "FA", "FB", "FC", "FD", "FE", "FF",
                                           "DA", "DB", "DC", "DD", "DE", "DF", "DG", "DH")],
                      function(x) vapply(x, `[[`, "", "code")))
  expect_setequal(gg, c(names(.v09426_kst_aridisols), names(.v09426_kst_vertisols),
                        names(.v09426_kst_andisols)))
  # and the names agree with the KST code table where it is installed
  k <- tryCatch(kst13_codes(), error = function(e) NULL)
  if (!is.null(k)) {
    k <- k[nchar(k$code) == 4L & substr(k$code, 1, 1) %in% c("D", "F", "G"), ]
    mine <- unlist(unname(lapply(rules$subgroups[gg], function(x)
      stats::setNames(vapply(x, `[[`, "", "name"), vapply(x, `[[`, "", "code")))))
    expect_identical(tolower(mine[k$code]), tolower(stats::setNames(k$name, k$code)))
  }
})

test_that("Aridisol subgroups bordering on ustic or xeric read the regime record", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09426_pedon(top_cm = 0, bottom_cm = 50, designation = "A")
  r <- ns$sg_aridisols_border_usda(p, "ustic")
  expect_true(is.na(r$passed))
  expect_identical(r$missing, "site$soil_moisture_regime")
  p$site$soil_moisture_regime <- "aridic"
  expect_false(ns$sg_aridisols_border_usda(p, "ustic")$passed)
  p$site$soil_moisture_regime <- "ustic aridic"
  expect_true(ns$sg_aridisols_border_usda(p, "ustic")$passed)
  expect_false(ns$sg_aridisols_border_usda(p, "xeric")$passed)
  # Xeric Haplodurids: and a mean annual soil temperature below 22 C with a
  # summer-winter difference of 5 C or more (KST p. 158)
  p$site$soil_moisture_regime <- "aridic (xeric)"
  expect_true(is.na(ns$sg_aridisols_border_usda(p, "xeric", temperature = TRUE)$passed))
  p$site$soil_temperature_regime <- "thermic"
  expect_true(ns$sg_aridisols_border_usda(p, "xeric", temperature = TRUE)$passed)
  p$site$soil_temperature_regime <- "isothermic"
  expect_false(ns$sg_aridisols_border_usda(p, "xeric", temperature = TRUE)$passed)
})

test_that("Vertic Aridisols: cracks with slickensides, or the linear extensibility", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09426_pedon(top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                     designation = c("A", "Bt", "Bk"), clay_pct = c(30, 45, 40))
  r <- ns$sg_aridisols_vertic_usda(p)
  expect_true(is.na(r$passed))
  expect_identical(r$missing, "cole_value")
  p$horizons$cole_value <- c(0.02, 0.03, 0.02)
  expect_false(ns$sg_aridisols_vertic_usda(p)$passed)
  p$horizons$cole_value <- c(0.05, 0.09, 0.07)
  expect_true(ns$sg_aridisols_vertic_usda(p)$passed)
  p$horizons$cole_value <- NA_real_
  p$horizons$cracks_width_cm <- c(1, 1, NA)
  p$horizons$slickensides <- c(NA, "common", "common")
  expect_true(ns$sg_aridisols_vertic_usda(p)$passed)
})

test_that("Sodic Aridisols need a horizon 25 cm thick with ESP 15 or more", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09426_pedon(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 100),
                     designation = c("A", "Bw", "Bk"), esp_pct = c(5, 20, 8))
  expect_false(ns$sg_aridisols_sodic_usda(p)$passed)
  p$horizons$esp_pct <- c(5, 20, 18)
  expect_true(ns$sg_aridisols_sodic_usda(p)$passed)
  p$horizons$esp_pct <- c(5, 20, NA)
  r <- ns$sg_aridisols_sodic_usda(p)
  expect_true(is.na(r$passed))
  expect_true("esp_pct" %in% r$missing)
})

test_that("Vertisol crack patterns: cracks_open_days, else the recorded regime (L32)", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09426_pedon(top_cm = 0, bottom_cm = 100, designation = "Bss", clay_pct = 50)
  ck <- function(pt) ns$sg_vertisols_cracks_usda(p, pt)$passed
  expect_true(is.na(ck("aridic")))
  p$site$soil_moisture_regime <- "ustic"
  expect_false(ck("aridic")); expect_false(ck("udic")); expect_true(ck("ustic"))
  p$site$soil_moisture_regime <- "aridic"
  expect_true(ck("aridic")); expect_true(ck("xeric_aridic"))
  p$site$soil_moisture_regime <- "aquic"
  expect_true(is.na(ck("aridic"))); expect_true(is.na(ck("ustic")))
  p$site$soil_moisture_regime <- "xeric"
  expect_true(is.na(ck("xeric")))
  p$site$soil_temperature_regime <- "mesic"
  expect_true(ck("xeric"))
  p$site$cracks_open_days <- 120
  p$site$soil_moisture_regime <- "ustic"
  expect_false(ck("aridic")); expect_true(ck("udic")); expect_true(ck("ustic"))
})

test_that("Halic, Sodic, Entic and Chromic Vertisols, KST 13th ed. limits", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09426_pedon(top_cm = c(0, 10, 40, 70), bottom_cm = c(10, 40, 70, 120),
                     designation = c("Ap", "Bss1", "Bss2", "BC"),
                     clay_pct = c(45, 50, 55, 50), ec_dS_m = c(3, 20, 8, 4),
                     esp_pct = c(4, 6, 10, 12),
                     munsell_value_moist = c(3, 3, 4, 5),
                     munsell_chroma_moist = c(2, 2, 2, 3))
  # Halic: 15 dS/m through 15 cm (not the 30 dS/m of the Histosols)
  expect_true(ns$sg_vertisols_halic_usda(p)$passed)
  p$horizons$ec_dS_m <- c(3, 20, 8, NA)
  expect_true(ns$sg_vertisols_halic_usda(p)$passed)
  p$horizons$ec_dS_m <- c(3, 10, NA, 4)
  expect_true(is.na(ns$sg_vertisols_halic_usda(p)$passed))
  # Sodic: ESP 15 in any horizon within 100 cm (not the natric horizon)
  expect_false(ns$sg_vertisols_sodic_usda(p)$passed)
  p$horizons$esp_pct <- c(4, 6, 16, 12)
  expect_true(ns$sg_vertisols_sodic_usda(p)$passed)
  # Entic: a layer 25 cm thick with less than 27 % clay
  expect_false(ns$sg_vertisols_entic_usda(p)$passed)
  p$horizons$clay_pct <- c(45, 50, 20, 22)
  expect_true(ns$sg_vertisols_entic_usda(p)$passed)
  p$horizons$clay_pct <- c(45, 50, NA, NA)
  p$horizons$texture_class_usda <- c(NA, NA, "sandy loam", "loam")
  expect_true(ns$sg_vertisols_entic_usda(p)$passed)
  # Chromic: a moist value of 3 with no dry colour is undecided; 2 is not
  r <- ns$sg_vertisols_chromic_usda(p)
  expect_true(is.na(r$passed))
  expect_true("munsell_value_dry" %in% r$missing)
  p$horizons$munsell_value_moist <- c(2, 2, 4, 5)
  expect_false(ns$sg_vertisols_chromic_usda(p)$passed)
  p$horizons$munsell_chroma_moist <- c(3, 2, 2, 3)
  expect_true(ns$sg_vertisols_chromic_usda(p)$passed)
  expect_false(ns$sg_vertisols_chromic_usda(p, chroma = FALSE)$passed)
})

test_that("Haplusterts: the KST order reaches the Udic compounds", {
  testthat::skip_on_cran()
  p <- make_vertisol_canonical()
  p$site$soil_moisture_regime <- "ustic"
  expect_identical(classify_usda(p, on_missing = "silent")$name, "Chromic Haplusterts")
  p$site$cracks_open_days <- 100
  expect_identical(classify_usda(p, on_missing = "silent")$name, "Chromic Udic Haplusterts")
  p$horizons$na_cmol <- c(0.4, 9, 0.6)
  expect_identical(classify_usda(p, on_missing = "silent")$name, "Sodic Haplusterts")
})

test_that("Andisol subgroup tests: Acrudoxic, Alic and Eutric, Pachic, Vitric", {
  testthat::skip_on_cran()
  ns <- asNamespace("soilKey")
  p <- .v09426_pedon(top_cm = c(0, 25, 60, 100), bottom_cm = c(25, 60, 100, 150),
                     designation = c("A", "Bw1", "Bw2", "C"),
                     oc_pct = c(8, 5, 2, 1),
                     ca_cmol = c(1, 0.3, 0.2, 0.2), mg_cmol = c(0.5, 0.1, 0.1, 0.1),
                     k_cmol = c(0.2, 0.1, 0.1, 0.1), na_cmol = c(0.1, 0.1, 0.1, 0.1),
                     al_kcl_cmol = c(1, 1, 0.5, 0.5))
  # bases + KCl Al under 2.0 in 30 cm or more between 25 and 100 cm
  expect_true(ns$sg_andisols_acric_usda(p)$passed)
  p$horizons$al_kcl_cmol <- c(1, 3, 3, 0.5)
  expect_false(ns$sg_andisols_acric_usda(p)$passed)
  # KCl Al over 2.0 in 10 cm between 25 and 50 cm: Alic (and not Eutric)
  expect_true(ns$sg_andisols_alic_usda(p)$passed)
  p$horizons$al_kcl_cmol <- c(1, NA, 0.5, 0.5)
  expect_true(is.na(ns$sg_andisols_alic_usda(p)$passed))
  p$horizons$oc_pct <- c(8, 4.5, 2, 1)
  # Pachic of the Fulvi- great groups: > 6 % weighted, > 4 % everywhere,
  # through 50 cm within 60 cm
  expect_false(ns$sg_andisols_pachic_usda(p, fulvic = TRUE)$passed)
  p$horizons$oc_pct <- c(9, 6, 2, 1)
  expect_true(ns$sg_andisols_pachic_usda(p, fulvic = TRUE)$passed)
  # Eutric (Hapludands): bases over 25 cmol in 15 cm between 25 and 75 cm
  expect_false(ns$sg_andisols_eutric_bases_usda(p)$passed)
  p$horizons$ca_cmol <- c(1, 30, 0.2, 0.2)
  expect_true(ns$sg_andisols_eutric_bases_usda(p)$passed)
  # Vitric: 1500 kPa under 15 % in 25 cm of andic layers within 100 cm
  v <- .v09426_pedon(top_cm = c(0, 30), bottom_cm = c(30, 100),
                     designation = c("A", "Bw"), oc_pct = c(3, 1),
                     bulk_density_g_cm3 = c(0.8, 0.85), al_ox_pct = c(2.5, 2.5),
                     fe_ox_pct = c(1, 1), phosphate_retention_pct = c(90, 90),
                     water_content_1500kpa = c(12, 10))
  expect_true(ns$sg_andisols_vitric_usda(v)$passed)
  v$horizons$water_content_1500kpa <- c(40, 35)
  expect_false(ns$sg_andisols_vitric_usda(v)$passed)
})

test_that("canonical fixtures: Andosol Hydric Melanudands, Calcisol Typic Haplocalcids", {
  testthat::skip_on_cran()
  expect_identical(classify_usda(make_andosol_canonical(), on_missing = "silent")$name,
                   "Hydric Melanudands")
  p <- make_calcisol_canonical(); p$site$soil_moisture_regime <- "aridic"
  r <- classify_usda(p, on_missing = "silent")
  expect_identical(r$name, "Typic Haplocalcids")
  # Vertic (no COLE measured) and Aquic (no saturation record) are undecided
  # before Typic
  st <- vapply(r$trace$subgroups, function(t) is.na(t$passed), NA)
  expect_true(all(c("GFBD", "GFBF") %in% names(st)[st]))
})
