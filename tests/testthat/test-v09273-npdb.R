# v0.9.273: the National Pedon Database of Canada as a benchmark for the
# Canadian key. The tests write a small synthetic set in the NPDB layout (no
# NPDB record is shipped), with the quirks of the real files: a byte-order
# mark, blank fields written as a space, organic horizons recorded upward,
# two colour formats and total N in three units.

.npdb_fixture <- function() {
  d <- file.path(tempdir(), paste0("npdb-", as.integer(stats::runif(1, 1, 1e8))))
  dir.create(d)
  w <- function(name, df) {
    f <- file.path(d, name)
    con <- file(f, open = "wb")
    writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)   # byte-order mark
    df[] <- lapply(df, function(v) ifelse(is.na(v), " ", as.character(v)))
    lines <- c(paste(names(df), collapse = ","), do.call(paste, c(df, sep = ",")))
    writeBin(charToRaw(paste0(paste(lines, collapse = "\r\n"), "\r\n")), con)   # CR-LF records
    close(con)
  }
  pid <- sprintf("%07d", c(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4))
  lay <- c(1:3, 1:4, 1:3, 1:4)
  w("Info.csv", data.frame(OID_ = 1:4, PROVINCE = c("alberta", "alberta", "quebec", "manitoba"),
    DATASET_ID = 1:4, PEDON_ID = sprintf("%07d", 1:4),
    CSSC_ORDER = c("chernozem", "luvisol", "regosol", "chernozem"),
    CSSC_GTGRP = c("dark brown", "gray", "regosol", "brown"),
    CSSC_SBGRP = c("orthic", "orthic", "gleyed cumulous", "solodic"),
    DD_LAT_N = c(51.7, 54.1, 46.8, 49.5), DD_LONG_N = c(-112.1, -113.5, -71.2, -98.2)))
  w("Horizons.csv", data.frame(OID_ = 1:14, PEDON_ID = pid, LAYER_ID = lay,
    U_DEPTH = c(0, 18, 40, 5, 0, 15, 50, 0, 10, 60, 0, 15, 35, 65),
    L_DEPTH = c(18, 40, 90, 0, 15, 50, NA, 10, 60, 100, 15, 35, 65, 0),
    HZN_MAS = c("A", "B", "C", "LFH", "A", "B", "C", "A", "C", "C", "A", "B", "C", "C"),
    HZN_SUF = c("h", "m", "k", NA, "e", "t", "k", "h", NA, NA, "h", "nt", "ca", "kgj"),
    HZN_MOD = NA, HZN_LIT = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 2, NA, NA, NA, NA)))
  w("Morphology.csv", data.frame(OID_ = 1:14, PEDON_ID = pid, LAYER_ID = lay,
    COLOUR_1 = c("100yr3020", "100yr4030", "025y 5030", NA, "10.0yr6.0/2.0", "100yr4040", "025y 5040",
                 "10yr2010", "n 50", "025y 4020", "100yr3020", "100yr4030", "025y 6020", "025y 5020"),
    COLOUR_2 = c("100yr4020", "100yr5030", "025y 6030", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    S_PRM_KIND = c("granular", "prismatic", "massive", NA, "platy", "subangular\rblocky", "massive",
                   "granular", "single grained", "massive", "fine", "columnar", "massive", "massive"),
    S_PRM_MOD = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "granular", NA, NA, NA),
    CONSIS_DRY = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, "rigid", NA, NA)))
  w("Physical.csv", data.frame(OID_ = 1:14, PEDON_ID = pid, LAYER_ID = lay,
    T_CLAY = c(25, 26, 24, NA, 12, 28, 26, 10, 9, 8, 30, 35, 30, 28),
    T_SAND = c(40, 40, 42, NA, 50, 40, 41, -0.8, 60, 62, 30, 25, 30, 32)))
  w("Chemical.csv", data.frame(OID_ = 1:14, PEDON_ID = pid, LAYER_ID = lay,
    CARB_ORG = c(3, 1, 0.4, 30, 1, 0.5, 0.3, 2, 0.5, 0.4, 2.5, 1, 0.5, 0.3),
    N_TOTAL = c(0.25, 0.09, 0.04, NA, 0.8, 0.4, NA, 9, 9, 9, 0.3, 0.3, 0.3, NA),
    PH_H2O = c(7, 7.2, 8, NA, 5.5, 6, 7.8, 6.5, 6.8, 7, 7.0, 7.4, 7.9, 8.0),
    PH_CACL2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 7.5, 7.9, 8.3, 8.4),
    EXCH_CA = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 20, 12, NA, NA),
    EXCH_MG = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 5, 8, NA, NA),
    EXCH_K = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 0.5, NA, NA),
    EXCH_NA = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 0.2, 4, NA, NA),
    CEC = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, 28, 25, NA, NA),
    BASE_SATN = c(95, 98, 964, NA, 60, 70, 100, 80, 85, 90, 45, 40, NA, NA)))
  w("Profiles.csv", data.frame(OID_ = 1:4, PEDON_ID = sprintf("%07d", 1:4),
    DRAINAGE = c("well drained", "moderately well drained", "imperfectly drained", "well drained")))
  d
}

test_that("load_npdb_pedons reads the NPDB layout", {
  skip_on_cran()
  d <- .npdb_fixture()
  p <- load_npdb_pedons(d, verbose = FALSE)
  expect_named(p, c("1", "2", "3", "4"))
  expect_true(all(vapply(p, inherits, TRUE, "PedonRecord")))
  # designations in the Canadian notation, from their parts
  expect_identical(p[["1"]]$horizons$designation, c("Ah", "Bm", "Ck"))
  expect_identical(p[["3"]]$horizons$designation, c("Ah", "C", "IIC"))
  # the LFH recorded upward sits above the mineral surface; the profile starts at 0
  h2 <- p[["2"]]$horizons
  expect_identical(h2$designation, c("LFH", "Ae", "Bt", "Ck"))
  expect_equal(h2$top_cm, c(0, 5, 20, 55))
  expect_equal(h2$bottom_cm, c(5, 20, 55, NA))
  # a deepest mineral horizon with reversed depths keeps its top, open below
  h4 <- p[["4"]]$horizons
  expect_equal(h4$top_cm, c(0, 15, 35, 65))
  expect_true(is.na(h4$bottom_cm[4]))
  expect_false(p[["4"]]$site$npdb_depth_order_mismatch)
  # colours: COLOUR_2, lighter on every horizon, is the dry colour
  expect_identical(p[["1"]]$horizons$munsell_hue_moist, c("10YR", "10YR", "2.5Y"))
  expect_equal(p[["1"]]$horizons$munsell_value_moist, c(3, 4, 5))
  expect_equal(p[["1"]]$horizons$munsell_value_dry, c(4, 5, 6))
  expect_true(all(is.na(h2$munsell_value_dry)))
  expect_equal(h2$munsell_chroma_moist[2], 2)
  expect_identical(p[["3"]]$horizons$munsell_hue_moist[1:2], c("10YR", "N"))
  # total N: percent, g/kg, the no-data code 9, and a value copied down the profile
  expect_equal(p[["1"]]$horizons$n_total_pct, c(0.25, 0.09, 0.04))
  expect_equal(h2$n_total_pct[2:3], c(0.08, 0.04))
  expect_true(all(is.na(p[["3"]]$horizons$n_total_pct)))
  expect_true(all(is.na(h4$n_total_pct)))
  # base saturation from the bases and the CEC where the recorded one disagrees
  expect_equal(h4$bs_pct[1:2], c(100 * 26.2 / 28, 98))
  # a dataset with the two pH columns swapped
  expect_equal(h4$ph_h2o[1], 7.5)
  expect_equal(h4$ph_cacl2[1], 7.0)
  # impossible values are not read; structure and consistence words
  expect_true(is.na(p[["1"]]$horizons$bs_pct[3]))
  expect_true(is.na(p[["3"]]$horizons$sand_pct[1]))
  expect_identical(h2$structure_type[3], "subangular blocky")
  expect_identical(h4$structure_type[1], "granular")
  expect_identical(h4$rupture_resistance[2], "extremely hard")
  # the site and the survey's classification as 1998 names
  s <- p[["2"]]$site
  expect_identical(s$designation_system, "cssc")
  expect_identical(s$drainage_class, "moderately well drained")
  expect_identical(c(s$reference_cssc_order, s$reference_cssc_great_group, s$reference_cssc_subgroup),
                   c("Luvisolic", "Gray Luvisol", "Orthic Gray Luvisol"))
  expect_identical(p[["3"]]$site$reference_cssc_subgroup, "Gleyed Cumulic Regosol")
  expect_identical(p[["4"]]$site$reference_cssc_subgroup, "Solonetzic Brown Chernozem")
  expect_length(load_npdb_pedons(d, max_n = 2, seed = 1, verbose = FALSE), 2L)
  expect_error(load_npdb_pedons(file.path(d, "nowhere")), "not found")
})

test_that("NPDB Munsell codes are read", {
  m <- soilKey:::.npdb_munsell(c("100yr4030", "025y 4020", "10.0yr4.0/3.0", "075yr3020", "junk", NA,
                                 "10yr2010", "5yr3020", "n 50", "500yr4030", "005y 0401"))
  expect_identical(m$hue, c("10YR", "2.5Y", "10YR", "7.5YR", NA, NA, "10YR", "5YR", "N", NA, NA))
  expect_equal(m$value, c(4, 4, 4, 3, NA, NA, 2, 3, 5, NA, NA))
})

test_that("the survey's words become 1998 names, or NA", {
  skip_on_cran()
  rules <- load_rules("cssc3")
  ref <- function(o, g, s) unlist(soilKey:::.npdb_reference(o, g, s, rules))
  expect_identical(ref("chernozem", "dark brown", "orthic"),
                   c(order = "Chernozemic", great_group = "Dark Brown Chernozem",
                     subgroup = "Orthic Dark Brown Chernozem"))
  expect_identical(ref("luvisol", "gray", "dark")[["subgroup"]], "Dark Gray Luvisol")
  expect_identical(ref("luvisol", "gray luvisol", "gleyed dark gray")[["subgroup"]],
                   "Gleyed Dark Gray Luvisol")
  expect_identical(ref("solonetz", "solidized solonetz", NA)[["great_group"]], "Solodized Solonetz")
  expect_identical(ref("organic", "fibric", "terric")[["subgroup"]], "Terric Fibrisol")
  # names with no 1998 class
  expect_true(is.na(ref("brunisol", "humo-ferric", "orthic")[["great_group"]]))
  expect_identical(ref("chernozem", "dark brown", "solodic")[["subgroup"]],
                   "Solonetzic Dark Brown Chernozem")
  expect_identical(ref("regosol", "rego", "gleyed rego")[["subgroup"]], "Gleyed Regosol")
  expect_identical(ref("cryosol", "static", "rego")[["subgroup"]], "Regosolic Static Cryosol")
  # aliases stay in their own order or great group
  expect_true(is.na(ref("chernozem", "rego", "cumulic")[["great_group"]]))
  expect_true(is.na(ref("solonetz", "solod", "dark")[["subgroup"]]))
  expect_true(is.na(ref(NA, NA, NA)[["order"]]))
})

test_that("benchmark_npdb compares and sets the Chernozems apart", {
  skip_on_cran()
  p <- load_npdb_pedons(.npdb_fixture(), verbose = FALSE)
  b <- benchmark_npdb(p, level = "order", verbose = FALSE)
  expect_identical(b$level, "order")
  expect_identical(b$n_compared, 2L)
  expect_true(b$accuracy >= 0 && b$accuracy <= 1)
  expect_identical(nrow(b$predictions), 4L)
  expect_identical(sum(b$set_aside), 2L)
  expect_named(b$per_class_recall, c("reference", "n", "recall"))
  b2 <- benchmark_npdb(p, level = "great_group", set_aside = character(0), verbose = FALSE)
  expect_identical(b2$n_compared, 4L)
  expect_null(b2$set_aside)
  expect_error(benchmark_npdb(list()), "non-empty")
})
