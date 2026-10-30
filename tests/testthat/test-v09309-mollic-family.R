# v0.9.309: the mollic family (Chernozems, Kastanozems, Phaeozems, Umbrisols)
# by the text of WRB 2022: Ch 4 pp. 111-114, the chernic (3.1.6) and umbric
# (3.1.39) horizons, protocalcic properties (3.2.8) and base saturation read
# from pH (Annex 2, Ch 9.13, Table 9.2).

ph_pedon <- function() make_phaeozem_canonical()

# ---- base saturation from pH (Annex 2, Table 9.2) -------------------------

test_that("Table 9.2: pH for a base saturation of 50% by organic carbon", {
  skip_on_cran()
  f <- soilKey:::.wrb_bs50_from_ph
  expect_true(f(1, 5.0, NA));  expect_false(f(1, 4.9, NA))
  expect_true(f(3, 5.3, NA));  expect_false(f(3, 5.2, NA))
  expect_true(f(10, 5.7, NA)); expect_false(f(10, 5.6, NA))
  # pHKCl only where pHwater is not recorded
  expect_true(f(1, NA, 4.0));  expect_false(f(3, NA, 4.4))
  expect_false(f(1, 4.5, 6))   # pHwater decides
  # Corg not recorded: only where the three rows agree
  expect_true(f(NA, 5.8, NA)); expect_false(f(NA, 4.9, NA))
  expect_true(is.na(f(NA, 5.4, NA)))
  # organic material, or no pH: undecided
  expect_true(is.na(f(25, 6, NA))); expect_true(is.na(f(1, NA, NA)))
})

test_that("BS status: measured, summed from the bases, then pH", {
  skip_on_cran()
  h <- data.frame(top_cm = 0, bottom_cm = 20, bs_pct = NA_real_,
                  ca_cmol = 4, mg_cmol = 1, k_cmol = 0.5, na_cmol = 0.5,
                  cec_cmol = 10, oc_pct = 1, ph_h2o = 4.5, ph_kcl = NA_real_)
  s <- soilKey:::.wrb_bs_status(h, 1)
  expect_equal(s$value, 60); expect_true(s$passed)
  h$ca_cmol <- NA
  s <- soilKey:::.wrb_bs_status(h, 1)
  expect_false(s$passed); expect_match(s$source, "pH water")
  h$ph_h2o <- NA
  expect_true(is.na(soilKey:::.wrb_bs_status(h, 1)$passed))
  # pH answers the 50% threshold only
  h$ph_h2o <- 7
  expect_true(is.na(soilKey:::.wrb_bs_status(h, 1, min_bs = 80)$passed))
})


# ---- Phaeozems (Ch 4, p. 113) ---------------------------------------------

test_that("phaeozem: mollic + BS >= 50% to 100 cm, carbonates allowed", {
  skip_on_cran()
  expect_true(isTRUE(phaeozem(ph_pedon())$passed))
  p <- ph_pedon()
  p$horizons$caco3_pct <- c(0, 2, 10, 20)
  expect_true(isTRUE(phaeozem(p)$passed))
})

test_that("phaeozem fails on BS < 50% within 100 cm, not below it", {
  skip_on_cran()
  p <- ph_pedon()
  p$horizons$bs_pct[3] <- 40          # Bw 60-100
  expect_false(isTRUE(phaeozem(p)$passed))
  expect_false(is.na(phaeozem(p)$passed))
  p <- ph_pedon()
  p$horizons$bs_pct[4] <- 30          # C 100-150, below the window
  expect_true(isTRUE(phaeozem(p)$passed))
})

test_that("phaeozem reads pH where BS is missing, else is undecided", {
  skip_on_cran()
  p <- ph_pedon()
  p$horizons$bs_pct[3] <- NA; p$horizons$ca_cmol[3] <- NA
  expect_true(isTRUE(phaeozem(p)$passed))           # pH 6.8
  p$horizons$ph_h2o[3] <- 4.6
  expect_false(isTRUE(phaeozem(p)$passed))          # OC 0.8, pH < 5.0
  p$horizons$ph_h2o[3] <- NA; p$horizons$ph_kcl[3] <- NA
  expect_true(is.na(phaeozem(p)$passed))
  expect_true("bs_pct" %in% phaeozem(p)$missing)
})

test_that("phaeozem stops at a limiting layer; a short description is decided on what is described", {
  skip_on_cran()
  p <- ph_pedon()
  h <- p$horizons
  h$designation[3] <- "R"; h$bs_pct[3:4] <- NA; h$ph_h2o[3:4] <- NA
  h$ph_kcl[3:4] <- NA; h$ca_cmol[3:4] <- NA
  p$horizons <- h
  expect_true(isTRUE(phaeozem(p)$passed))           # continuous rock at 60 cm
  p <- ph_pedon()
  p$horizons <- p$horizons[1:2, ]                    # described to 60 cm only
  # Hugo (2026-10-29): decided on the part that is described
  expect_true(isTRUE(phaeozem(p)$passed))
})


# ---- protocalcic properties (3.2.8, p. 78) ---------------------------------

proto_pedon <- function(...) {
  hz <- data.table::data.table(top_cm = c(0, 30), bottom_cm = c(30, 80),
                               designation = c("A", "Bk"), ...)
  PedonRecord$new(site = list(id = "pc", lat = 45, lon = 30, country = "TEST"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("protocalcic properties are visible secondary carbonates", {
  skip_on_cran()
  expect_true(isTRUE(protocalcic_properties(
    proto_pedon(caco3_pct = c(0, 5), secondary_carbonates_pct = c(0, 10)))$passed))
  expect_false(isTRUE(protocalcic_properties(
    proto_pedon(caco3_pct = c(0, 5), secondary_carbonates_pct = c(0, 3)))$passed))
  expect_false(is.na(protocalcic_properties(
    proto_pedon(caco3_pct = c(0, 5), secondary_carbonates_pct = c(0, 3)))$passed))
})

test_that("lab CaCO3 alone leaves protocalcic properties undecided", {
  skip_on_cran()
  r <- protocalcic_properties(proto_pedon(caco3_pct = c(0, 8)))
  expect_true(is.na(r$passed))                         # k suffix, no abundance
  expect_identical(r$evidence$layer_status, c(FALSE, NA))
  expect_true("secondary_carbonates_pct" %in% r$missing)
  expect_false(isTRUE(protocalcic_properties(proto_pedon(caco3_pct = c(0, 0)))$passed))
  expect_false(is.na(protocalcic_properties(proto_pedon(caco3_pct = c(0, 0)))$passed))
  r <- protocalcic_properties(proto_pedon(caco3_pct = c(NA, NA),
                                          effervescence_class = c("none", "none")))
  expect_false(isTRUE(r$passed)); expect_false(is.na(r$passed))
})

test_that("a k suffix counts only under the morphological inference option", {
  skip_on_cran()
  p <- proto_pedon(caco3_pct = c(0, 8))
  old <- options(soilKey.morphological_inference = TRUE)
  on.exit(options(old))
  expect_true(isTRUE(protocalcic_properties(p)$passed))
  options(soilKey.morphological_inference = FALSE)
  expect_true(is.na(protocalcic_properties(p)$passed))
})


# ---- Kastanozems (p. 112) and Chernozems (p. 111) ---------------------------

test_that("kastanozem_strict: carbonate layer <= 70 cm, BS >= 50% above it", {
  skip_on_cran()
  p <- make_kastanozem_canonical()
  r <- kastanozem_strict(p)
  expect_true(isTRUE(r$passed))
  expect_identical(r$evidence$carbonate_layer, 3L)
  # protocalcic layer below 70 cm only
  p$horizons$secondary_carbonates_pct <- c(0, 0, 0, 10)
  p$horizons$top_cm[3:4] <- c(60, 75); p$horizons$bottom_cm[3] <- 75
  p$horizons$secondary_carbonates_pct[3] <- 2
  expect_false(isTRUE(kastanozem_strict(p)$passed))
  expect_false(is.na(kastanozem_strict(p)$passed))
})

test_that("kastanozem_strict: BS < 50% above the carbonate layer fails", {
  skip_on_cran()
  p <- make_kastanozem_canonical()
  p$horizons$bs_pct[2] <- 45
  expect_false(isTRUE(kastanozem_strict(p)$passed))
  # BS missing above: read from pH (7.4 here)
  p <- make_kastanozem_canonical()
  p$horizons$bs_pct[2] <- NA; p$horizons$ca_cmol[2] <- NA
  expect_true(isTRUE(kastanozem_strict(p)$passed))
})

test_that("a carbonate layer at the mineral soil surface has nothing above it", {
  skip_on_cran()
  p <- make_kastanozem_canonical()
  p$horizons$bs_pct[2] <- 30                       # AB, above the Bk
  expect_false(isTRUE(kastanozem_strict(p)$passed))
  # secondary carbonates from the mineral soil surface down
  p$horizons$secondary_carbonates_pct <- c(10, 10, 10, 5)
  p$horizons$caco3_pct <- c(3, 4, 7, 14)
  r <- kastanozem_strict(p)
  expect_true(isTRUE(r$passed))
  expect_identical(r$evidence$carbonate_layer, 1L)
  expect_length(r$evidence$bs_above$layers, 0L)
})

test_that("kastanozem_strict is undecided when carbonates are lab CaCO3 only", {
  skip_on_cran()
  p <- make_kastanozem_canonical()
  p$horizons$secondary_carbonates_pct <- NA_real_
  r <- kastanozem_strict(p)
  expect_true(is.na(r$passed))
  expect_true("secondary_carbonates_pct" %in% r$missing)
})

test_that("the carbonate layer must lie above a petrocalcic horizon", {
  skip_on_cran()
  p <- make_kastanozem_canonical()
  h <- p$horizons
  h$designation[3] <- "Bkm"; h$caco3_pct[3] <- 60
  h$cementation_class[3] <- "strongly"; h$secondary_carbonates_pct[3] <- 60
  p$horizons <- h
  expect_true(isTRUE(petrocalcic(p)$passed))
  expect_false(isTRUE(kastanozem_strict(p)$passed))
})

test_that("chernozem_strict: within 50 cm of the lower limit of the mollic", {
  skip_on_cran()
  p <- make_chernozem_canonical()
  r <- chernozem_strict(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$mollic_lower_limit_cm, 100)
  # no carbonates in the B (100-160): the Ck at 160 cm is 60 cm below the mollic
  h <- p$horizons
  h$designation[4] <- "Bw"; h$secondary_carbonates_pct[4] <- 0; h$caco3_pct[4] <- 0
  h$bottom_cm[4] <- 160; h$top_cm[5] <- 160; h$bottom_cm[5] <- 200
  p$horizons <- h
  expect_false(isTRUE(chernozem_strict(p)$passed))
  expect_false(is.na(chernozem_strict(p)$passed))
  p <- make_chernozem_canonical()
  p$horizons$secondary_carbonates_pct <- NA_real_
  expect_true(is.na(chernozem_strict(p)$passed))
})


# ---- chernic horizon (3.1.6, pp. 40-41) ------------------------------------

test_that("chernic: criteria 1-7, no worm holes needed", {
  skip_on_cran()
  p <- make_chernozem_canonical()
  p$horizons$worm_holes_pct <- NA_real_
  r <- chernic(p)
  expect_true(isTRUE(r$passed))
  expect_identical(r$layers, 1:3)
  # chroma 3 in the upper 20 cm (the Kastanozem fixture)
  expect_false(isTRUE(chernic(make_kastanozem_canonical())$passed))
})

test_that("chernic structure: granular or fine subangular blocky", {
  skip_on_cran()
  p <- make_chernozem_canonical()
  p$horizons$structure_type[1] <- "subangular blocky"
  p$horizons$structure_size[1] <- "coarse"
  expect_false(isTRUE(chernic(p)$passed))
  p$horizons$structure_size[1] <- "fine"
  expect_true(isTRUE(chernic(p)$passed))
  p$horizons$structure_type[1] <- "angular blocky"
  expect_false(isTRUE(chernic(p)$passed))
  p$horizons$structure_type[1] <- NA
  expect_true(is.na(chernic(p)$passed))
})

test_that("chernic: BS, organic carbon and thickness per layer", {
  skip_on_cran()
  p <- make_chernozem_canonical()
  p$horizons$bs_pct[1] <- 40
  expect_false(isTRUE(chernic(p)$passed))
  p <- make_chernozem_canonical()
  p$horizons$oc_pct[2] <- 0.8                      # Ah2: < 1% SOC
  expect_true(isTRUE(chernic(p)$passed))           # Ah1 alone is 30 cm
  p$horizons$bottom_cm[1] <- 25; p$horizons$top_cm[2] <- 25
  expect_false(isTRUE(chernic(p)$passed))          # 25 cm < 30
  p <- make_chernozem_canonical()
  p$horizons$munsell_chroma_moist[1] <- 3          # surface layer fails colour
  expect_false(isTRUE(chernic(p)$passed))
})

test_that("chernic criterion 5: SOC above a dark parent material", {
  skip_on_cran()
  p <- make_chernozem_canonical()
  p$horizons$munsell_value_moist[5] <- 4            # Ck, dark parent material
  p$horizons$oc_pct[5] <- 1.0
  r <- chernic(p)
  expect_true(isTRUE(r$passed))                     # Ah1, Ah2 >= 2.0%
  expect_identical(r$layers, 1:2)                   # AB 1.5% < 1.0 + 1
  p$horizons$oc_pct[5] <- 3.5
  expect_false(isTRUE(chernic(p)$passed))
})

test_that("chernic colour 4b: 15-40% CaCO3 lifts the dry value limit", {
  skip_on_cran()
  p <- make_chernozem_canonical()
  p$horizons$munsell_value_dry[1:3] <- 6
  expect_false(isTRUE(chernic(p)$passed))           # 4a: dry value > 5
  p$horizons$caco3_pct[1:3] <- 20
  expect_true(isTRUE(chernic(p)$passed))            # 4b
})


# ---- umbric horizon (3.1.39, pp. 69-70) ------------------------------------

test_that("umbric: the dark stack from the mineral soil surface", {
  skip_on_cran()
  p <- make_umbrisol_canonical()
  r <- umbric_horizon(p)
  expect_true(isTRUE(r$passed))
  expect_identical(r$evidence$color$layers, 1:2)
  # an organic layer on top is not part of the mineral soil surface
  h <- p$horizons
  o <- h[1, ]
  o$designation <- "Oa"; o$top_cm <- 0; o$bottom_cm <- 10; o$oc_pct <- 30
  o$munsell_value_moist <- 2; o$munsell_chroma_moist <- 1; o$bs_pct <- 60
  h$top_cm <- h$top_cm + 10; h$bottom_cm <- h$bottom_cm + 10
  p$horizons <- data.table::rbindlist(list(o, h))
  r <- umbric_horizon(p)
  expect_true(isTRUE(r$passed))
  expect_identical(r$evidence$color$layers, 2:3)
})

test_that("umbric: thin layers add up; the BS is a weighted average", {
  skip_on_cran()
  p <- make_umbrisol_canonical()
  h <- p$horizons
  h$top_cm <- c(0, 10, 40); h$bottom_cm <- c(10, 40, 150)
  h$bs_pct <- c(60, 30, 22)
  p$horizons <- h
  r <- umbric_horizon(p)
  expect_true(isTRUE(r$passed))                    # (60*10 + 30*30) / 40 = 37.5
  p$horizons$bs_pct <- c(30, 60, 22)
  expect_false(isTRUE(umbric_horizon(p)$passed))   # 52.5
  # BS missing: pH 4.8 and 5.0 at 4.5% and 2% SOC are below 50% (Table 9.2)
  p <- make_umbrisol_canonical()
  p$horizons$bs_pct <- NA_real_; p$horizons$ca_cmol <- NA_real_
  expect_true(isTRUE(umbric_horizon(p)$passed))
})

test_that("umbric is undecided when a layer of unknown colour could make it", {
  skip_on_cran()
  p <- make_umbrisol_canonical()
  h <- p$horizons
  h$top_cm <- c(0, 15, 70); h$bottom_cm <- c(15, 70, 150)
  h$munsell_value_moist[2] <- 3; h$munsell_chroma_moist[2] <- NA
  h$munsell_value_dry[2] <- NA; h$munsell_chroma_dry[2] <- NA
  h$oc_pct[2] <- 1.0
  p$horizons <- h
  expect_true(is.na(umbric_horizon(p)$passed))
})


# ---- the mollic colour test with no chroma ---------------------------------

test_that("mollic colour: a missing chroma is undecided, a light value fails", {
  skip_on_cran()
  h <- data.frame(top_cm = 0, bottom_cm = 25, munsell_value_moist = 3,
                  munsell_chroma_moist = NA_real_, munsell_value_dry = NA_real_,
                  munsell_chroma_dry = NA_real_, oc_pct = 1.0)
  r <- soilKey:::test_mollic_color(h)
  expect_true(is.na(r$passed))
  expect_true("munsell_chroma_moist" %in% r$missing)
  h$munsell_value_moist <- 5
  expect_false(isTRUE(soilKey:::test_mollic_color(h)$passed))
  expect_false(is.na(soilKey:::test_mollic_color(h)$passed))
  h$munsell_value_moist <- 3; h$munsell_chroma_dry <- 2
  expect_true(isTRUE(soilKey:::test_mollic_color(h)$passed))
})


# ---- the key: Umbrisols take an umbric or mollic or hortic horizon (p. 114) --

test_that("a mollic soil that fails the Phaeozems keys out as an Umbrisol", {
  skip_on_cran()
  p <- ph_pedon()
  p$horizons$bs_pct[3:4] <- c(35, 30)              # Bw 60-100 below 50%
  expect_false(isTRUE(phaeozem(p)$passed))
  expect_false(isTRUE(umbric_horizon(p)$passed))
  expect_true(isTRUE(mollic(p)$passed))
  r <- classify_wrb2022(p, on_missing = "silent")
  expect_identical(as.character(r$rsg_or_order), "Umbrisols")
})
