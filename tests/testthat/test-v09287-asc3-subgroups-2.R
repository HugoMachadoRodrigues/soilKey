# v0.9.287: subgroups of the Australian Soil Classification for the
# Dermosols, Kandosols and Ferrosols (read on the B2, a strongly acid B2,
# argic and bauxitic horizons in the B, a snuffy A), and the dark horizon of
# the Chernic suborders decided over every extent it may have.

sg_s2 <- function(p) { lv <- classify_asc3(p, on_missing = "silent")$levels; lv$code[lv$level == "subgroups"] }
ex_s2 <- function(order, h = list(), extra = NULL) {
  p <- make_asc3_example(order)
  d <- as.data.frame(p$horizons)
  for (k in names(h)) d[[k]] <- h[[k]]
  if (!is.null(extra)) d <- rbind(d, extra[names(d)])
  PedonRecord$new(site = p$site, horizons = ensure_horizon_schema(data.table::as.data.table(d)))
}
row_like <- function(p, i, ...) { d <- as.data.frame(p$horizons)[i, ]; v <- list(...); for (k in names(v)) d[[k]] <- v[[k]]; d }

test_that("the examples key to a subgroup, named on the great group", {
  skip_on_cran()
  nm <- function(o) classify_asc3(make_asc3_example(o), on_missing = "silent")$name
  expect_identical(nm("dermosols"), "Haplic, Eutrophic, Brown Dermosol")
  expect_identical(nm("kandosols"), "Haplic, Mesotrophic, Red Kandosol")
  expect_identical(nm("ferrosols"), "Haplic, Eutrophic, Red Ferrosol")
  expect_length(classify_asc3(make_asc3_example("ferrosols"), on_missing = "silent")$provisional_because, 0L)
})

test_that("Acidic subgroups: the major part of the B2 strongly acid", {
  skip_on_cran()
  p <- ex_s2("dermosols", list(ph_h2o = c(6.2, 5.0, 5.0, 5.2)))
  expect_true(soilKey:::acidic_b2_asc(p)$passed)
  expect_identical(sg_s2(p), "AI")
  # mottled as well
  expect_identical(sg_s2(ex_s2("dermosols", list(ph_h2o = c(6.2, 5.0, 5.0, 5.2), redoximorphic_features_pct = c(0, 25, 25, 5),
                                                 mottle_contrast = c(NA, "distinct", "distinct", NA)))), "AJ")
  # pHCa without pHw
  p <- ex_s2("dermosols", list(ph_h2o = NA_real_, ph_cacl2 = c(5.5, 4.4, 4.4, 4.5)))
  expect_true(soilKey:::acidic_b2_asc(p)$passed)
  expect_false(soilKey:::acidic_b2_asc(make_asc3_example("dermosols"))$passed)
  expect_true(is.na(soilKey:::acidic_b2_asc(ex_s2("dermosols", list(ph_h2o = NA_real_)))$passed))
})

test_that("the calcareous Kandosols take the list without the acidic subgroups", {
  skip_on_cran()
  sg <- load_rules("asc3")$subgroups
  codes <- function(path) vapply(soilKey:::.level_list(sg, path, path[3]), function(t) t$code, "")
  calc <- codes(c("KA", "AA", "CV")); dys <- codes(c("KA", "AA", "AF"))
  acidic <- c("GY", "FV", "GW", "GX", "HO", "AU", "AJ", "AI")
  expect_false(any(acidic %in% calc)); expect_true(all(acidic %in% dys))
  expect_identical(setdiff(dys, calc), acidic)
  expect_identical(length(codes(c("DE", "AA", "AH"))), 33L)
  expect_identical(length(codes(c("FE", "AA", "AH"))), 14L)
})

test_that("Kandosol subgroups: an argic or a bauxitic horizon within the B", {
  skip_on_cran()
  k <- make_asc3_example("kandosols")
  p <- ex_s2("kandosols", extra = row_like(k, 4, designation = "B3 lamellae", top_cm = 130, bottom_cm = 150))
  expect_true(soilKey:::argic_horizon_asc(p, "b")$passed)
  expect_identical(sg_s2(p), "AP")
  p <- ex_s2("kandosols", list(bauxitic_nodules_pct = c(0, 0, 30, 30)))
  expect_true(soilKey:::nodular_horizon_asc(p, "bauxitic", "b")$passed)
  expect_identical(sg_s2(p), "AS")
  # bauxite nodules in the A only: not within the B
  p <- ex_s2("kandosols", list(bauxitic_nodules_pct = c(30, 30, 0, 0)))
  expect_false(soilKey:::nodular_horizon_asc(p, "bauxitic", "b")$passed)
})

test_that("Ferrosol subgroups: Snuffy, and the lower part of the B2 sodic", {
  skip_on_cran()
  p <- ex_s2("ferrosols", list(ped_size_max_mm = c(2, 20, 20, 20), strength_dry = c("weak", NA, NA, NA)))
  expect_true(soilKey:::snuffy_asc(p)$passed)
  expect_identical(sg_s2(p), "EN")
  # the dry strength not recorded leaves it open; coarser peds rule it out
  expect_true(is.na(soilKey:::snuffy_asc(ex_s2("ferrosols", list(ped_size_max_mm = c(2, 20, 20, 20))))$passed))
  expect_false(soilKey:::snuffy_asc(make_asc3_example("ferrosols"))$passed)
  expect_false(soilKey:::snuffy_asc(ex_s2("ferrosols", list(ped_size_max_mm = c(2, 20, 20, 20), strength_dry = "firm")))$passed)
  # the deepest B2 sodic
  p <- ex_s2("ferrosols", list(na_cmol = c(0.2, 0.2, 0.2, 1.5)))
  expect_true(soilKey:::lower_b_sodic_asc(p, "b2")$passed)
  expect_identical(sg_s2(p), "EO")
  # a sodic BC under a B2 that is not: sodic in the lower B, not in the lower B2
  f <- make_asc3_example("ferrosols")
  p <- ex_s2("ferrosols", extra = row_like(f, 4, designation = "BC", top_cm = 150, bottom_cm = 170, na_cmol = 1.5))
  expect_true(soilKey:::lower_b_sodic_asc(p, "b")$passed)
  expect_false(soilKey:::lower_b_sodic_asc(p, "b2")$passed)
})

test_that("the dark horizon of the Chernic suborders is decided over every extent it may have", {
  skip_on_cran()
  mk <- function(top, bot, dv, ph, ps = 10, grade = "moderate") {
    n <- length(top)
    PedonRecord$new(site = list(id = "t", country = "AU", designation_system = "asc", drainage_class = "well drained"),
      horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(
        designation = c(paste0("A1", seq_len(n)), "B21t", "B22t"), top_cm = c(top, max(bot), max(bot) + 30),
        bottom_cm = c(bot, max(bot) + 30, max(bot) + 60), field_texture = c(rep("SL", n), "MC", "MC"),
        clay_pct = c(rep(14, n), 48, 48), boundary_distinctness = c(rep("clear", n - 1), "abrupt", "gradual", NA),
        structure_grade = c(rep(grade, length.out = n), "moderate", "moderate"),
        structure_type = c(rep("granular", n), "angular blocky", "angular blocky"),
        ped_size_max_mm = c(rep(ps, length.out = n), 20, 20), ph_h2o = c(ph, 6.5, 6.5), oc_pct = c(rep(2, n), 0.3, 0.2),
        munsell_hue_moist = "10YR", munsell_value_moist = c(rep(2, n), 4, 4), munsell_chroma_moist = c(rep(1, n), 6, 6),
        munsell_value_dry = c(dv, NA, NA), munsell_chroma_dry = c(rep(1, n), NA, NA), stringsAsFactors = FALSE))))
  }
  # a gap directly above a row that is not black may be black: 0-21 cm may be the horizon
  p <- mk(c(0, 9, 21), c(5, 19, 40), c(4, 4, 6), c(5, 5, 5))
  expect_true(is.na(soilKey:::.asc_chernic_parts(p)$dark))
  # black rows to 31 cm, then one that may be black, acid, with coarse peds: as deep as 46 cm,
  # neither melacic nor melanic
  p <- mk(c(0, 8, 23, 31), c(8, 23, 31, 46), c(4, 4, 4, NA), c(5, 6.5, 6.5, 5), ps = c(10, 10, 10, 30),
          grade = c("weak", "moderate", "moderate", "moderate"))
  cp <- soilKey:::.asc_chernic_parts(p)
  expect_true(is.na(cp$dark)); expect_true(is.na(cp$melanic))
  # with that row recorded not black, melanic
  p <- mk(c(0, 8, 23, 31), c(8, 23, 31, 46), c(4, 4, 4, 6), c(5, 6.5, 6.5, 5), ps = c(10, 10, 10, 30),
          grade = c("weak", "moderate", "moderate", "moderate"))
  expect_true(soilKey:::.asc_chernic_parts(p)$dark)
})

test_that("a gap above the first layer of another kind may be of the same material", {
  skip_on_cran()
  mk <- function(df) PedonRecord$new(site = list(id = "t", country = "AU", designation_system = "asc", drainage_class = "very poorly drained"),
                                     horizons = ensure_horizon_schema(data.table::as.data.table(df)))
  # 18 cm of peat, a gap to 25 cm, then mineral soil: the peat may be 25 cm thick
  p <- mk(data.frame(designation = c("O2", "P1", "A1", "B2"), top_cm = c(0, 10, 25, 40), bottom_cm = c(10, 18, 40, 80),
                     oc_pct = c(30, 25, 3, 1), field_texture = c(NA, NA, "SL", "LC"), stringsAsFactors = FALSE))
  expect_true(is.na(soilKey:::.asc_chernic_parts(p)$peaty))
  p$horizons$top_cm[3] <- 18
  expect_false(soilKey:::.asc_chernic_parts(p)$peaty)
  # 25 cm of burnt peat, a gap to 32 cm: the fusic layers may be 0.3 m thick
  p <- mk(data.frame(designation = c("A1", "B21"), top_cm = c(0, 32), bottom_cm = c(25, 60),
                     layer_origin = c("burnt peat", NA), field_texture = c("SL", "LC"), stringsAsFactors = FALSE))
  expect_true(is.na(soilKey:::anthroposol_kind_asc(p, "fusic")$passed))
  # a gap under the organic rows: the mineral surface lies at either end, and the dark horizon is absent at both
  p <- mk(data.frame(designation = c("O2", "A1", "B2"), top_cm = c(0, 11, 40), bottom_cm = c(10, 40, 80),
                     oc_pct = c(30, 1, 0.3), field_texture = c(NA, "SL", "SCL"), munsell_value_moist = c(2, 5, 5),
                     munsell_chroma_moist = c(1, 3, 4), munsell_value_dry = c(NA, 6, 7), stringsAsFactors = FALSE))
  cp <- soilKey:::.asc_chernic_parts(p)
  expect_false(cp$dark); expect_false(cp$melacic); expect_false(cp$melanic)
})

test_that("a row of unknown role between two A horizons is an A horizon; rows after the last designated one are not B2", {
  skip_on_cran()
  # over a clear textural B, black A11, an undesignated row, A12
  mkd <- function(d2) PedonRecord$new(site = list(id = "t", country = "AU", designation_system = "asc", drainage_class = "well drained"),
    horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(
      designation = c("A11", d2, "A12", "B21t", "B22t"), top_cm = c(0, 8, 14, 25, 55), bottom_cm = c(8, 14, 25, 55, 90),
      field_texture = c("SL", "SL", "SL", "MC", "MC"), clay_pct = c(14, 14, 14, 48, 48),
      boundary_distinctness = c("clear", "clear", "abrupt", "gradual", NA), structure_grade = "moderate",
      structure_type = c("granular", "granular", "granular", "angular blocky", "angular blocky"), ped_size_max_mm = c(10, 10, 10, 20, 20),
      ph_h2o = 6.5, munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 2, 4, 4), munsell_chroma_moist = c(1, 1, 1, 6, 6),
      munsell_value_dry = c(4, 4, 4, NA, NA), munsell_chroma_dry = c(1, 1, 1, NA, NA), stringsAsFactors = FALSE))))
  expect_true(soilKey:::.asc_chernic_parts(mkd("A12"))$dark)
  expect_true(soilKey:::.asc_chernic_parts(mkd(NA_character_))$dark)
  # a sodic B22 over a BC and a row below the description of unknown Na: the lower part of the B2 is sodic
  f <- make_asc3_example("ferrosols")
  d <- as.data.frame(f$horizons); d$na_cmol <- c(0.2, 0.2, 0.2, 1.5)
  d <- rbind(d, d[4, ], d[4, ]); d$designation <- c("A1", "B21", "B22", "B23", "BC", NA)
  d$top_cm <- c(0, 15, 50, 100, 150, 170); d$bottom_cm <- c(15, 50, 100, 150, 170, 190); d$na_cmol[5:6] <- c(0.2, NA)
  p <- PedonRecord$new(site = f$site, horizons = ensure_horizon_schema(data.table::as.data.table(d)))
  expect_true(soilKey:::lower_b_sodic_asc(p, "b2")$passed)
  # with no designated B2 where the order leaves the B2 open (the description ends in a B1), undecided
  d <- as.data.frame(f$horizons)[1:2, ]; d$designation <- c("A1", "B1"); d$na_cmol <- 3
  p <- PedonRecord$new(site = f$site, horizons = ensure_horizon_schema(data.table::as.data.table(d)))
  expect_true(is.na(soilKey:::lower_b_sodic_asc(p, "b2")$passed))
})

test_that("rows of unknown organic status, and A horizons of a lower sequence, in the dark horizon", {
  skip_on_cran()
  mk <- function(des, top, bot, oc, extra = list()) {
    n <- length(des)
    df <- data.frame(designation = des, top_cm = top, bottom_cm = bot, oc_pct = oc,
                     field_texture = ifelse(des %in% c("B21t", "2B2"), "MC", ifelse(is.na(oc), NA, "SL")),
                     clay_pct = ifelse(des %in% c("B21t", "2B2"), 48, ifelse(is.na(oc), NA, 14)),
                     boundary_distinctness = c(rep("clear", n - 1), NA), structure_grade = "moderate", structure_type = "granular",
                     ped_size_max_mm = 10, ph_h2o = 5, munsell_hue_moist = "10YR", munsell_value_moist = 2, munsell_chroma_moist = 1,
                     munsell_value_dry = 4, munsell_chroma_dry = 1, stringsAsFactors = FALSE)
    for (k in names(extra)) df[[k]] <- extra[[k]]
    PedonRecord$new(site = list(id = "t", country = "AU", designation_system = "asc", drainage_class = "well drained"),
                    horizons = ensure_horizon_schema(data.table::as.data.table(df)))
  }
  # a row of unknown organic status within the A horizons may end the horizon
  p <- mk(c("A11", NA, "A13", "B21t"), c(0, 8, 13, 21), c(8, 13, 21, 50), c(2, NA, 2, 0.5),
          list(munsell_value_moist = c(2, 2, 2, 4), munsell_chroma_moist = c(1, 1, 1, 6), munsell_value_dry = c(4, 4, 4, NA),
               boundary_distinctness = c("clear", "clear", "abrupt", NA)))
  expect_true(is.na(soilKey:::.asc_chernic_parts(p)$melacic))
  # an A horizon below the first B does not make a row above it an A horizon
  p <- mk(c("A1", NA, "B21t", "2A1", "2B2"), c(0, 10, 20, 40, 50), c(10, 20, 40, 50, 80), c(2, 2, 0.5, 2, 0.5),
          list(ph_h2o = 6.5, munsell_value_moist = c(2, 2, 4, 2, 4), munsell_chroma_moist = c(1, 1, 6, 1, 6),
               munsell_value_dry = c(4, 4, NA, 4, NA), boundary_distinctness = c("clear", "abrupt", "clear", "clear", NA)))
  expect_true(is.na(soilKey:::.asc_chernic_parts(p)$dark))
})
