# v0.9.283: great groups of the Australian Soil Classification for the
# Kurosols, Sodosols, Chromosols, Ferrosols, Dermosols and Kandosols: pans,
# structure and plasticity of the B2t or B2, base status, exchangeable Ca/Mg,
# ESP and carbonate.

.mk_gg <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

# A Chromosol: sandy loam A over a red clay B2t at an abrupt boundary.
.chrom <- function(h = list(), site = list()) .mk_gg(utils::modifyList(list(
  designation = c("A1", "A2e", "B21t", "B22t", "BC"), top_cm = c(0, 12, 30, 55, 80),
  bottom_cm = c(12, 30, 55, 80, 110), field_texture = c("SL", "SL", "LMC", "MC", "LC"),
  clay_pct = c(14, 12, 42, 48, 38), boundary_distinctness = c("clear", "abrupt", "gradual", "gradual", NA),
  structure_grade = c("weak", "structureless", "strong", "moderate", "weak"),
  structure_type = c("granular", "massive", "prismatic", "blocky", "blocky"),
  ph_h2o = c(6.0, 6.0, 6.5, 6.5, 6.8), cec_cmol = c(8, 5, 20, 22, 18), na_cmol = 0.4,
  ca_cmol = c(4, 2, 10, 10, 10), mg_cmol = c(1, 0.5, 5, 5, 5), k_cmol = 0.5, redoximorphic_features_pct = 0,
  oc_pct = c(1.8, 0.5, 0.4, 0.3, 0.2), munsell_hue_moist = c("10YR", "10YR", "2.5YR", "2.5YR", "5YR"),
  munsell_value_moist = c(3, 6, 4, 4, 5), munsell_chroma_moist = c(2, 3, 6, 6, 6)), h), site)

# the name down to the great group (the subgroups follow since 0.9.286)
nm <- function(p) { lv <- classify_asc3(p, on_missing = "silent")$levels; lv <- lv[lv$level != "subgroups", ]; lv$name[nrow(lv)] }

test_that("the examples key to a decided great group, named with the suborder", {
  skip_on_cran()
  want <- c(kurosols = "Mesotrophic, Red Kurosol", sodosols = "Mesonatric, Red Sodosol",
            chromosols = "Eutrophic, Red Chromosol", ferrosols = "Eutrophic, Red Ferrosol",
            dermosols = "Eutrophic, Brown Dermosol", kandosols = "Mesotrophic, Red Kandosol")
  for (o in names(want)) {
    r <- classify_asc3(make_asc3_example(o), on_missing = "silent")
    # since 0.9.286 the name may go on to the subgroup
    expect_identical(r$levels$name[r$levels$level == "great_groups"], want[[o]], info = o)
    expect_identical(r$levels$level[1:3], c("orders", "suborders", "great_groups"), info = o)
    expect_length(grep("^subgroups:", r$provisional_because, value = TRUE, invert = TRUE), 0L)
  }
  # the list is the one under the order and suborder ("CH-AB")
  p <- .chrom(list(munsell_hue_moist = c("10YR", "10YR", "7.5YR", "7.5YR", "7.5YR")))
  expect_identical(nm(p), "Eutrophic, Brown Chromosol")
})

test_that("base status classes of the major part of the B2t, and Eutrophic needs a non-calcareous B", {
  skip_on_cran()
  expect_identical(nm(.chrom(list(ca_cmol = c(4, 2, 1, 1, 1), mg_cmol = c(1, 0.5, 0.5, 0.5, 0.5),
                                  k_cmol = 0.1, na_cmol = 0.1))), "Dystrophic, Red Chromosol")     # 1.8/42
  expect_identical(nm(.chrom(list(ca_cmol = c(4, 2, 3, 3, 3), mg_cmol = c(1, 0.5, 1.5, 1.5, 1.5)))),
                   "Mesotrophic, Red Chromosol")                                                   # 5.4/42
  # no lab clay: the clay range of the field texture (MC 45-55 %) still decides
  p <- .chrom(list(clay_pct = c(14, 12, NA, NA, NA), field_texture = c("SL", "SL", "MC", "MC", "LC")))
  expect_identical(nm(p), "Eutrophic, Red Chromosol")
  # a calcareous BC: not Eutrophic; on to the carbonate classes
  p <- .chrom(list(effervescence_class = c("none", "none", "none", "slight", "moderate"), ph_h2o = c(6, 6, 6.5, 7.5, 8),
                   secondary_carbonates_pct = 0, carbonate_soft_pct = 0, carbonate_hard_pct = 0))
  expect_false(soilKey:::base_status_asc(p, "eutrophic", "b2t")$passed)
  expect_identical(nm(p), "Hypocalcic, Red Chromosol")
  # no exchangeable bases: undecided
  r <- classify_asc3(.chrom(list(ca_cmol = NA)), on_missing = "silent")
  expect_identical(r$name, "Red Chromosol"); expect_true("great_groups:AF" %in% r$provisional_because)
})

test_that("Magnesic, Natric and the ESP classes of the Sodosols", {
  skip_on_cran()
  expect_identical(nm(.chrom(list(ca_cmol = c(4, 2, 0.4, 0.4, 0.4), mg_cmol = c(1, 0.5, 8, 8, 8)))),
                   "Magnesic, Red Chromosol")
  ku <- make_asc3_example("kurosols")
  ku$horizons$ca_cmol <- c(4, 2, 0.1, 0.1, 0.1); ku$horizons$mg_cmol <- c(1, 0.5, 3, 3, 3)
  expect_identical(nm(ku), "Magnesic, Red Kurosol")
  ku$horizons$na_cmol <- c(0.1, 0.1, 3, 3, 3)                                       # ESP 15: sodic
  expect_identical(nm(ku), "Magnesic-Natric, Red Kurosol")
  so <- make_asc3_example("sodosols")
  so$horizons$na_cmol <- c(0.1, 0.1, 2, 2, 2)                                       # ESP 10
  expect_identical(nm(so), "Subnatric, Red Sodosol")
  so$horizons$na_cmol <- c(0.1, 0.1, 6, 6.6, 6)                                     # ESP 30
  expect_identical(nm(so), "Hypernatric, Red Sodosol")
  so$horizons$redoximorphic_features_pct <- c(0, 0, 25, 25, 0); so$horizons$mottle_contrast <- "distinct"
  expect_identical(nm(so), "Mottled-Hypernatric, Red Sodosol")
  # one major part that is both mottled and in the class
  p <- .chrom(list(designation = c("A1", "A2e", "B21t", "B22t", "B23t", "BC"), top_cm = c(0, 12, 30, 39, 41, 80),
                   bottom_cm = c(12, 30, 39, 41, 80, 110), field_texture = c("SL", "SL", "LMC", "MC", "MC", "LC"),
                   clay_pct = c(14, 12, 42, 48, 48, 38), boundary_distinctness = c("clear", "abrupt", "gradual", "gradual", "gradual", NA),
                   structure_grade = "moderate", structure_type = "blocky", ph_h2o = 8, cec_cmol = 20,
                   na_cmol = c(0.1, 0.1, 0.6, 2, 2, 2), ca_cmol = 6, mg_cmol = 5, k_cmol = 0.5,
                   redoximorphic_features_pct = c(0, 0, 25, 25, 0, 0), mottle_contrast = "distinct",
                   oc_pct = 0.5, munsell_hue_moist = "2.5YR", munsell_value_moist = 4, munsell_chroma_moist = 6))
  expect_false(soilKey:::esp_great_group_asc(p, "subnatric", mottled = TRUE)$passed)
  # every horizon of the solum effervescent
  so <- make_asc3_example("sodosols"); so$horizons$effervescence_class <- "moderate"
  expect_identical(nm(so), "Effervescent, Red Sodosol")
})

test_that("pans in or under the B: Duric, Petroferric, Petrocalcic, Placic", {
  skip_on_cran()
  pg <- function(p, k, w = "b") soilKey:::pan_great_group_asc(p, k, w)$passed
  under <- function(pan, des = "Bm", h = list()) .chrom(utils::modifyList(list(
    designation = c("A1", "A2e", "B21t", "B22t", des), pan_type = c(NA, NA, NA, NA, pan),
    cementation_class = c(NA, NA, NA, NA, "strongly")), h))
  expect_true(pg(under("red-brown hardpan"), "duric"))
  expect_identical(nm(under("red-brown hardpan")), "Duric, Red Chromosol")
  expect_false(pg(under("calcrete"), "duric"))
  expect_true(is.na(pg(under(NA), "duric")))                    # a cemented layer of unknown kind
  expect_false(pg(.chrom(list(pan_type = "none")), "duric"))
  expect_false(pg(.chrom(), "duric"))                           # no pan recorded
  # ferricrete within the solum
  p <- .chrom(list(pan_type = c(NA, NA, NA, "ferricrete", NA), cementation_class = c(NA, NA, NA, "indurated", NA)))
  expect_true(pg(p, "petroferric", "solum"))
  # a calcrete pan directly under a B that is not calcareous
  p <- under("calcrete", "Bkm", list(effervescence_class = c("none", "none", "none", "none", "strong")))
  expect_true(soilKey:::petrocalcic_asc(p)$passed)
  expect_identical(nm(p), "Petrocalcic, Red Chromosol")
  p <- under("calcrete", "Bkm", list(effervescence_class = c("none", "none", "none", "slight", "strong")))
  expect_false(soilKey:::petrocalcic_asc(p)$passed)
  # a B-designated pan with B horizons below it is within the B
  p <- .chrom(list(designation = c("A1", "A2e", "B21t", "B22m", "BC"),
                   pan_type = c(NA, NA, NA, "red-brown hardpan", NA), cementation_class = c(NA, NA, NA, "strongly", NA)))
  expect_identical(nm(p), "Duric, Red Chromosol")
  # a calcrete pan under a B that is not calcareous, a calcareous BCk below it
  p <- .chrom(list(designation = c("A1", "A2e", "B21t", "Bkm", "BCk"), pan_type = c(NA, NA, NA, "calcrete", NA),
                   cementation_class = c(NA, NA, NA, "strongly", NA),
                   effervescence_class = c("none", "none", "none", "strong", "strong")))
  expect_true(soilKey:::petrocalcic_asc(p)$passed)
  # a red-brown hardpan under a calcrete pan under the B is under substrate
  p <- .chrom(list(designation = c("A1", "A2e", "B21t", "Bkm", "C"), pan_type = c(NA, NA, NA, "calcrete", "red-brown hardpan"),
                   cementation_class = c(NA, NA, NA, "strongly", "strongly")))
  expect_false(pg(p, "duric"))
  # a duripan may be a red-brown hardpan; a Bcm is a ferricrete
  expect_true(is.na(pg(under("duripan"), "duric")))
  p <- under(NA, "Bcm")
  expect_true(pg(p, "petroferric", "b")); expect_false(pg(p, "duric"))
  # a thin ironpan (Bsm) in a Kandosol
  ka <- make_asc3_example("kandosols"); ka$horizons$designation <- c("A1", "A3", "B21", "B22sm")
  expect_identical(nm(ka), "Placic, Red Kandosol")
})

test_that("Pedaric and Subplastic", {
  skip_on_cran()
  p <- .chrom(list(structure_grade = c("weak", "structureless", "strong", "strong", "weak"),
                   structure_type = c("granular", "massive", "polyhedral", "subangular blocky", "blocky"),
                   strength_dry = c("weak", "weak", "very weak", "very weak", "firm")))
  expect_true(soilKey:::pedaric_asc(p, "upper_b2t")$passed)
  expect_identical(nm(p), "Pedaric, Red Chromosol")
  p$horizons$strength_dry <- c("weak", "weak", "weak", "weak", "firm")          # weak when dry passes
  expect_true(soilKey:::pedaric_asc(p, "upper_b2t")$passed)
  p$horizons$strength_dry <- "firm"
  expect_false(soilKey:::pedaric_asc(p, "upper_b2t")$passed)
  p$horizons$strength_dry <- "weak"; p$horizons$structure_type <- NA                # strong, of unknown type
  expect_true(is.na(soilKey:::pedaric_asc(p, "upper_b2t")$passed))
  # many undesignated rows far below the upper B2t do not leave it undecided
  q <- .mk_gg(list(designation = c("A1", "A2e", "B21t", "B22t", rep(NA, 9), "C"),
                   top_cm = c(0, 12, 30, 55, seq(80, 160, 10), 170), bottom_cm = c(12, 30, 55, 80, seq(90, 170, 10), 200),
                   field_texture = c("SL", "SL", "LMC", "MC", rep("MC", 9), "LC"), clay_pct = c(14, 12, 42, 48, rep(45, 9), 38),
                   boundary_distinctness = c("clear", "abrupt", rep("gradual", 11), NA),
                   structure_grade = c("weak", "structureless", "moderate", "moderate", rep("weak", 10)),
                   structure_type = c("granular", "massive", rep("blocky", 12)), ph_h2o = 6.5, cec_cmol = 20, na_cmol = 0.4,
                   ca_cmol = 10, mg_cmol = 5, k_cmol = 0.5, oc_pct = 0.5,
                   munsell_hue_moist = "2.5YR", munsell_value_moist = 4, munsell_chroma_moist = 6))
  expect_false(soilKey:::pedaric_asc(q, "upper_b2t")$passed)
  # no known B2 below a B1t textural B: any undesignated row may begin the B2t
  q <- .mk_gg(list(designation = c("A1", "A2e", "B1t", NA, NA, "C"), top_cm = c(0, 12, 30, 40, 50, 60),
                   bottom_cm = c(12, 30, 40, 50, 60, 90), field_texture = c("SL", "SL", "LMC", "MC", "MC", "LC"),
                   clay_pct = c(14, 12, 42, 48, 48, 38), boundary_distinctness = c("clear", "abrupt", rep("gradual", 3), NA),
                   structure_grade = c("weak", "structureless", "strong", "strong", "moderate", "weak"),
                   structure_type = c("granular", "massive", rep("blocky", 4)),
                   strength_dry = c(NA, NA, "very weak", "very weak", "firm", NA), ph_h2o = 6.5, cec_cmol = 20, na_cmol = 0.4,
                   ca_cmol = 10, mg_cmol = 5, k_cmol = 0.5, oc_pct = 0.5,
                   munsell_hue_moist = "2.5YR", munsell_value_moist = 4, munsell_chroma_moist = 6))
  expect_true(is.na(soilKey:::pedaric_asc(q, "upper_b2t")$passed))
  p <- .chrom(list(subplasticity = c(NA, NA, "strongly subplastic", "strongly subplastic", NA)))
  expect_identical(nm(p), "Subplastic, Red Chromosol")
  expect_false(soilKey:::subplastic_asc(.chrom(), "b2t")$passed)    # not recorded: not
})

test_that("Ferrosols and Kandosols: Calcareous, Leptic, Mellic, Andic", {
  skip_on_cran()
  fe <- make_asc3_example("ferrosols")
  fe$horizons$effervescence_class <- c("none", "none", "none", "slight"); fe$horizons$ph_h2o <- c(5.8, 5.6, 5.8, 7.6)
  expect_identical(nm(fe), "Calcareous, Red Ferrosol")
  ka <- make_asc3_example("kandosols")
  expect_false(soilKey:::mellic_asc(ka)$passed)                     # firm when moist
  ka$horizons$strength_moist <- c(NA, NA, "weak", "weak")
  expect_true(is.na(soilKey:::mellic_asc(ka)$passed))               # porosity not recorded
  expect_false(soilKey:::andic_great_group_asc(ka)$passed)
  ka$horizons$layer_origin <- c("basaltic tephra", "basaltic tephra", NA, NA)
  expect_true(is.na(soilKey:::andic_great_group_asc(ka)$passed))
})

test_that("the NatSoil benchmark reports the great groups; NatSoil pans", {
  skip_on_cran()
  a <- make_asc3_example("chromosols"); a$site$id <- "a"
  a$site$reference_asc_order <- "CH"; a$site$reference_asc_suborder <- "AA"; a$site$reference_asc_great_group <- "AH"
  b <- make_asc3_example("chromosols"); b$site$id <- "b"
  b$site$reference_asc_order <- "CH"; b$site$reference_asc_suborder <- "AA"; b$site$reference_asc_great_group <- "AG"
  bm <- benchmark_natsoil(list(a = a, b = b), verbose = FALSE)
  expect_identical(bm$great_groups$n, 2L)
  expect_equal(bm$great_groups$accuracy, 50)
  expect_identical(bm$results$predicted_great_group, c("AH", "AH"))
  skip_if_not_installed("jsonlite")
  site <- list(agency_code = "999", proj_code = "TEST", s_id = "7", usage_license = "https://creativecommons.org/licenses/by/4.0/",
               citation = "Synthetic", z_observations = list(list(o_asc_ord = "CH", o_asc_subord = "AA", o_asc_gg = "BJ",
    z_horizons = list(
      list(h_desig_master = "A", h_desig_subdiv = "1", h_depth_upper = 0, h_depth_lower = 0.2, h_texture = "SL",
           z_pans = list(list(pan_type = "Z"))),
      list(h_desig_master = "B", h_desig_subdiv = "2", h_depth_upper = 0.2, h_depth_lower = 0.5, h_texture = "MC",
           z_pans = list(list(pan_type = "R", pan_cementation = "3")))))))
  dir <- withr::local_tempdir(); f <- file.path(dir, "s.json.gz")
  con <- gzfile(f, "w"); writeLines(jsonlite::toJSON(list(site), auto_unbox = TRUE, null = "null"), con); close(con)
  h <- as.data.frame(load_natsoil_pedons(f, verbose = FALSE)[[1]]$horizons)
  expect_identical(h$pan_type, c("none", "red-brown hardpan"))
})
