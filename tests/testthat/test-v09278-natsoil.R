# v0.9.278: NatSoil (CSIRO) read through its API, as pedons, and the
# benchmark of the Australian order key. Synthetic records written in the
# shape the API returns; no network and no NatSoil data.

.ns_lab <- function(code, value, prefix = " ") list(labm_code = code, labr_value = value, labr_value_prefix = prefix)

.ns_horizon <- function(num, master, subdiv, suffix, top, bottom, texture, qual = NULL, bound = NULL,
                        grade = NULL, type = NULL, labs = list(), extra = list()) {
  c(list(h_desig_num_pref = num, h_desig_master = master, h_desig_subdiv = subdiv, h_desig_suffix = suffix,
         h_depth_upper = top, h_depth_lower = bottom, h_texture = texture, h_texture_qual = qual,
         h_bound_distinct = bound,
         z_structures = if (is.null(grade)) list() else list(list(str_ped_grade = grade, str_ped_type = type)),
         z_colours = list(list(col_moisture_stat = "M", col_hue = "10YR", col_value = 4, col_chroma = 3)),
         z_samples = if (length(labs)) list(list(z_lab_results = labs)) else list()),
    extra)
}

# A duplex profile (a Chromosol), a site with no licence and a site with no
# ASC order.
.ns_sites <- function() {
  duplex <- list(
    agency_code = "999", proj_code = "TEST", s_id = "1",
    usage_license = "https://creativecommons.org/licenses/by/4.0/",
    citation = "Synthetic record for the tests",
    z_observations = list(list(
      o_asc_ord = "CH", o_asc_subord = "AB", o_asc_gg = "AH", o_asc_subg = "BS", o_asc_conf = "1",
      o_drainage = "5", o_latitude_GDA94 = -35.1, o_longitude_GDA94 = 149.2, o_date_desc = "01011990",
      z_surf_conditions = list(list(scon_stat = "H", scon_stat_desc = "Hard setting")),
      z_horizons = list(
        .ns_horizon(NULL, "A", "1", NULL, 0, 0.12, "SL", bound = "C", grade = "W", type = "GR",
                    labs = list(.ns_lab("4A1", 6.0), .ns_lab("P10_NR_C", 12), .ns_lab("6A1_UC", 1.0))),
        .ns_horizon(NULL, "A", "2", "e", 0.12, 0.30, "SL", bound = "A", grade = "V",
                    labs = list(.ns_lab("4A1", 6.2), .ns_lab("P10_NR_C", 10)), extra = list(h_bound_shape = "T")),
        .ns_horizon(NULL, "B", "21", "t", 0.30, 0.55, "LMC", grade = "S", type = "PR", bound = "G",
                    labs = list(.ns_lab("4A1", 6.6), .ns_lab("P10_NR_C", 42), .ns_lab("15_NR_NA", 0.1, "<"),
                                .ns_lab("15_NR_CEC", 20), .ns_lab("19B_NR", 0.1, "<")),
                    extra = list(z_segregations = list(list(seg_nature = "K", seg_abun = NULL),
                                                       list(seg_nature = "F", seg_abun = 2)),
                                 z_cutans = list(list(cutan_type = "C", cutan_abun = 2)))),
        .ns_horizon(NULL, "B", "22", "t", 0.55, 0.80, "MC", grade = "M", type = "AB",
                    labs = list(.ns_lab("4A1", 6.8), .ns_lab("P10_NR_C", 46), .ns_lab("15N1", 3))))
    )))
  unlicensed <- duplex; unlicensed$s_id <- "2"; unlicensed$usage_license <- "Data not licensed for external use."
  unclassified <- duplex; unclassified$s_id <- "3"; unclassified$z_observations[[1]]$o_asc_ord <- NULL
  list(duplex, unlicensed, unclassified)
}

.ns_write <- function(x, file) {
  con <- gzfile(file, "w"); on.exit(close(con))
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), con)
}

test_that("NatSoil records become pedons with the attributes the ASC key reads", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  f <- file.path(dir, "sites.json.gz"); .ns_write(.ns_sites(), f)
  peds <- load_natsoil_pedons(f, verbose = FALSE)
  expect_named(peds, "999_TEST_1")                       # no licence, no order: left out
  p <- peds[[1]]; h <- as.data.frame(p$horizons)
  expect_identical(h$designation, c("A1", "A2e", "B21t", "B22t"))
  expect_identical(h$top_cm, c(0, 12, 30, 55)); expect_identical(h$bottom_cm, c(12, 30, 55, 80))
  expect_identical(h$boundary_distinctness[1:2], c("clear", "abrupt"))
  expect_identical(h$boundary_topography[1:2], c(NA, "tongued"))         # since 0.9.284
  expect_identical(h$structure_grade, c("weak", "structureless", "strong", "moderate"))
  expect_identical(h$structure_type[2], "massive")
  expect_equal(h$oc_pct[1], 1.3)                         # uncorrected Walkley-Black x 1.3
  expect_equal(h$na_cmol[3], 0.05)                       # "<0.1" read as half the limit
  expect_equal(h$caco3_pct[3], 0.05)
  expect_identical(h$secondary_carbonates_pct[3], 0)     # the K record has no abundance: a template
  expect_identical(h$slickensides[3], "absent")          # clay skins, no slickensides
  expect_equal(h$esp_pct[4], 3)
  expect_identical(p$site$reference_asc_order, "CH")
  expect_identical(p$site$drainage_class, "well drained")
  expect_identical(p$site$surface_condition, "hard setting")
  expect_identical(p$site$designation_system, "asc")
  expect_match(p$site$licence, "creativecommons")
  # classified_only = FALSE keeps the site with no order, never the unlicensed one
  expect_named(load_natsoil_pedons(f, classified_only = FALSE, verbose = FALSE),
               c("999_TEST_1", "999_TEST_3"))
})

test_that("the benchmark compares the order and counts decided pedons", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  f <- file.path(dir, "sites.json.gz"); .ns_write(.ns_sites(), f)
  peds <- load_natsoil_pedons(f, verbose = FALSE)
  # a second, surveyed Tenosol that keys as an Arenosol counts for the edition
  sand <- make_asc3_example("arenosols"); sand$site$id <- "sand"; sand$site$reference_asc_order <- "TE"
  b <- benchmark_natsoil(c(peds, list(sand = sand)), verbose = FALSE)
  expect_identical(b$n, 2L)
  expect_identical(b$results$predicted, c("CH", "RE"))
  expect_equal(b$accuracy, 50); expect_equal(b$accuracy_edition, 100)
  expect_identical(b$decided, 2L)
  expect_identical(as.integer(b$confusion["CH", "CH"]), 1L)
})

test_that("an ESP from the laboratory stands in for Na and CEC", {
  skip_on_cran()
  p <- make_asc3_example("sodosols")
  p$horizons$na_cmol <- NA_real_; p$horizons$esp_pct <- c(1, 1, 15, 15, 15)
  expect_true(soilKey:::sodosol_asc(p)$passed)
  p$horizons$esp_pct <- c(1, 1, 2, 2, 2)
  expect_false(soilKey:::sodosol_asc(p)$passed)
})

test_that("fetch_natsoil saves licensed, classified sites and can be resumed", {
  skip_on_cran()
  skip_if_not_installed("httr"); skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  sites <- .ns_sites()
  calls <- 0L
  fake_get <- function(query, ...) {
    calls <<- calls + 1L
    if (!is.null(query$page_num)) {
      page <- if (query$page_num == 1L) lapply(sites, function(s) { s$z_observations[[1]]$z_horizons <- NULL; s }) else list()
      return(as.character(jsonlite::toJSON(page, auto_unbox = TRUE, null = "null")))
    }
    hit <- Filter(function(s) s$s_id == query$s_id, sites)
    as.character(jsonlite::toJSON(hit, auto_unbox = TRUE, null = "null"))
  }
  testthat::local_mocked_bindings(.natsoil_get = fake_get)
  got <- fetch_natsoil(dir, pause = 0, verbose = FALSE)
  expect_identical(got$s_id, "1")                        # licensed and classified only
  expect_true(all(got$fetched))
  expect_identical(calls, 3L)                            # two pages and one profile
  n0 <- calls
  fetch_natsoil(dir, pause = 0, verbose = FALSE)         # nothing new to download
  expect_identical(calls, n0)
  peds <- load_natsoil_pedons(dir, verbose = FALSE)
  expect_named(peds, "999_TEST_1")
})

# ---- review round -----------------------------------------------------------------

.ns_one <- function(horizons, extra_obs = list()) {
  list(agency_code = "999", proj_code = "T", s_id = "9", usage_license = "https://creativecommons.org/licenses/by/4.0/",
       z_observations = list(c(list(o_asc_ord = "KA", z_horizons = horizons), extra_obs)))
}

test_that("samples count for the horizons they overlap, weighed by the overlap", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  smp <- function(top, bot, labs) list(samp_upper_depth = top, samp_lower_depth = bot, z_lab_results = labs)
  hz <- list(
    list(h_desig_master = "A", h_desig_subdiv = "1", h_depth_upper = 0, h_depth_lower = 0.03,
         z_samples = list(smp(0.03, 0.22, list(.ns_lab("4A1", 7.1))))),          # filed here, lies below
    list(h_desig_master = "B", h_desig_subdiv = "21", h_depth_upper = 0.03, h_depth_lower = 0.43,
         z_samples = list(smp(0.03, 0.13, list(.ns_lab("P10_NR_C", 20))),
                          smp(0.13, 0.43, list(.ns_lab("P10_NR_C", 40), .ns_lab("15a1_Na", 0.5))))))
  dir <- withr::local_tempdir(); f <- file.path(dir, "x.json.gz"); .ns_write(list(.ns_one(hz)), f)
  h <- as.data.frame(load_natsoil_pedons(f, verbose = FALSE)[[1]]$horizons)
  expect_true(is.na(h$ph_h2o[1]))                       # the A1 has no sample within it
  expect_equal(h$ph_h2o[2], 7.1)
  expect_equal(h$clay_pct[2], (20 * 10 + 40 * 30) / 40)  # weighed by thickness
  expect_equal(h$na_cmol[2], 0.5)                        # lower-case code
})

test_that("censored values near a key's limit, pH, organic C, colours and abundances", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  hz <- list(
    list(h_desig_master = "A", h_desig_subdiv = "1", h_depth_upper = 0, h_depth_lower = 0.1, h_texture = "SL",
         z_colours = list(list(col_moisture_stat = "D", col_hue = "10YR", col_value = 6, col_chroma = 3)),
         z_phs = list(list(ph_value = 6.5)),
         z_coarse_frags = list(list(cf_abun = 5), list(cf_abun = 4), list(cf_abun = 4)),
         z_samples = list(list(z_lab_results = list(.ns_lab("19A1", 2, "<"), .ns_lab("4B1", 4.2),
                                                    .ns_lab("2_LOI", 30))))),
    list(h_desig_master = "P", h_desig_subdiv = "", h_depth_upper = 0.1, h_depth_lower = 0.3, h_texture = "HP",
         z_coarse_frags = list(list(cf_lith = "QZ")),
         z_segregations = list(list(seg_nature = "K", seg_form = "S")),
         z_cutans = list(list(cutan_type = "K", cutan_abun = 0)),
         z_samples = list(list(z_lab_results = list(.ns_lab("2_LOI", 60))))))
  dir <- withr::local_tempdir(); f <- file.path(dir, "x.json.gz"); .ns_write(list(.ns_one(hz)), f)
  h <- as.data.frame(load_natsoil_pedons(f, verbose = FALSE)[[1]]$horizons)
  expect_true(is.na(h$caco3_pct[1]))                    # "<2" may be above or below 0.5 %
  expect_true(is.na(h$ph_h2o[1])); expect_equal(h$ph_cacl2[1], 4.2)   # no field pH over a lab pH
  expect_true(is.na(h$oc_pct[1])); expect_equal(h$oc_pct[2], 30)      # loss on ignition in peat only
  expect_true(is.na(h$munsell_value_moist[1])); expect_equal(h$munsell_value_dry[1], 6)
  expect_equal(h$coarse_fragments_pct[1], 100)           # 70 + 35 + 35, capped
  expect_true(is.na(h$coarse_fragments_pct[2]))          # described with no abundance
  expect_equal(h$secondary_carbonates_pct[2], 1)          # a form, no abundance: very few
  expect_identical(h$slickensides[2], "absent")
})

test_that("site pages are named by page size and written whole", {
  skip_on_cran()
  skip_if_not_installed("httr"); skip_if_not_installed("jsonlite")
  dir <- withr::local_tempdir()
  sites <- .ns_sites()
  fake_get <- function(query, ...) {
    if (!is.null(query$page_num)) {
      size <- as.integer(query$page_size); from <- (query$page_num - 1L) * size + 1L
      page <- if (from <= length(sites)) sites[from:min(length(sites), from + size - 1L)] else list()
      page <- lapply(page, function(s) { s$z_observations[[1]]$z_horizons <- NULL; s })
      return(as.character(jsonlite::toJSON(page, auto_unbox = TRUE, null = "null")))
    }
    as.character(jsonlite::toJSON(Filter(function(s) s$s_id == query$s_id, sites), auto_unbox = TRUE, null = "null"))
  }
  testthat::local_mocked_bindings(.natsoil_get = fake_get)
  a <- fetch_natsoil(dir, page_size = 1L, max_sites = 0, pause = 0, verbose = FALSE)
  b <- fetch_natsoil(dir, page_size = 2L, pause = 0, verbose = FALSE)
  expect_identical(a$s_id, "1"); expect_identical(b$s_id, "1")
  expect_length(list.files(dir, pattern = "\\.part$", recursive = TRUE), 0L)
  expect_true(all(grepl("^page_s[12]_", list.files(file.path(dir, "sites")))))
  msg <- testthat::capture_messages(load_natsoil_pedons(list.files(file.path(dir, "sites"), full.names = TRUE)))
  expect_true(any(grepl("site page", msg)))
})

test_that("second review: colours with no status, unusable sample depths, slivers, censored fallbacks", {
  skip_on_cran()
  skip_if_not_installed("jsonlite")
  smp <- function(top, bot, labs) list(samp_upper_depth = top, samp_lower_depth = bot, z_lab_results = labs)
  hz <- list(
    list(h_desig_master = "A", h_desig_subdiv = "1", h_depth_upper = 0, h_depth_lower = 0.3,
         z_colours = list(list(col_moisture_stat = NULL, col_hue = "7.5YR", col_value = 3, col_chroma = 2)),
         z_samples = list(smp(0.2, 0.2, list(.ns_lab("4A1", 5.5))),               # a point sample
                          smp(0.05, 0.35, list(.ns_lab("P10_NR_C", 12))))),        # mostly in the A1
    list(h_desig_master = "B", h_desig_subdiv = "2", h_depth_upper = 0.3, h_depth_lower = 0.6,
         z_samples = list(smp(0.3, 0.6, list(.ns_lab("19A1", 2, "<"), .ns_lab("19B1", 3),
                                             .ns_lab("6A1_UC", 10, "<"))))))
  dir <- withr::local_tempdir(); f <- file.path(dir, "x.json.gz"); .ns_write(list(.ns_one(hz)), f)
  h <- as.data.frame(load_natsoil_pedons(f, verbose = FALSE)[[1]]$horizons)
  expect_identical(h$munsell_hue_moist[1], "7.5YR")
  expect_equal(h$ph_h2o[1], 5.5)                         # point sample: its own horizon
  expect_true(is.na(h$clay_pct[2]))                      # a 5 cm sliver of the A1's sample
  expect_equal(h$caco3_pct[2], 3)                        # "<2" is unusable: the next method
  expect_true(is.na(h$oc_pct[2]))                        # "<10" x 1.3 may exceed 12 %
})
