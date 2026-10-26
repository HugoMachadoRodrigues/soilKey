# v0.9.277: the Australian Soil Classification, 3rd ed. (online version with
# errata), keyed to the 15 orders: the Australian horizon notation, the field
# texture grades, the order diagnostics and the key through the generic
# classifier.

.mk_asc <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}

test_that("Australian designations are read by role, in both notations", {
  skip_on_cran()
  d <- soilKey:::.asc_designations(c("A1", "A2e", "B21t", "B22", "BC", "B/C", "2A1b", "Bhs", "P1", "R"),
                                   list(designation_system = "asc"))
  expect_identical(d$role, c("A1", "A2", "B2", "B2", "BC", "B/C", "A1", "B2", "P", "R"))
  expect_identical(d$t[3], TRUE); expect_identical(d$e[2], TRUE)
  expect_identical(d$buried[7], TRUE); expect_identical(d$prefix[7], "2")
  expect_identical(d$h[8] && d$s[8], TRUE)
  # FAO/USDA designations: an E is an A2, a Bt a B2t, Oa an O2
  u <- soilKey:::.asc_designations(c("Oa", "A", "E", "Bt1", "Bt2", "BC", "C"), list(country = "AU"))
  expect_false(u$notation$ok)
  expect_identical(u$role, c("O2", "A1", "A2", "B2", "B2", "BC", "C"))
  expect_identical(u$t[4], TRUE)
  # an Australian site with A1/B21 designations is read in its own notation
  expect_true(soilKey:::.asc_notation(c("A1", "B21", "B22"), list(country = "AU"))$ok)
  expect_false(soilKey:::.asc_notation(c("A", "Bt1", "C"), list())$ok)
})

test_that("field texture grades carry the glossary's clay ranges", {
  tx <- soilKey:::.asc_texture(c("S", "LS", "KSL", "FSCL", "CL", "LMC", "SLC", "MC+", "HC", "WCS", "HP", NA))
  expect_identical(tx$grade, c("S", "LS", "SL", "SCL", "CL", "LMC", "LC", "MC", "HC", "CS", "peat", NA))
  expect_identical(tx$clay_lo[1:6], c(0, 3, 10, 20, 30, 40))
  expect_identical(tx$clay_hi[1:6], c(5, 8, 20, 30, 35, 45))
  expect_identical(c(tx$clay_lo[8], tx$clay_hi[8]), c(50, 55))   # MC+ is the upper half
  expect_identical(tx$sandy[c(1, 3, 10)], c(TRUE, FALSE, TRUE))
  expect_identical(tx$clayey[c(5, 6, 9)], c(FALSE, TRUE, TRUE))
  expect_true(is.na(tx$clayey[11]))
})

test_that("the Chromosol example keys to its order on CRAN too", {
  r <- classify_asc3(make_asc3_example("chromosols"), on_missing = "silent")
  expect_identical(r$rsg_or_order, "Chromosol")
  expect_identical(r$system_id, "asc3")
})

test_that("each example pedon keys to its order, with nothing undecided", {
  skip_on_cran()
  ord <- c(anthroposols = "Anthroposol", organosols = "Organosol", podosols = "Podosol",
           vertosols = "Vertosol", hydrosols = "Hydrosol", kurosols = "Kurosol",
           sodosols = "Sodosol", chromosols = "Chromosol", calcarosols = "Calcarosol",
           ferrosols = "Ferrosol", dermosols = "Dermosol", kandosols = "Kandosol",
           arenosols = "Arenosol", rudosols = "Rudosol", tenosols = "Tenosol")
  for (o in names(ord)) {
    r <- classify_asc3(make_asc3_example(o), on_missing = "silent")
    expect_identical(r$rsg_or_order, ord[[o]], info = o)
    expect_length(grep("^great_groups:", r$provisional_because, value = TRUE, invert = TRUE), 0L)   # great groups may wait (0.9.285)
    expect_identical(r$levels$level[1], "orders")
  }
})

test_that("the registry lists the ASC as an opt-in system", {
  skip_on_cran()
  s <- soilkey_systems()
  expect_true("asc3" %in% s$id)
  expect_false(s$default[s$id == "asc3"])
  expect_identical(soilKey:::.system_resolve("asc"), "asc3")
  res <- classify_all(make_asc3_example("kandosols"), systems = c("wrb", "asc"), on_missing = "silent")
  expect_identical(res$summary$asc3, "Mesotrophic Red Kandosol")
  expect_length(yaml::read_yaml(system.file("rules", "asc3", "readings.yaml",
                                            package = "soilKey"))$readings, 52L)
})

# ---- the clear or abrupt textural B --------------------------------------------

.duplex <- function(tex, clay = NA, bnd = "abrupt", des = c("A1", "B21t"), top2 = 25) {
  .mk_asc(list(designation = des, top_cm = c(0, top2), bottom_cm = c(top2, 70),
               field_texture = tex, clay_pct = clay, boundary_distinctness = c(bnd, NA)))
}

test_that("the textural B reads lab clay where no field texture is given", {
  skip_on_cran()
  tb <- function(p) soilKey:::textural_b_asc(p)$passed
  expect_true(tb(.duplex(c(NA, NA), clay = c(15, 32))))            # 15 -> 32: double and >= 20
  expect_false(tb(.duplex(c(NA, NA), clay = c(15, 28))))           # not double
  expect_true(tb(.duplex(c(NA, NA), clay = c(25, 46))))            # +21 points
  expect_false(tb(.duplex(c(NA, NA), clay = c(36, 60))))           # no contrast within clays
  # field-texture equivalents
  expect_true(tb(.duplex(c("LS", "SCL"))))                           # 5.5 -> 25
  expect_false(tb(.duplex(c("SL", "SCL"))))                          # 15 -> 25, not double
  expect_true(tb(.duplex(c("L", "LMC"))))
  expect_false(tb(.duplex(c("L", "CL"))))
  expect_true(is.na(tb(.duplex(c("L", "C")))))                        # a plain clay may be lighter
  # the boundary and the horizon above
  expect_false(tb(.duplex(c("LS", "MC"), bnd = "gradual")))
  expect_true(is.na(tb(.duplex(c("LS", "MC"), bnd = NA))))
  expect_false(tb(.duplex(c("LS", "MC"), top2 = 3)))                 # 30 mm or less above
  # any B under the boundary counts (a shallow B3 over rock)
  expect_true(tb(.duplex(c("CS", "LMC"), des = c("A1", "B3"))))
})

test_that("the B2t chemistry decides among Kurosols, Sodosols and Chromosols", {
  skip_on_cran()
  # low charge: Na and CEC too low to compute an ESP are read as not sodic
  p <- make_asc3_example("sodosols")
  p$horizons$cec_cmol[3:5] <- 2.5; p$horizons$na_cmol[3:5] <- 0.2
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Chromosol")
  # a strongly subplastic sodic B2t is not a Sodosol
  p <- make_asc3_example("sodosols")
  p$horizons$subplasticity[3:4] <- "strongly subplastic"
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Chromosol")
  # ESP over the effective CEC where the pHw is 5.5 or less
  p <- make_asc3_example("chromosols")
  p$horizons$ph_h2o[3:5] <- 5.5; p$horizons$ph_cacl2[3:5] <- 5.0
  p$horizons$ecec_cmol[3:5] <- 5; p$horizons$na_cmol[3:5] <- 0.4
  expect_true(isTRUE(soilKey:::sodosol_asc(soilKey:::.cssc_prepare_pedon(p))$passed))
})

# ---- Vertosols, Hydrosols, Calcarosols --------------------------------------------

test_that("Vertosols read cracks from the horizons or a cracking surface", {
  skip_on_cran()
  p <- make_asc3_example("vertosols")
  p$horizons$cracks_width_cm <- NA_real_; p$site$surface_condition <- NA_character_
  r <- classify_asc3(p, on_missing = "silent")
  expect_true("orders:VE" %in% r$provisional_because)
  # a solum described dry with no crack record leaves it undecided; described
  # moist ('unless too moist') the cracks are not required
  p$horizons$soil_water_status <- c("dry", "dry", "dry", "moist")      # 100 of 150 cm dry
  expect_true(is.na(soilKey:::vertosol_asc(p)$evidence$cracks))
  p$horizons$soil_water_status <- c("dry", "dry", "moderately moist", "moist")  # 100 of 150 cm moist
  expect_true(soilKey:::vertosol_asc(p)$evidence$cracks)
  p$horizons$soil_water_status <- c("moist", "moist", "wet", "wet")
  v <- soilKey:::vertosol_asc(p)
  expect_true(v$passed); expect_identical(v$evidence$cracks_route, "described moist (waived)")
  p$horizons$soil_water_status <- NA_character_
  p$site$surface_condition <- "self-mulching; cracking"
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Vertosol")
  expect_identical(soilKey:::vertosol_asc(p)$evidence$cracks_route, "cracking surface")
  # slickensides recorded absent everywhere and no lenticular peds rule it out
  p$horizons$slickensides <- "absent"; p$horizons$structure_type <- "blocky"
  expect_false(isTRUE(soilKey:::vertosol_asc(p)$passed))
  expect_false(is.na(soilKey:::vertosol_asc(p)$passed))
})

test_that("Hydrosols come from saturation days, then the drainage class", {
  skip_on_cran()
  hy <- function(p) soilKey:::hydrosol_asc(p)$passed
  p <- make_asc3_example("hydrosols")
  expect_true(hy(p))                                         # 120-200 days
  p$horizons$water_saturation_days <- 30
  expect_false(hy(p))                                        # days beat the drainage class
  p$horizons$water_saturation_days <- NA_real_
  expect_true(is.na(hy(p)))                                  # poorly drained: wet, not yet saturated
  p$site$drainage_class <- "very poorly drained"
  expect_true(hy(p))
  p$site$drainage_class <- "imperfectly drained"                 # wet for weeks, not months (0.9.280)
  expect_false(hy(p))
  p$site$drainage_class <- "moderately well drained"
  expect_false(hy(p))
  p$site$drainage_class <- "rapidly drained"
  expect_false(hy(p))
  p$site$tidal_inundation <- TRUE
  expect_true(hy(p))
  # a poorly drained soil keys past the Hydrosols, provisionally
  p <- make_asc3_example("hydrosols")
  p$horizons$water_saturation_days <- NA_real_
  r <- classify_asc3(p, on_missing = "silent")
  expect_true("orders:HY" %in% r$provisional_because)
})

test_that("calcareous: no carbonate record and an acid pH read as not calcareous", {
  skip_on_cran()
  ca <- function(p) soilKey:::calcarosol_asc(p)$passed
  p <- make_asc3_example("calcarosols")
  expect_true(ca(p))
  # carbonate from shells or limestone is not pedogenic
  p$horizons$layer_origin <- "shell grit"
  expect_false(ca(p))
  # no record at all: acid horizons are not calcareous, neutral ones undecided
  p <- make_asc3_example("calcarosols")
  p$horizons$effervescence_class <- NA_character_; p$horizons$secondary_carbonates_pct <- NA_real_
  p$horizons$caco3_pct <- NA_real_; p$horizons$designation <- c("A1", "B21", "B22", "BC", "C")
  p$horizons$ph_h2o <- c(6.0, 6.2, 6.4, 6.5, 6.6)
  expect_false(ca(p))
  p$horizons$ph_h2o <- c(7.5, 7.8, 8.0, 8.2, 8.4)
  expect_true(is.na(ca(p)))
})

# ---- Podosols, the B2, Ferrosols ------------------------------------------------

test_that("only an illuvial B of the present soil makes a Podosol", {
  skip_on_cran()
  po <- function(des) soilKey:::podosol_asc(.mk_asc(list(designation = des, top_cm = c(0, 20, 50),
                                                          bottom_cm = c(20, 50, 90))))$passed
  expect_true(po(c("A1", "A2e", "B2hs")))
  expect_true(po(c("A1", "A2", "Bs")))
  expect_false(po(c("A1", "B21ts", "B22t")))    # a textural B with sesquioxides
  expect_false(po(c("A1", "B2", "BCs")))        # a transition
  expect_false(po(c("A1", "A2", "B1sb")))       # buried
  expect_false(po(c("A1", "A2", "B2sm")))       # a thin ironpan
})

test_that("a description that stops in an A or a B1 leaves the B2 open", {
  skip_on_cran()
  mk <- function(des) .mk_asc(list(designation = des, top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 70),
                                   field_texture = c("SL", "SL", "SCL"),
                                   structure_grade = c("weak", "structureless", "structureless")))
  expect_true(is.na(soilKey:::kandosol_asc(mk(c("A1", "A2", "B1")))$passed))
  expect_true(is.na(soilKey:::dermosol_asc(mk(c("A11", "A12", "A3")))$passed))
  expect_false(soilKey:::kandosol_asc(mk(c("A1", "B3", "C")))$passed)
  expect_true(soilKey:::kandosol_asc(mk(c("A1", "A3", "B2")))$passed)
})

test_that("Ferrosols need free iron, and a vertic B2 keeps a soil out", {
  skip_on_cran()
  p <- make_asc3_example("ferrosols")
  p$horizons$fe_dcb_pct <- NA_real_
  r <- classify_asc3(p, on_missing = "silent")
  expect_identical(r$rsg_or_order, "Dermosol")
  expect_true("orders:FE" %in% r$provisional_because)
  p <- make_asc3_example("ferrosols")
  p$horizons$slickensides <- c("absent", "common", "common", "few")
  expect_false(soilKey:::ferrosol_asc(p)$passed)
})

# ---- Arenosols, Rudosols, Tenosols ----------------------------------------------

test_that("deep sands, rudimentary soils and the rest are told apart", {
  skip_on_cran()
  # an A2 is a pedological colour change: not a Rudosol
  p <- make_asc3_example("rudosols")
  p$horizons$designation <- c("A1", "A2", "R")
  expect_false(soilKey:::rudosol_asc(p)$passed)
  # a layer above 15 % clay within 1 m is not an Arenosol
  p <- make_asc3_example("arenosols")
  p$horizons$field_texture[3] <- "SCL"; p$horizons$clay_pct[3] <- 22
  expect_false(soilKey:::arenosol_asc(p)$passed)
  # a sand description that stops at 0.6 m cannot rule out a hard layer
  p <- make_asc3_example("arenosols")
  p$horizons$bottom_cm[4] <- 60; p$horizons$top_cm[4] <- 50; p$horizons$bottom_cm[3] <- 50
  expect_true(is.na(soilKey:::arenosol_asc(p)$evidence$no_hard_layer))
})

test_that("a profile in the FAO/USDA notation keys from the roles of its horizons", {
  skip_on_cran()
  p <- make_asc3_example("chromosols")
  p$site$designation_system <- NULL
  p$horizons$designation <- c("A", "E", "Bt1", "Bt2", "BC")
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Chromosol")
})

# ---- cases from the review (each with the text's answer) -------------------------

test_that("older and capitalised Australian designations are read", {
  d <- soilKey:::.asc_designations(c("A1A2", "A1/A2", "B3C", "A2B1", "B22/C", "3D?", "CK", "B2WT", "BC", "k"),
                                   list(designation_system = "asc"))
  expect_identical(d$role, c("A2", "A2", "BC", "AB", "B/C", "D", "C", "B2", "BC", "other"))
  expect_identical(d$k[7], TRUE); expect_identical(d$t[8], TRUE)
  expect_identical(d$known[10], FALSE)
  # slickensides (ss) and an Oe are not a Bs and a bleached horizon
  u <- soilKey:::.asc_designations(c("Oe", "Ap", "Bss1", "Bhs"), list(country = "US"))
  expect_identical(u$s, c(FALSE, FALSE, FALSE, TRUE)); expect_false(any(u$e))
})

test_that("a Vertisol in USDA notation is a Vertosol, not a Podosol", {
  skip_on_cran()
  p <- make_asc3_example("vertosols")
  p$site$designation_system <- NULL; p$site$country <- "US"
  p$horizons$designation <- c("Ap", "Bss1", "Bss2", "BCk")
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Vertosol")
})

test_that("rock within 1 m rules out an Arenosol", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "C1", "C2", "R"), top_cm = c(0, 10, 40, 90),
                    bottom_cm = c(10, 40, 90, 120), field_texture = c("S", "S", "S", NA)))
  expect_false(soilKey:::arenosol_asc(p)$evidence$no_hard_layer)
})

test_that("unreadable designations leave the gates undecided", {
  skip_on_cran()
  p <- make_asc3_example("chromosols")
  p$horizons$designation <- tolower(p$horizons$designation)
  r <- classify_asc3(p, on_missing = "silent")
  expect_false(identical(r$rsg_or_order, "Rudosol") && length(r$provisional_because) == 0L)
  expect_true(is.na(soilKey:::rudosol_asc(p)$passed))
  expect_true(is.na(soilKey:::podosol_asc(p)$passed))
})

test_that("the Anthroposol origin is read by whole words, and 0.3 m only over a buried soil", {
  skip_on_cran()
  for (o in c("mineral", "transported alluvium", "infilled channel", "valley fill")) {
    p <- make_asc3_example("chromosols"); p$horizons$layer_origin <- o
    expect_false(soilKey:::anthroposol_asc(p)$passed, info = o)
  }
  # 20 cm of spoil on rock buries no soil
  p <- .mk_asc(list(designation = c("C", "R"), top_cm = c(0, 20), bottom_cm = c(20, 50),
                    layer_origin = c("spoil", NA), artefacts_pct = c(10, 0)))
  expect_true(soilKey:::anthroposol_asc(p)$passed)
  # 20 cm of spoil over a soil is a phase of that soil
  p <- .mk_asc(list(designation = c("C", "2A1b", "2B2b"), top_cm = c(0, 20, 35), bottom_cm = c(20, 35, 80),
                    layer_origin = c("spoil", NA, NA), artefacts_pct = c(10, 0, 0)))
  expect_false(soilKey:::anthroposol_asc(p)$passed)
})

test_that("a buried B is not the textural B, and a thin cover is flagged", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "C", "2A1b", "2B21tb"), top_cm = c(0, 10, 60, 75),
                    bottom_cm = c(10, 60, 75, 100), field_texture = c("S", "S", "SL", "MC"),
                    boundary_distinctness = c("clear", "abrupt", "abrupt", NA)))
  expect_false(isTRUE(soilKey:::textural_b_asc(p)$passed))
  # a cover under 0.3 m that is only a Rudosol is flagged; one that keys to
  # another order is classified as such
  p$horizons$top_cm <- c(0, 10, 20, 35); p$horizons$bottom_cm <- c(10, 20, 35, 100)
  p$horizons$structure_grade <- c("structureless", "structureless", "weak", "strong")
  r <- classify_asc3(p, on_missing = "silent")
  expect_identical(r$rsg_or_order, "Rudosol")
  expect_true(any(grepl("buried soil", r$warnings)))
  p$horizons$designation[2] <- "A2"
  r <- classify_asc3(p, on_missing = "silent")
  expect_false(any(grepl("buried soil", r$warnings)))
})

test_that("the upper 0.2 m is that of the B2t, not of a B1 or BC", {
  skip_on_cran()
  # a 10 cm strongly acid B2t over a BC that is not: a Kurosol
  p <- .mk_asc(list(designation = c("A1", "A2", "B2t", "BC"), top_cm = c(0, 15, 30, 40),
                    bottom_cm = c(15, 30, 40, 80), field_texture = c("SL", "SL", "MC", "LC"),
                    boundary_distinctness = c("clear", "abrupt", "clear", NA),
                    ph_h2o = c(6, 6, 5.0, 6.5)))
  expect_true(soilKey:::kurosol_asc(p)$passed)
  # the textural boundary at a B1: the window is still the B2t below it
  p <- .mk_asc(list(designation = c("A1", "B1", "B21t", "B22t"), top_cm = c(0, 20, 30, 60),
                    bottom_cm = c(20, 30, 60, 90), field_texture = c("LS", "LMC", "MC", "MC"),
                    boundary_distinctness = c("abrupt", "clear", "gradual", NA),
                    ph_h2o = c(6, 6.5, 5.0, 5.2)))
  k <- soilKey:::kurosol_asc(p)
  expect_true(k$passed); expect_identical(k$evidence$zone_cm, c(30, 50))
})

test_that("SL+ exceeds 15 % clay and SL does not, as the key equates them", {
  skip_on_cran()
  mk <- function(tx) .mk_asc(list(designation = c("A1", "B21", "B22", "C"), top_cm = c(0, 15, 40, 80),
                                  bottom_cm = c(15, 40, 80, 120), field_texture = c("LS", tx, tx, "LS"),
                                  structure_grade = c("weak", "structureless", "structureless", NA)))
  expect_true(soilKey:::kandosol_asc(mk("SL+"))$passed)
  expect_false(soilKey:::kandosol_asc(mk("SL"))$passed)
  p <- make_asc3_example("arenosols")
  p$horizons$field_texture[3] <- "SL+"; p$horizons$clay_pct[3] <- NA
  expect_false(soilKey:::arenosol_asc(p)$passed)
})

test_that("missing records leave the textural B open", {
  skip_on_cran()
  tb <- function(p) soilKey:::textural_b_asc(p)$passed
  p <- make_asc3_example("chromosols"); p$horizons$top_cm[3] <- 35            # a 5 cm gap
  expect_true(is.na(tb(p)))
  p <- make_asc3_example("chromosols"); p$horizons$bottom_cm[2] <- 40        # an overlap
  expect_true(is.na(tb(p)))
  p <- make_asc3_example("chromosols"); p <- .mk_asc(as.list(as.data.frame(p$horizons)[1:2, ]))
  expect_true(is.na(tb(p)))                                                  # stops in the A2e
})

test_that("Organosols: unknown layers and unseen depth leave the order open", {
  skip_on_cran()
  org <- function(p) soilKey:::organosol_asc(p)$passed
  # 25 cm of undecided organic C on rock
  p <- .mk_asc(list(top_cm = c(0, 25), bottom_cm = c(25, 50), oc_pct = c(15, NA), clay_pct = c(10, NA),
                    designation = NA_character_), site = list(designation_system = NULL, drainage_class = NULL))
  p$horizons$designation <- c(NA, NA)
  expect_true(is.na(org(p)))
  # peat to 30 cm over 35 cm of mineral soil, described to 65 cm
  p <- .mk_asc(list(designation = c("P1", "C"), top_cm = c(0, 30), bottom_cm = c(30, 65),
                    field_texture = c(NA, "LC")))
  expect_true(is.na(org(p)))
  # a measured, undecided organic C is not settled by a designation
  x <- soilKey:::.asc_horizons(.mk_asc(list(designation = "A1", top_cm = 0, bottom_cm = 25, oc_pct = 15,
                                            clay_pct = 10), site = list(drainage_class = NULL)))
  expect_true(is.na(soilKey:::.asc_organic_material(x, .mk_asc(list(designation = "A1", top_cm = 0,
    bottom_cm = 25, oc_pct = 15, clay_pct = 10), site = list(drainage_class = NULL)))))
})

test_that("Rudosols, Vertosols and Sodosols: the remaining review cases", {
  skip_on_cran()
  # a strongly structured buried A1 does not stop a Rudosol
  p <- .mk_asc(list(designation = c("A1", "C", "2A1b"), top_cm = c(0, 10, 50), bottom_cm = c(10, 50, 80),
                    structure_grade = c("weak", "structureless", "strong")))
  expect_true(soilKey:::rudosol_asc(p)$passed)
  # an undesignated, strongly structured clay under the A1 may be a B
  p <- .mk_asc(list(designation = c("A1", NA), top_cm = c(0, 10), bottom_cm = c(10, 60),
                    field_texture = c("SL", "LC"), structure_grade = c("weak", "strong")))
  expect_true(is.na(soilKey:::rudosol_asc(p)$passed))
  # undesignated sandy subsoil keeps a clay A1 from passing as a clayey solum
  p <- .mk_asc(list(designation = c("A1", NA, NA), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
                    field_texture = c("MC", "SL", "LS"), slickensides = c("common", NA, NA)))
  expect_false(isTRUE(soilKey:::vertosol_asc(p)$evidence$clayey_solum))
  # a peaty surface is not tested for clay
  p <- make_asc3_example("vertosols")
  p <- .mk_asc(list(designation = c("P", "A1", "B21", "B22"), top_cm = c(0, 10, 20, 60),
                    bottom_cm = c(10, 20, 60, 120), field_texture = c(NA, "MC", "HC", "HC"),
                    slickensides = c(NA, "absent", "common", "many"), cracks_width_cm = c(NA, 1, 1, NA)))
  expect_true(soilKey:::vertosol_asc(p)$evidence$clayey_solum)
  # sodic and not strongly subplastic in one and the same major part
  p <- make_asc3_example("sodosols")
  p$horizons$top_cm <- c(0, 12, 30, 36, 80); p$horizons$bottom_cm <- c(12, 30, 36, 80, 110)
  p$horizons$na_cmol <- c(0.1, 0.1, 4, 4, 4)
  p$horizons$subplasticity <- c(NA, NA, NA, "strongly subplastic", NA)
  expect_false(soilKey:::sodosol_asc(p)$passed)
})

# ---- second review round ----------------------------------------------------------

test_that("a boundary inside the B is not the textural B", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "B21", "B22", "B23"), top_cm = c(0, 15, 40, 80),
                    bottom_cm = c(15, 40, 80, 130), field_texture = c("L", "CL", "MC", "MC"),
                    clay_pct = c(22, 32, 55, 56), boundary_distinctness = c("gradual", "clear", "gradual", NA),
                    structure_grade = c("weak", "moderate", "strong", "strong"), ph_h2o = 6.5, fe_dcb_pct = 2))
  expect_false(soilKey:::textural_b_asc(p)$passed)
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Dermosol")
})

test_that("a row of unknown role inside the B leaves the B2 tests open", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", NA, "B22", "BC"), top_cm = c(0, 15, 50, 70),
                    bottom_cm = c(15, 50, 70, 110), field_texture = c("SL", "SCL", "SCL", "SL"),
                    boundary_distinctness = c("gradual", "gradual", "gradual", NA),
                    structure_grade = c("weak", "moderate", "weak", "weak"), ph_h2o = 6.5, fe_dcb_pct = 2))
  expect_true(is.na(soilKey:::dermosol_asc(p)$passed))
  p$horizons$designation[2] <- "B21"
  expect_true(soilKey:::dermosol_asc(p)$passed)
  # in the upper 0.2 m of the B2t
  p <- .mk_asc(list(designation = c("A1", "A2e", "B21t", NA, "B22t", "BC"), top_cm = c(0, 12, 30, 36, 50, 80),
                    bottom_cm = c(12, 30, 36, 50, 80, 110), field_texture = c("SL", "SL", "LMC", "LMC", "MC", "LC"),
                    clay_pct = c(14, 12, 42, 42, 48, 38), ph_h2o = c(6, 6, 5.0, 6.5, 6.5, 6.8),
                    boundary_distinctness = c("clear", "abrupt", "gradual", "gradual", "gradual", NA)))
  expect_true(is.na(soilKey:::kurosol_asc(p)$passed))
})

test_that("thin anthropic material over a soil of unknown roles is undecided; litter is skipped", {
  skip_on_cran()
  p <- .mk_asc(list(top_cm = c(0, 5, 30), bottom_cm = c(5, 30, 80), artefacts_pct = c(5, 0, 0),
                    layer_origin = c("fill material", NA, NA)), site = list(designation_system = NULL))
  expect_true(is.na(soilKey:::anthroposol_asc(p)$passed))
  p <- .mk_asc(list(designation = c("O1", "A1", "C", "R"), top_cm = c(0, 2, 10, 60), bottom_cm = c(2, 10, 60, 80),
                    layer_origin = c(NA, "mine spoil", "mine spoil", NA), artefacts_pct = c(0, 5, 10, 0)))
  expect_true(soilKey:::anthroposol_asc(p)$passed)
})

test_that("an undesignated row between solum horizons is solum, elsewhere it keeps the test open", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", NA, "B22k", "BCk"), top_cm = c(0, 12, 40, 70),
                    bottom_cm = c(12, 40, 70, 110), field_texture = c("SL", "SCL", "SCL", "SL"),
                    effervescence_class = c("slight", "none", "strong", "very strong"), caco3_pct = c(2, 0, 25, 20),
                    ph_h2o = c(8.2, 6.8, 8.8, 9)))
  expect_false(soilKey:::calcarosol_asc(p)$passed)      # between the A1 and the B22k: solum
  p$horizons$designation <- c("A1", "B21k", "B22k", NA)
  p$horizons$effervescence_class <- c("slight", "strong", "strong", "none")
  p$horizons$caco3_pct <- c(2, 20, 25, 0); p$horizons$ph_h2o <- c(8.2, 8.8, 8.8, 6.8)
  expect_true(soilKey:::calcarosol_asc(p)$passed)       # shallow test decides
})

test_that("capitals with a P, D or R suffix and the CB transition are read", {
  d <- soilKey:::.asc_designations(c("AP", "A1P", "2B2B", "B2D", "CB", "A0"), list(designation_system = "asc"))
  expect_identical(d$role, c("A1", "A1", "B2", "B2", "CB", "other"))
  expect_identical(d$p[1:2], c(TRUE, TRUE)); expect_true(d$buried[3])
})

test_that("rock ends the profile: no unseen organic or sandy depth below it", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "R"), top_cm = c(0, 15), bottom_cm = c(15, 25),
                    field_texture = c("LS", NA), oc_pct = c(2, NA), structure_grade = c("weak", NA), ph_h2o = c(6.5, NA)))
  expect_false(soilKey:::organosol_asc(p)$passed)
  p <- .mk_asc(list(designation = c("A1", "C", "R"), top_cm = c(0, 10, 25), bottom_cm = c(10, 25, 40),
                    field_texture = c("S", "S", NA), effervescence_class = c("strong", "strong", NA),
                    oc_pct = c(1, 0.3, NA), structure_grade = c("weak", "structureless", NA), ph_h2o = c(8.5, 8.8, NA)))
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Calcarosol")
})

test_that("clay lamellae keep a deep sand an Arenosol, and a cemented pan is a hard layer", {
  skip_on_cran()
  for (lam in c("B2t lamellae", "lamellae")) {
    p <- .mk_asc(list(designation = c("A1", "A2", lam, "C2"), top_cm = c(0, 10, 40, 41), bottom_cm = c(10, 40, 41, 150),
                      field_texture = c("S", "S", "SCL", "S"), structure_grade = "structureless",
                      structure_type = "single-grain", boundary_distinctness = c("gradual", "gradual", "abrupt", NA)))
    expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Arenosol", info = lam)
  }
  p <- .mk_asc(list(designation = c("A1", "C1", "C2", "2Cm"), top_cm = c(0, 10, 40, 85),
                    bottom_cm = c(10, 40, 85, 120), field_texture = "S", structure_grade = "structureless"))
  expect_false(soilKey:::arenosol_asc(p)$passed)
})

test_that("a buried soil below ends the present soil; negative cracking words are not cracks", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "A2", "2A1b", "2B2tb"), top_cm = c(0, 15, 60, 75),
                    bottom_cm = c(15, 60, 75, 120), field_texture = c("LS", "S", "SL", "MC"),
                    boundary_distinctness = c("gradual", "abrupt", "abrupt", NA)))
  expect_identical(soilKey:::.asc_b2_rows(soilKey:::.asc_horizons(p)), integer(0))
  expect_false(soilKey:::textural_b_asc(p)$passed)
  for (w in c("uncracked", "cracks absent", "no cracks", "non-cracking")) {
    p <- make_asc3_example("vertosols"); p$horizons$cracks_width_cm <- NA_real_
    p$site$surface_condition <- w
    expect_true(is.na(soilKey:::vertosol_asc(p)$evidence$cracks), info = w)
  }
})

test_that("a missing and an unreadable designation are read alike", {
  skip_on_cran()
  res <- lapply(list(NA_character_, "?"), function(d2) {
    p <- .mk_asc(list(designation = c("A1", d2, "C"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 120),
                      field_texture = c("CL", "LC", "SCL"), structure_grade = c("weak", "strong", "weak")))
    c(soilKey:::podosol_asc(p)$passed, soilKey:::dermosol_asc(p)$passed, soilKey:::rudosol_asc(p)$passed)
  })
  expect_identical(res[[1]], res[[2]])
  expect_true(all(is.na(res[[1]])))
})

# ---- third review round -----------------------------------------------------------

test_that("the b of older codes (A2sb, A2cb) is not buried above an unburied B", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "A2sb", "B21", "B22"), top_cm = c(0, 10, 30, 60),
                    bottom_cm = c(10, 30, 60, 100), field_texture = c("SL", "SL", "LMC", "MC"),
                    clay_pct = c(12, 10, 40, 45), boundary_distinctness = c("clear", "abrupt", "gradual", NA)))
  x <- soilKey:::.asc_horizons(p)
  expect_false(any(x$des$buried))
  expect_true(soilKey:::textural_b_asc(p)$passed)
  expect_false(any(grepl("buried soil", classify_asc3(p, on_missing = "silent")$warnings)))
})

test_that("empty rows below the description do not open the textural B or the B2", {
  skip_on_cran()
  p <- make_asc3_example("dermosols")
  h <- as.data.frame(p$horizons)[c(1:4, 4, 4), ]
  h$designation <- c("A1", "B21", "B22", "BC", NA, NA); h$top_cm <- c(0, 15, 50, 90, 130, 150)
  h$bottom_cm <- c(15, 50, 90, 130, 150, 170); h[5:6, c("field_texture", "clay_pct", "structure_grade")] <- NA
  p <- .mk_asc(as.list(h))
  expect_false(soilKey:::textural_b_asc(p)$passed)
  r <- classify_asc3(p, on_missing = "silent")
  expect_identical(r$rsg_or_order, "Dermosol"); expect_length(r$provisional_because, 0L)
})

test_that("rows of unknown role decide a major part only when every reading agrees", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "B21", NA, NA, "C"), top_cm = c(0, 10, 40, 60, 100),
                    bottom_cm = c(10, 40, 60, 100, 130), field_texture = c("L", "CL", "CL", "CL", "SL"),
                    structure_grade = c("weak", "strong", "weak", "massive", NA)))
  # B21 (30 cm, strong) with 60 cm of possibly-B2 weak rows: undecided
  expect_true(is.na(soilKey:::dermosol_asc(p)$passed))
  p$horizons$structure_grade[3:4] <- c("strong", "moderate")
  expect_true(soilKey:::dermosol_asc(p)$passed)
})

test_that("rows of unknown role outside the present soil are not read", {
  skip_on_cran()
  # a buried soil with an undesignated row ends the present soil
  p <- .mk_asc(list(designation = c("A1", "A2", "2A1b", NA, "2B2tb"), top_cm = c(0, 15, 60, 75, 90),
                    bottom_cm = c(15, 60, 75, 90, 120), field_texture = c("LS", "S", "SL", "SCL", "MC")))
  expect_identical(soilKey:::.asc_b2_rows(soilKey:::.asc_horizons(p)), integer(0))
  expect_false(soilKey:::podosol_asc(p)$passed)
  # an old A0 code above the A1 is not a possible Podosol B or a B
  p <- make_asc3_example("chromosols")
  p <- .mk_asc(list(designation = c("A0", "A1", "C", "R"), top_cm = c(0, 3, 15, 50), bottom_cm = c(3, 15, 50, 70),
                    field_texture = c(NA, "LS", "LS", NA), structure_grade = c(NA, "weak", "structureless", NA)))
  expect_false(soilKey:::podosol_asc(p)$passed)
  expect_true(soilKey:::rudosol_asc(p)$passed)
})

test_that("an AE in the FAO notation is read as an A2", {
  skip_on_cran()
  p <- make_asc3_example("chromosols")
  p$site$designation_system <- NULL
  p$horizons$designation <- c("A", "AE", "Bt1", "Bt2", "C")
  expect_identical(soilKey:::.asc_designations(p$horizons$designation, p$site)$role[2], "A2")
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Chromosol")
})

test_that("an empty row within 0.8 m leaves the Organosol open", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("P1", NA), top_cm = c(0, 35), bottom_cm = c(35, 80),
                    field_texture = c(NA, NA)))
  expect_true(is.na(soilKey:::organosol_asc(p)$passed))
})

# ---- fourth review round ----------------------------------------------------------

test_that("the cb and sb of older codes are bleaching, not burial or sesquioxides", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1sb", "A21cb", "A22cb", "B1sb", "B21", "B22"),
                    top_cm = c(0, 5, 15, 28, 37, 76), bottom_cm = c(5, 15, 28, 37, 76, 120),
                    field_texture = c("SL", "SL", "SL", "SCL", "LMC", "MC"),
                    boundary_distinctness = c("clear", "clear", "clear", "clear", "gradual", NA)))
  x <- soilKey:::.asc_horizons(p)
  expect_false(any(x$des$buried)); expect_false(any(x$des$s)); expect_true(all(x$des$e[2:3]))
  expect_false(soilKey:::podosol_asc(p)$passed)
})

test_that("a b at the surface, with no cover, is not burial; a b soil keeps its lowest row", {
  skip_on_cran()
  p <- make_asc3_example("chromosols")
  p$horizons$designation <- paste0(p$horizons$designation, "b")
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Chromosol")
  p <- .mk_asc(list(designation = c("A1", "C", "2A1b", "2B2tb", "2BC"), top_cm = c(0, 10, 20, 35, 70),
                    bottom_cm = c(10, 20, 35, 70, 100), field_texture = c("S", "S", "SL", "MC", "LC"),
                    structure_grade = c("structureless", "structureless", "weak", "strong", "moderate")))
  expect_true(soilKey:::.asc_horizons(p)$des$buried[5])
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Rudosol")
})

test_that("unknown rows: a second A sequence and a C cover do not hide them", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c("A1", "B1", NA, "2A1", "2C"), top_cm = c(0, 10, 30, 60, 80),
                    bottom_cm = c(10, 30, 60, 80, 120), field_texture = c("L", "CL", "CL", "SL", "S"),
                    structure_grade = c("weak", "moderate", "strong", "weak", NA)))
  expect_true(is.na(soilKey:::dermosol_asc(p)$passed))
  p <- .mk_asc(list(designation = c("C1", "A1", NA, "C2"), top_cm = c(0, 10, 25, 60),
                    bottom_cm = c(10, 25, 60, 100), field_texture = c("S", "SL", "LC", "S"),
                    structure_grade = c(NA, "weak", "strong", NA)))
  expect_true(is.na(soilKey:::rudosol_asc(p)$passed))
})

# ---- fifth review round -----------------------------------------------------------

test_that("a b inside one material, with unmarked horizons below, leaves no hole", {
  skip_on_cran()
  p <- make_asc3_example("chromosols")
  p$horizons$designation <- c("A1", "A2b", "B21t", "B22t", "BC")
  expect_false(any(soilKey:::.asc_horizons(p)$des$buried))
  expect_identical(classify_asc3(p, on_missing = "silent")$rsg_or_order, "Chromosol")
  # a prefixed cb is burial with concretions; a 2A1b over 2A12 stays buried
  p <- .mk_asc(list(designation = c("A1", "C", "2A1b", "2A12"), top_cm = c(0, 10, 30, 45),
                    bottom_cm = c(10, 30, 45, 80)))
  expect_identical(soilKey:::.asc_horizons(p)$des$buried, c(FALSE, FALSE, TRUE, TRUE))
  d <- soilKey:::.asc_horizons(.mk_asc(list(designation = c("A1", "2A2cb", "2B2tb"), top_cm = c(0, 30, 45),
                                            bottom_cm = c(30, 45, 80))))$des
  expect_identical(d$buried, c(FALSE, TRUE, TRUE))
})

test_that("no row above the first A horizon is a possible Podosol B", {
  skip_on_cran()
  p <- .mk_asc(list(designation = c(NA, "A2", "B21", "B22"), top_cm = c(0, 10, 30, 60),
                    bottom_cm = c(10, 30, 60, 100), field_texture = c("SL", "SL", "SCL", "SCL")))
  expect_false(soilKey:::podosol_asc(p)$passed)
})

# ---- v0.9.279: field texture first in the textural B ------------------------------

test_that("the textural B reads the field texture first, lab clay only without it", {
  skip_on_cran()
  tb <- function(p) soilKey:::textural_b_asc(p)$passed
  # SL over LMC is a texture contrast by the field texture, whatever the lab says
  expect_true(tb(.duplex(c("SL", "LMC"), clay = c(15, 28))))
  # L over CL is not, even with lab clay of 22 over 45
  expect_false(tb(.duplex(c("L", "CL"), clay = c(22, 45))))
  # a horizon without a field texture is read by its lab clay
  expect_true(tb(.duplex(c("SL", NA), clay = c(NA, 38))))
  expect_false(tb(.duplex(c(NA, "SCL"), clay = c(19, NA))))        # 19 % above: SCL is not double
})
