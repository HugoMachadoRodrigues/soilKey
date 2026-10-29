# v0.9.293: the New Zealand Soil Classification, 4th ed., keyed to the 15
# orders; the New Zealand notation, the order gates and the readings.

.nz <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  site <- utils::modifyList(list(id = "nz", country = "NZ", designation_system = "nzsc",
                                 drainage_class = "well drained"), site)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.nz_order <- function(p) classify_nzsc4(p, on_missing = "silent")$levels$code[1]

test_that("each example profile keys to its order, decided", {
  skip_on_cran()
  codes <- c(organic = "O", gley = "G", ultic = "U", podzol = "Z", allophanic = "L", pumice = "M",
             melanic = "E", semiarid = "S", oxidic = "X", granular = "N", pallic = "P", brown = "B",
             anthropic = "A", recent = "R", raw = "W")
  for (o in names(codes)) {
    r <- classify_nzsc4(make_nzsc4_example(o), on_missing = "silent")
    expect_identical(r$levels$code[1], codes[[o]], info = o)
    expect_identical(r$system_id, "nzsc4", info = o)
    # decided at the level of orders (since 0.9.298 a group may still be open)
    expect_length(grep("^orders:", r$provisional_because, value = TRUE), 0L)
  }
  expect_identical(classify_nzsc4(make_nzsc4_example("brown"), on_missing = "silent")$rsg_or_order, "Brown Soils")
})

test_that("the registry lists the NZSC as an opt-in system with valid rules", {
  skip_on_cran()
  s <- soilkey_systems()
  expect_true("nzsc4" %in% s$id)
  expect_false(s$default[s$id == "nzsc4"])
  expect_identical(soilKey:::.system_resolve("nzsc"), "nzsc4")
  v <- soilKey:::.validate_rules("nzsc4")
  expect_length(v$missing, 0L); expect_length(v$unsuffixed, 0L); expect_length(v$codes, 0L)
  res <- classify_all(make_nzsc4_example("pallic"), systems = c("wrb", "nzsc"), on_missing = "silent")
  expect_identical(res$summary$nzsc4, "Typic Fragic Pallic Soils")   # with its subgroup since 0.9.301
  expect_length(yaml::read_yaml(system.file("rules", "nzsc4", "readings.yaml", package = "soilKey"))$readings, 69L)
})

test_that("the New Zealand notation is read", {
  skip_on_cran()
  d <- soilKey:::.nzsc_designations(c("Bw(g)", "Bhm", "Bsm", "Blm", "Bfm", "eAp", "bAh", "Ahb", "2Bt",
                                      "Cr", "CR", "Om", "Oa", "Bx", "B(x)", "BCt", "Bqm", "Bw(h)1", "E"),
                                    list(country = "NZ"))
  expect_identical(d$role, c("B", "B", "B", "B", "B", "A", "A", "A", "B", "C", "CR", "O", "O", "B", "B", "BC", "B", "B", "E"))
  expect_true(d$w[1] && d$g_weak[1] && !d$g[1])
  expect_identical(which(d$humus_pan), 2L); expect_identical(which(d$ortstein), 3L)
  expect_identical(which(d$ironstone_pan), 4L); expect_identical(which(d$placic), 5L)
  expect_identical(which(d$anthropic), 6L); expect_identical(which(d$buried), c(7L, 8L))
  expect_identical(d$prefix[9], "2"); expect_true(d$t[9] && d$t[16])
  expect_true(d$r[10])                                     # a reductimorphic C in the NZ notation
  expect_identical(d$decomp[12:13], c("mesic", "humified"))
  expect_true(d$x[14] && !d$x_weak[14] && d$x_weak[15] && !d$x[15])
  expect_identical(which(d$duripan), 17L); expect_true(d$h_weak[18] && d$w[18])
  expect_false(soilKey:::.nzsc_designations("Cr", list(country = "US"))$r)
  # v0.9.298: outside the New Zealand notation a Cr is a paralithic contact
  expect_identical(soilKey:::.nzsc_designations("Cr", list(country = "US"))$role, "CR")   # weathered rock elsewhere
})

test_that("Organic, Gley and Podzol gates", {
  skip_on_cran()
  # Organic: 35 cm of wet peat; litter alone is not
  peat <- .nz(list(designation = c("Om", "Oh", "Cg"), top_cm = c(0, 20, 35), bottom_cm = c(20, 35, 90),
                   oc_pct = c(40, 30, 1), munsell_value_moist = c(2, 2, 5), munsell_chroma_moist = c(1, 1, 1)),
              list(drainage_class = "very poorly drained"))
  expect_identical(.nz_order(peat), "O")
  litter <- .nz(list(designation = c("Lf", "Ah", "Bw", "C"), top_cm = c(0, 40, 50, 80), bottom_cm = c(40, 50, 80, 120)))
  expect_false(isTRUE(organic_soils_nzsc(litter)$passed))  # fibric litter is neither O nor Lm/Lh
  # Gley: a reductimorphic horizon from 20 cm over loam to 60 cm only, or over rock
  g <- function(lower, extra = list()) .nz(utils::modifyList(list(
    designation = c("Ah", "Bg", lower), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
    munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 1, 3), phosphate_retention_pct = 20,
    fluidity_class = "non-fluid"), extra))
  expect_true(isTRUE(gley_soils_nzsc(g("R"))$passed))       # over a lithic contact
  # Hugo's decision: reaching the base of the B horizon is enough
  expect_true(isTRUE(gley_soils_nzsc(g("C"))$passed))
  expect_true(isTRUE(gley_soils_nzsc(g("Cg"))$passed))      # continues to 100 cm
  short <- .nz(list(designation = c("Ah", "Bg", "Bw", "C"), top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 100),
                    munsell_value_moist = c(3, 5, 5, 6), munsell_chroma_moist = c(2, 1, 4, 3), phosphate_retention_pct = 20))
  expect_false(isTRUE(gley_soils_nzsc(short)$passed))       # ends at 40 cm, above the base of the B
  at30 <- .nz(list(designation = c("Ap", "Bg", "Cg"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
                   munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 1, 1), phosphate_retention_pct = 20))
  expect_false(isTRUE(gley_soils_nzsc(at30)$passed))        # from 30 cm: a mottled, not a gley, profile form
  expect_false(isTRUE(gley_soils_nzsc(g("Cg", list(fluidity_class = c("moderately fluid", "non-fluid", "non-fluid"))))$passed))
  # Gley route 2: an ironstone-pan within 30 cm and a positive Childs' test
  ip <- .nz(list(designation = c("Ah", "Blm", "Bw", "C"), top_cm = c(0, 20, 22, 60), bottom_cm = c(20, 22, 60, 100),
                 childs_test = c("positive", NA, NA, NA), munsell_value_moist = c(3, 2, 5, 6), munsell_chroma_moist = c(2, 1, 6, 4)))
  expect_true(isTRUE(gley_soils_nzsc(ip)$passed))
  # Hugo's decision: a pale B designated without g is not reductimorphic; an
  # undesignated one is read by its colours
  pale <- function(d) .nz(list(designation = c("Ah", d, "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                               munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 2, 3),
                               phosphate_retention_pct = 20, bulk_density_g_cm3 = 1.3))
  x <- soilKey:::.nzsc_horizons(pale("Bw"))
  expect_false(any(soilKey:::.nzsc_reductimorphic_rows(x)[2]))
  x2 <- soilKey:::.nzsc_horizons(pale(NA))
  expect_true(soilKey:::.nzsc_reductimorphic_rows(x2)[2])
  # Podzol: a Bh with pH below 5.5, a humus-pan alone
  pz <- function(ph, d = "Bh") .nz(list(designation = c("Ah", "E", d, "C"), top_cm = c(0, 10, 25, 40), bottom_cm = c(10, 25, 40, 90),
                                        ph_h2o = ph))
  expect_true(isTRUE(podzol_soils_nzsc(pz(4.6))$passed))
  expect_false(isTRUE(podzol_soils_nzsc(pz(6.0))$passed))
  expect_true(isTRUE(podzol_soils_nzsc(pz(6.0, "Bhm"))$passed))
})

test_that("allophanic soil material: a measured P retention decides", {
  skip_on_cran()
  al <- function(pr, bd = 0.7, field = "strong") .nz(list(
    designation = c("Ah", "Bw1", "Bw2", "C"), top_cm = c(0, 20, 50, 90), bottom_cm = c(20, 50, 90, 120),
    phosphate_retention_pct = pr, bulk_density_g_cm3 = bd, naf_test = field, sensitivity_class = "sensitive",
    strength_moist = "weak", stickiness = "slightly sticky"))
  expect_true(isTRUE(allophanic_soils_nzsc(al(90))$passed))
  expect_false(isTRUE(allophanic_soils_nzsc(al(80))$passed))       # the field tests do not override it
  expect_true(isTRUE(allophanic_soils_nzsc(al(NA))$passed))        # they stand in when it is not measured
  expect_false(isTRUE(allophanic_soils_nzsc(al(NA, field = "moderate"))$passed))
  expect_false(isTRUE(allophanic_soils_nzsc(al(90, bd = 1.0))$passed))
  expect_true(is.na(allophanic_soils_nzsc(al(90, bd = NA))$passed))
})

test_that("Brown, Pallic and Recent colours; the overthickened A note", {
  skip_on_cran()
  cc <- soilKey:::.nzsc_colour_class(data.frame(munsell_hue_moist = c("7.5YR", "10YR", "10YR", "2.5Y", "10YR"),
                                                munsell_value_moist = c(4, 5, 5, 5, 3), munsell_chroma_moist = c(4, 6, 2, 6, 3)))
  expect_identical(cc, c("brown", "brown", "pale", "intermediate", "intermediate"))
  # overthickened A: the key starts at Recent Soils when the site records it
  p <- make_nzsc4_example("brown")
  h <- p$horizons; h$designation <- c("Ap1", "Ap2", "Bw", "C"); h$top_cm <- c(0, 25, 50, 85); h$bottom_cm <- c(25, 50, 85, 120)
  p$horizons <- h
  expect_identical(.nz_order(p), "B")
  p$site$overthickened_a <- TRUE
  r <- classify_nzsc4(p, on_missing = "silent")
  expect_identical(r$levels$code[1], "R")
  expect_match(paste(r$warnings, collapse = " "), "overthickened")
})

test_that("text review: undesignated rows, Bs, carbonate, transitions, Pallic and Melanic details", {
  skip_on_cran()
  # with no designations the B diagnostics are undecided, not failed
  p <- make_nzsc4_example("brown"); h <- p$horizons; h$designation <- NA_character_; p$horizons <- h
  r <- classify_nzsc4(p, on_missing = "silent")
  expect_true(length(r$provisional_because) > 0L)
  expect_match(paste(r$warnings, collapse = " "), "No horizon designations")
  # a Bs needs an eluvial horizon above it (or unrecorded coatings); a Bw
  # under an E with a strong reactive-aluminium test and weak blocks is one
  bs <- .nz(list(designation = c("Ah", "Bs", "C"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 90), ph_h2o = 5.0))
  expect_true(is.na(podzol_soils_nzsc(bs)$passed))
  bw <- .nz(list(designation = c("Ah", "E", "Bw", "C"), top_cm = c(0, 10, 25, 60), bottom_cm = c(10, 25, 60, 100),
                 naf_test = c(NA, NA, "strong", NA), structure_grade = c("moderate", "weak", "weak", "structureless"),
                 structure_type = c("granular", "platy", "blocky", "massive"), ph_h2o = 5.0))
  expect_true(isTRUE(podzol_soils_nzsc(bw)$passed))
  # a k suffix alone is no evidence of pedogenic carbonate
  ck <- .nz(list(designation = c("Ap", "Bw", "Ck"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
                 effervescence_class = c("none", "none", "strong"), ph_h2o = c(6.5, 7.0, 8.2)))
  expect_true(is.na(semiarid_soils_nzsc(ck)$evidence$pedogenic_carbonate))
  # a BA is not a weathered-B horizon
  x <- soilKey:::.nzsc_horizons(.nz(list(designation = c("Ah", "BA", "Bw", "C"), top_cm = c(0, 20, 30, 60),
                                         bottom_cm = c(20, 30, 60, 100), structure_grade = "moderate",
                                         structure_type = "blocky")))
  expect_identical(soilKey:::.nzsc_weathered_b_rows(x)[2:3], c(FALSE, TRUE))
  # Pallic 2(b)(iii) fails when sodic features are known to be absent
  pa <- .nz(list(designation = c("Ap", "Bt", "C"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 100),
                 munsell_hue_moist = "10YR", munsell_value_moist = c(4, 5, 6), munsell_chroma_moist = c(2, 3, 3),
                 esp_pct = 2, structure_type = c("granular", "blocky", "massive"), clay_pct = c(18, 30, 20),
                 structure_grade = c("weak", "weak", "structureless"), effervescence_class = "none",
                 silt_pct = c(60, 50, 60), phosphate_retention_pct = 20))
  expect_false(pallic_soils_nzsc(pa)$evidence$b_horizon)
  # Melanic parent material: "non-calcareous" is not calcareous
  nc <- .nz(list(designation = c("Ah", "Bw"), top_cm = c(0, 20), bottom_cm = c(20, 60), rock_origin = "non-calcareous loess"))
  expect_false(soilKey:::.nzsc_melanic_parent(nc, soilKey:::.nzsc_horizons(nc)))
})

test_that("logic review: thin topsoils, open depths, continuous colours, other notations", {
  skip_on_cran()
  # a 0.5 cm litter over an A, and a 0.5 cm profile, return
  expect_true(.nz_order(.nz(list(designation = c("L", "Ah", "Bw", "C"), top_cm = c(0, 0.5, 15, 40),
                                 bottom_cm = c(0.5, 15, 40, 90)))) %in% c("R", "W", "B"))
  expect_identical(.nz_order(.nz(list(designation = "eAp", top_cm = 0, bottom_cm = 0.5))), "W")
  # rows with no designation below a designated one may be anything
  g <- .nz(list(designation = c("Ap", NA, NA), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 100),
                munsell_value_moist = c(3, 5, 6), munsell_chroma_moist = c(2, 1, 1)), list(drainage_class = "poorly drained"))
  r <- classify_nzsc4(g, on_missing = "silent")
  expect_true("orders:G" %in% r$provisional_because)
  # continuous Munsell chroma (4.5) is read to the nearest chip; N is neutral
  expect_identical(soilKey:::.nzsc_colour_class(data.frame(munsell_hue_moist = c("N", "5RP", "10YR"),
                                                           munsell_value_moist = c(6, 4, 5), munsell_chroma_moist = c(0, 4, 4.4))),
                   c("pale", "brown", "intermediate"))
  # a deepest horizon with no bottom depth
  expect_true(isTRUE(oxidic_soils_nzsc(.nz(list(designation = c("Ah", "Bo"), top_cm = c(0, 35), bottom_cm = c(35, NA))))$passed))
  # FAO/USDA: the ss of slickensides is not an s; O horizons of a well
  # drained forest floor are not wet peat
  d <- soilKey:::.nzsc_designations(c("Bss", "Btss"), list(country = "US"))
  expect_false(any(d$s))
  oi <- .nz(list(designation = c("Oi", "Oe", "A", "Bw"), top_cm = c(0, 10, 35, 50), bottom_cm = c(10, 35, 50, 90),
                 oc_pct = c(45, 40, 3, 1)), list(country = "US", designation_system = NA))
  expect_false(isTRUE(organic_soils_nzsc(oi)$passed))
  # duplicated rows count once; runs are read row by row
  x <- soilKey:::.nzsc_horizons(.nz(list(designation = c("Om", "Om"), top_cm = c(0, 0), bottom_cm = c(20, 20))))
  expect_identical(soilKey:::.nzsc_cum(x, c(TRUE, TRUE), 0, 60)[1], 20)
  x <- soilKey:::.nzsc_horizons(.nz(list(designation = c("A", "B", "C"), top_cm = c(0, 5, 60), bottom_cm = c(5, 60, 100))))
  expect_identical(soilKey:::.nzsc_run_max(x, c(NA, TRUE, TRUE), 0, 60), c(55, 60))
  x <- soilKey:::.nzsc_horizons(.nz(list(designation = c("A", "B"), top_cm = c(0, 20), bottom_cm = c(10, 45))))
  expect_identical(soilKey:::.nzsc_run_max(x, c(TRUE, TRUE), 0, 200, from_top = TRUE), c(10, 10))
})
