# v0.9.286: subgroups of the Australian Soil Classification for the
# Kurosols, Sodosols and Chromosols; one list per order, the name written on
# the great group's ("Bleached, Eutrophic, Red Chromosol", as the book writes
# it), and the great groups now named with the book's comma.

.mk_s1 <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
nm_s1 <- function(p) classify_asc3(p, on_missing = "silent")$name
sg_s1 <- function(p) { lv <- classify_asc3(p, on_missing = "silent")$levels; lv$code[lv$level == "subgroups"] }

test_that("the engine finds a list for every class under a part of the path, and writes names on the parent", {
  skip_on_cran()
  lists <- list("CH-*" = "order list", "CH-AA-*" = "suborder list", "BG" = "parent list")
  expect_identical(soilKey:::.level_list(lists, c("CH", "AA", "BG"), "BG"), "parent list")
  expect_identical(soilKey:::.level_list(lists, c("CH", "AA", "XX"), "XX"), "suborder list")
  expect_identical(soilKey:::.level_list(lists, c("CH", "AB", "XX"), "XX"), "order list")
  expect_null(soilKey:::.level_list(lists, c("KU", "AB", "XX"), "XX"))
  # the book's example
  expect_identical(nm_s1(make_asc3_example("chromosols")), "Bleached, Eutrophic, Red Chromosol")
  expect_identical(classify_asc3(make_asc3_example("chromosols"), on_missing = "silent")$levels$level,
                   c("orders", "suborders", "great_groups", "subgroups"))
})

# a Chromosol (clear textural B, not sodic, not acid) the tests change
.chrom1 <- function(h = list(), site = list()) .mk_s1(utils::modifyList(list(
  designation = c("A1", "A2e", "B21t", "B22t", "BC"), top_cm = c(0, 12, 30, 60, 90), bottom_cm = c(12, 30, 60, 90, 120),
  field_texture = c("SL", "SL", "LMC", "MC", "LC"), clay_pct = c(14, 12, 42, 48, 38),
  boundary_distinctness = c("clear", "abrupt", "gradual", "gradual", NA),
  structure_grade = c("weak", "structureless", "moderate", "moderate", "weak"), structure_type = c("granular", "massive", "angular blocky", "angular blocky", "angular blocky"),
  ph_h2o = c(6.2, 6.4, 6.8, 7.0, 7.2), cec_cmol = 20, na_cmol = 0.4, ca_cmol = 10, mg_cmol = 5, k_cmol = 0.5,
  oc_pct = c(1.5, 0.4, 0.3, 0.2, 0.1), slickensides = "absent", redoximorphic_features_pct = 0, effervescence_class = "none",
  munsell_hue_moist = c("10YR", "10YR", "2.5YR", "2.5YR", "5YR"), munsell_value_moist = c(4, 7, 4, 4, 5),
  munsell_chroma_moist = c(3, 2, 6, 6, 6), munsell_hue_dry = c("10YR", "10YR", NA, NA, NA),
  munsell_value_dry = c(5, 8, NA, NA, NA), munsell_chroma_dry = c(3, 2, NA, NA, NA)), h), site)

test_that("Chromosol subgroups: bleaching, sodicity, mottling, vertic properties", {
  skip_on_cran()
  expect_identical(sg_s1(.chrom1()), "AT")                                   # Bleached
  # sodic in the lower part of the B (the BC)
  expect_identical(sg_s1(.chrom1(list(na_cmol = c(0.4, 0.4, 0.4, 0.4, 3)))), "BA")   # Bleached-Sodic
  # no bleached A2, a mottled B2t
  p <- .chrom1(list(designation = c("A1", "A2", "B21t", "B22t", "BC"), munsell_value_dry = c(5, 5, NA, NA, NA),
                    redoximorphic_features_pct = c(0, 0, 25, 25, 0), mottle_contrast = c(NA, NA, "distinct", "distinct", NA)))
  expect_identical(sg_s1(p), "DQ")                                           # Mottled
  p$horizons$redoximorphic_features_pct <- 0
  expect_identical(sg_s1(p), "CD")                                           # Haplic: whole coloured
  # at least 0.3 m of the B with vertic properties (lenticular peds in clay)
  p <- .chrom1(list(structure_type = c("granular", "massive", "lenticular", "lenticular", "angular blocky")))
  expect_identical(sg_s1(p), "BB")                                           # Bleached-Vertic
  # slickensides not recorded in a clayey B leave the vertic test open
  p <- .chrom1(list(slickensides = NA))
  expect_true(is.na(soilKey:::vertic_b_asc(p)$passed))
  # a ferric horizon within the solum and a sodic lower B: Ferric-Sodic
  p <- .chrom1(list(ferric_nodules_pct = c(0, 0, 30, 0, 0), na_cmol = c(0.4, 0.4, 0.4, 0.4, 3)))
  expect_identical(sg_s1(p), "HC")
  # fine-earth effervescence throughout the solum
  p <- .chrom1(list(effervescence_class = "slight", ph_h2o = 8, designation = c("A1", "A2", "B21t", "B22t", "BC"),
                    munsell_value_dry = c(5, 5, NA, NA, NA), carbonate_soft_pct = 0, carbonate_hard_pct = 0,
                    secondary_carbonates_pct = 0))
  expect_identical(sg_s1(p), "IE")
})

test_that("Kurosol and Sodosol subgroups", {
  skip_on_cran()
  # the Kurosol example: bleached
  expect_identical(nm_s1(make_asc3_example("kurosols")), "Bleached, Mesotrophic, Red Kurosol")
  # Sodosol subgroups by base status of the B2t and the carbonate classes
  so <- function(h) .chrom1(utils::modifyList(list(na_cmol = c(0.4, 0.4, 4, 4, 4), ph_h2o = 8), h))
  expect_identical(classify_asc3(so(list()), on_missing = "silent")$rsg_or_order, "Sodosol")
  expect_identical(sg_s1(so(list(ca_cmol = 12, mg_cmol = 6, k_cmol = 0.5))), "AH")         # Eutrophic
  # Magnesic first: Ca/Mg below 0.1
  expect_identical(sg_s1(so(list(ca_cmol = 0.3, mg_cmol = 6))), "DB")
  # a gypsic horizon within the B or BC
  p <- so(list(gypsum_visible_pct = c(0, 0, 0, 30, 30)))
  expect_true(soilKey:::gypsic_horizon_asc(p, 100, "b")$passed)
  expect_identical(sg_s1(p), "BZ")
  # a siliceous pan in the upper B
  p <- so(list(pan_type = c(NA, NA, "silcrete", NA, NA), cementation_class = c(NA, NA, "strongly", NA, NA)))
  expect_true(soilKey:::silpan_ab_asc(p)$passed)
})

test_that("the new subgroup diagnostics, three-valued", {
  skip_on_cran()
  # the lower part of the B sodic: the deepest B; a row of unknown role below may be it
  p <- .chrom1(list(na_cmol = c(0.4, 0.4, 0.4, 0.4, 3)))
  expect_true(soilKey:::lower_b_sodic_asc(p)$passed)
  p <- .chrom1(list(na_cmol = c(0.4, 0.4, 0.4, 0.4, NA), cec_cmol = c(20, 20, 20, 20, NA)))
  expect_true(is.na(soilKey:::lower_b_sodic_asc(p)$passed))
  # a reticulate horizon below the B2t, named in the record
  p <- .chrom1(list(designation = c("A1", "A2e", "B21t", "B22t", "C"), layer_origin = c(NA, NA, NA, NA, "reticulate horizon")))
  expect_true(soilKey:::reticulate_below_asc(p)$passed)
  expect_false(soilKey:::reticulate_below_asc(.chrom1())$passed)
  # melacic and melanic on their own
  expect_false(soilKey:::chernic_sg_asc(.chrom1(), "melanic")$passed)
})

test_that("subgroup diagnostics: extents, unknown rows, missing depths", {
  skip_on_cran()
  # melacic and melanic on their own: an A12 of unknown dry colour may or may not belong to the dark horizon
  p <- .chrom1(list(designation = c("A11", "A12", "B21t", "B22t", "BC"), top_cm = c(0, 25, 80, 100, 130),
                    bottom_cm = c(25, 80, 100, 130, 150), munsell_value_moist = c(2, 2, 4, 4, 5), munsell_chroma_moist = c(1, 1, 6, 6, 6),
                    munsell_value_dry = c(4, NA, NA, NA, NA), munsell_chroma_dry = c(1, NA, NA, NA, NA), ph_h2o = c(5.0, 6.5, 6.8, 7, 7.2),
                    structure_grade = c("moderate", "moderate", "moderate", "moderate", "weak"), ped_size_max_mm = 10,
                    field_texture = c("SL", "SL", "LMC", "MC", "LC"), boundary_distinctness = c("clear", "abrupt", "gradual", "gradual", NA)))
  expect_true(is.na(soilKey:::chernic_sg_asc(p, "melacic")$passed))
  expect_true(is.na(soilKey:::chernic_sg_asc(p, "melanic")$passed))
  # a reticulate B23 of unknown bottom under the B22t is not below the B2t
  p <- .chrom1(list(designation = c("A1", "A2e", "B21t", "B22t", "B23"), bottom_cm = c(12, 30, 60, 90, NA),
                    layer_origin = c(NA, NA, NA, NA, "reticulate horizon")))
  expect_false(soilKey:::reticulate_below_asc(p)$passed)
  # a siliceous pan on an undesignated row between two A2 horizons
  p <- .chrom1(list(designation = c("A1", "A21e", NA, "B21t", "B22t"), pan_type = c(NA, NA, "siliceous pan", NA, NA),
                    cementation_class = c(NA, NA, "strongly", NA, NA)))
  expect_true(soilKey:::silpan_ab_asc(p)$passed %in% c(TRUE, NA))
  # no B top depth: undecided, no error
  p <- .chrom1(list(na_cmol = c(0.4, 0.4, 0.4, 0.4, 3))); p$horizons$top_cm[3:5] <- NA
  expect_true(is.na(soilKey:::lower_b_sodic_asc(p)$passed))
  # a gap in the record may end the dark horizon: 0-15 cm alone is too thin
  p <- .chrom1(list(designation = c("A11", "A12", "B21t", "B22t", "BC"), top_cm = c(0, 18, 30, 60, 90),
                    bottom_cm = c(15, 30, 60, 90, 120), munsell_value_moist = c(2, 2, 4, 4, 5), munsell_chroma_moist = c(1, 1, 6, 6, 6),
                    munsell_value_dry = c(4, 4, NA, NA, NA), munsell_chroma_dry = c(1, 1, NA, NA, NA), ph_h2o = c(5.0, 5.0, 6.8, 7, 7.2)))
  expect_true(is.na(soilKey:::chernic_sg_asc(p, "melacic")$passed))
  expect_false(soilKey:::chernic_sg_asc(p, "melanic")$passed)
  # a row of unknown role just above the first A3 may be an A2
  p <- .chrom1(list(designation = c("A1", NA, "A3", "B21t", "B22t"), top_cm = c(0, 10, 15, 30, 60), bottom_cm = c(10, 15, 30, 60, 90),
                    pan_type = c(NA, "siliceous pan", NA, NA, NA), cementation_class = c(NA, "strongly", NA, NA, NA)))
  expect_true(is.na(soilKey:::silpan_ab_asc(p)$passed))
  # one that starts at the bottom of the uppermost B2, or below the top of a
  # B2 of unrecorded bottom, lies below it
  sp <- function(top, bot) .chrom1(list(designation = c("A1", "A2e", "B21t", NA, "BC"), top_cm = top, bottom_cm = bot,
                                        pan_type = c(NA, NA, NA, "silcrete", NA), cementation_class = c(NA, NA, NA, "strongly", NA)))
  expect_false(soilKey:::silpan_ab_asc(sp(c(0, 12, 30, 60, 90), c(12, 30, 60, 90, 120)))$passed)
  expect_false(soilKey:::silpan_ab_asc(sp(c(0, 12, 30, 90, 100), c(12, 30, NA, 100, 120)))$passed)
  # a reticulate C below a B22t of unrecorded bottom lies below it
  p <- .chrom1(list(designation = c("A1", "A2e", "B21t", "B22t", "C"), layer_origin = c(NA, NA, NA, NA, "reticulate horizon")))
  p$horizons$bottom_cm[4] <- NA
  expect_true(soilKey:::reticulate_below_asc(p)$passed)
  # a row between two vertic B horizons is a B horizon
  p <- .chrom1(list(designation = c("A1", "A2e", "B21t", NA, "B22t"), top_cm = c(0, 12, 30, 50, 60), bottom_cm = c(12, 30, 50, 60, 65),
                    structure_type = c("granular", "massive", "lenticular", "lenticular", "lenticular")))
  expect_true(soilKey:::vertic_b_asc(p)$passed)
})

test_that("the Natric Kurosols take no Sodic subgroup", {
  skip_on_cran()
  p <- make_asc3_example("kurosols")
  p$horizons$na_cmol <- 0.1 * p$horizons$cec_cmol
  r <- classify_asc3(p, on_missing = "silent")
  expect_true(r$levels$code[r$levels$level == "great_groups"] %in% c("FD", "GP"))
  expect_false(r$levels$code[r$levels$level == "subgroups"] %in% c("BA", "HB", "EO"))
  # the trace carries the names written on the parent
  sg_trace <- r$trace$subgroups
  expect_false(any(grepl("{parent}", vapply(sg_trace, function(e) e$name, ""), fixed = TRUE)))
  expect_error(utils::capture.output(print(r)), NA)
})
