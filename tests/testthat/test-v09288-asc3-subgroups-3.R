# v0.9.288: subgroups of the Australian Soil Classification for the
# Vertosols, Calcarosols, Podosols and Organosols.

sg_s3 <- function(p) { lv <- classify_asc3(p, on_missing = "silent")$levels; lv$code[lv$level == "subgroups"] }
ex_s3 <- function(order, h = list(), site = list()) {
  p <- make_asc3_example(order)
  d <- as.data.frame(p$horizons)
  for (k in names(h)) d[[k]] <- h[[k]]
  PedonRecord$new(site = utils::modifyList(p$site, site), horizons = ensure_horizon_schema(data.table::as.data.table(d)))
}
mk_s3 <- function(df, site = list()) {
  PedonRecord$new(site = utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                                drainage_class = "well drained"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(df)))
}

test_that("the examples key to a subgroup, named on the great group", {
  skip_on_cran()
  want <- c(vertosols = "Endocalcareous, Self-mulching, Black Vertosol",
            calcarosols = "Ceteric, Regolithic, Hypercalcic Calcarosol",
            podosols = "Fragic, Humosesquic/Sesquic, Aeric Podosol",
            organosols = "Terric, Acidic, Sapric Organosol")
  for (o in names(want)) {
    r <- classify_asc3(make_asc3_example(o), on_missing = "silent")
    expect_identical(r$name, want[[o]], info = o)
    expect_length(r$provisional_because, 0L)
  }
})

test_that("the lists: 43 Vertosol, 12 Calcarosol (10 for the Carbonatic), 15 Podosol and 8 Organosol subgroups", {
  skip_on_cran()
  sg <- load_rules("asc3")$subgroups
  codes <- function(path) vapply(soilKey:::.level_list(sg, path, path[3]), function(t) t$code, "")
  expect_length(codes(c("VE", "AA", "XX")), 43L)
  expect_length(codes(c("CA", "CV", "XX")), 12L)
  expect_identical(setdiff(codes(c("CA", "CV", "XX")), codes(c("CA", "JT", "XX"))), c("HK", "CP"))
  expect_length(codes(c("PO", "AL", "XX")), 15L)
  expect_length(codes(c("OR", "BW", "XX")), 8L)
  # Ceteric takes every Calcarosol that reaches it
  expect_true(isTRUE(sg[["CA-*"]][[12]]$tests$default))
})

test_that("Vertosols: the upper 0.5 m of the solum and the solum below it", {
  skip_on_cran()
  # strongly acid above 0.5 m: Epiacidic
  p <- ex_s3("vertosols", list(ph_h2o = c(5.0, 5.2, 6.5, 7.0), effervescence_class = "none"))
  expect_true(soilKey:::solum_part_asc(p, "acid", "upper", "major")$passed)
  expect_false(soilKey:::solum_part_asc(p, "acid", "lower", "major")$passed)
  expect_identical(sg_s3(p), "GA")
  # a hypersodic surface horizon is not a subsurface one; one below it is
  p <- ex_s3("vertosols", list(na_cmol = c(7, 1, 2, 3)))
  expect_false(soilKey:::solum_part_asc(p, "hypersodic", "upper", "some_subsurface")$passed)
  expect_true(soilKey:::solum_part_asc(p, "hypersodic", "upper", "some")$passed)
  p <- ex_s3("vertosols", list(na_cmol = c(0.5, 7, 2, 3)))
  expect_true(soilKey:::solum_part_asc(p, "hypersodic", "upper", "some_subsurface")$passed)
  # Episodic: the weighted ESP of the upper 0.1 m (A1 0-10, CEC 40): Na 3 gives 7.5
  p <- ex_s3("vertosols", list(na_cmol = c(3, 1, 2, 3)))
  expect_true(soilKey:::episodic_asc(p)$passed)
  expect_false(soilKey:::episodic_asc(make_asc3_example("vertosols"))$passed)
  expect_identical(sg_s3(p), "GI")   # Episodic-Endocalcareous
  # ESP not recorded at the surface: undecided
  expect_true(is.na(soilKey:::episodic_asc(ex_s3("vertosols", list(na_cmol = c(NA, 1, 2, 3))))$passed))
  # a solum thinner than 0.5 m has no part below 0.5 m
  d <- data.frame(designation = c("A1", "B2", "C"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 80), ph_h2o = 5,
                  stringsAsFactors = FALSE)
  expect_false(soilKey:::solum_part_asc(mk_s3(d), "acid", "lower", "major")$passed)
  # Sulfidic: a sulfide test not done is undecided
  expect_true(is.na(soilKey:::acid_sulfate_gg_asc(ex_s3("vertosols", list(sulfidic_s_pct = NA_real_)), "sulfidic")$passed))
})

test_that("Calcarosols: surface carbonate, subplasticity, hypersodicity", {
  skip_on_cran()
  # the upper 0.1 m dominantly soft carbonate
  p <- ex_s3("calcarosols", list(carbonate_soft_pct = c(60, 12, 22, 18, 5)))
  expect_true(soilKey:::calsilic_gg_asc(p, "supravescent")$passed)
  expect_identical(sg_s3(p), "HK")
  p <- ex_s3("calcarosols", list(effervescence_class = c("strong", "strong", "strong", "very strong", "strong")))
  expect_identical(sg_s3(p), "CP")
  p <- ex_s3("calcarosols", list(effervescence_class = c("none", "strong", "strong", "very strong", "strong"),
                                 secondary_carbonates_pct = c(0, 15, 30, 25, 5), caco3_pct = c(0, 12, 25, 20, 10)))
  expect_identical(sg_s3(p), "IB")
  p <- ex_s3("calcarosols", list(subplasticity = c(NA, "strongly subplastic", "strongly subplastic", "none", NA)))
  expect_true(soilKey:::subplastic_asc(p, "b")$passed)
  expect_identical(sg_s3(p), "ET")
  p <- ex_s3("calcarosols", list(na_cmol = c(0.3, 3, 0.3, 0.3, 0.3)))
  expect_identical(sg_s3(p), "BR")
})

test_that("Podosols: the coherence of the B and a densipan in the A2", {
  skip_on_cran()
  # the Aeric example: weakly coherent and porous B horizons, as its page describes them
  p <- make_asc3_example("podosols")
  expect_false(soilKey:::podosol_coherence_asc(p, "strong")$passed)
  expect_true(soilKey:::podosol_coherence_asc(p, "weak")$passed)
  expect_identical(sg_s3(p), "BY")
  p <- ex_s3("podosols", list(strength_moist = c("loose", "loose", "very firm", "weak", "loose")))
  expect_true(soilKey:::podosol_coherence_asc(p, "strong")$passed)
  expect_identical(sg_s3(p), "DV")
  # a row of unknown role between two B horizons is a B horizon
  p <- ex_s3("podosols", list(designation = c("A1", "A2e", "B21hs", NA, "C"), strength_moist = c("loose", "loose", "firm", "strong", "loose")))
  expect_true(soilKey:::podosol_coherence_asc(p, "strong")$passed %in% c(TRUE, NA))
  # a thin ironpan's own strength does not make the B strongly coherent
  p <- ex_s3("podosols", list(designation = c("A1", "A2e", "B2hs", "B2sm", "C"), strength_moist = c("loose", "loose", "firm", "rigid", "loose")))
  expect_false(soilKey:::podosol_coherence_asc(p, "strong")$passed)
  expect_false(soilKey:::podosol_coherence_asc(p, "weak")$passed)
  p <- ex_s3("podosols", list(strength_moist = c("loose", "loose", NA, "weak", "loose")))
  expect_true(is.na(soilKey:::podosol_coherence_asc(p, "strong")$passed))
  # coffee rock is strongly coherent
  p <- ex_s3("podosols", list(strength_moist = c("loose", "loose", "firm", "weak", "loose"),
                              layer_origin = c(NA, NA, "coffee rock", NA, NA)))
  expect_true(soilKey:::podosol_coherence_asc(p, "strong")$passed)
  # a densipan in the A2
  p <- ex_s3("podosols", list(pan_type = c(NA, "densipan", NA, NA, NA)))
  expect_true(soilKey:::densipan_a2_asc(p)$passed)
  expect_identical(sg_s3(p), "BI")
  expect_false(soilKey:::densipan_a2_asc(make_asc3_example("podosols"))$passed)
})

test_that("Organosols: what the organic materials overlie", {
  skip_on_cran()
  org <- function(d3, extra = list()) {
    d <- data.frame(designation = c("P1", "P2", d3), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
                    field_texture = c("HP", "AP", "LC"), clay_pct = c(NA, NA, 38), oc_pct = c(38, 30, 1.5),
                    sulfidic_s_pct = 0.005, ph_h2o = c(4.8, 5.0, 5.5), munsell_value_moist = c(2, 2, 4),
                    munsell_chroma_moist = c(1, 1, 2), stringsAsFactors = FALSE)
    for (k in names(extra)) d[[k]] <- extra[[k]]
    mk_s3(d, list(drainage_class = "very poorly drained"))
  }
  expect_identical(sg_s3(org("R")), "CZ")
  expect_identical(sg_s3(org("C", list(layer_origin = c(NA, NA, "saprolite")))), "DU")
  expect_identical(sg_s3(org("C", list(layer_origin = c(NA, NA, "marl")))), "DD")
  # black mineral material under the peat
  expect_identical(sg_s3(org("A1", list(munsell_value_moist = c(2, 2, 2), munsell_value_dry = c(NA, NA, 4)))), "IJ")
  expect_identical(sg_s3(org("C", list(layer_origin = c(NA, NA, "alluvium")))), "FS")
  # a C of unrecorded origin may be saprolite
  expect_true(is.na(soilKey:::organosol_sg_asc(org("C"), "paralithic")$passed))
  # peat to 1 m over alluvium: Regolithic
  d <- data.frame(designation = c("P1", "P2", "C"), top_cm = c(0, 40, 100), bottom_cm = c(40, 100, 150),
                  field_texture = c("HP", "AP", "LC"), oc_pct = c(38, 30, 1.5), layer_origin = c(NA, NA, "alluvium"),
                  munsell_value_moist = c(2, 2, 4), munsell_chroma_moist = c(1, 1, 2), stringsAsFactors = FALSE)
  expect_false(soilKey:::organosol_sg_asc(mk_s3(d), "terric")$passed)
  expect_true(soilKey:::organosol_sg_asc(mk_s3(d), "regolithic")$passed)
  # an alluvial cover above the peat is not within or below it
  d2 <- rbind(data.frame(designation = "C1", top_cm = 0, bottom_cm = 15, field_texture = "SL", oc_pct = 1,
                         layer_origin = "alluvium", munsell_value_moist = 4, munsell_chroma_moist = 3), d)
  d2$top_cm[2:4] <- c(15, 40, 100)
  expect_false(soilKey:::organosol_sg_asc(mk_s3(d2), "terric")$passed)
  # an A or B horizon under the peat is soil material, not saprolite
  p <- org("B2")
  expect_false(soilKey:::organosol_sg_asc(p, "paralithic")$passed)
  expect_true(soilKey:::organosol_sg_asc(p, "terric")$passed)
  # a row of unknown material under the peat may be organic: Modic undecided
  p <- org("C", list(oc_pct = c(38, 30, 15), field_texture = c("HP", "AP", NA), clay_pct = NA_real_,
                     layer_origin = c(NA, NA, "alluvium"), munsell_value_moist = c(2, 2, 2), munsell_value_dry = c(NA, NA, 4)))
  expect_true(is.na(soilKey:::organosol_sg_asc(p, "modic")$passed))
})

test_that("the solum: rows after the description, its base at any row of unknown role, pans, unknown depths", {
  skip_on_cran()
  v <- function(h) {
    n <- length(h$designation)
    d <- data.frame(designation = h$designation, top_cm = h$top_cm, bottom_cm = h$bottom_cm,
                    field_texture = "MC", clay_pct = 55, ph_h2o = h$ph_h2o %||% 7, cec_cmol = 40,
                    na_cmol = h$na_cmol %||% 1, stringsAsFactors = FALSE)
    if (!is.null(h$pan_type)) d$pan_type <- h$pan_type
    mk_s3(d)
  }
  # a row below the last designated one may be solum
  p <- v(list(designation = c("A1", "B21", "B22", NA), top_cm = c(0, 10, 40, 60), bottom_cm = c(10, 40, 60, 120),
              ph_h2o = c(7, 7, 7, 4.5)))
  expect_true(is.na(soilKey:::solum_part_asc(p, "acid", "lower", "major")$passed))
  # the solum may end at the first of two rows of unknown role
  p <- v(list(designation = c("A1", "B2", NA, NA, "C"), top_cm = c(0, 10, 60, 80, 200), bottom_cm = c(10, 60, 80, 200, 250),
              ph_h2o = c(7, 7, 4.5, 7, 7)))
  expect_true(is.na(soilKey:::solum_part_asc(p, "acid", "lower", "major")$passed))
  # no designations at all: undecided
  p <- v(list(designation = c(NA, NA), top_cm = c(0, 30), bottom_cm = c(30, 120), ph_h2o = c(7, 7)))
  expect_true(is.na(soilKey:::solum_part_asc(p, "acid", "lower", "major")$passed))
  # a horizon of unknown depth below a hypersodic one
  p <- v(list(designation = c("A1", "B21", "B22", "B23"), top_cm = c(0, 10, 40, 70), bottom_cm = c(10, 40, 70, NA),
              na_cmol = c(1, 1, 10, 10)))
  expect_true(soilKey:::solum_part_asc(p, "hypersodic", "lower", "some")$passed)
  p <- v(list(designation = c("A1", "B21", "B22", "B23"), top_cm = c(0, 10, 50, 100), bottom_cm = c(10, 50, 100, NA)))
  expect_false(soilKey:::solum_part_asc(p, "acid", "lower", "major")$passed)
  # a calcrete pan is substrate, not solum
  p <- v(list(designation = c("A1", "B21", "B22", "B2km"), top_cm = c(0, 10, 30, 45), bottom_cm = c(10, 30, 45, 120),
              ph_h2o = c(6, 6, 6, NA), pan_type = c(NA, NA, NA, "calcrete")))
  expect_false(soilKey:::solum_part_asc(p, "calcareous", "lower", "major")$passed)
  # Episodic: overlapping rows counted once
  p <- v(list(designation = c("A11", "A12", "B2"), top_cm = c(0, 0, 10), bottom_cm = c(10, 10, 80), na_cmol = c(1.6, 1.6, 1)))
  expect_false(soilKey:::episodic_asc(p)$passed)
  # the major part of the B with no B designated: every row that may be one fails
  d <- data.frame(designation = c("A1", NA, "C"), top_cm = c(0, 10, 50), bottom_cm = c(10, 50, 100), ca_cmol = 10, mg_cmol = 5,
                  subplasticity = "none", stringsAsFactors = FALSE)
  expect_false(soilKey:::magnesic_asc(mk_s3(d), "b")$passed)
  expect_false(soilKey:::subplastic_asc(mk_s3(d), "b")$passed)
  # Supravescent: a moderate effervescence without a lab figure is not dominantly carbonate
  p <- ex_s3("calcarosols", list(effervescence_class = c("moderate", "strong", "strong", "very strong", "strong"),
                                 caco3_pct = NA_real_, carbonate_soft_pct = 0))
  expect_false(soilKey:::calsilic_gg_asc(p, "supravescent")$passed)
})

test_that("a thin ironpan band, sandy material under peat, a residual A1", {
  skip_on_cran()
  # an uncemented B with a thin ironpan band keeps its own strength
  p <- ex_s3("podosols", list(designation = c("A1", "A2e", "B21hs", "B22s", "C"), pan_type = c(NA, NA, NA, "thin ironpan", NA),
                              strength_moist = c("loose", "loose", "firm", "very firm", "loose")))
  expect_true(soilKey:::podosol_coherence_asc(p, "strong")$passed)
  expect_false(soilKey:::podosol_coherence_asc(p, "weak")$passed)
  org <- function(d3, extra = list()) {
    d <- data.frame(designation = c("P1", "P2", d3), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
                    field_texture = c("HP", "AP", "LC"), oc_pct = c(38, 30, 1.5), munsell_value_moist = c(2, 2, 4),
                    munsell_chroma_moist = c(1, 1, 2), stringsAsFactors = FALSE)
    for (k in names(extra)) d[[k]] <- extra[[k]]
    mk_s3(d, list(drainage_class = "very poorly drained"))
  }
  # sandy material is not marl
  expect_false(soilKey:::organosol_sg_asc(org("C", list(field_texture = c("HP", "AP", "S"))), "marly")$passed)
  # an A1 of residual origin is soil material, not saprolite
  expect_false(soilKey:::organosol_sg_asc(org("A1", list(layer_origin = c(NA, NA, "residual"))), "paralithic")$passed)
})

test_that("the solum with no designated horizon, many rows of unknown role, horizons of unknown depth", {
  skip_on_cran()
  v <- function(des, top, bot, ph = 7, na = 1) mk_s3(data.frame(designation = des, top_cm = top, bottom_cm = bot, field_texture = "MC",
                                                               clay_pct = 55, ph_h2o = ph, cec_cmol = 40, na_cmol = na,
                                                               stringsAsFactors = FALSE))
  # no designated solum horizon: the solum may be the first row alone
  p <- v(c(NA, NA, "C"), c(0, 20, 200), c(20, 200, 250), ph = c(4.5, 7, 7))
  expect_true(is.na(soilKey:::solum_part_asc(p, "acid", "upper", "major")$passed))
  # many rows of unknown role below the solum are all tried
  p <- v(c("A1", "B21", rep(NA, 9)), c(0, 10, seq(30, 100, 10), 110), c(10, 30, seq(40, 110, 10), 300),
         ph = c(rep(7, 10), 4.5))
  expect_true(is.na(soilKey:::solum_part_asc(p, "acid", "lower", "major")$passed))
  # a horizon of unknown depth is no thicker than the room down to the next one
  p <- v(c("A1", "B21", "B22", "C"), c(0, 10, 60, 150), c(10, NA, 150, 200), ph = c(7, 7, 4.5, 7))
  expect_true(soilKey:::solum_part_asc(p, "acid", "lower", "major")$passed)
  # Episodic: a surface horizon of unknown depth may be thin
  p <- v(c("A1", "B21"), c(0, 8), c(NA, 30), na = c(4, 1))
  expect_true(is.na(soilKey:::episodic_asc(p)$passed))
})

test_that("two horizons of unknown depth may be one thin and the other thick", {
  skip_on_cran()
  p <- mk_s3(data.frame(designation = c("A1", "B21", NA), top_cm = c(0, 10, 30), bottom_cm = c(NA, 30, NA),
                        field_texture = "MC", clay_pct = 55, ph_h2o = c(7, 7, NA), stringsAsFactors = FALSE))
  expect_true(is.na(soilKey:::solum_part_asc(p, "acid", "upper", "major")$passed))
})
