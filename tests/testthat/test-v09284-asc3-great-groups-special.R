# v0.9.284: great groups of the Australian Soil Classification for the
# Vertosols (surface), Calcarosols (what the soil overlies, the B),
# Podosols (kinds of Podosol B) and Organosols (Folic, acid sulfate
# materials, carbonate and acidity).

.mk_g2 <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "AU", designation_system = "asc",
                                 drainage_class = "well drained"), site)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
nm2 <- function(p) classify_asc3(p, on_missing = "silent")$name

test_that("the examples key to a decided great group", {
  skip_on_cran()
  want <- c(vertosols = "Self-mulching, Black Vertosol", calcarosols = "Regolithic, Hypercalcic Calcarosol",
            podosols = "Humosesquic/Sesquic, Aeric Podosol", organosols = "Acidic, Sapric Organosol")
  for (o in names(want)) {
    r <- classify_asc3(make_asc3_example(o), on_missing = "silent")
    expect_identical(r$name, want[[o]], info = o)
    expect_length(r$provisional_because, 0L)
  }
})

test_that("Vertosol great groups from the surface and the A horizon", {
  skip_on_cran()
  ve <- function(surf = NA, a = list()) {
    p <- make_asc3_example("vertosols")
    p$site$surface_condition <- surf
    for (k in names(a)) p$horizons[[k]][1] <- a[[k]]
    p
  }
  expect_identical(nm2(ve("cracking; self-mulching")), "Self-mulching, Black Vertosol")
  expect_identical(nm2(ve("cracking", list(structure_type = "subangular blocky", structure_grade = "moderate"))),
                   "Epipedal, Black Vertosol")
  # a self-mulching surface the record does not show is not self-mulching
  expect_identical(nm2(ve(NA, list(structure_type = "polyhedral", structure_grade = "strong"))),
                   "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("surface crust; cracking", list(structure_type = "angular blocky", structure_grade = "strong"))),
                   "Crusty, Black Vertosol")
  expect_identical(nm2(ve("hard setting", list(structure_type = "massive", structure_grade = "structureless"))),
                   "Massive, Black Vertosol")
  # self-mulching is tested first
  expect_identical(nm2(ve("surface crust; self-mulching")), "Self-mulching, Black Vertosol")
  # a strong granular A is neither pedal-blocky nor massive: no great group
  expect_identical(nm2(ve("cracking")), "Black Vertosol")
})

test_that("a thin massive or weak surface horizon is a surface crusty horizon", {
  skip_on_cran()
  vh <- function(des, top, bot, grade, type, surf = "cracking; hard setting") {
    n <- length(des)
    .mk_g2(list(designation = des, top_cm = top, bottom_cm = bot, field_texture = "MC", clay_pct = 55,
                slickensides = c(rep("absent", n - 2L), "common", "many"), structure_type = type,
                structure_grade = grade, cracks_width_cm = c(1.5, rep(NA, n - 1L)), ph_h2o = 8,
                munsell_hue_moist = "10YR", munsell_value_moist = 2, munsell_chroma_moist = 1),
           list(surface_condition = surf))
  }
  four <- list(c("A11", "A12", "B21", "B22"), c(0, 2, 10, 50), c(2, 10, 50, 100))
  crusty <- function(g, t) do.call(vh, c(four, list(g, t)))
  # 2 cm of weak platy A11 over a moderate blocky A12: Crusty
  expect_identical(nm2(crusty(c("weak", "moderate", "strong", "strong"),
                              c("platy", "angular blocky", "lenticular", "lenticular"))), "Crusty, Black Vertosol")
  # the same over a weak A12: no pedal clay under the crust, and not Massive
  expect_identical(nm2(crusty(c("structureless", "weak", "strong", "strong"),
                              c("massive", "subangular blocky", "lenticular", "lenticular"))), "Black Vertosol")
  # a thin surface horizon that is itself pedal is no crust: Epipedal
  expect_identical(nm2(crusty(c("moderate", "strong", "strong", "strong"),
                              c("subangular blocky", "angular blocky", "lenticular", "lenticular"))),
                   "Epipedal, Black Vertosol")
  # an AB at the surface is the A horizon
  ab <- function(t) vh(c("AB", "B21", "B22"), c(0, 10, 50), c(10, 50, 100), c("strong", "strong", "strong"),
                       c(t, "lenticular", "lenticular"), surf = "cracking")
  expect_identical(nm2(ab("polyhedral")), "Epipedal, Black Vertosol")
  expect_identical(nm2(ab("granular")), "Black Vertosol")
})

.calc2 <- function(h = list()) .mk_g2(utils::modifyList(list(
  designation = c("A1", "B21k", "B22k", "C"), top_cm = c(0, 12, 40, 80), bottom_cm = c(12, 40, 80, 120),
  field_texture = c("SL", "SCL", "SCL", "SL"), clay_pct = c(14, 24, 26, 15),
  effervescence_class = c("slight", "strong", "strong", "strong"), secondary_carbonates_pct = c(0, 15, 30, 5),
  caco3_pct = c(2, 12, 25, 10), carbonate_soft_pct = c(0, 12, 22, 5), carbonate_hard_pct = c(0, 3, 5, 0),
  caso4_pct = 0, boundary_distinctness = c("gradual", "gradual", "clear", NA),
  structure_grade = c("weak", "weak", "weak", "structureless"), layer_origin = c(NA, NA, NA, "aeolian"),
  ph_h2o = c(8.2, 8.6, 8.8, 9.0)), h))

test_that("Calcarosol great groups: what the soil directly overlies, and the B", {
  skip_on_cran()
  cg <- function(p, k) soilKey:::calcarosol_gg_asc(p, k)$passed
  expect_identical(nm2(.calc2()), "Regolithic, Hypercalcic Calcarosol")
  expect_identical(nm2(.calc2(list(designation = c("A1", "B21k", "B22k", "R"), layer_origin = NA))),
                   "Lithic, Hypercalcic Calcarosol")
  expect_true(cg(.calc2(list(layer_origin = c(NA, NA, NA, "saprolite"))), "paralithic"))
  expect_true(cg(.calc2(list(layer_origin = c(NA, NA, NA, "lacustrine marl"))), "marly"))
  # a C of unrecorded origin: undecided among them
  expect_true(is.na(cg(.calc2(list(layer_origin = NA)), "regolithic")))
  # a description that stops at the base of the solum
  p <- .mk_g2(list(designation = c("A1", "B21k"), top_cm = c(0, 12), bottom_cm = c(12, 60), field_texture = "SCL",
                   clay_pct = 24, effervescence_class = "strong", caco3_pct = 20, ph_h2o = 8.5))
  expect_true(is.na(cg(p, "lithic"))); expect_false(cg(p, "duric"))
  # pans, a Rendic A over a Bk of soft carbonate, lamellae, a pedal B
  expect_true(cg(.calc2(list(designation = c("A1", "B21k", "B22k", "Ckm"), pan_type = c(NA, NA, NA, "calcrete"),
                             cementation_class = c(NA, NA, NA, "strongly"))), "petrocalcic"))
  expect_true(cg(.calc2(list(carbonate_soft_pct = c(0, 85, 22, 5), caco3_pct = c(2, 85, 25, 10))), "rendic"))
  expect_false(cg(.calc2(), "rendic"))
  expect_true(cg(.calc2(list(designation = c("A1", "B21k", "B2 lamellae", "C"))), "argic"))
  expect_true(cg(.calc2(list(structure_grade = c("weak", "moderate", "strong", "structureless"))), "pedal"))
})

.podo2 <- function(des, site = list(), h = list()) .mk_g2(utils::modifyList(list(
  designation = c("A1", "A2e", des, "C"), top_cm = c(0, 10, 45, 60, 90), bottom_cm = c(10, 45, 60, 90, 130),
  field_texture = c("S", "S", "LS", "S", "S"), clay_pct = c(3, 2, 5, 3, 2), oc_pct = c(2, 0.3, 1.8, 0.6, 0.1),
  ph_h2o = 5), h), utils::modifyList(list(drainage_class = "rapidly drained"), site))

test_that("Podosol great groups from the kinds of Podosol B", {
  skip_on_cran()
  expect_identical(nm2(.podo2(c("B21s", "B22s"))), "Sesquic, Aeric Podosol")
  expect_identical(nm2(.podo2(c("B2hs", "B2s"))), "Humosesquic/Sesquic, Aeric Podosol")
  expect_identical(nm2(.podo2(c("B21hs", "B22hs"))), "Humosesquic, Aeric Podosol")
  semi <- list(drainage_class = "imperfectly drained")
  expect_identical(nm2(.podo2(c("B21h", "B22h"), semi)), "Humic, Semiaquic Podosol")
  expect_identical(nm2(.podo2(c("B2h", "B2s"), semi)), "Humic/Sesquic, Semiaquic Podosol")
  # a cemented B directly under the Bh: a Basi horizon
  p <- .podo2(c("B2h", "B3m"), semi, list(cementation_class = c(NA, NA, NA, "strongly", NA)))
  expect_identical(nm2(p), "Humic/Alsilic, Semiaquic Podosol")
  # a tongued lower boundary of the A2 may be pipey
  p <- .podo2(c("B21s", "B22s"), h = list(boundary_topography = c(NA, "tongued", NA, NA, NA)))
  r <- classify_asc3(p, on_missing = "silent")
  expect_true("great_groups:EB" %in% r$provisional_because)
})

.peat2 <- function(h = list(), site = list()) .mk_g2(utils::modifyList(list(
  designation = c("P1", "P2", "C"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 150),
  field_texture = c("HP", "AP", "LC"), clay_pct = c(NA, NA, 38), oc_pct = c(38, 30, 1.5),
  sulfidic_s_pct = c(0.005, 0.005, 0.002), ph_h2o = c(4.8, 5.0, 5.5)), h),
  utils::modifyList(list(drainage_class = "very poorly drained"), site))

test_that("Organosol great groups", {
  skip_on_cran()
  og <- function(p, k) soilKey:::organosol_gg_asc(p, k)$passed
  expect_identical(nm2(.peat2()), "Acidic, Sapric Organosol")
  expect_identical(nm2(.peat2(list(ph_h2o = c(6.2, 6.5, 6.5)))), "Basic, Sapric Organosol")
  expect_identical(nm2(.peat2(list(ph_h2o = 7.8, effervescence_class = "moderate"))), "Calcareous, Sapric Organosol")
  # sulfidic S of 0.01 % or more within 1.5 m
  expect_identical(nm2(.peat2(list(sulfidic_s_pct = c(0.005, 0.2, 0.1)))), "Sulfidic, Sapric Organosol")
  # sulfidic S not measured: undecided
  r <- classify_asc3(.peat2(list(sulfidic_s_pct = NA)), on_missing = "silent")
  expect_true("great_groups:EU" %in% r$provisional_because)
  # hypersulfidic by incubation
  p <- .peat2(list(sulfidic_s_pct = c(0.005, 0.2, 0.1), incubation_ph = c(4.6, 3.2, 3.5), ph_h2o = c(4.8, 5.0, 5.5)))
  expect_identical(nm2(p), "Hypersulfidic, Sapric Organosol")
  # sulfuric material at least 15 cm thick within 1.5 m (jarosite)
  p <- .peat2(list(ph_h2o = c(3.6, 3.5, 5.5), jarosite_present = c(TRUE, TRUE, NA)))
  expect_true(og(p, "sulfuric"))
  # Folic: freely drained, organic materials on rock (Definition (ii))
  p <- .mk_g2(list(designation = c("O2", "R"), top_cm = c(0, 15), bottom_cm = c(15, 40), oc_pct = c(30, NA),
                   field_texture = c("AP", NA), sulfidic_s_pct = c(0.002, NA), ph_h2o = c(5, NA)))
  expect_true(og(p, "folic"))
  expect_false(og(.peat2(), "folic"))
})

test_that("the surface of a Vertosol: the record read item by item, a peat surface, massive or weak blocky", {
  skip_on_cran()
  ve <- function(surf = NA, a = list()) {
    p <- make_asc3_example("vertosols"); p$site$surface_condition <- surf
    for (k in names(a)) p$horizons[[k]][1] <- a[[k]]
    p
  }
  blocky <- list(structure_type = "subangular blocky", structure_grade = "moderate")
  # only a moderately or strongly self-mulching surface is Self-mulching
  expect_identical(nm2(ve("cracking; weakly self-mulching", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("cracking; not self-mulching", blocky)), "Epipedal, Black Vertosol")
  # a crust the record denies, and a real crust beside a salt crust
  expect_identical(nm2(ve("cracking; no surface crust", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("cracking; non-crusting", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("surface crust; salt crust", blocky)), "Crusty, Black Vertosol")
  # "massive or weak blocky": a weak granular A is neither
  expect_identical(nm2(ve("cracking", list(structure_type = "subangular blocky", structure_grade = "weak"))),
                   "Massive, Black Vertosol")
  expect_identical(nm2(ve("cracking", list(structure_type = "granular", structure_grade = "weak"))), "Black Vertosol")
  # a peat at the surface has no pedal or massive surface A
  vs <- function(p, k) soilKey:::vertosol_surface_asc(p, k)$passed
  pt <- .mk_g2(list(designation = c("P1", "A1", "B21", "B22"), top_cm = c(0, 25, 35, 70), bottom_cm = c(25, 35, 70, 120),
                    field_texture = c("HP", "MC", "MC", "MC"), clay_pct = c(NA, 55, 58, 60), oc_pct = c(30, 2, 1, 0.8),
                    structure_type = c(NA, "angular blocky", "lenticular", "lenticular"),
                    structure_grade = c(NA, "strong", "strong", "strong")), list(surface_condition = "cracking"))
  expect_false(vs(pt, "epipedal")); expect_false(vs(pt, "massive")); expect_false(vs(pt, "crusty"))
  # a thin organic layer (not a peaty horizon) is passed over: the A below it is the surface
  pt$horizons$bottom_cm[1] <- 5; pt$horizons$top_cm[2] <- 5
  expect_true(vs(pt, "epipedal")); expect_false(vs(pt, "peaty"))
  # a P short of organic material is passed over too
  pt$horizons$bottom_cm[1] <- 25; pt$horizons$top_cm[2] <- 25; pt$horizons$oc_pct[1] <- 15
  expect_true(vs(pt, "epipedal")); expect_false(vs(pt, "peaty"))
  # more negations, and a qualifier after a comma
  expect_identical(nm2(ve("cracking; surface crust absent", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("cracking; uncrusted", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("cracking; self-mulching, weak", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("cracking; self-mulching, weakly developed", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("cracking; self-mulching, weakly-developed", blocky)), "Epipedal, Black Vertosol")
  expect_identical(nm2(ve("cracking; self-mulching, weakly to moderately", blocky)), "Epipedal, Black Vertosol")
  # an A1 of measured peat-level organic C is a peaty horizon, not a mineral A
  pk <- ve("cracking", list(structure_type = "angular blocky", structure_grade = "strong"))
  pk$horizons$oc_pct <- c(25, 1, 0.8, 0.5); pk$horizons$bottom_cm[1] <- 25; pk$horizons$top_cm[2] <- 25
  expect_false(vs(pk, "epipedal"))
  # a structureless type is massive
  expect_true(vs(ve("cracking", list(structure_type = "structureless", structure_grade = "structureless")), "massive"))
  # an A of granular peds is not massive or weak blocky, whatever its unrecorded grade
  expect_false(vs(ve("cracking", list(structure_type = "granular", structure_grade = NA)), "massive"))
  # no A horizon at the top: no pedal or massive A
  nb <- .mk_g2(list(designation = c("B21", "B22"), top_cm = c(0, 40), bottom_cm = c(40, 100), field_texture = "MC",
                    structure_type = "angular blocky", structure_grade = "strong"), list(surface_condition = "cracking"))
  expect_false(vs(nb, "epipedal")); expect_false(vs(nb, "massive"))
})

test_that("Calcarosols: lamellae in the B, rows of unknown role, pans and gaps", {
  skip_on_cran()
  cg <- function(p, k) soilKey:::calcarosol_gg_asc(p, k)$passed
  # lamellae outside the B are not an argic horizon within it
  expect_false(cg(.calc2(list(designation = c("A1", "B21k", "B22k", "C lamellae"))), "argic"))
  expect_false(cg(.calc2(list(designation = c("A2 lamellae", "B21k", "B22k", "C"))), "argic"))
  # an undesignated row at the base of the solum may be a B: what lies under the soil is undecided
  five <- function(h) .mk_g2(utils::modifyList(list(
    designation = c("A1", "B21k", "B22k", NA, "R"), top_cm = c(0, 12, 40, 80, 110), bottom_cm = c(12, 40, 80, 110, 130),
    field_texture = c("SL", "SCL", "SCL", "SL", NA), clay_pct = c(14, 24, 26, 15, NA),
    effervescence_class = c("slight", "strong", "strong", "strong", NA), caco3_pct = c(2, 12, 25, 10, NA),
    carbonate_soft_pct = c(0, 12, 22, 5, NA), structure_grade = c("weak", "weak", "weak", "weak", NA),
    layer_origin = c(NA, NA, NA, "aeolian", NA), ph_h2o = c(8.2, 8.6, 8.8, 9.0, NA)), h))
  p <- five(list())
  expect_true(is.na(cg(p, "lithic"))); expect_true(is.na(cg(p, "regolithic")))
  # a weak undesignated row that may be most of the B leaves Pedal undecided
  p <- five(list(designation = c("A1", "B21k", NA, "C", "R"), top_cm = c(0, 12, 30, 80, 110), bottom_cm = c(12, 30, 80, 110, 130),
                 structure_grade = c("weak", "moderate", "weak", "weak", NA)))
  expect_true(is.na(cg(p, "pedal")))
  # a silcrete pan under the solum is none of the four substrates: no great group, not provisional on them
  p <- .calc2(list(designation = c("A1", "B21k", "B22k", "Cqm"), pan_type = c(NA, NA, NA, "silcrete"),
                   cementation_class = c(NA, NA, NA, "strongly"), layer_origin = NA))
  for (k in c("lithic", "paralithic", "marly", "regolithic")) expect_false(cg(p, k), info = k)
  # a calcrete pan recorded after a gap may lie directly under the soil
  p <- .calc2(list(designation = c("A1", "B21k", "B22k", "Ckm"), top_cm = c(0, 12, 40, 85), bottom_cm = c(12, 40, 80, 120),
                   pan_type = c(NA, NA, NA, "calcrete"), cementation_class = c(NA, NA, NA, "strongly"), layer_origin = NA))
  expect_true(is.na(cg(p, "petrocalcic")))
  # an undesignated row of soft carbonate under the A may be a Bk
  p <- .calc2(list(designation = c("A1", NA, NA, NA), carbonate_soft_pct = c(0, 85, 22, 5), caco3_pct = c(2, 85, 25, 10)))
  expect_true(is.na(cg(p, "rendic")))
  # a cemented layer that is no pan is not unconsolidated; an uncemented pan is no pan
  expect_false(cg(.calc2(list(cementation_class = c(NA, NA, NA, "strongly"), pan_type = c(NA, NA, NA, "none"))),
                  "regolithic"))
  expect_true(cg(.calc2(list(pan_type = c(NA, NA, NA, "densipan"))), "regolithic"))
  # a lamellae horizon under the C is not within the B
  p <- .mk_g2(list(designation = c("A1", "B21k", "B22k", "C", "lamellae"), top_cm = c(0, 12, 40, 80, 120),
                   bottom_cm = c(12, 40, 80, 120, 140), field_texture = "SCL", clay_pct = 24,
                   effervescence_class = "strong", caco3_pct = 20, ph_h2o = 8.5))
  expect_false(cg(p, "argic"))
  # a lamellae horizon in the B is part of the solum: what lies under it is the substrate
  p <- .mk_g2(list(designation = c("A1", "B21k", "lamellae", "R"), top_cm = c(0, 12, 40, 80), bottom_cm = c(12, 40, 80, 100),
                   field_texture = c("SL", "SCL", "SL", NA), clay_pct = c(14, 24, 12, NA),
                   effervescence_class = c("slight", "strong", "strong", NA), caco3_pct = c(2, 20, 15, NA), ph_h2o = 8.5))
  expect_true(cg(p, "argic")); expect_true(cg(p, "lithic"))
  # solimovic (colluvial) material is unconsolidated
  expect_true(cg(.calc2(list(layer_origin = c(NA, NA, NA, "solimovic"))), "regolithic"))
})

test_that("Podosols: a missing depth and rows of unknown role", {
  skip_on_cran()
  pg <- function(p, k) soilKey:::podosol_gg_asc(p, k)$passed
  semi <- list(drainage_class = "imperfectly drained")
  # a cemented B of unknown top under a Bh: only Humic and Humic/Alsilic stay open
  p <- .podo2(c("B2h", "B3m"), semi, list(top_cm = c(0, 10, 45, NA, 90), cementation_class = c(NA, NA, NA, "strongly", NA)))
  expect_true(is.na(pg(p, "humic"))); expect_true(is.na(pg(p, "humic_alsilic")))
  expect_false(pg(p, "sesquic")); expect_false(pg(p, "humic_sesquic"))
  # an undesignated row under a Bhs may be a Bs
  p <- .podo2(c("B2hs", NA), h = list())
  expect_true(is.na(pg(p, "humosesquic"))); expect_true(is.na(pg(p, "humosesquic_sesquic")))
  expect_false(pg(p, "humic"))
  # an undesignated row between a Bh and a cemented B may be a Bh over a Basi
  p <- .mk_g2(list(designation = c("A1", "A2e", "B21h", NA, "B3m", "C"), top_cm = c(0, 10, 45, 55, 65, 90),
                   bottom_cm = c(10, 45, 55, 65, 90, 130), field_texture = "S", oc_pct = c(2, 0.3, 1.8, 1, 0.5, 0.1),
                   cementation_class = c(NA, NA, NA, NA, "strongly", NA), ph_h2o = 5), semi)
  expect_true(is.na(pg(p, "humic_alsilic")))
})

test_that("Organosols: undecided organic rows, hyposulfidic, the upper 1.5 m", {
  skip_on_cran()
  og <- function(p, k) soilKey:::organosol_gg_asc(p, k)$passed
  # an organic row of undecided status that could change the major part
  p <- .mk_g2(list(designation = c("P1", "P2", "C"), top_cm = c(0, 50, 120), bottom_cm = c(50, 120, 150),
                   field_texture = c("HP", NA, "LC"), clay_pct = c(NA, NA, 40), oc_pct = c(38, 15, 1.5),
                   ph_h2o = c(4.5, 6.5, 6.5), sulfidic_s_pct = 0.002, effervescence_class = "none"),
              list(drainage_class = "very poorly drained"))
  expect_true(is.na(og(p, "acidic"))); expect_true(is.na(og(p, "basic")))
  # a drop of less than 0.5 unit to just below 4 is hyposulfidic
  p <- .peat2(list(designation = c("P1", "P2", "Cg"), sulfidic_s_pct = c(0.005, 0.005, 0.3), incubation_ph = c(NA, NA, 3.9),
                   ph_h2o = c(4.8, 5.0, 4.2)))
  expect_identical(nm2(p), "Hyposulfidic, Sapric Organosol")
  # without a field pH, hyposulfidic is undecided
  p$horizons$ph_h2o[3] <- NA; p$horizons$incubation_ph[3] <- 6.5
  expect_true(is.na(og(p, "hyposulfidic")))
  # a sulfuric run that starts above 1.5 m may go on below it
  p <- .mk_g2(list(designation = c("P1", "P2", "Cg1", "Cg2", "Cg3"), top_cm = c(0, 40, 90, 140, 150),
                   bottom_cm = c(40, 90, 140, 150, 170), field_texture = c("HP", "AP", "LC", "LC", "LC"),
                   clay_pct = c(NA, NA, 40, 40, 40), oc_pct = c(38, 30, 2, 2, 2), ph_h2o = c(4.8, 5.0, 5.5, 3.5, 3.5),
                   jarosite_present = c(NA, NA, FALSE, TRUE, TRUE), sulfidic_s_pct = c(0.005, 0.005, 0.005, 0.2, 0.2)),
              list(drainage_class = "very poorly drained"))
  expect_true(og(p, "sulfuric"))
  # buried horizons are part of the upper 1.5 m
  p <- .mk_g2(list(designation = c("P1", "P2", "2A1b", "2Bb"), top_cm = c(0, 40, 90, 110), bottom_cm = c(40, 90, 110, 150),
                   field_texture = c("HP", "AP", "LC", "LC"), clay_pct = c(NA, NA, 40, 40), oc_pct = c(38, 30, 3, 1),
                   ph_h2o = c(4.8, 5.0, 6.0, 6.5), sulfidic_s_pct = c(0.005, 0.005, 0.5, 0.6)),
              list(drainage_class = "very poorly drained"))
  expect_true(og(p, "sulfidic"))
  # a description that stops above 1.5 m leaves sulfuric and sulfidic material open below it
  p <- .peat2(list(bottom_cm = c(25, 70, 110)))
  expect_true(is.na(og(p, "sulfuric"))); expect_true(is.na(og(p, "sulfidic")))
  expect_false(og(p, "hypersulfidic"))
  # and so does a gap in the record within it
  p <- .peat2(list(top_cm = c(0, 40, 110), bottom_cm = c(40, 90, 160)))
  expect_true(is.na(og(p, "sulfidic")))
  # a description that ends on rock is complete, and the rock is not tested
  p <- .peat2(list(designation = c("P1", "P2", "R"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 90),
                   sulfidic_s_pct = c(0.005, 0.005, NA), ph_h2o = c(4.8, 5.0, NA), field_texture = c("HP", "AP", NA)))
  expect_false(og(p, "sulfidic")); expect_false(og(p, "sulfuric"))
  # but a gap above the rock is still not described
  p$horizons$top_cm[2] <- 35
  expect_true(is.na(og(p, "sulfidic")))
})
