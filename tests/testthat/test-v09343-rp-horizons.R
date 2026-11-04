# v0.9.343: the reference horizons of the French Referentiel pedologique 2008
# identified on the recorded data (R/diagnostics-rp-horizons.R,
# R/rp-readers.R), Hugo's decisions of 2026-11-02 (Q10-Q13, Q17-Q19 and the
# defaults). One hand-written undesignated layer per code (or per family where
# codes share criteria), read by the fixed order of tests; the undecided
# steps and their candidates, the notation against the data, the dominance
# of G, the depth datum, the intertropical and andosol gates, the coarse
# elements by weight and their volume band, the WRB stand-ins.

# --- builders --------------------------------------------------------------------

# One layer: a plain, structureless, slightly acid mineral layer unless told.
.fr_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_moist = "10YR", munsell_value_moist = 5,
            munsell_chroma_moist = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 40, sand_pct = 40, oc_pct = 0.1, ph_h2o = 6.0,
            clay_films_amount = "none", effervescence_class = "none", caco3_pct = 0)
  a <- list(...)
  d[names(a)] <- a
  d
}
# A profile of layers; a temperate French site (Cfb, 46 N), forest, well
# drained, unless told.
.fr_P <- function(..., site = list()) {
  s <- list(id = "fr", lat = 46, lon = 2, koppen_climate = "Cfb", land_use = "forest",
            drainage_class = "well drained")
  s[names(site)] <- site
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = s, horizons = h)
}
# A surface A (dark, humic, acid, structured) and a structured subsoil S.
.fr_A <- function(top = 0, bottom = 20, ...) .fr_L(top, bottom, munsell_value_moist = 4, munsell_chroma_moist = 3,
                                                   structure_type = "subangular blocky", structure_grade = "moderate",
                                                   oc_pct = 2, ph_h2o = 5.0, bs_pct = 30, ...)
.fr_S <- function(top, bottom, ...) .fr_L(top, bottom, munsell_hue_moist = "7.5YR", munsell_value_moist = 5,
                                          munsell_chroma_moist = 6, structure_type = "subangular blocky",
                                          structure_grade = "moderate", oc_pct = 0.4, ph_h2o = 5.5, bs_pct = 30, ...)
.fr_C <- function(top, bottom, ...) .fr_L(top, bottom, ...)
# The frame row of a layer (by its top).
.fr_row <- function(fr, top) fr[fr$top_cm == top, , drop = FALSE]
.fr_code <- function(p, top) .fr_row(.rp_identify(p), top)$code


# --- the frame ---------------------------------------------------------------------

test_that("the frame has the columns of identify_horizons() and the RP columns", {
  skip_on_cran()
  fr <- .rp_identify(.fr_P(.fr_A(0, 20), .fr_S(20, 60), .fr_C(60, 120)))
  expect_true(all(c("horizon_idx", "top_cm", "bottom_cm", "designation", "code", "overlay", "features",
                    "features_na", "route", "passed", "evidence", "missing", "candidates") %in% names(fr)))
  expect_true(all(c("family", "composed", "variants", "overlay_na", "imperfect", "data_code", "stand_ins",
                    "fallbacks", "lower_grade", "top_rp", "bottom_rp", "lith", "hint") %in% names(fr)))
  expect_identical(fr$code, c("A", "S", "C"))
  expect_identical(fr$route, rep("data", 3))
  expect_true(all(grepl("^\\[soilKey's order of tests\\]", fr$evidence)))
  # since the registry entry rp2008 (0.9.346), identify_horizons() reaches it
  expect_identical(identify_horizons(.fr_P(.fr_A(0, 20), .fr_S(20, 60), .fr_C(60, 120)), "rp2008")$code,
                   c("A", "S", "C"))
})


# --- step 1: layers and materials ------------------------------------------------------

test_that("layers and materials: R, D, P, TH, Z, Mli", {
  skip_on_cran()
  rock <- .fr_L(60, 100, clay_pct = NA, silt_pct = NA, sand_pct = NA, oc_pct = NA, structure_type = NA,
                structure_grade = NA, rupture_resistance = "extremely hard")
  expect_identical(.fr_code(.fr_P(.fr_A(), .fr_S(20, 60), rock), 60), "R")
  # the FAO R gives it too, as a hint read by the data route
  fr <- .rp_identify(.fr_P(.fr_A(designation = "Ah"), .fr_S(20, 60, designation = "Bw"),
                           .fr_L(60, 100, designation = "R")))
  expect_identical(.fr_row(fr, 60)$code, "R")
  expect_true("designation_hint" %in% .fr_row(fr, 60)$fallbacks[[1]])
  # D: coarse elements over 60 % by weight with no pedogenic sign
  scree <- .fr_L(60, 100, coarse_fragments_wt_pct = 80, oc_pct = 0.05, roots_abundance = "none")
  expect_identical(.fr_code(.fr_P(.fr_A(), .fr_S(20, 60), scree), 60), "D")
  # P: below 0 C
  expect_identical(.fr_code(.fr_P(.fr_A(), .fr_S(20, 60), .fr_L(60, 100, permafrost_temp_C = -2)), 60), "P")
  # TH: 0.75 % S or more, waterlogged
  th <- .fr_L(30, 80, s_total_pct = 1.2, water_saturation_days = 365, munsell_hue_moist = "5GY",
              munsell_value_moist = 4, munsell_chroma_moist = 1)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 30), th), 30), "TH")
  # Z and its kinds
  expect_identical(.fr_code(.fr_P(.fr_A(), .fr_L(20, 60, anthropic_material = "technological")), 20), "Ztc")
  expect_identical(.fr_code(.fr_P(.fr_A(), .fr_L(20, 60, artefacts_pct = 70)), 20), "Z")
  # Mli: a limnic layer in a profile with holorganic layers
  peat <- .fr_L(0, 40, oc_pct = 45, clay_pct = NA, silt_pct = NA, sand_pct = NA, water_saturation_days = 300,
                fiber_content_rubbed_pct = 50, structure_type = NA, structure_grade = NA)
  expect_identical(.fr_code(.fr_P(peat, .fr_L(40, 80, layer_origin = "limnic")), 40), "Mli")
})


# --- step 2: holorganic (Q19) -----------------------------------------------------------------

test_that("H and its kinds by the fibres or von Post, O and its kinds by the fine organic matter", {
  skip_on_cran()
  h <- function(...) .fr_L(0, 40, oc_pct = 45, clay_pct = NA, silt_pct = NA, sand_pct = NA,
                           structure_type = NA, structure_grade = NA, ...)
  wet <- list(drainage_class = "very poorly drained")
  expect_identical(.fr_code(.fr_P(h(fiber_content_rubbed_pct = 50), .fr_C(40, 80), site = wet), 0), "Hf")
  expect_identical(.fr_code(.fr_P(h(fiber_content_rubbed_pct = 20), .fr_C(40, 80), site = wet), 0), "Hm")
  expect_identical(.fr_code(.fr_P(h(fiber_content_rubbed_pct = 5), .fr_C(40, 80), site = wet), 0), "Hs")
  r <- .fr_row(.rp_identify(.fr_P(h(von_post_index = 5), .fr_C(40, 80), site = wet)), 0)
  expect_identical(r$code, "H")
  expect_identical(r$variants[[1]], c("Hf", "Hm"))
  # ash by the loss on ignition: 40 % LOI is 60 % ash, not holorganic
  expect_false(identical(.fr_code(.fr_P(h(loi_pct = 40, oc_pct = NA), .fr_C(40, 80), site = wet), 0), "H"))
  # aerated: O, its kind by the fine organic matter
  o <- function(fom) .fr_L(0, 5, oc_pct = 40, clay_pct = NA, silt_pct = NA, sand_pct = NA, structure_type = NA,
                           structure_grade = NA, water_saturation_days = 0, fine_organic_matter_pct = fom)
  expect_identical(.fr_code(.fr_P(o(5), .fr_A(5, 20)), 0), "OL")
  expect_identical(.fr_code(.fr_P(o(50), .fr_A(5, 20)), 0), "OFm")
  expect_identical(.fr_code(.fr_P(o(85), .fr_A(5, 20)), 0), "OHr")
  # no water regime recorded: H and O are both candidates (Q19)
  r <- .fr_row(.rp_identify(.fr_P(h(), .fr_C(40, 80), site = list(drainage_class = NA))), 0)
  expect_true(is.na(r$passed))
  expect_setequal(r$candidates[[1]], c("H", "O"))
  expect_true("water_saturation_days" %in% r$missing[[1]])
})


# --- step 3: coarse elements by weight (Q11) ----------------------------------------------------

test_that("X by weight, its kinds, and the band of the volume fallback", {
  skip_on_cran()
  x <- function(...) .fr_L(20, 60, oc_pct = 1, structure_type = "granular", structure_grade = "weak", ...)
  expect_identical(.fr_code(.fr_P(.fr_A(), x(coarse_fragments_wt_pct = 70, gravel_wt_pct = 60)), 20), "Xgr")
  expect_identical(.fr_code(.fr_P(.fr_A(), x(cobbles_wt_pct = 40, stones_wt_pct = 25)), 20), "Xc")
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(), x(coarse_fragments_wt_pct = 70))), 20)
  expect_identical(r$code, "X")
  expect_setequal(r$variants[[1]], c("Xgr", "Xc", "Xp"))
  # the volume: 65 % is at least 60 % by weight; 50 % with no bulk density is
  # undecided; 20 % is not peyric; 50 % with a bulk density of 1.3 converts
  r65 <- .fr_row(.rp_identify(.fr_P(.fr_A(), x(coarse_fragments_pct = 65))), 20)
  expect_identical(r65$code, "X")
  expect_true("weight_from_volume_band" %in% r65$fallbacks[[1]])
  r50 <- .fr_row(.rp_identify(.fr_P(.fr_A(), x(coarse_fragments_pct = 50))), 20)
  expect_true("X" %in% r50$candidates[[1]])
  expect_true("coarse_fragments_wt_pct" %in% r50$missing[[1]] || r50$code != "X")
  expect_false(isTRUE(.fr_row(.rp_identify(.fr_P(.fr_A(), x(coarse_fragments_pct = 20))), 20)$code == "X"))
  rbd <- .fr_row(.rp_identify(.fr_P(.fr_A(), x(coarse_fragments_pct = 50, bulk_density_g_cm3 = 1.3))), 20)
  expect_identical(rbd$code, "X")
  expect_true("weight_from_volume" %in% rbd$fallbacks[[1]])
})


# --- step 4: hydromorphy (Q13, Q17, default 18) ---------------------------------------------------

test_that("G dominates every other character; Gr, Go, AG; g as a feature", {
  skip_on_cran()
  bt_grey <- .fr_S(30, 80, clay_pct = 40, clay_films_amount = "many", munsell_hue_moist = "5GY",
                   munsell_value_moist = 5, munsell_chroma_moist = 1, redoximorphic_features_pct = 2)
  fr <- .rp_identify(.fr_P(.fr_A(0, 30, clay_pct = 15), bt_grey, .fr_C(80, 120)))
  r <- .fr_row(fr, 30)
  expect_identical(r$code, "Gr")
  expect_true("BT" %in% r$candidates[[1]])   # the BT reading goes to the candidates, never BTG
  # Go: segregations at the voids
  go <- .fr_S(30, 80, munsell_hue_moist = "2.5Y", munsell_value_moist = 6, munsell_chroma_moist = 1,
              redoximorphic_features_pct = 20, redox_segregations_location = "voids", water_saturation_days = 200)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 30), go, .fr_C(80, 120)), 30), "Go")
  # inside the peds: g, a feature of the S
  g <- .fr_S(30, 80, redoximorphic_features_pct = 20, redox_segregations_location = "within peds")
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 30), g, .fr_C(80, 120))), 30)
  expect_identical(r$code, "S")
  expect_true("g" %in% r$features[[1]])
  expect_identical(r$composed, "Sg")
  # recorded relict: not the functional -g (Q13)
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 30), .fr_S(30, 80, redoximorphic_features_pct = 20, redox_relict = TRUE),
                                  .fr_C(80, 120))), 30)
  expect_false("g" %in% r$features[[1]])
  # 2-5 % is only peu tachete (Q17)
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 30), .fr_S(30, 80, redoximorphic_features_pct = 4), .fr_C(80, 120))), 30)
  expect_false("g" %in% r$features[[1]])
  # an A with reductic characters is AG
  ag <- .fr_A(0, 20, munsell_hue_moist = "5GY", munsell_value_moist = 4, munsell_chroma_moist = 1, oc_pct = 3)
  expect_identical(.fr_code(.fr_P(ag, .fr_S(20, 60)), 0), "AG")
  # a grey matrix with mottles and no location: Go or g undecided
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 30), .fr_S(30, 80, munsell_hue_moist = "2.5Y", munsell_value_moist = 6,
                                                      munsell_chroma_moist = 1, redoximorphic_features_pct = 20,
                                                      water_saturation_days = 200),
                                  .fr_C(80, 120))), 30)
  expect_true("G" %in% r$candidates[[1]] || is.na(r$passed))
  expect_true("redox_segregations_location" %in% r$missing[[1]])
})


# --- step 5: accumulations ------------------------------------------------------------------------

test_that("U, K and its kinds, Si, Fe, Y and its kinds", {
  skip_on_cran()
  base <- function(l) .fr_P(.fr_A(0, 20), l, .fr_C(80, 120))
  expect_identical(.fr_code(base(.fr_S(20, 80, ph_h2o = 3.2, jarosite_present = TRUE)), 20), "U")
  expect_identical(.fr_code(base(.fr_S(20, 80, secondary_carbonates_pct = 25, caco3_pct = 30)), 20), "K")
  expect_identical(.fr_code(base(.fr_S(20, 80, secondary_carbonates_pct = 40, caco3_pct = 50,
                                       cementation_class = "indurated")), 20), "Km")
  expect_identical(.fr_code(base(.fr_S(20, 80, secondary_carbonates_pct = 40, caco3_pct = 50,
                                       cementation_class = "weakly")), 20), "Kc")
  expect_identical(.fr_code(base(.fr_S(20, 80, duripan_pct = 30)), 20), "Si")
  expect_identical(.fr_code(base(.fr_S(20, 80, duripan_pct = 60, cementation_class = "indurated",
                                       roots_abundance = "none")), 20), "Sim")
  expect_identical(.fr_code(base(.fr_S(20, 80, ferric_nodules_pct = 25)), 20), "Fe")
  expect_identical(.fr_code(base(.fr_S(20, 80, ferric_nodules_pct = 40, cementation_class = "indurated")), 20), "Fem")
  expect_identical(.fr_code(.fr_P(.fr_L(0, 30, caso4_pct = 50, caco3_pct = 5, oc_pct = 0.3), .fr_C(30, 90)), 0), "Ys")
  expect_identical(.fr_code(base(.fr_S(20, 80, caso4_pct = 30, caco3_pct = 10)), 20), "Yp")
})


# --- step 6: surface horizons -----------------------------------------------------------------

test_that("the A horizons: An, Ach, Aso, Aca, Ado, Aci, Amg, A, Av, Ae, Js", {
  skip_on_cran()
  sub <- function() list(.fr_S(30, 80), .fr_C(80, 120))
  one <- function(a) .fr_code(do.call(.fr_P, c(list(a), sub())), 0)
  bio <- list(biogenic_aggregates_pct = 50, biogenic_aggregate_size = "macro")
  expect_identical(one(.fr_A(0, 30, oc_pct = 12, munsell_value_moist = 2, munsell_chroma_moist = 1,
                             water_saturation_days = 250)), "An")
  expect_identical(one(do.call(.fr_A, c(list(0, 30, oc_pct = 4, munsell_value_moist = 2, munsell_chroma_moist = 1,
                                             ph_h2o = 7, bs_pct = 95, boundary_distinctness = "gradual"), bio))), "Ach")
  expect_identical(one(.fr_A(0, 30, oc_pct = 2.5, munsell_value_moist = 3, munsell_chroma_moist = 2,
                             ph_h2o = 6.0, bs_pct = 65)), "Aso")
  expect_identical(one(do.call(.fr_A, c(list(0, 30, oc_pct = 3, munsell_value_moist = 3, munsell_chroma_moist = 3,
                                             ph_h2o = 8.2, bs_pct = 100, caco3_pct = 20,
                                             effervescence_class = "strong", effervescence_extent = "continuous"), bio))), "Aca")
  expect_identical(one(do.call(.fr_A, c(list(0, 30, oc_pct = 3, munsell_value_moist = 3, munsell_chroma_moist = 3,
                                             ph_h2o = 8.0, bs_pct = 100, caco3_pct = 15, mgco3_pct = 15,
                                             effervescence_class = "slight"), bio))), "Ado")
  expect_identical(one(do.call(.fr_A, c(list(0, 30, oc_pct = 3, ph_h2o = 6.8, bs_pct = 90, ca_cmol = 16,
                                             mg_cmol = 2), bio))), "Aci")
  expect_identical(one(do.call(.fr_A, c(list(0, 30, oc_pct = 3, ph_h2o = 6.8, bs_pct = 90, ca_cmol = 4,
                                             mg_cmol = 4), bio))), "Amg")
  expect_identical(one(.fr_A(0, 30)), "A")
  # Av is an A with the feature v
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 30, clay_pct = 50, slickensides = "few"), .fr_S(30, 80), .fr_C(80, 120))), 0)
  expect_identical(r$code, "A")
  expect_true("v" %in% r$features[[1]])
  # Ae: an A directly above an E
  e <- .fr_L(20, 40, munsell_value_moist = 7, munsell_chroma_moist = 2, clay_pct = 10, oc_pct = 0.3,
             structure_type = "platy", structure_grade = "weak")
  fr <- .rp_identify(.fr_P(.fr_A(0, 20, clay_pct = 12), e, .fr_S(40, 90, clay_pct = 35, clay_films_amount = "many")))
  expect_identical(fr$code, c("Ae", "E", "BT"))
  expect_true("a" %in% fr$features[[2]])
  # Js: little organic matter and a weak structure; with no pedological
  # structure (single grain) no Js but a C (p. 25; 0.9.348, the review's D2)
  expect_identical(one(.fr_L(0, 15, oc_pct = 0.3, structure_type = "granular", structure_grade = "weak")), "Js")
  expect_identical(one(.fr_L(0, 15, oc_pct = 0.3, structure_type = "single grain", structure_grade = "structureless")), "C")
})


# --- step 7: subsurface horizons ------------------------------------------------------------------

test_that("E, BP and its kinds, BT, BTd, V, SV, the S family, C", {
  skip_on_cran()
  # a podzol: E over BPh over BPs
  pz <- .fr_P(.fr_A(0, 10, oc_pct = 4, ph_h2o = 4),
              .fr_L(10, 25, munsell_value_moist = 6, munsell_chroma_moist = 1, oc_pct = 0.5, ph_h2o = 4,
                    al_ox_pct = 0.1, fe_ox_pct = 0.1, structure_type = "single grain"),
              .fr_L(25, 40, munsell_hue_moist = "5YR", munsell_value_moist = 3, munsell_chroma_moist = 3, oc_pct = 3,
                    al_ox_pct = 1.0, fe_ox_pct = 1.0, ph_h2o = 4.2, structure_type = "massive"),
              .fr_L(40, 70, munsell_hue_moist = "7.5YR", munsell_value_moist = 5, munsell_chroma_moist = 6, oc_pct = 1,
                    al_ox_pct = 1.2, fe_ox_pct = 0.8, ph_h2o = 4.5, structure_type = "massive"),
              .fr_C(70, 120))
  fr <- .rp_identify(pz)
  expect_identical(fr$code[2:4], c("E", "BPh", "BPs"))
  # BT, with the degradation of BTd as the feature d
  lu <- function(...) .fr_P(.fr_A(0, 20, clay_pct = 15),
                            .fr_S(20, 70, clay_pct = 30, clay_films_amount = "many", ...), .fr_C(70, 120))
  expect_identical(.fr_code(lu(), 20), "BT")
  r <- .fr_row(.rp_identify(lu(degradation_form = "tongues")), 20)
  expect_identical(r$composed, "BTd")
  # V and SV
  vt <- .fr_P(.fr_A(0, 20, clay_pct = 50),
              .fr_S(20, 50, clay_pct = 55, cracks_width_cm = 2, structure_type = "prismatic"),
              .fr_S(50, 100, clay_pct = 58, slickensides = "many", structure_type = "wedge-shaped"),
              .fr_C(100, 140, clay_pct = 50))
  expect_identical(.rp_identify(vt)$code[2:3], c("SV", "V"))
  # the S family
  s <- function(...) .fr_code(.fr_P(.fr_A(0, 20), .fr_S(20, 60, ...), .fr_C(60, 100, caco3_pct = 40)), 20)
  expect_identical(s(clay_pct = 50, structure_type = "prismatic", structure_size = "coarse", ph_h2o = 6.5, bs_pct = 70), "Sp")
  expect_identical(s(caco3_pct = 10, effervescence_class = "strong", effervescence_extent = "continuous",
                     munsell_hue_moist = "10YR", ph_h2o = 8.2, bs_pct = 100), "Sca")
  expect_identical(s(ph_h2o = 7.2, bs_pct = 95, ca_cmol = 18, mg_cmol = 2, munsell_hue_moist = "10YR"), "Sci")
  expect_identical(s(ph_h2o = 7.2, bs_pct = 95, ca_cmol = 5, mg_cmol = 5), "Smg")
  expect_identical(s(ph_h2o = 4.5, bs_pct = 15, al_cmol = 4, cec_cmol = 12, ca_cmol = 1, mg_cmol = 0.4,
                     k_cmol = 0.2, na_cmol = 0.1), "Sal")
  expect_identical(s(), "S")
  # Scah: more carbon than the norm of Sca
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 20), .fr_S(20, 60, caco3_pct = 10, effervescence_class = "strong",
                                                      effervescence_extent = "continuous", munsell_hue_moist = "10YR",
                                                      ph_h2o = 8.2, bs_pct = 100, oc_pct = 1.5),
                                  .fr_C(60, 100, caco3_pct = 40))), 20)
  expect_identical(r$composed, "Scah")
  # C: no pedological structure
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20), .fr_S(20, 60), .fr_C(60, 100)), 60), "C")
})


test_that("the other codes: M, Mt, OHta, Femp, OXm, OXc, ND, BP, BTss, Snd, OX, FE, Sn, FS, Sdo, Jp", {
  skip_on_cran()
  trop <- list(lat = -12, koppen_climate = "Aw")
  # M: a coherent soft rock (the CPCS R gives R, M or D)
  m <- .fr_L(60, 100, designation = "R", clay_pct = NA, silt_pct = NA, sand_pct = NA, oc_pct = NA,
             structure_type = NA, structure_grade = NA, rupture_resistance = "soft")
  expect_identical(.fr_code(.fr_P(.fr_A(designation = "A1"), .fr_S(20, 60, designation = "(B)"), m), 60), "M")
  # Mt: a mineral layer between two peats
  pt <- function(t, b) .fr_L(t, b, oc_pct = 45, clay_pct = NA, silt_pct = NA, sand_pct = NA, structure_type = NA,
                             structure_grade = NA, fiber_content_rubbed_pct = 50, water_saturation_days = 300)
  expect_identical(.fr_code(.fr_P(pt(0, 30), .fr_L(30, 50, clay_pct = 30, oc_pct = 2), pt(50, 90)), 30), "Mt")
  # OHta: a greasy OH saturated by Ca and Mg
  oh <- .fr_L(0, 8, oc_pct = 35, clay_pct = NA, silt_pct = NA, sand_pct = NA, structure_type = NA, structure_grade = NA,
              water_saturation_days = 0, fine_organic_matter_pct = 85, sensitivity_class = "smeary",
              ca_cmol = 40, mg_cmol = 10, cec_cmol = 55, ph_h2o = 6.5)
  expect_identical(.fr_code(.fr_P(oh, .fr_A(8, 25)), 0), "OHta")
  # Femp: a thin iron pan; OXm and OXc in the tropics
  base <- function(l, site = list()) .fr_P(.fr_A(0, 20), l, .fr_C(80, 120), site = site)
  expect_identical(.fr_code(base(.fr_S(20, 21, pan_type = "thin ironpan")), 20), "Femp")
  expect_identical(.fr_code(base(.fr_S(20, 80, plinthite_pct = 50, cementation_class = "indurated"), trop), 20), "OXm")
  expect_identical(.fr_code(base(.fr_S(20, 80, plinthite_pct = 50, cementation_class = "moderately"), trop), 20), "OXc")
  # ND: nodules over 60 % by weight, in the tropics
  expect_identical(.fr_code(base(.fr_S(20, 80, nodules_wt_pct = 70), trop), 20), "ND")
  # BP alone
  bp <- .fr_P(.fr_A(0, 10, oc_pct = 4, ph_h2o = 4),
              .fr_L(10, 25, munsell_value_moist = 6, munsell_chroma_moist = 1, oc_pct = 0.5, al_ox_pct = 0.1,
                    fe_ox_pct = 0.1, structure_type = "single grain"),
              .fr_L(25, 50, munsell_hue_moist = "5YR", munsell_value_moist = 4, munsell_chroma_moist = 4, oc_pct = 1.5,
                    al_ox_pct = 1.0, fe_ox_pct = 1.0), .fr_C(50, 100))
  expect_identical(.rp_identify(bp)$code[3], "BP")
  # BTss: a dark BT on limestone under a BT
  bb <- .fr_P(.fr_A(0, 20, clay_pct = 15, oc_pct = 2),
              .fr_S(20, 50, clay_pct = 30, clay_films_amount = "many", oc_pct = 0.4),
              .fr_S(50, 65, clay_pct = 45, clay_films_amount = "many", oc_pct = 1, munsell_value_moist = 3,
                    munsell_chroma_moist = 3),
              .fr_L(65, 100, designation = "R", caco3_pct = 90, clay_pct = NA, silt_pct = NA, sand_pct = NA,
                    oc_pct = NA, structure_type = NA, structure_grade = NA))
  expect_identical(.rp_identify(bb)$code[3], "BT\u00df")
  # Snd: the silandic S below an And
  an <- .fr_A(0, 30, oc_pct = 8, munsell_value_moist = 2, munsell_chroma_moist = 2, ph_h2o = 5.5, al_pyro_pct = 0.5,
              si_ox_pct = 1.5, al_ox_pct = 4, fe_ox_pct = 2, bulk_density_g_cm3 = 0.7, phosphate_retention_pct = 90)
  snd <- .fr_S(30, 80, oc_pct = 1.5, ph_h2o = 5.8, al_pyro_pct = 0.4, al_ox_pct = 3, fe_ox_pct = 2, si_ox_pct = 1.2,
               bulk_density_g_cm3 = 0.8, phosphate_retention_pct = 88)
  expect_identical(.fr_code(.fr_P(an, snd, .fr_C(80, 120)), 30), "Snd")
  # OX: oxides over 60 % of the clay, a pH KCl at or above the pH in water
  ox <- .fr_S(20, 150, munsell_hue_moist = "10R", munsell_value_moist = 3, munsell_chroma_moist = 3, clay_pct = 10,
              oxides_triacid_pct = 70, cec_cmol = 0.3, ph_h2o = 6.0, ph_kcl = 6.2, structure_type = "granular",
              structure_grade = "weak")
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 10), ox, site = trop), 20), "OX")
  # FE: the loose ferruginous horizon
  fe <- .fr_S(20, 120, munsell_hue_moist = "5YR", munsell_value_moist = 5, munsell_chroma_moist = 7, clay_pct = 30,
              oc_pct = 0.2, ph_h2o = 6.2, bs_pct = 35, ca_cmol = 1.5, mg_cmol = 0.8, k_cmol = 0.2, na_cmol = 0.1,
              cec_cmol = 25)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 25), fe, site = trop), 20), "FE")
  # Sn: heavy clay, shiny pressure faces, halloysite
  sn <- .fr_S(20, 120, clay_pct = 60, pressure_faces = "many", halloysite_pct = 70, cec_cmol = 20, clay_films_amount = "few",
              munsell_hue_moist = "5YR", munsell_value_moist = 4, munsell_chroma_moist = 4)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 55), sn), 20), "Sn")
  # FS: red, angular, iron released
  fs <- .fr_S(20, 80, munsell_hue_moist = "2.5YR", munsell_value_moist = 4, munsell_chroma_moist = 6,
              structure_type = "angular blocky", structure_grade = "strong", fe_dcb_pct = 3, fe_total_pct = 4.5)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20), fs, .fr_C(80, 120)), 20), "FS")
  # Sdo: carbonated, a weak cold effervescence, dolomitic
  sdo <- .fr_S(20, 60, caco3_pct = 10, mgco3_pct = 12, effervescence_class = "slight", ph_h2o = 8, bs_pct = 100)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20), sdo, .fr_C(60, 100, caco3_pct = 40)), 20), "Sdo")
  # Jp: under a Js, before the C
  js <- .fr_L(0, 15, oc_pct = 0.3, structure_type = "granular", structure_grade = "weak")
  jp <- .fr_L(15, 50, oc_pct = 0.1, structure_type = "massive", structure_grade = "structureless")
  expect_identical(.rp_identify(.fr_P(js, jp, .fr_C(50, 100)))$code[1:2], c("Js", "Jp"))
})

# --- the domain gates (Q18, default 15) ---------------------------------------------------------------

test_that("F, OX and the other intertropical horizons only in the intertropical domain", {
  skip_on_cran()
  f <- .fr_S(20, 150, munsell_hue_moist = "2.5YR", munsell_value_moist = 4, munsell_chroma_moist = 6, clay_pct = 45,
             cec_cmol = 4, ph_h2o = 5.0, sio2_sulfuric_pct = 15, al2o3_sulfuric_pct = 25, structure_type = "granular",
             structure_grade = "weak")
  trop <- .fr_P(.fr_A(0, 20, clay_pct = 40), f, site = list(lat = -15, koppen_climate = "Aw"))
  expect_identical(.fr_code(trop, 20), "F")
  temp <- .fr_P(.fr_A(0, 20, clay_pct = 40), f)
  expect_false(identical(.fr_code(temp, 20), "F"))
  # neither the climate nor the latitude recorded: F stays a candidate
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 20, clay_pct = 40), f, site = list(lat = NA, koppen_climate = NA))), 20)
  expect_true("F" %in% r$candidates[[1]])
  # declared
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 40), f, site = list(intertropical = TRUE)), 20), "F")
  # RT: a reticulate pattern, in the tropics only
  rt <- .fr_S(60, 150, variegated_colors = "10R 4/8; 10YR 8/1; 7.5YR 6/8", cec_cmol = 30)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20), .fr_S(20, 60), rt, site = list(lat = -10, koppen_climate = "Aw")), 60), "RT")
  expect_false(identical(.fr_code(.fr_P(.fr_A(0, 20), .fr_S(20, 60), rt), 60), "RT"))
})

test_that("the andosol horizons only with andic or vitric properties", {
  skip_on_cran()
  an <- function(...) .fr_A(0, 30, oc_pct = 8, munsell_value_moist = 2, munsell_chroma_moist = 2,
                            al_ox_pct = 4, fe_ox_pct = 2, bulk_density_g_cm3 = 0.7, phosphate_retention_pct = 90, ...)
  sub <- .fr_S(30, 80, al_ox_pct = 3, fe_ox_pct = 2, bulk_density_g_cm3 = 0.8, phosphate_retention_pct = 88)
  expect_identical(.fr_code(.fr_P(an(ph_h2o = 5.5, al_pyro_pct = 0.5, si_ox_pct = 1.5), sub, .fr_C(80, 120)), 0), "And")
  expect_identical(.fr_code(.fr_P(an(ph_h2o = 4.2, al_pyro_pct = 3, si_ox_pct = 0.3), sub, .fr_C(80, 120)), 0), "Alu")
  # the same chemistry with no andic measurement is no andosol horizon
  na <- .fr_A(0, 30, oc_pct = 8, munsell_value_moist = 2, munsell_chroma_moist = 2, ph_h2o = 5.5)
  expect_false(.fr_code(.fr_P(na, .fr_S(30, 80), .fr_C(80, 120)), 0) %in% c("And", "Alu", "Avi"))
})


# --- the undecided steps (Q10) ---------------------------------------------------------------------------

test_that("an undecided test gives candidates, and the designation of another notation narrows them", {
  skip_on_cran()
  # an A with no S/CEC: Aci and Amg are undecided before the A haplique
  a <- .fr_A(0, 20, ph_h2o = 6.8, bs_pct = NA, biogenic_aggregates_pct = 40, biogenic_aggregate_size = "macro")
  r <- .fr_row(.rp_identify(.fr_P(a, .fr_S(20, 60), .fr_C(60, 100))), 0)
  expect_true(is.na(r$passed))
  expect_true(all(c("Aci", "A") %in% r$candidates[[1]]))
  expect_true("bs_pct" %in% r$missing[[1]])
  # a subsoil with no structure: S or C; the FAO Bw narrows it to the S
  nos <- .fr_S(20, 60, structure_type = NA, structure_grade = NA)
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(), nos, .fr_C(60, 100))), 20)
  expect_true(is.na(r$passed))
  expect_true(all(c("S", "C") %in% r$candidates[[1]]))
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(designation = "Ah"), modifyList(nos, list(designation = "Bw")),
                                  .fr_C(60, 100, designation = "C"))), 20)
  expect_identical(r$code, "S")
  expect_true("designation_hint" %in% r$fallbacks[[1]])
  expect_match(r$evidence, "narrows the candidates", fixed = TRUE)
})


# --- the notation and the data (default 7), the depth datum (default 8) ------------------------------------

test_that("a layer written in the RP notation keeps its code; a different reading goes to the evidence", {
  skip_on_cran()
  p <- .fr_P(.fr_A(0, 20, designation = "A", clay_pct = 15),
             .fr_S(20, 70, designation = "S", clay_pct = 30, clay_films_amount = "many"),
             .fr_C(70, 120, designation = "C"), site = list(designation_system = "rp2008"))
  fr <- .rp_identify(p)
  expect_identical(fr$code, c("A", "S", "C"))
  expect_identical(fr$route, c("both", "notation", "both"))
  expect_identical(fr$data_code[2], "BT")
  expect_match(fr$evidence[2], "the data read BT; the notation's S is kept", fixed = TRUE)
  # LAch, NaBTg2 and II Scag keep their overlays, features and numeral
  q <- .fr_P(.fr_A(0, 20, designation = "LAch"), .fr_S(20, 60, designation = "NaBTg2"),
             .fr_C(60, 100, designation = "II Scag"), site = list(designation_system = "rp2008"))
  fr <- .rp_identify(q)
  expect_identical(fr$composed, c("LAch", "NaBTg", "II Scag"))
})

test_that("depths count from the top of the first layer that is not a litter OL", {
  skip_on_cran()
  ol <- .fr_L(0, 3, oc_pct = 45, clay_pct = NA, silt_pct = NA, sand_pct = NA, structure_type = NA,
              structure_grade = NA, designation = "OL")
  p <- .fr_P(ol, .fr_A(3, 20, designation = "A"), .fr_S(20, 60, designation = "S"),
             site = list(designation_system = "rp2008"))
  fr <- .rp_identify(p)
  expect_identical(fr$top_rp, c(-3, 0, 17))
  expect_match(fr$evidence[1], "litter OL", fixed = TRUE)
  expect_match(fr$evidence[2], "depths from 3 cm", fixed = TRUE)
  # the FAO Oi too; an undesignated organic layer counts
  fr <- .rp_identify(.fr_P(modifyList(ol, list(designation = "Oi")), .fr_A(3, 20, designation = "Ah")))
  expect_identical(fr$top_rp, c(-3, 0))
  fr <- .rp_identify(.fr_P(modifyList(ol, list(designation = NA)), .fr_A(3, 20)))
  expect_identical(fr$top_rp, c(0, 3))
})


# --- the stand-ins and the overlays (default 6) -----------------------------------------------------------

test_that("a WRB stand-in is flagged and lowers the grade; L, Na and Sa are overlays", {
  skip_on_cran()
  # clay films not recorded: soilKey's argic() stands in for the coatings of BT
  p <- .fr_P(.fr_A(0, 20, clay_pct = 12), .fr_S(20, 70, clay_pct = 30, clay_films_amount = NA), .fr_C(70, 120, clay_pct = 25))
  r <- .fr_row(.rp_identify(p), 20)
  expect_true("argic" %in% r$stand_ins[[1]])
  expect_true(r$lower_grade)
  expect_match(r$evidence, "soilKey's WRB stand-in, flagged: argic()", fixed = TRUE)
  # a CEC by cobaltihexammine stands in for the acetate at pH 7 (Q12)
  q <- .fr_P(.fr_A(0, 20, bs_pct = 90, ca_cmol = 16, mg_cmol = 2, ph_h2o = 6.8, cec_method = "cobaltihexammine",
                   biogenic_aggregates_pct = 40, biogenic_aggregate_size = "macro"), .fr_S(20, 60))
  r <- .fr_row(.rp_identify(q), 0)
  expect_identical(r$code, "Aci")
  expect_true("cec_method_substitute" %in% r$fallbacks[[1]])
  expect_true(r$lower_grade)
  # the overlays: a ploughed A (L), a sodic BT (Na), a salic A (Sa)
  lp <- .rp_identify(.fr_P(.fr_A(0, 25), .fr_S(25, 60), site = list(land_use = "cropland")))
  expect_identical(lp$composed[1], "LA")
  na <- .fr_S(20, 70, clay_pct = 30, clay_films_amount = "many", structure_type = "columnar",
              na_cmol = 4, ca_cmol = 8, mg_cmol = 3, k_cmol = 0.5)
  expect_identical(.fr_row(.rp_identify(.fr_P(.fr_A(0, 20, clay_pct = 12), na, .fr_C(70, 120))), 20)$composed, "NaBT")
  sa <- .rp_identify(.fr_P(.fr_A(0, 20, ec_dS_m = 20), .fr_S(20, 60)))
  expect_identical(sa$composed[1], "SaA")
})

test_that("the humic words of the solum and the hydromorphy of the profile", {
  skip_on_cran()
  dark <- function(t, b, ...) .fr_A(t, b, munsell_value_moist = 2, munsell_chroma_moist = 2, ...)
  x <- .rp_profile(.fr_P(dark(0, 25, oc_pct = 3), dark(25, 50, oc_pct = 0.8), .fr_C(50, 100)))
  h <- .rp_humique(x)
  expect_true(h$humique)
  expect_true(h$clinohumique)
  x <- .rp_profile(.fr_P(dark(0, 15), .fr_S(15, 60), .fr_C(60, 100)))
  expect_false(.rp_humique(x)$humique)
  # the top of the first -g and of the first G, their thickness, from the datum
  fr <- .rp_identify(.fr_P(.fr_A(0, 20), .fr_S(20, 50, redoximorphic_features_pct = 15, redox_segregations_location = "within peds"),
                           .fr_S(50, 100, munsell_hue_moist = "5GY", munsell_value_moist = 5, munsell_chroma_moist = 1)))
  hy <- .rp_hydromorphy(fr)
  expect_identical(c(hy$g_top, hy$G_top, hy$thickness), c(20, 50, 80))
})

test_that("the fixes of the review (D1-D13, d1-d7, m1-m5) and Hugo's decision d3", {
  skip_on_cran()
  # D1: a grey matrix of another hue needs saturation, a water table or a
  # dipyridyl test, never the drainage class alone; an A only in a gley hue
  grey <- .fr_S(30, 80, munsell_hue_moist = "10YR", munsell_value_moist = 5, munsell_chroma_moist = 1)
  expect_false(.fr_code(.fr_P(.fr_A(0, 30), grey, .fr_C(80, 120), site = list(drainage_class = "poorly drained")), 30) %in% c("G", "Gr", "Go"))
  expect_identical(.fr_code(.fr_P(.fr_A(0, 30), modifyList(grey, list(childs_test = "positive")), .fr_C(80, 120)), 30), "Gr")
  expect_false(.fr_code(.fr_P(.fr_A(0, 20, munsell_value_moist = 4, munsell_chroma_moist = 1, water_saturation_days = 250),
                              .fr_S(20, 60)), 0) == "AG")
  # Gr only with 5 % or less; 5-10 % with no location stays undecided
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 30), modifyList(grey, list(childs_test = "positive", redoximorphic_features_pct = 8)),
                                  .fr_C(80, 120))), 30)
  expect_true(is.na(r$passed))
  # D2: the A run continues only over 1 % carbon, and a layer that continues
  # it with a subsurface code too is undecided between the two
  r <- .rp_identify(.fr_P(.fr_A(0, 20, oc_pct = 3), .fr_S(20, 50, oc_pct = 1.5, munsell_value_moist = 4, munsell_chroma_moist = 3),
                          .fr_C(50, 100)))
  expect_true(is.na(r$passed[2]))
  expect_true(all(c("A", "S") %in% r$candidates[[2]]))
  # D3, D4: no E over a rock unless bleached; an E has no more clay than above
  rock <- .fr_L(40, 80, clay_pct = NA, silt_pct = NA, sand_pct = NA, oc_pct = NA, structure_type = NA, structure_grade = NA,
                rupture_resistance = "extremely hard")
  e <- function(...) .fr_L(20, 40, munsell_value_moist = 6, munsell_chroma_moist = 3, oc_pct = 0.4, clay_pct = 10,
                           structure_type = "platy", structure_grade = "weak", ...)
  expect_false(identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 12), e(), rock), 20), "E"))
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 12), e(munsell_chroma_moist = 1), rock), 20), "E")
  # D5: Aso on the list of p. 13 only (10YR 3/3: the grisols reading alone)
  expect_false(identical(.fr_code(.fr_P(.fr_A(0, 30, oc_pct = 2.5, munsell_value_moist = 3, munsell_chroma_moist = 3,
                                              ph_h2o = 6.0, bs_pct = 65), .fr_S(30, 80)), 0), "Aso"))
  # D6: BT and FS both hold: FSt
  fst <- .fr_S(20, 70, clay_pct = 35, clay_films_amount = "common", munsell_hue_moist = "2.5YR", munsell_value_moist = 4,
               munsell_chroma_moist = 6, structure_type = "angular blocky", structure_grade = "strong")
  expect_identical(.fr_row(.rp_identify(.fr_P(.fr_A(0, 20, clay_pct = 15), fst, .fr_C(70, 120))), 20)$composed, "FSt")
  # D7: a BT with 25 % ferric nodules is a BTcn; 45 % is Fe
  btn <- function(n) .fr_S(20, 70, clay_pct = 30, clay_films_amount = "many", ferric_nodules_pct = n)
  expect_identical(.fr_row(.rp_identify(.fr_P(.fr_A(0, 20, clay_pct = 12), btn(25), .fr_C(70, 120))), 20)$composed, "BTcn")
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 12), btn(45), .fr_C(70, 120)), 20), "Fe")
  # D8: jarosite or a pH under 4: U, not TH
  u <- .fr_L(0, 30, s_total_pct = 1.5, jarosite_present = TRUE, ph_h2o = 3.3, water_saturation_days = 365, oc_pct = 2)
  expect_identical(.fr_code(.fr_P(u, .fr_C(30, 80)), 0), "U")
  # D9: Jp under an A in a young deposit
  jp <- .fr_L(20, 60, oc_pct = 0.2, structure_type = "subangular blocky", structure_grade = "weak", layer_origin = "fluvic", bs_pct = 30)
  expect_identical(.fr_code(.fr_P(.fr_A(0, 20), jp, .fr_C(60, 100)), 20), "Jp")
  # D10: an FAO g keeps the G candidates
  gg <- modifyList(grey, list(designation = "Bg", water_saturation_days = 200, redoximorphic_features_pct = 20,
                              structure_type = NA, structure_grade = NA))
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 30, designation = "Ah"), gg, .fr_C(80, 120, designation = "C"))), 30)
  expect_true("G" %in% r$candidates[[1]] || r$code %in% c("G", "Gr", "Go"))
  # D11: a measured Alox + 1/2 Feox under 2 % closes the andosol domain
  expect_false(.rp_andic(.rp_profile(.fr_P(.fr_A(0, 20, al_ox_pct = 0.3, fe_ox_pct = 0.4), .fr_S(20, 60))), 1)$passed)
  # D12: no Na on a massive layer with little sodium, none undecided on a C
  na <- .fr_L(20, 60, structure_type = "massive", ph_h2o = 5, cec_cmol = 3, ca_cmol = 0.2, mg_cmol = 0.1, k_cmol = 0.1, na_cmol = 0.1)
  r <- .fr_row(.rp_identify(.fr_P(.fr_A(0, 20), na)), 20)
  expect_false("Na" %in% c(r$overlay[[1]], r$overlay_na[[1]]))
  # d1: no Ach without a lighter mineral layer below (over a rock)
  ach <- .fr_A(0, 30, oc_pct = 5, munsell_value_moist = 2, munsell_chroma_moist = 1, ph_h2o = 7.2, bs_pct = 95,
               biogenic_aggregates_pct = 50, biogenic_aggregate_size = "macro")
  expect_false(identical(.fr_code(.fr_P(ach, modifyList(rock, list(top_cm = 30, bottom_cm = 60))), 0), "Ach"))
  # d2: no Ae on a ploughed layer
  ae <- .rp_identify(.fr_P(.fr_A(0, 20, clay_pct = 12), e(munsell_chroma_moist = 2),
                           .fr_S(40, 90, clay_pct = 35, clay_films_amount = "many"), site = list(land_use = "cropland")))
  expect_identical(ae$composed[1], "LA")
  # d4: a soft chalk with no fine earth is M
  chalk <- .fr_L(20, 60, clay_pct = NA, silt_pct = NA, sand_pct = NA, oc_pct = NA, structure_type = NA, structure_grade = NA,
                 rupture_resistance = "slightly hard", caco3_pct = 95)
  expect_identical(.fr_code(.fr_P(.fr_A(), chalk), 20), "M")
  # d5, m5: OL and OF in a profile in capitals are still the litter and the OF
  ol <- .fr_L(0, 3, oc_pct = 45, clay_pct = NA, silt_pct = NA, sand_pct = NA, structure_type = NA, structure_grade = NA,
              designation = "OL")
  fr <- .rp_identify(.fr_P(ol, modifyList(ol, list(top_cm = 3, bottom_cm = 6, designation = "OF")),
                           .fr_A(6, 20, designation = "A"), .fr_S(20, 60, designation = "B")))
  expect_identical(fr$top_rp[1:3], c(-3, 0, 3))
  expect_identical(fr$code[1:2], c("OL", "OF"))
  # d7: a mineral designation with no data is no H or O
  r <- .rp_identify(.fr_P(list(top_cm = 0, bottom_cm = 20, designation = "Ap"), list(top_cm = 20, bottom_cm = 60, designation = "Bt")))
  expect_false(any(c("H", "O") %in% unlist(r$candidates)))
  # m1: OX needs pH 5.5-6.5
  ox <- .fr_S(20, 150, munsell_hue_moist = "10R", munsell_value_moist = 3, munsell_chroma_moist = 3, clay_pct = 10,
              oxides_triacid_pct = 70, cec_cmol = 0.3, ph_h2o = 4.4, ph_kcl = 4.6)
  expect_false(identical(.fr_code(.fr_P(.fr_A(0, 20, clay_pct = 10), ox, site = list(lat = -12, koppen_climate = "Aw")), 20), "OX"))
  # m3: a loss on ignition contradicted by the carbon is flagged; no A over 30 %
  r <- .fr_row(.rp_identify(.fr_P(.fr_L(0, 10, oc_pct = 35, loi_pct = 30), .fr_S(10, 50))), 0)
  expect_true("loi_oc_contradiction" %in% r$fallbacks[[1]])
  expect_false(isTRUE(r$code == "A") || "A" %in% r$candidates[[1]])
  # Hugo's decision d3: crumb with worm holes is biomacrostructured; a
  # polyhedral structure is not; crumb with no activity recorded stays open
  x <- .rp_profile(.fr_P(.fr_A(0, 20, structure_type = "crumb", worm_holes_pct = 3), .fr_A(20, 30, structure_type = "crumb"),
                         .fr_A(30, 40, structure_type = "subangular blocky")))
  expect_true(.rp_biomacro(x, 1)$passed)
  expect_true(is.na(.rp_biomacro(x, 2)$passed))
  expect_false(.rp_biomacro(x, 3)$passed)
})

test_that("the readings of the RP horizons are listed in readings.yaml", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "rp2008", "readings.yaml", package = "soilKey"))$readings
  txt <- vapply(rd, function(r) r$reading, "")
  expect_true(any(grepl("order of tests", txt, fixed = TRUE)))
  expect_true(all(grepl("\\((Hugo|soilKey's reading)", txt)))
})
