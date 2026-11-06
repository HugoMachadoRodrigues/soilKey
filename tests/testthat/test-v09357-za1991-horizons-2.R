# v0.9.357: the South African soil classification of 1991, part 3: the other
# seventeen horizons and materials on the recorded data (the E and G
# horizons and the wet materials, podzol B, placic pan, soft and hard
# plinthic B, soft and hardpan carbonate horizons, dorbank, saprolite, hard
# rock, stratified alluvium, regic sand, man-made deposit, unconsolidated
# material), the position pass and .za_identify(). One passing layer, a
# failing twin and an undecided one per horizon; the stand-in flags.

# --- builders --------------------------------------------------------------------

.za_L <- function(top, bottom, ...) {
  d <- list(top_cm = top, bottom_cm = bottom, munsell_hue_moist = "10YR", munsell_value_moist = 5,
            munsell_chroma_moist = 4, munsell_hue_dry = "10YR", munsell_value_dry = 6,
            munsell_chroma_dry = 4, structure_type = "massive", structure_grade = "structureless",
            clay_pct = 20, silt_pct = 20, sand_pct = 60, oc_pct = 0.3, effervescence_class = "none",
            clay_films_amount = "none")
  a <- list(...)
  d[names(a)] <- a
  d
}
.za_P <- function(..., site = list()) {
  s <- list(id = "za", lat = -29, lon = 30)
  s[names(site)] <- site
  ls <- list(...)
  nm <- unique(unlist(lapply(ls, names)))
  h <- as.data.frame(lapply(stats::setNames(nm, nm), function(k)
    unlist(lapply(ls, function(l) if (is.null(l[[k]])) NA else l[[k]]))), stringsAsFactors = FALSE)
  old <- options(soilKey.warn_unrecognised = FALSE)
  on.exit(options(old))
  PedonRecord$new(site = s, horizons = h)
}
# an orthic A (weak, brown, 1 % OC, 15 % clay)
.za_A <- function(...) .za_L(0, 25, designation = "A", structure_type = "subangular blocky",
                             structure_grade = "weak", oc_pct = 1, munsell_value_moist = 4,
                             munsell_chroma_moist = 3, munsell_value_dry = 5, munsell_chroma_dry = 3,
                             clay_pct = 15, ca_cmol = 1, mg_cmol = 0.5, k_cmol = 0.2, na_cmol = 0.1, ...)
.za_t <- function(f, p, i) f(soilKey:::.za_x(p), i)
.zp <- function(f, p, i) .za_t(f, p, i)$passed
.has_tag <- function(r, t) any(grepl(t, r$tag, fixed = TRUE))
.codes <- function(p) soilKey:::.za_identify(p)$code
# one layer under an orthic A
.two <- function(...) .za_P(.za_A(), .za_L(25, 150, ...))


test_that("thresholds of part 3: sources and stand-in flags", {
  skip_on_cran()
  th <- soilKey:::.za_thresholds()
  new <- c("e_light_topsoil", "g_or_prismacutanic", "podzol_ph", "podzol_alfe", "podzol_oc",
           "podzol_thickness", "podzol_colour", "placic_min_thickness", "placic_max_thickness",
           "hard_plinthic_cementation", "hardpan_carbonate_cementation", "hardpan_carbonate_thickness",
           "carbonate_caco3", "soft_carbonate_secondary", "soft_carbonate_thickness", "dorbank_cementation",
           "dorbank_thickness", "dorbank_duripan", "saprolite_rock", "regic_sand_texture", "man_made_artefacts")
  for (n in new) {
    expect_false(is.null(th[[n]]), info = n)
    expect_true(is.character(th[[n]]$source) && nzchar(th[[n]]$source), info = n)
    expect_true(is.logical(th[[n]]$stand_in), info = n)
  }
  # the WRB stand-ins of Q13-Q16 are flagged; the open pages' numbers are not
  expect_true(all(vapply(c("podzol_ph", "placic_max_thickness", "carbonate_caco3", "dorbank_cementation",
                           "hardpan_carbonate_cementation"), function(n) th[[n]]$stand_in, NA)))
  expect_false(th$e_light_topsoil$stand_in)
  expect_false(th$g_or_prismacutanic$stand_in)
})

test_that("man-made deposit and hard rock", {
  skip_on_cran()
  f <- soilKey:::.za_t_MMD
  expect_true(.zp(f, .two(designation = "C", anthropic_material = "earthy"), 2))
  expect_false(.zp(f, .two(designation = "C"), 2))
  r <- .za_t(f, .two(designation = "C", artefacts_pct = 30), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:man_made_artefacts"))
  g <- soilKey:::.za_t_HRK
  expect_true(.zp(g, .two(designation = "R"), 2))
  expect_true(.zp(g, .two(designation = "C", pan_type = "silcrete"), 2))
  expect_false(.zp(g, .two(designation = "C"), 2))
})

test_that("hardpan carbonate, dorbank and hard plinthic B: cementation and agent (Q15, Q16)", {
  skip_on_cran()
  h <- soilKey:::.za_t_HCH; d <- soilKey:::.za_t_DOR; p <- soilKey:::.za_t_HPL
  calc <- .two(designation = "Ckm", cementation_class = "indurated", pan_type = "calcrete")
  r <- .za_t(h, calc, 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:hardpan_carbonate_cementation"))
  expect_false(.zp(d, calc, 2)); expect_false(.zp(p, calc, 2))
  expect_false(.zp(h, .two(designation = "Ck", cementation_class = "weakly", pan_type = "calcrete"), 2))
  # cemented, agent not recorded: undecided among the three
  und <- .two(designation = "Bm", cementation_class = "strongly")
  expect_true(is.na(.zp(h, und, 2))); expect_true(is.na(.zp(d, und, 2))); expect_true(is.na(.zp(p, und, 2)))
  # the agent from what the layer holds
  expect_true(.zp(h, .two(designation = "Bm", cementation_class = "strongly", caco3_pct = 40), 2))
  expect_true(.zp(d, .two(designation = "Bqm", cementation_class = "strongly", duripan_pct = 30), 2))
  expect_true(.zp(p, .two(designation = "Bm", cementation_class = "indurated", pan_type = "ferricrete"), 2))
  # a hard plinthic B is too hard to cut: moderately cemented iron is not one
  expect_false(.zp(p, .two(designation = "Bm", cementation_class = "moderately", pan_type = "ferricrete"), 2))
  # not cemented: none of them (an observation not recorded is absent)
  expect_false(.zp(h, .two(designation = "B"), 2))
  # thin: a placic pan, not a hard plinthic B or a hardpan
  thin <- .za_P(.za_A(), .za_L(25, 60, designation = "Bs"), .za_L(60, 61, designation = "Bsm", cementation_class = "strongly",
                                                                     pan_type = "thin ironpan"))
  expect_false(.zp(p, thin, 3))
  expect_true(.zp(soilKey:::.za_t_PLP, thin, 3))
})

test_that("placic pan (Q14)", {
  skip_on_cran()
  f <- soilKey:::.za_t_PLP
  mk <- function(...) .za_P(.za_A(), .za_L(25, 60, designation = "Bs"), .za_L(60, 61, designation = "Bsm", ...))
  r <- .za_t(f, mk(cementation_class = "weakly", pan_type = "ironpan"), 3)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:placic_max_thickness"))
  expect_false(.zp(f, mk(), 3))                                      # no cementation recorded
  expect_true(.zp(f, mk(cementation_class = "weakly"), 3))          # Bsm: the letters, flagged
  expect_true(is.na(.zp(f, .za_P(.za_A(), .za_L(25, 26, designation = "Bm", cementation_class = "weakly")), 2)))
})

test_that("saprolite and soft carbonate horizon", {
  skip_on_cran()
  s <- soilKey:::.za_t_SAP
  expect_true(.zp(s, .two(designation = "C", weathering_stage = "saprolite"), 2))
  expect_false(.zp(s, .two(designation = "C", weathering_stage = "fresh"), 2))
  r <- .za_t(s, .two(designation = "C", saprolite_pct = 70), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:saprolite_rock"))
  expect_false(.zp(s, .two(designation = "C", saprolite_pct = 20), 2))
  expect_true(.zp(s, .two(designation = "Cr"), 2))
  expect_false(.zp(s, .two(designation = "R"), 2))
  k <- soilKey:::.za_t_SCH
  sc <- .two(designation = "Ck", caco3_pct = 30, secondary_carbonates_pct = 15, effervescence_class = "strong")
  r <- .za_t(k, sc, 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:carbonate_caco3"))
  expect_false(.zp(k, .two(designation = "Ck", caco3_pct = 5, effervescence_class = "strong"), 2))
  # calcareous, CaCO3 not recorded: undecided
  expect_true(is.na(.zp(k, .two(designation = "Ck", effervescence_class = "strong"), 2)))
  # secondary carbonates not recorded: absent, flagged in the note
  r <- .za_t(k, .two(designation = "Ck", caco3_pct = 30, effervescence_class = "strong"), 2)
  expect_false(r$passed)
  expect_true(.zp(k, .two(designation = "Ck", pan_type = "nodular calcrete", effervescence_class = "strong"), 2))
})

test_that("E horizon (Q9): grey dry, eluvial, under the topsoil", {
  skip_on_cran()
  f <- soilKey:::.za_t_E
  e <- function(...) .za_P(.za_A(), .za_L(25, 60, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2,
                                          munsell_chroma_moist = 2, clay_pct = 8, ...))
  r <- .za_t(f, e(), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:grey_dry"))
  expect_false(.zp(f, e(clay_pct = 30), 2))                          # more clay than the A, nothing below
  # R2: less clay than the layer below, or at most 2 points over the layer above
  e3 <- function(ce, cb) .za_P(.za_A(), .za_L(25, 60, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2,
                                               munsell_chroma_moist = 2, clay_pct = ce), .za_L(60, 150, clay_pct = cb))
  expect_true(.zp(f, e3(17, 10), 2))
  expect_false(.zp(f, e3(18, 10), 2))
  expect_true(.zp(f, e3(18, 30), 2))
  expect_true(is.na(.zp(f, e3(18, NA), 2)))
  expect_false(.zp(f, e(cross_bedding = TRUE), 2))                   # S 33
  expect_false(.zp(f, .two(designation = "E"), 2))                   # 10YR 6/4 dry: not grey
  expect_true(is.na(.zp(f, e(clay_pct = NA), 2)))
  expect_true(is.na(.zp(f, e(munsell_value_dry = NA, munsell_chroma_dry = NA), 2)))
  # a grey layer under a B is no E (the position pass)
  p <- .za_P(.za_A(), .za_L(25, 60, designation = "B", munsell_hue_moist = "2.5YR", munsell_value_moist = 3,
                            munsell_chroma_moist = 6, munsell_hue_dry = "2.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6),
             .za_L(60, 150, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2, munsell_chroma_moist = 3, clay_pct = 8))
  expect_true(.zp(f, p, 3))
  expect_false(identical(.codes(p)[3], "E"))
})

test_that("podzol B (Q13): the spodic chemistry, else the morphological route", {
  skip_on_cran()
  f <- soilKey:::.za_t_PZB
  E <- .za_L(10, 40, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 1, munsell_chroma_moist = 1,
             clay_pct = 3, sand_pct = 92, silt_pct = 5)
  A <- .za_A(bottom_cm = 10, clay_pct = 5)
  pz <- function(...) .za_P(A, E, .za_L(40, 80, designation = "Bhs", munsell_hue_moist = "7.5YR", munsell_value_moist = 3,
                                        munsell_chroma_moist = 4, munsell_hue_dry = "7.5YR", munsell_value_dry = 4,
                                        munsell_chroma_dry = 4, oc_pct = 1.5, ph_h2o = 4.5, clay_pct = 5, ...))
  r <- .za_t(f, pz(al_ox_pct = 0.4, fe_ox_pct = 0.6), 3)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:podzol_alfe"))
  expect_false(.zp(f, pz(al_ox_pct = 0.1, fe_ox_pct = 0.2), 3))
  # no oxalate data: the designation and the colours, flagged
  r <- .za_t(f, pz(), 3)
  expect_true(r$passed); expect_true(.has_tag(r, "podzol_b_morphology"))
  expect_false(.zp(f, pz(ph_h2o = 6.5), 3))
  # a grey E above, illuvial colours, no designation, no oxalate: undecided
  p <- .za_P(A, E, .za_L(40, 80, designation = "B", munsell_hue_moist = "7.5YR", munsell_value_moist = 3,
                         munsell_chroma_moist = 4, oc_pct = 1.5, clay_pct = 5))
  expect_true(is.na(.zp(f, p, 3)))
  # a red apedal B under an orthic A: no mark
  expect_false(.zp(f, .two(designation = "B", munsell_hue_moist = "2.5YR"), 2))
})

test_that("soft plinthic B: more than 10 % mottles and grey colours here or below", {
  skip_on_cran()
  f <- soilKey:::.za_t_SPL
  sp <- function(...) .za_P(.za_A(), .za_L(25, 150, designation = "B", munsell_value_dry = 7, munsell_chroma_dry = 6,
                                           munsell_chroma_moist = 6, redoximorphic_features_pct = 25,
                                           mottle_colors = "10YR 6/1; 7.5YR 5/8", ...))
  expect_true(.zp(f, sp(), 2))
  expect_false(.zp(f, sp(redoximorphic_features_pct = 10), 2))
  expect_false(.zp(f, sp(cementation_class = "indurated"), 2))
  # high-chroma mottles only, nothing described below: undecided
  expect_true(is.na(.zp(f, sp(mottle_colors = "7.5YR 5/8"), 2)))
  # grey directly below
  p <- .za_P(.za_A(), .za_L(25, 80, designation = "B", munsell_chroma_moist = 6, munsell_chroma_dry = 6,
                            redoximorphic_features_pct = 25, mottle_colors = "7.5YR 5/8"),
             .za_L(80, 150, designation = "C", munsell_chroma_moist = 1, munsell_chroma_dry = 1))
  expect_true(.zp(f, p, 2))
})

test_that("stratified alluvium, regic sand and unconsolidated material", {
  skip_on_cran()
  a <- soilKey:::.za_t_SAL
  expect_true(.zp(a, .two(designation = "C", stratification_pattern = "continuous", layer_origin = "fluvic"), 2))
  r <- .za_t(a, .two(designation = "C", stratification_pattern = "continuous"), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "origin_not_recorded"))
  expect_false(.zp(a, .two(designation = "C", stratification_pattern = "continuous", layer_origin = "aeolic"), 2))
  expect_false(.zp(a, .two(designation = "C"), 2))
  s <- soilKey:::.za_t_RSA
  rs <- function(...) .two(designation = "C", sand_pct = 92, silt_pct = 5, clay_pct = 3, layer_origin = "aeolic", ...)
  r <- .za_t(s, rs(), 2)
  expect_true(r$passed); expect_true(.has_tag(r, "stand_in:regic_sand_texture"))
  expect_false(.zp(s, rs(clay_pct = 20, silt_pct = 20, sand_pct = 60), 2))
  expect_false(.zp(s, .two(designation = "C", sand_pct = 92, silt_pct = 5, clay_pct = 3), 2))   # no aeolian mark
  expect_true(is.na(.zp(s, rs(clay_pct = NA, silt_pct = NA, sand_pct = NA), 2)))
  u <- soilKey:::.za_t_UMN
  expect_true(.zp(u, .two(designation = "C"), 2))
  expect_false(.zp(u, .two(designation = "Bw"), 2))
  expect_false(.zp(u, .two(designation = "C", weathering_stage = "saprolite"), 2))
  expect_true(is.na(.zp(u, .two(designation = NA), 2)))
})

test_that("the position pass: the same grey layer is G, USW or UMW by what lies above", {
  skip_on_cran()
  grey <- list(designation = "Bg", munsell_value_moist = 5, munsell_chroma_moist = 1, munsell_value_dry = 6,
               munsell_chroma_dry = 1, clay_pct = 40)
  g <- do.call(.za_L, c(list(60, 150), grey))
  # under an orthic A: G
  p1 <- .za_P(.za_A(bottom_cm = 60), g)
  expect_identical(.codes(p1), c("OA", "G"))
  expect_true(g_horizon_za(p1)$passed)
  # under a red apedal B: the unspecified material with signs of wetness
  rab <- .za_L(25, 60, designation = "B", munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 6,
               munsell_hue_dry = "2.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6, clay_pct = 25)
  p2 <- .za_P(.za_A(), rab, g)
  expect_identical(.codes(p2), c("OA", "RAB", "USW"))
  expect_true(unspecified_material_wet_za(p2)$passed)
  # under a pedocutanic B: the unconsolidated material with signs of wetness
  pcu <- .za_L(25, 60, designation = "Bt", structure_type = "angular blocky", structure_grade = "moderate",
               clay_films_amount = "many", clay_pct = 35)
  p3 <- .za_P(.za_A(), pcu, g)
  expect_identical(.codes(p3), c("OA", "PCU", "UMW"))
  expect_true(unconsolidated_material_wet_za(p3)$passed)
  # a C under a pedocutanic B: with mottles of wetness UMW, without UMN
  p4 <- .za_P(.za_A(), pcu, .za_L(60, 160, designation = "C", redoximorphic_features_pct = 10, mottle_colors = "10YR 6/2"))
  expect_identical(.codes(p4), c("OA", "PCU", "UMW"))
  p5 <- .za_P(.za_A(), pcu, .za_L(60, 160, designation = "C"))
  expect_identical(.codes(p5), c("OA", "PCU", "UMN"))
  expect_true(unconsolidated_material_dry_za(p5)$passed)
  # a C under an orthic A is no material of the key
  p6 <- .za_P(.za_A(), .za_L(25, 160, designation = "C"))
  f6 <- soilKey:::.za_identify(p6)
  expect_true(is.na(f6$code[2])); expect_false(f6$passed[2])
  # a grey layer under a soft plinthic B: signs of wetness only
  spl <- .za_L(25, 60, designation = "B", munsell_chroma_moist = 6, munsell_value_dry = 7, munsell_chroma_dry = 6,
               redoximorphic_features_pct = 25, mottle_colors = "10YR 6/1; 7.5YR 5/8")
  p7 <- .za_P(.za_A(), spl, g)
  expect_identical(.codes(p7)[2], "SPL")
  expect_false(identical(.codes(p7)[3], "G"))
  # the layer above undecided (a red or yellow-brown apedal B, or none): USW or G
  nohue <- .za_L(25, 60, designation = "B", munsell_hue_moist = NA, munsell_hue_dry = NA, munsell_chroma_moist = 6,
                 munsell_chroma_dry = 6, clay_pct = 25)
  f8 <- soilKey:::.za_identify(.za_P(.za_A(), nohue, g))
  expect_true(is.na(f8$code[3]))
  expect_setequal(f8$candidates[[3]], c("USW", "G"))
})

test_that("the 500 mm rule of K 19 and 27: G or prismacutanic under an E", {
  skip_on_cran()
  mk <- function(e_bottom) .za_P(
    .za_A(bottom_cm = 20, boundary_distinctness = "abrupt"),
    .za_L(20, e_bottom, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2, munsell_chroma_moist = 2,
          clay_pct = 8, boundary_distinctness = "abrupt"),
    .za_L(e_bottom, 150, designation = "Btg", structure_type = "prismatic", structure_grade = "strong",
          munsell_value_moist = 5, munsell_chroma_moist = 1, munsell_value_dry = 6, munsell_chroma_dry = 1,
          clay_pct = 45, coatings_value_moist = 2))
  expect_identical(.codes(mk(70))[3], "G")
  expect_identical(.codes(mk(40))[3], "PRC")
  f <- soilKey:::.za_identify(mk(50))
  expect_true(is.na(f$code[3]))
  expect_setequal(f$candidates[[3]], c("PRC", "G"))
})

test_that("lithocutanic B under the topsoil, saprolite under a B (default 17)", {
  skip_on_cran()
  lc <- list(designation = "BC", bedrock_pct = 60)
  p1 <- .za_P(.za_A(), do.call(.za_L, c(list(25, 60), lc)), .za_L(60, 80, designation = "R"))
  expect_identical(.codes(p1), c("OA", "LCU", "HRK"))
  pcu <- .za_L(25, 60, designation = "Bt", structure_type = "angular blocky", structure_grade = "moderate",
               clay_films_amount = "many", clay_pct = 35)
  p2 <- .za_P(.za_A(), pcu, .za_L(60, 90, designation = "BC", bedrock_pct = 30))
  f2 <- soilKey:::.za_identify(p2)
  expect_identical(f2$code[3], "SAP")
  expect_true("lithocutanic_under_b_as_saprolite" %in% f2$tags[[3]])
})

test_that("the placic pan only under a podzol B; the red structured B only under an orthic A", {
  skip_on_cran()
  pan <- .za_L(60, 61, designation = "Bsm", cementation_class = "strongly", pan_type = "thin ironpan")
  p <- .za_P(.za_A(), .za_L(25, 60, designation = "Bw"), pan)
  expect_false(identical(.codes(p)[3], "PLP"))
  E <- .za_L(10, 40, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 1, munsell_chroma_moist = 1, clay_pct = 3)
  bh <- .za_L(40, 60, designation = "Bhs", munsell_hue_moist = "7.5YR", munsell_value_moist = 3, munsell_chroma_moist = 4,
              oc_pct = 1.5, ph_h2o = 4.5, al_ox_pct = 0.4, fe_ox_pct = 0.6, clay_pct = 5)
  p2 <- .za_P(.za_A(bottom_cm = 10, clay_pct = 5), E, bh, pan)
  expect_identical(.codes(p2), c("OA", "E", "PZB", "PLP"))
})

test_that("T-humic (iv) on the identified first subsoil", {
  skip_on_cran()
  ha <- .za_L(0, 30, designation = "A", munsell_value_moist = 3, munsell_chroma_moist = 2, munsell_value_dry = 4,
              munsell_chroma_dry = 2, oc_pct = 3, clay_pct = 30, ca_cmol = 0.5, mg_cmol = 0.2, k_cmol = 0.1,
              na_cmol = 0.05, structure_type = "subangular blocky", structure_grade = "weak")
  yab <- .za_L(30, 150, designation = "B", munsell_value_moist = 5, munsell_chroma_moist = 6, munsell_value_dry = 6,
               munsell_chroma_dry = 6, clay_pct = 35)
  expect_identical(.codes(.za_P(ha, yab)), c("HA", "YAB"))
  # over a G: falls to orthic
  g <- .za_L(30, 150, designation = "Bg", munsell_value_moist = 5, munsell_chroma_moist = 1, munsell_value_dry = 6,
             munsell_chroma_dry = 1, clay_pct = 40)
  f <- soilKey:::.za_identify(.za_P(ha, g))
  expect_identical(f$code, c("OA", "G"))
  expect_true(any(grepl("T-humic \\(iv\\)", f$notes[[1]])))
})

test_that(".za_identify: the notation route and the data route", {
  skip_on_cran()
  rab <- .za_L(25, 150, designation = "B", munsell_hue_moist = "2.5YR", munsell_value_moist = 3, munsell_chroma_moist = 6,
               munsell_hue_dry = "2.5YR", munsell_value_dry = 4, munsell_chroma_dry = 6)
  p <- .za_P(.za_A(diagnostic_za = "orthic A"), c(rab, diagnostic_za = "red apedal B"))
  f <- soilKey:::.za_identify(p)
  expect_identical(f$code, c("OA", "RAB"))
  expect_identical(f$route, c("both", "both"))
  # a name the data disagree with is kept
  p2 <- .za_P(.za_A(diagnostic_za = "orthic A"), c(rab, diagnostic_za = "yellow-brown apedal B"))
  f2 <- soilKey:::.za_identify(p2)
  expect_identical(f2$code[2], "YAB")
  expect_identical(f2$route[2], "notation")
  expect_identical(f2$data_code[2], "RAB")
  # the data route
  f3 <- soilKey:::.za_identify(.za_P(.za_A(), rab))
  expect_identical(f3$route, c("data", "data"))
  expect_identical(f3$za_label[2], "Red apedal B [RAB]")
  # an undecided layer: candidates and the attributes that would decide it
  p4 <- .za_P(.za_A(), .za_L(25, 150, designation = "B", munsell_hue_moist = NA, munsell_hue_dry = NA,
                             munsell_chroma_moist = 6, munsell_chroma_dry = 6))
  f4 <- soilKey:::.za_identify(p4)
  expect_true(is.na(f4$code[2]))
  expect_true(all(c("RAB", "YAB") %in% f4$candidates[[2]]))
  expect_true("munsell_hue_moist" %in% f4$missing[[2]])
})

test_that("a light grey topsoil deeper than 350 mm has an E (S 33)", {
  skip_on_cran()
  p <- .za_P(.za_A(bottom_cm = 35, clay_pct = 6),
             .za_L(35, 70, designation = "A2", munsell_value_dry = 7, munsell_chroma_dry = 2, clay_pct = 5),
             .za_L(70, 150, designation = "B", munsell_value_moist = 5, munsell_chroma_moist = 6,
                   munsell_value_dry = 6, munsell_chroma_dry = 6, clay_pct = 12))
  expect_identical(.codes(p)[1:2], c("OA", "E"))
})

test_that("the predicates of part 3 over a pedon carry the flags", {
  skip_on_cran()
  p <- .two(designation = "Ckm", cementation_class = "indurated", pan_type = "calcrete")
  r <- hardpan_carbonate_za(p)
  expect_true(r$passed)
  expect_match(r$notes, "stand_in:hardpan_carbonate", fixed = TRUE)
  expect_false(dorbank_za(p)$passed)
  expect_true(is.na(hard_plinthic_b_za(.two(designation = "Bm", cementation_class = "strongly"))$passed))
  for (f in c("e_horizon_za", "podzol_b_za", "placic_pan_za", "soft_plinthic_b_za", "soft_carbonate_za",
              "saprolite_za", "hard_rock_za", "stratified_alluvium_za", "regic_sand_za", "man_made_deposit_za",
              "wet_material_za", "unconsolidated_material_za"))
    expect_s3_class(get(f, envir = asNamespace("soilKey"))(p), "DiagnosticResult")
})

# --- the review of 2026-11-06: Hugo's decisions D2-D7, R1-R3, R9 ---------------------

.pcu <- function(t, b, ...) .za_L(t, b, designation = "Bt", structure_type = "angular blocky", structure_grade = "moderate",
                                  clay_films_amount = "many", clay_pct = 35, ...)
.pzb <- function(t, b, ...) .za_L(t, b, designation = "Bhs", munsell_hue_moist = "7.5YR", munsell_value_moist = 3,
                                  munsell_chroma_moist = 4, munsell_hue_dry = "7.5YR", munsell_value_dry = 4,
                                  munsell_chroma_dry = 4, oc_pct = 1.5, ph_h2o = 4.5, al_ox_pct = 0.4, fe_ox_pct = 0.6,
                                  clay_pct = 5, silt_pct = 5, sand_pct = 90, ...)
.Ez <- function(t, b, ...) .za_L(t, b, designation = "E", munsell_value_moist = 5, munsell_chroma_moist = 2,
                                 munsell_value_dry = 7, munsell_chroma_dry = 1, clay_pct = 3, ...)
.Asand <- function(...) .za_A(clay_pct = 5, sand_pct = 85, silt_pct = 10, ...)
.rabL <- function(t, b, ...) .za_L(t, b, designation = "B", munsell_hue_moist = "2.5YR", munsell_value_moist = 3,
                                   munsell_chroma_moist = 6, munsell_hue_dry = "2.5YR", munsell_value_dry = 4,
                                   munsell_chroma_dry = 6, clay_pct = 25, ...)
.yabL <- function(t, b, ...) .za_L(t, b, designation = "B", munsell_value_moist = 5, munsell_chroma_moist = 6,
                                   munsell_value_dry = 6, munsell_chroma_dry = 6, clay_pct = 25, ...)

test_that("D2: the B horizons by position; under a podzol or pedocutanic B, material", {
  skip_on_cran()
  # A / Bt / BC (10YR 5/4) / C: the BC is unconsolidated material
  p <- .za_P(.za_A(), .pcu(25, 80), .za_L(80, 100, designation = "BC"), .za_L(100, 160, designation = "C"))
  f <- soilKey:::.za_identify(p)
  expect_identical(f$code, c("OA", "PCU", "UMN", "UMN"))
  expect_true("b_under_podzol_or_pedocutanic_as_material" %in% f$tags[[3]])
  # A / Bhs / B2 yellow sandy: material under the podzol B
  p2 <- .za_P(.Asand(bottom_cm = 15), .pzb(15, 50), .yabL(50, 150, designation = "B2", clay_pct = 6, sand_pct = 88, silt_pct = 6))
  expect_identical(soilKey:::.za_identify(p2)$code, c("OA", "PZB", "UMN"))
  # a red apedal B under a yellow-brown one stays; a yellow-brown under a red one does not
  expect_identical(soilKey:::.za_identify(.za_P(.za_A(), .yabL(25, 70), .rabL(70, 150)))$code, c("OA", "YAB", "RAB"))
  expect_false(identical(soilKey:::.za_identify(.za_P(.za_A(), .rabL(25, 70), .yabL(70, 150)))$code[3], "YAB"))
})

test_that("D3: a grey eluvial layer is an E, never a G", {
  skip_on_cran()
  # E dry 10YR 6/3 (not grey by the WRB stand-in), moist 5/2, 8 % clay under 15 %
  p <- .za_P(.za_A(designation = "Ap"), .za_L(25, 45, designation = "E", munsell_value_dry = 6, munsell_chroma_dry = 3,
                                              munsell_value_moist = 5, munsell_chroma_moist = 2, clay_pct = 8),
             .pcu(45, 110), .za_L(110, 160, designation = "C"))
  f <- soilKey:::.za_identify(p)
  expect_identical(f$code[1:3], c("OA", "E", "PCU"))
  expect_true("e_by_moist_colour" %in% f$tags[[2]])
  # A 8 % / E 9 % / G: the E by R2
  p2 <- .za_P(.za_A(clay_pct = 8), .za_L(25, 60, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2,
                                        munsell_chroma_moist = 2, clay_pct = 9),
              .za_L(60, 150, designation = "G", munsell_value_moist = 5, munsell_chroma_moist = 1, munsell_value_dry = 6,
                    munsell_chroma_dry = 1, clay_pct = 40))
  expect_identical(soilKey:::.za_identify(p2)$code, c("OA", "E", "G"))
  # a grey layer with less clay than the topsoil (not eluvial enough for an E) is no G
  p3 <- .za_P(.za_A(), .za_L(25, 150, designation = "B", munsell_value_moist = 5, munsell_chroma_moist = 1,
                             munsell_value_dry = 4, munsell_chroma_dry = 1, clay_pct = 10))
  expect_false(identical(soilKey:::.za_identify(p3)$code[2], "G"))
})

test_that("R1: a grey pedocutanic B needs one more sign to be the G", {
  skip_on_cran()
  p <- .za_P(.za_A(), .pcu(25, 90, munsell_value_moist = 4, munsell_chroma_moist = 2, munsell_value_dry = 5,
                           munsell_chroma_dry = 2), .za_L(90, 160, designation = "C"))
  expect_identical(soilKey:::.za_identify(p)$code[2], "PCU")
  p2 <- .za_P(.za_A(), .pcu(25, 90, munsell_value_moist = 5, munsell_chroma_moist = 2, munsell_value_dry = 6,
                            munsell_chroma_dry = 2), .za_L(90, 160, designation = "C"))
  expect_identical(soilKey:::.za_identify(p2)$code[2], "G")
})

test_that("R2: the E only under an orthic A or an E", {
  skip_on_cran()
  ma <- .za_L(0, 25, designation = "A", munsell_value_moist = 2, munsell_chroma_moist = 1, munsell_value_dry = 3,
              munsell_chroma_dry = 1, structure_type = "subangular blocky", structure_grade = "moderate",
              slickensides = "none", clay_pct = 35, oc_pct = 2, ca_cmol = 15, mg_cmol = 5, k_cmol = 0.5, na_cmol = 0.3)
  p <- .za_P(ma, .za_L(25, 60, designation = "E", munsell_value_dry = 7, munsell_chroma_dry = 2, munsell_chroma_moist = 2,
                       clay_pct = 8),
             .za_L(60, 150, designation = "G", munsell_value_moist = 5, munsell_chroma_moist = 1, munsell_value_dry = 6,
                   munsell_chroma_dry = 1, clay_pct = 40))
  f <- soilKey:::.za_identify(p)
  expect_identical(f$code[1], "MA")
  expect_false(identical(f$code[2], "E"))
})

test_that("R3: a B1 with mottles but no grey, over a soft plinthic B2, is apedal", {
  skip_on_cran()
  spl <- .za_L(60, 150, designation = "B2", munsell_value_moist = 6, munsell_chroma_moist = 6, munsell_value_dry = 7,
               munsell_chroma_dry = 6, redoximorphic_features_pct = 25, mottle_colors = "10YR 6/1; 7.5YR 5/8", clay_pct = 30)
  p <- .za_P(.za_A(), .yabL(25, 60, designation = "B1", redoximorphic_features_pct = 12, mottle_colors = "7.5YR 5/8"), spl)
  expect_identical(soilKey:::.za_identify(p)$code, c("OA", "YAB", "SPL"))
  # with grey mottles of its own the B1 is the soft plinthic B
  p2 <- .za_P(.za_A(), .yabL(25, 60, designation = "B1", redoximorphic_features_pct = 12, mottle_colors = "10YR 6/1"), spl)
  expect_identical(soilKey:::.za_identify(p2)$code[2], "SPL")
})

test_that("D5: the podzol B on the oxalate route needs 0.5 % organic carbon", {
  skip_on_cran()
  f <- soilKey:::.za_t_PZB
  mk <- function(...) .za_P(.Asand(bottom_cm = 10), .Ez(10, 40), .pzb(40, 70, ...), .za_L(70, 150, designation = "C"))
  expect_true(.zp(f, mk(), 3))
  expect_false(.zp(f, mk(oc_pct = 0.1), 3))
  expect_true(is.na(.zp(f, mk(oc_pct = NA), 3)))
})

test_that("D6: saprolite under the topsoil or an E is the lithocutanic B", {
  skip_on_cran()
  f <- soilKey:::.za_identify(.za_P(.za_A(bottom_cm = 20), .za_L(20, 60, designation = "Cr"), .za_L(60, 80, designation = "R")))
  expect_identical(f$code, c("OA", "LCU", "HRK"))
  expect_true("saprolite_under_topsoil_as_lithocutanic" %in% f$tags[[2]])
  f2 <- soilKey:::.za_identify(.za_P(.za_A(bottom_cm = 20), .za_L(20, 45, designation = "E", munsell_value_dry = 7,
                                                                   munsell_chroma_dry = 2, munsell_chroma_moist = 2, clay_pct = 8),
                                     .za_L(45, 80, designation = "Cr")))
  expect_identical(f2$code, c("OA", "E", "LCU"))
})

test_that("D7: a red apedal surface layer with little carbon over rock or a dorbank is a B", {
  skip_on_cran()
  f <- soilKey:::.za_identify(.za_P(.rabL(0, 30, designation = "A", oc_pct = 0.2), .za_L(30, 50, designation = "R")))
  expect_identical(f$code, c("RAB", "HRK"))
  expect_true(all(c("surface_b_low_oc", "stand_in:surface_b_oc") %in% f$tags[[1]]))
  dor <- .za_L(30, 60, designation = "Bqm", cementation_class = "strongly", pan_type = "duripan")
  expect_identical(soilKey:::.za_identify(.za_P(.rabL(0, 30, designation = NA, oc_pct = 0.2), dor))$code, c("RAB", "DOR"))
  # with 1 % organic carbon it stays the orthic A
  expect_identical(soilKey:::.za_identify(.za_P(.rabL(0, 30, designation = "A", oc_pct = 1), .za_L(30, 50, designation = "R")))$code[1], "OA")
})

test_that("R9: a surface layer designated C is a topsoil", {
  skip_on_cran()
  f <- soilKey:::.za_identify(.za_P(.za_L(0, 30, designation = "C"), .za_L(30, 50, designation = "R")))
  expect_identical(f$code, c("OA", "HRK"))
})
