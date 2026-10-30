# v0.9.315: WRB 2022 follow-ups read from the text: Rendzic (Ch 5, p. 145),
# calcaric material and Endocalcaric (Ch 3.3.3, p. 86; Ch 5, p. 129), and the
# texture qualifiers of one slash group used together with specifiers (Ch 2.1,
# p. 24-25; Ch 2.3.1, p. 27). Small made-up pedons; no third-party data.

.fu_ped <- function(..., site = list(id = "made-up")) {
  hz <- data.frame(..., stringsAsFactors = FALSE)
  PedonRecord$new(site = site, horizons = hz)
}

# A mollic horizon 0-30 cm (two layers) over a third layer; `caco3`, `eff`,
# `desg3` set the layers' carbonates and the designation of the third.
.fu_mollic <- function(caco3 = c(10, 12, NA), eff = NA_character_, desg3 = "Ck",
                       site = list(id = "made-up"), n = 3) {
  i <- seq_len(n)
  .fu_ped(designation = c("Ah1", "Ah2", desg3, "C2")[i],
          top_cm = c(0, 15, 30, 60)[i], bottom_cm = c(15, 30, 60, 90)[i],
          caco3_pct = caco3[i], effervescence_class = eff,
          oc_pct = c(3, 2, 0.5, 0.2)[i], clay_pct = 25, silt_pct = 40, sand_pct = 35,
          munsell_hue_moist = "10YR", munsell_value_moist = c(2, 2, 5, 6)[i],
          munsell_chroma_moist = c(1, 2, 4, 4)[i], munsell_value_dry = c(3, 3, 6, 7)[i],
          bs_pct = 90, ph_h2o = 7.8, structure_grade = "moderate",
          structure_type = "granular", site = site)
}


# ---- Rendzic (Ch 5, p. 145) ---------------------------------------------------

test_that("v0.9.315 Rendzic: a mollic horizon containing or directly over >= 40% CCE", {
  skip_on_cran()
  over <- .fu_mollic(caco3 = c(10, 12, 55))
  expect_true(isTRUE(mollic(over)$passed))
  expect_true(isTRUE(qual_rendzic(over)$passed))
  expect_equal(qual_rendzic(over)$layers, 1:2)
  expect_true(isTRUE(qual_rendzic(.fu_mollic(caco3 = c(45, 12, 20)))$passed))  # contains
  # 2 to < 40% CCE is calcaric material but not enough (was TRUE: any >= 2%)
  expect_false(qual_rendzic(.fu_mollic(caco3 = c(10, 12, 20)))$passed)
  # >= 40% deeper down, not directly under the mollic horizon
  deep <- .fu_mollic(caco3 = c(10, 12, 5, 60), desg3 = "Bw", n = 4)
  expect_false(qual_rendzic(deep)$passed)
})

test_that("v0.9.315 Rendzic: undecided without the CCE, not when the HCl reaction is not visible", {
  skip_on_cran()
  # strong effervescence: calcaric material, but how much carbonate is unknown
  r <- qual_rendzic(.fu_mollic(caco3 = c(10, 12, NA), eff = c(NA, NA, "strong")))
  expect_true(is.na(r$passed))
  # slight effervescence is not calcaric material (Ch 3.3.3, p. 86)
  expect_false(qual_rendzic(.fu_mollic(caco3 = c(10, 12, NA), eff = c(NA, NA, "slight")))$passed)
  # nothing described below the mollic horizon
  expect_true(is.na(qual_rendzic(.fu_mollic(caco3 = c(10, 12), n = 2))$passed))
  # no mollic horizon
  nom <- .fu_mollic(caco3 = c(10, 12, 55))
  nom$horizons$oc_pct <- 0.2
  expect_false(isTRUE(mollic(nom)$passed))
  expect_false(isTRUE(qual_rendzic(nom)$passed))
})

test_that("v0.9.315 Rendzic: calcareous rock read from its CCE or the rock named", {
  skip_on_cran()
  lime <- .fu_mollic(caco3 = c(10, 12, NA), desg3 = "R",
                     site = list(id = "made-up", parent_material = "Jurassic limestone"))
  expect_true(isTRUE(qual_rendzic(lime)$passed))
  # a rock with no CCE and no carbonate rock named is undecided
  rock <- .fu_mollic(caco3 = c(10, 12, NA), desg3 = "R",
                     site = list(id = "made-up", parent_material = "granite"))
  expect_true(is.na(qual_rendzic(rock)$passed))
  # a measured CCE decides for the rock too
  marl <- .fu_mollic(caco3 = c(10, 12, 30), desg3 = "R",
                     site = list(id = "made-up", parent_material = "limestone"))
  expect_false(qual_rendzic(marl)$passed)
})


# ---- calcaric material and Endocalcaric (Ch 3.3.3, p. 86; Ch 5, p. 129) ---------

test_that("v0.9.315 calcaric material reads the HCl reaction where CaCO3 is not measured", {
  skip_on_cran()
  mk <- function(caco3, eff)
    .fu_ped(designation = c("A", "B", "C"), top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 110),
            caco3_pct = caco3, effervescence_class = eff, clay_pct = 20, silt_pct = 40,
            sand_pct = 40)
  r <- calcaric_material(mk(c(NA, NA, NA), c("none", "slight", "moderate")))
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3L)
  expect_false(calcaric_material(mk(c(NA, NA, NA), c("none", "slight", "none")))$passed)
  expect_true(is.na(calcaric_material(mk(c(NA, NA, NA), c("none", NA, "none")))$passed))
  # a measured CCE decides over the field reaction
  expect_false(calcaric_material(mk(c(1, 1, 1), c("strong", "strong", "strong")))$passed)
  expect_true(isTRUE(calcaric_material(mk(c(0, 0, 3), NA))$passed))
})

test_that("v0.9.315 Endocalcaric: the HCl reaction, depths from the mineral soil surface", {
  skip_on_cran()
  mk <- function(caco3, eff, top = c(0, 30, 60), bottom = c(30, 60, 110),
                 desg = c("A", "B", "C"), oc = 1)
    .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, caco3_pct = caco3,
            effervescence_class = eff, oc_pct = oc, clay_pct = 20, silt_pct = 40,
            sand_pct = 40)
  # CaCO3 measured above (no calcic horizon), the HCl reaction below
  p <- mk(c(0, 0, NA), c(NA, NA, "strong"))
  expect_false(isTRUE(calcic(p)$passed))
  expect_true(isTRUE(qual_endocalcaric(p)$passed))           # was FALSE: CaCO3 only
  expect_false(qual_endocalcaric(mk(c(0, 0, NA), c(NA, NA, "slight")))$passed)
  # a layer that might be calcaric from 30 cm leaves it undecided
  expect_true(is.na(qual_endocalcaric(mk(c(0, NA, NA), c(NA, NA, "strong")))$passed))
  # no deep layer known, one might exist: undecided, not absent
  expect_true(is.na(qual_endocalcaric(mk(c(0, 0, NA), c(NA, NA, NA)))$passed))
  # 20 cm of organic material on top: 60-110 cm starts 40 cm below the mineral
  # soil surface, so not Endo- (was TRUE, counted from 0 cm)
  org <- mk(c(NA, 0, NA), c(NA, NA, "strong"), top = c(0, 20, 60), bottom = c(20, 60, 110),
            desg = c("Oa", "A", "C"), oc = c(30, 1, 1))
  expect_false(qual_endocalcaric(org)$passed)
})


# ---- texture qualifiers of one slash group (Ch 2.1, p. 24-25; Ch 2.3.1, p. 27) --

.fu_tex <- function(top = c(0, 30, 60), bottom = c(30, 60, 110),
                    clay = c(30, 30, 45), silt = c(55, 55, 45),
                    desg = c("A", "E", "Bt")) {
  .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, clay_pct = clay,
          silt_pct = silt, sand_pct = 100 - clay - silt, oc_pct = 1)
}

test_that("v0.9.315 specifier of a layer qualifier (Ch 2.3.1 rule 2, p. 27)", {
  skip_on_cran()
  sp <- function(t, b, lim = NA_real_, ms = 0)
    .wrb_layer_specifier(data.frame(top_cm = t, bottom_cm = b), seq_along(t), ms, lim)
  expect_identical(sp(0, 40), "Epi")
  expect_identical(sp(60, 100), "Endo")
  expect_identical(sp(60, 150), "Endo")
  expect_identical(sp(0, 60), "Ano")
  expect_identical(sp(10, 70), "Amphi")
  expect_identical(sp(10, 120), "Kato")
  expect_identical(sp(0, 120), "Panto")
  expect_identical(sp(0, 80, lim = 80), "Panto")          # down to a limiting layer
  expect_identical(sp(c(0, 60), c(20, 90)), "Poly")
  expect_true(is.na(sp(c(0, 30), c(20, 45))))             # Epi- or Poly-: not chosen
  expect_identical(sp(20, 80, ms = 20), "Ano")            # from the mineral soil surface
})

test_that("v0.9.315 texture group: both qualifiers with specifiers, top to bottom (p. 26 example)", {
  skip_on_cran()
  p <- .fu_tex()                                   # silty clay loam 0-60, silty clay 60-110
  expect_true(isTRUE(qual_loamic(p)$passed))
  expect_true(isTRUE(qual_clayic(p)$passed))
  on <- resolve_wrb_qualifiers(p, "LV", specifiers = TRUE)$supplementary
  expect_identical(on[1:2], c("Anoloamic", "Endoclayic"))
  expect_false(any(c("Clayic", "Loamic") %in% on))
  # without specifiers only the first that applies in the list (Clayic)
  off <- resolve_wrb_qualifiers(p, "LV", specifiers = FALSE)$supplementary
  expect_true("Clayic" %in% off)
  expect_false(any(grepl("oamic$", off)))
})

test_that("v0.9.315 texture group: only the first without a decided specifier", {
  skip_on_cran()
  grp <- function(p) .wrb_texture_group_specified(
    p, c("Clayic", "Loamic"),
    list(Clayic = qual_clayic(p)$layers, Loamic = qual_loamic(p)$layers))
  expect_identical(unname(grp(.fu_tex())), c("Endoclayic", "Anoloamic"))
  # a limiting layer starting < 60 cm: no subqualifier
  rk <- .fu_tex(top = c(0, 25, 55), bottom = c(25, 55, 70), clay = c(30, 45, NA),
                silt = c(55, 45, NA), desg = c("A", "Bt", "R"))
  expect_null(grp(rk))
  # a layer of unknown texture within 100 cm
  unk <- .fu_tex(clay = c(30, NA, 45))
  expect_null(grp(unk))
  # the description stops above 100 cm
  short <- .fu_tex(bottom = c(30, 60, 90))
  expect_null(grp(short))
  expect_identical(resolve_wrb_qualifiers(short, "LV", specifiers = TRUE)$supplementary[1],
                   "Clayic")
})


# ---- Technosols lists (Ch 4, p. 97) ---------------------------------------------

test_that("v0.9.315 Technosols: the lists of the text, two supplementary columns read in order", {
  skip_on_cran()
  q <- yaml::read_yaml(system.file("rules/wrb2022/qualifiers.yaml", package = "soilKey"))
  tc <- q$rsg_qualifiers$TC
  expect_identical(tc$supplementary[1], "Arenic/Clayic/Loamic/Siltic")
  expect_false(any(c("Arenic/Clayic/Sulfidic", "Loamic/Siltic",
                     "Anthraquic/Irragric/Transportic", "Hortic/Plaggic/Pretic/Vitric")
                   %in% tc$supplementary))
  expect_true("Anthraquic/Irragric/Hortic/Plaggic/Pretic/Terric" %in% tc$supplementary)
  expect_identical(utils::tail(tc$supplementary, 6),
                   c("Sulfidic", "Tephric", "Thionic", "Toxic", "Transportic", "Vitric"))
  expect_identical(tc$principal[1:3], c("Ekranic/Thyric", "Linic", "Urbic"))
})


# ---- mollic and umbric horizons, criterion 6a (p. 53, p. 70) --------------------

.fu_m6 <- function(top, bottom, desg, oc = c(3, 2, 0.5, 0.2)[seq_along(top)], cem = NA,
                   caco3 = NA, bs = 90) {
  n <- length(top)
  .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, oc_pct = oc,
          cementation_class = cem, caco3_pct = caco3, clay_pct = 25, silt_pct = 40,
          sand_pct = 35, munsell_hue_moist = "10YR",
          munsell_value_moist = c(2, 2, 5, 6)[seq_len(n)],
          munsell_chroma_moist = c(1, 2, 4, 4)[seq_len(n)],
          munsell_value_dry = c(3, 3, 6, 7)[seq_len(n)], bs_pct = bs,
          ph_h2o = if (bs >= 50) 7 else 4.8)
}

test_that("v0.9.315 mollic and umbric: >= 10 cm directly over continuous rock (6a)", {
  skip_on_cran()
  on_rock <- .fu_m6(c(0, 15), c(15, 40), c("Ah", "R"), oc = c(3, NA))
  m <- mollic(on_rock)
  expect_true(isTRUE(m$passed))                       # was FALSE (>= 20 cm only)
  expect_true(m$evidence$thickness$details$criterion_6a)
  expect_equal(m$layers, 1L)
  expect_true(isTRUE(umbric_horizon(.fu_m6(c(0, 15), c(15, 40), c("Ah", "R"),
                                           oc = c(3, NA), bs = 30))$passed))
  # < 10 cm: no; the rock is not part of the horizon, so it is not undecided
  expect_false(mollic(.fu_m6(c(0, 8), c(8, 40), c("Ah", "R"), oc = c(3, NA)))$passed)
  # over a layer that 6a does not name
  expect_false(mollic(.fu_m6(c(0, 15), c(15, 40), c("Ah", "Bw"), oc = c(3, 0.3)))$passed)
  # >= 20 cm needs nothing below
  expect_true(isTRUE(mollic(.fu_m6(c(0, 15), c(15, 30), c("Ah1", "Ah2"), oc = c(3, 2)))$passed))
})

test_that("v0.9.315 mollic 6a: petrocalcic horizon below; undecided cemented layers", {
  skip_on_cran()
  pk <- .fu_m6(c(0, 15), c(15, 40), c("Ah", "Bkm"), oc = c(3, 0.3),
               cem = c(NA, "strongly"), caco3 = c(NA, 30))
  expect_true(isTRUE(petrocalcic(pk)$passed))
  expect_true(isTRUE(mollic(pk)$passed))
  # a cemented layer whose petro* horizon cannot be decided
  und <- .fu_m6(c(0, 15), c(15, 40), c("Ah", "Bkm"), oc = c(3, 0.3))
  expect_true(is.na(mollic(und)$passed))
  # recorded as not cemented, or a designation without the suffix m: no petro*
  expect_false(mollic(.fu_m6(c(0, 15), c(15, 40), c("Ah", "Bk"), oc = c(3, 0.3),
                             cem = c(NA, "weakly")))$passed)
  # nothing described below a horizon of 10 to < 20 cm
  expect_true(is.na(mollic(.fu_m6(0, 15, "Ah", oc = 3))$passed))
})


# ---- Arenic and Clayic read as Loamic (Ch 5, p. 128, p. 130) ----------------------

test_that("v0.9.315 Arenic and Clayic: depths from the mineral soil surface, within 100 cm", {
  skip_on_cran()
  tx <- function(top, bottom, clay, silt, desg, oc = 1)
    .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, clay_pct = clay,
            silt_pct = silt, sand_pct = 100 - clay - silt, oc_pct = oc)
  # 30 cm of organic material on top: the sand of 90-130 cm lies within 100 cm
  # of the mineral soil surface (was FALSE, counted from 0 cm)
  org <- tx(c(0, 30, 90), c(30, 90, 130), c(NA, 20, 5), c(NA, 40, 5),
            c("Oa", "A", "C"), oc = c(30, 1, 0.2))
  expect_true(isTRUE(qual_arenic(org)$passed))
  # clay of 85-130 cm: 15 cm within 100 cm (was TRUE, the 45 cm unclipped)
  deep <- tx(c(0, 30, 85), c(30, 85, 130), c(20, 25, 50), c(40, 40, 30), c("A", "Bw", "Bt"))
  expect_false(qual_clayic(deep)$passed)
})

test_that("v0.9.315 Clayic: sandy clay is sand > 45%; the limiting-layer alternative; undecided", {
  skip_on_cran()
  tx <- function(clay, silt, top = c(0, 30), bottom = c(30, 70), desg = c("A", "Bt"))
    .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, clay_pct = clay,
            silt_pct = silt, sand_pct = 100 - clay - silt, oc_pct = 1)
  # clay 36%, sand 45%: clay loam (Table 8.39, p. 182), not sandy clay (was TRUE)
  expect_false(qual_clayic(tx(c(36, 36), c(19, 19)))$passed)
  expect_true(isTRUE(qual_loamic(tx(c(36, 36), c(19, 19)))$passed))
  expect_true(isTRUE(qual_clayic(tx(c(36, 36), c(18, 18)))$passed))   # sand 46%
  # the major part above a limiting layer starting > 10 and < 60 cm
  rk <- tx(c(45, 25, NA), c(30, 40, NA), top = c(0, 15, 25), bottom = c(15, 25, 50),
           desg = c("A", "Bw", "R"))
  expect_equal(.barrier_top_cm(rk), 25)
  expect_true(isTRUE(qual_clayic(rk)$passed))                         # 15 of 25 cm (was FALSE)
  # no texture recorded: undecided (was FALSE)
  expect_true(is.na(qual_clayic(tx(c(NA, NA), c(NA, NA)))$passed))
})


# ---- Endodolomitic from the mineral soil surface (Ch 5, p. 131) ------------------

test_that("v0.9.315 Endodolomitic: depths from the mineral soil surface", {
  skip_on_cran()
  mk <- function(top, bottom, desg, oc)
    .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, oc_pct = oc,
            caco3_pct = c(NA, 0, 0, 8)[seq_along(top)], clay_pct = 20, silt_pct = 40,
            sand_pct = 40)
  # dolomitic C from 60 cm: Endo- without organic layers on top
  plain <- mk(c(0, 30, 60), c(30, 60, 110), c("A", "Bw", "Ckdo"), oc = 1)
  plain$horizons$caco3_pct <- c(0, 0, 8)
  expect_true(isTRUE(qual_endodolomitic(plain)$passed))
  # 20 cm of organic material on top: 60-110 cm starts 40 cm below the mineral
  # soil surface, so not Endo- (was TRUE, counted from 0 cm)
  org <- mk(c(0, 20, 40, 60), c(20, 40, 60, 110), c("Oa", "A", "Bw", "Ckdo"),
            oc = c(30, 1, 1, 1))
  expect_false(qual_endodolomitic(org)$passed)
})


# ---- Hypercalcic (Ch 5, p. 129; Ch 2.1, p. 23) ------------------------------------

test_that("v0.9.315 Hypercalcic: every layer of the calcic horizon >= 50%, from the mineral soil surface", {
  skip_on_cran()
  mk <- function(caco3, sec = c(0, 20, 20, 0), top = c(0, 30, 45, 60),
                 bottom = c(30, 45, 60, 110), desg = c("A", "Bk1", "Bk2", "C"), oc = 1)
    .fu_ped(designation = desg, top_cm = top, bottom_cm = bottom, caco3_pct = caco3,
            secondary_carbonates_pct = sec, oc_pct = oc, clay_pct = 20, silt_pct = 40,
            sand_pct = 40)
  p <- mk(c(5, 55, 60, 10))
  expect_true(isTRUE(calcic(p)$passed))
  expect_true(isTRUE(qual_hypercalcic(p)$passed))
  # one layer of the calcic horizon below 50% (was TRUE: any layer)
  expect_false(qual_hypercalcic(mk(c(5, 55, 30, 10)))$passed)
  # 20 cm of organic material on top: a calcic horizon starting at 105 cm
  # starts 85 cm below the mineral soil surface (was FALSE, counted from 0 cm)
  org <- mk(c(NA, 5, 55, 60), sec = c(NA, 0, 20, 20), top = c(0, 20, 105, 120),
            bottom = c(20, 105, 120, 140), desg = c("Oa", "A", "Bk1", "Bk2"),
            oc = c(30, 1, 0.5, 0.3))
  expect_true(isTRUE(calcic(org)$passed))
  expect_true(isTRUE(qual_hypercalcic(org)$passed))
  # an undecided calcic horizon: undecided (was FALSE) ...
  und <- mk(c(NA, NA, NA, NA), sec = NA)
  expect_true(is.na(calcic(und)$passed))
  expect_true(is.na(qual_hypercalcic(und)$passed))
  # ... and FALSE when no layer within reach has, or may have, 50%
  expect_false(qual_hypercalcic(mk(c(5, 30, 30, 30), sec = NA))$passed)
})


# ---- depth specifiers of single qualifiers (Ch 2.3.1 rules 1 and 2, p. 27) ------

test_that("v0.9.315 .compute_depth_specifier follows the rule its Chapter 5 figure names", {
  skip_on_cran()
  ped <- function(top, bottom, desg = NULL, oc = 1) {
    hz <- data.frame(top_cm = top, bottom_cm = bottom, oc_pct = oc)
    if (!is.null(desg)) hz$designation <- desg
    PedonRecord$new(site = list(id = "made-up"), horizons = hz)
  }
  cds <- .compute_depth_specifier
  # rule 2, from the mineral soil surface: 20 cm of organic material on top
  org <- ped(c(0, 20, 50, 80), c(20, 50, 80, 130), c("Oa", "A", "Bk", "C"),
             oc = c(30, 1, 0.5, 0.2))
  expect_equal(cds(org, 2:3, "Calcic"), "Ano")        # 20-80 cm = 0-60 cm below
  # rule 1 (Leptic: Epi- and Endo- only), the upper limit of the rock, from the
  # soil surface
  rk <- ped(c(0, 30, 70), c(30, 70, 100), c("A", "Bw", "R"))
  expect_equal(cds(rk, 3L, "Leptic"), "Endo")
  # a limiting layer starting <= 50 cm: Epi- for the qualifier that refers to
  # it, no specifier for the others
  sh <- ped(c(0, 20, 40), c(20, 40, 100), c("A", "Bw", "R"))
  expect_equal(cds(sh, 3L, "Leptic"), "Epi")
  expect_equal(cds(sh, 1:2, "Calcic"), "")
  # no figure in Chapter 5: no specifier
  expect_false("Cambic" %in% .WRB_DEPTH_SPECIFIABLE)
  expect_false("Retic" %in% .WRB_DEPTH_SPECIFIABLE)
  expect_true(all(c("Clayic", "Loamic", "Siltic") %in% .WRB_DEPTH_SPECIFIABLE))
  # rule 1 points
  expect_identical(.wrb_point_specifier(c(30, 70)), "Amphi")
  expect_identical(.wrb_point_specifier(c(30, 70), only = c("Epi", "Endo")), NA_character_)
  expect_identical(.wrb_point_specifier(30), "Epi")
})

test_that("v0.9.315 specifiers = TRUE: a single texture qualifier gets its rule-2 specifier", {
  skip_on_cran()
  p <- .fu_tex(clay = c(30, 30, 30), silt = c(55, 55, 55))   # silty clay loam 0-110
  on <- resolve_wrb_qualifiers(p, "LV", specifiers = TRUE)$supplementary
  expect_identical(on[1], "Pantoloamic")
  expect_identical(resolve_wrb_qualifiers(p, "LV", specifiers = FALSE)$supplementary[1],
                   "Loamic")
})
