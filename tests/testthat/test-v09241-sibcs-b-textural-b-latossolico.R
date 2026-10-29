# v0.9.241: the B textural and the B latossolico are tested by the items of
# the book (SiBCS 2018, Cap 2, p. 54-59).
#
# B_textural() was the WRB argic horizon: a layer with 6 percentage points of
# clay, a ratio of 1.4 or 20 points more than the least clayey layer anywhere
# above it. Item (h) of the book had been joined to it, and items (f), (g)
# and (i) were not tested. B_latossolico() asked its 50 cm of each layer on
# its own, and since v0.9.61 a B textural did not exclude it unless its clay
# films were common or more. The Neossolos step set a diagnostic B aside when
# the profile had carater fluvico.

.bx <- function(designation, top, bottom, clay, silt = 20, cec = NA_real_, ...) {
  n <- length(designation)
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    clay_pct = clay, silt_pct = rep_len(silt, n),
    sand_pct = 100 - clay - rep_len(silt, n), cec_cmol = rep_len(cec, n), ...)
  PedonRecord$new(site = list(id = "bx", country = "BR", lat = -20, lon = -44),
                  horizons = ensure_horizon_schema(hz))
}

# ---- the readers --------------------------------------------------------------

test_that("the texture francoarenosa or finer is told from the silt and the clay", {
  expect_equal(.francoarenosa_or_finer(c(15, 5, 10), c(10, 10, 5)),
               c(TRUE, FALSE, FALSE))
  # 15% of clay or more: so whatever the silt, which may not be recorded
  expect_true(.francoarenosa_or_finer(NA, 20))
  expect_true(is.na(.francoarenosa_or_finer(NA, 10)))
})

test_that("the distinctness of a transition tells whether it takes 7.5 cm or less", {
  expect_true(all(.boundary_within_7_5_cm(c("abrupt", "clear", "Abrupta", "Clara"))))
  expect_false(any(.boundary_within_7_5_cm(c("gradual", "diffuse", "Gradual", "Difusa"))))
  # a maximum: a range counts by its wider end
  expect_false(.boundary_within_7_5_cm("Clara a gradual"))
  expect_true(all(is.na(.boundary_within_7_5_cm(c(NA, "", "ondulada")))))
})

test_that("cerosidade above 'pouca e fraca' is read as a maximum", {
  over <- .films_above_few_and_weak(
    c("Comum", "Abundante", "many", "Pouca", "Pouca a comum", "few"),
    c("Fraca", "Fraca", NA, "Moderada", "Fraca", "Fraca a moderada"))
  expect_true(all(over))
  under <- .films_above_few_and_weak(
    c("Pouca", "few", "Muito pouca", "Ausente", NA, "", "Pouca"),
    c("Fraca", "weak", "Fraca", NA, "Moderada", "Forte", NA))
  expect_false(any(under))
})

test_that("the number of the material is read in Arabic and in Roman numerals", {
  expect_equal(.desg_material_sibcs(c("Bt", "2Bt1", "IIBt", "III(B)2", "IIIC", "A", NA)),
               c(1L, 2L, 2L, 3L, 3L, 1L, NA))
  # an I that is part of no number: "IC" is the C of the first material
  expect_equal(.desg_material_sibcs("IC1"), 1L)
})

test_that("the thickness of adjoining layers is that of the horizon", {
  h <- data.frame(top_cm = c(0, 20, 45, 80, 130), bottom_cm = c(20, 45, 80, 115, 200))
  # layers 2 and 3 adjoin (25 + 35 cm); layer 5 does not adjoin layer 3
  r <- test_minimum_thickness(h, 50, c(2, 3, 5), adjoining = TRUE)
  expect_true(r$passed)
  expect_equal(r$layers, c(2L, 3L, 5L))
  r <- test_minimum_thickness(h, 65, c(2, 3, 5), adjoining = TRUE)
  expect_equal(r$layers, 5L)
  # a layer between them that is no candidate breaks the horizon
  expect_false(test_minimum_thickness(h, 50, c(2, 4), adjoining = TRUE)$passed)
  # each layer on its own, as before, when not asked
  expect_equal(test_minimum_thickness(h, 50, c(2, 3, 5))$layers, 5L)
  expect_false(test_minimum_thickness(h, 50, integer(0), adjoining = TRUE)$passed)
})

# ---- B textural: item (h) -----------------------------------------------------

test_that("item (h): the means of footnote 4 are arithmetic", {
  # A 20%; BA 28% (10 cm) and Bt 38% (70 cm) in the control section. The
  # arithmetic mean of the B is 33 (ratio 1.65, below 1.70); weighted by the
  # thickness it was 36.75 (1.84).
  p <- .bx(c("A", "BA", "Bt"), c(0, 20, 30), c(20, 30, 100), c(20, 28, 38))
  rt <- test_ratio_textural_sibcs(p$horizons)
  expect_equal(rt$details$b_mean_clay, 33)
  expect_equal(round(rt$details$ratio, 2), 1.65)
  expect_false(rt$passed)
  expect_false(isTRUE(B_textural(p)$passed))
  # with 40% in the Bt the mean is 34 and the ratio 1.70: "maior que 1,70"
  q <- .bx(c("A", "BA", "Bt"), c(0, 20, 30), c(20, 30, 100), c(20, 28, 40))
  expect_false(isTRUE(B_textural(q)$passed))
  q <- .bx(c("A", "BA", "Bt"), c(0, 20, 30), c(20, 30, 100), c(20, 28, 41))
  r <- B_textural(q)
  expect_true(isTRUE(r$passed))
  expect_true(r$evidence$item_h)
  expect_equal(r$layers, 2:3)
})

test_that("item (h) reads the designations with a material number, '(B)' and 'Btc'", {
  for (d in list(c("A", "IIBt"), c("A", "2Bt"), c("A1", "(B)2"), c("A", "Btc"))) {
    p <- .bx(d, c(0, 20), c(20, 80), c(15, 35))
    expect_true(isTRUE(test_ratio_textural_sibcs(p$horizons)$passed),
                info = paste(d, collapse = " "))
  }
  # BC is the transitional horizon, and is left out of the mean of the B
  p <- .bx(c("A", "Bt", "BC"), c(0, 20, 40), c(20, 40, 60), c(15, 30, 10))
  expect_equal(test_ratio_textural_sibcs(p$horizons)$details$b_mean_clay, 30)
})

# ---- B textural: item (g) -----------------------------------------------------

test_that("item (g): an abrupt change from the A to the B, within 7.5 cm", {
  skip_on_cran()
  # A 40% and B 60%: ratio 1.50, below the 1.70 of item (h); 20 points more
  g <- function(boundary) .bx(c("A1", "A3", "Bt"), c(0, 15, 30), c(15, 30, 90),
                             c(40, 40, 60),
                             boundary_distinctness = c("clear", boundary, NA))
  for (b in c("abrupt", "clear", "Abrupta", "Clara")) {
    r <- B_textural(g(b))
    expect_true(isTRUE(r$passed), info = b)
    expect_true(r$evidence$item_g, info = b)
    expect_false(r$evidence$item_h, info = b)
  }
  for (b in c("gradual", "diffuse", "Gradual", "Difusa"))
    expect_false(isTRUE(B_textural(g(b))$passed), info = b)
  # not recorded: the two layers are compared
  expect_true(isTRUE(B_textural(g(NA_character_))$passed))
  # 19 points are not 20
  p <- .bx(c("A", "Bt"), c(0, 30), c(30, 90), c(40, 59))
  expect_false(isTRUE(B_textural(p)$passed))
  # under 200 g/kg in the A, the B has twice as much; that is item (h) too
  p <- .bx(c("A", "Bt"), c(0, 30), c(30, 90), c(12, 24))
  expect_true(B_textural(p)$evidence$item_g)
})

test_that("a jump of clay between two B horizons, or to a C, is no item (g)", {
  p <- .bx(c("A", "B1", "B2"), c(0, 20, 50), c(20, 50, 120), c(40, 45, 70))
  r <- B_textural(p)
  expect_false(r$evidence$item_g)
  expect_false(isTRUE(r$passed))
  # the test of the WRB property compares every pair of adjoining layers;
  # since v0.9.244 the SiBCS attribute is the transition from the A or E to
  # the B (v0.9.312: with the abrupt transition of its criterion 5)
  q <- p$clone(deep = TRUE); q$horizons$boundary_distinctness <- "abrupt"
  expect_true(isTRUE(abrupt_textural_difference(q)$passed))
  expect_false(isTRUE(mudanca_textural_abrupta(p)$passed))
})

# ---- B textural: item (f) -----------------------------------------------------

test_that("item (f): an E horizon above a B with more clay", {
  p <- .bx(c("A", "E", "Bt"), c(0, 15, 40), c(15, 40, 100), c(20, 18, 30))
  r <- B_textural(p)
  expect_true(isTRUE(r$passed))
  expect_true(r$evidence$item_f)
  expect_false(r$evidence$item_h)
  expect_false(r$evidence$item_g)
  # the same clay with no E
  q <- .bx(c("A", "AB", "Bt"), c(0, 15, 40), c(15, 40, 100), c(20, 18, 30))
  expect_false(isTRUE(B_textural(q)$passed))
  # an E above a B with no more clay than the A
  q <- .bx(c("A", "E", "Bt"), c(0, 15, 40), c(15, 40, 100), c(30, 18, 30))
  expect_false(isTRUE(B_textural(q)$passed))
})

# ---- B textural: item (i) -----------------------------------------------------

.cer <- function(clay_b, type, grade, amount, strength, lower = FALSE) {
  # an A and three B sub-horizons with little more clay than the A; the
  # cerosidade in the first B, or only in the last when `lower`
  at <- if (lower) 4L else 2L
  f <- rep(NA_character_, 4); s <- f
  f[at] <- amount; s[at] <- strength
  .bx(c("A", "Bt1", "Bt2", "Bt3"), c(0, 20, 50, 90), c(20, 50, 90, 150),
      c(clay_b - 5, clay_b, clay_b, clay_b),
      structure_type = c("granular", rep(type, 3)),
      structure_grade = c("moderate", rep(grade, 3)),
      clay_films_amount = f, clay_films_strength = s)
}

test_that("item (i3): clayey B, blocky structure, cerosidade common and weak or few and moderate", {
  skip_on_cran()
  yes <- list(c("Comum", "Fraca"), c("Pouca", "Moderada"), c("Abundante", "Forte"),
              c("common", "weak"), c("few", "moderate"), c("Comum", NA))
  for (x in yes) {
    r <- B_textural(.cer(45, "Blocos subangulares", "Moderada", x[1], x[2]))
    expect_true(isTRUE(r$passed), info = paste(x, collapse = " / "))
    expect_equal(r$evidence$item_i, "i3", info = paste(x, collapse = " / "))
  }
  no <- list(c("Pouca", "Fraca"),            # "nao admitindo, portanto, cerosidade pouca e fraca"
             c("Pouca a comum", "Fraca"),    # a range counts by its lower end
             c("Pouca", "Fraca a moderada"),
             c("Pouca", NA), c("Ausente", NA), c(NA, NA))
  for (x in no)
    expect_false(isTRUE(B_textural(.cer(45, "Blocos subangulares", "Moderada", x[1], x[2]))$passed),
                 info = paste(x, collapse = " / "))
  # the structure of the item: prismatic or blocky, of moderate or strong grade
  expect_true(isTRUE(B_textural(.cer(45, "Prismatica", "Forte", "Comum", "Fraca"))$passed))
  expect_true(isTRUE(B_textural(.cer(45, "polyhedral", "strong", "common", "weak"))$passed))
  expect_false(isTRUE(B_textural(.cer(45, "Blocos subangulares", "Fraca", "Comum", "Moderada"))$passed))
  expect_false(isTRUE(B_textural(.cer(45, "Granular", "Forte", "Comum", "Moderada"))$passed))
  expect_false(isTRUE(B_textural(.cer(45, NA, NA, "Comum", "Moderada"))$passed))
})

test_that("items (i1) and (i2): B of texture media, cerosidade of moderate grade", {
  r <- B_textural(.cer(25, "Blocos angulares", "Forte", "Pouca", "Moderada"))
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$item_i, "i2")
  # common and weak is enough in a clayey B, not in one of texture media
  expect_false(isTRUE(B_textural(.cer(25, "Blocos angulares", "Forte", "Comum", "Fraca"))$passed))
  r <- B_textural(.cer(25, "Macica", NA, "Pouca", "Moderada"))
  expect_true(isTRUE(r$passed))
  expect_equal(r$evidence$item_i, "i1")
  expect_equal(B_textural(.cer(25, "Graos simples", NA, "few", "strong"))$evidence$item_i, "i1")
})

test_that("item (i) is asked of the upper part of the B", {
  expect_true(isTRUE(B_textural(.cer(45, "Blocos subangulares", "Moderada", "Comum", "Moderada"))$passed))
  expect_false(isTRUE(B_textural(.cer(45, "Blocos subangulares", "Moderada", "Comum", "Moderada",
                                      lower = TRUE))$passed))
})

# ---- B textural: item (j), no B, no designation, the morphological mode --------

test_that("item (j): with only a ploughed layer above, or a discontinuity, items (h) or (i) are needed", {
  skip_on_cran()
  g <- function(d) .bx(d, c(0, 30), c(30, 90), c(40, 60),
                       boundary_distinctness = c("abrupt", NA))
  expect_true(isTRUE(B_textural(g(c("A", "Bt")))$passed))            # item (g)
  for (d in list(c("Ap", "Bt"), c("A", "2Bt"), c("A", "IIBt"))) {
    r <- B_textural(g(d))
    expect_true(r$evidence$item_g, info = paste(d, collapse = " "))
    expect_true(r$evidence$item_j, info = paste(d, collapse = " "))
    expect_false(isTRUE(r$passed), info = paste(d, collapse = " "))
  }
  # item (h) is enough across a discontinuity
  p <- .bx(c("Ap", "IIBt"), c(0, 30), c(30, 90), c(20, 45))
  r <- B_textural(p)
  expect_true(r$evidence$item_j)
  expect_true(isTRUE(r$passed))
})

test_that("a clay increase between C layers is no B textural, and the Fluvisol needs no exception", {
  skip_on_cran()
  p <- make_fluvisol_canonical()           # Ah, C1, C2, C3, stratified
  expect_true(isTRUE(argic(p, lithic_discontinuity = FALSE)$passed))
  expect_true(isTRUE(carater_fluvico(p)$passed))
  r <- B_textural(p)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$reason, "no B horizon")
  expect_true(isTRUE(neossolo(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Neossolos")
})

test_that("a diagnostic B is one, with or without carater fluvico", {
  skip_on_cran()
  # the stratified profile above with its second and third layers described
  # as a B textural: the carater fluvico no longer sets the B aside
  p <- make_fluvisol_canonical()
  p$horizons$designation <- c("A", "E", "Bt", "C")
  expect_true(isTRUE(carater_fluvico(p)$passed))
  expect_true(isTRUE(B_textural(p)$passed))
  r <- neossolo(p)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$reason, "has B diagnostic horizon")
})

test_that("a profile with no designation at all is tested by the clay increase between layers", {
  p <- .bx(rep(NA_character_, 3), c(0, 30, 60), c(30, 60, 150), c(20, 35, 40))
  r <- B_textural(p)
  expect_true(isTRUE(r$passed))
  expect_match(r$notes, "no horizon designation")
  # layers named by numbers have a designation, and no B horizon
  q <- .bx(c("1", "2", "3"), c(0, 30, 60), c(30, 60, 150), c(20, 35, 40))
  expect_false(isTRUE(B_textural(q)$passed))
})

test_that("in the morphological mode the 't' stands for the B textural the data do not show", {
  skip_on_cran()
  old <- options(soilKey.morphological_inference = FALSE)
  on.exit(options(old), add = TRUE)
  for (d in list(c("A", "Bt"), c("A", "IIBt"), c("A1", "B2t"))) {
    p <- .bx(d, c(0, 20), c(20, 100), c(30, 38))      # ratio 1.27
    options(soilKey.morphological_inference = FALSE)
    expect_false(isTRUE(B_textural(p)$passed), info = paste(d, collapse = " "))
    options(soilKey.morphological_inference = TRUE)
    r <- B_textural(p)
    expect_true(isTRUE(r$passed), info = paste(d, collapse = " "))
    expect_equal(r$layers, 2L, info = paste(d, collapse = " "))
    expect_true(r$evidence$morphological_inference$passed)
  }
  p <- .bx(c("A", "Bw"), c(0, 20), c(20, 100), c(30, 38))
  expect_false(isTRUE(B_textural(p)$passed))
})

# ---- B latossolico ------------------------------------------------------------

.lat <- function(designation = c("A", "B1", "B21", "B22"),
                 top = c(0, 20, 45, 80), bottom = c(20, 45, 80, 115),
                 clay = c(45, 50, 52, 52), cec = c(8, 4, 4, 4), ...) {
  .bx(designation, top, bottom, clay, silt = 15, cec = cec, ...)
}

test_that("the 50 cm are those of the horizon, not of each sub-horizon", {
  skip_on_cran()
  p <- .lat()                                # B1 25 cm, B21 35 cm, B22 35 cm
  r <- B_latossolico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:4)
  expect_false(isTRUE(ferralic(p, min_thickness = 50, max_cec = 17)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Latossolos")
  # a sub-horizon that does not meet the requirements breaks the horizon
  q <- .lat(cec = c(8, 4, 12, 4))            # B21: 23 cmolc per kg of clay
  expect_false(isTRUE(B_latossolico(q)$passed))
  # two samples with a gap between them are no 50 cm
  q <- .lat(c("A", "B"), c(0, 60), c(20, 80), c(45, 52), c(8, 4))
  expect_false(isTRUE(B_latossolico(q)$passed))
})

test_that("only B horizons make the B latossolico, where the designations are recorded", {
  # an A of 40 cm and a Bw of 40 cm that meet the requirements: 40 cm of B
  p <- .lat(c("A", "Bw"), c(0, 40), c(40, 80), c(45, 50), c(4, 4))
  expect_false(isTRUE(B_latossolico(p)$passed))
  # a C of 100 cm that meets them is no B horizon
  p <- .lat(c("A", "C"), c(0, 30), c(30, 130), c(45, 50), c(8, 4))
  expect_false(isTRUE(B_latossolico(p)$passed))
  # with no designation the layers are taken as they come
  p <- .lat(rep(NA_character_, 2), c(0, 30), c(30, 130), c(45, 50), c(8, 4))
  expect_true(isTRUE(B_latossolico(p)$passed))
})

test_that("cerosidade above 'pouca e fraca' is no B latossolico (item h)", {
  skip_on_cran()
  f <- function(amount, grade) .lat(clay_films_amount = c(NA, rep(amount, 3)),
                                    clay_films_strength = c(NA, rep(grade, 3)))
  expect_true(isTRUE(B_latossolico(f("Pouca", "Fraca"))$passed))
  expect_true(isTRUE(B_latossolico(f("Ausente", NA))$passed))
  for (x in list(c("Comum", "Fraca"), c("Pouca", "Moderada"), c("Pouca a comum", "Fraca")))
    expect_false(isTRUE(B_latossolico(f(x[1], x[2]))$passed), info = paste(x, collapse = " / "))
})

test_that("the layers of a B textural are no B latossolico", {
  skip_on_cran()
  # low-activity clay throughout, and a relacao textural of 2.25
  p <- .lat(clay = c(20, 45, 45, 45), cec = c(4, 4, 4, 4))
  expect_true(isTRUE(B_textural(p)$passed))
  r <- B_latossolico(p)
  expect_false(isTRUE(r$passed))
  expect_true(isTRUE(r$evidence$B_textural$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Argissolos")
})

test_that("a B latossolico below a B textural makes no Latossolo", {
  skip_on_cran()
  # "o primeiro horizonte diagnostico de subsuperficie, a contar da
  # superficie, tem prevalencia" (Cap 4, p. 110). A relacao textural of 1.89
  # in the Bt, and a BC of 50 cm of low-activity clay, which is no layer of
  # the B textural and meets the requirements of the B latossolico.
  p <- .bx(c("A", "AB", "Bt1", "Bt2", "BC"), c(0, 20, 55, 115, 170),
           c(20, 55, 115, 170, 220), c(18, 28, 45, 42, 38), silt = 22,
           cec = c(8, 6, 5.5, 4.5, 4))
  expect_true(isTRUE(B_textural(p)$passed))
  bl <- B_latossolico(p)
  expect_true(isTRUE(bl$passed))
  expect_equal(bl$layers, 5L)
  r <- latossolo(p)
  expect_false(isTRUE(r$passed))
  expect_true(r$evidence$below_b_textural)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Argissolos")
})

test_that("the canonical Latossolo and Argissolo keep their horizons", {
  l <- make_latossolo_canonical()
  expect_true(isTRUE(B_latossolico(l)$passed))
  expect_false(isTRUE(B_textural(l)$passed))
  a <- make_argissolo_canonical()
  expect_true(isTRUE(B_textural(a)$passed))
  expect_false(isTRUE(B_latossolico(a)$passed))
})

# ---- B incipiente and the latossolic character ---------------------------------

test_that("the B incipiente is what is no B textural and no B latossolico of this system", {
  skip_on_cran()
  # two Bi of 30 cm with 16.5 cmolc per kg of clay: a B latossolico of 60 cm
  # (below 17), which the WRB ferralic horizon (below 16) was not
  p <- .bx(c("A", "Bi1", "Bi2"), c(0, 20, 50), c(20, 50, 80), c(38, 40, 40),
           silt = 15, cec = c(9, 6.6, 6.6))
  expect_true(isTRUE(B_latossolico(p)$passed))
  expect_false(isTRUE(ferralic(p)$passed))
  r <- B_incipiente(p)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$excluded_by$B_latossolico, 2:3)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Latossolos")
})

test_that("a B latossolico below a B textural makes the latossolic character", {
  # relacao textural of 2.25 in the upper B; 100 cm of low-activity clay below
  p <- .bx(c("A", "Bt1", "Bt2", "Bw1", "Bw2"), c(0, 20, 40, 60, 110),
           c(20, 40, 60, 110, 160), c(20, 45, 45, 45, 45), silt = 15,
           cec = c(4, 4, 4, 4, 4))
  bt <- B_textural(p)
  expect_true(isTRUE(bt$passed))
  expect_equal(bt$evidence$upper_layers, 2:3)
  r <- carater_latossolico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 4:5)
  # not when those layers have cerosidade above few and weak
  p$horizons$clay_films_amount <- c(NA, NA, NA, "Comum", "Comum")
  expect_false(isTRUE(carater_latossolico(p)$passed))
})

# ---- the loader ---------------------------------------------------------------

test_that("the BDsolos loader reads the distinctness and the topography of the transition", {
  q <- function(...) paste0(paste0('"', c(...), '"', collapse = ";"), ";")
  hdr <- q("Código PA", "UF", "Classificação Atual", "Símbolo Horizonte",
           "Profundidade Superior", "Profundidade Inferior",
           "Composição Granulométrica da terra fina - Argila (g/kg)",
           "Transição de horizonte subjacente - Topografia",
           "Transição de horizonte subjacente - Nitidez")
  rows <- c(q("77", "RS", "PLANOSSOLO HÁPLICO", "A",  "0",  "30", "150", "Plana", "Abrupta"),
            q("77", "RS", "PLANOSSOLO HÁPLICO", "Bt", "30", "90", "400", "Ondulada", "Gradual"),
            q("77", "RS", "PLANOSSOLO HÁPLICO", "C",  "90", "150", "300", "", ""))
  f <- tempfile(fileext = ".csv"); on.exit(unlink(f), add = TRUE)
  writeLines(c("BDsolos", "", hdr, rows), f, useBytes = FALSE)
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(f, verbose = FALSE)))
  h <- peds[[1]]$horizons
  expect_equal(h$boundary_distinctness, c("Abrupta", "Gradual", NA))
  expect_equal(h$boundary_topography, c("Plana", "Ondulada", NA))
  expect_equal(.boundary_within_7_5_cm(h$boundary_distinctness), c(TRUE, FALSE, NA))
})
