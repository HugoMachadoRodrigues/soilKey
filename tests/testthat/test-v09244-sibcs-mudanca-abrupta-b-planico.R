# v0.9.244: the mudanca textural abrupta and the B planico as the book states
# them.
#
# SiBCS 2018, Cap 1, p. 30-31: the mudanca textural abrupta is "um consideravel
# aumento no teor de argila em pequena distancia na zona de transicao entre o
# horizonte A ou E e o horizonte subjacente B", twice the clay when the A or E
# has less than 200 g/kg, 200 g/kg more otherwise, "em uma distancia vertical
# <= 7,5 cm". Cap 2, p. 65: the B planico is "subjacente a horizontes A ou E,
# apresentando mudanca textural abrupta ou transicao abrupta associada a
# relacao textural com valor dentro do especificado para o horizonte B
# textural (subitem h), porem calculado entre o primeiro sub-horizonte B e o
# horizonte imediatamente acima (A ou E)".
#
# mudanca_textural_abrupta() was the WRB abrupt textural difference: every pair
# of adjoining layers, in the order of the rows, with no distance. The B
# planico took any layer with such a jump, and did not know the second way.

.ab <- function(designation, top, bottom, clay, boundary = NA_character_, ...) {
  n <- length(designation)
  hz <- data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = bottom,
    clay_pct = clay, silt_pct = 20, sand_pct = 80 - clay,
    boundary_distinctness = rep_len(boundary, n), ...)
  PedonRecord$new(site = list(id = "ab", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}


# v0.9.312: the WRB abrupt textural difference reads its criterion 5 (a
# transition of 2 cm or less) from the boundary; these profiles record none
.with_abrupt <- function(p) {
  q <- p$clone(deep = TRUE); q$horizons$boundary_distinctness <- "abrupt"; q
}

# ---- the mudanca textural abrupta ------------------------------------------------

test_that("it is the transition from the A or E to the first B", {
  # under 200 g/kg in the A: twice as much in the B
  p <- .ab(c("A", "Bt"), c(0, 30), c(30, 90), c(12, 24))
  r <- mudanca_textural_abrupta(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2L)
  expect_false(isTRUE(mudanca_textural_abrupta(.ab(c("A", "Bt"), c(0, 30), c(30, 90), c(12, 23)))$passed))
  # 200 g/kg or more: 200 g/kg more ("de 220 para no minimo 420")
  expect_true(isTRUE(mudanca_textural_abrupta(.ab(c("A", "Bt"), c(0, 30), c(30, 90), c(22, 42)))$passed))
  expect_false(isTRUE(mudanca_textural_abrupta(.ab(c("A", "Bt"), c(0, 30), c(30, 90), c(22, 41)))$passed))
  # from an E
  expect_true(isTRUE(mudanca_textural_abrupta(.ab(c("A", "E", "Bt"), c(0, 15, 40), c(15, 40, 90), c(14, 10, 25)))$passed))
})

test_that("a jump between two B horizons, to a C, or below a layer that is no A or E, is none", {
  # the WRB property, which the function used to be, finds each of these
  bb <- .ab(c("A", "B1", "B2"), c(0, 20, 50), c(20, 50, 120), c(40, 45, 70))
  expect_true(isTRUE(abrupt_textural_difference(.with_abrupt(bb))$passed))
  expect_false(isTRUE(mudanca_textural_abrupta(bb)$passed))
  bc <- .ab(c("A", "Bw", "C"), c(0, 20, 60), c(20, 60, 120), c(20, 25, 50))
  expect_true(isTRUE(abrupt_textural_difference(.with_abrupt(bc))$passed))
  expect_false(isTRUE(mudanca_textural_abrupta(bc)$passed))
  # a C between the A and the B
  cb <- .ab(c("A", "C", "2Bt"), c(0, 20, 40), c(20, 40, 100), c(30, 10, 40))
  expect_false(isTRUE(mudanca_textural_abrupta(cb)$passed))
  # no B horizon
  expect_false(isTRUE(mudanca_textural_abrupta(.ab(c("A", "C1", "C2"), c(0, 20, 60), c(20, 60, 120), c(10, 30, 60)))$passed))
})

test_that("it is an increase: no clay over no clay is none", {
  expect_false(isTRUE(mudanca_textural_abrupta(.ab(c("A", "E", "Bh"), c(0, 10, 60), c(10, 60, 90), c(1, 0, 0)))$passed))
  expect_true(isTRUE(mudanca_textural_abrupta(.ab(c("A", "E", "Bt"), c(0, 10, 60), c(10, 60, 90), c(1, 0, 18)))$passed))
})

test_that("a BC is no first B sub-horizon (v0.9.246)", {
  skip_on_cran()
  # item (h) of the B textural leaves the BC out of the B, and the B planico
  # refers to it; a BC that comes before any other B was taken for the first
  bc <- .ab(c("A", "BC", "C"), c(0, 20, 50), c(20, 50, 120), c(12, 30, 30))
  r <- mudanca_textural_abrupta(bc)
  expect_false(isTRUE(r$passed))
  expect_true(is.na(r$evidence$first_b))
  for (d in c("B/C", "BC1", "BCg"))
    expect_false(isTRUE(mudanca_textural_abrupta(
      .ab(c("A", d, "C"), c(0, 20, 50), c(20, 50, 120), c(12, 30, 30)))$passed), info = d)
  # a BA is a B sub-horizon, and "Btc" is a Bt with concretions, not a BC
  expect_true(isTRUE(mudanca_textural_abrupta(
    .ab(c("A", "BA", "Bt"), c(0, 20, 50), c(20, 50, 120), c(12, 30, 40)))$passed))
  expect_true(isTRUE(mudanca_textural_abrupta(
    .ab(c("A", "Btc", "C"), c(0, 20, 50), c(20, 50, 120), c(12, 30, 30)))$passed))
  # with a BC between the A and the Bt, the Bt is the first B and the horizon
  # immediately above it is no A or E
  r <- mudanca_textural_abrupta(.ab(c("A", "BC", "Bt"), c(0, 20, 50), c(20, 50, 120), c(12, 14, 40)))
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$first_b, 3L)
  # the colour and the structure of a B planico in a BC make none
  p <- .ab(c("A", "BC", "C"), c(0, 20, 50), c(20, 50, 120), c(12, 30, 30),
           munsell_hue_moist = "10YR", munsell_value_moist = 4,
           munsell_chroma_moist = 2, structure_type = "Prismatica",
           structure_size = "Media")
  expect_false(isTRUE(B_planico(p)$passed))
})

test_that("the rows are read in the order of their depths", {
  p <- .ab(c("Bt", "A"), c(30, 0), c(90, 30), c(24, 12))
  r <- mudanca_textural_abrupta(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 1L)
})

test_that("the distance of 7.5 cm is read from the distinctness of the transition", {
  skip_on_cran()
  g <- function(b) .ab(c("A", "Bt"), c(0, 30), c(30, 90), c(12, 30), boundary = c(b, NA))
  for (b in c("abrupt", "clear", "Abrupta", "Clara"))
    expect_true(isTRUE(mudanca_textural_abrupta(g(b))$passed), info = b)
  for (b in c("gradual", "diffuse", "Gradual", "Difusa"))
    expect_false(isTRUE(mudanca_textural_abrupta(g(b))$passed), info = b)
  # not recorded: the two horizons are compared
  r <- mudanca_textural_abrupta(g(NA_character_))
  expect_true(isTRUE(r$passed))
  expect_true(is.na(r$evidence$within_7_5_cm))
})

test_that("with no clay in one of the two horizons the answer is NA, and a profile with no designation is tested as before", {
  p <- .ab(c("A", "Bt"), c(0, 30), c(30, 90), c(NA, 30))
  r <- mudanca_textural_abrupta(p)
  expect_true(is.na(r$passed))
  expect_equal(r$missing, "clay_pct")
  q <- .ab(rep(NA_character_, 3), c(0, 20, 50), c(20, 50, 120), c(40, 45, 70))
  expect_true(isTRUE(mudanca_textural_abrupta(q)$passed))
})

# ---- the B planico ---------------------------------------------------------------

.pl <- function(designation, top, bottom, clay, boundary = NA_character_,
                hue = "10YR", chroma = 2, type = "Prismatica") {
  n <- length(designation)
  .ab(designation, top, bottom, clay, boundary,
      munsell_hue_moist = rep_len(hue, n), munsell_value_moist = 4,
      munsell_chroma_moist = rep_len(chroma, n),
      structure_type = rep_len(type, n), structure_size = "Media")
}

test_that("the B planico is the first B sub-horizon, below an abrupt textural change", {
  skip_on_cran()
  p <- .pl(c("A", "E", "Bt1", "Bt2"), c(0, 15, 40, 70), c(15, 40, 70, 120), c(14, 10, 30, 35))
  r <- B_planico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3L)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Planossolos")
  # the canonical Planossolo
  expect_true(isTRUE(B_planico(make_planossolo_canonical())$passed))
})

test_that("its colour and its structure are asked of that first sub-horizon", {
  # the colour of item (a) only in the second B sub-horizon
  p <- .pl(c("A", "E", "Bt1", "Bt2"), c(0, 15, 40, 70), c(15, 40, 70, 120), c(14, 10, 30, 35),
           chroma = c(3, 3, 6, 2))
  r <- B_planico(p)
  expect_false(isTRUE(r$passed))
  expect_equal(r$evidence$cor_ok, 4L)
  # the structure of the definition only in the second
  q <- .pl(c("A", "E", "Bt1", "Bt2"), c(0, 15, 40, 70), c(15, 40, 70, 120), c(14, 10, 30, 35),
           type = c("Granular", "Granular", "Granular", "Prismatica"))
  expect_false(isTRUE(B_planico(q)$passed))
})

test_that("it is a type of B textural: a first B sub-horizon coarser than francoarenosa is none", {
  # the Bh of an Espodossolo, 4% of clay below an E with 1%: twice the clay,
  # an abrupt transition, a dark colour of low chroma and a massive structure
  sandy <- function(clay_b, silt_b) {
    p <- .pl(c("A1", "E", "Bh"), c(0, 30, 75), c(30, 75, 120), c(4, 1, clay_b),
             boundary = c("Clara", "Abrupta", NA), hue = "5YR", chroma = 2,
             type = c("Graos simples", "Graos simples", "Macica"))
    p$horizons$silt_pct <- c(9, 4, silt_b)
    p
  }
  p <- sandy(4, 7)
  expect_true(isTRUE(mudanca_textural_abrupta(p)$passed))
  r <- B_planico(p)
  expect_false(isTRUE(r$passed))
  expect_false(r$evidence$texture_ok)
  # francoarenosa: silt plus twice the clay of 30% or more
  expect_false(isTRUE(B_planico(sandy(6, 17))$passed))
  expect_true(isTRUE(B_planico(sandy(6, 18))$passed))
  expect_true(isTRUE(B_planico(sandy(15, 0))$passed))
})

test_that("a jump of clay inside the B is no B planico", {
  # A 40%, B1 45%, B2 70%: the jump is between the two B sub-horizons
  p <- .pl(c("A", "B1", "B2"), c(0, 20, 50), c(20, 50, 120), c(40, 45, 70))
  expect_true(isTRUE(abrupt_textural_difference(.with_abrupt(p))$passed))
  expect_false(isTRUE(B_planico(p)$passed))
})

test_that("an abrupt transition with the textural ratio of item (h) makes one too", {
  skip_on_cran()
  # A 25%, Bt 44%: 19 points and a ratio of 1.76, above the 1.70 of item (h)
  g <- function(b, clay_b = 44) .pl(c("A", "Bt"), c(0, 30), c(30, 90), c(25, clay_b),
                                    boundary = c(b, NA))
  expect_false(isTRUE(mudanca_textural_abrupta(g("Abrupta"))$passed))
  r <- B_planico(g("Abrupta"))
  expect_true(isTRUE(r$passed))
  expect_true(r$evidence$abrupt_transition_with_ratio)
  expect_equal(round(r$evidence$transition$ratio, 2), 1.76)
  expect_equal(r$evidence$transition$ratio_limit, 1.7)
  # a transition that is clear, or not recorded, is not abrupt
  expect_false(isTRUE(B_planico(g("Clara"))$passed))
  expect_false(isTRUE(B_planico(g(NA_character_))$passed))
  # a ratio of 1.68 is not above 1.70
  expect_false(isTRUE(B_planico(g("Abrupta", clay_b = 42))$passed))
  # the limits of item (h) by the clay of the horizon above
  lim <- function(a) .mudanca_abrupta_a_b(.ab(c("A", "Bt"), c(0, 30), c(30, 90), c(a, a + 5))$horizons)$ratio_limit
  expect_equal(c(lim(10), lim(15), lim(40), lim(41)), c(1.8, 1.7, 1.7, 1.5))
})

test_that("item (g) of the B textural reads the same transition", {
  p <- .ab(c("A1", "A3", "Bt"), c(0, 15, 30), c(15, 30, 90), c(40, 40, 60),
           boundary = c("clear", "abrupt", NA))
  expect_true(B_textural(p)$evidence$item_g)
  expect_true(isTRUE(mudanca_textural_abrupta(p)$passed))
  q <- .ab(c("A1", "A3", "Bt"), c(0, 15, 30), c(15, 30, 90), c(40, 40, 60),
           boundary = c("clear", "gradual", NA))
  expect_false(B_textural(q)$evidence$item_g)
  expect_false(isTRUE(mudanca_textural_abrupta(q)$passed))
})
