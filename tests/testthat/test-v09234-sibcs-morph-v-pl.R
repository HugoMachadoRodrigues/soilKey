# v0.9.234: in the morphological mode, the SiBCS diagnostics read the "v" and
# the "pl" of the Brazilian horizon notation.
#
# The morphological mode (options(soilKey.morphological_inference = TRUE))
# lets a horizon designation stand for a diagnostic the data cannot test. Two
# letters of the Brazilian notation were not read by it:
#   - "v", vertic features (Bv, Cv, Biv). vertic_horizon() reads it only under
#     an option of its own, soilKey.vertic_designation_inference, because the
#     function also serves WRB and USDA, where "v" is plinthite (KST, FAO). Of
#     the 72 profiles of the national BDsolos export whose reference order is
#     Vertissolos, 14 carry the suffix and none was reached in either mode;
#   - "pl", plinthite in the surveys before the present notation (B21pl, Cpl),
#     where "f" is written today. 80 of the 331 reference Plintossolos carry
#     it, against 64 with "f", which the mode already read.
# Both are read in the SiBCS wrappers only; the default mode is unchanged.

.morph <- function(on) {
  old <- options(soilKey.morphological_inference = on,
                 soilKey.vertic_designation_inference = NULL)
  old
}

.vertic_by_letter <- function() {
  # the canonical Vertissolo as an old survey records it: no slickensides, no
  # cracks, no COLE, and the vertic features in the designation
  p <- make_vertissolo_canonical()
  h <- p$horizons
  h$designation <- c("A", "Bv", "Cv")
  h$slickensides <- NA_character_
  # v0.9.245: nor the wedge-shaped structure, which is a vertic feature too
  h$structure_type <- c("subangular blocky", "prismatic", "prismatic")
  h$cracks_width_cm <- NA_real_
  if ("cracks_depth_cm" %in% names(h)) h$cracks_depth_cm <- NA_real_
  if ("cole_value" %in% names(h)) h$cole_value <- NA_real_
  p$horizons <- h
  p
}

.plinthic_by_letters <- function(designation) {
  # the canonical Plintossolo without the volume of plinthite, which no
  # survey of the BDsolos export records
  p <- make_plintossolo_canonical()
  p$horizons$plinthite_pct <- NA_real_
  p$horizons$designation <- designation
  p
}

test_that("the 'v' of a Brazilian designation is a vertic horizon in the morphological mode", {
  skip_on_cran()
  p <- .vertic_by_letter()
  old <- .morph(FALSE); on.exit(options(old), add = TRUE)
  expect_false(isTRUE(horizonte_vertico(p)$passed))
  expect_false(identical(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Vertissolos"))
  .morph(TRUE)
  expect_true(isTRUE(horizonte_vertico(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Vertissolos")
  # the option of vertic_horizon() is as it was after the call
  expect_null(getOption("soilKey.vertic_designation_inference"))
  # with the number of a material in front, too
  q <- p$clone(deep = TRUE); q$horizons$designation <- c("A", "IIBv", "IICv")
  expect_true(isTRUE(horizonte_vertico(q)$passed))
})

test_that("for WRB and USDA the 'v' is still not vertic", {
  p <- .vertic_by_letter()
  old <- .morph(TRUE); on.exit(options(old), add = TRUE)
  # vertic_horizon() itself, which serves the three systems, does not read it
  expect_false(isTRUE(vertic_horizon(p)$passed))
})

test_that("the old 'pl' is plinthite in the morphological mode, as 'f' is", {
  skip_on_cran()
  old <- .morph(FALSE); on.exit(options(old), add = TRUE)
  pl <- .plinthic_by_letters(c("A1", "B21pl", "B22pl"))
  f  <- .plinthic_by_letters(c("A", "Btf", "Cf"))
  expect_false(isTRUE(horizonte_plintico(pl)$passed))
  expect_false(isTRUE(horizonte_plintico(f)$passed))
  .morph(TRUE)
  r <- horizonte_plintico(pl)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 2:3)
  expect_equal(r$evidence$designation_inference$source, "designation_pl")
  expect_true(isTRUE(horizonte_plintico(f)$passed))
  expect_equal(classify_sibcs(pl, on_missing = "silent")$rsg_or_order, "Plintossolos")
  expect_equal(classify_sibcs(f, on_missing = "silent")$rsg_or_order, "Plintossolos")
  # with the number of a material in front
  expect_true(isTRUE(horizonte_plintico(.plinthic_by_letters(c("A1", "IIB2pl", "IIC1pl")))$passed))
})

test_that("a 'pl' that is not 15 cm thick, or not there, is no plinthic horizon", {
  old <- .morph(TRUE); on.exit(options(old), add = TRUE)
  thin <- .plinthic_by_letters(c("A1", "B2pl", "C"))
  thin$horizons$top_cm <- c(0, 20, 30); thin$horizons$bottom_cm <- c(20, 30, 150)
  expect_false(isTRUE(horizonte_plintico(thin)$passed))
  expect_false(isTRUE(horizonte_plintico(.plinthic_by_letters(c("Ap", "Bt", "C")))$passed))
  # "Ap" is not "pl", nor is the "p1" of a series that goes on to "p2"
  # (v0.9.248 reads the "p1" that stands alone: test-v09248)
  expect_false(isTRUE(horizonte_plintico(.plinthic_by_letters(c("Ap", "Bp1", "Bp2")))$passed))
})

test_that("measured plinthite needs no letter and no mode", {
  skip_on_cran()
  old <- .morph(FALSE); on.exit(options(old), add = TRUE)
  p <- make_plintossolo_canonical()
  expect_true(isTRUE(horizonte_plintico(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Plintossolos")
})

# ---- where the plinthic horizon starts --------------------------------------
#
# Reading "pl" brought out a second thing. plintossolo() accepted a plinthic
# horizon that started anywhere within 200 cm. The definition (SiBCS 2018,
# Cap 3, p. 104; key, Cap 4, p. 113) accepts it within 40 cm, or within 200 cm
# only "quando precedidos de horizonte glei ou situados imediatamente abaixo
# do horizonte A ou E ou de outro horizonte que apresente cores palidas,
# variegadas ou com mosqueados em quantidade abundante". In the morphological
# mode, plinthite deep in a B textural ("B21 B22pl") made a Plintossolo: of the
# 302 profiles of the national BDsolos export keyed Plintossolos with "pl"
# read, 127 had that reference order.

.deep_plinthite <- function(above = list()) {
  # the canonical Plintossolo with its plinthite only in the last horizon,
  # which starts at 80 cm; `above` sets fields of the horizon over it
  p <- make_plintossolo_canonical()
  p$horizons$plinthite_pct <- c(0, 0, 25)
  for (k in names(above)) p$horizons[[k]][2] <- above[[k]]
  p
}

test_that("the canonical Plintossolo has its plinthic horizon within 40 cm", {
  skip_on_cran()
  p <- make_plintossolo_canonical()
  r <- plintossolo(p)
  expect_true(isTRUE(r$passed))
  expect_true(r$evidence$in_40)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Plintossolos")
})

test_that("plinthite that starts below 40 cm under a B horizon makes no Plintossolo", {
  skip_on_cran()
  p <- .deep_plinthite()
  expect_true(isTRUE(horizonte_plintico(p)$passed))
  r <- plintossolo(p)
  expect_false(isTRUE(r$passed))
  expect_false(r$evidence$in_40)
  expect_false(r$evidence$in_200)
  expect_false(identical(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Plintossolos"))
})

test_that("it does when the horizon above is an A or an E", {
  skip_on_cran()
  for (d in c("E", "A2", "AB", "2E")) {
    p <- .deep_plinthite(list(designation = d))
    expect_true(isTRUE(plintossolo(p)$passed), info = d)
  }
  for (d in c("Bt", "Bw", "BA", "C")) {
    p <- .deep_plinthite(list(designation = d))
    expect_false(isTRUE(plintossolo(p)$passed), info = d)
  }
})

test_that("it does when the horizon above is pale, or has mottles in abundant amount", {
  skip_on_cran()
  pale <- list(c("10YR", 6, 3), c("7.5YR", 5, 4), c("2.5Y", 6, 2), c("5Y", 5, 6))
  for (x in pale) {
    p <- .deep_plinthite(list(munsell_hue_moist = x[1], munsell_value_moist = as.numeric(x[2]),
                              munsell_chroma_moist = as.numeric(x[3])))
    expect_true(isTRUE(plintossolo(p)$passed), info = paste(x, collapse = " "))
  }
  not_pale <- list(c("10YR", 4, 4),   # brown: the hue and the chroma of the list, not the lightness
                   c("10YR", 6, 6),   # chroma above 4
                   c("5YR", 6, 3),    # a hue that is not in the list
                   c("2.5YR", 5, 2))
  for (x in not_pale) {
    p <- .deep_plinthite(list(munsell_hue_moist = x[1], munsell_value_moist = as.numeric(x[2]),
                              munsell_chroma_moist = as.numeric(x[3])))
    expect_false(isTRUE(plintossolo(p)$passed), info = paste(x, collapse = " "))
  }
  # "mosqueados em quantidade abundante (> 20% em volume)"
  expect_true(isTRUE(plintossolo(.deep_plinthite(list(redoximorphic_features_pct = 30)))$passed))
  expect_false(isTRUE(plintossolo(.deep_plinthite(list(redoximorphic_features_pct = 10)))$passed))
})

test_that("below 200 cm no plinthic horizon makes a Plintossolo", {
  p <- .deep_plinthite(list(designation = "E"))
  p$horizons$top_cm <- c(0, 20, 210); p$horizons$bottom_cm <- c(20, 210, 300)
  expect_false(isTRUE(plintossolo(p)$passed))
})

test_that("a Cambissolo is excluded by the requirements of Plintossolos, not by any plinthite", {
  skip_on_cran()
  cam <- make_cambissolo_canonical()        # A, Bw, BC, C; the C starts at 100 cm
  expect_true(isTRUE(cambissolo(cam)$passed))
  n <- nrow(cam$horizons)
  deep <- cam$clone(deep = TRUE)
  deep$horizons$plinthite_pct <- c(rep(0, n - 1L), 25)
  expect_true(isTRUE(horizonte_plintico(deep)$passed))
  expect_true(isTRUE(B_incipiente(deep)$passed))
  # the BC above it is pale (7.5YR 5/4): the requirements are met, and the
  # profile is no Cambissolo
  expect_true(isTRUE(plintossolo(deep)$passed))
  r <- cambissolo(deep)
  expect_false(isTRUE(r$passed))
  expect_true(r$evidence$plintossolo_excluded)
  # the same plinthite under a BC that is not pale: no Plintossolo, and the
  # Cambissolo stays one. Before, any plinthic horizon excluded it.
  brown <- deep$clone(deep = TRUE)
  brown$horizons$munsell_value_moist[n - 1L] <- 4
  expect_true(isTRUE(horizonte_plintico(brown)$passed))
  expect_false(isTRUE(plintossolo(brown)$passed))
  r <- cambissolo(brown)
  expect_true(isTRUE(r$passed))
  expect_false(r$evidence$plintossolo_excluded)
  expect_equal(classify_sibcs(brown, on_missing = "silent")$rsg_or_order, "Cambissolos")
})

# ---- the canonical Plintossolo carries neither "v" nor "f" -------------------

test_that("the SiBCS fixture is read the same by the three keys, in both modes", {
  skip_on_cran()
  old <- .morph(FALSE); on.exit(options(old), add = TRUE)
  p <- make_plintossolo_canonical(); w <- make_plinthosol_canonical()
  expect_equal(p$horizons$designation, c("A", "Bt", "C"))
  expect_equal(w$horizons$designation, c("A", "Btv", "Cv"))
  expect_equal(classify_wrb2022(p, on_missing = "silent")$name,
               classify_wrb2022(w, on_missing = "silent")$name)
  expect_equal(classify_usda(p, on_missing = "silent")$name,
               classify_usda(w, on_missing = "silent")$name)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Plintossolos")
  .morph(TRUE)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Plintossolos")
})
