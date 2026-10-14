# v0.9.253: the histic horizon with the whole of its thickness, and the
# thickness the key asks of it in the Organossolos step.
#
# SiBCS 2018, Cap 2, p. 49-50: the horizonte histico results from plant
# residues laid down at the surface, "ainda que, no presente, possa
# encontrar-se recoberto por horizontes ou depositos minerais", and has to be
# (a) 20 cm thick or more, or (c) 10 cm or more over a lithic contact. Only a
# layer starting within 5 cm of the surface was looked at: a horizon
# described in sub-horizons counted its first alone, a buried one was not
# seen.
#
# Cap 4, p. 111: the Organossolos have a horizonte histico of (a) "20 cm ou
# mais, quando sobrejacente a um contato litico", or (b) "40 cm ou mais,
# continuo ou cumulativo nos primeiros 80 cm a partir da superficie do solo".
# Any horizonte histico made an Organossolo.

.org_profile <- function(designation, top, bottom, oc) {
  hz <- data.table::data.table(designation = designation, top_cm = top,
                               bottom_cm = bottom, oc_pct = oc,
                               clay_pct = 20, silt_pct = 30, sand_pct = 50)
  PedonRecord$new(site = list(id = "org", country = "BR"),
                  horizons = ensure_horizon_schema(hz))
}

test_that("the sub-horizons of a histic horizon make its thickness", {
  skip_on_cran()
  # Oo1 of 15 cm alone is under the 20 cm; with Oo2 and Oo3 the horizon has 80
  p <- .org_profile(c("Oo1", "Oo2", "Oo3", "2Cg"), c(0, 15, 32, 80),
                    c(15, 32, 80, 110), c(39, 45, 44, 0.5))
  r <- horizonte_histico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 1:3)
  expect_equal(r$evidence$thickness_cm, 80)
  expect_equal(nrow(r$evidence$horizons), 1L)
  o <- organossolo(p)
  expect_true(isTRUE(o$passed))
  expect_equal(o$evidence$within_80_cm, 80)
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Organossolos")
})

test_that("a buried histic horizon is one", {
  # 2Hdz from 22 cm, below a mineral Ap
  p <- .org_profile(c("Ap1", "Ap2", "2Hdz", "2Hdzj", "3Cg"), c(0, 12, 22, 50, 76),
                    c(12, 22, 50, 76, 120), c(4.8, 1.1, 26.5, 18, 0.5))
  r <- horizonte_histico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 3:4)
  expect_equal(r$evidence$horizons$top_cm, 22)
  expect_equal(r$evidence$horizons$thickness_cm, 54)
  o <- organossolo(p)
  expect_true(isTRUE(o$passed))
  expect_equal(o$evidence$within_80_cm, 54)
})

test_that("layers that do not adjoin are two runs, each with its own thickness", {
  # 15 cm, a mineral layer, 15 cm: neither run is a histic horizon
  p <- .org_profile(c("H1", "Cg1", "H2", "Cg2"), c(0, 15, 30, 45), c(15, 30, 45, 100),
                    c(30, 1, 30, 1))
  r <- horizonte_histico(p)
  expect_false(isTRUE(r$passed))
  expect_length(r$layers, 0L)
  expect_equal(r$evidence$horizons$thickness_cm, c(15, 15))
  expect_false(isTRUE(organossolo(p)$passed))
  # 25 cm, a mineral layer, 25 cm: two horizons, 50 cm of them within 80 cm
  q <- .org_profile(c("H1", "Cg1", "H2", "Cg2"), c(0, 25, 40, 65), c(25, 40, 65, 100),
                    c(30, 1, 30, 1))
  r <- horizonte_histico(q)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, c(1L, 3L))
  expect_equal(r$evidence$horizons$passed, c(TRUE, TRUE))
  o <- organossolo(q)
  expect_true(isTRUE(o$passed))
  expect_equal(o$evidence$within_80_cm, 50)
})

test_that("a histic horizon of 20 to 39 cm over a mineral horizon is no Organossolo", {
  skip_on_cran()
  p <- .org_profile(c("H", "Cg"), c(0, 35), c(35, 100), c(19, 1.5))
  expect_true(isTRUE(horizonte_histico(p)$passed))
  o <- organossolo(p)
  expect_false(isTRUE(o$passed))
  expect_equal(o$evidence$within_80_cm, 35)
  expect_false(isTRUE(o$evidence$over_lithic_contact))
  expect_false(identical(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Organossolos"))
  # the limit: "40 cm ou mais"
  expect_true(isTRUE(organossolo(.org_profile(c("H", "Cg"), c(0, 40), c(40, 100), c(19, 1.5)))$passed))
  expect_false(isTRUE(organossolo(.org_profile(c("H", "Cg"), c(0, 39), c(39, 100), c(19, 1.5)))$passed))
})

test_that("the 40 cm are those within the first 80 cm", {
  # 20 cm at the surface and 100 cm from 61 cm down: 39 cm within 80 cm
  p <- .org_profile(c("H1", "Cg", "H2", "H3"), c(0, 20, 61, 80), c(20, 61, 80, 180),
                    c(15, 0.8, 16.5, 34))
  r <- horizonte_histico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, c(1L, 3L, 4L))
  o <- organossolo(p)
  expect_equal(o$evidence$within_80_cm, 39)
  expect_false(isTRUE(o$passed))
  # one centimetre higher, 40 cm
  q <- .org_profile(c("H1", "Cg", "H2", "H3"), c(0, 20, 60, 80), c(20, 60, 80, 180),
                    c(15, 0.8, 16.5, 34))
  expect_true(isTRUE(organossolo(q)$passed))
  # a horizon that begins below 80 cm adds nothing
  deep <- .org_profile(c("A", "Cg", "H"), c(0, 20, 90), c(20, 90, 160), c(2, 1, 30))
  expect_true(isTRUE(horizonte_histico(deep)$passed))
  expect_false(isTRUE(organossolo(deep)$passed))
})

test_that("over a lithic contact the key asks 20 cm, the horizon 10 cm", {
  p20 <- .org_profile(c("O", "R"), c(0, 20), c(20, 60), c(25, NA))
  r <- horizonte_histico(p20)
  expect_true(isTRUE(r$passed))
  expect_true(isTRUE(r$evidence$on_rock))
  o <- organossolo(p20)
  expect_true(isTRUE(o$passed))
  expect_true(isTRUE(o$evidence$over_lithic_contact))
  # 12 cm over the rock: a histic horizon (item c), no Organossolo
  p12 <- .org_profile(c("O", "R"), c(0, 12), c(12, 60), c(25, NA))
  expect_true(isTRUE(horizonte_histico(p12)$passed))
  expect_false(isTRUE(organossolo(p12)$passed))
  # 12 cm over a mineral horizon: no histic horizon
  expect_false(isTRUE(horizonte_histico(
    .org_profile(c("O", "A", "R"), c(0, 12, 30), c(12, 30, 60), c(25, 2, NA)))$passed))
  # the contact is asked right below the horizon, in the order of the depths
  shuffled <- .org_profile(c("R", "O2", "O1"), c(20, 8, 0), c(60, 20, 8), c(NA, 25, 30))
  r <- horizonte_histico(shuffled)
  expect_true(isTRUE(r$evidence$on_rock))
  expect_equal(r$evidence$thickness_cm, 20)
  expect_true(isTRUE(organossolo(shuffled)$passed))
})

test_that("what makes a layer organic is as before", {
  # an O or H with 8% of organic carbon or more; any layer with 12% or more
  expect_true(isTRUE(horizonte_histico(.org_profile(c("H", "Cg"), c(0, 25), c(25, 90), c(9, 1)))$passed))
  expect_false(isTRUE(horizonte_histico(.org_profile(c("A", "Cg"), c(0, 25), c(25, 90), c(9, 1)))$passed))
  expect_true(isTRUE(horizonte_histico(.org_profile(c("A", "Cg"), c(0, 25), c(25, 90), c(13, 1)))$passed))
  expect_false(isTRUE(horizonte_histico(.org_profile(c("H", "Cg"), c(0, 25), c(25, 90), c(6, 1)))$passed))
  # no organic carbon recorded at all: not known
  r <- horizonte_histico(.org_profile(c("H", "Cg"), c(0, 25), c(25, 90), c(NA_real_, NA_real_)))
  expect_true(is.na(r$passed))
  expect_equal(r$missing, "oc_pct")
  expect_true(is.na(organossolo(.org_profile(c("H", "Cg"), c(0, 25), c(25, 90), c(NA_real_, NA_real_)))$passed))
})

test_that("a histic horizon under 40 cm is that of the classes further down the key", {
  skip_on_cran()
  # H of 30 cm over a glei horizon: Gleissolo Melanico, by its histic horizon
  hz <- data.table::data.table(
    designation = c("H", "Cg1", "Cg2"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 130),
    oc_pct = c(20, 0.8, 0.4), clay_pct = c(30, 35, 35), silt_pct = 30, sand_pct = c(40, 35, 35),
    munsell_hue_moist = c("10YR", "2.5Y", "2.5Y"), munsell_value_moist = c(2, 5, 5),
    munsell_chroma_moist = c(1, 1, 1))
  p <- PedonRecord$new(site = list(id = "gm", country = "BR", drainage = "Mal drenado"),
                       horizons = ensure_horizon_schema(hz))
  expect_false(isTRUE(organossolo(p)$passed))
  expect_true(isTRUE(horizonte_histico(p)$passed))
  expect_true(isTRUE(gleissolo_melanico(p)$passed))
  res <- classify_sibcs(p, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Gleissolos")
  expect_match(res$name, "Mel")
})

test_that("the canonical Organossolo keeps its order, with its three H sub-horizons", {
  skip_on_cran()
  p <- make_organossolo_canonical()
  r <- horizonte_histico(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, 1:3)
  expect_equal(r$evidence$thickness_cm, 80)
  expect_true(isTRUE(organossolo(p)$passed))
  expect_equal(classify_sibcs(p, on_missing = "silent")$rsg_or_order, "Organossolos")
})
