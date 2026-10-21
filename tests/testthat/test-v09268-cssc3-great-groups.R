# v0.9.268: the 31 great groups of the Canadian System of Soil
# Classification, 3rd ed. (Key to Soil Great Groups, Ch 3, pp. 35-36).

.gg <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  p <- PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  classify_cssc3(p, on_missing = "silent")
}

test_that("the rules list 31 great groups under the ten orders", {
  r <- load_rules("cssc3")
  expect_identical(sort(names(r$great_groups)), LETTERS[1:10])
  expect_identical(sum(lengths(r$great_groups)), 31L)
})

test_that("each example pedon keys to its great group", {
  skip_on_cran()
  want <- c(cryosolic = "Turbic Cryosol", organic = "Mesisol",
            vertisolic = "Vertisol", podzolic = "Humo-Ferric Podzol",
            gleysolic = "Humic Gleysol", solonetzic = "Solonetz",
            chernozemic = "Dark Brown Chernozem", luvisolic = "Gray Luvisol",
            brunisolic = "Eutric Brunisol", regosolic = "Regosol")
  for (o in names(want)) {
    r <- classify_cssc3(make_cssc3_example(o), on_missing = "silent")
    expect_identical(r$name, want[[o]], info = o)
    expect_identical(r$levels$level, c("orders", "great_groups"), info = o)
    expect_length(r$provisional_because, 0L)
  }
})

test_that("Cryosolic great groups", {
  skip_on_cran()
  expect_identical(.gg(list(designation = c("Of", "Om", "Cz"), top_cm = c(0, 30, 55),
                            bottom_cm = c(30, 55, 90), oc_pct = c(45, 40, 1)))$name,
                   "Organic Cryosol")
  expect_identical(.gg(list(designation = c("Ah", "Bm", "Cz"), top_cm = c(0, 10, 40),
                            bottom_cm = c(10, 40, 90), oc_pct = c(4, 1, 0.5)))$name,
                   "Static Cryosol")
})

test_that("Organic great groups", {
  skip_on_cran()
  expect_identical(.gg(list(designation = c("LFH", "R"), top_cm = c(0, 45),
                            bottom_cm = c(45, 60), oc_pct = c(40, NA)))$name, "Folisol")
  expect_identical(.gg(list(designation = c("Of", "Of", "C"), top_cm = c(0, 40, 130),
                            bottom_cm = c(40, 130, 150), oc_pct = c(46, 44, 1),
                            fiber_content_rubbed_pct = c(70, 60, NA)))$name, "Fibrisol")
  expect_identical(.gg(list(designation = c("Om", "Oh", "C"), top_cm = c(0, 30, 130),
                            bottom_cm = c(30, 130, 150), oc_pct = c(40, 35, 1)))$name, "Humisol")
})

test_that("Vertisolic great groups follow the surface colour and drainage", {
  skip_on_cran()
  p <- make_cssc3_example("vertisolic")
  p$horizons$munsell_value_dry[1] <- 3
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Humic Vertisol")
  # poorly drained: the Ah thickness decides
  p$site$drainage_class <- "poorly drained"
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Humic Vertisol")
  p$horizons$bottom_cm[1] <- 8; p$horizons$top_cm[2] <- 8
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Vertisol")
})

test_that("Podzolic great groups follow the Bh and Bhf thickness", {
  skip_on_cran()
  base <- list(designation = c("LFH", "Ae", "Bh", "Bf", "C"), top_cm = c(0, 5, 12, 25, 60),
               bottom_cm = c(5, 12, 25, 60, 100), oc_pct = c(40, 0.6, 3, 1.5, 0.2),
               clay_pct = c(NA, 5, 8, 6, 5), sand_pct = c(NA, 78, 70, 76, 82),
               fe_pyro_pct = c(NA, 0.05, 0.1, 0.5, 0.05), al_pyro_pct = c(NA, 0.04, 0.3, 0.4, 0.05),
               munsell_hue_moist = c(NA, "10YR", "7.5YR", "5YR", "10YR"),
               munsell_value_moist = c(NA, 6, 2, 4, 5), munsell_chroma_moist = c(NA, 2, 2, 6, 3))
  expect_identical(.gg(base)$name, "Humic Podzol")
  bhf <- base; bhf$designation[3] <- "Bhf"; bhf$oc_pct[3] <- 7; bhf$fe_pyro_pct[3] <- 0.6
  bhf$munsell_value_moist[3] <- 3; bhf$munsell_chroma_moist[3] <- 4
  expect_identical(.gg(bhf)$name, "Ferro-Humic Podzol")
})

test_that("Gleysolic great groups", {
  skip_on_cran()
  expect_identical(.gg(list(designation = c("Aeg", "Btg", "Ckg"), top_cm = c(0, 15, 50),
                            bottom_cm = c(15, 50, 100), clay_pct = c(12, 28, 26),
                            clay_films_amount = c("none", "common", NA),
                            munsell_hue_moist = c("10YR", "2.5Y", "2.5Y"),
                            munsell_value_moist = c(5, 5, 5),
                            munsell_chroma_moist = c(1, 1, 2)))$name, "Luvic Gleysol")
  expect_identical(.gg(list(designation = c("Ah", "Bg", "Cg"), top_cm = c(0, 5, 40),
                            bottom_cm = c(5, 40, 100), oc_pct = c(4, 0.6, 0.3),
                            munsell_hue_moist = c("10YR", "5Y", "5Y"),
                            munsell_value_moist = c(3, 5, 5),
                            munsell_chroma_moist = c(1, 1, 1)))$name, "Gleysol")
})

test_that("Solonetzic great groups", {
  skip_on_cran()
  sol <- list(designation = c("Ah", "Ae", "Bnt", "Csk"), top_cm = c(0, 10, 14, 40),
              bottom_cm = c(10, 14, 40, 90))
  expect_identical(.gg(sol)$name, "Solodized Solonetz")
  sod <- list(designation = c("Ah", "Ae", "AB", "Bnt", "Csk"), top_cm = c(0, 10, 14, 22, 40),
              bottom_cm = c(10, 14, 22, 40, 90))
  expect_identical(.gg(sod)$name, "Solod")
  vs <- list(designation = c("Ah", "Bnt", "Bnss", "Csk"), top_cm = c(0, 12, 35, 70),
             bottom_cm = c(12, 35, 70, 100))
  expect_identical(.gg(vs)$name, "Vertic Solonetz")
})

test_that("Chernozemic great groups follow the dry colour of the A", {
  skip_on_cran()
  p <- make_cssc3_example("chernozemic")
  p$horizons$munsell_value_dry[1] <- 5
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Brown Chernozem")
  p$horizons$munsell_value_dry[1] <- 3
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Black Chernozem")
  # chroma 1.5 or less with an Ahe: Dark Gray
  p$horizons$munsell_value_dry[1] <- 4
  p$horizons$munsell_chroma_dry[1] <- 1
  p$horizons$designation[1] <- "Ahe"
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Dark Gray Chernozem")
})

test_that("Luvisolic, Brunisolic and Regosolic great groups", {
  skip_on_cran()
  gbl <- list(designation = c("Ah", "Ae", "Bt", "Ck"), top_cm = c(0, 8, 20, 60),
              bottom_cm = c(8, 20, 60, 100), clay_pct = c(15, 12, 28, 26),
              clay_films_amount = c(NA, "none", "common", NA))
  expect_identical(.gg(gbl, list(mean_annual_soil_temp_C = 9))$name, "Gray Brown Luvisol")
  expect_identical(.gg(gbl, list(mean_annual_soil_temp_C = 4))$name, "Gray Luvisol")
  br <- list(designation = c("Ah", "Bm", "C"), top_cm = c(0, 12, 40), bottom_cm = c(12, 40, 90),
             ph_cacl2 = c(6, 6.2, 6.8))
  expect_identical(.gg(br)$name, "Melanic Brunisol")
  br$ph_cacl2 <- c(4.8, 5.0, 5.2)
  expect_identical(.gg(br)$name, "Sombric Brunisol")
  br$bottom_cm[1] <- 6; br$top_cm[2] <- 6
  expect_identical(.gg(br)$name, "Dystric Brunisol")
  expect_identical(.gg(list(designation = c("Ah", "C"), top_cm = c(0, 15),
                            bottom_cm = c(15, 100)))$name, "Humic Regosol")
})

test_that("only terric, lithic and hydric layers widen the organic tier", {
  skip_on_cran()
  # a 5 cm mineral layer with peat below is a cumulic layer (Ch 2, p. 19):
  # the middle tier is 20 cm fibric and 55 cm mesic
  expect_identical(.gg(list(designation = c("Of", "C", "Om"), top_cm = c(0, 60, 65),
                            bottom_cm = c(60, 65, 160), oc_pct = c(45, 2, 40),
                            fiber_content_rubbed_pct = c(60, NA, 25)))$name, "Mesisol")
  # a water lens with peat under it is not a hydric layer
  expect_identical(.gg(list(designation = c("Of", "W", "Om"), top_cm = c(0, 60, 90),
                            bottom_cm = c(60, 90, 160), oc_pct = c(45, NA, 40),
                            fiber_content_rubbed_pct = c(60, NA, 25)))$name, "Mesisol")
  # a mineral substratum with no peat below is terric: surface and middle
  # tiers together, 60 cm fibric over 25 cm mesic
  expect_identical(.gg(list(designation = c("Of", "Om", "C"), top_cm = c(0, 60, 85),
                            bottom_cm = c(60, 85, 160), oc_pct = c(45, 40, 1),
                            fiber_content_rubbed_pct = c(60, 25, NA)))$name, "Fibrisol")
})

test_that("measured rubbed fibre outranks the O designation", {
  skip_on_cran()
  r <- .gg(list(designation = c("Of", "Of", "Oh"), top_cm = c(0, 40, 120),
                bottom_cm = c(40, 120, 160), oc_pct = c(46, 44, 40),
                fiber_content_rubbed_pct = c(60, 5, 5)))
  expect_identical(r$name, "Humisol")
})

test_that("Folisols need the folic thickness of Ch 9", {
  skip_on_cran()
  # 8 cm of folic material over an Ah and peat fails all three criteria
  expect_identical(.gg(list(designation = c("F", "Ah", "Om", "C"), top_cm = c(0, 8, 15, 100),
                            bottom_cm = c(8, 15, 100, 160), oc_pct = c(45, 8, 42, 1),
                            fiber_content_rubbed_pct = c(NA, NA, 25, NA)))$name, "Mesisol")
  # thin litter over peat is an Organic soil, not a Regosolic one
  r <- .gg(list(designation = c("F", "Om"), top_cm = c(0, 5), bottom_cm = c(5, 160),
                oc_pct = c(45, 42)))
  expect_identical(r$rsg_or_order, "Organic")
  expect_identical(r$name, "Mesisol")
  # 15 cm of folic material on a 5 cm mineral layer over bedrock
  expect_identical(.gg(list(designation = c("F", "H", "Bm", "R"), top_cm = c(0, 8, 15, 20),
                            bottom_cm = c(8, 15, 20, 40), oc_pct = c(45, 40, 2, NA)))$name,
                   "Folisol")
})

test_that("an unknown organic status leaves the Organic Cryosol undecided", {
  skip_on_cran()
  r <- .gg(list(designation = c("Oi", "Oe", "Cf"), top_cm = c(0, 30, 70),
                bottom_cm = c(30, 70, 100), oc_pct = c(45, NA, 1),
                permafrost_temp_C = c(NA, NA, -1)), list(country = "US"))
  expect_true(is.na(r$trace$great_groups$AA$passed))
  expect_true("great_groups:AA" %in% r$provisional_because)
})

test_that("the surface A is read by properties where it has no designation", {
  skip_on_cran()
  gley <- list(top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100), oc_pct = c(6, 0.8, 0.4),
               munsell_hue_moist = c("10YR", "2.5Y", "5Y"), munsell_value_moist = c(2, 5, 5),
               munsell_chroma_moist = c(1, 1, 1))
  # a missing designation on the surface horizon of a Canadian profile
  expect_identical(.gg(c(list(designation = c(NA, "Bg", "Cg")), gley))$name, "Humic Gleysol")
  # an A of unknown colour and organic C is undecided, not absent
  unk <- c(list(designation = c("A", "Bg", "Cg")), gley)
  unk$munsell_hue_moist[1] <- NA; unk$munsell_value_moist[1] <- NA
  unk$munsell_chroma_moist[1] <- NA; unk$oc_pct[3] <- NA
  r <- .gg(unk, list(country = "US"))
  expect_true(is.na(r$trace$great_groups$EB$passed))
  expect_true("great_groups:EB" %in% r$provisional_because)
  # without designations, an A split in two counts in full (7 + 7 cm)
  split <- list(designation = NA_character_, top_cm = c(0, 7, 14, 50),
                bottom_cm = c(7, 14, 50, 100), oc_pct = c(6, 4, 0.8, 0.4),
                munsell_hue_moist = c("10YR", "10YR", "2.5Y", "5Y"),
                munsell_value_moist = c(2, 2, 5, 5), munsell_chroma_moist = c(1, 1, 1, 1))
  expect_identical(.gg(split, list(country = "US"))$name, "Humic Gleysol")
  # a colour break ends the A: a lighter B rich in organic C is not part of it
  pz <- list(designation = NA_character_, top_cm = c(0, 6, 30, 60),
             bottom_cm = c(6, 30, 60, 100), oc_pct = c(5, 2, 0.6, 0.2),
             munsell_value_moist = c(2, 4, 4, 5))
  a <- .cssc_surface_a(.cssc_horizons(PedonRecord$new(
    site = list(id = "t", country = "US"),
    horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(pz))))))
  expect_identical(a$ah_cm, 6)
})

test_that("Vertisolic drainage classes and cultivated surfaces", {
  skip_on_cran()
  p <- make_cssc3_example("vertisolic")
  p$site$drainage_class <- "rapidly drained"
  r <- classify_cssc3(p, on_missing = "silent")
  expect_identical(r$name, "Vertisol")
  expect_length(r$provisional_because, 0L)
  # a range across the two groups is undecided when the branches disagree
  # (a dark surface, but an Ah thinner than 10 cm)
  p$site$drainage_class <- "imperfectly to poorly drained"
  p$horizons$munsell_value_dry[1] <- 3
  p$horizons$bottom_cm[1] <- 8; p$horizons$top_cm[2] <- 8
  expect_true(is.na(classify_cssc3(p, on_missing = "silent")$trace$great_groups$CA$passed))
  # poorly drained and cultivated: the Ch 13 rule for the Ap
  p <- make_cssc3_example("vertisolic"); p$site$drainage_class <- "poorly drained"
  p$horizons$designation[1] <- "Ap"; p$horizons$bottom_cm[1] <- 20; p$horizons$top_cm[2] <- 20
  p$horizons$munsell_value_moist[1] <- 2; p$horizons$oc_pct[1] <- 4
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Humic Vertisol")
  p$horizons$oc_pct[1] <- 1.5
  expect_identical(classify_cssc3(p, on_missing = "silent")$name, "Vertisol")
})

test_that("Folisol folic material starts at the surface and lies on the rock", {
  skip_on_cran()
  # folic litter over peat over rock: the folic layer is not on the rock
  expect_identical(.gg(list(designation = c("LFH", "Om", "R"), top_cm = c(0, 12, 70),
                            bottom_cm = c(12, 70, 90), oc_pct = c(40, 42, NA),
                            fiber_content_rubbed_pct = c(NA, 25, NA)))$name, "Mesisol")
  # a buried folic layer under peat does not make a Folisol
  r <- .gg(list(designation = c("Of", "Om", "Fb", "C"), top_cm = c(0, 30, 60, 105),
                bottom_cm = c(30, 60, 105, 160), oc_pct = c(46, 42, 40, 1),
                fiber_content_rubbed_pct = c(70, 25, NA, NA)))
  expect_false(r$trace$great_groups$BA$passed)
})

test_that("organic layers and contacts of unknown status stay undecided", {
  skip_on_cran()
  # a thin mineral layer over a layer that may be peat or mineral
  r <- .gg(list(designation = c("Oh", "Of", "C", NA), top_cm = c(0, 40, 60, 70),
                bottom_cm = c(40, 60, 70, 160), oc_pct = c(40, 45, 2, NA),
                fiber_content_rubbed_pct = c(5, 60, NA, NA)))
  expect_true(is.na(r$trace$great_groups$BD$passed))
  # water that is the deepest layer recorded but ends above 160 cm
  r <- .gg(list(designation = c("Oh", "Of", "W"), top_cm = c(0, 45, 80),
                bottom_cm = c(45, 80, 130), oc_pct = c(40, 45, NA),
                fiber_content_rubbed_pct = c(5, 60, NA)))
  expect_true(is.na(r$trace$great_groups$BB$passed))
  # the order: a layer of unknown status inside the organic column
  r <- .gg(list(designation = c("Of", NA, "Om"), top_cm = c(0, 50, 90),
                bottom_cm = c(50, 90, 160), oc_pct = c(45, NA, 40),
                fiber_content_rubbed_pct = c(60, NA, 25)))
  expect_true("orders:B" %in% r$provisional_because)
})

test_that("the Organic order reads measured fibre first and peat under litter", {
  skip_on_cran()
  # designated Om but 60 % rubbed fibre: fibric, so 60 cm are needed
  expect_false(identical(.gg(list(designation = c("Om", "C"), top_cm = c(0, 50),
                                  bottom_cm = c(50, 100), oc_pct = c(45, 1),
                                  fiber_content_rubbed_pct = c(60, NA)))$rsg_or_order,
                         "Organic"))
  # designated Of but 25 % rubbed fibre: mesic, 40 cm are enough
  expect_identical(.gg(list(designation = c("Of", "C"), top_cm = c(0, 50),
                            bottom_cm = c(50, 100), oc_pct = c(45, 1),
                            fiber_content_rubbed_pct = c(25, NA)))$name, "Mesisol")
  # litter does not count towards the peat limit: 35 cm of Om under it
  expect_identical(.gg(list(designation = c("LFH", "Om", "Cg"), top_cm = c(0, 15, 50),
                            bottom_cm = c(15, 50, 100), oc_pct = c(40, 42, 1),
                            munsell_hue_moist = c(NA, NA, "5Y"), munsell_value_moist = c(NA, NA, 5),
                            munsell_chroma_moist = c(NA, NA, 1)))$rsg_or_order, "Gleysolic")
})

test_that("the surface A in another notation and with undecided horizons", {
  skip_on_cran()
  # US notation: a dark Bg1 is a B, not part of the A
  us <- list(designation = c("A", "Bg1", "Bg2", "Cg"), top_cm = c(0, 8, 30, 60),
             bottom_cm = c(8, 30, 60, 100), oc_pct = c(5, 1.2, 0.5, 0.3),
             munsell_hue_moist = c("10YR", "2.5Y", "5Y", "5Y"),
             munsell_value_moist = c(2, 2.5, 5, 5), munsell_chroma_moist = c(1, 1, 1, 1))
  expect_identical(.gg(us, list(country = "US"))$name, "Gleysol")
  # every A the profile allows is tested: 0-6 (too thin), 0-12 (passes),
  # 0-20 (organic C below 2 %), so the Humic Gleysol is undecided
  wm <- list(designation = NA_character_, top_cm = c(0, 6, 12, 20, 50),
             bottom_cm = c(6, 12, 20, 50, 100), oc_pct = c(1.5, 4, 0.5, 0.4, 0.4),
             munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y", "5Y"),
             munsell_value_moist = c(2, NA, 2.5, 5, 5), munsell_chroma_moist = c(1, 1, 1, 1, 1))
  expect_true(is.na(.gg(wm, list(country = "US"))$trace$great_groups$EB$passed))
  # the deepest horizon, undesignated, cannot be ruled out of the A
  r <- .gg(list(designation = c("Ah", NA), top_cm = c(0, 8), bottom_cm = c(8, 20),
                oc_pct = c(4, 3.5), munsell_value_moist = c(2, 2)))
  expect_true(is.na(r$trace$great_groups$JA$passed))
  # a missing bottom depth is undecided, not an error
  r <- .gg(list(designation = c("Of", "Om", "C", "C"), top_cm = c(0, 40, 80, 100),
                bottom_cm = c(40, 80, NA, 160), oc_pct = c(45, 40, 1, 1),
                fiber_content_rubbed_pct = c(60, 25, NA, NA)))
  expect_false(any(grepl("Error", c(r$warnings, unlist(lapply(r$trace$great_groups,
                                                              function(t) t$notes))))))
})

test_that("the Ch 13 Vertisol text decides a cultivated Ap", {
  skip_on_cran()
  vert <- function(designation, top, bottom, vm, vd, oc) {
    n <- length(designation)
    .gg(list(designation = designation, top_cm = top, bottom_cm = bottom,
             clay_pct = rep(62, n), munsell_hue_moist = rep("10YR", n),
             munsell_value_moist = vm, munsell_chroma_moist = rep(2, n),
             munsell_value_dry = vd, oc_pct = oc,
             slickensides = c(rep(NA, n - 2L), "many", "common")),
        list(drainage_class = "poorly drained"))
  }
  # an Ap over a remnant Ah: the contrast is with the horizon under the A
  expect_identical(vert(c("Ap", "Ah", "Bv", "Bssk", "Ck"), c(0, 15, 25, 50, 95),
                        c(15, 25, 50, 95, 130), c(2.5, 2.5, 4.5, 4.5, 5),
                        c(4, 4, 6, 6, 6), c(4, 3, 1.5, 0.8, 0.5))$name, "Humic Vertisol")
  # moist value 3.0 but dry 5.5: one property of the Vertisol text is enough
  expect_identical(vert(c("Ap", "Bv", "Bssk", "Ck"), c(0, 15, 50, 95), c(15, 50, 95, 130),
                        c(3, 6, 6, 6), c(5.5, 7, 7, 7), c(3, 1.5, 0.8, 0.5))$name,
                   "Vertisol")
})
