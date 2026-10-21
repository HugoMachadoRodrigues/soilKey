# v0.9.269: the subgroups of the Cryosolic, Organic and Vertisolic orders of
# the Canadian System of Soil Classification, 3rd ed. (Key to Soil Subgroups,
# Ch 3, pp. 37-40).

.sg <- function(h, site = list()) {
  site <- utils::modifyList(list(id = "t", country = "CA"), site)
  p <- PedonRecord$new(site = site, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
  classify_cssc3(p, on_missing = "silent")
}

test_that("the rules list 63 subgroups under the nine great groups of A, B, C", {
  r <- load_rules("cssc3")
  gg <- c("AA", "AB", "AC", "BA", "BB", "BC", "BD", "CA", "CB")
  expect_true(all(gg %in% names(r$subgroups)))
  expect_identical(sum(lengths(r$subgroups[gg])), 63L)
})

test_that("the example pedons of the three orders reach a subgroup", {
  skip_on_cran()
  want <- c(cryosolic = "Brunisolic Dystric Turbic Cryosol",
            organic = "Terric Humic Mesisol", vertisolic = "Orthic Vertisol")
  for (o in names(want)) {
    r <- classify_cssc3(make_cssc3_example(o), on_missing = "silent")
    expect_identical(r$name, want[[o]], info = o)
    expect_identical(r$levels$level, c("orders", "great_groups", "subgroups"), info = o)
    expect_length(r$provisional_because, 0L)
  }
  # since v0.9.271 every order keys its subgroups
  r <- classify_cssc3(make_cssc3_example("solonetzic"), on_missing = "silent")
  expect_identical(r$name, "Dark Brown Solonetz")
})

test_that("Organic Cryosol subgroups", {
  skip_on_cran()
  expect_identical(.sg(list(designation = c("Of", "Om", "Wz", "Cz"), top_cm = c(0, 30, 50, 90),
                            bottom_cm = c(30, 50, 90, 120), oc_pct = c(45, 42, NA, 1),
                            fiber_content_rubbed_pct = c(60, 25, NA, NA),
                            permafrost_temp_C = c(NA, NA, -2, -2)))$name,
                   "Glacic Organic Cryosol")
  expect_identical(.sg(list(designation = c("Of", "Cz"), top_cm = c(0, 50), bottom_cm = c(50, 90),
                            oc_pct = c(45, 1), fiber_content_rubbed_pct = c(60, NA),
                            permafrost_temp_C = c(NA, -1)))$name,
                   "Terric Fibric Organic Cryosol")
  expect_identical(.sg(list(designation = c("Oh", "Cz"), top_cm = c(0, 60), bottom_cm = c(60, 100),
                            oc_pct = c(40, 1), fiber_content_rubbed_pct = c(5, NA),
                            permafrost_temp_C = c(NA, -1)))$name,
                   "Terric Humic Organic Cryosol")
  # no mineral contact within 1 m: the material below 40 cm decides
  expect_identical(.sg(list(designation = c("Of", "Ofz"), top_cm = c(0, 60), bottom_cm = c(60, 130),
                            oc_pct = c(45, 45), fiber_content_rubbed_pct = c(60, 60),
                            permafrost_temp_C = c(NA, -1)))$name,
                   "Fibric Organic Cryosol")
  # a W layer with no sign of being frozen is undecided
  r <- .sg(list(designation = c("Of", "Om", "W", "Cz"), top_cm = c(0, 30, 50, 90),
                bottom_cm = c(30, 50, 90, 120), oc_pct = c(45, 42, NA, 1),
                fiber_content_rubbed_pct = c(60, 25, NA, NA), permafrost_temp_C = c(NA, NA, NA, -2)))
  expect_true(is.na(r$trace$subgroups$AAA$passed))
})

test_that("Turbic and Static Cryosol subgroups", {
  skip_on_cran()
  pf <- function(n) c(rep(NA, n - 1L), -1)
  expect_identical(.sg(list(designation = c("Ahy", "Bgy", "Cgz"), top_cm = c(0, 8, 40),
                            bottom_cm = c(8, 40, 80), oc_pct = c(4, 1, 0.5),
                            munsell_hue_moist = c("10YR", "2.5Y", "5Y"),
                            munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 1, 1),
                            permafrost_temp_C = pf(3)))$name, "Gleysolic Turbic Cryosol")
  histic <- list(designation = c("Omy", "Bmy", "Cz"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                 oc_pct = c(40, 1, 0.5), ph_cacl2 = c(5, 6, 6.5), permafrost_temp_C = pf(3))
  expect_identical(.sg(histic)$name, "Histic Eutric Turbic Cryosol")
  histic$ph_cacl2 <- c(4, 4.8, 5)
  expect_identical(.sg(histic)$name, "Histic Dystric Turbic Cryosol")
  expect_identical(.sg(list(designation = c("Ohy", "Cy", "Cz"), top_cm = c(0, 20, 60),
                            bottom_cm = c(20, 60, 90), oc_pct = c(40, 1, 0.5),
                            permafrost_temp_C = pf(3)))$name, "Histic Regosolic Turbic Cryosol")
  bm <- list(designation = c("Ahy", "Bmy", "Cz"), top_cm = c(0, 8, 35), bottom_cm = c(8, 35, 80),
             oc_pct = c(4, 1, 0.5), ph_cacl2 = c(6, 6.2, 6.5), permafrost_temp_C = pf(3))
  expect_identical(.sg(bm)$name, "Brunisolic Eutric Turbic Cryosol")
  thin <- list(designation = c("Ahy", "Bmy", "Cy", "Cz"), top_cm = c(0, 8, 14, 40),
               bottom_cm = c(8, 14, 40, 80), oc_pct = c(4, 1, 0.6, 0.5),
               ph_cacl2 = c(4.5, 4.8, 5, 5.2), permafrost_temp_C = pf(4))
  expect_identical(.sg(thin)$name, "Orthic Dystric Turbic Cryosol")
  expect_identical(.sg(list(designation = c("Ahy", "Cy", "Cz"), top_cm = c(0, 8, 50),
                            bottom_cm = c(8, 50, 80), oc_pct = c(4, 0.6, 0.5),
                            permafrost_temp_C = pf(3)))$name, "Regosolic Turbic Cryosol")
  expect_identical(.sg(list(designation = c("Ae", "Bt", "Cz"), top_cm = c(0, 10, 40),
                            bottom_cm = c(10, 40, 80), clay_pct = c(12, 28, 26),
                            clay_films_amount = c("none", "common", NA), oc_pct = c(1, 0.6, 0.4),
                            permafrost_temp_C = pf(3)))$name, "Luvisolic Static Cryosol")
  expect_identical(.sg(list(designation = c("Ah", "C", "Cz"), top_cm = c(0, 6, 50),
                            bottom_cm = c(6, 50, 80), oc_pct = c(3, 0.6, 0.5),
                            permafrost_temp_C = pf(3)))$name, "Regosolic Static Cryosol")
  # unknown pH of the B leaves the Eutric and Dystric subgroups undecided
  bm$ph_cacl2 <- NULL
  r <- .sg(bm)
  expect_true(all(c("subgroups:ABE", "subgroups:ABF") %in% r$provisional_because))
})

test_that("Folisol subgroups", {
  skip_on_cran()
  expect_identical(.sg(list(designation = c("L", "F", "H", "Om", "R"), top_cm = c(0, 2, 30, 45, 60),
                            bottom_cm = c(2, 30, 45, 60, 80), oc_pct = c(45, 45, 40, 42, NA)))$name,
                   "Histic Folisol")
  expect_identical(.sg(list(designation = c("F", "H", "R"), top_cm = c(0, 30, 45), bottom_cm = c(30, 45, 60),
                            oc_pct = c(45, 40, NA), woody_fragments_pct = c(50, 40, NA)))$name,
                   "Lignic Folisol")
  expect_identical(.sg(list(designation = c("L", "F", "H", "R"), top_cm = c(0, 2, 35, 42),
                            bottom_cm = c(2, 35, 42, 60), oc_pct = c(45, 45, 40, NA)))$name,
                   "Hemic Folisol")
  expect_identical(.sg(list(designation = c("F", "H", "R"), top_cm = c(0, 8, 45), bottom_cm = c(8, 45, 60),
                            oc_pct = c(45, 40, NA)))$name, "Humic Folisol")
  # neither F nor H dominant: no subgroup, the name stays at the great group
  r <- .sg(list(designation = c("F", "H", "R"), top_cm = c(0, 25, 50), bottom_cm = c(25, 50, 60),
                oc_pct = c(45, 40, NA)))
  expect_identical(r$name, "Folisol")
})

test_that("Fibrisol, Mesisol and Humisol subgroups", {
  skip_on_cran()
  peat <- function(designation, top, bottom, oc, fibre)
    .sg(list(designation = designation, top_cm = top, bottom_cm = bottom, oc_pct = oc,
             fiber_content_rubbed_pct = fibre))$name
  expect_identical(peat(c("Of", "Of", "W"), c(0, 60, 100), c(60, 100, 200), c(45, 45, NA),
                        c(60, 60, NA)), "Hydric Fibrisol")
  expect_identical(peat(c("Of", "Oh", "C"), c(0, 80, 100), c(80, 100, 160), c(45, 40, 1),
                        c(60, 5, NA)), "Terric Humic Fibrisol")
  expect_identical(peat(c("Of", "C"), c(0, 100), c(100, 160), c(45, 1), c(60, NA)),
                   "Terric Fibrisol")
  expect_identical(peat(c("Of", "Oco", "Of"), c(0, 80, 100), c(80, 100, 160), c(45, 20, 45),
                        c(60, NA, 60)), "Limnic Fibrisol")
  expect_identical(peat(c("Of", "C", "Of"), c(0, 70, 80), c(70, 80, 160), c(45, 2, 45),
                        c(60, NA, 60)), "Cumulic Fibrisol")
  expect_identical(peat(c("Of", "Oh"), c(0, 100), c(100, 160), c(45, 40), c(60, 5)),
                   "Humic Fibrisol")
  expect_identical(peat("Of", 0, 160, 45, 60), "Typic Fibrisol")
  expect_identical(peat(c("Om", "Of"), c(0, 120), c(120, 160), c(45, 45), c(25, 60)),
                   "Fibric Mesisol")
  expect_identical(peat("Oh", 0, 160, 40, 5), "Typic Humisol")
  # a mineral layer that is the deepest recorded and thinner than 30 cm may
  # go on: the Terric subgroup is undecided
  r <- .sg(list(designation = c("Of", "C"), top_cm = c(0, 120), bottom_cm = c(120, 135),
                oc_pct = c(45, 1), fiber_content_rubbed_pct = c(60, NA)))
  expect_true(is.na(r$trace$subgroups$BBD$passed))
})

test_that("Vertisolic subgroups follow mottles within 50 cm", {
  skip_on_cran()
  v <- function(designation, contrast = NA, redox = NA)
    .sg(list(designation = designation, top_cm = c(0, 15, 50, 95), bottom_cm = c(15, 50, 95, 130),
             clay_pct = 62, munsell_hue_moist = "10YR", munsell_value_moist = c(3, 3, 4, 4),
             munsell_chroma_moist = 2, munsell_value_dry = c(4, 4, 5, 5),
             slickensides = c(NA, "few", "many", "common"), oc_pct = c(3, 1.5, 0.8, 0.5),
             mottle_contrast = c(NA, contrast, NA, NA),
             redoximorphic_features_pct = c(redox, NA, NA, NA)),
        list(drainage_class = "well drained"))$name
  base <- c("Ah", "Bv", "Bssk", "Ck")
  expect_identical(v(base, "prominent"), "Gleysolic Vertisol")
  expect_identical(v(base, "distinct"), "Gleyed Vertisol")
  expect_identical(v(c("Ah", "Bvg", "Bssk", "Ck")), "Gleysolic Vertisol")
  expect_identical(v(c("Ah", "Bvgj", "Bssk", "Ck")), "Gleyed Vertisol")
  expect_identical(v(base), "Orthic Vertisol")
  # each horizon is read by its own record: the measured faint mottles of the
  # Bvg outrank its g, and the Ah is read from its designation (no g)
  expect_identical(v(c("Ah", "Bvg", "Bssk", "Ck"), "faint", redox = 0), "Gleyed Vertisol")
  expect_identical(v(c("Ah", "Bvg", "Bssk", "Ck"), "faint"), "Gleyed Vertisol")
})

test_that("limnic and folic layers stay out of the great-group peat tally", {
  skip_on_cran()
  # 30 cm of Oco in the middle tier would otherwise be peat of unknown kind
  r <- .sg(list(designation = c("Om", "Oco", "Om"), top_cm = c(0, 60, 90), bottom_cm = c(60, 90, 160),
                oc_pct = c(45, 20, 45), fiber_content_rubbed_pct = c(25, NA, 25)))
  expect_identical(r$levels$name[2], "Mesisol")
  expect_identical(r$name, "Limnic Mesisol")
})

test_that("review fixes: substratum, open bottoms, woody LFH, ice, limnic", {
  skip_on_cran()
  # a mineral layer at the base, with no peat below, is a substratum, not a
  # cumulic layer
  expect_identical(.sg(list(designation = c("Of", "Om", "C"), top_cm = c(0, 20, 145),
                            bottom_cm = c(20, 145, 200), oc_pct = c(45, 42, 1),
                            fiber_content_rubbed_pct = c(60, 25, NA)))$name, "Typic Mesisol")
  expect_identical(.sg(list(designation = c("Om", "C", "R"), top_cm = c(0, 100, 125),
                            bottom_cm = c(100, 125, 140), oc_pct = c(42, 1, NA),
                            fiber_content_rubbed_pct = c(25, NA, NA)))$name, "Typic Mesisol")
  # 40 cm of mineral are certain even when the last bottom depth is missing
  expect_identical(.sg(list(designation = c("Oh", "C", "C"), top_cm = c(0, 100, 140),
                            bottom_cm = c(100, 140, NA), oc_pct = c(40, 1, 1),
                            fiber_content_rubbed_pct = c(5, NA, NA)))$name, "Terric Humisol")
  # woody material in a combined LFH horizon
  expect_identical(.sg(list(designation = c("LFH", "R"), top_cm = c(0, 45), bottom_cm = c(45, 60),
                            oc_pct = c(45, NA), woody_fragments_pct = c(60, NA)))$name,
                   "Lignic Folisol")
  # an ice layer needs more than 95 % ice
  r <- .sg(list(designation = c("Om", "Om", "Cz"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
                oc_pct = c(42, 42, 1), fiber_content_rubbed_pct = c(25, 25, NA),
                ice_pct = c(NA, 95, NA), permafrost_temp_C = c(NA, -1, -1)))
  expect_false(r$trace$subgroups$AAA$passed)
  # bedrock of marl origin is rock, not a limnic layer
  expect_identical(.sg(list(designation = c("Oh", "R"), top_cm = c(0, 130), bottom_cm = c(130, 160),
                            oc_pct = c(38, NA), fiber_content_rubbed_pct = c(6, NA),
                            rock_origin = c(NA, "marlstone")))$name, "Typic Humisol")
  # an L master in the USDA notation is a limnic layer
  expect_identical(.sg(list(designation = c("Oa1", "Lma", "Oa2"), top_cm = c(0, 100, 140),
                            bottom_cm = c(100, 140, 160), oc_pct = c(38, 2, 38),
                            fiber_content_rubbed_pct = c(6, NA, 6)),
                       list(designation_system = "usda"))$levels$name[3], "Limnic Humisol")
  # 45 cm of mesic peat over marl: the middle tier holds no peat, so the
  # surface tier decides
  r <- .sg(list(designation = c("Om", "Ck"), top_cm = c(0, 45), bottom_cm = c(45, 160),
                oc_pct = c(44, 3), fiber_content_rubbed_pct = c(22, NA),
                layer_origin = c(NA, "marl")))
  expect_identical(r$name, "Limnic Mesisol")
})

test_that("review fixes: gleying and mottles horizon by horizon, rows in any order", {
  skip_on_cran()
  # an undesignated horizon within 50 cm, with no mottle record, is undecided
  r <- .sg(list(designation = c("Ah", "Bv", NA, "Bssk", "Ck"), top_cm = c(0, 15, 30, 50, 95),
                bottom_cm = c(15, 30, 50, 95, 130), clay_pct = 62, munsell_hue_moist = "10YR",
                munsell_value_moist = c(3, 3, 3, 4, 4), munsell_chroma_moist = 2,
                munsell_value_dry = c(4, 4, 4, 5, 5),
                slickensides = c(NA, "few", "few", "many", "common"),
                oc_pct = c(3, 1.5, 1, 0.8, 0.5)), list(drainage_class = "well drained"))
  expect_true(is.na(r$trace$subgroups$CAA$passed))
  # a gleyed horizon only above the permafrost table is not a Gleysolic Cryosol
  expect_identical(.sg(list(designation = c("Ahy", "Bmy", "Bgy", "Cz"), top_cm = c(0, 8, 35, 55),
                            bottom_cm = c(8, 35, 55, 90), oc_pct = c(4, 1, 0.8, 0.5),
                            munsell_hue_moist = c("10YR", "10YR", "2.5Y", "2.5Y"),
                            munsell_value_moist = c(3, 4, 5, 5), munsell_chroma_moist = c(2, 4, 1, 2),
                            ph_cacl2 = 6, permafrost_temp_C = c(NA, NA, NA, -1)))$name,
                   "Brunisolic Eutric Turbic Cryosol")
  # horizons entered from the bottom up
  expect_identical(.sg(list(designation = c("Ck", "Bssk", "Bvgj", "Ah"), top_cm = c(95, 50, 15, 0),
                            bottom_cm = c(130, 95, 50, 15), clay_pct = 62,
                            munsell_value_dry = c(5, 5, 4, 4),
                            slickensides = c("common", "many", "few", NA),
                            oc_pct = c(0.5, 0.8, 1.5, 3)),
                       list(drainage_class = "well drained"))$name, "Gleyed Vertisol")
})

test_that("second review fixes: thick A, open bottoms, other notations", {
  skip_on_cran()
  gley <- function(designation, top, bottom, value, chroma, hue)
    .sg(list(designation = designation, top_cm = top, bottom_cm = bottom,
             oc_pct = c(5, rep(0.6, length(top) - 1L)), munsell_hue_moist = hue,
             munsell_value_moist = value, munsell_chroma_moist = chroma))
  # a gap below a thick A: the next mineral horizon is still tested
  expect_identical(gley(c("Ah", "Bg", "Cg"), c(0, 62, 90), c(60, 90, 130), c(2, 5, 5), c(1, 1, 1),
                        c("10YR", "2.5Y", "5Y"))$rsg_or_order, "Gleysolic")
  # the surface A is Ap and Ah together (80 cm): the Bg below it is tested
  expect_identical(gley(c("Ap", "Ah", "Bg"), c(0, 25, 80), c(25, 80, 100), c(2, 2, 5), c(1, 1, 1),
                        c("10YR", "10YR", "2.5Y"))$rsg_or_order, "Gleysolic")
  # a peat layer of known kind with no bottom depth counts as its own kind
  r <- .sg(list(designation = c("Of", "Om", "Oh"), top_cm = c(0, 50, 90), bottom_cm = c(50, 90, NA),
                oc_pct = c(45, 42, 40), fiber_content_rubbed_pct = c(60, 22, 5)))
  expect_identical(r$levels$name[2], "Mesisol")
  # a missing bottom depth ends where the next horizon begins
  expect_identical(.sg(list(designation = c("Of", "Omz", "Cz"), top_cm = c(0, 20, 80),
                            bottom_cm = c(20, NA, 120), oc_pct = c(45, 42, 1),
                            fiber_content_rubbed_pct = c(60, 25, NA),
                            permafrost_temp_C = c(NA, -1, -1)))$levels$name[2], "Organic Cryosol")
  # the deepest peat layer with no bottom may be deep: the order is undecided
  r <- .sg(list(designation = c("Of", "Om"), top_cm = c(0, 30), bottom_cm = c(30, NA),
                oc_pct = c(45, 42), fiber_content_rubbed_pct = c(60, 25)))
  expect_true("orders:B" %in% r$provisional_because)
  # a mineral layer over a layer of unknown organic status may be cumulic
  r <- .sg(list(designation = c("Om", "C", NA), top_cm = c(0, 60, 70), bottom_cm = c(60, 70, 75),
                oc_pct = c(42, 2, NA), fiber_content_rubbed_pct = c(25, NA, NA)))
  expect_true(is.na(r$trace$subgroups$BCF$passed))
  # USDA notation: the Bg under the organic surface is the uppermost B
  expect_identical(.sg(list(designation = c("Oe", "Bg", "Cf"), top_cm = c(0, 25, 50),
                            bottom_cm = c(25, 50, 100), oc_pct = c(40, 1, 0.5),
                            munsell_hue_moist = c(NA, "2.5Y", "10YR"),
                            munsell_value_moist = c(NA, 5, 4), munsell_chroma_moist = c(NA, 1, 3),
                            redoximorphic_features_pct = c(NA, NA, 0),
                            permafrost_temp_C = c(NA, NA, -1)),
                       list(designation_system = "usda"))$name, "Gleysolic Static Cryosol")
})

test_that("third review fixes: buried A, deep E, open layers, row order", {
  skip_on_cran()
  gley <- function(designation, top, bottom, value, chroma, hue, site = list())
    .sg(list(designation = designation, top_cm = top, bottom_cm = bottom,
             oc_pct = c(5, rep(0.6, length(top) - 1L)), munsell_hue_moist = hue,
             munsell_value_moist = value, munsell_chroma_moist = chroma), site)
  # an Ahb under the Ap is buried: the surface A is 25 cm, the Bg is too deep
  expect_false(identical(gley(c("Ap", "Ahb", "Bg", "Cg"), c(0, 25, 60, 90), c(25, 60, 90, 120),
                              c(2, 2, 5, 5), c(1, 1, 1, 1),
                              c("10YR", "10YR", "2.5Y", "5Y"))$rsg_or_order, "Gleysolic"))
  # a thick E reaching below 60 cm is tested (USDA notation)
  expect_identical(gley(c("A", "Eg", "Btg"), c(0, 10, 70), c(10, 70, 100), c(2, 6, 5), c(1, 1, 1),
                        c("10YR", "2.5Y", "2.5Y"), list(designation_system = "usda"))$rsg_or_order,
                   "Gleysolic")
  # the horizon right below a thick A is tested even when it is an Aeg
  expect_identical(.sg(list(designation = c("Ah", "Aeg", "Btg"), top_cm = c(0, 60, 70),
                            bottom_cm = c(60, 70, 100), oc_pct = c(5, 1, 0.5),
                            clay_pct = c(20, 15, 30), clay_films_amount = c(NA, "none", "common"),
                            munsell_hue_moist = c("10YR", "2.5Y", "2.5Y"),
                            munsell_value_moist = c(2, 6, 5), munsell_chroma_moist = c(1, 1, 1)))$rsg_or_order,
                   "Gleysolic")
  # an open peat layer alone in the middle tier decides it
  r <- .sg(list(designation = c("Om", "Oh"), top_cm = c(0, 40), bottom_cm = c(40, NA),
                oc_pct = c(42, 40), fiber_content_rubbed_pct = c(25, 5)))
  expect_identical(r$levels$name[2], "Humisol")
  # option 2 with an open organic layer under a mineral surface: undecided
  r <- .sg(list(designation = c("C", "Om"), top_cm = c(0, 20), bottom_cm = c(20, NA),
                oc_pct = c(1, 42), fiber_content_rubbed_pct = c(NA, 25)))
  expect_true("orders:B" %in% r$provisional_because)
  # rows given bottom-up are read in depth order; the pedon is not changed
  p <- make_cssc3_example("luvisolic")
  q <- PedonRecord$new(site = p$site, horizons = data.table::copy(p$horizons)[4:1, ])
  expect_identical(classify_cssc3(q, on_missing = "silent")$name, "Orthic Gray Luvisol")
  expect_identical(q$horizons$designation, c("Ck", "Bt", "Ae", "LFH"))
})
