# v0.9.316: HSCS 2024 (Micheli et al. 2024) subtype and variety properties of
# the morphological, hydromorphic and rock group (8.5, 8.6, on the horizons
# and properties of 8.2 and 8.3). Made-up profiles only.

# an example with some columns replaced (whole columns, or row edits as a
# list of row = value), or a new profile from `h` over the example's base
.hm_mod <- function(type, h = list(), site = list()) {
  p <- make_hscs2024_example(type)
  hz <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  n <- max(c(nrow(hz), vapply(h, function(v) if (is.list(v)) 0L else length(v), 0L)))
  if (n > nrow(hz)) hz <- hz[c(seq_len(nrow(hz)), rep(nrow(hz), n - nrow(hz))), , drop = FALSE]
  for (nm in names(h)) {
    v <- h[[nm]]
    if (is.list(v)) { if (!nm %in% names(hz)) hz[[nm]] <- NA; for (k in names(v)) hz[[nm]][as.integer(k)] <- v[[k]] }
    else hz[[nm]] <- if (length(v) == 1L) rep(v, nrow(hz)) else v
  }
  rownames(hz) <- NULL
  PedonRecord$new(site = utils::modifyList(p$site, site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(hz)))
}
.hm_p <- function(id, p) get(paste0(".hscs_p_", id), envir = asNamespace("soilKey"))(soilKey:::.hscs_horizons(p))
# The properties of this group; on integration (v0.9.316) the other groups'
# properties are stubbed to undecided inside .hm_name(), so the names here show
# this group's properties only.
.hm_mine <- c("agyagbemosodasos", "kovarvanyos", "podzolos", "vaskivalasos", "duzzado", "glejes",
              "pangovizes", "fako", "rozsdabarna", "voros", "kerges", "rendzina", "ranker", "erubaz",
              "kozethataros", "cementalt", "futo")
.hm_name <- function(p) {
  ns <- asNamespace("soilKey")
  ids <- setdiff(names(soilKey:::.hscs_properties_rules()$properties), .hm_mine)
  fns <- paste0(".hscs_p_", ids)
  fns <- fns[vapply(fns, exists, NA, envir = ns, inherits = FALSE)]
  stub <- function(x) soilKey:::.hscs_prop(NA, note = "stubbed")
  if (length(fns))
    do.call(testthat::local_mocked_bindings,
            c(stats::setNames(rep(list(stub), length(fns)), fns), list(.package = "soilKey", .env = environment())))
  classify_hscs2024(p, on_missing = "silent")$name
}
.hm_status <- function(p, id) {
  st <- classify_hscs2024(p, on_missing = "silent")$qualifiers$status
  st[st$id == id, , drop = FALSE]
}

test_that("agyagbemosodasos: a clay accumulation horizon within 100 cm", {
  skip_on_cran()
  r <- .hm_p("agyagbemosodasos", make_hscs2024_example("clay_illuvial"))
  expect_true(r$passed); expect_equal(r$top, 35)
  expect_false(.hm_p("agyagbemosodasos", make_hscs2024_example("sandy"))$passed)
  r <- .hm_p("agyagbemosodasos", .hm_mod("clay_illuvial", list(clay_pct = list(`3` = NA), silt_pct = list(`3` = NA),
                                                                  sand_pct = list(`3` = NA))))
  expect_true(is.na(r$passed)); expect_true("clay_pct" %in% r$missing)
})

# a sandy soil with clay lamellae: thin rows, or a designation naming them
.hm_lamellae <- function(...) {
  .hm_mod("sandy", utils::modifyList(list(
    designation = c("A", "E", "Bt", "E2", "Bt2", "C"), top_cm = c(0, 25, 40, 43, 55, 58),
    bottom_cm = c(25, 40, 43, 55, 58, 130), clay_pct = c(6, 3, 10, 3, 10, 2), silt_pct = c(8, 4, 5, 4, 5, 3),
    sand_pct = c(86, 93, 85, 93, 85, 95), oc_pct = c(0.8, 0.2, 0.1, 0.1, 0.1, 0.1),
    munsell_hue_moist = "2.5Y", munsell_value_moist = c(4, 6, 5, 6, 5, 6), munsell_chroma_moist = c(3, 3, 4, 3, 4, 3)),
    list(...)))
}

test_that("kovarvanyos: lamellae of clay accumulation within 100 cm", {
  skip_on_cran()
  p <- .hm_lamellae()
  r <- .hm_p("kovarvanyos", p)
  expect_true(r$passed); expect_equal(r$top, 40)
  # a Homoktalaj: the lamellae (6 cm in all) make no clay accumulation horizon
  expect_identical(classify_hscs2024(p, on_missing = "silent")$levels$code[1], "HOM")
  expect_identical(.hm_name(p), "Sek\u00e9lyen kov\u00e1rv\u00e1nyos Homoktalaj")
  # a designation naming the lamellae
  q <- .hm_lamellae(designation = c("A", "E", "Bt lamellae", "E2", "C", "C"), clay_pct = c(6, 3, 10, 3, 2, 2))
  expect_true(.hm_p("kovarvanyos", q)$passed)
  # no lamellae, and a lamellae designation with no clay measured
  expect_false(.hm_p("kovarvanyos", make_hscs2024_example("sandy"))$passed)
  q <- .hm_lamellae(designation = c("A", "E", "Bt lamellae", "E2", "C", "C"), clay_pct = list(`3` = NA),
                    silt_pct = list(`3` = NA), sand_pct = list(`3` = NA))
  expect_true(is.na(.hm_p("kovarvanyos", q)$passed))
  # a thin row with the clay of the row below it is not a lamella
  q <- .hm_lamellae(clay_pct = c(6, 3, 10, 10, 10, 10), silt_pct = 5, sand_pct = c(89, 92, 85, 85, 85, 85))
  expect_false(isTRUE(.hm_p("kovarvanyos", q)$passed))
})

test_that("podzolos: a red, acid, humus-bearing subsurface horizon within 100 cm", {
  skip_on_cran()
  red <- list(ph_h2o = c(5, 5, 5.5, 6), oc_pct = c(2, 0.6, 0.8, 0.2), munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"),
              munsell_chroma_moist = c(2, 3, 6, 4))
  p <- .hm_mod("clay_illuvial", red)
  r <- .hm_p("podzolos", p)
  expect_true(r$passed); expect_equal(r$top, 35)
  # 7.5YR with chroma above 4 is red; with chroma 4 it is not
  expect_true(.hm_p("podzolos", .hm_mod("clay_illuvial", utils::modifyList(red, list(
    munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR")))))$passed)
  q <- .hm_mod("clay_illuvial", utils::modifyList(red, list(munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"),
                                                            munsell_chroma_moist = c(2, 3, 4, 4))))
  expect_false(.hm_p("podzolos", q)$passed)
  # pH 6 or more, or too little carbon, fail; pH not measured is undecided
  expect_false(.hm_p("podzolos", .hm_mod("clay_illuvial", utils::modifyList(red, list(ph_h2o = 6.5))))$passed)
  expect_false(.hm_p("podzolos", .hm_mod("clay_illuvial", utils::modifyList(red, list(oc_pct = c(2, 0.6, 0.3, 0.2)))))$passed)
  expect_true(is.na(.hm_p("podzolos", .hm_mod("clay_illuvial", utils::modifyList(red, list(ph_h2o = NA_real_))))$passed))
  expect_identical(.hm_name(p), "Sek\u00e9lyen podzolos Agyagbemos\u00f3d\u00e1sos talaj (sek\u00e9lyen v\u00f6r\u00f6s)")
})

test_that("vaskivalasos: concretions or large red mottles in a subsurface horizon, 15 cm within 100 cm", {
  skip_on_cran()
  p <- .hm_mod("deposit", list(ferric_nodules_pct = c(0, 8, 0)))
  r <- .hm_p("vaskivalasos", p)
  expect_true(r$passed); expect_equal(r$top, 20)
  expect_identical(.hm_name(p), "Sek\u00e9lyen vaskiv\u00e1l\u00e1sos Hordal\u00e9ktalaj")
  # 4 % concretions, or none recorded, fail
  expect_false(.hm_p("vaskivalasos", .hm_mod("deposit", list(ferric_nodules_pct = c(0, 4, 0))))$passed)
  expect_false(.hm_p("vaskivalasos", make_hscs2024_example("deposit"))$passed)
  # red mottles of 20 %: their size is not recorded
  q <- .hm_mod("deposit", list(redoximorphic_features_pct = c(0, 20, 0), mottle_colors = c(NA, "5YR 5/8", NA)))
  expect_true(is.na(.hm_p("vaskivalasos", q)$passed))
  # grey or 7.5YR mottles are not iron mottles
  q <- .hm_mod("deposit", list(redoximorphic_features_pct = c(0, 20, 0), mottle_colors = c(NA, "10YR 6/2; 7.5YR 5/8", NA)))
  expect_false(.hm_p("vaskivalasos", q)$passed)
  # a Reti talaj and a Duzzado agyagtalaj
  expect_identical(.hm_name(.hm_mod("meadow", list(ferric_nodules_pct = c(0, 10, 0)))),
                   "Sek\u00e9lyen vaskiv\u00e1l\u00e1sos R\u00e9ti talaj")
  expect_identical(.hm_name(.hm_mod("swelling_clay", list(ferric_nodules_pct = c(0, 6, 0)))),
                   "Sek\u00e9lyen vaskiv\u00e1l\u00e1sos Duzzad\u00f3 agyagtalaj")
})

test_that("duzzado: a swelling horizon within 100 cm", {
  skip_on_cran()
  p <- .hm_mod("clay_illuvial", list(clay_pct = c(15, 12, 35, 32), slickensides = c("absent", "absent", "common", "few")))
  r <- .hm_p("duzzado", p)
  expect_true(r$passed); expect_equal(r$top, 35)
  expect_identical(.hm_name(p), "Agyagbemos\u00f3d\u00e1sos talaj (sek\u00e9lyen duzzad\u00f3)")
  expect_false(.hm_p("duzzado", .hm_mod("clay_illuvial", list(slickensides = "few")))$passed)
  # slickensides not recorded under blocky peds: undecided
  expect_true(is.na(.hm_p("duzzado", make_hscs2024_example("clay_illuvial"))$passed))
})

test_that("glejes: 25 cm with the groundwater gley pattern and reductive conditions within 100 cm", {
  skip_on_cran()
  p <- make_hscs2024_example("solonchak")
  r <- .hm_p("glejes", p)
  expect_true(r$passed); expect_equal(r$top, 20)
  expect_identical(.hm_name(p), "Sek\u00e9lyen glejes Szoloncs\u00e1k talaj")
  expect_false(.hm_p("glejes", make_hscs2024_example("steppe"))$passed)
  # reduced colours with no sign of reductive conditions: undecided; a
  # negative dipyridyl test there: no
  q <- .hm_mod("steppe", list(munsell_hue_moist = c("10YR", "10YR", "5Y", "5Y"), munsell_chroma_moist = c(2, 2, 1, 1)))
  expect_true(is.na(.hm_p("glejes", q)$passed))
  q <- .hm_mod("steppe", list(munsell_hue_moist = c("10YR", "10YR", "5Y", "5Y"), munsell_chroma_moist = c(2, 2, 1, 1),
                              childs_test = "negative"))
  expect_false(.hm_p("glejes", q)$passed)
  q <- .hm_mod("steppe", list(munsell_hue_moist = c("10YR", "10YR", "5Y", "5Y"), munsell_chroma_moist = c(2, 2, 1, 1),
                              childs_test = c(NA, NA, NA, "positive")))
  r <- .hm_p("glejes", q)
  expect_true(r$passed); expect_equal(r$top, 60)
  # an Antropogen talaj gleyed from 70 cm
  a <- .hm_mod("anthropogenic", list(designation = c("Au", "2Cu", "3Cr"), top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 130),
                                     munsell_hue_moist = c("10YR", "10YR", "5Y"), munsell_chroma_moist = c(2, 3, 1)))
  expect_identical(.hm_name(a), "K\u00f6z\u00e9pm\u00e9lyen glejes Antropog\u00e9n talaj")
  # within 100 cm of the mineral soil surface, under a litter layer
  q <- .hm_mod("steppe", list(designation = c("O", "A", "Bk", "Cr"), top_cm = c(0, 10, 60, 105), bottom_cm = c(10, 60, 105, 140),
                              oc_pct = c(30, 2, 0.5, 0.3), munsell_hue_moist = c("10YR", "10YR", "10YR", "5Y"),
                              munsell_chroma_moist = c(2, 2, 3, 1)))
  r <- .hm_p("glejes", q)
  expect_true(r$passed); expect_equal(r$top, 105)
})

# a clay-illuvial soil with a stagnic colour pattern in the E and the Bt,
# reductive conditions shown below 50 cm (within 50 cm, its mottles would
# make it a Reti talaj)
.hm_stagnic <- function(...) {
  .hm_mod("clay_illuvial", utils::modifyList(list(
    munsell_hue_moist = "10YR", munsell_value_moist = c(3, 6, 4, 5), munsell_chroma_moist = c(2, 2, 4, 4),
    redoximorphic_features_pct = c(0, 10, 20, 0), mottle_colors = c(NA, "7.5YR 5/6", "10YR 6/2; 5YR 5/6", NA),
    childs_test = c(NA, NA, NA, "positive")), list(...)))
}

test_that("the stagnic colour pattern of 8.3, read from the matrix and mottle colours", {
  skip_on_cran()
  x <- soilKey:::.hscs_horizons(.hm_stagnic())
  expect_identical(soilKey:::.hscs_pm_stagnic_rows(x), c(FALSE, TRUE, TRUE, FALSE))
  pair <- function(...) soilKey:::.hscs_pm_stagnic_pair(...)
  hue <- function(h) soilKey:::.hscs_pm_hue(h)
  # a grey matrix with rust mottles; red mottles alone (no pale part) are not it
  expect_true(pair(hue(c("2.5Y", "10YR")), c(6, 5), c(2, 6), m = 1L))
  expect_false(pair(hue(c("10YR", "7.5YR")), c(4, 5), c(3, 6), m = 1L))
  # against the matrix: pale and red mottles in a brown matrix
  expect_true(pair(hue(c("10YR", "10YR", "5YR")), c(5, 6, 5), c(4, 2, 6), m = 1L))
  # a value missing leaves it undecided
  expect_true(is.na(pair(hue(c("2.5Y", "10YR")), c(NA, 5), c(2, 6), m = 1L)))
  # a g suffix; mottles recorded with no colours
  expect_true(soilKey:::.hscs_pm_stagnic_rows(soilKey:::.hscs_horizons(
    .hm_mod("clay_illuvial", list(designation = c("A", "E", "Btg", "C")))))[3])
  expect_true(is.na(soilKey:::.hscs_pm_stagnic_rows(soilKey:::.hscs_horizons(
    .hm_mod("clay_illuvial", list(redoximorphic_features_pct = c(0, 0, 15, 0)))))[3]))
})

test_that("pangovizes: reductive conditions and the stagnic pattern in 25 % of the soil volume", {
  skip_on_cran()
  p <- .hm_stagnic()
  r <- .hm_p("pangovizes", p)
  expect_true(r$passed); expect_equal(r$top, 20)
  expect_identical(.hm_name(p), "Sek\u00e9lyen pang\u00f3vizes Agyagbemos\u00f3d\u00e1sos talaj")
  # no test for reductive conditions: undecided; negative tests throughout: no
  expect_true(is.na(.hm_p("pangovizes", .hm_stagnic(childs_test = NA_character_))$passed))
  expect_false(.hm_p("pangovizes", .hm_stagnic(childs_test = "negative"))$passed)
  # the pattern in the E only (15 cm of 100): no
  expect_false(.hm_p("pangovizes", .hm_stagnic(mottle_colors = c(NA, "7.5YR 5/6", NA, NA),
                                               redoximorphic_features_pct = c(0, 10, 0, 0)))$passed)
  # no mottles recorded: no pattern
  expect_false(.hm_p("pangovizes", make_hscs2024_example("clay_illuvial"))$passed)
})

test_that("fako: a pale subsurface horizon (dry colour), 5 cm or more", {
  skip_on_cran()
  dry <- list(munsell_value_dry = c(4, 7, 4, 4, 4), munsell_chroma_dry = c(2, 2, 3, 3, 3))
  p <- .hm_mod("solonetz", dry)
  r <- .hm_p("fako", p)
  expect_true(r$passed); expect_equal(r$top, 12)
  expect_identical(.hm_name(p), "Sek\u00e9lyen fak\u00f3 Szolonyec talaj")
  # value 5 or 6 with chroma 3 or less too; chroma 4 or value 4 not
  expect_true(.hm_p("fako", .hm_mod("solonetz", list(munsell_value_dry = c(4, 5, 4, 4, 4), munsell_chroma_dry = 3)))$passed)
  expect_false(.hm_p("fako", .hm_mod("solonetz", list(munsell_value_dry = c(4, 7, 4, 4, 4), munsell_chroma_dry = 4)))$passed)
  expect_false(.hm_p("fako", .hm_mod("solonetz", list(munsell_value_dry = 4, munsell_chroma_dry = 2)))$passed)
  # the surface horizon does not count; no dry colours: undecided
  expect_false(.hm_p("fako", .hm_mod("solonetz", list(munsell_value_dry = c(7, 4, 4, 4, 4), munsell_chroma_dry = 2)))$passed)
  expect_true(is.na(.hm_p("fako", make_hscs2024_example("solonetz"))$passed))
  # a Szoloncsak talaj with a pale C
  q <- .hm_mod("solonchak", list(munsell_value_dry = c(4, 7, 4), munsell_chroma_dry = c(2, 2, 2)))
  expect_identical(.hm_name(q), "Sek\u00e9lyen glejes, sek\u00e9lyen fak\u00f3 Szoloncs\u00e1k talaj")
})

test_that("kerges: the columnar horizon begins within 10 cm (Szolonyec talajok)", {
  skip_on_cran()
  p <- .hm_mod("solonetz", list(top_cm = c(0, 5, 9, 45, 80), bottom_cm = c(5, 9, 45, 80, 120)))
  expect_true(.hm_p("kerges", p)$passed)
  expect_identical(.hm_name(p), "K\u00e9rges Szolonyec talaj")
  expect_false(.hm_p("kerges", make_hscs2024_example("solonetz"))$passed)
  q <- .hm_mod("solonetz", list(top_cm = c(0, 5, 9, 45, 80), bottom_cm = c(5, 9, 45, 80, 120),
                                structure_type = c(NA, NA, "columnar", "prismatic", "massive")))
  expect_true(.hm_p("kerges", q)$passed)
  q <- .hm_mod("solonetz", list(structure_type = c(NA, "platy", "columnar", "prismatic", "massive")))
  expect_true(is.na(.hm_p("kerges", q)$passed))
})

test_that("rozsdabarna: 30 cm of hue 10YR or redder, or chroma 5 or more, within 100 cm", {
  skip_on_cran()
  p <- make_hscs2024_example("sandy")
  r <- .hm_p("rozsdabarna", p)
  expect_true(r$passed); expect_equal(r$top, 0)
  expect_identical(.hm_name(p), "Felsz\u00ednt\u0151l rozsdabarna Homoktalaj")
  expect_false(.hm_p("rozsdabarna", .hm_mod("sandy", list(munsell_hue_moist = "2.5Y")))$passed)
  r <- .hm_p("rozsdabarna", .hm_mod("sandy", list(munsell_hue_moist = "2.5Y", munsell_chroma_moist = c(2, 5, 5))))
  expect_true(r$passed); expect_equal(r$top, 25)
  expect_true(is.na(.hm_p("rozsdabarna", .hm_mod("sandy", list(munsell_hue_moist = NA_character_)))$passed))
})

test_that("voros: 15 cm redder than 7.5YR, or 7.5YR with moist chroma above 4, within 150 cm", {
  skip_on_cran()
  p <- .hm_mod("brown_earth", list(munsell_chroma_moist = c(3, 6, 4)))
  r <- .hm_p("voros", p)
  expect_true(r$passed); expect_equal(r$top, 25)
  expect_identical(.hm_name(p), "Barnaf\u00f6ld (sek\u00e9lyen v\u00f6r\u00f6s)")
  # 7.5YR 4/4 is not red; the soil below 120 cm may be
  expect_true(is.na(.hm_p("voros", make_hscs2024_example("brown_earth"))$passed))
  q <- .hm_mod("brown_earth", list(bottom_cm = c(25, 60, 160)))
  expect_false(.hm_p("voros", q)$passed)
  # m\u00e9lyben: a red layer below 100 cm; the dry hue when no moist hue is recorded
  q <- .hm_mod("brown_earth", list(top_cm = c(0, 25, 60, 110), bottom_cm = c(25, 60, 110, 160),
                                   designation = c("A", "Bw", "C", "2C"), munsell_hue_moist = c("10YR", "10YR", "10YR", "5YR"),
                                   munsell_value_moist = c(3, 4, 5, 5), munsell_chroma_moist = c(3, 4, 4, 6),
                                   clay_pct = c(20, 22, 20, 20), silt_pct = c(42, 40, 42, 42), sand_pct = c(38, 38, 38, 38),
                                   oc_pct = c(1.5, 0.5, 0.2, 0.1)))
  s <- .hm_status(q, "voros")
  expect_true(s$passed); expect_identical(s$label, "m\u00e9lyben v\u00f6r\u00f6s")
  q <- .hm_mod("brown_earth", list(munsell_hue_moist = c("10YR", NA, "10YR"), munsell_hue_dry = c(NA, "5YR", NA)))
  expect_true(.hm_p("voros", q)$passed)
})

# a Kozethatasu talaj on rock named by the site's parent material
.hm_rock <- function(pm, ...) .hm_mod("rock_influenced", list(...), site = list(parent_material = pm))

test_that("rendzina, ranker, erubaz: the rock the soil formed on", {
  skip_on_cran()
  v <- function(pm, ...) {
    p <- .hm_rock(pm, ...)
    c(rendzina = .hm_p("rendzina", p)$passed, ranker = .hm_p("ranker", p)$passed, erubaz = .hm_p("erubaz", p)$passed)
  }
  expect_identical(v("limestone"), c(rendzina = TRUE, ranker = FALSE, erubaz = FALSE))
  expect_identical(v("m\u00e9szk\u0151"), c(rendzina = TRUE, ranker = FALSE, erubaz = FALSE))
  expect_identical(v("dolomit"), c(rendzina = TRUE, ranker = FALSE, erubaz = FALSE))
  expect_identical(v("granite"), c(rendzina = FALSE, ranker = TRUE, erubaz = FALSE))
  expect_identical(v("homokk\u0151"), c(rendzina = FALSE, ranker = TRUE, erubaz = FALSE))
  expect_identical(v("andezit"), c(rendzina = FALSE, ranker = FALSE, erubaz = TRUE))
  expect_identical(v("basalt"), c(rendzina = FALSE, ranker = FALSE, erubaz = TRUE))
  # a rhyolite tuff is a tuff
  expect_identical(v("riolittufa"), c(rendzina = FALSE, ranker = FALSE, erubaz = TRUE))
  # loose material, or loess over limestone: none
  expect_identical(v("loess over limestone"), c(rendzina = FALSE, ranker = FALSE, erubaz = FALSE))
  expect_identical(v("homok"), c(rendzina = FALSE, ranker = FALSE, erubaz = FALSE))
  # two rocks, an unknown name, no parent material: undecided
  expect_identical(v("basalt and limestone"), c(rendzina = NA, ranker = FALSE, erubaz = NA))
  expect_identical(v("bedrock"), c(rendzina = NA, ranker = NA, erubaz = NA))
  expect_true(is.na(.hm_p("rendzina", make_hscs2024_example("rock_influenced"))$passed))
  # no R horizon in a description that does not end on rock: undecided
  p <- .hm_mod("brown_earth", site = list(parent_material = "limestone"))
  expect_true(is.na(.hm_p("rendzina", p)$passed))
  # names
  expect_identical(.hm_name(.hm_rock("limestone")), "Rendzina K\u0151zethat\u00e1s\u00fa talaj")
  b <- .hm_mod("brown_earth", list(designation = c("A", "Bw", "C", "R"), top_cm = c(0, 25, 60, 120),
                                   bottom_cm = c(25, 60, 120, 150), clay_pct = list(`4` = NA), silt_pct = list(`4` = NA),
                                   sand_pct = list(`4` = NA), oc_pct = list(`4` = NA)), site = list(parent_material = "granite"))
  expect_identical(.hm_name(b), "Ranker Barnaf\u00f6ld")
})

test_that("kozethataros: hard rock or debris within 100 cm", {
  skip_on_cran()
  p <- .hm_mod("weakly_developed", list(designation = c("A", "C", "R"), top_cm = c(0, 15, 80), bottom_cm = c(15, 80, 120),
                                        clay_pct = list(`3` = NA), silt_pct = list(`3` = NA), sand_pct = list(`3` = NA),
                                        oc_pct = list(`3` = NA)))
  r <- .hm_p("kozethataros", p)
  expect_true(r$passed); expect_equal(r$top, 80)
  expect_identical(.hm_name(p), "F\u00f6ldeskop\u00e1r (k\u00f6z\u00e9pm\u00e9lyen k\u0151zethat\u00e1ros)")
  # debris (85 % coarse fragments) from 40 cm
  q <- .hm_mod("weakly_developed", list(designation = c("A", "C", "C2"), top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 100),
                                        coarse_fragments_pct = c(0, 10, 85)))
  r <- .hm_p("kozethataros", q)
  expect_true(r$passed); expect_equal(r$top, 40)
  expect_false(.hm_p("kozethataros", make_hscs2024_example("weakly_developed"))$passed)
  expect_true(is.na(.hm_p("kozethataros", .hm_mod("weakly_developed", list(bottom_cm = c(15, 60))))$passed))
  # rock at 100 cm is not within 100 cm; a Mezosegi talaj on debris from 90 cm
  q <- .hm_mod("weakly_developed", list(designation = c("A", "C", "R"), top_cm = c(0, 15, 100), bottom_cm = c(15, 100, 120)))
  expect_false(.hm_p("kozethataros", q)$passed)
  m <- .hm_mod("steppe", list(coarse_fragments_pct = c(0, 0, 0, 85)))
  expect_identical(.hm_name(m), "Mez\u0151s\u00e9gi talaj (k\u00f6z\u00e9pm\u00e9lyen k\u0151zethat\u00e1ros)")
})

# a Karbonat talaj with a cemented layer from 60 cm
.hm_cemented <- function(...) {
  .hm_mod("carbonate", utils::modifyList(list(
    designation = c("Ak", "Ck", "Ckm", "Ck2"), top_cm = c(0, 20, 60, 80), bottom_cm = c(20, 60, 80, 110),
    caco3_pct = c(30, 35, 45, 35), oc_pct = c(1.5, 0.3, 0.1, 0.1), clay_pct = 18, silt_pct = 57, sand_pct = 25,
    munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6, 7, 6), munsell_chroma_moist = c(2, 3, 2, 3)), list(...)))
}

test_that("cementalt: a cemented hard layer within 100 cm, named by its agent", {
  skip_on_cran()
  p <- .hm_cemented()
  r <- .hm_p("cementalt", p)
  expect_true(r$passed); expect_equal(r$top, 60); expect_identical(r$label, "cement\u00e1lt karbon\u00e1tos")
  expect_identical(.hm_name(p), "Karbon\u00e1t talaj (k\u00f6z\u00e9pm\u00e9lyen cement\u00e1lt karbon\u00e1tos)")
  lab <- function(...) .hm_p("cementalt", .hm_cemented(...))$label
  expect_identical(lab(designation = c("Ak", "Ck", "Bqm", "Ck2")), "cement\u00e1lt kovasavas")
  expect_identical(lab(designation = c("Ak", "Ck", "Bsm", "Ck2")), "cement\u00e1lt vaskiv\u00e1l\u00e1sos")
  expect_identical(lab(designation = c("Ak", "Ck", "Bym", "Ck2")), "cement\u00e1lt gipszes")
  expect_identical(lab(designation = c("Ak", "Ck", "C", "Ck2"), cementation_class = c(NA, NA, "strongly", NA),
                       caco3_pct = c(30, 35, 0, 35), caso4_pct = c(0, 0, 8, 0)), "cement\u00e1lt gipszes")
  expect_identical(lab(designation = c("Ak", "Ck", "C", "Ck2"), cementation_class = c(NA, NA, "indurated", NA),
                       caco3_pct = c(30, 35, 0, 35), coarse_fragments_pct = c(0, 0, 60, 0),
                       coarse_fragments_shape = c(NA, NA, "rounded", NA)), "cement\u00e1lt kavicsos")
  expect_identical(lab(designation = c("Ak", "Ck", "C", "Ck2"), pan_type = c(NA, NA, "calcrete", NA)),
                   "cement\u00e1lt karbon\u00e1tos")
  # no agent recorded: the catalogue's name
  r <- .hm_p("cementalt", .hm_cemented(designation = c("Ak", "Ck", "C", "Ck2"), cementation_class = c(NA, NA, "strongly", NA),
                                       caco3_pct = c(30, 35, 0, 35)))
  expect_true(r$passed); expect_null(r$label)
  # weakly cemented, or none: no; a description that ends above 100 cm: undecided
  expect_false(.hm_p("cementalt", .hm_cemented(designation = c("Ak", "Ck", "C", "Ck2"),
                                               cementation_class = c(NA, NA, "weakly", NA)))$passed)
  expect_false(.hm_p("cementalt", make_hscs2024_example("carbonate"))$passed)
  expect_true(is.na(.hm_p("cementalt", .hm_mod("carbonate", list(bottom_cm = c(20, 60))))$passed))
})

test_that("futo: a drifting sand surface (Homoktalajok)", {
  skip_on_cran()
  p <- .hm_mod("sandy", site = list(wind_deflation = TRUE))
  expect_true(.hm_p("futo", p)$passed)
  expect_identical(.hm_name(p), "Felsz\u00ednt\u0151l rozsdabarna, fut\u00f3 Homoktalaj")
  expect_false(.hm_p("futo", .hm_mod("sandy", site = list(wind_deflation = FALSE)))$passed)
  expect_true(is.na(.hm_p("futo", make_hscs2024_example("sandy"))$passed))
  expect_true(.hm_p("futo", .hm_mod("sandy", site = list(surface_condition = "bare; wind deflation")))$passed)
  # a pedal surface horizon is not structureless single grains
  q <- .hm_mod("sandy", list(structure_grade = c("moderate", "structureless", "structureless"),
                             structure_type = c("granular", "single grain", "single grain")),
               site = list(surface_condition = "wind deflation"))
  expect_false(.hm_p("futo", q)$passed)
})
