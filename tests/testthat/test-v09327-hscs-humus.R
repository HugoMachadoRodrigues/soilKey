# v0.9.316: the humus, organic and peat properties of the HSCS 2024
# (Micheli et al. 2024, 8.1 and 8.5): humus classes, umbric and black
# surface horizons, peat and litter at the surface, the peat subtypes of the
# Laptalajok. Made-up profiles only.

# a copy of the example with columns (and site fields) changed
.hk_mod <- function(p, ..., site = NULL) {
  p <- p$clone(deep = TRUE)
  h <- as.data.frame(p$horizons, stringsAsFactors = FALSE)
  a <- list(...)
  for (k in names(a)) h[[k]] <- a[[k]]
  p$horizons <- ensure_horizon_schema(data.table::as.data.table(h))
  if (!is.null(site)) p$site <- utils::modifyList(p$site, site)
  p
}
# a profile from columns, with what the earlier types read (as the examples)
.hk_ped <- function(h, site = list()) {
  base <- list(soluble_salts_pct = 0.02, na_cmol = 0.2, mg_cmol = 3, cec_cmol = 20,
               redoximorphic_features_pct = 0, coarse_fragments_pct = 0, caco3_pct = 0)
  h <- utils::modifyList(base, h)
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  PedonRecord$new(site = utils::modifyList(list(id = "hk", country = "HU", designation_system = "fao2006"), site),
                  horizons = ensure_horizon_schema(data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.hk_p <- function(p, id) {
  fn <- get(paste0(".hscs_p_", id), envir = asNamespace("soilKey"))
  fn(soilKey:::.hscs_horizons(p))$passed
}
.hk_res <- function(p) classify_hscs2024(p, on_missing = "silent")
.hk_type <- function(r) r$levels$code[r$levels$level == "types"][1]

test_that("the humus classes read the humic horizon layer by layer", {
  skip_on_cran()
  # steppe: A 0-40 2.5 % OC, AB 40-60 1.2 %, both humic
  p <- make_hscs2024_example("steppe")
  expect_true(.hk_p(p, "igen_erosen_humuszos"))
  expect_false(.hk_p(p, "humuszgazdag"))
  expect_false(.hk_p(p, "humuszos"))
  # the A at 1.5 %: humuszgazdag
  p1 <- .hk_mod(p, oc_pct = c(1.5, 1.2, 0.5, 0.3))
  expect_true(.hk_p(p1, "humuszgazdag"))
  expect_false(.hk_p(p1, "igen_erosen_humuszos"))
  # 2 % only in the upper 15 cm: the 20 cm of the humic horizon are 1-2 %
  p2 <- .hk_mod(p, top_cm = c(0, 15, 60, 90), bottom_cm = c(15, 60, 90, 130), oc_pct = c(3, 1.2, 0.5, 0.3))
  expect_true(.hk_p(p2, "humuszgazdag"))
  # brown earth with base saturation: the A at 0.8 % is humuszos
  b <- .hk_mod(make_hscs2024_example("brown_earth"), bs_pct = c(70, 70, 70), oc_pct = c(0.8, 0.5, 0.2))
  expect_true(.hk_p(b, "humuszos"))
  expect_false(.hk_p(b, "humuszgazdag"))
  # base saturation not recorded on a non-calcareous A: undecided
  b0 <- make_hscs2024_example("brown_earth")
  expect_true(is.na(.hk_p(b0, "humuszgazdag")))
  # organic carbon of 8 % or more is lapfold, not igen erosen humuszos
  m <- .hk_mod(make_hscs2024_example("meadow"), oc_pct = c(10, 1, 0.4), structure_type = c("granular", NA, NA),
               bs_pct = c(80, NA, NA))
  expect_false(.hk_p(m, "igen_erosen_humuszos"))
  expect_true(.hk_p(m, "lapfoldes"))
})

test_that("humuszszegeny, lapfoldes and nyers read the A horizon at the mineral soil surface", {
  skip_on_cran()
  w <- make_hscs2024_example("weakly_developed")      # A 0-15 at 0.5 % OC
  expect_true(.hk_p(w, "humuszszegeny"))
  expect_false(.hk_p(w, "nyers"))
  expect_false(.hk_p(w, "lapfoldes"))
  expect_false(.hk_p(.hk_mod(w, oc_pct = c(0.8, 0.1)), "humuszszegeny"))
  expect_true(is.na(.hk_p(.hk_mod(w, oc_pct = c(NA, 0.1)), "humuszszegeny")))
  # the A with its subhorizons, thickness-weighted (Hugo's decision, 2026-10-30):
  # A1 0-10 at 0.7 % and A2 10-30 at 0.4 % make 0.5 %
  a2 <- .hk_mod(make_hscs2024_example("clay_illuvial"), designation = c("A1", "A2", "Bt", "C"),
                top_cm = c(0, 10, 30, 80), bottom_cm = c(10, 30, 80, 120), oc_pct = c(0.7, 0.4, 0.4, 0.2))
  expect_true(.hk_p(a2, "humuszszegeny"))
  # the E below the A is not part of it
  expect_false(.hk_p(.hk_mod(make_hscs2024_example("clay_illuvial"), oc_pct = c(0.8, 0.3, 0.4, 0.2)), "humuszszegeny"))
  # an undesignated row under the A may belong to it: 0.5 % alone, 0.75 % with it
  u <- .hk_mod(make_hscs2024_example("clay_illuvial"), designation = c("A", NA, "Bt", "C"),
               oc_pct = c(0.5, 0.9, 0.4, 0.2), top_cm = c(0, 15, 40, 80), bottom_cm = c(15, 40, 80, 120))
  expect_true(is.na(.hk_p(u, "humuszszegeny")))
  expect_true(.hk_p(.hk_mod(u, oc_pct = c(0.5, 0.4, 0.4, 0.2)), "humuszszegeny"))
  # a top row with no designation is the surface horizon alone
  expect_true(.hk_p(.hk_mod(u, designation = c(NA, NA, "Bt", "C")), "humuszszegeny"))
  # nyers: below 0.2 % and no diagnostic horizon
  n <- .hk_mod(w, oc_pct = c(0.1, 0.05))
  expect_true(.hk_p(n, "nyers"))
  expect_false(.hk_p(n, "humuszszegeny"))
  # a cambic horizon is a diagnostic horizon
  expect_false(.hk_p(.hk_mod(make_hscs2024_example("brown_earth"), oc_pct = c(0.1, 0.1, 0.05)), "nyers"))
  # so are the podzolic and iron accumulation horizons (the podzolos and
  # vaskivalasos properties)
  pz <- .hk_ped(list(designation = c("A", "Bs", "C"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 120),
                     oc_pct = c(0.1, 0.8, 0.05), ph_h2o = c(5, 4.6, 5.2), clay_pct = c(3, 5, 3), sand_pct = c(92, 88, 94),
                     silt_pct = c(5, 7, 3), munsell_hue_moist = c("10YR", "5YR", "10YR"),
                     munsell_value_moist = c(5, 4, 6), munsell_chroma_moist = c(2, 6, 4)))
  expect_true(.hk_p(pz, "podzolos"))
  expect_false(.hk_p(pz, "nyers"))
  fe <- .hk_mod(n, ferric_nodules_pct = c(0, 10))
  expect_true(.hk_p(fe, "vaskivalasos"))
  expect_false(.hk_p(fe, "nyers"))
  # salts not recorded leave a solonchak horizon possible
  expect_true(is.na(.hk_p(.hk_mod(n, soluble_salts_pct = c(NA, NA)), "nyers")))
  # a lapfold surface over peat in a Laptalaj
  lp <- .hk_ped(list(designation = c("A", "H1", "H2", "Cr"), top_cm = c(0, 20, 40, 65), bottom_cm = c(20, 40, 65, 120),
                     oc_pct = c(12, 35, 30, 1), bulk_density_g_cm3 = c(0.9, 0.3, 0.3, 1.4), clay_pct = c(30, NA, NA, 20)))
  expect_true(.hk_p(lp, "lapfoldes"))
  # a peat at the surface: the mineral soil under it is no surface horizon
  pt <- .hk_mod(make_hscs2024_example("peat"), oc_pct = c(35, 30, 10))
  expect_false(.hk_p(pt, "lapfoldes"))
  expect_false(.hk_p(pt, "humuszszegeny"))
})

test_that("umbrikus, fekete and mely humuszos retegu", {
  skip_on_cran()
  b <- .hk_mod(make_hscs2024_example("brown_earth"), bs_pct = c(30, 40, 50))
  expect_true(.hk_p(b, "umbrikus"))
  expect_false(.hk_p(b, "humuszgazdag"))
  expect_false(.hk_p(.hk_mod(b, bs_pct = c(70, 40, 50)), "umbrikus"))
  expect_true(is.na(.hk_p(make_hscs2024_example("brown_earth"), "umbrikus")))
  s <- make_hscs2024_example("steppe")                  # humic 0-60, chroma 2
  expect_true(.hk_p(s, "fekete"))
  expect_true(.hk_p(s, "mely_humuszos_retegu"))
  expect_false(.hk_p(.hk_mod(s, munsell_chroma_moist = c(3, 3, 3, 3)), "fekete"))
  expect_true(is.na(.hk_p(.hk_mod(s, munsell_chroma_moist = c(NA, 2, 3, 3)), "fekete")))
  # the AB not dark: the humic horizon is 40 cm
  expect_false(.hk_p(.hk_mod(s, munsell_value_moist = c(2, 4, 5, 6)), "mely_humuszos_retegu"))
  # the AB with no structure recorded may continue it
  expect_true(is.na(.hk_p(.hk_mod(s, structure_type = c("granular", NA, "subangular blocky", "massive"),
                                  structure_grade = c("strong", NA, "weak", "structureless")), "mely_humuszos_retegu")))
})

test_that("tozeges and szaraztozeges tell peat from litter", {
  skip_on_cran()
  hor <- list(designation = c("H", "A", "C1", "C2"), top_cm = c(0, 15, 35, 60), bottom_cm = c(15, 35, 60, 120),
              oc_pct = c(30, 1.2, 0.3, 0.5), clay_pct = c(NA, 18, 12, 15), silt_pct = c(NA, 62, 28, 70),
              sand_pct = c(NA, 20, 60, 15), structure_grade = c(NA, "weak", "structureless", "structureless"),
              structure_type = c(NA, "granular", "massive", "massive"),
              munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, 5, 5), munsell_chroma_moist = c(1, 2, 3, 3))
  p <- .hk_ped(hor)
  expect_true(.hk_p(p, "tozeges"))                      # FAO H: saturated
  expect_false(.hk_p(p, "szaraztozeges"))
  o <- .hk_ped(utils::modifyList(hor, list(designation = c("O", "A", "C1", "C2"))))
  expect_false(.hk_p(o, "tozeges"))                     # FAO O: not saturated
  expect_false(.hk_p(.hk_ped(utils::modifyList(hor, list(bottom_cm = c(8, 35, 60, 120), top_cm = c(0, 8, 35, 60)))),
                     "tozeges"))                        # 8 cm
  # an organic layer with no designation: the drainage class decides
  u <- utils::modifyList(hor, list(designation = c(NA, "A", "C1", "C2")))
  expect_true(is.na(.hk_p(.hk_ped(u), "tozeges")))
  expect_true(.hk_p(.hk_ped(u, list(drainage_class = "very poorly drained")), "tozeges"))
  expect_false(.hk_p(.hk_ped(u, list(drainage_class = "well drained")), "tozeges"))
  expect_true(is.na(.hk_p(.hk_ped(u, list(drainage_class = "well drained", artificially_drained = TRUE)), "tozeges")))
  # measured days of saturation come first
  expect_false(.hk_p(.hk_ped(utils::modifyList(hor, list(water_saturation_days = c(60, NA, NA, NA)))), "tozeges"))
  # litter: an O 12 cm thick under forest
  be <- list(designation = c("O", "A", "Bw", "C"), top_cm = c(0, 12, 37, 72), bottom_cm = c(12, 37, 72, 132),
             oc_pct = c(35, 1.5, 0.5, 0.2), clay_pct = c(NA, 20, 22, 20), silt_pct = c(NA, 42, 40, 42),
             sand_pct = c(NA, 38, 38, 38), structure_grade = c(NA, "moderate", "moderate", "structureless"),
             structure_type = c(NA, "granular", "subangular blocky", "massive"),
             munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(2, 3, 4, 5),
             munsell_chroma_moist = c(1, 3, 4, 4))
  expect_true(.hk_p(.hk_ped(be, list(vegetation = "beech forest")), "szaraztozeges"))
  expect_true(.hk_p(.hk_ped(be, list(land_use = "erd\u0151")), "szaraztozeges"))
  expect_false(.hk_p(.hk_ped(be, list(vegetation = "grassland")), "szaraztozeges"))   # a grass mat
  expect_true(is.na(.hk_p(.hk_ped(be), "szaraztozeges")))
  expect_false(.hk_p(.hk_ped(be, list(vegetation = "beech forest")), "tozeges"))
  # a forest floor in the L, F, H notation: its H is litter, not peat
  ff <- .hk_ped(list(designation = c("L", "F", "H", "A", "Bw", "C"), top_cm = c(0, 3, 7, 12, 37, 72),
                     bottom_cm = c(3, 7, 12, 37, 72, 132), oc_pct = c(45, 40, 30, 1.5, 0.5, 0.2),
                     clay_pct = c(NA, NA, NA, 20, 22, 20)))
  expect_true(.hk_p(ff, "szaraztozeges"))
  expect_false(.hk_p(ff, "tozeges"))
})

test_that("the peat subtypes of the Laptalajok", {
  skip_on_cran()
  p <- make_hscs2024_example("peat")                   # H1 0-30, H2 30-80, Cr
  expect_true(is.na(.hk_p(p, "rostos_tozeges")))
  f <- .hk_mod(p, fiber_content_rubbed_pct = c(80, 70, NA))
  expect_true(.hk_p(f, "rostos_tozeges"))
  expect_false(.hk_p(f, "vegyes_tozeges"))
  expect_false(.hk_p(f, "kotus"))
  # weighted over the peat: (30 x 80 + 50 x 50) / 80 = 61 %: vegyes
  v <- .hk_mod(p, fiber_content_rubbed_pct = c(80, 50, NA))
  expect_true(.hk_p(v, "vegyes_tozeges"))
  expect_false(.hk_p(v, "rostos_tozeges"))
  expect_true(.hk_p(.hk_mod(p, fiber_content_rubbed_pct = c(10, 5, NA)), "kotus"))
  # von Post, the FAO suffix, the unrubbed fibre as a cap
  expect_true(.hk_p(.hk_mod(p, von_post_index = c(3L, 2L, NA)), "rostos_tozeges"))
  expect_true(.hk_p(.hk_mod(p, designation = c("Ha1", "Ha2", "Cr")), "kotus"))
  expect_true(.hk_p(.hk_mod(p, designation = c("He1", "He2", "Cr")), "vegyes_tozeges"))
  expect_true(.hk_p(.hk_mod(p, fiber_content_unrubbed_pct = c(12, 10, NA)), "kotus"))
  # in a Laptalaj every organic horizon is peat (Hugo's decision, 2026-10-30),
  # an O of the FAO notation too
  o <- .hk_mod(p, designation = c("O1", "O2", "Cr"), fiber_content_rubbed_pct = c(80, 70, NA))
  expect_true(.hk_p(o, "rostos_tozeges"))
  expect_true(.hk_p(o, "mely"))
  expect_false(.hk_p(o, "tozeges"))                    # outside them, an O is no peat
  expect_true(is.na(.hk_p(.hk_mod(p, fiber_content_unrubbed_pct = c(60, 60, NA)), "vegyes_tozeges")))
  expect_false(.hk_p(.hk_mod(p, fiber_content_unrubbed_pct = c(60, 60, NA)), "rostos_tozeges"))
  # Sphagnum: recorded when met
  expect_false(.hk_p(p, "mohatozeges"))
  expect_true(.hk_p(.hk_mod(p, sphagnum_fibres_pct = c(90, 60, NA)), "mohatozeges"))
  expect_false(.hk_p(.hk_mod(p, sphagnum_fibres_pct = c(90, 20, NA)), "mohatozeges"))
  # (30 x 90 + 50 x 20) / 80 is 46 %; with the H2 of unknown thickness, anywhere from 20 to 90 %
  expect_false(.hk_p(.hk_mod(p, sphagnum_fibres_pct = c(90, 20, NA)), "mohatozeges"))
  expect_true(is.na(.hk_p(.hk_mod(p, sphagnum_fibres_pct = c(90, 20, NA), bottom_cm = c(30, NA, 120)), "mohatozeges")))
  # drained
  expect_true(is.na(.hk_p(p, "lecsapolt")))
  expect_true(.hk_p(.hk_mod(p, site = list(artificially_drained = TRUE)), "lecsapolt"))
  expect_false(.hk_p(.hk_mod(p, site = list(artificially_drained = FALSE)), "lecsapolt"))
  expect_true(.hk_p(.hk_mod(p, site = list(land_use = "drained peatland, meadow")), "lecsapolt"))
})

test_that("mely and sekely read how deep the peat reaches", {
  skip_on_cran()
  p <- make_hscs2024_example("peat")
  expect_true(.hk_p(p, "mely"))
  expect_false(.hk_p(p, "sekely"))
  s <- .hk_mod(p, top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 120))
  expect_true(.hk_p(s, "sekely"))
  expect_false(.hk_p(s, "mely"))
  # a description that ends in the peat at 45 cm: it may go deeper
  e <- .hk_ped(list(designation = c("H1", "H2"), top_cm = c(0, 20), bottom_cm = c(20, 45), oc_pct = c(35, 30),
                    bulk_density_g_cm3 = 0.3))
  expect_true(is.na(.hk_p(e, "mely")))
  expect_true(is.na(.hk_p(e, "sekely")))
  # 45 cm of peat under 20 cm of mineral cover reach 65 cm: mely
  c20 <- .hk_ped(list(designation = c("A", "H1", "H2", "Cr"), top_cm = c(0, 20, 40, 65), bottom_cm = c(20, 40, 65, 120),
                      oc_pct = c(12, 35, 30, 1), bulk_density_g_cm3 = c(0.9, 0.3, 0.3, 1.4), clay_pct = c(30, NA, NA, 20)))
  expect_true(.hk_p(c20, "mely"))
  expect_false(.hk_p(c20, "sekely"))
  # no peat: neither
  expect_false(.hk_p(make_hscs2024_example("steppe"), "mely"))
  expect_false(.hk_p(make_hscs2024_example("steppe"), "sekely"))
})

test_that("the names of the Laptalajok carry the peat subtypes in table order", {
  skip_on_cran()
  p <- .hk_mod(make_hscs2024_example("peat"), fiber_content_rubbed_pct = c(80, 70, NA))
  r <- .hk_res(p)
  expect_identical(.hk_type(r), "LAP")
  expect_identical(r$qualifiers$subtypes, c("rostost\u0151zeges", "m\u00e9ly"))
  expect_identical(sub(" \\(.*$", "", r$name), "Rostost\u0151zeges, m\u00e9ly L\u00e1ptalaj")
  q <- .hk_mod(make_hscs2024_example("peat"), top_cm = c(0, 20, 45), bottom_cm = c(20, 45, 120),
               fiber_content_rubbed_pct = c(30, 20, NA), sphagnum_fibres_pct = c(90, 80, NA),
               site = list(artificially_drained = TRUE))
  r <- .hk_res(q)
  expect_identical(sub(" \\(.*$", "", r$name),
                   "Vegyes t\u0151zeges, mohat\u0151zeges, lecsapolt, sek\u00e9ly L\u00e1ptalaj")
  # the text lists szaraztozeges for the Laptalajok but defines it outside
  # them: the frame keeps it out (not_in)
  st <- r$qualifiers$status
  expect_false(st$passed[st$id == "szaraztozeges"])
})

test_that("each type names its humus, organic or peat property", {
  skip_on_cran()
  ex <- make_hscs2024_example
  cases <- list(
    ANT = list(.hk_mod(ex("anthropogenic"), oc_pct = c(10, 0.8, 0.2)), "l\u00e1pf\u00f6ldes"),
    KOV = list(.hk_mod(ex("rocky_skeletal"), oc_pct = c(0.4, NA)), "humuszszeg\u00e9ny"),
    KOZ = list(.hk_mod(ex("rock_influenced"), structure_type = c("granular", NA), structure_grade = c("strong", NA)),
               "igen er\u0151sen humuszos"),
    SZN = list(.hk_mod(ex("solonetz"), oc_pct = c(0.5, 1, 0.8, 0.4, 0.2)), "humuszszeg\u00e9ny"),
    SZK = list(.hk_mod(ex("solonchak"), oc_pct = c(0.3, 0.5, 0.3)), "humuszszeg\u00e9ny"),
    DUZ = list(.hk_mod(ex("swelling_clay"), bs_pct = c(80, 85, 90)), c("igen er\u0151sen humuszos", "fekete")),
    RET = list(.hk_mod(ex("meadow"), oc_pct = c(10, 1, 0.4), structure_type = c("granular", NA, NA),
                       bs_pct = c(80, NA, NA)), c("l\u00e1pf\u00f6ldes", "fekete")),
    KAR = list(.hk_mod(ex("carbonate"), munsell_value_moist = c(3, 6), structure_type = c("granular", "massive")),
               "humuszgazdag"),
    MEZ = list(ex("steppe"), c("m\u00e9ly humuszos r\u00e9teg\u0171", "igen er\u0151sen humuszos", "fekete")),
    AGY = list(.hk_mod(ex("clay_illuvial"), oc_pct = c(0.5, 0.4, 0.4, 0.2)), "humuszszeg\u00e9ny"),
    BAR = list(.hk_mod(ex("brown_earth"), bs_pct = c(30, 40, 50)), "umbrikus"),
    FOL = list(.hk_mod(ex("weakly_developed"), oc_pct = c(0.1, 0.05)), "nyers"))
  for (code in names(cases)) {
    r <- .hk_res(cases[[code]][[1]])
    expect_identical(.hk_type(r), code, info = code)
    expect_true(all(cases[[code]][[2]] %in% r$qualifiers$subtypes), info = paste(code, r$name))
    # the name puts the subtype properties before the type, the first capitalised
    expect_identical(r$name, soilKey:::.hscs_full_name(r$levels$name[r$levels$level == "types"][1],
                                                     r$qualifiers$subtypes, r$qualifiers$varieties), info = code)
    expect_match(r$name, cases[[code]][[2]][length(cases[[code]][[2]])], ignore.case = TRUE, info = code)
  }
  # MEZ: the order of its table (mely humuszos retegu before the humus row, fekete last)
  r <- .hk_res(ex("steppe"))
  s <- r$qualifiers$subtypes
  expect_true(which(s == "m\u00e9ly humuszos r\u00e9teg\u0171") < which(s == "igen er\u0151sen humuszos"))
  expect_identical(s[length(s)], "fekete")
  # HOR with a peat at the surface, HOM with a litter under forest
  hor <- .hk_ped(list(designation = c("H", "A", "C1", "C2"), top_cm = c(0, 15, 35, 60), bottom_cm = c(15, 35, 60, 120),
                      oc_pct = c(30, 1.2, 0.3, 0.5), clay_pct = c(NA, 18, 12, 15), silt_pct = c(NA, 62, 28, 70),
                      sand_pct = c(NA, 20, 60, 15), structure_grade = c(NA, "weak", "structureless", "structureless"),
                      structure_type = c(NA, "granular", "massive", "massive"), munsell_hue_moist = "10YR",
                      munsell_value_moist = c(2, 4, 5, 5), munsell_chroma_moist = c(1, 2, 3, 3)))
  r <- .hk_res(hor)
  expect_identical(.hk_type(r), "HOR")
  expect_true("t\u0151zeges" %in% r$qualifiers$subtypes)
  hom <- .hk_ped(list(designation = c("O", "A", "C1", "C2"), top_cm = c(0, 12, 37, 82), bottom_cm = c(12, 37, 82, 142),
                      oc_pct = c(35, 0.8, 0.2, 0.1), clay_pct = c(NA, 6, 3, 2), silt_pct = c(NA, 8, 4, 3),
                      sand_pct = c(NA, 86, 93, 95), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, 6, 6),
                      munsell_chroma_moist = c(1, 2, 4, 4)), list(vegetation = "Scots pine plantation"))
  r <- .hk_res(hom)
  expect_identical(.hk_type(r), "HOM")
  expect_true("sz\u00e1razt\u0151zeges" %in% r$qualifiers$subtypes)
})
