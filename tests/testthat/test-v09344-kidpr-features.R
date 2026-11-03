# v0.9.344 -- the genetic features of the Russian Field Guide of 2008 (3.2,
# pp. 59-74, and the subtype lists of chapter 4): one test per feature, data
# first, the notation where the data say nothing, then the defaults (a field
# observation not recorded is absent, a measurement undecided); the hosts and
# the proxies of Hugo's decision Q17.

# A profile of the formula (a "-" separated string or a vector of
# designations), 20 cm a layer unless the depths are given; Russian notation
# unless `ru = FALSE` (then FAO).
.ftp <- function(des, ..., site = list(), ru = TRUE, top = NULL, bot = NULL) {
  des <- if (length(des) == 1L) strsplit(des, "-", fixed = TRUE)[[1L]] else des
  n <- length(des)
  if (is.null(top)) { top <- seq(0, by = 20, length.out = n); bot <- top + 20 }
  h <- c(list(designation = des, top_cm = top, bottom_cm = bot), list(...))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  s <- utils::modifyList(list(id = "ft"), site)
  s$designation_system <- if (ru) "kidpr" else s$designation_system %||% "FAO"
  PedonRecord$new(site = s, horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
# One entry on a profile: its row of the dispatcher.
.ft1 <- function(p, f, host = NULL, place = NULL, division = NA_character_) {
  .kidpr_features(p, list(list(feature = f, host = host, place = place)), division = division)[1L, ]
}
.ftv <- function(p, f, ...) .ft1(p, f, ...)$passed


# --- the catalogue ------------------------------------------------------------------

test_that("a test for each of the 67 features; the aliases and host forms read (the 73 index forms)", {
  skip_on_cran()
  m <- .kidpr_ft_meta()
  f <- .kidpr_catalogue()$features
  expect_length(m, 67L)
  expect_setequal(names(m), vapply(f$features, function(e) e$code, ""))
  for (e in m) {
    expect_true(is.function(get0(paste0(".kidpr_ft_", e$fn), envir = asNamespace("soilKey"))), info = e$code)
    expect_true(e$route %in% c("data", "key", "notation"), info = e$code)
    expect_true(e$kind %in% c("observed", "measured"), info = e$code)
    expect_true(e$q17 %in% c("proxy", "notation_only", "defined", "key", "Q19", "soilkey"), info = e$code)
    expect_true(e$place %in% c("on", "in", "under", "over", "near", "surface", "profile"), info = e$code)
    if (e$place != "profile") {
      h <- .kidpr_ft_hosts(e$hosts, e$not_on)
      expect_true(identical(h, "any") || all(h %in% c(.kidpr_vocabulary()$horizons, "C", "D", "CLM", "TT")), info = e$code)
    }
  }
  q17 <- function(k) names(m)[vapply(m, function(e) identical(e$q17, k), NA)]
  # Q17: the notation-only features, the proxies and the defined ones
  expect_setequal(q17("notation_only"), c("hi", "pl", "crm", "f", "el", "ic", "agr", "ad", "pr", "ff", "fn", "ocr", "yu"))
  expect_setequal(c(q17("proxy"), "ur"),
                  c("g", "q", "v", "s", "cs", "nn", "i", "sn", "BF", "BH", "an", "h", "lc", "mc", "nc", "dc", "ml",
                    "\u22a5", "@", "tr", "y", "[hh]", "ro", "r", "rh", "ur", "rr"))
  expect_setequal(q17("defined"), c("x", "rx", "ur"))
  expect_true(all(vapply(m[q17("notation_only")], function(e) e$route != "data", NA)))
  # the aliases of the book and the host forms of the lists: 67 + tu, ko, ag,
  # as + Di, Del + the other host forms
  p <- make_kidpr2008_example("CA")
  al <- c("tu", "ko", "ag", "as", "ael", "hh", "akl")
  hf <- vapply(f$host_forms, function(e) as.character(e$written), "")
  r <- .kidpr_features(p, c(al, hf))
  expect_identical(nrow(r), length(al) + length(hf))
  expect_identical(r$code[1:7], c("tr", "ek", "agr", "cs", "ae", "[hh]", "[akl]"))
  expect_identical(r$code[r$feature == "AUlc-BCAmc"], "lc+mc")
  expect_identical(r$host[r$feature == "ELf"], "EL")
  expect_identical(r$place[r$feature == "AUel"], "in")
  expect_error(.kidpr_features(p, "zz"), "no feature")
})


# --- the notation -------------------------------------------------------------------

test_that("every feature reads from the Russian notation where the data say nothing", {
  skip_on_cran()
  # the feature written, and the same profile without it
  tab <- list(
    ao = list("O-ao-BHF-C", "O-BHF-C"), mr = list("TEmr-TT", "TE-TT"), te = list("TOte-TT", "TO-TT"),
    h = list("Th-TT", "T-TT"), e = list("O-e-BHF-C", "O-BHF-C"), el = list("AUel-BCA-C", "AU-BCA-C"),
    BH = list("O-BH-C", "O-BHF-C"), BF = list("O-BF-C", "O-BHF-C"), f = list("O-ELf-BT-C", "O-EL-BT-C"),
    "[e-hf]" = list(c("AY", "EL", "[e-hf]", "EL", "BT", "C"), "AY-EL-BT-C"),
    "[akl]" = list("AJ[akl]-BCA-C", "AJ-BCA-C"), i = list("AY-BMi-C", "AY-BM-C"), m = list("AY-Cm", "AY-C"),
    crm = list("O-crm-BT-C", "O-BT-C"), an = list("O-BHFan-C", "O-BHF-C"), pl = list("O-CRpl-C", "O-CR-C"),
    sn = list("AU-BMsn-C", "AU-BM-C"), v = list("AU-BMv-C", "AU-BM-C"), g = list("AY-BTg-C", "AY-BT-C"),
    q = list("AU-BCAq-C", "AU-BCA-C"), ml = list("AU-BCAml-C", "AU-BCA-C"), s = list("AU-BCAs-C", "AU-BCA-C"),
    tk = list("tk-AJ-BCA-C", "AJ-BCA-C"), ff = list("O-BHFff-C", "O-BHF-C"), fn = list("O-BHFfn-C", "O-BHF-C"),
    nn = list("AY-ELnn-BT-C", "AY-EL-BT-C"), cf = list("O-cf-G-CG", "O-G-CG"), ox = list("O-Gox-CG", "O-G-CG"),
    ek = list("AY-EL-ko-D", "AY-EL-D"), gr = list("AY-BFMgr-C", "AY-BFM-C"), hi = list("O-BHFhi-C", "O-BHF-C"),
    mc = list("AU-BCAmc-C", "AU-BCA-C"), lc = list("AUlc-BCA-C", "AU-BCA-C"), nc = list("AU-BCAnc-C", "AU-BCA-C"),
    dc = list("AU-BCAdc-C", "AU-BCA-C"), ic = list("AU-BMic-C", "AU-BM-C"), cs = list("AU-BCAcs-C", "AU-BCA-C"),
    tr = list("AYtr-EL-BT-C", "AY-EL-BT-C"), "@" = list("O-CR@-C", "O-CR-C"), ocr = list("T-CRocr-C", "T-CR-C"),
    agr = list("Pagr-BT-C", "P-BT-C"), pb = list("Ppb-BT-C", "P-BT-C"), "[hh]" = list("AY-EL-BT[hh]-C", "AY-EL-BT-C"),
    ct = list("AY-EL-BELct-BT-C", "AY-EL-BEL-BT-C"), y = list("AY-EL-BTy-C", "AY-EL-BT-C"), yu = list("AU-BMyu-C", "AU-BM-C"),
    d = list("d-AY-BM-C", "AY-BM-C"), ad = list("Pad-BT-C", "P-BT-C"), pa = list("AYpa-EL-BT-C", "AY-EL-BT-C"),
    pr = list("P-BTpr-C", "P-BT-C"), md = list("md-T-TT", "T-TT"), pir = list("pir-T-TT", "T-TT"),
    ca = list("AY-BMca-C", "AY-BM-C"), u = list("AY-BMu-C", "AY-BM-C"), ro = list("AY-BMro-C", "AY-BM-C"),
    "\u22a5" = list("O-CR-C\u22a5", "O-CR-C"), r = list("r-AY-BM-C", "AY-BM-C"), rh = list("rh-AY-BM-C", "AY-BM-C"),
    aq = list("RUaq-RU-C", "RU-RU-C"), ae = list("RUae-RU-C", "RU-RU-C"), ur = list("ur-AY-BM-C", "AY-BM-C"),
    rr = list("rr-AY-BM-C", "AY-BM-C"), "TT~~" = list("T-TT~~", "T-TT"), "TT\"" = list("T-TT\"", "T-TT"),
    w = list("Pw-BT-C", "P-BT-C"), x = list("AY+X-EL-BT-C", "AY-EL-BT-C"), rx = list("rx-AY-BM-C", "AY-BM-C"))
  # every feature has its pair
  expect_setequal(names(tab), names(.kidpr_ft_meta()))
  for (f in names(tab)) {
    yes <- .ftp(tab[[f]][[1L]]); no <- .ftp(tab[[f]][[2L]])
    expect_true(all(.kidpr_designations(yes$horizons$designation, yes$site)$known), info = f)
    a <- .ft1(yes, f); b <- .ft1(no, f)
    expect_true(isTRUE(a$passed), info = paste(f, "written"))
    expect_identical(a$route, "notation", info = paste(f, "written"))
    expect_false(isTRUE(b$passed), info = paste(f, "not written"))
    expect_false(is.na(b$passed), info = paste(f, "not written"))
  }
  # the written form of an alias is the feature (ko = ek)
  expect_true(.ftv(.ftp("AY-EL-ko-D"), "ek"))
})


# --- the data -------------------------------------------------------------------------

test_that("each feature with a proxy passes and fails on the recorded data", {
  skip_on_cran()
  # (feature, profile where it passes, profile where it fails): the hosts
  # from the notation, the verdict from the data
  cases <- list(
    g = list(.ftp("AY-BT-C", mottle_colors = c(NA, "7.5YR 5/8", NA)), .ftp("AY-BT-C", redoximorphic_features_pct = 0)),
    q = list(.ftp("AU-BCA-C", mottle_colors = c(NA, "5Y 5/3", NA)), .ftp("AU-BCA-C", mottle_colors = c(NA, "none", NA))),
    v = list(.ftp("AU-BM-C", slickensides = c(NA, "few", NA)), .ftp("AU-BM-C", slickensides = c(NA, "absent", NA))),
    s = list(.ftp("AU-BCA-C", soluble_salts_pct = c(0.05, 0.3, 0.2)), .ftp("AU-BCA-C", soluble_salts_pct = c(0.05, 0.05, 0.02))),
    cs = list(.ftp("AU-BCA-C", gypsum_visible_pct = c(0, 2, 0)), .ftp("AU-BCA-C", gypsum_visible_pct = 0)),
    nn = list(.ftp("AY-EL-BT-C", ferric_nodules_pct = c(0, 12, 0, 0)), .ftp("AY-EL-BT-C", ferric_nodules_pct = c(0, 3, 0, 0))),
    i = list(.ftp("AY-BM-C", clay_films_amount = c(NA, "few", NA)), .ftp("AY-BM-C", clay_films_amount = c(NA, "none", NA))),
    sn = list(.ftp("AU-BM-C", structure_type = c("granular", "columnar", NA)),
              .ftp("AU-BM-C", structure_type = c("granular", "subangular blocky", NA))),
    BH = list(.ftp("O-BHF-C", oc_pct = c(30, 2.5, 0.2)), .ftp("O-BHF-C", oc_pct = c(30, 1.2, 0.2))),
    BF = list(.ftp("O-BHF-C", oc_pct = c(30, 1.2, 0.2)), .ftp("O-BHF-C", oc_pct = c(30, 2.5, 0.2))),
    an = list(.ftp("O-BHF-C", fe_ox_pct = c(NA, 1.5, NA), al_ox_pct = c(NA, 2, NA)),
              .ftp("O-BHF-C", fe_ox_pct = c(NA, 0.5, NA), al_ox_pct = c(NA, 2, NA))),
    h = list(.ftp("T-TT", fiber_content_rubbed_pct = c(10, 50)), .ftp("T-TT", fiber_content_rubbed_pct = c(50, 50))),
    mc = list(.ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "pseudomycelia", NA)),
              .ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "soft concretions", NA))),
    nc = list(.ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "soft concretions", NA)),
              .ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "pseudomycelia", NA))),
    dc = list(.ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "powdery", NA)),
              .ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "powdery; soft concretions", NA))),
    lc = list(.ftp("AU-BCA-C", secondary_carbonates_form = c("coatings", NA, NA)),
              .ftp("AU-BCA-C", secondary_carbonates_form = c("none", NA, NA))),
    ml = list(.ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "hard concretions", NA)),
              .ftp("AU-BCA-C", secondary_carbonates_form = c(NA, "pseudomycelia", NA))),
    "@" = list(.ftp("O-CR-C", cryoturbation = c(NA, "present", NA)), .ftp("O-CR-C", cryoturbation = "none")),
    y = list(.ftp("AY-EL-BT-C", boundary_topography = c(NA, "tonguing", NA, NA)),
             .ftp("AY-EL-BT-C", boundary_topography = c(NA, "wavy", NA, NA))),
    "[hh]" = list(.ftp("AY-EL-BT-C", munsell_value_moist = c(3, 6, 4, 5), oc_pct = c(2, 0.4, 0.9, 0.2)),
                  .ftp("AY-EL-BT-C", munsell_value_moist = c(3, 6, 5, 5), oc_pct = c(2, 0.4, 0.2, 0.2))),
    ro = list(.ftp("AY-BM-C", munsell_hue_moist = c("2.5YR", "2.5YR", "5YR")),
              .ftp("AY-BM-C", munsell_hue_moist = c("2.5YR", "10YR", "5YR"))),
    "\u22a5" = list(.ftp("O-CR-C", permafrost_temp_C = c(NA, NA, -1)),
                    .ftp("O-CR-C", top = c(0, 10, 120), bot = c(10, 120, 150), permafrost_temp_C = c(NA, NA, -1))),
    te = list(.ftp("TO-TT", top = c(0, 30), bot = c(30, 100), peat_moor_type = c("high-moor", "low-moor")),
              .ftp("TO-TT", top = c(0, 30), bot = c(30, 100), peat_moor_type = "high-moor")),
    m = list(.ftp("AY-C", structure_grade = c(NA, "weak"), structure_type = c(NA, "crumb"), clay_pct = c(20, 25)),
             .ftp("AY-C", structure_type = c(NA, "massive"))),
    tk = list(.ftp("AJ-BCA-C", surface_crust_type = c("clay", NA, NA), polygonal_cracks_spacing_cm = c(20, NA, NA)),
              .ftp("AJ-BCA-C", surface_crust_type = c("biological", NA, NA))),
    ox = list(.ftp("O-G-CG", redoximorphic_features_pct = c(NA, 60, NA)), .ftp("O-G-CG", redoximorphic_features_pct = c(NA, 20, NA))),
    gr = list(.ftp("AY-BFM-C", structure_type = c(NA, "granular", NA)), .ftp("AY-BFM-C", structure_type = c(NA, "subangular blocky", NA))),
    pb = list(.ftp("P-BT-C", oc_pct = c(1.2, 0.4, 0.2), foreign_fragments_pct = c(10, 0, 0)),
              .ftp("P-BT-C", oc_pct = c(0.6, 0.4, 0.2), foreign_fragments_pct = c(10, 0, 0))),
    pa = list(.ftp("P-BT-C", site = list(land_use = "abandoned arable land")), .ftp("P-BT-C", site = list(land_use = "arable"))),
    d = list(.ftp("AY-BM-C", structure_type = c("platy", NA, NA)), .ftp("AY-BM-C", structure_type = c("granular", NA, NA))),
    md = list(.ftp("H-T-TT", fiber_content_rubbed_pct = c(10, 50, 50)), .ftp("H-T-TT", fiber_content_rubbed_pct = 50)),
    pir = list(.ftp("AY-T-TT", top = c(0, 5, 30), bot = c(5, 30, 100), layer_origin = c("pyrogenic", NA, NA)),
               .ftp("AY-T-TT", top = c(0, 5, 30), bot = c(5, 30, 100), layer_origin = c("aeolic", NA, NA))),
    aq = list(.ftp("RU-RU-C", layer_origin = c("alluvium", "alluvium", NA)), .ftp("RU-RU-C", layer_origin = c("aeolic", "aeolic", NA))),
    ae = list(.ftp("RU-RU-C", layer_origin = c("aeolic", "aeolic", NA)), .ftp("RU-RU-C", layer_origin = c("alluvium", "alluvium", NA))),
    "TT~~" = list(.ftp("T-C-TT", layer_origin = c(NA, "alluvium", NA)), .ftp("T-C-TT", layer_origin = c(NA, "aeolic", NA))),
    "TT\"" = list(.ftp("T-C-TT", layer_origin = c(NA, "volcanic ash", NA)), .ftp("T-C-TT", layer_origin = c(NA, "alluvium", NA))))
  for (f in names(cases)) {
    a <- .ft1(cases[[f]][[1L]], f); b <- .ft1(cases[[f]][[2L]], f)
    expect_true(isTRUE(a$passed), info = paste(f, "passes"))
    expect_true(a$route %in% c("data", "both"), info = paste(f, "passes"))
    expect_true(isFALSE(b$passed), info = paste(f, "fails"))
    # sn is one-sided: a structure that is not columnar only fails to
    # confirm it, and the notation decides
    expect_true(b$route %in% c("data", "both", if (f == "sn") "notation"), info = paste(f, "fails"))
  }
})

test_that("the features the key to horizons ends in are read on the data with no Russian notation", {
  skip_on_cran()
  sand <- list(clay_pct = 3, sand_pct = 90, silt_pct = 7)
  # ao: a coarse-humus layer of 6 cm under O (key 47a)
  p <- .ftp(c("Oe", "A", "Bs", "C"), ru = FALSE, top = c(0, 6, 12, 50), bot = c(6, 12, 50, 100), oc_pct = c(35, 10, 1, 0.2),
            fiber_content_rubbed_pct = c(50, NA, NA, NA), plant_residues_pct = c(80, 20, 0, 0), clay_pct = c(NA, 5, 5, 4),
            sand_pct = c(NA, 85, 85, 90), silt_pct = c(NA, 10, 10, 6), munsell_hue_dry = "10YR",
            munsell_value_dry = c(3, 5, 5, 6), munsell_chroma_dry = c(2, 2, 6, 4))
  r <- .ft1(p, "ao")
  expect_true(r$passed); expect_identical(r$route, "data"); expect_match(r$evidence, "47")
  expect_false(.ftv(.ftp(c("Oe", "Bs", "C"), ru = FALSE, oc_pct = c(35, 1, 0.2)), "ao"))
  # e: a whitish band of 1.5 cm (key 50a)
  pe <- .ftp(c("Oe", "E", "Bs", "C"), ru = FALSE, top = c(0, 5, 6.5, 40), bot = c(5, 6.5, 40, 100), oc_pct = c(35, 0.3, 1, 0.2),
             ph_h2o = 4.5, fiber_content_rubbed_pct = c(50, NA, NA, NA), clay_pct = c(NA, 3, 3, 3), sand_pct = c(NA, 90, 90, 92),
             silt_pct = c(NA, 7, 7, 5), munsell_hue_dry = "10YR", munsell_value_dry = c(3, 7, 5, 6), munsell_chroma_dry = c(2, 1, 6, 4),
             structure_type = c(NA, "single grain", "subangular blocky", "single grain"))
  expect_true(.ftv(pe, "e"))
  # e: bleached spots in a humus horizon (leached_spots)
  expect_true(.ftv(.ftp("AY-BM-C", leached_spots = c(TRUE, NA, NA)), "e"))
  expect_false(.ftv(.ftp("AY-BM-C", leached_spots = c(FALSE, NA, NA)), "e"))
  # tr: a mixed layer at the surface with its base above 40 cm (key 7b)
  pt <- .ftp(c("Ah", "Bw", "C"), ru = FALSE, top = c(0, 25, 60), bot = c(25, 60, 100), foreign_fragments_pct = c(85, 0, 0),
             oc_pct = c(2, 0.6, 0.2))
  expect_true(.ftv(pt, "tr"))
  expect_false(.ftv(.ftp(c("Ah", "Bw", "C"), ru = FALSE, foreign_fragments_pct = 0), "tr"))
  # ca: effervescence with no carbonate neoformations
  pc <- .ftp(c("Ah", "Bw", "C"), ru = FALSE, effervescence_extent = c("none", "none", "local"),
             secondary_carbonates_form = c(NA, NA, "none"), oc_pct = c(2, 0.6, 0.2), ph_h2o = c(6, 6.5, 7.8))
  expect_true(.ftv(pc, "ca"))
  expect_false(.ftv(.ftp(c("Ah", "Bw", "C"), ru = FALSE, effervescence_extent = "none"), "ca"))
  # u: dark throughout from a dark rock (key 13)
  pu <- .ftp(c("Ah", "Bw", "C"), ru = FALSE, munsell_hue_dry = "10YR", munsell_value_dry = c(2, 3, 3), munsell_chroma_dry = 1,
             site = list(parent_material = "black shale"))
  expect_true(.ftv(pu, "u"))
  pu$site$parent_material <- "loess"
  expect_false(.ftv(pu, "u"))
  # r and rh: the deposit of 20 cm over a buried humus horizon (key 38a, 35a)
  dep <- function(hue, chroma, ...) .ftp(c("C", "Ahb", "Bw", "C"), ru = FALSE, top = c(0, 20, 40, 80), bot = c(20, 40, 80, 120),
                                         oc_pct = c(0.3, 2.5, 0.6, 0.2), clay_pct = 20, silt_pct = 40, sand_pct = 40,
                                         munsell_hue_dry = c(hue, "10YR", "10YR", "10YR"), munsell_value_dry = c(6, 4, 5, 6),
                                         munsell_chroma_dry = c(chroma, 2, 4, 4), ...)
  expect_true(.ftv(dep("7.5YR", 4), "r")); expect_false(.ftv(dep("7.5YR", 4), "rh"))
  expect_true(.ftv(dep("10YR", 2), "rh")); expect_false(.ftv(dep("10YR", 2), "r"))
  # g on the data alone, on a layer the key leaves among mineral horizons
  pg <- .ftp(c("Ah", "E", "Bt", "C"), ru = FALSE, oc_pct = c(2.5, 0.5, 0.4, 0.1), clay_pct = c(15, 10, 30, 25), silt_pct = 50,
             sand_pct = c(35, 40, 20, 25), munsell_hue_dry = "10YR", munsell_value_dry = c(4, 7, 5, 6),
             munsell_chroma_dry = c(2, 1, 4, 4), mottle_colors = c(NA, NA, "7.5YR 5/8", NA), ph_h2o = 5.5)
  r <- .ft1(pg, "g")
  expect_true(r$passed); expect_identical(r$route, "data"); expect_identical(unlist(r$layers), 3L)
})

test_that("the data come first; the notation decides only where they say nothing", {
  skip_on_cran()
  # g written on BTg, but the redox features recorded at 0 %: the data decide
  r <- .ft1(.ftp("AY-BTg-C", redoximorphic_features_pct = 0), "g")
  expect_false(r$passed); expect_identical(r$route, "data")
  expect_match(r$evidence, "the data decide", fixed = TRUE)
  # mottles recorded on a BT the notation does not mark: g
  r <- .ft1(.ftp("AY-BT-C", mottle_colors = c(NA, "7.5YR 5/8", NA)), "g")
  expect_true(r$passed); expect_identical(r$route, "data")
  # data and notation agree: both
  r <- .ft1(.ftp("AU-BCAmc-C", secondary_carbonates_form = c(NA, "pseudomycelia", NA)), "mc")
  expect_true(r$passed); expect_identical(r$route, "both")
  # g written, mottles not recorded: the notation
  r <- .ft1(.ftp("AY-BTg-C"), "g")
  expect_true(r$passed); expect_identical(r$route, "notation")
  # an FAO letter can add a feature (Bg suggests g), never take it away
  r <- .ft1(.ftp(c("Ah", "Bg", "C"), ru = FALSE), "g", host = "any")
  expect_true(r$passed); expect_identical(r$route, "notation"); expect_match(r$evidence, "another notation")
})

test_that("field observations not recorded are absent, measurements undecided", {
  skip_on_cran()
  p <- .ftp(c("Ah", "Bw", "C"), ru = FALSE)
  obs <- c("g", "q", "v", "cs", "sn", "@", "tr", "i", "ml", "d", "tk", "e", "el", "x", "rx", "ur", "rr")
  r <- .kidpr_features(p, obs)
  for (k in seq_len(nrow(r))) {
    expect_false(r$passed[k], info = r$code[k])
    expect_identical(r$route[k], "data", info = r$code[k])
  }
  expect_match(r$evidence[r$code == "g"], "absent (a field observation)", fixed = TRUE)
  # measured: salts, carbon, permafrost, the colours of ro, the second
  # humus horizon
  meas <- .kidpr_features(p, list(list(feature = "s"), list(feature = "\u22a5"), list(feature = "ro"),
                                  list(feature = "BH", host = "any"), list(feature = "[hh]", host = "any")))
  expect_true(all(is.na(meas$passed)))
  expect_true("soluble_salts_pct" %in% unlist(meas$missing[1L]))
  expect_true(all(c("permafrost_temp_C", "ice_pct") %in% unlist(meas$missing[2L])))
  # the features of the notation only, in a profile with no Russian notation:
  # absent (nothing records them)
  r <- .kidpr_features(p, list(list(feature = "hi", host = "any", place = "on"), list(feature = "yu"), list(feature = "ic", host = "any")))
  expect_true(all(r$passed %in% FALSE))
  expect_match(r$evidence[1L], "notation only", fixed = TRUE)
  # in a profile written in the Russian notation, a measured feature not
  # written is absent: the notation says it
  r <- .ft1(.ftp("AU-BCA-C"), "s")
  expect_false(r$passed); expect_identical(r$route, "notation")
  # with no salts measured and no notation, undecided
  expect_true(is.na(.ftv(.ftp(c("Ah", "Bk", "C"), ru = FALSE), "s")))
})


# --- hosts and places ----------------------------------------------------------------

test_that("a feature holds only on its hosts", {
  skip_on_cran()
  # g is not read on G (the gley horizon is the feature in full)
  r <- .ft1(.ftp("O-G-CG", mottle_colors = c(NA, "7.5YR 5/8", NA)), "g")
  expect_false(r$passed); expect_false(2L %in% unlist(r$layers))
  # an only on BHF; on BM no host, unless the entry names it
  p <- .ftp("AY-BM-C", fe_ox_pct = c(NA, 1.5, NA), al_ox_pct = c(NA, 2, NA))
  r <- .ft1(p, "an")
  expect_false(r$passed); expect_match(r$evidence, "no host horizon")
  expect_true(.ftv(p, "an", host = "BM"))
  # mc on BCA only: pseudomycelium in the AU is lc, not mc
  p <- .ftp("AU-BCA-C", secondary_carbonates_form = c("pseudomycelia", "powdery", NA))
  expect_false(.ftv(p, "mc")); expect_true(.ftv(p, "lc")); expect_true(.ftv(p, "dc"))
  # by analogy (Q17): pl in CRM, cf at the edge of CR
  expect_true(.ftv(.ftp("O-CRMpl-C"), "pl"))
  expect_true(.ftv(.ftp("O-cf-CR-C"), "cf"))
  expect_false(.ftv(.ftp("O-BMpl-C"), "pl"))
  # the host forms: f on EL (ELf) is no f on C
  expect_true(.ftv(.ftp("AY-ELf-BT-C"), "ELf"))
  expect_false(.ftv(.ftp("AY-EL-BT-Cf"), "ELf"))
  expect_true(.ftv(.ftp("AY-EL-BT-Cf"), "f"))
  # Th: h in T, not in the AU
  expect_true(.ftv(.ftp("Th-G-CG"), "Th"))
  expect_false(.ftv(.ftp("T-AUh-G-CG"), "Th"))
  expect_true(.ftv(.ftp("T-AUh-G-CG"), "AUh"))
  # w: the W written over the agro horizon (the host is the horizon under it)
  expect_true(.ftv(.ftp("W-P-BT-C"), "Pw"))
  expect_false(.ftv(.ftp("W-BT-C"), "Pw"))
  # ao: the band under O, not over it
  expect_false(.ftv(.ftp("ao-O-BHF-C"), "ao"))
  # a host the data leave undecided leaves a passing feature undecided
  pe <- .ftp(c("Ah", "Bw", "C"), ru = FALSE, structure_type = c("granular", "granular", "massive"))
  r <- .ft1(pe, "gr")
  expect_true(is.na(r$passed)); expect_match(r$evidence, "horizon of the layer undecided")
  # no host at all: FALSE (the feature is self-restricting)
  r <- .ft1(.ftp("AY-BM-C"), "agr")
  expect_false(r$passed); expect_match(r$evidence, "no host horizon")
})

test_that("the double el of CI and CM is read by its place: in the AU, or under it", {
  skip_on_cran()
  inn <- list(feature = "el", host = "AU", place = "in")
  und <- list(feature = "el", host = "AU", place = "under")
  ent <- list(inn, und)
  # el written on the AU (AUel): solodised (in), not eluviated (under)
  r <- .kidpr_features(.ftp("AUel-BCA-C"), ent)
  expect_identical(r$passed, c(TRUE, FALSE))
  # el written on the horizon under the AU: eluviated
  r <- .kidpr_features(.ftp("AU-BCAel-C"), ent)
  expect_identical(r$passed, c(FALSE, TRUE))
  # a small el inside the AU, or directly under it
  r <- .kidpr_features(.ftp("AU-el-AU-BCA-C"), ent)
  expect_identical(r$passed, c(TRUE, FALSE))
  r <- .kidpr_features(.ftp("AU-el-BCA-C"), ent)
  expect_identical(r$passed, c(FALSE, TRUE))
  # the host form AUel reads el in the AU, Gel el on the G
  expect_true(.ftv(.ftp("AUel-G-CG"), "AUel")); expect_false(.ftv(.ftp("AU-Gel-CG"), "AUel"))
  expect_true(.ftv(.ftp("AU-Gel-CG"), "Gel"))
  # on the data: the band of key 52a directly under the AU is eluviated
  p <- .ftp(c("Ah", "E", "Bt", "C"), ru = FALSE, top = c(0, 15, 19, 60), bot = c(15, 19, 60, 100), oc_pct = c(3, 0.3, 0.5, 0.2),
            ph_h2o = 5.5, clay_pct = c(15, 10, 30, 25), sand_pct = c(35, 40, 20, 25), silt_pct = 50, munsell_hue_dry = "10YR",
            munsell_value_dry = c(4, 7, 5, 6), munsell_chroma_dry = c(2, 1, 4, 4),
            structure_type = c("granular", "platy", "subangular blocky", "massive"))
  r <- .kidpr_features(p, ent)
  expect_identical(r$passed, c(FALSE, TRUE))
  expect_identical(r$route[2L], "data")
})

test_that("paired subtypes need both parts; the division sets pb and x", {
  skip_on_cran()
  expect_true(.ftv(.ftp("AUlc-BCAmc-C"), "AUlc-BCAmc"))
  expect_false(.ftv(.ftp("AUlc-BCAnc-C"), "AUlc-BCAmc"))
  expect_true(.ftv(.ftp("AUlc-BCAnc-C"), "AUlc-BCAnc"))
  r <- .ft1(.ftp("AU-BCA-C", secondary_carbonates_form = c("coatings", "pseudomycelia", NA)), "AUlc-BCAmc")
  expect_true(r$passed); expect_identical(r$code, "lc+mc")
  # pb: in the agro horizon; in CJ the deflated natural soil (Q19)
  p <- .ftp("AJ-BCA-C", site = list(wind_deflation = TRUE))
  expect_false(.ftv(p, "pb"))
  expect_true(.ftv(p, "pb", division = "CJ"))
  expect_false(.ftv(.ftp("AJ-BCA-C"), "pb", division = "CJ"))
  # erosion recorded stands for the fragments in the agro horizon
  r <- .ft1(.ftp("P-BT-C", oc_pct = c(1.2, 0.4, 0.2), site = list(anthropic_truncation = TRUE)), "pb")
  expect_true(r$passed)
})


# --- x, rx, ur, rr ----------------------------------------------------------------------

test_that("x is an X horizon in a soil that is no chemozem; rx, ur and rr the deposited layer", {
  skip_on_cran()
  p <- .ftp(c("Ah", "Bw", "C"), ru = FALSE, oc_pct = c(2, 0.5, 0.2), contamination_hazard = c("extremely hazardous", NA, NA))
  r <- .ft1(p, "x")
  expect_true(r$passed); expect_identical(unlist(r$layers), 1L)
  expect_false(.ftv(p, "x", division = "CU"))
  p$horizons$contamination_hazard <- c("moderately hazardous", NA, NA)
  expect_false(.ftv(p, "x"))
  # contamination with no category: undecided; nothing recorded: no X (Q23)
  r <- .ft1(.ftp(c("Ah", "Bw", "C"), ru = FALSE, contamination_type = c("heavy_metals", NA, NA)), "x")
  expect_true(is.na(r$passed)); expect_true("contamination_hazard" %in% unlist(r$missing))
  expect_false(.ftv(.ftp(c("Ah", "Bw", "C"), ru = FALSE), "x"))
  # an X below 30 cm is no X
  expect_false(.ftv(.ftp(c("Ah", "Bw", "C"), ru = FALSE, contamination_hazard = c(NA, NA, "extremely hazardous")), "x"))
  # the deposited layer: 20 cm over a buried humus horizon
  dep <- function(...) .ftp(c("C", "Ahb", "Bw", "C"), ru = FALSE, top = c(0, 20, 40, 80), bot = c(20, 40, 80, 120),
                            oc_pct = c(0.3, 2.5, 0.6, 0.2), clay_pct = 20, silt_pct = 40, sand_pct = 40, ...)
  v <- function(p) .kidpr_features(p, c("ur", "rr", "rx"))$passed
  # 30 % artefacts: urbi-stratified
  expect_identical(v(dep(artefacts_pct = c(30, 0, 0, 0))), c(TRUE, FALSE, FALSE))
  # 70 % artefacts, not toxic: arti-stratified
  expect_identical(v(dep(artefacts_pct = c(70, 0, 0, 0))), c(FALSE, TRUE, FALSE))
  # toxic (a hazard above the permissible category): toxi-stratified, and not
  # arti-stratified
  expect_identical(v(dep(artefacts_pct = c(70, 0, 0, 0), contamination_hazard = c("highly hazardous", NA, NA, NA))),
                   c(FALSE, FALSE, TRUE))
  expect_identical(v(dep(artefacts_pct = c(10, 0, 0, 0), contamination_hazard = c("permissible", NA, NA, NA))),
                   c(FALSE, FALSE, FALSE))
  # contamination recorded with no category: rx undecided
  r <- .kidpr_features(dep(contamination_type = c("hydrocarbons", NA, NA, NA)), "rx")
  expect_true(is.na(r$passed))
  # a layer thicker than 40 cm is no deposited layer of these subtypes
  thick <- .ftp(c("C", "Ahb", "C"), ru = FALSE, top = c(0, 50, 70), bot = c(50, 70, 120), oc_pct = c(0.3, 2.5, 0.2),
                clay_pct = 20, silt_pct = 40, sand_pct = 40, artefacts_pct = c(30, 0, 0),
                contamination_hazard = c("highly hazardous", NA, NA))
  expect_identical(.kidpr_features(thick, c("ur", "rx"))$passed, c(FALSE, FALSE))
  # with no buried horizon there is no deposited layer
  expect_identical(.kidpr_features(.ftp(c("C", "Bw", "C"), ru = FALSE, artefacts_pct = c(30, 0, 0)), c("ur", "rr", "rx"))$passed,
                   c(FALSE, FALSE, FALSE))
})


# --- the review of 0.9.344 ---------------------------------------------------------

test_that("the fixes of the review: y, ca, v, pa, ro, x, g and the one-sided proxies", {
  skip_on_cran()
  # D1 y: tongues only from an eluvial horizon
  expect_false(.ftv(.ftp("AU-BT-C", boundary_topography = c("tonguing", NA, NA)), "y"))
  expect_true(.ftv(.ftp("AU-BTy-C", boundary_topography = c("tonguing", NA, NA)), "y"))
  # D2 ca: a recorded form of secondary carbonates is a neoformation; an FAO
  # k is pedogenic
  ca <- function(form) .ftv(.ftp(c("Ah", "Bw", "C"), ru = FALSE, effervescence_class = c("none", "none", "strong"),
                                 secondary_carbonates_form = c(NA, NA, form), oc_pct = c(2, 0.5, 0.2)), "ca")
  expect_false(ca("hard concretions")); expect_false(ca("coatings")); expect_true(ca("none"))
  expect_false(.ftv(.ftp(c("Ah", "Bw", "Ck"), ru = FALSE), "ca"))
  # D3 v: a structure recorded without slickensides leaves it to the notation
  expect_true(.ftv(.ftp("AU-BMv-C", structure_type = c(NA, "angular blocky", NA)), "v"))
  # D4 pa: a written pa in a soil now under forest stands; tillage now rules it out
  expect_true(.ftv(.ftp("AYpa-EL-BT-C", site = list(land_use = "forest")), "pa"))
  expect_false(.ftv(.ftp("PUpa-BCA-C", site = list(land_use = "arable")), "pa"))
  # D5 ro: the organic layers by their horizon; gley and neutral hues not red
  expect_true(.ftv(.ftp("O-BM-C", munsell_hue_moist = c("10YR", "2.5YR", "2.5YR")), "ro"))
  expect_false(.ftv(.ftp(c("Ah", "Bw", "Cg"), ru = FALSE, munsell_hue_moist = c("5YR", "2.5YR", "5GY")), "ro"))
  expect_true(.ftv(.ftp("AY-BM-C", munsell_hue_moist = c("10RP", "5R", "2.5YR")), "ro"))
  # D6 x: a hazard recorded within 30 cm decides; the X written within 30 cm
  expect_false(.ftv(.ftp("AY+X-EL-BT-C", contamination_hazard = c("permissible", NA, NA, NA)), "x"))
  expect_false(.ftv(.ftp("AY-EL-BT+X-C"), "x"))
  # D7 g: mottles recorded as none are no redox sign
  expect_false(.ftv(.ftp("AY-BT-C", mottle_colors = c(NA, "none", NA)), "g"))
  # R1 sn and q: failing to confirm leaves the notation to decide
  expect_true(.ftv(.ftp("AU-BMsn-C", structure_type = c("granular", "prismatic", NA)), "sn"))
  expect_true(.ftv(.ftp("AU-BCAq-C", mottle_colors = c(NA, "10YR 6/2", NA)), "q"))
  # R2 m: grade not recorded, a weak no; texture not recorded, undecided
  expect_true(.ftv(.ftp("AY-Cm", structure_type = c(NA, "crumb")), "m"))
  expect_true(is.na(.ftv(.ftp(c("AY", "C"), ru = FALSE, structure_grade = c(NA, "weak"), structure_type = c(NA, "crumb")), "m")))
  # R3 d: the first mineral layer under an organic one
  expect_true(.ftv(.ftp(c("Oe", "Ah", "Bw", "C"), ru = FALSE, top = c(0, 3, 15, 50), bot = c(3, 15, 50, 90),
                        structure_type = c(NA, "platy", NA, NA), oc_pct = c(40, 3, 0.5, 0.1),
                        fiber_content_rubbed_pct = c(50, NA, NA, NA)), "d"))
  # R6 s: over 1 % in the top 20 cm is the salt horizon, no s in the profile;
  # ECe over EC 1:5
  expect_false(.ftv(.ftp("AU-BCA-C", soluble_salts_pct = c(1.5, 0.5, 0.3)), "s"))
  expect_false(.ftv(.ftp(c("Ah", "Bk", "C"), ru = FALSE, ec_dS_m = c(1, 3, 1), ec_1to5_dS_m = c(0.1, 0.3, 0.1)), "s"))
})

test_that("the limits of the definitions, the places of te, crm and [e-hf], and the dispatcher's input", {
  skip_on_cran()
  expect_true(.ftv(.ftp("TO-te-TT"), "te"))
  expect_true(.ftv(.ftp("O-crm-BT-C"), "crm")); expect_false(.ftv(.ftp("AY-EL-BT-crm-C"), "crm"))
  expect_false(.ftv(.ftp("AY-BM-C-C", clay_films_amount = c(NA, "none", "none", "few")), "i"))
  expect_false(.ftv(.ftp("AY-EL-BT-C", ferric_nodules_pct = c(0, 25, 0, 0)), "nn"))
  expect_false(.ftv(.ftp(c("Ah", "Bw", "C"), ru = FALSE, top = c(0, 1, 20), bot = c(1, 20, 50),
                         surface_crust_type = c("clay", NA, NA), polygonal_cracks_spacing_cm = c(10, NA, NA)), "tk"))
  expect_false(.ftv(.ftp("AY-BM-C", structure_type = c("platy", NA, NA), structure_size = c("fine", NA, NA)), "d"))
  # the muck band on top: only on the AU of a dark-humus gley soil
  p <- function(f) .ftp(f, top = c(0, 3, 20), bot = c(3, 20, 60))
  expect_true(.ftv(p("H-AU-G"), "h")); expect_false(.ftv(p("H-AU-BCA"), "AUh"))
  # ml not in a surface humus horizon
  expect_false(.ftv(.ftp("AU-BCA-C", secondary_carbonates_form = c("hard concretions", NA, NA)), "ml"))
  expect_true(.ftv(.ftp("O-CR-C", cryoturbation = c(NA, "yes", NA)), "@"))
  # [e-hf] beside EL, as the book writes it (p. 60)
  expect_true(.ftv(.ftp(c("O", "[e-hf]", "EL", "BEL", "BT", "C")), "[e-hf]"))
  # the dispatcher
  p <- .ftp("AY-BM-C")
  expect_error(.kidpr_features(p, list(list(feature = "g", host = "ZZ"))), "no horizon")
  expect_error(.kidpr_features(p, list(list(feature = "g", place = "beside"))), "no place")
  r <- .kidpr_features(p, list())
  expect_identical(nrow(r), 0L)
  expect_identical(names(r), c("feature", "code", "host", "place", "layers", "passed", "route", "evidence", "missing"))
})


# --- the dispatcher -------------------------------------------------------------------

test_that("the dispatcher returns one row per entry, and decides every feature of the 27 examples", {
  skip_on_cran()
  divs <- eval(formals(make_kidpr2008_example)$division)
  for (d in divs) {
    r <- .kidpr_features(make_kidpr2008_example(d), division = d)
    expect_identical(nrow(r), 67L, info = d)
    expect_identical(names(r), c("feature", "code", "host", "place", "layers", "passed", "route", "evidence", "missing"))
    expect_false(anyNA(r$passed), info = d)
    expect_true(all(r$route %in% c("data", "notation", "both") | is.na(r$route)), info = d)
  }
  # the examples written with features: s, cs, ca on the solonetz (CK)
  r <- .kidpr_features(make_kidpr2008_example("CK"), c("s", "cs", "ca", "g"), division = "CK")
  expect_identical(r$passed, c(TRUE, TRUE, TRUE, FALSE))
  # the permafrost of the cryozem from the notation (C with the sign)
  r <- .kidpr_features(make_kidpr2008_example("CG"), "\u22a5", division = "CG")
  expect_true(r$passed); expect_identical(r$route, "notation")
  # the features do not change the classification (the subtypes follow)
  res <- classify_kidpr2008(make_kidpr2008_example("CK"), on_missing = "silent")
  expect_false("subtypes" %in% res$levels$level)
})

test_that("the readings of the features are Hugo's decisions, each naming the features it settles", {
  skip_on_cran()
  rd <- yaml::read_yaml(system.file("rules", "kidpr2008", "readings.yaml", package = "soilKey"))$readings
  topics <- c("Features read on the data where Q17 is silent",
              "Features read from the notation only where Q17 is silent",
              "The hosts and places of the features",
              "The deposited layers, the red profile and the cut-points of the proxies")
  ft <- Filter(function(x) x$topic %in% topics, rd)
  expect_length(ft, 4L)
  for (x in ft) expect_match(x$reading, "\\(Hugo, 2026-11-02\\)\\.$", info = x$topic)
  expect_false(any(grepl("for Hugo to confirm", vapply(rd, `[[`, "", "reading"), fixed = TRUE)))
  # every feature whose route was soilKey's reading is named in one of them
  m <- .kidpr_ft_meta()
  sk <- names(m)[vapply(m, function(e) identical(e$q17, "soilkey"), NA)]
  txt <- paste(vapply(ft, `[[`, "", "reading"), collapse = " ")
  for (f in sk) expect_true(grepl(f, txt, fixed = TRUE), info = f)
})
