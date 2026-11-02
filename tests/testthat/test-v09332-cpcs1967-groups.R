# v0.9.332: the 101 groups of the French soil classification (CPCS 1967).
# Made-up profiles; no French data.

.fr <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "FR", designation_system = "cpcs", drainage_class = "well drained")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.key <- function(p) classify_cpcs1967(p, on_missing = "silent")
.grp <- function(p) {
  r <- .key(p)
  c(code = if (nrow(r$levels) >= 3) r$levels$code[3] else NA_character_,
    sub = if (nrow(r$levels) >= 2) r$levels$code[2] else NA_character_,
    # the group level and above (v0.9.333 keys the subgroups too)
    prov = paste(grep("^subgroups:", r$provisional_because, value = TRUE, invert = TRUE), collapse = ","))
}
.g <- function(p) .grp(p)[["code"]]
.with_site <- function(p, ...) { q <- p$clone(deep = TRUE); s <- q$site; a <- list(...); s[names(a)] <- a; q$site <- s; q }
.with_h <- function(p, ...) {
  q <- p$clone(deep = TRUE); h <- data.table::copy(q$horizons); a <- list(...)
  for (k in names(a)) h[[k]] <- if (length(a[[k]]) == 1L) rep(a[[k]], nrow(h)) else a[[k]]
  q$horizons <- ensure_horizon_schema(h); q
}

test_that("every example keys to its group, decided, with the French names", {
  skip_on_cran()
  want <- c(raw_mineral = "I/12", weakly_developed = "II/42", vertisol = "III/11", andosol = "IV/11",
            calcimagnesic = "V/11", isohumic = "VI/21", brunified = "VII/11", podzolised = "VIII/11",
            sesquioxide = "IX/22", ferrallitic = "X/21", hydromorphic = "XI/31", sodic = "XII/11")
  for (g in names(want)) {
    r <- .key(make_cpcs1967_example(g))
    expect_identical(r$levels$code[3], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
  }
  r <- .key(make_cpcs1967_example("podzolised"))
  expect_identical(r$levels$name[3], "Podzols")
  expect_identical(r$levels$name_native[3], "Podzols")
  rules <- load_rules("cpcs1967")
  expect_identical(sum(lengths(rules$groups)), 101L)
  expect_identical(sort(names(rules$groups)), sort(unlist(lapply(rules$subclasses, function(l)
    vapply(l, function(t) t$code, "")), use.names = FALSE)))
  expect_length(soilKey:::.validate_rules("cpcs1967")$missing, 0)
})

test_that("class I: deposits, rock, patterned ground, hot deserts", {
  skip_on_cran()
  raw <- make_cpcs1967_example("raw_mineral")
  expect_identical(.g(.with_h(raw, artefacts_pct = c(0, 30, 30))), "I/16")
  expect_identical(.g(.with_h(raw, layer_origin = "volcanic ash")), "I/15")
  expect_identical(.g(.with_h(raw, layer_origin = "colluvium")), "I/13")
  expect_identical(.g(.with_h(raw, layer_origin = "aeolian sand")), "I/14")
  # glacial deposits are not recent deposits: the erosion group
  expect_identical(.g(.with_h(raw, layer_origin = "fluvioglacial gravel")), "I/11")
  expect_identical(.g(.with_h(raw, layer_origin = NA_character_)), "I/11")
  # cold deserts
  rock <- .fr(list(designation = c("(A)", "R"), top_cm = c(0, 5), bottom_cm = c(5, 50), oc_pct = c(0.1, NA),
                   clay_pct = c(5, NA), silt_pct = c(15, NA), sand_pct = c(80, NA)), list(koppen_climate = "ET"))
  expect_identical(.grp(rock)[["sub"]], "I-2")
  expect_identical(.g(rock), "I/21")
  cold <- .with_site(raw, koppen_climate = "ET")
  expect_identical(.g(.with_site(cold, patterned_ground = "sorted stone polygons")), "I/23")
  expect_identical(.g(.with_site(cold, patterned_ground = "earth hummocks")), "I/22")
  r <- .grp(cold)
  expect_identical(r[["code"]], "I/22"); expect_match(r[["prov"]], "groups:I/23")
  # hot deserts
  expect_identical(.g(.with_site(rock, koppen_climate = "BWh")), "I/31")
  hot <- .with_site(raw, koppen_climate = "BWh")
  expect_identical(.g(.with_h(hot, desert_pavement_pct = c(60, NA, NA), varnish_pct = c(20, NA, NA))), "I/33")
  tak <- .with_h(hot, clay_pct = c(40, 30, 30), silt_pct = c(40, 40, 40), sand_pct = c(20, 30, 30),
                 structure_type = c("platy", NA, NA), cracks_depth_cm = c(5, NA, NA), desert_pavement_pct = 0)
  expect_identical(.g(tak), "I/35")
  dune <- .with_h(hot, layer_origin = "aeolian sand", clay_pct = 2, silt_pct = 4, sand_pct = 94, desert_pavement_pct = 0)
  expect_identical(.g(dune), "I/34")
  expect_identical(.g(.with_h(hot, desert_pavement_pct = 0, layer_origin = NA_character_)), "I/32")
})

test_that("class II: ice, humic and xeric soils, deposits", {
  skip_on_cran()
  wd <- make_cpcs1967_example("weakly_developed")
  expect_identical(.g(.with_h(wd, layer_origin = "spoil heap")), "II/46")
  expect_identical(.g(.with_h(wd, layer_origin = "pumice")), "II/45")
  expect_identical(.g(.with_h(wd, layer_origin = "colluvial slope deposit")), "II/43")
  expect_identical(.g(.with_h(wd, layer_origin = "dune sand")), "II/44")
  # with no deposit recorded the AC soil with class V chemistry is calcimagnesic
  er <- .with_h(wd, layer_origin = NA_character_, caco3_pct = 0, ph_h2o = 6.2, bs_pct = 60, ca_cmol = 7, mg_cmol = 1.5)
  expect_identical(.grp(.with_site(er, landform = "eroded hillslope"))[["code"]], "II/41")
  # permafrost
  pf <- .with_h(wd, ice_pct = c(0, 30, 40))
  expect_identical(.grp(pf)[["sub"]], "II-1")
  expect_identical(.g(.with_site(pf, patterned_ground = "palsa")), "II/11")
  expect_identical(.g(.with_site(pf, patterned_ground = "low-centre ice-wedge polygons")), "II/12")
  pf5 <- .with_h(wd, ice_pct = c(0, 5, 5))
  expect_identical(.g(.with_site(pf5, patterned_ground = "mud boils")), "II/13")
  expect_identical(.g(.with_site(pf5, patterned_ground = "none")), "II/14")
  r <- .grp(pf5)
  expect_identical(r[["code"]], "II/14"); expect_match(r[["prov"]], "groups:II/13")
  expect_identical(.g(.with_site(.with_h(wd, ice_pct = c(0, 5, 5), ice_form = c(NA, "ice lenses", NA)),
                                 patterned_ground = "thufurs")), "II/11")
  # humic weakly developed soils
  rk <- .fr(list(designation = c("A1", "C", "R"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                 oc_pct = c(5, 0.6, NA), n_total_pct = c(0.3, 0.05, NA), clay_pct = c(15, 10, NA), silt_pct = c(30, 25, NA),
                 sand_pct = c(55, 65, NA), ph_h2o = c(5.2, 5.4, NA), cec_cmol = c(20, 8, NA), bs_pct = c(30, 30, NA),
                 coarse_fragments_pct = c(20, 50, NA), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, NA),
                 munsell_chroma_moist = c(1, 3, NA), redoximorphic_features_pct = 0), list(parent_material = "granite"))
  expect_identical(.grp(rk)[["sub"]], "II-2")
  expect_identical(.g(rk), "II/21")
  expect_identical(.g(.with_h(rk, rock_origin = c(NA, NA, "hard limestone"))), "II/22")
  expect_identical(.g(.with_h(rk, al_ox_pct = c(2.5, 2, NA), fe_ox_pct = c(1, 1, NA))), "II/23")
  # with nothing recorded about the rock, II/22 is undecided
  r <- .grp(.with_site(rk, parent_material = NULL))
  expect_identical(r[["code"]], "II/21"); expect_match(r[["prov"]], "groups:II/22")
  # xeric weakly developed soils
  xr <- .with_h(rk, oc_pct = c(0.45, 0.3, NA), ph_h2o = c(6.4, 6.6, NA))
  expect_identical(.grp(.with_site(xr, koppen_climate = "BSh"))[["sub"]], "II-3")
  expect_identical(.g(.with_site(xr, koppen_climate = "BSh")), "II/31")
  expect_identical(.g(.with_site(xr, koppen_climate = "Csa")), "II/32")
})

test_that("classes III and IV", {
  skip_on_cran()
  v <- make_cpcs1967_example("vertisol")
  expect_identical(.g(.with_h(v, structure_type = c("angular blocky", "prismatic", "prismatic", "massive"))), "III/12")
  expect_identical(.g(.with_site(v, surface_condition = "self-mulching")), "III/12")
  r <- .grp(.with_h(v, structure_type = c("massive", "prismatic", "prismatic", "massive")))
  expect_identical(r[["code"]], "III/12"); expect_match(r[["prov"]], "groups:III/11")
  sl <- .with_site(v, slope_pct = 8)
  expect_identical(.g(sl), "III/21")
  expect_identical(.g(.with_h(sl, structure_type = c("subangular blocky", "prismatic", "prismatic", "massive"))), "III/21")
  expect_identical(.g(.with_h(sl, structure_type = c("platy", "prismatic", "prismatic", "massive"))), "III/22")
  a <- .with_h(make_cpcs1967_example("andosol"), oc_pct = c(4, 2, 1))
  expect_identical(.grp(a)[["sub"]], "IV-2")
  expect_identical(.g(a), "IV/22")
  expect_identical(.g(.with_h(a, bs_pct = c(60, 70, 70))), "IV/21")
  expect_identical(.g(.with_h(a, bs_pct = NA_real_, ph_h2o = 6.5)), "IV/21")
})

test_that("class V", {
  skip_on_cran()
  c1 <- make_cpcs1967_example("calcimagnesic")
  expect_identical(.g(.with_h(c1, rock_origin = c(NA, "dolomite"))), "V/13")
  expect_identical(.g(.with_h(c1, effervescence_class = c("slight", NA))), "V/13")
  br <- .fr(list(designation = c("A1", "(B)", "R"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 80),
                 oc_pct = c(3, 1, NA), n_total_pct = c(0.3, 0.1, NA), clay_pct = c(30, 32, NA), silt_pct = c(40, 40, NA),
                 sand_pct = c(30, 28, NA), caco3_pct = c(15, 20, NA), ph_h2o = c(8, 8.1, NA), cec_cmol = c(30, 28, NA),
                 ca_cmol = c(28, 26, NA), mg_cmol = c(1, 1, NA), na_cmol = c(0.2, 0.2, NA), bs_pct = c(100, 100, NA),
                 munsell_hue_moist = c("10YR", "10YR", NA), munsell_value_moist = c(3, 4, NA), munsell_chroma_moist = c(2, 3, NA),
                 redoximorphic_features_pct = c(0, 0, NA), structure_type = c("granular", "subangular blocky", NA)),
            list(parent_material = "limestone"))
  expect_identical(.grp(br)[["sub"]], "V-1")
  expect_identical(.g(br), "V/12")
  # V-2: a saturated A without carbonate
  sat <- .with_h(c1, caco3_pct = c(0, NA), effervescence_class = c("none", NA), oc_pct = c(15, NA))
  expect_identical(.grp(sat)[["sub"]], "V-2")
  expect_identical(.g(sat), "V/22")
  mel <- .with_h(c1, caco3_pct = c(0, NA), effervescence_class = c("none", NA), structure_type = c("angular blocky", NA),
                 structure_size = c("fine", NA))
  expect_identical(.g(mel), "V/23")
  expect_identical(.g(.with_h(c1, caco3_pct = c(0, NA), effervescence_class = c("none", NA))), "V/21")
  # V-3
  gy <- .with_h(c1, caso4_pct = c(10, NA))
  expect_identical(.grp(gy)[["sub"]], "V-3")
  expect_identical(.g(gy), "V/31")
  expect_identical(.g(.with_h(br, caso4_pct = c(10, 10, NA))), "V/32")
})

test_that("class VI: the organic matter of the top 20 cm", {
  skip_on_cran()
  iso <- make_cpcs1967_example("isohumic")
  expect_identical(.g(.with_h(iso, oc_pct = c(2, 1.5, 1, 0.3))), "VI/22")
  expect_identical(.g(.with_h(iso, oc_pct = c(1.2, 1, 0.8, 0.3))), "VI/23")
  expect_identical(soilKey:::.cpcs_om20(soilKey:::.cpcs_profile(iso)), 3 * 1.72)
  med <- .with_site(iso, koppen_climate = "Csa")
  expect_identical(.g(med), "VI/31")
  expect_identical(.g(.with_h(med, oc_pct = c(0.5, 0.45, 0.4, 0.2))), "VI/32")
  expect_identical(.g(.with_site(iso, koppen_climate = "Cfb")), "VI/11")
  expect_identical(.g(.with_site(iso, koppen_climate = "BSh")), "VI/41")
})

test_that("class VII", {
  skip_on_cran()
  b <- make_cpcs1967_example("brunified")
  le <- .with_h(b, clay_pct = c(15, 30, 20), sand_pct = c(45, 30, 40))
  expect_identical(.g(le), "VII/12")
  expect_identical(.g(.with_h(le, clay_films_amount = c(NA, "few", NA))), "VII/11")
  expect_identical(.g(.with_h(le, clay_films_amount = c(NA, "common", NA))), "VII/12")
  ab <- .fr(list(designation = c("A1", "A2", "Bt", "C"), top_cm = c(0, 30, 45, 90), bottom_cm = c(30, 45, 90, 130),
                 oc_pct = c(3, 1, 0.5, 0.2), n_total_pct = c(0.25, 0.08, 0.05, 0.02), clay_pct = c(15, 12, 30, 25),
                 silt_pct = 50, sand_pct = c(35, 38, 20, 25), ph_h2o = c(5.8, 5.6, 6, 6.2), cec_cmol = c(15, 10, 18, 15),
                 bs_pct = 40, ca_cmol = c(4, 3, 5, 4), mg_cmol = 1, na_cmol = 0.1,
                 munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 4, 5), munsell_chroma_moist = c(2, 2, 4, 4),
                 munsell_value_dry = c(4, 6, 5, 6), munsell_chroma_dry = c(2, 2, 4, 4), redoximorphic_features_pct = 0),
            list(koppen_climate = "Dfb"))
  expect_identical(.grp(ab)[["sub"]], "VII-2")
  expect_identical(.g(ab), "VII/21")
  expect_identical(.g(.with_h(ab, boundary_topography = c(NA, "tongued", NA, NA))), "VII/22")
  expect_identical(.g(.with_site(ab, koppen_climate = "Dfc")), "VII/31")
  tr <- .with_h(.with_site(b, koppen_climate = "Aw"), bs_pct = 70, ph_h2o = 6.5, munsell_chroma_moist = c(2, 3, 3))
  expect_identical(.grp(tr)[["sub"]], "VII-4")
  expect_identical(.g(tr), "VII/41")
})

test_that("class VIII: the colour table", {
  skip_on_cran()
  p <- make_cpcs1967_example("podzolised")
  pale <- .with_h(p, munsell_value_moist = c(2, 3, 5, 2, 4, 6), munsell_chroma_moist = c(1, 1, 3, 2, 6, 4),
                  munsell_value_dry = c(NA, 4, 6, 3, 5, 7), munsell_chroma_dry = c(NA, 1, 3, 2, 6, 3))
  expect_identical(.g(pale), "VIII/12")
  oc <- .fr(list(designation = c("Ao", "A1", "B2fe", "C"), top_cm = c(0, 5, 15, 60), bottom_cm = c(5, 15, 60, 100),
                 oc_pct = c(35, 5, 1.2, 0.2), n_total_pct = c(1.2, 0.2, 0.06, 0.02), clay_pct = c(NA, 8, 8, 5),
                 silt_pct = c(NA, 12, 12, 8), sand_pct = c(NA, 80, 80, 87), ph_h2o = 4.4, cec_cmol = c(NA, 10, 8, 3),
                 bs_pct = 10, munsell_hue_moist = c("7.5YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(2, 3, 5, 6),
                 munsell_chroma_moist = c(1, 2, 6, 4), redoximorphic_features_pct = c(NA, 0, 0, 0)),
            list(parent_material = "granite"))
  expect_identical(.g(oc), "VIII/13")
  expect_identical(.g(.with_h(oc, munsell_chroma_moist = c(1, 2, 3, 4), oc_pct = c(35, 5, 1.5, 0.2))), "VIII/14")
  # no group of VIII-1 fits: the name stops at the sub-class
  r <- .key(.with_h(oc, munsell_chroma_moist = c(1, 2, 3, 4), oc_pct = c(35, 5, 0.8, 0.2), n_total_pct = c(1.2, 0.2, 0.04, 0.02)))
  expect_identical(r$levels$code, c("VIII", "VIII-1"))
  expect_length(r$provisional_because, 0)
  expect_identical(.g(.with_site(p, koppen_climate = "Dfc")), "VIII/21")
  expect_identical(.g(.with_site(p, koppen_climate = "ET", lat = 46)), "VIII/22")
  r <- .grp(.with_site(p, soil_temperature_regime = "cryic"))
  expect_identical(r[["sub"]], "VIII-2"); expect_true(is.na(r[["code"]]))
  hy <- .with_h(p, redoximorphic_features_pct = c(NA, 0, 15, 0, 0, 0))
  expect_identical(.grp(hy)[["sub"]], "VIII-3")
  expect_identical(.g(hy), "VIII/31")
  # a g with 0 % mottles recorded is not hydromorphic: the g row has none recorded
  mk <- .with_h(p, designation = c("Ao", "A1", "g", "B2h", "B2fe", "C"), redoximorphic_features_pct = c(NA, 0, NA, 0, 0, 0))
  expect_identical(.g(mk), "VIII/32")
  expect_identical(.g(.with_site(mk, groundwater_kind = "observed")), "VIII/31")
  tp <- .with_site(.with_h(p, top_cm = c(0, 5, 12, 80, 90, 120), bottom_cm = c(5, 12, 80, 90, 120, 160),
                           redoximorphic_features_pct = c(NA, 0, 10, 0, 0, 0), ferric_nodules_pct = c(NA, 0, 0, 0, 10, 0)),
                   lat = -3)
  expect_identical(.g(tp), "VIII/33")
})

test_that("class IX", {
  skip_on_cran()
  s <- make_cpcs1967_example("sesquioxide")
  expect_identical(.g(.with_h(s, caco3_pct = c(0, 0, 0, 5), top_cm = c(0, 20, 60, 90), bottom_cm = c(20, 60, 90, 140))), "IX/21")
  # carbonate below 100 cm is no reserve
  expect_identical(.g(.with_h(s, caco3_pct = c(0, 0, 0, 5))), "IX/22")
  expect_identical(.g(.with_h(s, bs_pct = c(96, 96, 98, 98), ph_h2o = 7.2)), "IX/21")
  f <- .with_h(s, bs_pct = c(55, 55, 58, 60))
  expect_identical(.grp(f)[["sub"]], "IX-1")
  expect_identical(.g(f), "IX/12")
  expect_identical(.g(.with_h(f, clay_pct = c(25, 40, 42, 40), clay_films_amount = c("none", "few", "few", "few"))), "IX/13")
  expect_identical(.g(.with_h(f, clay_pct = c(35, 40, 42, 35))), "IX/11")
  # a thin A (under 15 cm and a third of the solum): leached only with clay
  # films in the B (p. 59)
  thin <- .with_h(f, top_cm = c(0, 10, 60, 100), bottom_cm = c(10, 60, 100, 140))
  expect_identical(.g(thin), "IX/12")
  expect_identical(.g(.with_h(thin, clay_films_amount = c("none", "few", "few", "few"))), "IX/11")
  # the calcium reserve read row by row: a B at V 80 fails whatever its pH
  expect_identical(.grp(.with_h(s, bs_pct = c(80, 80, 80, 80), ph_h2o = NA_real_))[["prov"]], "")
})

test_that("class X", {
  skip_on_cran()
  x <- make_cpcs1967_example("ferrallitic")
  # (with the C/N of a ferrallitic B, under 14: a C/N above 14 is a podzolic B)
  expect_identical(.g(.with_h(x, oc_pct = c(5, 2, 1, 0.8), n_total_pct = c(0.45, 0.18, 0.09, 0.07))), "X/22")
  expect_identical(.g(.with_h(x, clay_pct = c(30, 45, 60, 60), sand_pct = c(60, 45, 30, 30))), "X/23")
  expect_identical(.g(.with_h(x, coarse_fragments_pct = c(0, 30, 5, 0))), "X/24")
  expect_identical(.g(.with_h(x, sand_feldspar_pct = c(NA, 12, NA, NA))), "X/25")
  x1 <- .with_h(x, bs_pct = 50)
  expect_identical(.g(x1), "X/11")
  expect_identical(.g(.with_h(x1, clay_pct = c(30, 45, 60, 60), sand_pct = c(60, 45, 30, 30))), "X/12")
  expect_identical(.g(.with_h(x1, designation = c("A1", "B1", "IIB21", "IIB22"))), "X/13")
  expect_identical(.g(.with_h(x1, sand_mineralogy = c(NA, "micacea", NA, NA))), "X/14")
  x3 <- .with_h(x, bs_pct = 10)
  expect_identical(.g(x3), "X/31")
  expect_identical(.g(.with_h(x3, oc_pct = c(2, 1.5, 1.2, 1.1), n_total_pct = c(0.18, 0.13, 0.11, 0.1))), "X/32")
  le <- .with_h(x3, clay_pct = c(30, 45, 60, 60), sand_pct = c(60, 45, 30, 30))
  expect_identical(.g(le), "X/36")
  expect_identical(.g(.with_h(le, clay_films_amount = c(NA, "few", "few", "few"))), "X/33")
  expect_identical(.g(.with_h(x3, layer_origin = c("colluvium", NA, NA, NA))), "X/34")
  expect_identical(.g(.with_h(x3, sand_amphibole_pct = c(NA, 15, NA, NA))), "X/35")
})

test_that("class XI", {
  skip_on_cran()
  h <- make_cpcs1967_example("hydromorphic")
  expect_identical(.g(.with_h(h, pan_type = c(NA, "ferricrete", NA))), "XI/35")
  expect_identical(.g(.with_h(h, secondary_carbonates_pct = c(0, 10, 0))), "XI/36")
  expect_identical(.g(.with_site(h, groundwater_kind = "perched")), "XI/33")
  pg <- .fr(list(designation = c("A1", "Bg1", "Bg2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                 oc_pct = c(2.5, 0.5, 0.3), n_total_pct = c(0.2, 0.05, 0.03), clay_pct = 30, silt_pct = 40, sand_pct = 30,
                 ph_h2o = 6.2, cec_cmol = 18, bs_pct = 60, ca_cmol = 8, mg_cmol = 2, na_cmol = 0.2,
                 munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 4, 4),
                 redoximorphic_features_pct = c(0, 25, 30)), list(drainage_class = "imperfectly drained"))
  expect_identical(.g(pg), "XI/32")
  am <- .with_h(pg, designation = c("A1", "Bg", "G"), munsell_hue_moist = c("10YR", "10YR", "5GY"),
                munsell_chroma_moist = c(2, 4, 1), top_cm = c(0, 20, 90), bottom_cm = c(20, 90, 130))
  expect_identical(.g(am), "XI/34")
  # the deep gley of the amphigley starts at 80 cm
  expect_identical(.g(.with_h(am, top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120))), "XI/31")
  # in a declared FAO notation, g is a pseudogley and l the fringe of a gley
  fao <- .with_site(am, designation_system = "fao", groundwater_kind = "observed")
  expect_identical(.g(.with_h(fao, designation = c("A", "Bg", "Cr"))), "XI/34")
  expect_identical(.g(.with_h(fao, designation = c("A", "Bl", "Cr"))), "XI/31")
  # mottling continuous with the reduced matrix, in a notation that is not
  # declared, is the gley's own
  expect_identical(.g(.with_site(.with_h(am, designation = c("A", "Bg", "Cr")), designation_system = NULL,
                                 groundwater_kind = "observed")), "XI/31")
  an <- .with_h(h, oc_pct = c(10, 0.5, 0.3), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120))
  expect_identical(.grp(an)[["sub"]], "XI-2")
  expect_identical(.g(an), "XI/21")
  expect_identical(.g(.with_h(an, designation = c("A1", "Bg", "Bg"), munsell_hue_moist = "10YR",
                              munsell_chroma_moist = c(2, 4, 4), redoximorphic_features_pct = c(0, 30, 30))), "XI/22")
  pt <- .fr(list(designation = c("H1", "H2", "C"), top_cm = c(0, 40, 90), bottom_cm = c(40, 90, 130),
                 oc_pct = c(40, 35, 0.5), clay_pct = c(NA, NA, 30), ph_h2o = 4.5, munsell_hue_moist = "10YR",
                 munsell_value_moist = c(2, 2, 5), munsell_chroma_moist = c(1, 1, 2), redoximorphic_features_pct = c(NA, NA, 0)),
            list(drainage_class = "very poorly drained"))
  expect_identical(.grp(pt)[["sub"]], "XI-1")
  expect_identical(.g(.with_h(pt, fiber_content_unrubbed_pct = c(80, 75, NA))), "XI/11")
  expect_identical(.g(.with_h(pt, von_post_index = c("H5", "H4", NA))), "XI/12")
  expect_identical(.g(.with_h(pt, fiber_content_rubbed_pct = c(10, 12, NA))), "XI/13")
  # the class over the greatest thickness of the top 80 cm decides
  expect_identical(.g(.with_h(pt, fiber_content_unrubbed_pct = c(80, 10, NA), top_cm = c(0, 30, 90), bottom_cm = c(30, 90, 130))), "XI/13")
  # a tie goes to the first class of the key
  expect_identical(.g(.with_h(pt, fiber_content_unrubbed_pct = c(80, 10, NA))), "XI/11")
  r <- .key(pt)
  expect_identical(r$levels$code, c("XI", "XI-1"))
  expect_match(paste(r$provisional_because, collapse = ","), "groups:XI/11")
  # the FAO suffixes i, e, a of an O, with no fibre data
  fao <- .with_site(.with_h(pt, designation = c("Oa", "Oa", "C")), designation_system = "fao")
  expect_identical(.g(fao), "XI/13")
})

test_that("class XII", {
  skip_on_cran()
  so <- .fr(list(designation = c("A1", "Bt", "C"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 100),
                 oc_pct = c(1, 0.5, 0.2), n_total_pct = c(0.1, 0.05, 0.02), clay_pct = c(15, 35, 30), silt_pct = 40,
                 sand_pct = c(45, 25, 30), ec_dS_m = c(1, 2, 3), na_cmol = c(0.5, 4, 3), cec_cmol = 15, bs_pct = 100,
                 ph_h2o = c(7.5, 8.8, 8.6), structure_type = c("granular", "columnar", "massive"),
                 consistence_moist = c("friable", "very firm", "firm"), munsell_hue_moist = "10YR",
                 munsell_value_moist = c(4, 3, 5), munsell_chroma_moist = c(2, 2, 3), redoximorphic_features_pct = 0))
  expect_identical(.grp(so)[["sub"]], "XII-2")
  expect_identical(.g(so), "XII/22")
  sd <- .fr(list(designation = c("A1", "A2", "Bt", "C"), top_cm = c(0, 10, 25, 60), bottom_cm = c(10, 25, 60, 100),
                 oc_pct = c(1.5, 0.3, 0.5, 0.2), n_total_pct = c(0.12, 0.03, 0.05, 0.02), clay_pct = c(12, 10, 35, 30),
                 silt_pct = 40, sand_pct = c(48, 50, 25, 30), ec_dS_m = c(0.5, 1, 2, 3), na_cmol = c(0.3, 0.4, 4, 3),
                 cec_cmol = 15, bs_pct = c(60, 60, 100, 100), ph_h2o = c(6, 6.2, 8.8, 8.6),
                 structure_type = c("granular", "platy", "columnar", "massive"),
                 consistence_moist = c("friable", "friable", "very firm", "firm"), munsell_hue_moist = "10YR",
                 munsell_value_moist = c(4, 6, 3, 5), munsell_chroma_moist = c(2, 2, 2, 3),
                 munsell_value_dry = c(5, 7, 4, 6), munsell_chroma_dry = c(2, 2, 2, 3), redoximorphic_features_pct = 0))
  expect_identical(.g(sd), "XII/23")
  expect_identical(.g(.with_h(so, designation = c("A1", "(B)", "C"), clay_pct = 30, sand_pct = 30)), "XII/21")
})

test_that("the fixes of the reviews", {
  skip_on_cran()
  # "non-sorted" circles are patterned ground, not a negation
  cold <- .with_site(make_cpcs1967_example("raw_mineral"), koppen_climate = "ET")
  expect_identical(.g(.with_site(cold, patterned_ground = "non-sorted circles")), "I/23")
  expect_identical(.g(.with_site(cold, patterned_ground = TRUE)), "I/23")
  # the humic ferrallitic soils: more than 1 % OM to at least 100 cm
  x <- make_cpcs1967_example("ferrallitic")
  short <- .with_h(x, oc_pct = c(1.5, 1.2, 1, 0.8), n_total_pct = c(0.14, 0.11, 0.09, 0.07),
                   top_cm = c(0, 15, 30, 45), bottom_cm = c(15, 30, 45, 60))
  r <- .grp(short)
  expect_identical(r[["code"]], "X/21"); expect_match(r[["prov"]], "groups:X/22")
  # a stone line needs a recorded row below with less gravel
  expect_identical(.g(.with_h(x, coarse_fragments_pct = c(0, 0, 0, 25))), "X/21")
  expect_identical(.g(.with_h(x, coarse_fragments_pct = c(NA, 30, NA, NA))), "X/21")
  # negated or trace minerals are not weatherable minerals
  expect_identical(.g(.with_h(x, sand_mineralogy = c(NA, "quartz; no feldspar or mica", NA, NA))), "X/21")
  expect_identical(.g(.with_h(x, sand_mineralogy = c(NA, "traces of mica", NA, NA))), "X/21")
  # a fine subangular blocky A is not polyhedral (V/23)
  c1 <- make_cpcs1967_example("calcimagnesic")
  expect_identical(.g(.with_h(c1, caco3_pct = c(0, NA), effervescence_class = c("none", NA),
                              structure_type = c("subangular blocky", NA), structure_size = c("fine", NA))), "V/21")
  # primary structure first
  expect_false(soilKey:::.cpcs_round_row("prismatic parting to subangular blocky"))
  expect_true(soilKey:::.cpcs_round_row("subangular blocky parting to granular"))
  # gley or stagnogley: the water table decides when the morphology is both
  h <- make_cpcs1967_example("hydromorphic")
  nogw <- .with_site(h, groundwater_kind = NULL, groundwater_depth_cm = NULL)
  r <- .grp(nogw)
  expect_identical(r[["code"]], "XI/33"); expect_match(r[["prov"]], "groups:XI/31")
  expect_identical(.g(.with_site(h, groundwater_kind = "none observed")), "XI/33")
  # a gley from 70 cm with no water table recorded is still a gley
  deep <- .with_h(nogw, designation = c("A", "Bg", "Cg"), munsell_hue_moist = c("10YR", "10YR", "5GY"),
                  munsell_chroma_moist = c(2, 4, 1), top_cm = c(0, 20, 70), bottom_cm = c(20, 70, 120))
  expect_identical(.g(.with_site(deep, groundwater_kind = "none", designation_system = "fao")), "XI/31")
  # a matrix colour not recorded leaves the gley undecided
  nocol <- .with_site(.fr(list(designation = c("A", "Bg", "Cg"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                               oc_pct = c(2, 0.4, 0.2), redoximorphic_features_pct = c(2, 30, 40))), designation_system = "fao")
  expect_match(.grp(nocol)[["prov"]], "groups:XI/31")
  # the solodised soils keep an A2 whose albic test cannot be read
  sd <- .fr(list(designation = c("A1", "A2", "Bt", "C"), top_cm = c(0, 10, 25, 60), bottom_cm = c(10, 25, 60, 100),
                 oc_pct = c(1.5, 0.3, 0.5, 0.2), n_total_pct = c(0.12, 0.03, 0.05, 0.02), clay_pct = c(12, 10, 35, 30),
                 silt_pct = 40, sand_pct = c(48, 50, 25, 30), ec_dS_m = c(0.5, 1, 2, 3), na_cmol = c(0.3, 0.4, 4, 3),
                 cec_cmol = 15, bs_pct = c(60, 60, 100, 100), ph_h2o = c(6, 6.2, 8.8, 8.6),
                 structure_type = c("granular", "platy", "columnar", "massive"),
                 consistence_moist = c("friable", "friable", "very firm", "firm"), redoximorphic_features_pct = 0))
  r <- .grp(sd)
  expect_identical(r[["code"]], "XII/22"); expect_match(r[["prov"]], "groups:XII/23")
  # clay films recorded in part of the B do not rule the leached soils out
  b <- make_cpcs1967_example("brunified")
  le <- .with_h(b, designation = c("A1", "BA", "Bt"), clay_pct = c(10, 15, 30), sand_pct = c(50, 45, 30),
                clay_films_amount = c(NA, "few", NA))
  expect_identical(.g(le), "VII/12")
  # an A1/A2/C profile has no textural B
  ac <- .with_h(b, designation = c("A1", "A2", "C"))
  expect_identical(.grp(ac)[["prov"]], "")
  # a recent deposit recorded whose kind cannot be read: the erosion group is undecided
  wd <- make_cpcs1967_example("weakly_developed")
  r <- .grp(.with_h(wd, layer_origin = "recent deposit"))
  expect_identical(r[["code"]], "II/41"); expect_match(r[["prov"]], "groups:II/46")
  # the mean OM of the top 20 cm skips a row of no thickness
  iso <- .with_h(make_cpcs1967_example("isohumic"), top_cm = c(0, 35, 70, 100), bottom_cm = c(35, 70, 100, 140))
  expect_equal(soilKey:::.cpcs_om20(soilKey:::.cpcs_profile(iso)), 3 * 1.72)
  # a pan recorded as absent is no cemented B
  p <- make_cpcs1967_example("podzolised")
  tp <- .with_site(.with_h(p, top_cm = c(0, 5, 12, 80, 90, 120), bottom_cm = c(5, 12, 80, 90, 120, 160),
                           redoximorphic_features_pct = c(NA, 0, 10, 0, 0, 0), pan_type = c(NA, NA, NA, NA, "no pan", NA)),
                   lat = -3)
  expect_identical(.g(tp), "VIII/31")
  # a molken-podzol under a water table recorded as none
  mk <- .with_h(p, designation = c("Ao", "A1", "g", "B2h", "B2fe", "C"), redoximorphic_features_pct = c(NA, 0, NA, 0, 0, 0))
  expect_identical(.g(.with_site(mk, groundwater_kind = "none")), "VIII/32")
})

test_that("Hugo's answers on the review (2026-11-01)", {
  skip_on_cran()
  # VIII/13: a thin A2 is allowed, a Bh over 3 cm is not
  oc <- .fr(list(designation = c("Ao", "A1", "A2", "B2fe", "C"), top_cm = c(0, 5, 15, 17, 60), bottom_cm = c(5, 15, 17, 60, 100),
                 oc_pct = c(35, 5, 0.6, 1.2, 0.2), n_total_pct = c(1.2, 0.2, 0.04, 0.06, 0.02), clay_pct = c(NA, 8, 6, 8, 5),
                 silt_pct = c(NA, 12, 10, 12, 8), sand_pct = c(NA, 80, 84, 80, 87), ph_h2o = 4.4, cec_cmol = c(NA, 10, 3, 8, 3),
                 bs_pct = 10, munsell_hue_moist = c("7.5YR", "10YR", "10YR", "7.5YR", "10YR"),
                 munsell_value_moist = c(2, 3, 4, 5, 6), munsell_chroma_moist = c(1, 2, 2, 6, 4),
                 redoximorphic_features_pct = c(NA, 0, 0, 0, 0)), list(parent_material = "granite"))
  expect_identical(.g(oc), "VIII/13")
  bh <- .with_h(oc, designation = c("Ao", "A1", "B2h", "B2fe", "C"), oc_pct = c(35, 5, 3, 1.2, 0.2),
                n_total_pct = c(1.2, 0.2, 0.15, 0.06, 0.02), top_cm = c(0, 5, 15, 25, 60), bottom_cm = c(5, 15, 25, 60, 100))
  expect_false(isTRUE(.g(bh) == "VIII/13"))
  # an alpine site of a Dfc climate
  p <- make_cpcs1967_example("podzolised")
  expect_identical(.g(.with_site(p, koppen_climate = "Dfc", lat = 46, elevation_m = 1800)), "VIII/22")
  expect_identical(.g(.with_site(p, koppen_climate = "Dfc", lat = 62, elevation_m = 1800)), "VIII/21")
  # V/31: an ACR profile without A3 or (B), whatever its depth
  gy <- .fr(list(designation = c("A1", "C"), top_cm = c(0, 30), bottom_cm = c(30, 120), oc_pct = c(1.5, 0.3),
                 n_total_pct = c(0.15, 0.03), clay_pct = 25, silt_pct = 40, sand_pct = 35, caso4_pct = c(15, 40),
                 caco3_pct = c(5, 5), ph_h2o = 7.8, cec_cmol = 15, ca_cmol = 14, mg_cmol = 0.5, bs_pct = 100,
                 munsell_hue_moist = "10YR", munsell_value_moist = c(4, 6), munsell_chroma_moist = c(2, 3),
                 redoximorphic_features_pct = 0), list(koppen_climate = "BSh", parent_material = "gypsum marl"))
  r <- .grp(gy)
  expect_identical(r[["sub"]], "V-3"); expect_identical(r[["code"]], "V/31")
  # VII/12 without clay: a plain B of the CPCS notation is textural
  b <- make_cpcs1967_example("brunified")
  expect_identical(.g(.with_h(b, designation = c("A1", "B2", "C"), clay_pct = NA_real_)), "VII/12")
  expect_identical(.g(.with_h(b, clay_pct = NA_real_)), "VII/11")
  # II/12: an ice-wedge network recorded is a strong ice segregation
  wd <- .with_h(make_cpcs1967_example("weakly_developed"), ice_pct = c(0, 5, 5))
  expect_identical(.g(.with_site(wd, patterned_ground = "ice-wedge polygons")), "II/12")
  # XII/23 without a pH is undecided; XII/22 reads a columnar B as compact
  sd <- .fr(list(designation = c("A1", "A2", "Bt", "C"), top_cm = c(0, 10, 25, 60), bottom_cm = c(10, 25, 60, 100),
                 oc_pct = c(1.5, 0.3, 0.5, 0.2), n_total_pct = c(0.12, 0.03, 0.05, 0.02), clay_pct = c(12, 10, 35, 30),
                 silt_pct = 40, sand_pct = c(48, 50, 25, 30), ec_dS_m = c(0.5, 1, 2, 3), na_cmol = c(0.3, 0.4, 4, 3),
                 cec_cmol = 15, bs_pct = c(60, 60, 100, 100), structure_type = c("granular", "platy", "columnar", "massive"),
                 consistence_moist = c("friable", "friable", NA, NA), munsell_hue_moist = "10YR",
                 munsell_value_moist = c(4, 6, 3, 5), munsell_chroma_moist = c(2, 2, 2, 3),
                 munsell_value_dry = c(5, 7, 4, 6), munsell_chroma_dry = c(2, 2, 2, 3), redoximorphic_features_pct = 0))
  firm <- .with_h(sd, consistence_moist = c("friable", "friable", "very firm", "firm"))
  r <- .grp(firm)
  expect_identical(r[["code"]], "XII/22"); expect_match(r[["prov"]], "groups:XII/23")
  expect_identical(.g(.with_h(firm, ph_h2o = c(6, 6.2, 8.8, 8.6))), "XII/23")
  expect_identical(.g(.with_h(firm, ph_h2o = c(7.5, 7.6, 8.8, 8.6))), "XII/22")
  # the class passes on the compact C; the columnar Bt with no consistence is the Solonetz B
  col <- .with_h(sd, ph_h2o = c(7.5, 7.6, 8.8, 8.6), consistence_moist = c("friable", "friable", NA, "very firm"),
                 na_cmol = c(0.3, 0.4, 4, 4))
  expect_identical(.g(col), "XII/22")
  # the names that carry soilKey's insertion in brackets
  rules <- load_rules("cpcs1967")
  nm <- unlist(lapply(rules$groups, function(l) vapply(l, function(t) t$name, "")))
  expect_true(any(grepl("[possible]", nm, fixed = TRUE)))
  expect_true(any(grepl("[remani\u00e9s]", nm, fixed = TRUE)))
})

