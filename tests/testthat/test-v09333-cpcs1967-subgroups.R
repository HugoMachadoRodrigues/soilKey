# v0.9.333: the 276 subgroups of the French soil classification (CPCS 1967).
# Made-up profiles; no French data. Every subgroup is reached at least once;
# the last test checks the list.

.fr <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "FR", designation_system = "cpcs", drainage_class = "well drained")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.key <- function(p) classify_cpcs1967(p, on_missing = "silent")
.with_site <- function(p, ...) { q <- p$clone(deep = TRUE); s <- q$site; a <- list(...); s[names(a)] <- a; q$site <- s; q }
.with_h <- function(p, ...) {
  q <- p$clone(deep = TRUE); h <- data.table::copy(q$horizons); a <- list(...)
  for (k in names(a)) h[[k]] <- if (length(a[[k]]) == 1L) rep(a[[k]], nrow(h)) else a[[k]]
  q$horizons <- ensure_horizon_schema(h); q
}
.reached <- new.env()
.sgp <- function(r) grep("^subgroups:", r$provisional_because, value = TRUE)
# the subgroup a profile keys to, decided; recorded for the coverage test
.hit <- function(p, code) {
  r <- .key(p)
  got <- if (nrow(r$levels) >= 4L) r$levels$code[4L] else NA_character_
  expect_identical(got, code)
  expect_identical(.sgp(r), character(0), info = code)
  if (identical(got, code) && !length(.sgp(r))) assign(code, TRUE, envir = .reached)
  invisible(r)
}
# a profile that stops at its group
.stop_at <- function(p, group, provisional = FALSE) {
  r <- .key(p)
  expect_identical(r$levels$code[nrow(r$levels)], group)
  expect_identical(length(.sgp(r)) > 0L, provisional, info = group)
  invisible(r)
}

test_that("276 subgroups, valid rules, and the examples keyed to a subgroup", {
  skip_on_cran()
  rules <- load_rules("cpcs1967")
  expect_identical(sum(lengths(rules$subgroups)), 276L)
  expect_identical(soilKey:::.system_get("cpcs1967")$levels, c("classes", "subclasses", "groups", "subgroups"))
  v <- soilKey:::.validate_rules("cpcs1967")
  for (k in c("missing", "mixed", "unsuffixed", "malformed", "codes")) expect_length(v[[k]], 0)
  # every list is under a group, and every code is the class and three digits
  grp <- unlist(lapply(rules$groups, function(l) vapply(l, function(t) t$code, "")), use.names = FALSE)
  expect_true(all(names(rules$subgroups) %in% grp))
  expect_length(setdiff(grp, names(rules$subgroups)), 22L)
  for (g in names(rules$subgroups)) for (t in rules$subgroups[[g]]) {
    expect_match(t$code, paste0("^", sub("/.*", "", g), "/", sub(".*/", "", g), "[0-9]$"))
    # a full printed name stands alone; an adjective follows its group
    expect_true(grepl("^\\{parent\\}, ", t$name) || !grepl("{parent}", t$name, fixed = TRUE))
    expect_identical(t$name, t$name_native)
  }
  want <- c(raw_mineral = "I/121", weakly_developed = "II/421", vertisol = "III/111", calcimagnesic = "V/113",
            isohumic = "VI/212", brunified = "VII/112", podzolised = "VIII/113", sesquioxide = "IX/221",
            ferrallitic = "X/216", hydromorphic = "XI/311", sodic = "XII/112")
  for (cl in names(want)) .hit(make_cpcs1967_example(cl), want[[cl]])
  # the andosol of the cold countries: IV/11 has no subgroups, a documented stop
  r <- .key(make_cpcs1967_example("andosol"))
  expect_identical(r$levels$code, c("IV", "IV-1", "IV/11"))
  expect_length(r$provisional_because, 0)
  # a full printed name stands alone; an adjective follows its group
  r <- .key(make_cpcs1967_example("podzolised"))
  expect_identical(r$name, "Podzols humo-ferrugineux")
  expect_identical(r$levels$name_native[4], "Podzols humo-ferrugineux")
  expect_identical(.key(make_cpcs1967_example("raw_mineral"))$name, "Sols min\u00e9raux bruts d'apport alluvial, modal")
})

test_that("class I", {
  skip_on_cran()
  raw <- make_cpcs1967_example("raw_mineral")
  for (o in c(I12 = NA, I13 = "colluvium", I14 = "aeolian sand", I15 = "volcanic ash")) {
    p <- if (is.na(o)) raw else .with_h(raw, layer_origin = o)
    g <- .key(p)$levels$code[3]; x <- sub("I/", "", g)
    .hit(p, paste0("I/", x, "1"))
    .hit(.with_site(p, groundwater_depth_cm = 60), paste0("I/", x, "2"))
    .hit(.with_h(p, redoximorphic_features_pct = c(0, 2, 0)), paste0("I/", x, "2"))
  }
  er <- .with_h(raw, layer_origin = NA_character_)
  .hit(er, "I/112")
  lith <- .fr(list(designation = c("(A)", "R"), top_cm = c(0, 15), bottom_cm = c(15, 60), oc_pct = c(0.1, NA),
                   clay_pct = c(5, NA), silt_pct = c(15, NA), sand_pct = c(80, NA)))
  .hit(lith, "I/111")
  # I/16 has no subgroups
  .stop_at(.with_h(raw, artefacts_pct = c(0, 30, 30)), "I/16")
  cold <- .with_site(raw, koppen_climate = "ET")
  .hit(.with_site(cold, patterned_ground = "none"), "I/221")
  .hit(.with_site(cold, patterned_ground = "earth hummocks"), "I/222")
  .hit(.with_site(cold, patterned_ground = "sand wedges"), "I/231")
  .hit(.with_site(cold, patterned_ground = "earth polygons"), "I/232")
  .hit(.with_site(cold, patterned_ground = "sorted stone polygons"), "I/233")
  .hit(.with_site(cold, patterned_ground = "mud boils between the stones"), "I/234")
  .hit(.with_site(cold, patterned_ground = "sorted stripes"), "I/235")
  # non-sorted circles are patterned ground that fits no listed subgroup
  .stop_at(.with_site(cold, patterned_ground = "non-sorted circles"), "I/23")
})

test_that("class II", {
  skip_on_cran()
  wd <- make_cpcs1967_example("weakly_developed")
  for (o in c("calcareous alluvium", "colluvial slope deposit", "dune sand", "pumice", "spoil heap")) {
    p <- .with_h(wd, layer_origin = o)
    x <- sub("II/", "", .key(p)$levels$code[3])
    .hit(p, paste0("II/", x, "1"))
    .hit(.with_site(p, groundwater_depth_cm = 50), paste0("II/", x, "2"))
    .hit(.with_h(p, oc_pct = c(3.5, 0.4, 0.2)), paste0("II/", x, "3"))
  }
  er <- .with_site(.with_h(wd, layer_origin = NA_character_, caco3_pct = 0, ph_h2o = 6.2, bs_pct = 60, ca_cmol = 7,
                           mg_cmol = 1.5), landform = "eroded hillslope")
  .hit(er, "II/411")
  .hit(.with_h(er, designation = c("A1", "R", "R")), "II/412")
  pf <- .with_h(wd, ice_pct = c(0, 30, 40))
  .hit(.with_site(pf, patterned_ground = "palsa"), "II/111")
  .hit(.with_site(pf, patterned_ground = "thufurs"), "II/112")
  .hit(.with_site(.with_h(pf, ice_form = c(NA, "large ice lenses", NA)), patterned_ground = "none"), "II/111")
  # strong ice and lenses of no recorded size: no subgroup
  .stop_at(.with_site(.with_h(pf, ice_form = c(NA, "ice lenses", NA)), patterned_ground = "none"), "II/11")
  .hit(.with_site(pf, patterned_ground = "low-centre ice-wedge polygons"), "II/121")
  .hit(.with_site(pf, patterned_ground = "ice-cored mounds in a polygon network"), "II/122")
  pf5 <- .with_h(wd, ice_pct = c(0, 5, 5))
  .hit(.with_site(pf5, patterned_ground = "small hummocks separated by wide cracks"), "II/131")
  .hit(.with_site(pf5, patterned_ground = "mud boils"), "II/132")
  .hit(.with_site(pf5, patterned_ground = "sorted stone circles"), "II/133")
  .hit(.with_site(pf5, patterned_ground = "sorted stripes"), "II/134")
  none <- .with_site(pf5, patterned_ground = "none")
  .hit(none, "II/141")
  .hit(.with_h(none, redoximorphic_features_pct = c(0, 0, 10)), "II/142")
  # signs above 50 cm are not deep hydromorphy
  .hit(.with_h(none, redoximorphic_features_pct = c(0, 3, 10)), "II/141")
  rk <- .fr(list(designation = c("A1", "C", "R"), top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 80),
                 oc_pct = c(5, 0.6, NA), n_total_pct = c(0.3, 0.05, NA), clay_pct = c(15, 10, NA), silt_pct = c(30, 25, NA),
                 sand_pct = c(55, 65, NA), ph_h2o = c(5.2, 5.4, NA), cec_cmol = c(20, 8, NA), bs_pct = c(30, 30, NA),
                 coarse_fragments_pct = c(20, 50, NA), munsell_hue_moist = "10YR", munsell_value_moist = c(2, 4, NA),
                 munsell_chroma_moist = c(1, 3, NA), redoximorphic_features_pct = 0), list(parent_material = "granite"))
  .hit(.with_site(rk, humus_form = "mor"), "II/211")
  .hit(.with_site(rk, humus_form = "moder", elevation_m = 1800), "II/213")
  .hit(.with_site(rk, humus_form = "moder"), "II/212")
  # a mull ranker stops at the group; no humus form recorded, undecided
  .stop_at(.with_site(rk, humus_form = "mull"), "II/21")
  .stop_at(rk, "II/21", provisional = TRUE)
  xr <- .with_site(.with_h(rk, oc_pct = c(0.45, 0.3, NA), ph_h2o = c(6.4, 6.6, NA)), koppen_climate = "BSh")
  .hit(xr, "II/311")
  .hit(.with_h(xr, ec_dS_m = c(5, 3, NA)), "II/312")
  .hit(.with_site(xr, wind_deflation = TRUE), "II/313")
})

test_that("class III", {
  skip_on_cran()
  v <- make_cpcs1967_example("vertisol")
  sl <- .with_site(v, slope_pct = 8)
  ang <- c("angular blocky", "prismatic", "prismatic", "massive")
  for (g in list(c("III/11", "v"), c("III/12", "a"), c("III/21", "s"), c("III/22", "sa"))) {
    p <- switch(g[2], v = v, a = .with_h(v, structure_type = ang), s = sl,
                sa = .with_h(sl, structure_type = c("platy", "prismatic", "prismatic", "massive")))
    x <- sub("III/", "", g[1])
    .hit(p, paste0("III/", x, "1"))
    .hit(.with_h(p, slickensides = c("few", "common", "common", "few")), paste0("III/", x, "2"))
    .hit(.with_h(p, redoximorphic_features_pct = c(0, 20, 0, 0)), paste0("III/", x, "3"))
    .hit(.with_h(p, ferric_nodules_pct = c(0, 6, 0, 0)), paste0("III/", x, "3"))
    .hit(.with_h(p, ec_dS_m = 5), paste0("III/", x, "4"))
  }
  .hit(.with_site(v, surface_condition = "self-mulching"), "III/125")
  sm <- .with_site(sl, surface_condition = "self-mulching")
  .hit(.with_h(sm, structure_size = c("fine", "coarse", "coarse", "coarse")), "III/225")
  # self-mulching with no structure size: 20 cm undecided
  r <- .key(sm)
  expect_identical(r$levels$code[4], "III/221"); expect_identical(.sgp(r), "subgroups:III/225")
  # fine structure to 10 cm only is not 20 cm
  .hit(.with_h(.with_h(sm, top_cm = c(0, 10, 60, 110), bottom_cm = c(10, 60, 110, 150)),
               structure_size = c("fine", "coarse", "coarse", "coarse")), "III/221")
})

test_that("class IV", {
  skip_on_cran()
  a <- .with_h(make_cpcs1967_example("andosol"), oc_pct = c(4, 2, 1))
  for (s in list(c("IV/22", NA), c("IV/21", 60))) {
    p <- if (is.na(s[2])) a else .with_h(a, bs_pct = c(60, 70, 70))
    expect_identical(.key(p)$levels$code[3], s[1])
    x <- sub("IV/", "", s[1])
    .hit(p, paste0("IV/", x, "2"))
    .hit(.with_h(p, top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 120)), paste0("IV/", x, "1"))
    .hit(.with_h(p, top_cm = c(0, 15, 40), bottom_cm = c(15, 40, 120)), paste0("IV/", x, "3"))
  }
})

test_that("class V", {
  skip_on_cran()
  c1 <- make_cpcs1967_example("calcimagnesic")
  .hit(.with_h(c1, oc_pct = c(6.5, NA)), "V/111")
  .hit(.with_h(c1, effervescence_class = c("very strong", NA)), "V/112")
  .hit(.with_h(c1, caco3_active_pct = c(12, NA)), "V/112")
  .hit(.with_h(c1, caco3_pct = c(5, NA), coarse_fragments_pct = c(30, NA)), "V/114")
  .hit(.with_site(c1, koppen_climate = "Csa"), "V/115")
  br <- .fr(list(designation = c("A1", "(B)", "R"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 80),
                 oc_pct = c(3, 1, NA), n_total_pct = c(0.3, 0.1, NA), clay_pct = c(30, 32, NA), silt_pct = c(40, 40, NA),
                 sand_pct = c(30, 28, NA), caco3_pct = c(15, 20, NA), ph_h2o = c(8, 8.1, NA), cec_cmol = c(30, 28, NA),
                 ca_cmol = c(28, 26, NA), mg_cmol = c(1, 1, NA), na_cmol = c(0.2, 0.2, NA), bs_pct = c(100, 100, NA),
                 munsell_hue_moist = c("10YR", "10YR", NA), munsell_value_moist = c(3, 4, NA), munsell_chroma_moist = c(2, 3, NA),
                 redoximorphic_features_pct = c(0, 0, NA), structure_type = c("granular", "subangular blocky", NA)),
            list(parent_material = "limestone"))
  .hit(br, "V/121")
  .hit(.with_h(br, secondary_carbonates_pct = c(0, 10, NA)), "V/122")
  .hit(.with_h(br, redoximorphic_features_pct = c(0, 2, NA)), "V/123")
  .hit(.with_h(br, slickensides = c(NA, "few", NA)), "V/124")
  .hit(.with_h(br, caso4_pct = c(10, 10, NA)), "V/321")
  .hit(.with_h(br, caso4_pct = c(10, 10, NA), gypsum_visible_pct = c(0, 10, NA)), "V/322")
  .hit(.with_h(c1, caco3_pct = c(0, NA), effervescence_class = c("none", NA), oc_pct = c(15, NA)), "V/221")
  noc <- .with_h(c1, caco3_pct = c(0, NA), effervescence_class = c("none", NA))
  .hit(.with_h(noc, structure_type = c("angular blocky", NA), structure_size = c("fine", NA)), "V/231")
  mel <- .fr(list(designation = c("A1", "Cca", "R"), top_cm = c(0, 25, 40), bottom_cm = c(25, 40, 60),
                  oc_pct = c(5, 0.3, NA), n_total_pct = c(0.45, 0.03, NA), clay_pct = c(35, 20, NA), silt_pct = c(40, 40, NA),
                  sand_pct = c(25, 40, NA), caco3_pct = c(0, 40, NA), secondary_carbonates_pct = c(0, 30, NA),
                  effervescence_class = c("none", "strong", NA), ph_h2o = c(8.0, 8.4, NA), cec_cmol = c(40, 10, NA),
                  ca_cmol = c(38, 9, NA), mg_cmol = c(1, 0.5, NA), na_cmol = c(0.2, 0.1, NA), bs_pct = c(100, 100, NA),
                  rock_origin = c(NA, NA, "hard limestone"), munsell_hue_moist = c("10YR", "10YR", NA),
                  munsell_value_moist = c(2, 7, NA), munsell_chroma_moist = c(2, 2, NA), redoximorphic_features_pct = c(0, 0, NA),
                  structure_type = c("angular blocky", "massive", NA), structure_size = c("fine", NA, NA)),
             list(parent_material = "limestone"))
  .hit(mel, "V/232")
  .hit(noc, "V/212")
  .hit(.with_h(noc, oc_pct = c(6.5, NA)), "V/211")
  .hit(.with_h(noc, top_cm = c(0, 60), bottom_cm = c(60, 90)), "V/213")
})

test_that("class VI", {
  skip_on_cran()
  iso <- make_cpcs1967_example("isohumic")
  tex <- list(clay_pct = c(25, 28, 38, 30))
  prism <- list(structure_type = c(NA, NA, "prismatic", NA), structure_size = c(NA, NA, "coarse", NA))
  hyd <- list(redoximorphic_features_pct = c(0, 0, 2, 0))
  crust <- list(secondary_carbonates_pct = c(0, 0, 0, 10))
  salt <- list(ec_dS_m = c(0.5, 0.5, 5, 1))
  alk <- list(na_cmol = c(0.3, 0.3, 2.5, 0.3))
  red <- list(munsell_hue_moist = c("10YR", "10YR", "5YR", "10YR"), munsell_chroma_moist = c(1, 2, 4, 3))
  w <- function(p, a) do.call(.with_h, c(list(p), a))
  # VI/11
  b1 <- .with_site(iso, koppen_climate = "Cfb")
  .hit(b1, "VI/111")
  .hit(w(w(b1, tex), alk), "VI/116")
  .hit(w(b1, tex), "VI/112")
  .hit(w(b1, hyd), "VI/113")
  .hit(w(b1, prism), "VI/114")
  .hit(w(b1, crust), "VI/115")
  # VI/21
  .hit(iso, "VI/212")
  .hit(.with_h(iso, oc_pct = c(6, 4.5, 2.5, 0.3)), "VI/211")
  .hit(w(iso, tex), "VI/213")
  # VI/22, VI/23
  for (g in list(c("VI/22", 2, 1.5), c("VI/23", 1.2, 1))) {
    p <- .with_h(iso, oc_pct = c(as.numeric(g[2]), as.numeric(g[3]), 0.8, 0.3))
    expect_identical(.key(p)$levels$code[3], g[1])
    x <- sub("VI/", "", g[1])
    .hit(p, paste0("VI/", x, "1"))
    .hit(w(p, prism), paste0("VI/", x, if (x == "22") "2" else "3"))
    .hit(w(p, hyd), paste0("VI/", x, if (x == "22") "3" else "2"))
    .hit(w(p, crust), paste0("VI/", x, "4"))
    .hit(w(p, salt), paste0("VI/", x, "5"))
    .hit(w(p, alk), paste0("VI/", x, "5"))
  }
  .hit(.with_h(.with_h(iso, oc_pct = c(2, 1.5, 0.8, 0.3)), ferric_nodules_pct = c(0, 0, 6, 0)), "VI/223")
  # VI/31
  med <- .with_site(iso, koppen_climate = "Csa")
  .hit(med, "VI/311")
  .hit(w(med, red), "VI/312")
  .hit(w(med, tex), "VI/313")
  .hit(.with_h(med, cementation_class = c(NA, NA, NA, "strongly cemented")), "VI/314")
  # an accumulation that is not cemented is no croute
  .hit(w(med, crust), "VI/311")
  .hit(w(med, hyd), "VI/315")
  .hit(w(med, prism), "VI/316")
  .hit(w(med, salt), "VI/317")
  # VI/32
  sz <- .with_h(med, oc_pct = c(0.5, 0.45, 0.4, 0.2))
  .hit(sz, "VI/321")
  .hit(.with_h(sz, pan_type = c(NA, NA, NA, "petrogypsic")), "VI/322")
  .hit(w(sz, hyd), "VI/323")
  .hit(w(sz, prism), "VI/324")
  .hit(w(sz, salt), "VI/325")
  # vertic characters above 50 cm are not "en profondeur"
  .hit(.with_h(sz, structure_type = c(NA, "prismatic", NA, NA), structure_size = c(NA, "coarse", NA, NA),
               designation = c("A1", "(B)1", "(B)2", "Cca")), "VI/321")
  # VI/41
  sa <- .with_site(iso, koppen_climate = "BSh")
  .hit(sa, "VI/411")
  .hit(w(sa, red), "VI/412")
  .hit(w(sa, hyd), "VI/413")
  .hit(w(sa, prism), "VI/414")
  .hit(w(sa, salt), "VI/415")
})

test_that("class VII", {
  skip_on_cran()
  b <- make_cpcs1967_example("brunified")
  .hit(b, "VII/112")
  .hit(.with_h(b, ph_h2o = c(6, 6, 6.2)), "VII/111")
  .hit(.with_h(b, al_ox_pct = c(1.5, 2.5, 1), fe_ox_pct = 0.5), "VII/113")
  .hit(.with_h(b, clay_pct = c(20, 24, 15)), "VII/114")
  # recorded erosion rules VII/114 out
  .hit(.with_site(.with_h(b, clay_pct = c(20, 24, 15)), landform = "eroded hillslope"), "VII/112")
  rk <- .with_h(b, designation = c("A1", "(B)", "R"), top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 100))
  .hit(rk, "VII/115")
  # a calcareous rock is not the rock of a brown ranker
  .hit(.with_h(rk, rock_origin = c(NA, NA, "hard limestone")), "VII/112")
  # VII/12
  .hit(.with_h(b, clay_pct = c(15, 30, 20), sand_pct = c(45, 30, 40)), "VII/121")
  le <- .with_h(b, clay_pct = c(10, 30, 20), sand_pct = c(50, 30, 40))
  .hit(le, "VII/123")
  .hit(.with_h(le, ph_h2o = c(6, 6, 6.2)), "VII/122")
  .hit(.with_h(le, redoximorphic_features_pct = c(0, 3, 0)), "VII/125")
  ab <- .fr(list(designation = c("A1", "A2", "Bt", "C"), top_cm = c(0, 30, 45, 90), bottom_cm = c(30, 45, 90, 130),
                 oc_pct = c(3, 1, 0.5, 0.2), n_total_pct = c(0.25, 0.08, 0.05, 0.02), clay_pct = c(15, 12, 30, 25),
                 silt_pct = 50, sand_pct = c(35, 38, 20, 25), ph_h2o = c(5.8, 5.6, 6, 6.2), cec_cmol = c(15, 10, 18, 15),
                 bs_pct = 40, ca_cmol = c(4, 3, 5, 4), mg_cmol = 1, na_cmol = 0.1,
                 munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 4, 5), munsell_chroma_moist = c(2, 2, 4, 4),
                 munsell_value_dry = c(4, 6, 5, 6), munsell_chroma_dry = c(2, 2, 4, 4), redoximorphic_features_pct = 0))
  .hit(ab, "VII/122")
  .hit(.with_h(ab, boundary_topography = c(NA, "tongued", NA, NA)), "VII/126")
  ash <- .with_h(ab, munsell_hue_moist = c("10YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 6, 5, 5),
                 munsell_chroma_moist = c(2, 1, 6, 4), munsell_value_dry = c(4, 7, 6, 6), munsell_chroma_dry = c(2, 1, 6, 4))
  .hit(ash, "VII/124")
  # without clay a B of the CPCS notation is textural, and the index is undecided
  r <- .key(.with_h(b, designation = c("A1", "B2", "C"), clay_pct = NA_real_))
  expect_identical(r$levels$code[4], "VII/123"); expect_identical(.sgp(r), "subgroups:VII/121")
  # VII/41
  tr <- .with_h(.with_site(b, koppen_climate = "Aw"), bs_pct = 70, ph_h2o = 6.5, munsell_chroma_moist = c(2, 3, 3))
  .stop_at(tr, "VII/41")
  .hit(.with_h(tr, redoximorphic_features_pct = c(0, 3, 0)), "VII/413")
  .hit(.with_h(tr, redoximorphic_features_pct = c(0, 3, 0), clay_pct = c(30, 35, 30), slickensides = c(NA, "few", NA)),
       "VII/412")
  .hit(.with_h(tr, bs_pct = 50, ferric_nodules_pct = c(0, 6, 0)), "VII/414")
  .hit(.with_h(tr, top_cm = c(0, 10, 40), bottom_cm = c(10, 40, 100)), "VII/411")
})

test_that("class VIII", {
  skip_on_cran()
  p <- make_cpcs1967_example("podzolised")
  .hit(p, "VIII/113")
  .hit(.with_h(p, designation = c("Ao", "A1", "A2", "B21h", "B22h", "C")), "VIII/111")
  .hit(.with_h(p, designation = c("Ao", "A1", "A2", "B21fe", "B22fe", "C")), "VIII/112")
  .hit(.with_h(p, designation = c("Ao", "A1", "A2", "B2hfe", "B3", "C")), "VIII/113")
  .hit(.with_h(p, redoximorphic_features_pct = c(NA, 0, 0, 0, 0, 10)), "VIII/115")
  # without symbols, the moist colour gives the kind of B
  r <- .key(.with_h(p, designation = c("Ao", "A1", "A2", "B21", "B22", "C")))
  expect_identical(r$levels$code[4], "VIII/113")
  hc <- .fr(list(designation = c("Ao", "A1", "A2", "C"), top_cm = c(0, 5, 12, 40), bottom_cm = c(5, 12, 40, 100),
                 oc_pct = c(35, 5, 0.5, 0.2), n_total_pct = c(1.2, 0.2, 0.03, 0.02), clay_pct = c(NA, 5, 4, 4),
                 silt_pct = c(NA, 10, 8, 6), sand_pct = c(NA, 85, 88, 90), ph_h2o = 4.2, cec_cmol = c(NA, 8, 2, 2), bs_pct = 10,
                 munsell_hue_moist = c("7.5YR", "10YR", "10YR", "10YR"), munsell_value_moist = c(2, 3, 6, 5),
                 munsell_chroma_moist = c(1, 1, 1, 4), munsell_value_dry = c(NA, 4, 7, 6), munsell_chroma_dry = c(NA, 1, 1, 3),
                 redoximorphic_features_pct = c(NA, 0, 0, 0)), list(parent_material = "sandstone"))
  .hit(hc, "VIII/114")
  pale <- .with_h(p, munsell_value_moist = c(2, 3, 5, 2, 4, 6), munsell_chroma_moist = c(1, 1, 3, 2, 6, 4),
                  munsell_value_dry = c(NA, 4, 6, 3, 5, 7), munsell_chroma_dry = c(NA, 1, 3, 2, 6, 3))
  .hit(pale, "VIII/121")
  .hit(.with_h(pale, redoximorphic_features_pct = c(NA, 0, 0, 0, 0, 10)), "VIII/122")
  # an A2 over a Bg with no podzolic B under it (p. 51)
  .hit(.with_h(pale, designation = c("Ao", "A1", "A2", "B2g", "BCg", "C")), "VIII/123")
  expect_false(identical(.key(.with_h(pale, designation = c("Ao", "A1", "A2", "B2g", "B2fe", "C")))$levels$code[4], "VIII/123"))
  .hit(.with_h(pale, designation = c("Ao", "A1", "A2", "g", "B2fe", "C"), oc_pct = c(35, 5, 0.5, 0.4, 1.2, 0.2),
               n_total_pct = c(1.2, 0.2, 0.03, 0.03, 0.06, 0.02)), "VIII/124")
  ap <- .fr(list(designation = c("Ap", "B2h", "B2fe", "C"), top_cm = c(0, 20, 30, 55), bottom_cm = c(20, 30, 55, 100),
                 oc_pct = c(4, 3, 1.2, 0.2), n_total_pct = c(0.2, 0.15, 0.06, 0.02), clay_pct = c(6, 6, 6, 4),
                 silt_pct = c(10, 9, 9, 6), sand_pct = c(84, 85, 85, 90), ph_h2o = 4.5, cec_cmol = c(8, 12, 6, 2), bs_pct = 15,
                 munsell_hue_moist = c("10YR", "5YR", "7.5YR", "10YR"), munsell_value_moist = c(3, 2, 4, 6),
                 munsell_chroma_moist = c(2, 2, 6, 4), redoximorphic_features_pct = 0), list(parent_material = "sandstone"))
  .hit(ap, "VIII/125")
  oc <- .fr(list(designation = c("Ao", "A1", "B2fe", "C"), top_cm = c(0, 5, 15, 60), bottom_cm = c(5, 15, 60, 100),
                 oc_pct = c(35, 5, 1.2, 0.2), n_total_pct = c(1.2, 0.2, 0.06, 0.02), clay_pct = c(NA, 8, 8, 5),
                 silt_pct = c(NA, 12, 12, 8), sand_pct = c(NA, 80, 80, 87), ph_h2o = 4.4, cec_cmol = c(NA, 10, 8, 3),
                 bs_pct = 10, munsell_hue_moist = c("7.5YR", "10YR", "7.5YR", "10YR"), munsell_value_moist = c(2, 3, 5, 6),
                 munsell_chroma_moist = c(1, 2, 6, 4), redoximorphic_features_pct = c(NA, 0, 0, 0)),
            list(parent_material = "granite"))
  .hit(oc, "VIII/131")
  .hit(.with_h(oc, redoximorphic_features_pct = c(NA, 0, 0, 10)), "VIII/132")
  .hit(.with_h(oc, munsell_chroma_moist = c(1, 2, 3, 4), oc_pct = c(35, 5, 1.5, 0.2)), "VIII/142")
  .hit(.with_h(oc, top_cm = c(0, 5, 45, 80), bottom_cm = c(5, 45, 80, 120), oc_pct = c(35, 7, 1, 0.2),
               munsell_chroma_moist = c(1, 2, 3, 4)), "VIII/141")
  hy <- .with_h(p, redoximorphic_features_pct = c(NA, 0, 15, 0, 0, 0))
  .stop_at(hy, "VIII/31")
  .hit(.with_h(hy, designation = c("Ao", "A1", "A2", "B2h", "G", "C"), munsell_hue_moist = c("7.5YR", "10YR", "10YR", "5YR", "5GY", "10YR"),
               munsell_value_moist = c(2, 3, 6, 2, 5, 6), munsell_chroma_moist = c(1, 1, 1, 2, 1, 4)), "VIII/311")
  .hit(.with_h(hy, ferric_nodules_pct = c(NA, 0, 0, 0, 10, 0)), "VIII/312")
})

test_that("class IX", {
  skip_on_cran()
  s <- make_cpcs1967_example("sesquioxide")
  .hit(s, "IX/221")
  .hit(.with_h(s, clay_pct = c(15, 40, 42, 35), redoximorphic_features_pct = c(0, 20, 20, 0)), "IX/225")
  .hit(.with_h(s, redoximorphic_features_pct = c(0, 5, 0, 0)), "IX/222")
  # pronounced signs with a clay index of 2.0 or less: 222 takes any sign
  # (Hugo, 2026-11-01)
  .hit(.with_h(s, redoximorphic_features_pct = c(0, 20, 0, 0)), "IX/222")
  .hit(.with_h(s, slickensides = c(NA, "few", "few", NA)), "IX/223")
  .hit(.with_h(s, clay_pct = c(15, 40, 42, 35)), "IX/224")
  ca <- .with_h(s, caco3_pct = c(0, 0, 0, 5), top_cm = c(0, 20, 60, 90), bottom_cm = c(20, 60, 90, 140))
  .hit(ca, "IX/211")
  .hit(.with_h(ca, munsell_hue_moist = c("5YR", "7.5YR", "7.5YR", "5YR"), ferric_nodules_pct = c(0, 2, 2, 0)), "IX/212")
  .hit(.with_h(ca, redoximorphic_features_pct = c(0, 5, 0, 0)), "IX/213")
  .hit(.with_h(ca, slickensides = c(NA, "few", "few", NA)), "IX/214")
  .hit(.with_h(ca, caco3_pct = c(0, 2, 0, 5)), "IX/215")
  f <- .with_h(s, bs_pct = c(55, 55, 58, 60))
  .hit(f, "IX/121")
  .hit(.with_h(f, cementation_class = c(NA, NA, "strongly cemented", NA), ferric_nodules_pct = c(0, 10, 0, 0)), "IX/123")
  .hit(.with_h(f, ferric_nodules_pct = c(0, 10, 0, 0)), "IX/122")
  .hit(.with_h(f, redoximorphic_features_pct = c(0, 3, 0, 0)), "IX/124")
  .hit(.with_h(f, layer_origin = c("colluvium", NA, NA, NA)), "IX/125")
  ap <- .with_h(f, clay_pct = c(25, 40, 42, 40), clay_films_amount = c("none", "few", "few", "few"))
  .hit(ap, "IX/131")
  .hit(.with_h(ap, redoximorphic_features_pct = c(0, 3, 0, 0)), "IX/132")
  pl <- .with_h(f, clay_pct = c(35, 40, 42, 35))
  .hit(pl, "IX/111")
  .hit(.with_h(pl, redoximorphic_features_pct = c(0, 3, 0, 0)), "IX/112")
  .hit(.with_h(pl, slickensides = c(NA, NA, "few", NA), structure_size = c(NA, NA, "coarse", NA)), "IX/113")
  # a fine structure at depth is not the coarse one of IX/113
  .hit(.with_h(pl, slickensides = c(NA, NA, "few", NA), structure_size = c(NA, NA, "fine", NA)), "IX/111")
  # a protovertic B above 50 cm over a coarse prismatic B below it (IX/113)
  .hit(.with_h(pl, slickensides = c(NA, "few", NA, NA), structure_size = c(NA, NA, "coarse", NA),
               structure_type = c(NA, NA, "prismatic", NA)), "IX/113")
  sand <- .fr(list(designation = c("A1", "B21", "B22", "C"), top_cm = c(0, 20, 50, 100), bottom_cm = c(20, 50, 100, 140),
                   oc_pct = c(0.4, 0.2, 0.15, 0.1), n_total_pct = c(0.04, 0.02, 0.015, 0.01), clay_pct = c(5, 7, 8, 8),
                   silt_pct = c(5, 5, 5, 5), sand_pct = c(92, 88, 87, 89), cec_cmol = c(2, 3, 3, 2), bs_pct = c(70, 85, 85, 60),
                   ph_h2o = 6.5, munsell_hue_moist = c("7.5YR", "5YR", "5YR", "5YR"), munsell_value_moist = c(4, 5, 5, 5),
                   munsell_chroma_moist = c(3, 6, 6, 6), redoximorphic_features_pct = 0), list(lat = 13))
  # the nearly saturated tropical sand stays in IX-1 (Hugo, 2026-11-01)
  .hit(sand, "IX/114")
  expect_identical(.key(sand)$levels$code[2], "IX-1")
})

test_that("class X", {
  skip_on_cran()
  x <- make_cpcs1967_example("ferrallitic")
  .hit(x, "X/216")
  x0 <- .with_h(x, clay_pct = c(55, 55, 60, 60), sand_pct = c(35, 35, 30, 30), oc_pct = c(1.5, 0.8, 0.5, 0.3))
  yel <- list(munsell_hue_moist = "7.5YR")
  ind <- list(cementation_class = c(NA, NA, "strongly cemented", NA))
  hyd <- list(redoximorphic_features_pct = c(0, 3, 0, 0))
  raj <- list(sand_feldspar_pct = c(NA, 4, NA, NA))
  hum <- list(oc_pct = c(3, 1.5, 0.5, 0.3))
  app <- list(clay_pct = c(45, 55, 60, 60))
  rem <- list(coarse_sand_pct = c(40, 20, 15, 15), fine_sand_pct = c(20, 25, 15, 15))
  elu <- list(coarse_fragments_pct = c(0, 30, 5, 0))
  eol <- list(layer_origin = c("aeolian sand", NA, NA, NA))
  w <- function(p, a) do.call(.with_h, c(list(p), a))
  # X-2
  .hit(x0, "X/211")
  .hit(w(x0, yel), "X/212"); .hit(w(x0, ind), "X/213"); .hit(w(x0, hyd), "X/214"); .hit(w(x0, raj), "X/215")
  .hit(w(x0, app), "X/216"); .hit(w(x0, hum), "X/217")
  hf <- .with_h(x, oc_pct = c(5, 2, 1, 0.8), n_total_pct = c(0.45, 0.18, 0.09, 0.07))
  .hit(hf, "X/221")
  deep <- .with_h(hf, designation = c("A11", "A12", "B21", "B22"), top_cm = c(0, 30, 60, 100), bottom_cm = c(30, 60, 100, 110),
                  munsell_value_moist = 4)
  .hit(deep, "X/222")
  thin <- .with_h(hf, munsell_value_moist = 4)
  .stop_at(thin, "X/22")
  .hit(.with_h(thin, sand_feldspar_pct = c(NA, 12, NA, NA)), "X/223")
  .hit(.with_h(thin, volcanic_glass_pct = c(5, NA, NA, NA)), "X/223")
  im <- .with_h(x0, clay_pct = c(30, 45, 60, 60), sand_pct = c(60, 45, 30, 30))
  .hit(im, "X/231"); .hit(w(im, yel), "X/232"); .hit(w(im, ind), "X/233"); .hit(w(im, hyd), "X/234"); .hit(w(im, rem), "X/235")
  rw <- .with_h(x0, layer_origin = c("colluvium", NA, NA, NA))
  .hit(rw, "X/241"); .hit(w(rw, yel), "X/242"); .hit(w(rw, ind), "X/243"); .hit(w(rw, hyd), "X/244"); .hit(w(rw, raj), "X/245")
  .hit(w(x0, elu), "X/246")
  rj <- .with_h(x0, sand_feldspar_pct = c(NA, 12, NA, NA))
  .stop_at(rj, "X/25")
  .hit(w(rj, eol), "X/251"); .hit(w(rj, hyd), "X/252"); .hit(.with_site(rj, landform = "eroded hillslope"), "X/253")
  # X-1
  x1 <- .with_h(x0, bs_pct = 50)
  .hit(x1, "X/111"); .hit(w(x1, ind), "X/112"); .hit(w(x1, hyd), "X/113"); .hit(w(x1, raj), "X/114"); .hit(w(x1, hum), "X/115")
  i1 <- .with_h(x1, clay_pct = c(30, 45, 60, 60), sand_pct = c(60, 45, 30, 30))
  .hit(i1, "X/121"); .hit(w(i1, ind), "X/122"); .hit(w(i1, hyd), "X/123"); .hit(w(i1, rem), "X/124")
  r1 <- .with_h(x1, designation = c("A1", "B1", "IIB21", "IIB22"))
  .hit(r1, "X/131"); .hit(w(r1, ind), "X/132"); .hit(w(r1, hyd), "X/133"); .hit(w(r1, raj), "X/134"); .hit(w(r1, elu), "X/135")
  j1 <- .with_h(x1, sand_mineralogy = c(NA, "micacea", NA, NA))
  .stop_at(j1, "X/14")
  .hit(w(j1, eol), "X/141"); .hit(w(j1, hyd), "X/142"); .hit(.with_site(j1, landform = "eroded hillslope"), "X/143")
  # X-3
  x3 <- .with_h(x0, bs_pct = 10)
  .hit(x3, "X/311"); .hit(w(x3, yel), "X/312"); .hit(w(x3, ind), "X/313"); .hit(w(x3, hyd), "X/314"); .hit(w(x3, raj), "X/315")
  .hit(w(x3, app), "X/316"); .hit(w(x3, hum), "X/317")
  h3 <- .with_h(x3, oc_pct = c(2, 1.5, 1.2, 1.1), n_total_pct = c(0.18, 0.13, 0.11, 0.1))
  .hit(h3, "X/321")
  gb <- .with_h(h3, munsell_hue_moist = "7.5YR", munsell_value_moist = 3, munsell_chroma_moist = c(2, 3, 3, 3), ph_h2o = 4.2)
  .hit(.with_h(gb, gibbsite_pct = c(NA, 30, 30, 30)), "X/322")
  .hit(.with_h(gb, gibbsite_pct = c(NA, 10, 10, 10)), "X/321")
  # with no gibbsite measured, Ki under 1.0 stands in
  .hit(.with_h(gb, sio2_sulfuric_pct = c(NA, 8, 8, 8)), "X/322")
  le <- .with_h(x3, clay_pct = c(30, 45, 60, 60), sand_pct = c(60, 45, 30, 30))
  .hit(le, "X/361")
  .hit(.with_h(le, designation = c("A1", "A2", "B21", "B22"), munsell_hue_moist = c("10YR", "10YR", "2.5YR", "2.5YR"),
               munsell_value_moist = c(3, 6, 4, 4), munsell_chroma_moist = c(2, 1, 8, 8), munsell_value_dry = c(4, 7, 5, 5),
               munsell_chroma_dry = c(2, 1, 8, 8)), "X/362")
  .hit(w(le, ind), "X/363"); .hit(w(le, hyd), "X/364")
  a3 <- .with_h(le, clay_films_amount = c(NA, "few", "few", "few"))
  .hit(a3, "X/331"); .hit(w(a3, yel), "X/332"); .hit(w(a3, ind), "X/333"); .hit(w(a3, hyd), "X/334"); .hit(w(a3, rem), "X/335")
  m3 <- .with_h(x3, layer_origin = c("colluvium", NA, NA, NA))
  .hit(m3, "X/341"); .hit(w(m3, yel), "X/342"); .hit(w(m3, hyd), "X/343"); .hit(w(m3, raj), "X/344")
  j3 <- .with_h(x3, sand_amphibole_pct = c(NA, 15, NA, NA))
  .hit(w(j3, eol), "X/351"); .hit(w(j3, hyd), "X/352"); .hit(.with_site(j3, landform = "eroded hillslope"), "X/353")
})

test_that("class XI", {
  skip_on_cran()
  pt <- .fr(list(designation = c("H1", "H2", "C"), top_cm = c(0, 40, 90), bottom_cm = c(40, 90, 130),
                 oc_pct = c(40, 35, 0.5), clay_pct = c(NA, NA, 30), ph_h2o = 4.5, munsell_hue_moist = "10YR",
                 munsell_value_moist = c(2, 2, 5), munsell_chroma_moist = c(1, 1, 2), redoximorphic_features_pct = c(NA, NA, 0)),
            list(drainage_class = "very poorly drained"))
  for (fb in list(list("XI/11", fiber_content_unrubbed_pct = c(80, 75, NA)), list("XI/12", von_post_index = c("H5", "H4", NA)),
                  list("XI/13", fiber_content_rubbed_pct = c(10, 12, NA)))) {
    p <- do.call(.with_h, c(list(pt), fb[-1]))
    x <- sub("XI/", "", fb[[1]])
    .hit(p, paste0("XI/", x, "1"))
    .hit(.with_h(p, ph_h2o = 6), paste0("XI/", x, "2"))
  }
  p <- .with_h(pt, fiber_content_rubbed_pct = c(10, 12, NA), ph_h2o = NA_real_)
  .stop_at(p, "XI/13", provisional = TRUE)
  .hit(.with_h(p, peat_moor_type = c("high-moor", "high-moor", NA)), "XI/131")
  .hit(.with_h(p, peat_moor_type = c("low-moor", "low-moor", NA)), "XI/132")
  h <- make_cpcs1967_example("hydromorphic")
  an <- .with_h(h, oc_pct = c(10, 0.5, 0.3), top_cm = c(0, 25, 60), bottom_cm = c(25, 60, 120))
  .hit(an, "XI/214")
  .hit(.with_h(an, ec_dS_m = c(5, 3, 2)), "XI/211")
  .hit(.with_h(an, n_total_pct = c(0.8, 0.05, 0.03)), "XI/212")
  .hit(.with_h(an, n_total_pct = c(0.8, 0.05, 0.03), ph_h2o = 7.5), "XI/213")
  .hit(.with_h(an, n_total_pct = c(0.8, 0.05, 0.03), caco3_pct = c(3, 0, 0)), "XI/213")
  # the humus form decides before the C/N
  .hit(.with_site(.with_h(an, n_total_pct = c(0.8, 0.05, 0.03)), humus_form = "hydromor"), "XI/214")
  .stop_at(.with_h(an, n_total_pct = NA_real_), "XI/21", provisional = TRUE)
  sg <- .with_h(an, designation = c("A1", "Bg", "Bg"), munsell_hue_moist = "10YR", munsell_chroma_moist = c(2, 4, 4),
                redoximorphic_features_pct = c(0, 30, 30))
  .stop_at(sg, "XI/22")
  .hit(.with_h(sg, designation = c("A1", "C1g", "C2g")), "XI/221")
  .hit(.with_h(h, secondary_carbonates_pct = c(0, 10, 0), pan_type = c(NA, "calcrete", NA)), "XI/361")
  .hit(.with_h(h, secondary_carbonates_pct = c(0, 10, 0), secondary_carbonates_form = c(NA, "soft concretions", NA)), "XI/362")
  .stop_at(.with_h(h, secondary_carbonates_pct = c(0, 10, 0)), "XI/36")
  .hit(h, "XI/311")
  gl <- .fr(list(designation = c("A1", "B2", "G"), top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 120),
                 oc_pct = c(3, 0.5, 0.3), n_total_pct = c(0.25, 0.05, 0.03), clay_pct = 35, silt_pct = 40, sand_pct = 25,
                 ph_h2o = 6.5, cec_cmol = 20, bs_pct = 70, ca_cmol = 11, mg_cmol = 2.5, na_cmol = 0.2,
                 munsell_hue_moist = c("10YR", "10YR", "5GY"), munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 2, 1),
                 redoximorphic_features_pct = c(0, 20, 5)),
            list(drainage_class = "poorly drained", groundwater_kind = "observed", groundwater_depth_cm = 70))
  .hit(gl, "XI/312")
  .hit(.with_h(gl, ec_dS_m = 5), "XI/313")
  .hit(.with_h(gl, designation = c("A1", "B2t", "G"), clay_pct = c(15, 30, 30), sand_pct = c(45, 30, 30)), "XI/314")
  pg <- .fr(list(designation = c("A1", "Bg1", "Bg2"), top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                 oc_pct = c(2.5, 0.5, 0.3), n_total_pct = c(0.2, 0.05, 0.03), clay_pct = 30, silt_pct = 40, sand_pct = 30,
                 ph_h2o = 6.2, cec_cmol = 18, bs_pct = 60, ca_cmol = 8, mg_cmol = 2, na_cmol = 0.2,
                 munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 5), munsell_chroma_moist = c(2, 4, 4),
                 redoximorphic_features_pct = c(0, 25, 30)), list(drainage_class = "imperfectly drained"))
  .stop_at(pg, "XI/32")
  .hit(.with_h(pg, redoximorphic_features_pct = c(5, 25, 30)), "XI/321")
  .hit(.with_site(pg, groundwater_kind = "perched"), "XI/322")
  .hit(.with_h(pg, clay_pct = c(20, 22, 40), sand_pct = c(40, 38, 20)), "XI/322")
  am <- .with_h(pg, designation = c("A1", "Bg", "G"), munsell_hue_moist = c("10YR", "10YR", "5GY"),
                munsell_chroma_moist = c(2, 4, 1), top_cm = c(0, 20, 90), bottom_cm = c(20, 90, 130))
  .hit(am, "XI/342")
  sep <- .fr(list(designation = c("A1", "Bg", "B", "G"), top_cm = c(0, 20, 70, 90), bottom_cm = c(20, 70, 90, 130),
                  oc_pct = c(2.5, 0.5, 0.4, 0.3), n_total_pct = c(0.2, 0.05, 0.04, 0.03), clay_pct = 30, silt_pct = 40,
                  sand_pct = 30, ph_h2o = 6.2, cec_cmol = 18, bs_pct = 60, ca_cmol = 8, mg_cmol = 2, na_cmol = 0.2,
                  munsell_hue_moist = c("10YR", "10YR", "10YR", "5GY"), munsell_value_moist = c(3, 5, 5, 5),
                  munsell_chroma_moist = c(2, 4, 4, 1), redoximorphic_features_pct = c(0, 30, 0, 2)),
             list(drainage_class = "imperfectly drained"))
  .hit(sep, "XI/341")
})

test_that("class XII", {
  skip_on_cran()
  sc <- make_cpcs1967_example("sodic")
  .hit(sc, "XII/112")
  .hit(.with_h(sc, salt_crust_pattern = c("efflorescent", NA, NA)), "XII/111")
  firm <- .with_h(sc, consistence_moist = "firm")
  .stop_at(firm, "XII/11")
  .hit(.with_h(firm, salt_crust_pattern = c("crusty", NA, NA)), "XII/113")
  .hit(.with_h(firm, ph_h2o = c(3.5, 8, 8), jarosite_present = c(TRUE, NA, NA)), "XII/114")
  so <- .fr(list(designation = c("A1", "Bt", "C"), top_cm = c(0, 15, 60), bottom_cm = c(15, 60, 100),
                 oc_pct = c(1, 0.5, 0.2), n_total_pct = c(0.1, 0.05, 0.02), clay_pct = c(15, 35, 30), silt_pct = 40,
                 sand_pct = c(45, 25, 30), ec_dS_m = c(1, 2, 3), na_cmol = c(0.5, 4, 3), cec_cmol = 15, bs_pct = 100,
                 ph_h2o = c(7.5, 8.8, 8.6), structure_type = c("granular", "columnar", "massive"),
                 consistence_moist = c("friable", "very firm", "very firm"), munsell_hue_moist = "10YR",
                 munsell_value_moist = c(4, 3, 5), munsell_chroma_moist = c(2, 2, 3), redoximorphic_features_pct = 0))
  .hit(so, "XII/221")
  .hit(.with_h(so, structure_type = c("granular", "prismatic", "massive")), "XII/222")
  .stop_at(.with_h(so, structure_type = c("granular", "angular blocky", "massive")), "XII/22")
  sab <- .with_h(so, designation = c("A1", "(B)", "C"), clay_pct = 30, sand_pct = 30)
  .hit(sab, "XII/212")
  .hit(.with_h(sab, structure_type = c("single grain", "columnar", "massive")), "XII/211")
  .hit(.with_h(sab, ec_dS_m = c(9, 8, 3), structure_type = c(NA, "columnar", "massive")), "XII/211")
  sd <- .fr(list(designation = c("A1", "A2", "Bt", "C"), top_cm = c(0, 10, 25, 60), bottom_cm = c(10, 25, 60, 100),
                 oc_pct = c(1.5, 0.3, 0.5, 0.2), n_total_pct = c(0.12, 0.03, 0.05, 0.02), clay_pct = c(12, 10, 35, 30),
                 silt_pct = 40, sand_pct = c(48, 50, 25, 30), ec_dS_m = c(0.5, 1, 2, 3), na_cmol = c(0.3, 0.4, 4, 3),
                 cec_cmol = 15, bs_pct = c(60, 60, 100, 100), ph_h2o = c(6, 6.2, 8.8, 8.6),
                 structure_type = c("granular", "platy", "columnar", "massive"),
                 consistence_moist = c("friable", "friable", "very firm", "very firm"), munsell_hue_moist = "10YR",
                 munsell_value_moist = c(4, 6, 3, 5), munsell_chroma_moist = c(2, 2, 2, 3),
                 munsell_value_dry = c(5, 7, 4, 6), munsell_chroma_dry = c(2, 2, 2, 3), redoximorphic_features_pct = 0))
  .hit(sd, "XII/231")
  .hit(.with_h(sd, na_cmol = c(0.3, 0.4, 1.2, 3)), "XII/232")
})

test_that("intergrades in the trace, and the hydromorphic facies of the brown soils", {
  skip_on_cran()
  # a sol brun calcaire with a crust and a pseudogley: the crust is the first
  br <- .fr(list(designation = c("A1", "(B)", "R"), top_cm = c(0, 20, 50), bottom_cm = c(20, 50, 80),
                 oc_pct = c(3, 1, NA), n_total_pct = c(0.3, 0.1, NA), clay_pct = c(30, 32, NA), silt_pct = c(40, 40, NA),
                 sand_pct = c(30, 28, NA), caco3_pct = c(15, 20, NA), ph_h2o = c(8, 8.1, NA), cec_cmol = c(30, 28, NA),
                 ca_cmol = c(28, 26, NA), mg_cmol = c(1, 1, NA), na_cmol = c(0.2, 0.2, NA), bs_pct = c(100, 100, NA),
                 munsell_hue_moist = c("10YR", "10YR", NA), munsell_value_moist = c(3, 4, NA), munsell_chroma_moist = c(2, 3, NA),
                 redoximorphic_features_pct = c(0, 2, NA), secondary_carbonates_pct = c(0, 10, NA),
                 structure_type = c("granular", "subangular blocky", NA)), list(parent_material = "limestone"))
  r <- .key(br)
  expect_identical(r$levels$code[4], "V/122")
  expect_match(paste(r$trace$subgroups[["V/122"]]$notes, collapse = " "), "intergrade vers Sols bruns calcaires a pseudogley|intergrade vers Sols bruns calcaires \u00e0 pseudogley")
  b <- make_cpcs1967_example("brunified")
  r <- .key(.with_h(b, redoximorphic_features_pct = c(0, 0, 10)))
  expect_identical(r$levels$code[4], "VII/112")
  expect_match(paste(r$trace$subgroups[["VII/112"]]$notes, collapse = " "), "faci\u00e8s hydromorphe")
  r <- .key(b)
  expect_false(any(grepl("faci\u00e8s", unlist(r$trace$subgroups[["VII/112"]]$notes))))
})

test_that("every subgroup is reached", {
  skip_on_cran()
  rules <- load_rules("cpcs1967")
  all <- unlist(lapply(rules$subgroups, function(l) vapply(l, function(t) t$code, "")), use.names = FALSE)
  expect_length(all, 276L)
  expect_identical(sort(setdiff(all, ls(.reached))), character(0))
})

test_that("the fixes of the logic review", {
  skip_on_cran()
  # a negated mottle record is no sign of hydromorphy
  f <- .with_h(make_cpcs1967_example("ferrallitic"), redoximorphic_features_pct = NA_real_, mottle_colors = "none observed")
  expect_identical(.key(f)$levels$code[4], "X/216")
  f2 <- .with_h(make_cpcs1967_example("ferrallitic"), redoximorphic_features_pct = NA_real_, mottle_colors = "no mottles")
  expect_identical(.key(f2)$levels$code[4], "X/216")
  # the stop at the group is warned once
  r <- classify_cpcs1967(make_asc3_example("hydrosols"), on_missing = "silent")
  if (nrow(r$levels) == 3L && !any(grepl("^subgroups:", r$provisional_because))) {
    expect_length(grep("Sous-groupe non d\u00e9fini", r$warnings), 1L)
    expect_length(grep("^No class of the subgroups list", r$warnings), 0L)
  }
  # an undesignated profile with no rock: the depth of the solum is not known
  x <- soilKey:::.cpcs_profile(.with_h(make_cpcs1967_example("brunified"), designation = NA_character_))
  expect_true(is.na(solum_depth_cpcs(.with_h(make_cpcs1967_example("brunified"), designation = NA_character_), at_most = 50)$passed))
  # a gypsum crust is not a calcareous crust
  g <- .with_h(make_cpcs1967_example("calcimagnesic"), pan_type = c(NA, "gypsum crust"))
  expect_false(any(soilKey:::.cpcs_calc_crust_rows(soilKey:::.cpcs_profile(g))))
})

