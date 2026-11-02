# v0.9.331: the 35 sub-classes of the French soil classification (CPCS 1967).
# Made-up profiles; no French data.

.fr <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "FR", designation_system = "cpcs", drainage_class = "well drained")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.sub <- function(p) {
  r <- classify_cpcs1967(p, on_missing = "silent")
  c(code = if (nrow(r$levels) >= 2) r$levels$code[2] else NA_character_,
    prov = paste(r$provisional_because, collapse = ","))
}
.with_site <- function(p, ...) { q <- p$clone(deep = TRUE); s <- q$site; s[names(list(...))] <- list(...); q$site <- s; q }

test_that("every example keys to its sub-class, decided, with the French names", {
  skip_on_cran()
  want <- c(raw_mineral = "I-1", weakly_developed = "II-4", vertisol = "III-1", andosol = "IV-1",
            calcimagnesic = "V-1", isohumic = "VI-2", brunified = "VII-1", podzolised = "VIII-1",
            sesquioxide = "IX-2", ferrallitic = "X-2", hydromorphic = "XI-3", sodic = "XII-1")
  for (g in names(want)) {
    r <- classify_cpcs1967(make_cpcs1967_example(g), on_missing = "silent")
    expect_identical(r$levels$code[2], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
  }
  r <- classify_cpcs1967(make_cpcs1967_example("ferrallitic"), on_missing = "silent")
  expect_identical(r$name, "Sols ferrallitiques moyennement d\u00e9satur\u00e9s en (B)")
  rules <- load_rules("cpcs1967")
  expect_identical(sum(lengths(rules$subclasses)), 35L)
  expect_length(soilKey:::.validate_rules("cpcs1967")$missing, 0)
})

test_that("the climate: Koppen first, then the regimes, then the latitude; none gives the default", {
  skip_on_cran()
  x <- function(...) soilKey:::.cpcs_profile(.with_site(make_cpcs1967_example("brunified"), ...))
  expect_true(soilKey:::.cpcs_climate(x(koppen_climate = "Dfc"), "boreal"))
  expect_false(soilKey:::.cpcs_climate(x(koppen_climate = "Cfb", soil_temperature_regime = "cryic"), "boreal"))
  expect_true(soilKey:::.cpcs_climate(x(soil_temperature_regime = "cryic"), "boreal"))
  expect_true(soilKey:::.cpcs_climate(x(lat = -8), "tropical"))
  expect_false(soilKey:::.cpcs_climate(x(), "tropical"))
  expect_true(is.na(soilKey:::.cpcs_climate(x(), "vi2", none = NA)))
  # an isohumic soil with no climate and a saturated A has no sub-class
  iso <- make_cpcs1967_example("isohumic"); s <- iso$site; s$koppen_climate <- NULL; iso$site <- s
  r <- .sub(iso)
  expect_true(is.na(r[["code"]])); expect_match(r[["prov"]], "subclasses:VI-1")
  expect_identical(.sub(.with_site(iso, koppen_climate = "Csa"))[["code"]], "VI-3")
  expect_identical(.sub(.with_site(iso, koppen_climate = "BSh"))[["code"]], "VI-4")
  expect_identical(.sub(.with_site(iso, soil_moisture_regime = "udic"))[["code"]], "VI-1")
  # the VI-1 property test: a partly desaturated A over a B at 80 % or more
  d <- iso$clone(deep = TRUE); d$horizons$bs_pct <- c(65, 70, 90, 100); d$horizons$caco3_pct <- c(0, 0, 0, 12)
  expect_identical(.sub(d)[["code"]], "VI-1")
})

test_that("classes I to IV", {
  skip_on_cran()
  raw <- make_cpcs1967_example("raw_mineral")
  expect_identical(.sub(.with_site(raw, koppen_climate = "ET"))[["code"]], "I-2")
  expect_identical(.sub(.with_site(raw, soil_moisture_regime = "aridic"))[["code"]], "I-3")
  # a humic ranker: 8.6 % OM, pH 5.5, on granite
  rk <- .fr(list(designation = c("A1", "R"), top_cm = c(0, 25), bottom_cm = c(25, 60), oc_pct = c(5, NA),
                 ph_h2o = c(5.5, NA), bs_pct = c(30, NA), clay_pct = c(15, NA), silt_pct = c(30, NA), sand_pct = c(55, NA),
                 rock_origin = c(NA, "granite")), list(parent_material = "granite"))
  expect_identical(.sub(rk)[["code"]], "II-2")
  expect_identical(.sub(.with_site(rk, soil_moisture_regime = "aridic"))[["code"]], "II-4")
  # a xeric soil poor in OM, then with a calcareous accumulation
  xe <- .fr(list(designation = c("A1", "C1", "C2"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 100),
                 oc_pct = c(0.6, 0.3, 0.2), ph_h2o = 7.6, bs_pct = 100, clay_pct = 12, silt_pct = 30, sand_pct = 58,
                 layer_origin = "colluvium"), list(soil_moisture_regime = "aridic"))
  expect_identical(.sub(xe)[["code"]], "II-3")
  xe$horizons$designation <- c("A1", "C1", "Cca")
  expect_identical(.sub(xe)[["code"]], "II-4")
  # the vertisols: slope and landform, else undecided
  v <- make_cpcs1967_example("vertisol")
  expect_identical(.sub(.with_site(v, slope_pct = 6))[["code"]], "III-2")
  s <- v$site; s$slope_pct <- NULL; s$landform <- NULL; v$site <- s
  r <- .sub(v); expect_identical(r[["code"]], "III-2"); expect_match(r[["prov"]], "subclasses:III-1")
  # the andosols of tropical countries: less organic matter
  a <- make_cpcs1967_example("andosol"); a$horizons$oc_pct <- c(4, 2, 1)
  expect_identical(.sub(a)[["code"]], "IV-2")
})

test_that("classes V, VII and VIII", {
  skip_on_cran()
  rz <- make_cpcs1967_example("calcimagnesic")
  g <- rz$clone(deep = TRUE); g$horizons$caso4_pct <- c(12, NA)
  expect_identical(.sub(g)[["code"]], "V-3")
  sat <- rz$clone(deep = TRUE); sat$horizons$caco3_pct <- c(0, NA); sat$horizons$ph_h2o <- c(7.2, NA)
  expect_identical(.sub(sat)[["code"]], "V-2")
  b <- make_cpcs1967_example("brunified")
  # tropical, with a eutrophic (B)
  # (a 10YR 5/6 B with V above 50 % would be a ferruginous tropical soil, IX)
  t <- .with_site(b, lat = -6); t$horizons$bs_pct <- c(70, 65, 60); t$horizons$munsell_chroma_moist <- c(2, 3, 4)
  expect_identical(.sub(t)[["code"]], "VII-4")
  # tropical, but the (B) not eutrophic: not named temperate, undecided
  r <- .sub(.with_site(b, lat = -6))
  expect_true(is.na(r[["code"]])); expect_match(r[["prov"]], "subclasses:VII-1")
  expect_identical(.sub(.with_site(b, koppen_climate = "Cfb"))[["code"]], "VII-1")
  # an A2 over a textural B, boreal or continental
  le <- .fr(list(designation = c("A1", "A2", "B2t", "C"), top_cm = c(0, 10, 30, 80), bottom_cm = c(10, 30, 80, 120),
                 oc_pct = c(3, 0.8, 0.4, 0.2), n_total_pct = c(0.25, 0.07, 0.04, 0.02), clay_pct = c(15, 14, 32, 25),
                 silt_pct = 40, sand_pct = c(45, 46, 28, 35), ph_h2o = 6.2, bs_pct = c(60, 55, 70, 75),
                 munsell_hue_moist = "10YR", munsell_value_moist = c(3, 5, 4, 5), munsell_chroma_moist = c(2, 3, 4, 3),
                 munsell_value_dry = c(4, 6, 5, 6), munsell_chroma_dry = c(2, 3, 4, 3), redoximorphic_features_pct = 0))
  expect_identical(.sub(.with_site(le, koppen_climate = "Dfc"))[["code"]], "VII-3")
  expect_identical(.sub(.with_site(le, koppen_climate = "Dfb"))[["code"]], "VII-2")
  expect_identical(.sub(.with_site(le, koppen_climate = "Cfb"))[["code"]], "VII-1")
  # podzols: hydromorphy in the A2, a cold climate
  pz <- make_cpcs1967_example("podzolised")
  expect_identical(.sub(.with_site(pz, koppen_climate = "Dfc"))[["code"]], "VIII-2")
  w <- pz$clone(deep = TRUE); w$horizons$redoximorphic_features_pct <- c(NA, 0, 15, 5, 0, 0)
  expect_identical(.sub(w)[["code"]], "VIII-3")
})

test_that("classes IX to XII", {
  skip_on_cran()
  s <- make_cpcs1967_example("sesquioxide")
  lo <- s$clone(deep = TRUE); lo$horizons$bs_pct <- c(55, 55, 60, 70)
  expect_identical(.sub(lo)[["code"]], "IX-1")
  f <- make_cpcs1967_example("ferrallitic")
  hi <- f$clone(deep = TRUE); hi$horizons$bs_pct <- 50
  expect_identical(.sub(hi)[["code"]], "X-1")
  st <- f$clone(deep = TRUE); st$horizons$bs_pct <- 10
  expect_identical(.sub(st)[["code"]], "X-3")
  # without base saturation, the sum of the bases
  nb <- f$clone(deep = TRUE); nb$horizons$bs_pct <- NA_real_; nb$horizons$cec_cmol <- NA_real_
  nb$horizons$ca_cmol <- 0.3; nb$horizons$mg_cmol <- 0.1
  expect_identical(.sub(nb)[["code"]], "X-3")
  pt <- .fr(list(designation = c("H1", "H2", "G"), top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                 oc_pct = c(30, 25, 1), clay_pct = c(NA, NA, 30), munsell_hue_moist = c("10YR", "10YR", "5G"),
                 munsell_value_moist = c(2, 2, 5), munsell_chroma_moist = c(1, 1, 1)),
            list(drainage_class = "very poorly drained"))
  expect_identical(.sub(pt)[["code"]], "XI-1")
  an <- .fr(list(designation = c("A1", "G"), top_cm = c(0, 25), bottom_cm = c(25, 90), oc_pct = c(8, 0.5),
                 clay_pct = 30, munsell_hue_moist = c("10YR", "5GY"), munsell_value_moist = c(2, 5),
                 munsell_chroma_moist = c(1, 1)))
  expect_identical(.sub(an)[["code"]], "XI-2")
  so <- make_cpcs1967_example("sodic")
  dg <- so$clone(deep = TRUE); dg$horizons$structure_type <- c("granular", "columnar", "massive")
  dg$horizons$consistence_moist <- c("friable", "very firm", "firm")
  expect_identical(.sub(dg)[["code"]], "XII-2")
})

test_that("after the review: every Koppen code, the Koppen case, the regimes, the X mean", {
  skip_on_cran()
  iso <- make_cpcs1967_example("isohumic")
  want <- c(Cfb = "VI-1", Af = "VI-1", Dfb = "VI-2", BWk = "VI-2", ET = "VI-2", Csa = "VI-3",
            BWh = "VI-4", Cwa = "VI-4", Aw = "VI-4")
  for (k in names(want)) expect_identical(.sub(.with_site(iso, koppen_climate = k))[["code"]], want[[k]], info = k)
  expect_identical(.sub(.with_site(iso, koppen_climate = "dfb"))[["code"]], "VI-2")
  s <- iso$site; s$koppen_climate <- NULL; iso$site <- s
  expect_identical(.sub(.with_site(iso, soil_temperature_regime = "mesic", soil_moisture_regime = "ustic"))[["code"]], "VI-2")
  expect_true(is.na(soilKey:::.cpcs_koppen(list(koppen_climate = "temperate"))))
  # a recorded temperature regime decides the tropics before the latitude
  x <- soilKey:::.cpcs_profile(.with_site(make_cpcs1967_example("brunified"), soil_temperature_regime = "cryic", lat = 10))
  expect_false(soilKey:::.cpcs_climate(x, "tropical"))
  # class IX reads the Koppen tropics: a yellow eutrophic B under Aw
  y <- make_cpcs1967_example("sesquioxide"); y$horizons$munsell_hue_moist <- "7.5YR"; y$horizons$munsell_value_moist <- 5
  expect_identical(classify_cpcs1967(.with_site(y, koppen_climate = "Aw"), on_missing = "silent")$levels$code[1], "IX")
  # the ferrallitic sub-class by the weighted mean of the first 100 cm of the (B)
  f <- make_cpcs1967_example("ferrallitic"); f$horizons$bs_pct <- c(20, 45, 30, 15)
  expect_false(is.na(.sub(f)[["code"]]))
  # an anmoor over a stagnogley (a mottled Bg)
  an <- .fr(list(designation = c("A1", "Bg", "C"), top_cm = c(0, 25, 70), bottom_cm = c(25, 70, 110),
                 oc_pct = c(8, 0.6, 0.3), clay_pct = 30, munsell_hue_moist = "10YR", munsell_value_moist = c(2, 5, 5),
                 munsell_chroma_moist = c(1, 2, 3), redoximorphic_features_pct = c(0, 30, 10)))
  expect_identical(.sub(an)[["code"]], "XI-2")
})
