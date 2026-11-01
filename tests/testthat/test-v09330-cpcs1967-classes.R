# v0.9.330: the twelve classes of the French soil classification (CPCS
# 1967). Made-up profiles; no French data.

.fr <- function(h, site = list()) {
  n <- max(lengths(h))
  h <- lapply(h, function(v) if (length(v) == 1L) rep(v, n) else v)
  base <- list(id = "t", country = "FR", designation_system = "cpcs", drainage_class = "well drained")
  PedonRecord$new(site = utils::modifyList(base, site), horizons = ensure_horizon_schema(
    data.table::as.data.table(data.frame(h, stringsAsFactors = FALSE))))
}
.cl <- function(p) {
  r <- classify_cpcs1967(p, on_missing = "silent")
  c(code = r$levels$code[1] %||% NA_character_, prov = paste(r$provisional_because, collapse = ","))
}

test_that("every example keys to its class, decided, with the French names", {
  skip_on_cran()
  want <- c(raw_mineral = "I", weakly_developed = "II", vertisol = "III", andosol = "IV", calcimagnesic = "V",
            isohumic = "VI", brunified = "VII", podzolised = "VIII", sesquioxide = "IX", ferrallitic = "X",
            hydromorphic = "XI", sodic = "XII")
  for (g in names(want)) {
    r <- classify_cpcs1967(make_cpcs1967_example(g), on_missing = "silent")
    expect_identical(r$levels$code[1], want[[g]], info = g)
    expect_length(r$provisional_because, 0)
    expect_identical(r$system_id, "cpcs1967")
  }
  r <- classify_cpcs1967(make_cpcs1967_example("calcimagnesic"), on_missing = "silent")
  expect_identical(r$name, "Sols calcimagn\u00e9siques")
  expect_identical(r$levels$name_native[1], "Sols calcimagn\u00e9siques")
  expect_true("cpcs1967" %in% soilkey_systems()$id)
  expect_length(soilKey:::.validate_rules("cpcs1967")$missing, 0)
  # the key order of the text's precedences
  codes <- vapply(load_rules("cpcs1967")$classes, `[[`, "", "code")
  expect_identical(codes, c("I", "III", "XII", "IV", "VIII", "X", "IX", "XI", "II", "VI", "V", "VII"))
})

test_that("the old French notation", {
  skip_on_cran()
  d <- c("Aoo", "A1", "A2", "(B)", "B2fe", "B2h", "BC", "Cca", "IIC", "G", "Cr", "(A)")
  x <- soilKey:::.cpcs_designations(d, list())
  expect_true(x$cpcs)
  expect_identical(x$role, c("O", "A", "E", "B", "B", "B", "B", "C", "C", "G", "C", "A"))
  expect_identical(x$bstruct[4], TRUE)
  expect_identical(x$benriched[5:6], c(TRUE, TRUE))
  expect_true(x$trans[7])
  expect_true(x$ca[8]); expect_identical(x$lith[9], 2L); expect_true(x$G[10])
  expect_false(x$G[11]); expect_true(x$wrock[11]); expect_true(x$embryo[12])
  # never from the country: FAO letters in a French profile
  expect_false(soilKey:::.cpcs_designations(c("Ap", "Bt", "C"), list(country = "FR"))$cpcs)
  expect_true(soilKey:::.cpcs_designations(c("Ap", "Bt", "C"), list(designation_system = "cpcs"))$cpcs)
  # a plain B is enriched in the CPCS notation, open in the FAO one
  expect_true(soilKey:::.cpcs_designations(c("A1", "B", "C"), list(designation_system = "cpcs"))$benriched[2])
  expect_true(is.na(soilKey:::.cpcs_designations(c("A", "B", "C"), list(designation_system = "fao"))$benriched[2]))
})

test_that("organic matter as 1.72 x C, depths from the top of the A1", {
  skip_on_cran()
  x <- soilKey:::.cpcs_profile(make_cpcs1967_example("podzolised"))
  expect_equal(x$surface, 5)
  expect_equal(x$om[2], 5 * 1.72)
  expect_equal(x$dtop[1], -5)
})

test_that("classes I and II: the traces of organic matter, the AC profile, permafrost", {
  skip_on_cran()
  raw <- make_cpcs1967_example("raw_mineral")
  expect_true(raw_mineral_soils_cpcs(raw)$passed)
  # 0.8 % OM in the top: not traces (the grey subdesert soils), class II
  p <- raw$clone(deep = TRUE); p$horizons$oc_pct <- c(0.47, 0.3, 0.1)
  expect_false(raw_mineral_soils_cpcs(p)$passed)
  expect_identical(.cl(p)[["code"]], "II")
  # 1.5 % OM allowed in a top layer of 3 cm
  q <- .fr(list(designation = c("(A)", "C1", "C2"), top_cm = c(0, 2, 40), bottom_cm = c(2, 40, 100),
                oc_pct = c(0.8, 0.2, 0.1), clay_pct = 8, silt_pct = 20, sand_pct = 72, layer_origin = "alluvium"))
  expect_true(raw_mineral_soils_cpcs(q)$passed)
  # without carbon, the notation: an A is not raw
  r <- raw$clone(deep = TRUE); r$horizons$oc_pct <- NA_real_; r$horizons$designation <- c("A1", "C1", "C2")
  expect_false(raw_mineral_soils_cpcs(r)$passed)
  # permafrost: class II, not hydromorphic
  w <- make_cpcs1967_example("hydromorphic"); w$site$soil_temperature_regime <- "pergelic"
  expect_false(hydromorphic_soils_cpcs(w)$passed)
  expect_identical(.cl(w)[["code"]], "II")
})

test_that("classes II and V: an AC soil on carbonate is calcimagnesic unless a cause of class II is recorded", {
  skip_on_cran()
  al <- make_cpcs1967_example("weakly_developed")
  expect_identical(.cl(al)[["code"]], "II")
  # the same calcareous AC soil with no recent deposit recorded: a calcimagnesic soil
  v <- al$clone(deep = TRUE); v$horizons$layer_origin <- "marl"
  v$horizons$oc_pct <- c(4, 0.6, 0.2)
  expect_false(weakly_developed_soils_cpcs(v)$passed)
  expect_identical(.cl(v)[["code"]], "V")
  # recent erosion recorded: class II
  e <- v$clone(deep = TRUE); e$site$anthropic_truncation <- TRUE
  expect_true(weakly_developed_soils_cpcs(e)$passed)
  # an acid humic AC soil (pH under 6.8): class II
  a <- v$clone(deep = TRUE); a$horizons$ph_h2o <- 6.2; a$horizons$caco3_pct <- 0; a$horizons$bs_pct <- 70
  a$horizons$ca_cmol <- 8
  expect_identical(.cl(a)[["code"]], "II")
})

test_that("classes V and VI: a sharp or a gradual fall of organic matter", {
  skip_on_cran()
  iso <- make_cpcs1967_example("isohumic")
  expect_true(isohumic_soils_cpcs(iso)$passed)
  ab <- iso$clone(deep = TRUE); ab$horizons$oc_pct <- c(3, 0.8, 0.5, 0.3)
  expect_false(isohumic_soils_cpcs(ab)$passed)
  # a rendzina: the A on rock is a sharp transition
  rz <- make_cpcs1967_example("calcimagnesic")
  expect_false(isohumic_soils_cpcs(rz)$passed)
  expect_true(calcimagnesic_soils_cpcs(rz)$passed)
  # an enriched B: not calcimagnesic
  b <- rz$clone(deep = TRUE); b$horizons$designation <- c("A1", "Bt")
  expect_false(calcimagnesic_soils_cpcs(b)$passed)
})

test_that("class III: slickensides, clay, cracks, no enriched B", {
  skip_on_cran()
  v <- make_cpcs1967_example("vertisol")
  expect_true(vertisols_cpcs(v)$passed)
  ns <- v$clone(deep = TRUE); ns$horizons$slickensides <- NA_character_
  expect_false(vertisols_cpcs(ns)$passed)
  lc <- v$clone(deep = TRUE); lc$horizons$clay_pct <- c(50, 25, 55, 45)
  expect_false(vertisols_cpcs(lc)$passed)
  cr <- v$clone(deep = TRUE); cr$horizons$cracks_width_cm <- c(0.2, 0.4, NA, NA)
  expect_false(vertisols_cpcs(cr)$passed)
  bt <- v$clone(deep = TRUE); bt$horizons$designation <- c("A1", "B2t", "B3t", "C")
  expect_false(vertisols_cpcs(bt)$passed)
})

test_that("class XII: EC through the profile, sodium with a degraded structure, sands and gypsum", {
  skip_on_cran()
  s <- make_cpcs1967_example("sodic")
  expect_true(sodic_soils_cpcs(s)$passed)
  # without a B, the horizons to 50 cm count: one of them under 7 dS/m
  part <- s$clone(deep = TRUE); part$horizons$ec_dS_m <- c(25, 5, 4)
  part$horizons$na_cmol <- 0.5
  expect_false(sodic_soils_cpcs(part)$passed)
  # Na above 10 % of the CEC with a columnar, very firm horizon
  na <- part$clone(deep = TRUE); na$horizons$na_cmol <- c(0.5, 3, 3)
  na$horizons$structure_type <- c("granular", "columnar", "massive")
  na$horizons$consistence_moist <- c("friable", "very firm", "firm")
  expect_true(sodic_soils_cpcs(na)$passed)
  na$horizons$structure_type <- c("granular", "subangular blocky", "subangular blocky")
  na$horizons$structure_grade <- "moderate"
  expect_false(sodic_soils_cpcs(na)$passed)
  # clean sands
  sa <- s$clone(deep = TRUE); sa$horizons$sand_pct <- 92; sa$horizons$clay_pct <- 3; sa$horizons$silt_pct <- 5
  expect_false(sodic_soils_cpcs(sa)$passed)
  # gypsum alone, little sodium
  gy <- s$clone(deep = TRUE); gy$horizons$na_cmol <- 0.3; gy$horizons$caso4_pct <- 15
  expect_false(sodic_soils_cpcs(gy)$passed)
  # nothing recorded: not sodic; salts seen: undecided
  no <- make_cpcs1967_example("brunified")
  expect_false(sodic_soils_cpcs(no)$passed)
  seen <- no$clone(deep = TRUE); seen$horizons$designation <- c("A1", "(B)Sa", "C"); seen$horizons$na_cmol <- NA_real_
  expect_true(is.na(sodic_soils_cpcs(seen)$passed))
})

test_that("class IV: andic over 30 cm, CEC above 25, a (B); an allophanic AC soil is weakly developed", {
  skip_on_cran()
  a <- make_cpcs1967_example("andosol")
  expect_true(andosols_cpcs(a)$passed)
  ac <- a$clone(deep = TRUE); ac$horizons$designation <- c("A1", "A12", "C")
  expect_false(andosols_cpcs(ac)$passed)
  expect_identical(.cl(ac)[["code"]], "II")
  lo <- a$clone(deep = TRUE); lo$horizons$cec_cmol <- c(20, 18, 15)
  expect_false(andosols_cpcs(lo)$passed)
  # no andic data and no volcanic material: not an andosol
  b <- make_cpcs1967_example("brunified")
  expect_false(andosols_cpcs(b)$passed)
})

test_that("class VIII: the podzolic B by criterion 2, the notation, the ashy A2", {
  skip_on_cran()
  p <- make_cpcs1967_example("podzolised")
  expect_true(podzolised_soils_cpcs(p)$passed)
  # C/N of 14 or less in the B: not podzolic
  q <- p$clone(deep = TRUE); q$horizons$n_total_pct <- c(1.2, 0.2, 0.03, 0.3, 0.1, 0.02)
  expect_false(podzolised_soils_cpcs(q)$passed)
  # no nitrogen: the Bh and Bfe of the notation decide
  r <- p$clone(deep = TRUE); r$horizons$n_total_pct <- NA_real_
  expect_true(podzolised_soils_cpcs(r)$passed)
  # an ashy A2 with no B (humo-ashy soil)
  ha <- .fr(list(designation = c("Ao", "A1", "A2", "C"), top_cm = c(0, 5, 10, 30), bottom_cm = c(5, 10, 30, 80),
                 oc_pct = c(30, 4, 0.3, 0.1), n_total_pct = c(1, 0.15, 0.02, 0.01), clay_pct = 5, silt_pct = 8,
                 sand_pct = 87, munsell_hue_moist = "10YR", munsell_value_moist = c(2, 3, 6, 6),
                 munsell_chroma_moist = c(1, 1, 1, 3), munsell_value_dry = c(NA, 4, 7, 7),
                 munsell_chroma_dry = c(NA, 1, 1, 3)))
  expect_true(podzolised_soils_cpcs(ha)$passed)
})

test_that("classes X and IX: Ki, the CEC of the clay, base saturation, iron, read in the B", {
  skip_on_cran()
  f <- make_cpcs1967_example("ferrallitic")
  expect_true(ferrallitic_soils_cpcs(f)$passed)
  # Ki above 2: not ferrallitic
  k <- f$clone(deep = TRUE); k$horizons$sio2_sulfuric_pct <- c(NA, 30, 30, 30); k$horizons$al2o3_sulfuric_pct <- c(NA, 15, 15, 15)
  expect_false(ferrallitic_soils_cpcs(k)$passed)
  # no Ki: the CEC of the clay
  n <- f$clone(deep = TRUE); n$horizons$sio2_sulfuric_pct <- NA_real_; n$horizons$al2o3_sulfuric_pct <- NA_real_
  expect_true(ferrallitic_soils_cpcs(n)$passed)
  n$horizons$cec_cmol <- c(20, 20, 20, 20)
  expect_false(ferrallitic_soils_cpcs(n)$passed)
  s <- make_cpcs1967_example("sesquioxide")
  expect_true(sesquioxide_soils_cpcs(s)$passed)
  lo <- s$clone(deep = TRUE); lo$horizons$bs_pct <- c(40, 35, 30, 40)
  expect_false(sesquioxide_soils_cpcs(lo)$passed)
  pale <- s$clone(deep = TRUE); pale$horizons$munsell_chroma_moist <- c(3, 3, 3, 3)
  expect_false(sesquioxide_soils_cpcs(pale)$passed)
})

test_that("class XI: peat, an organic surface on gley, hydromorphy over most of the profile", {
  skip_on_cran()
  g <- make_cpcs1967_example("hydromorphic")
  expect_true(hydromorphic_soils_cpcs(g)$passed)
  # a deep gley alone: not hydromorphic
  d <- .fr(list(designation = c("A1", "(B)", "C", "G"), top_cm = c(0, 20, 60, 85), bottom_cm = c(20, 60, 85, 130),
                oc_pct = c(2, 0.6, 0.3, 0.2), clay_pct = 25, munsell_hue_moist = c("10YR", "10YR", "10YR", "5GY"),
                munsell_value_moist = c(3, 5, 5, 5), munsell_chroma_moist = c(2, 4, 4, 1),
                redoximorphic_features_pct = c(0, 0, 2, 10)))
  expect_false(hydromorphic_soils_cpcs(d)$passed)
  # peat: 30 % OM or more over 40 cm of the top 80 cm
  pt <- .fr(list(designation = c("H1", "H2", "G"), top_cm = c(0, 30, 60), bottom_cm = c(30, 60, 100),
                 oc_pct = c(30, 25, 1), clay_pct = c(NA, NA, 30), munsell_hue_moist = c("10YR", "10YR", "5G"),
                 munsell_value_moist = c(2, 2, 5), munsell_chroma_moist = c(1, 1, 1)),
            list(drainage_class = "very poorly drained"))
  expect_true(hydromorphic_soils_cpcs(pt)$passed)
  expect_identical(.cl(pt)[["code"]], "XI")
  # 8 to 30 % OM over 20 cm on a gley (an anmoor)
  an <- .fr(list(designation = c("A1", "G"), top_cm = c(0, 25), bottom_cm = c(25, 90), oc_pct = c(8, 0.5),
                 clay_pct = 30, munsell_hue_moist = c("10YR", "5GY"), munsell_value_moist = c(2, 5),
                 munsell_chroma_moist = c(1, 1)))
  expect_true(hydromorphic_soils_cpcs(an)$passed)
  # a grey parent material (2.5Y, chroma 2) is not a reduced matrix
  gr <- d$clone(deep = TRUE); gr$horizons$munsell_hue_moist <- "2.5Y"; gr$horizons$munsell_chroma_moist <- 2
  gr$horizons$designation <- c("A1", "(B)", "C1", "C2"); gr$horizons$redoximorphic_features_pct <- 0
  expect_false(hydromorphic_soils_cpcs(gr)$passed)
})

test_that("an undesignated profile is read through soilKey's diagnostic horizons, with a warning", {
  skip_on_cran()
  b <- make_cpcs1967_example("brunified")
  b$horizons$designation <- NA_character_
  s <- b$site; s$designation_system <- NULL; b$site <- s
  r <- classify_cpcs1967(b, on_missing = "silent")
  expect_true(any(grepl("No horizon designations", r$warnings)))
  # the B cannot be told from these data: undecided, not a class
  expect_true(is.na(brunified_soils_cpcs(b)$passed))
  # a clear clay increase: soilKey's argic horizon shows the B
  b$horizons$clay_pct <- c(15, 35, 30); b$horizons$clay_films_amount <- c("none", "common", "few")
  x <- soilKey:::.cpcs_profile(b)
  expect_true(soilKey:::.cpcs_horizon_types(x, b)$b_any)
  expect_identical(.cl(b)[["code"]], "VII")
})

test_that("after the canonical profiles: iron, the fall of OM, the solum, the cracks, the notes", {
  skip_on_cran()
  # a 7.5YR 5/6 B: iron individualised only under a tropical climate
  s <- make_cpcs1967_example("sesquioxide")
  y <- s$clone(deep = TRUE); y$horizons$munsell_hue_moist <- "7.5YR"; y$horizons$munsell_value_moist <- 5
  expect_false(sesquioxide_soils_cpcs(y)$passed)
  y$site$lat <- -12
  expect_true(sesquioxide_soils_cpcs(y)$passed)
  # the gradual fall at a third: OM halving from one horizon to the next is gradual
  iso <- make_cpcs1967_example("isohumic")
  hv <- iso$clone(deep = TRUE); hv$horizons$oc_pct <- c(4, 2, 1, 0.3)
  expect_true(isohumic_soils_cpcs(hv)$passed)
  # S/T rising to 80 % in the B
  lo <- iso$clone(deep = TRUE); lo$horizons$bs_pct <- c(60, 65, 70, 100)
  expect_false(isohumic_soils_cpcs(lo)$passed)
  # class V needs no fall of OM: a soil poor in OM throughout
  g <- .fr(list(designation = c("A1", "(B)", "Ccs"), top_cm = c(0, 15, 50), bottom_cm = c(15, 50, 120),
                oc_pct = c(0.5, 0.3, 0.1), clay_pct = 22, silt_pct = 40, sand_pct = 38, ph_h2o = 7.8,
                bs_pct = 100, caco3_pct = 5, caso4_pct = c(1, 8, 30), munsell_hue_moist = "10YR",
                munsell_value_moist = c(5, 6, 7), munsell_chroma_moist = 3, redoximorphic_features_pct = 0))
  expect_identical(.cl(g)[["code"]], "V")
  # the EC of class XII in the solum: a non-saline C below does not count
  so <- make_cpcs1967_example("sodic")
  so$horizons$designation <- c("A1Sa", "BSa", "C"); so$horizons$ec_dS_m <- c(25, 18, 4)
  expect_true(sodic_soils_cpcs(so)$passed)
  # a note after the symbol is not read as symbols
  x <- soilKey:::.cpcs_designations(c("A", "Btg/glossic", "BC"), list())
  expect_false(x$cn[2]); expect_true(x$t[2]); expect_true(x$g[2])
  # the canonical profiles key to the CPCS classes of their kind
  can <- c(ferralsol = "X", podzol = "VIII", vertisol = "III", andosol = "IV", chernozem = "VI",
           kastanozem = "VI", phaeozem = "VI", solonchak = "XII", solonetz = "XII", histosol = "XI",
           gleysol = "XI", cambisol = "VII", luvisol = "VII", calcisol = "V", gypsisol = "V",
           fluvisol = "II", cryosol = "II", lixisol = "IX")
  for (k in names(can)) {
    expect_identical(.cl(get(paste0("make_", k, "_canonical"))())[["code"]], can[[k]], info = k)
  }
})
