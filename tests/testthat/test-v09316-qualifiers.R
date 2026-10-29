# v0.9.316: WRB 2022 Chapter 5 qualifiers read from the text: Calcaric,
# Skeletic, Geric, Aric, Sodic, Loamic, Siltic, Brunic and Pellic. Small
# made-up pedons; no third-party data.

.k16_ped <- function(..., site = list(id = "k16")) {
  hz <- ensure_horizon_schema(data.table::as.data.table(data.frame(..., stringsAsFactors = FALSE)))
  PedonRecord$new(site = site, horizons = hz)
}


# ---- Calcaric (Ch 5, p. 129; calcaric material, Ch 3.3.3, p. 86) --------------

test_that("v0.9.316 Calcaric: calcaric material in a layer >= 30 cm within 100 cm", {
  skip_on_cran()
  mk <- function(caco3, eff = rep(NA_character_, 4))
    .k16_ped(top_cm = c(0, 25, 60, 90), bottom_cm = c(25, 60, 90, 130),
             designation = c("A", "Bw", "BC", "C"), caco3_pct = caco3,
             effervescence_class = eff, clay_pct = c(20, 22, 20, 18),
             silt_pct = 40, sand_pct = c(40, 38, 40, 42))
  expect_true(isTRUE(qual_calcaric(mk(c(5, 6, 1, 1)))$passed))   # 0-60 cm
  # one horizon of 88-107 cm, 12 cm within 100 cm, is not enough (was TRUE)
  deep <- .k16_ped(top_cm = c(0, 40, 88), bottom_cm = c(40, 88, 107),
                   designation = c("A", "Bt1", "Bt2"), caco3_pct = c(0, 1.6, 2.9))
  expect_false(isTRUE(qual_calcaric(deep)$passed))
  expect_false(is.na(qual_calcaric(deep)$passed))
  # 0-25 and 90-130 cm are not one layer, and 10 cm of the second lie within 100 cm
  expect_false(qual_calcaric(mk(c(3, 1, 1, 3)))$passed)
  # undecided when the CaCO3 and the HCl reaction are not recorded
  expect_true(is.na(qual_calcaric(mk(c(NA, NA, 1, 1)))$passed))
  # the HCl reaction decides when CaCO3 is not measured: visible effervescence
  # (moderate or stronger) is calcaric, slight (audible only) is not
  eff <- mk(rep(NA, 4), c("moderate", "strong", "slight", "none"))
  expect_equal(.wrb_calcaric_status(eff$horizons), c(TRUE, TRUE, FALSE, FALSE))
  expect_false(qual_calcaric(mk(rep(NA, 4), c("slight", "slight", "none", "none")))$passed)
  # calcaric material 0-60 cm, but without CaCO3 the calcic horizon is undecided
  r <- qual_calcaric(eff)
  expect_true(is.na(calcic(eff)$passed))
  expect_true(is.na(r$passed))
  expect_equal(r$evidence$calcaric_layer_cm, 60)
})

test_that("v0.9.316 Calcaric: not with a calcic horizon starting <= 100 cm; undecided if the calcic is", {
  skip_on_cran()
  p <- .k16_ped(top_cm = c(0, 30, 70), bottom_cm = c(30, 70, 120),
                designation = c("A", "Bk", "C"), caco3_pct = c(8, 30, 10),
                secondary_carbonates_pct = c(0, 20, 0),
                clay_pct = 20, silt_pct = 40, sand_pct = 40)
  expect_true(isTRUE(calcic(p)$passed))
  expect_false(qual_calcaric(p)$passed)
  q <- p$clone(deep = TRUE)
  q$horizons$caco3_pct <- c(8, 6, 4)
  q$horizons$secondary_carbonates_pct <- NA_real_
  expect_false(isTRUE(calcic(q)$passed))
  expect_true(isTRUE(qual_calcaric(q)$passed))
})

test_that("v0.9.316 Calcaric: the major part above a limiting layer starting < 60 cm", {
  skip_on_cran()
  p <- .k16_ped(top_cm = c(0, 15, 35), bottom_cm = c(15, 35, 60),
                designation = c("A", "Bw", "R"), caco3_pct = c(1, 5, NA))
  expect_equal(.barrier_top_cm(p), 35)
  expect_true(isTRUE(qual_calcaric(p)$passed))          # 20 of 35 cm
  p$horizons$caco3_pct <- c(5, 1, NA)
  expect_false(qual_calcaric(p)$passed)                 # 15 of 35 cm
})


# ---- Skeletic (Ch 5, p. 146) -------------------------------------------------

test_that("v0.9.316 Skeletic: >= 40% coarse fragments averaged over 100 cm", {
  skip_on_cran()
  mk <- function(cf) .k16_ped(top_cm = c(0, 30, 85), bottom_cm = c(30, 85, 120),
                              designation = c("A", "Bw", "2C"), coarse_fragments_pct = cf)
  # one 15 cm layer with 90% within 100 cm: average 16.5% (was TRUE)
  expect_false(qual_skeletic(mk(c(3.5, 3.5, 90)))$passed)
  expect_true(isTRUE(qual_skeletic(mk(c(30, 50, 40)))$passed))   # 42.5%
  # unrecorded fragments: undecided unless the rest decides
  expect_true(is.na(qual_skeletic(mk(c(NA, 3.5, 90)))$passed))
  expect_true(isTRUE(qual_skeletic(mk(c(NA, 70, 90)))$passed))  # 52% already
  expect_false(qual_skeletic(mk(c(NA, 3, 3)))$passed)           # at most 32.4%
})

test_that("v0.9.316 Skeletic: from the mineral soil surface, to a limiting layer", {
  skip_on_cran()
  p <- .k16_ped(top_cm = c(0, 20, 40, 60), bottom_cm = c(20, 40, 60, 80),
                designation = c("Oa", "A", "C", "R"), oc_pct = c(30, 2, 0.5, NA),
                coarse_fragments_pct = c(NA, 30, 60, NA))
  r <- qual_skeletic(p)
  expect_equal(r$evidence$window_cm, c(20, 60))
  expect_true(isTRUE(r$passed))                           # (30 + 60) / 2 = 45%
})


# ---- Geric (Ch 5, p. 135) ----------------------------------------------------

test_that("v0.9.316 Geric: bases + Al < 6 per kg clay; missing Al is not 0", {
  skip_on_cran()
  mk <- function(al) .k16_ped(top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
                              designation = c("A", "Bw1", "Bw2"), clay_pct = c(30, 50, 50),
                              ca_cmol = c(1.5, 0.8, 0.8), mg_cmol = c(0.5, 1.2, 1.2),
                              k_cmol = c(0.1, 0.1, 0.1), na_cmol = c(0.1, 0.1, 0.1),
                              al_kcl_cmol = al)
  # bases 2.2 cmolc in 50% clay = 4.4 per kg clay
  expect_true(isTRUE(qual_geric(mk(c(0.5, 0.5, 0.5)))$passed))   # 5.4
  expect_false(qual_geric(mk(c(0.5, 1, 1)))$passed)              # 6.4
  expect_true(is.na(qual_geric(mk(c(0.5, NA, NA)))$passed))      # was TRUE
  # bases alone already >= 6 per kg clay: FALSE without Al
  hi <- mk(c(NA, NA, NA)); hi$horizons$ca_cmol <- c(4, 3, 3)
  expect_false(qual_geric(hi)$passed)
})


# ---- Aric (Ch 5, p. 129) -----------------------------------------------------

test_that("v0.9.316 Aric: a ploughed layer >= 10 cm with an abrupt lower boundary", {
  skip_on_cran()
  mk <- function(desg, bd, bot1 = 22)
    .k16_ped(top_cm = c(0, bot1, 50), bottom_cm = c(bot1, 50, 100),
             designation = desg, boundary_distinctness = bd)
  expect_true(isTRUE(qual_aric(mk(c("Ap", "Bw", "C"), c("abrupt", "clear", NA)))$passed))
  expect_true(isTRUE(qual_aric(mk(c("Ap", "Bw", "C"), c("very abrupt", "clear", NA)))$passed))
  expect_true(isTRUE(qual_aric(mk(c("Ap", "Bw", "C"), c("Abrupta", "Clara", NA)))$passed))
  # a gradual boundary (was TRUE), too thin, or not ploughed
  expect_false(qual_aric(mk(c("Ap", "Bw", "C"), c("gradual", "clear", NA)))$passed)
  expect_false(qual_aric(mk(c("Ap", "Bw", "C"), c("abrupt", "clear", NA), bot1 = 8))$passed)
  expect_false(qual_aric(mk(c("Ah", "Bw", "C"), c("abrupt", "clear", NA)))$passed)
  expect_false(qual_aric(mk(c("Apl", "Bw", "C"), c("abrupt", "clear", NA)))$passed)
  # undecided without the boundary
  expect_true(is.na(qual_aric(mk(c("Ap", "Bw", "C"), c(NA, "clear", NA)))$passed))
  # Ap1 + Ap2: the boundary of the layer is that of the Ap2
  p <- .k16_ped(top_cm = c(0, 10, 25, 60), bottom_cm = c(10, 25, 60, 100),
                designation = c("Ap1", "Ap2", "Bw", "C"),
                boundary_distinctness = c("clear", "abrupt", "clear", NA))
  r <- qual_aric(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$layers, c(1L, 2L))
})


# ---- Sodic (Ch 5, p. 147) ----------------------------------------------------

test_that("v0.9.316 Sodic: a layer >= 20 cm thick with >= 15% Na + Mg and >= 6% Na", {
  skip_on_cran()
  mk <- function(na, mg = c(1, 1, 1))
    .k16_ped(top_cm = c(0, 12, 52), bottom_cm = c(12, 52, 100),
             designation = c("Ah1", "Ah2", "B"), clay_pct = c(20, 20, 20),
             silt_pct = 30, sand_pct = 50,
             cec_cmol = c(10, 10, 10), na_cmol = na, mg_cmol = mg)
  expect_false(isTRUE(natric_horizon(mk(c(1, 1, 0.2)))$passed))
  expect_false(qual_sodic(mk(c(1, 0.2, 0.2)))$passed)    # a 12 cm Ah1 only (was TRUE)
  expect_true(isTRUE(qual_sodic(mk(c(1, 1, 0.2)))$passed))
  expect_true(is.na(qual_sodic(mk(c(1, NA, 0.2)))$passed))
  # Na >= 6% but Na + Mg < 15%
  expect_false(qual_sodic(mk(c(0.7, 0.7, 0.7), mg = c(0.5, 0.5, 0.5)))$passed)
})

test_that("v0.9.316 Sodic: not with a natric horizon starting <= 100 cm", {
  skip_on_cran()
  p <- .k16_ped(top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 130),
                designation = c("A", "Btn", "C"), cec_cmol = c(10, 10, 10),
                na_cmol = c(1, 2, 2), mg_cmol = c(1, 1, 1))
  local_mocked_bindings(natric_horizon = function(pedon, ...)
    DiagnosticResult$new(name = "natric_horizon", passed = TRUE, layers = 2L,
                         evidence = list(), missing = character(0), reference = "test"))
  expect_false(qual_sodic(p)$passed)
})


# ---- Loamic and Siltic (Ch 5, p. 140 and 146; Annex 1, Table 8.39, p. 182) ----

test_that("v0.9.316 Loamic and Siltic: the texture classes of the triangle", {
  skip_on_cran()
  mk <- function(clay, silt) .k16_ped(top_cm = c(0, 30, 80), bottom_cm = c(30, 80, 130),
                                      designation = c("A", "C1", "C2"),
                                      clay_pct = clay, silt_pct = silt,
                                      sand_pct = 100 - clay - silt)
  # silt loam throughout: Siltic, not Loamic (Loamic was TRUE)
  sil <- mk(c(15, 14, 19), c(70, 74, 62))
  expect_true(isTRUE(qual_siltic(sil)$passed))
  expect_false(qual_loamic(sil)$passed)
  # silty clay loam (clay 30%): Loamic, not Siltic (Siltic was TRUE)
  sicl <- mk(c(30, 30, 30), c(60, 60, 60))
  expect_true(isTRUE(qual_loamic(sicl)$passed))
  expect_false(qual_siltic(sicl)$passed)
  # sandy clay (clay 37%, sand 50%): neither
  sc <- mk(c(37, 37, 37), c(13, 13, 13))
  expect_equal(unique(.wrb_texture_class(sc$horizons)), "SC")
  expect_false(qual_loamic(sc)$passed)
  # sandy loam with 5% clay: Loamic (was FALSE)
  expect_true(isTRUE(qual_loamic(mk(c(5, 5, 5), c(30, 30, 30)))$passed))
  # loam in the upper 20 cm only, over sand: not 30 cm
  thin <- .k16_ped(top_cm = c(0, 20), bottom_cm = c(20, 120), designation = c("A", "C"),
                   clay_pct = c(20, 3), silt_pct = c(40, 4), sand_pct = c(40, 93))
  expect_false(qual_loamic(thin)$passed)
  # undecided without the particle sizes
  und <- mk(c(15, 14, 19), c(70, 74, 62)); und$horizons$silt_pct <- NA; und$horizons$sand_pct <- NA
  expect_true(is.na(qual_siltic(und)$passed))
})


# ---- Brunic (Ch 5, p. 129; cambic criteria, Ch 3.1.5, p. 39-40) ---------------

test_that("v0.9.316 Brunic: a whole cambic horizon is not Brunic; sand layers stay undecided", {
  skip_on_cran()
  # a sandy loam Bw meets cambic criterion 1, so it is not a Brunic layer (was TRUE)
  p <- .k16_ped(top_cm = c(0, 15, 45), bottom_cm = c(15, 45, 100),
                designation = c("A", "Bw", "C"), clay_pct = c(10, 12, 8),
                silt_pct = c(30, 30, 25), sand_pct = c(60, 58, 67),
                structure_grade = c("weak", "moderate", "structureless"),
                structure_type = c("granular", "subangular blocky", "massive"),
                munsell_hue_moist = c("10YR", "7.5YR", "10YR"),
                munsell_value_moist = c(3, 4, 5), munsell_chroma_moist = c(2, 6, 4))
  expect_false(qual_brunic(p)$passed)
  # a brown sandy B over a paler sand: criterion 1 fails unless the sand is very
  # fine, which soilKey cannot tell, so undecided
  s <- p$clone(deep = TRUE)
  s$horizons$clay_pct <- c(3, 3, 1); s$horizons$silt_pct <- c(6, 6, 3)
  s$horizons$sand_pct <- c(91, 91, 96)
  r <- qual_brunic(s)
  expect_true(is.na(r$passed))
  expect_true(any(grepl("very fine sand", r$missing)))
  # the only sandy layer is a plough layer (criterion 4): not Brunic
  ap <- s$clone(deep = TRUE); ap$horizons$designation <- c("Ap1", "Ap2", "C")
  ap$horizons$clay_pct <- c(10, 3, 8); ap$horizons$silt_pct <- c(30, 6, 25)
  ap$horizons$sand_pct <- c(60, 91, 67)
  expect_false(qual_brunic(ap)$passed)
  # too deep: the sand layer starts at 60 cm
  dp <- .k16_ped(top_cm = c(0, 60, 90), bottom_cm = c(60, 90, 130),
                 designation = c("A", "B", "C"), clay_pct = c(20, 3, 1),
                 silt_pct = c(30, 6, 3), sand_pct = c(50, 91, 96))
  expect_false(qual_brunic(dp)$passed)
})


# ---- Pellic (Ch 5, p. 143) ---------------------------------------------------

test_that("v0.9.316 Pellic: value <= 3 and chroma <= 2 moist throughout the upper 30 cm", {
  skip_on_cran()
  mk <- function(v, c) .k16_ped(top_cm = c(0, 20, 47), bottom_cm = c(20, 47, 95),
                                designation = c("Ap", "BA", "Bg"),
                                munsell_hue_moist = "10YR", munsell_value_moist = v,
                                munsell_chroma_moist = c)
  expect_true(isTRUE(qual_pellic(mk(c(3, 3, 6), c(2, 1, 2)))$passed))
  expect_false(qual_pellic(mk(c(3, 7, 6), c(2, 2, 2)))$passed)    # BA from 20 cm (was TRUE)
  expect_true(is.na(qual_pellic(mk(c(3, NA, 6), c(2, NA, 2)))$passed))
})
