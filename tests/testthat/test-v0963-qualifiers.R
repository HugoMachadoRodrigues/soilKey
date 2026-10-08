# =============================================================================
# Tests for v0.9.63 -- new WRB qualifiers (Tier-1 batch).
#
# Covers the 25 PQ + 18 SQ qualifiers added in v0.9.63 to close gaps
# identified by the v0.9.62 canonical audit. Each qualifier tested for:
#   * NA-safe behaviour when input data is missing
#   * Positive trigger when input data is present and matches threshold
#   * Negative trigger on a non-matching pedon
#   * DiagnosticResult contract preserved (name, passed, layers,
#     evidence, missing, reference)
# =============================================================================


# ---- Helpers ---------------------------------------------------------

.minimal_pedon <- function(...) {
  hz <- data.frame(
    designation = c("A", "B"),
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    munsell_hue_moist = c("10YR", "10YR"),
    munsell_value_moist = c(4, 4),
    munsell_chroma_moist = c(3, 4),
    munsell_hue_dry = c(NA_character_, NA_character_),
    munsell_value_dry = c(NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_),
    clay_pct = c(20, 30), silt_pct = c(20, 25), sand_pct = c(60, 45),
    ph_h2o = c(5.5, 5.0), oc_pct = c(2.0, 0.5),
    cec_cmol = c(8, 6), base_saturation_pct = c(40, 25),
    stringsAsFactors = FALSE
  )
  PedonRecord$new(
    site = list(id = "test-min", country = "BR"),
    horizons = hz)
}


# ---- 1. Coarsic ---------------------------------------------------

test_that("qual_coarsic returns NA when no coarse_fragments_pct", {
  skip_on_cran()
  res <- qual_coarsic(.minimal_pedon())
  expect_s3_class(res, "DiagnosticResult")
  expect_true(is.na(res$passed))
  expect_match(res$missing, "coarse_fragments_pct")
})


test_that("qual_coarsic: < 20% fine earth over 75 cm (v0.9.217)", {
  skip_on_cran()
  # WRB 2022: "< 20% (by volume) fine earth, averaged over a depth of 75 cm"
  p <- .minimal_pedon()
  p$horizons$coarse_fragments_pct <- c(85, 85)
  expect_true(isTRUE(qual_coarsic(p)$passed))
  p$horizons$coarse_fragments_pct <- c(80, 75)          # >= 20% fine earth
  expect_false(isTRUE(qual_coarsic(p)$passed))
})


test_that("qual_coarsic does not fire when CF below threshold", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$coarse_fragments_pct <- c(20, 30)
  expect_false(isTRUE(qual_coarsic(p)$passed))
})


# ---- 2. Fractic ---------------------------------------------------

test_that("qual_fractic: remnants of a broken-up petro- horizon, not cracks", {
  skip_on_cran()
  # WRB 2022: a layer of remnants of a broken-up petrocalcic or petrogypsic
  # horizon; soilKey records no such remnants, so it is NA, and shrink-swell
  # cracks (the v0.9.63 reading) no longer make it TRUE.
  expect_true(is.na(qual_fractic(.minimal_pedon())$passed))
  p <- .minimal_pedon()
  p$horizons$cracks_width_cm <- c(0, 2)
  p$horizons$cracks_depth_cm <- c(0, 50)
  expect_false(isTRUE(qual_fractic(p)$passed))
})


# ---- 3. Gibbsic ---------------------------------------------------

test_that("qual_gibbsic: no gibbsite data, so Al2O3 is not taken for it", {
  skip_on_cran()
  # WRB 2022: >= 25% gibbsite in the clay fraction. Al2O3 from sulfuric
  # attack also counts kaolinite Al, so the v0.9.63 proxy marked ordinary
  # clayey Ferralsols Gibbsic; without a gibbsite measurement it is NA.
  p <- .minimal_pedon()
  p$horizons$al2o3_sulfuric_pct <- c(28, 30)
  expect_false(isTRUE(qual_gibbsic(p)$passed))
  expect_true(is.na(qual_gibbsic(p)$passed))
})


# ---- 4. Ferritic --------------------------------------------------

test_that("qual_ferritic: >= 10% Fe-dithionite in a >= 30 cm layer (v0.9.217)", {
  skip_on_cran()
  # WRB 2022: a layer >= 30 cm thick, starting <= 100 cm, with >= 10% Fedith,
  # not part of a plinthic, pisoplinthic or petroplinthic horizon
  p <- .minimal_pedon()
  p$horizons$fe_dcb_pct <- c(12, 14)
  p$horizons$plinthite_pct <- c(0, 0)
  expect_true(isTRUE(qual_ferritic(p)$passed))
  p$horizons$fe_dcb_pct <- c(8, 9)
  expect_false(isTRUE(qual_ferritic(p)$passed))
})


# ---- 5. Profundihumic ---------------------------------------------

test_that("qual_profundihumic requires SOC >= 1.4 weighted to 100 cm", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$oc_pct <- c(3.0, 2.0)
  expect_true(isTRUE(qual_profundihumic(p)$passed))
  p$horizons$oc_pct <- c(0.8, 0.3)
  expect_false(isTRUE(qual_profundihumic(p)$passed))
})


# ---- 6. Wapnic ----------------------------------------------------

test_that("qual_wapnic: a calcic horizon within organic material (v0.9.217)", {
  skip_on_cran()
  # WRB 2022: "a calcic horizon within organic material, starting <= 100 cm"
  # (lake marl in peat); CaCO3 >= 80% in a mineral soil is not Wapnic
  peat <- PedonRecord$new(site = list(id = "w", country = "TEST"),
    horizons = ensure_horizon_schema(data.table::data.table(
      designation = c("Oa", "Ok", "Oa2"), top_cm = c(0, 30, 60),
      bottom_cm = c(30, 60, 120), oc_pct = c(40, 25, 40),
      caco3_pct = c(0, 40, 0), clay_pct = c(NA, 10, NA))))
  expect_true(isTRUE(qual_wapnic(peat)$passed))
  p <- .minimal_pedon()
  p$horizons$caco3_pct <- c(85, 90)
  expect_false(isTRUE(qual_wapnic(p)$passed))
})


# ---- 7-9. Mawic / Muusic / Murshic --------------------------------

test_that("qual_mawic: moss fibres are not Mawic (v0.9.217)", {
  skip_on_cran()
  # WRB 2022 Mawic: a layer of coarse fragments from the soil surface whose
  # interstices are mostly filled with organic material; how full they are is
  # not recorded, so it is never TRUE, and moss fibres (the v0.9.63 reading,
  # which is Bryic's) no longer make it TRUE.
  p <- .minimal_pedon()
  p$horizons$fiber_content_unrubbed_pct <- c(50, 60)
  p$horizons$layer_origin <- c("musgo Sphagnum", "musgo Sphagnum")
  expect_false(isTRUE(qual_mawic(p)$passed))
})


test_that("qual_muusic: organic material directly over ice (v0.9.217)", {
  skip_on_cran()
  # WRB 2022: "organic material starting at the soil surface that directly
  # overlies ice"
  p <- PedonRecord$new(site = list(id = "m", country = "TEST"),
    horizons = ensure_horizon_schema(data.table::data.table(
      designation = c("Oi", "Wf"), top_cm = c(0, 40), bottom_cm = c(40, 100),
      oc_pct = c(45, NA), ice_pct = c(NA, 90))))
  expect_true(isTRUE(qual_muusic(p)$passed))
  q <- .minimal_pedon()
  q$horizons$fiber_content_rubbed_pct <- c(80, 85)    # fibres, no ice
  expect_false(isTRUE(qual_muusic(q)$passed))
})


test_that("qual_murshic: a drained, structured histic horizon (v0.9.217)", {
  skip_on_cran()
  # WRB 2022: a drained histic horizon, >= 20 cm thick, with bulk density
  # >= 0.2 and granular or blocky structure (or cracks). Strongly decomposed
  # peat alone (the v0.9.63 reading) is not Murshic.
  dr <- PedonRecord$new(
    site = list(id = "m", country = "DE", drainage_class = "artificially drained",
                land_use = "drained grassland"),
    horizons = ensure_horizon_schema(data.table::data.table(
      designation = c("Hp", "Ha"), top_cm = c(0, 30), bottom_cm = c(30, 120),
      oc_pct = c(35, 40), bulk_density_g_cm3 = c(0.4, 0.3),
      structure_type = c("granular", "massive"),
      structure_grade = c("moderate", "massive"))))
  expect_true(isTRUE(qual_murshic(dr)$passed))
  p <- .minimal_pedon()
  p$horizons$fiber_content_rubbed_pct <- c(10, 8)
  expect_false(isTRUE(qual_murshic(p)$passed))
})


# ---- 10-13. Endo- / Pante- / Ortho- modifiers ----------------------

test_that("qual_endocalcaric depth-bounded modifier", {
  skip_on_cran()
  res <- qual_endocalcaric(.minimal_pedon())
  expect_s3_class(res, "DiagnosticResult")
})


test_that("qual_anofluvic / orthofluvic / pantofluvic don't error", {
  skip_on_cran()
  for (fn in list(qual_anofluvic, qual_orthofluvic, qual_pantofluvic)) {
    res <- fn(.minimal_pedon())
    expect_s3_class(res, "DiagnosticResult")
  }
})


# ---- 14-15. Oxy/Reductaquic/gleyic ----------------------------------

test_that("qual_oxyaquic: saturated, without gleyic or stagnic properties", {
  skip_on_cran()
  # WRB 2022: a layer >= 25 cm starting <= 75 cm, saturated >= 20 consecutive
  # days, and no gleyic or stagnic properties within 100 cm. Redox features
  # (the v0.9.63 reading) are what gleyic and stagnic properties are made of.
  p <- .minimal_pedon()
  p$horizons$water_saturation_days <- c(360, 360)
  p$horizons$redoximorphic_features_pct <- c(0, 0)
  expect_true(isTRUE(qual_oxyaquic(p)$passed))
  p$horizons$water_saturation_days <- c(10, 10)
  expect_false(isTRUE(qual_oxyaquic(p)$passed))
  q <- .minimal_pedon()
  q$horizons$redoximorphic_features_pct <- c(8, 10)
  expect_false(isTRUE(qual_oxyaquic(q)$passed))
})


# ---- 16. Hypernatric -----------------------------------------------

test_that("qual_hypernatric fires when ESP >= 70%", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$na_cmol  <- c(7, 5)
  p$horizons$cec_cmol <- c(10, 7)  # ESP = 70 / 71%
  expect_true(isTRUE(qual_hypernatric(p)$passed))
})


# ---- 17. Carbonatic / Carbonic -------------------------------------

test_that("qual_carbonatic: a salic horizon with carbonate-dominated solution", {
  skip_on_cran()
  # WRB 2022: a salic horizon whose 1:1 solution has pH >= 8.5 and
  # [HCO3-] > [SO4--] > 2[Cl-]; CaCO3 (the v0.9.63 reading) is not that, and
  # soilKey stores no solution anions, so it is never TRUE.
  p <- .minimal_pedon()
  p$horizons$caco3_pct <- c(60, 55)
  expect_false(isTRUE(qual_carbonatic(p)$passed))
})


test_that("qual_carbonic: organic carbon of artefacts, not of the soil", {
  skip_on_cran()
  # WRB 2022: ">= 5% organic carbon that belongs to artefacts"; a humus-rich
  # soil (the v0.9.63 reading) is not Carbonic.
  p <- .minimal_pedon()
  p$horizons$oc_pct <- c(8, 7)
  expect_false(isTRUE(qual_carbonic(p)$passed))
})


# ---- 18. Transportic / Relocatic / Isolatic ------------------------

test_that("qual_transportic: transported material, < 10% artefacts (v0.9.217)", {
  skip_on_cran()
  # WRB 2022: a layer >= 20 cm with < 10% artefacts, moved from a source area
  # outside the immediate vicinity. "Aterro" (fill) may be local material
  # (Relocatic) and no longer counts.
  p <- .minimal_pedon()
  p$horizons$layer_origin <- c("transported (imported fill)", "transported (imported fill)")
  p$horizons$artefacts_pct <- c(2, 2)
  expect_true(isTRUE(qual_transportic(p)$passed))
  q <- .minimal_pedon()
  q$horizons$layer_origin <- c("aterro antropico", "aterro antropico")
  expect_false(isTRUE(qual_transportic(q)$passed))
})


test_that("qual_isolatic: isolated above a barrier, not an artefact share", {
  skip_on_cran()
  # WRB 2022: above technic hard material, a geomembrane or a continuous
  # layer of artefacts, without contact to other soil material. That contact
  # is not recorded, so it is never TRUE; an urbic share (the v0.9.63 reading)
  # does not make it so.
  p <- .minimal_pedon()
  p$horizons$artefacts_urbic_pct <- c(15, 30)
  expect_false(isTRUE(qual_isolatic(p)$passed))
})


# ---- 19-22. SQ Endo/Epi-dystric/eutric -----------------------------

test_that("qual_endodystric / qual_epidystric depth-bounded", {
  skip_on_cran()
  expect_s3_class(qual_endodystric(.minimal_pedon()), "DiagnosticResult")
  expect_s3_class(qual_epidystric(.minimal_pedon()),  "DiagnosticResult")
  expect_s3_class(qual_endoeutric(.minimal_pedon()),  "DiagnosticResult")
  expect_s3_class(qual_epieutric(.minimal_pedon()),   "DiagnosticResult")
})


# ---- 23. argic + cambic engine arg ---------------------------------

test_that("argic() supports engine = 'aqp' and 'soilkey'", {
  skip_on_cran()
  testthat::skip_if_not_installed("aqp")
  p <- .minimal_pedon()
  r1 <- argic(p, engine = "soilkey")
  r2 <- argic(p, engine = "aqp")
  expect_s3_class(r1, "DiagnosticResult")
  expect_s3_class(r2, "DiagnosticResult")
  # The two engines may disagree -- that is expected
  expect_match(r2$reference, "engine=aqp")
})


test_that("cambic() supports engine = 'aqp' and 'soilkey'", {
  skip_on_cran()
  testthat::skip_if_not_installed("aqp")
  p <- .minimal_pedon()
  r1 <- cambic(p, engine = "soilkey")
  r2 <- cambic(p, engine = "aqp")
  expect_s3_class(r1, "DiagnosticResult")
  expect_s3_class(r2, "DiagnosticResult")
  expect_match(r2$reference, "engine=aqp")
})
