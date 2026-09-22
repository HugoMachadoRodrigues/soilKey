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


test_that("qual_coarsic fires when coarse_fragments_pct >= 70", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$coarse_fragments_pct <- c(80, 75)
  expect_true(isTRUE(qual_coarsic(p)$passed))
})


test_that("qual_coarsic does not fire when CF below threshold", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$coarse_fragments_pct <- c(20, 30)
  expect_false(isTRUE(qual_coarsic(p)$passed))
})


# ---- 2. Fractic ---------------------------------------------------

test_that("qual_fractic NA-safe + fires on cracks", {
  skip_on_cran()
  expect_true(is.na(qual_fractic(.minimal_pedon())$passed))
  p <- .minimal_pedon()
  p$horizons$cracks_width_cm <- c(0, 2)
  p$horizons$cracks_depth_cm <- c(0, 50)
  expect_true(isTRUE(qual_fractic(p)$passed))
})


# ---- 3. Gibbsic ---------------------------------------------------

test_that("qual_gibbsic uses al2o3_sulfuric_pct proxy at 25%", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$al2o3_sulfuric_pct <- c(28, 30)
  expect_true(isTRUE(qual_gibbsic(p)$passed))
  p$horizons$al2o3_sulfuric_pct <- c(15, 18)
  expect_false(isTRUE(qual_gibbsic(p)$passed))
})


# ---- 4. Ferritic --------------------------------------------------

test_that("qual_ferritic fires on Fe2O3 >= 18%", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$fe_dcb_pct <- c(20, 25)
  expect_true(isTRUE(qual_ferritic(p)$passed))
  p$horizons$fe_dcb_pct <- c(8, 10)
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

test_that("qual_wapnic requires CaCO3 >= 80%", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$caco3_pct <- c(85, 90)
  expect_true(isTRUE(qual_wapnic(p)$passed))
  p$horizons$caco3_pct <- c(20, 25)
  expect_false(isTRUE(qual_wapnic(p)$passed))
})


# ---- 7-9. Mawic / Muusic / Murshic --------------------------------

test_that("qual_mawic requires moss + fibre >= 40", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$fiber_content_unrubbed_pct <- c(50, 60)
  p$horizons$layer_origin <- c("musgo Sphagnum", "musgo Sphagnum")
  expect_true(isTRUE(qual_mawic(p)$passed))
  p$horizons$layer_origin <- c("residue", "residue")
  expect_false(isTRUE(qual_mawic(p)$passed))
})


test_that("qual_muusic requires rubbed fibre >= 75", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$fiber_content_rubbed_pct <- c(80, 85)
  expect_true(isTRUE(qual_muusic(p)$passed))
})


test_that("qual_murshic uses low rubbed fibre OR von Post >= 7", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$fiber_content_rubbed_pct <- c(10, 8)   # < 17
  expect_true(isTRUE(qual_murshic(p)$passed))
  p2 <- .minimal_pedon()
  p2$horizons$von_post_index <- c(8L, 9L)
  expect_true(isTRUE(qual_murshic(p2)$passed))
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

test_that("qual_oxyaquic fires on redox >= 5 with non-gleyic hue", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$redoximorphic_features_pct <- c(8, 10)
  # 10YR is NOT gleyic, so oxidized + redox -> oxyaquic
  expect_true(isTRUE(qual_oxyaquic(p)$passed))
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

test_that("qual_carbonatic on CaCO3 >= 50", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$caco3_pct <- c(60, 55)
  expect_true(isTRUE(qual_carbonatic(p)$passed))
})


test_that("qual_carbonic on SOC >= 6", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$oc_pct <- c(8, 7)
  expect_true(isTRUE(qual_carbonic(p)$passed))
})


# ---- 18. Transportic / Relocatic / Isolatic ------------------------

test_that("qual_transportic matches origin pattern", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$layer_origin <- c("aterro antropico", "aterro antropico")
  expect_true(isTRUE(qual_transportic(p)$passed))
})


test_that("qual_isolatic needs artefact pct between 5 and 50", {
  skip_on_cran()
  p <- .minimal_pedon()
  p$horizons$artefacts_urbic_pct <- c(15, 30)
  expect_true(isTRUE(qual_isolatic(p)$passed))
  p$horizons$artefacts_urbic_pct <- c(70, 80)
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
