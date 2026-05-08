# =============================================================================
# Tests for v0.9.62 -- aqp interop helpers + canonical references.
#
# Three test groups:
#   1. texture_class_from_pct() canonical USDA triangle
#   2. pedon_to_spc() roundtrip from soilKey schema -> aqp SPC
#   3. argic_aqp() / cambic_aqp() return DiagnosticResult and agree
#      with aqp's bare functions
#   4. canonical_reference() loads vendored RDA + falls back to
#      installed SoilTaxonomy
# =============================================================================


# ---- 1. texture_class_from_pct ---------------------------------------

test_that("texture_class_from_pct returns canonical USDA classes", {
  # Classic clay (heavy)
  expect_equal(texture_class_from_pct(60,  20, 20), "C")
  # Sandy clay loam (sandy texture, moderate clay)
  expect_equal(texture_class_from_pct(25,  10, 65), "SCL")
  # Silty clay loam
  expect_equal(texture_class_from_pct(30,  60, 10), "SICL")
  # Loam (interior of triangle)
  expect_equal(texture_class_from_pct(15,  40, 45), "L")
  # Sand: silt + 1.5*clay < 15 (here 5 + 7.5 = 12.5)
  expect_equal(texture_class_from_pct(5,    5, 90), "S")
  # Loamy sand: silt + 1.5*clay >= 15 AND silt + 2*clay < 30
  # Pick a clearly-LS triple: clay=4 silt=8 sand=88 -> silt+1.5*cl=14, NOT >=15... let me pick clay=5 silt=10 sand=85 (12.5 + 5 = 17.5 >= 15 yes; 5+10=15 wait that's silt+clay)
  # silt + 1.5*5 = 10+7.5 = 17.5 >= 15 OK
  # silt + 2*5 = 10+10 = 20 < 30 OK
  expect_equal(texture_class_from_pct(5,   10, 85), "LS")
})


test_that("texture_class_from_pct is NA-safe and vectorised", {
  expect_true(is.na(texture_class_from_pct(NA, 20, 60)))
  expect_true(is.na(texture_class_from_pct(20, NA, 60)))
  # (5, 5, 90): silt + 1.5*clay = 12.5 < 15 -> S
  expect_equal(
    texture_class_from_pct(c(60,  5, NA),
                              c(20,  5, 30),
                              c(20, 90, NA)),
    c("C", "S", NA))
})


# ---- 2. pedon_to_spc -------------------------------------------------

.make_simple_pedon <- function() {
  hz <- data.frame(
    designation = c("A", "Bt1", "Bt2"),
    top_cm = c(0, 20, 60), bottom_cm = c(20, 60, 120),
    munsell_hue_moist = c("10YR", "5YR", "5YR"),
    munsell_value_moist = c(4, 4, 4),
    munsell_chroma_moist = c(3, 6, 6),
    munsell_hue_dry = c(NA_character_, NA_character_, NA_character_),
    munsell_value_dry = c(NA_real_, NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_, NA_real_),
    clay_pct = c(20, 45, 50),
    silt_pct = c(20, 20, 20),
    sand_pct = c(60, 35, 30),
    ph_h2o = c(5.5, 5.0, 5.0),
    oc_pct = c(2.0, 0.5, 0.3),
    cec_cmol = c(8, 6, 5),
    base_saturation_pct = c(40, 25, 20),
    stringsAsFactors = FALSE
  )
  PedonRecord$new(site = list(id = "p-test", country = "BR"),
                    horizons = hz)
}


test_that("pedon_to_spc builds an aqp::SoilProfileCollection", {
  testthat::skip_if_not_installed("aqp")
  p <- .make_simple_pedon()
  spc <- pedon_to_spc(p)
  expect_true(inherits(spc, "SoilProfileCollection"))
  hz <- aqp::horizons(spc)
  expect_equal(nrow(hz), 3L)
  expect_equal(hz$top, c(0, 20, 60))
  expect_equal(hz$bottom, c(20, 60, 120))
  expect_equal(hz$name, c("A", "Bt1", "Bt2"))
  expect_equal(hz$texcl, c("SCL", "C", "C"))
  expect_equal(hz$clay, c(20, 45, 50))
  expect_equal(aqp::hzdesgnname(spc), "name")
})


test_that("pedon_to_spc errors on missing horizons or depths", {
  empty <- PedonRecord$new(
    site = list(id = "empty", country = "BR"),
    horizons = data.frame()
  )
  expect_error(pedon_to_spc(empty), "no horizons")

  bad_depths <- PedonRecord$new(
    site = list(id = "bad", country = "BR"),
    horizons = data.frame(
      designation = "A", top_cm = NA, bottom_cm = NA,
      stringsAsFactors = FALSE)
  )
  expect_error(pedon_to_spc(bad_depths),
                "no horizons with complete top/bottom depths")
})


# ---- 3. argic_aqp / cambic_aqp ---------------------------------------

test_that("argic_aqp returns a DiagnosticResult with the engine tag", {
  testthat::skip_if_not_installed("aqp")
  p <- .make_simple_pedon()
  res <- argic_aqp(p, require_t = FALSE)
  expect_s3_class(res, "DiagnosticResult")
  expect_equal(res$name, "argic_aqp")
  expect_true(is.logical(res$passed) || is.na(res$passed))
  expect_true("engine" %in% names(res$evidence))
  expect_match(res$evidence$engine, "getArgillicBounds")
  expect_match(res$reference, "Keys to Soil Taxonomy")
})


test_that("argic_aqp passes on a clear argic profile (clay 20 -> 50)", {
  testthat::skip_if_not_installed("aqp")
  p <- .make_simple_pedon()
  # require_t = FALSE so we don't need the "t" suffix in the
  # designation -- though "Bt1" / "Bt2" do have it. Test both ways.
  expect_true(isTRUE(argic_aqp(p, require_t = FALSE)$passed))
  expect_true(isTRUE(argic_aqp(p, require_t = TRUE)$passed))
})


test_that("argic_aqp does not pass on a homogeneous Latossolo (no clay increase)", {
  testthat::skip_if_not_installed("aqp")
  hz <- data.frame(
    designation = c("A", "Bw1", "Bw2"),
    top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 150),
    munsell_hue_moist = c("2.5YR", "2.5YR", "2.5YR"),
    munsell_value_moist = c(3, 3, 3),
    munsell_chroma_moist = c(5, 6, 6),
    munsell_hue_dry = c(NA, NA, NA),
    munsell_value_dry = c(NA_real_, NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_, NA_real_),
    clay_pct = c(58, 60, 62),     # all near-uniform -- no argic
    silt_pct = c(20, 20, 20),
    sand_pct = c(22, 20, 18),
    ph_h2o = c(5.5, 5.0, 5.0),
    oc_pct = c(2.0, 0.5, 0.3),
    cec_cmol = c(6, 4, 4),
    base_saturation_pct = c(20, 15, 15),
    stringsAsFactors = FALSE
  )
  p <- PedonRecord$new(site = list(id = "lato", country = "BR"),
                        horizons = hz)
  expect_false(isTRUE(argic_aqp(p, require_t = FALSE)$passed))
})


test_that("cambic_aqp returns a DiagnosticResult", {
  testthat::skip_if_not_installed("aqp")
  p <- .make_simple_pedon()
  res <- cambic_aqp(p)
  expect_s3_class(res, "DiagnosticResult")
  expect_equal(res$name, "cambic_aqp")
  expect_match(res$evidence$engine, "getCambicBounds")
})


test_that("cambic_aqp gracefully returns FALSE on sandy profile", {
  testthat::skip_if_not_installed("aqp")
  hz <- data.frame(
    designation = c("A", "C"),
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    munsell_hue_moist = c("10YR", "10YR"),
    munsell_value_moist = c(4, 5),
    munsell_chroma_moist = c(3, 4),
    munsell_hue_dry = c(NA, NA),
    munsell_value_dry = c(NA_real_, NA_real_),
    munsell_chroma_dry = c(NA_real_, NA_real_),
    clay_pct = c(3, 4),
    silt_pct = c(5, 5),
    sand_pct = c(92, 91),
    ph_h2o = c(5.5, 5.0),
    oc_pct = c(0.5, 0.1),
    cec_cmol = c(2, 1),
    base_saturation_pct = c(20, 15),
    stringsAsFactors = FALSE
  )
  p <- PedonRecord$new(site = list(id = "sand", country = "BR"),
                        horizons = hz)
  res <- cambic_aqp(p)
  expect_false(isTRUE(res$passed))
})


# ---- 4. compare_engines + canonical references -----------------------

test_that("compare_engines returns paired results for argic and cambic", {
  testthat::skip_if_not_installed("aqp")
  p <- .make_simple_pedon()
  out <- compare_engines(p, "argic")
  expect_named(out, c("soilkey", "aqp", "agree"))
  expect_s3_class(out$soilkey, "DiagnosticResult")
  expect_s3_class(out$aqp,     "DiagnosticResult")
  expect_true(is.logical(out$agree))

  out2 <- compare_engines(p, "cambic")
  expect_named(out2, c("soilkey", "aqp", "agree"))
})


test_that("canonical_reference loads WRB_4th_2022 with both backends", {
  # Default: prefer SoilTaxonomy if installed
  if (requireNamespace("SoilTaxonomy", quietly = TRUE)) {
    out <- canonical_reference("WRB_4th_2022", prefer_pkg = TRUE)
    expect_named(out, c("rsg", "pq", "sq"))
    expect_equal(nrow(out$rsg), 118L)
  }
  # Force vendored
  out2 <- canonical_reference("WRB_4th_2022", prefer_pkg = FALSE)
  expect_named(out2, c("rsg", "pq", "sq"))
  expect_equal(nrow(out2$rsg), 118L)
})


test_that("kst13_canonical and st_features_canonical return expected shapes", {
  kst <- kst13_canonical(prefer_pkg = FALSE)
  expect_type(kst, "list")
  expect_true(length(kst) > 100L)  # 3,153 nested entries

  feat <- st_features_canonical(prefer_pkg = FALSE)
  expect_s3_class(feat, "data.frame")
  expect_equal(nrow(feat), 84L)
  expect_true(all(c("group", "name", "chapter", "page",
                      "description", "criteria") %in% names(feat)))
})
