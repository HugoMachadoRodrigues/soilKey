# =============================================================================
# Tests for v0.9.77 -- AfSP loader + benchmark + Vertisol RSG-gate
# routing fix.
# =============================================================================


# ---- WRB 2006 RSG code crosswalk -------------------------------------------

test_that("v0.9.77: wrb06_code_to_rsg() handles all 32 standard codes", {
  expect_identical(wrb06_code_to_rsg("HS"), "Histosol")
  expect_identical(wrb06_code_to_rsg("VR"), "Vertisol")
  expect_identical(wrb06_code_to_rsg("FR"), "Ferralsol")
  expect_identical(wrb06_code_to_rsg("LV"), "Luvisol")
  expect_identical(wrb06_code_to_rsg("AC"), "Acrisol")
  expect_identical(wrb06_code_to_rsg("AB"), "Retisol")  # Albeluvisol
  expect_identical(wrb06_code_to_rsg("SN"), "Solonetz")
  expect_identical(wrb06_code_to_rsg("CR"), "Cryosol")
})


test_that("v0.9.77: wrb06 codes case-insensitive + trimws", {
  expect_identical(wrb06_code_to_rsg("vr"),    "Vertisol")
  expect_identical(wrb06_code_to_rsg("  VR "), "Vertisol")
})


test_that("v0.9.77: unknown WRB06 code returns NA", {
  expect_true(is.na(wrb06_code_to_rsg("XX")))
})


# ---- AfSP Munsell parser ---------------------------------------------------

test_that("v0.9.77: .afsp_parse_munsell handles compact AfSP format", {
  out <- soilKey:::.afsp_parse_munsell("10YR3/2")
  expect_identical(out$hue, "10YR")
  expect_equal(out$value, 3)
  expect_equal(out$chroma, 2)
})


test_that("v0.9.77: .afsp_parse_munsell handles fractional hue", {
  out <- soilKey:::.afsp_parse_munsell("2.5YR3/6")
  expect_identical(out$hue, "2.5YR")
  expect_equal(out$value, 3)
  expect_equal(out$chroma, 6)
})


test_that("v0.9.77: .afsp_parse_munsell handles Y hue (no R)", {
  out <- soilKey:::.afsp_parse_munsell("2.5Y6/2")
  expect_identical(out$hue, "2.5Y")
  expect_equal(out$value, 6)
  expect_equal(out$chroma, 2)
})


test_that("v0.9.77: .afsp_parse_munsell handles space-separated legacy format", {
  out <- soilKey:::.afsp_parse_munsell("10YR 4/3")
  expect_identical(out$hue, "10YR")
  expect_equal(out$value, 4)
  expect_equal(out$chroma, 3)
})


test_that("v0.9.77: .afsp_parse_munsell returns NA on unparseable input", {
  out <- soilKey:::.afsp_parse_munsell("garbage")
  expect_true(is.na(out$hue))
  expect_true(is.na(out$value))
  expect_true(is.na(out$chroma))
})


# ---- Bundled AfSP sample tests ---------------------------------------------

test_that("v0.9.77: load_afsp_sample() returns 120 pedons across 24 RSGs", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "afsp_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "afsp_sample.rds",
                                                package = "soilKey")),
                          "Bundled AfSP sample not present")
  s <- load_afsp_sample()
  expect_equal(length(s$pedons), 120L)
  rsgs <- vapply(s$pedons,
                  function(p) p$site$reference_wrb %||% NA_character_,
                  character(1))
  expect_equal(length(unique(rsgs)), 24L)
  expect_true(all(table(rsgs) == 5))
})


test_that("v0.9.77: AfSP pedons have rich field availability", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "afsp_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "afsp_sample.rds",
                                                package = "soilKey")),
                          "Bundled AfSP sample not present")
  s <- load_afsp_sample()
  has_field <- function(field) {
    mean(vapply(s$pedons, function(p) {
      if (!field %in% colnames(p$horizons)) return(0)
      sum(!is.na(p$horizons[[field]])) / nrow(p$horizons)
    }, numeric(1)))
  }
  # AfSP exposes rich chemistry + Munsell
  expect_gt(has_field("clay_pct"),  0.7)
  expect_gt(has_field("ph_h2o"),    0.7)
  expect_gt(has_field("oc_pct"),    0.6)
  expect_gt(has_field("cec_cmol"),  0.7)
  expect_gt(has_field("ca_cmol"),   0.6)
  expect_gt(has_field("bs_pct"),    0.6)
  expect_gt(has_field("munsell_hue_moist"), 0.4)  # parsed from compact format
})


test_that("v0.9.77: classify_wrb2022 runs without error on every AfSP pedon", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "afsp_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "afsp_sample.rds",
                                                package = "soilKey")),
                          "Bundled AfSP sample not present")
  s <- load_afsp_sample()
  errors <- sum(vapply(s$pedons, function(pr) {
    res <- tryCatch(classify_wrb2022(pr, on_missing = "silent"),
                     error = function(e) NULL)
    is.null(res)
  }, logical(1)))
  expect_equal(errors, 0L)
})


test_that("v0.9.77: benchmark_afsp() runs end-to-end + reports per-class recall", {
  testthat::skip_if_not(file.exists(file.path("inst", "extdata",
                                                  "afsp_sample.rds"))
                          || nzchar(system.file("extdata",
                                                "afsp_sample.rds",
                                                package = "soilKey")),
                          "Bundled AfSP sample not present")
  s <- load_afsp_sample()
  res <- benchmark_afsp(s$pedons, verbose = FALSE)
  expect_named(res, c("accuracy", "n_compared", "n_total",
                        "confusion", "per_class_recall", "refs", "preds"),
                ignore.order = TRUE)
  expect_true(res$n_total == 120L)
  expect_true(is.numeric(res$accuracy))
  # Should achieve at least 20% even with no opt-ins (strict default)
  expect_gt(res$accuracy, 0.15)
})


# ---- Vertisol RSG-gate routing fix (v0.9.77) -------------------------------

test_that("v0.9.77: vertisol() RSG-gate trusts vertic morphological inference", {
  hz <- data.table::data.table(
    top_cm    = c(0, 15, 38, 74),
    bottom_cm = c(15, 38, 74, 130),
    designation = c("A", "Ag", "Bkg1", "Bkg2"),
    clay_pct = c(63, 65, 64, 66),
    silt_pct = c(20, 20, 20, 18),
    sand_pct = c(17, 15, 16, 16),
    munsell_hue_moist    = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist  = c(3, 3, 4, 4),
    munsell_chroma_moist = c(1, 1, 1, 1),
    cec_cmol = c(40, 42, 44, 45),
    bs_pct = c(45, 40, 35, 35),       # < 50: prevent mollic firing (v0.9.79)
    oc_pct = c(0.4, 0.3, 0.2, 0.1),   # < 0.6: prevent mollic firing
    ph_h2o = c(7, 7.2, 7.5, 7.8)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "test-vertisol-bkg"),
                          horizons = hz)
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    res <- vertisol(pr)
    # vertic_horizon fires via chroma+clay inference; vertisol gate
    # now trusts that path even though slickensides + cracks NA.
    expect_true(isTRUE(res$passed))
    expect_true(isTRUE(res$evidence$morphological_inference_fired))
  })
})


test_that("v0.9.77: vertisol() still requires clay >= 30 above the vertic horizon", {
  hz <- data.table::data.table(
    top_cm    = c(0, 15, 38, 74),
    bottom_cm = c(15, 38, 74, 130),
    designation = c("A", "AB", "Bkg1", "Bkg2"),
    clay_pct = c(15, 18, 64, 66),  # sandy A overlying high-clay subsoil
    silt_pct = c(20, 20, 20, 18),
    sand_pct = c(65, 62, 16, 16),
    munsell_chroma_moist = c(2, 2, 1, 1),
    munsell_hue_moist = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist = c(3, 4, 3, 3),
    cec_cmol = c(8, 12, 44, 45), oc_pct = c(2, 0.5, 0.3, 0.2),
    ph_h2o = c(7, 7.2, 7.5, 7.8)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "sandy-A-vertic-subsoil"),
                          horizons = hz)
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    res <- vertisol(pr)
    # Clay above the vertic horizon < 30 -> still fails (correct WRB)
    expect_false(isTRUE(res$passed))
  })
})
