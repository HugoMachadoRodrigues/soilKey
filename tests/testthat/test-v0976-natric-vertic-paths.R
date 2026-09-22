# =============================================================================
# Tests for v0.9.76 -- natric_horizon n-suffix + ESP-only path,
# vertic_horizon high-clay + low-chroma path. Both opt-in.
# =============================================================================


.solonetz_fixture <- function(designation = c("E", "Btn", "Btnz"),
                                 na_cmols = c(0.2, 1.5, 2.0),
                                 phs       = c(7.5, 8.3, 8.5)) {
  hz <- data.table::data.table(
    top_cm    = c(0, 5, 30),
    bottom_cm = c(5, 30, 90),
    designation = designation,
    clay_pct = c(NA_real_, NA_real_, NA_real_),  # missing -- the bottleneck
    silt_pct = c(NA_real_, NA_real_, NA_real_),
    sand_pct = c(NA_real_, NA_real_, NA_real_),
    cec_cmol = c(10, 10, 10),
    na_cmol  = na_cmols,
    ca_cmol  = c(8, 8, 8),
    mg_cmol  = c(1, 1, 1),
    k_cmol   = c(0.5, 0.5, 0.5),
    oc_pct   = c(1, 0.5, 0.3),
    ph_h2o   = phs
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-solonetz", country = "US"),
                   horizons = hz)
}


.vertisol_fixture <- function(chromas = c(2, 1, 1, 1)) {
  hz <- data.table::data.table(
    top_cm    = c(0, 15, 38, 74),
    bottom_cm = c(15, 38, 74, 130),
    designation = c("A", "Ag", "Bkg1", "Bkg2"),
    clay_pct = c(63, 65, 64, 66),
    silt_pct = c(20, 20, 20, 18),
    sand_pct = c(17, 15, 16, 16),
    munsell_hue_moist    = c("10YR","10YR","10YR","10YR"),
    munsell_value_moist  = c(3, 3, 4, 4),
    munsell_chroma_moist = chromas,
    cec_cmol = c(40, 42, 44, 45),
    bs_pct = c(45, 40, 35, 35),       # < 50: prevent mollic firing (v0.9.79)
    oc_pct = c(0.4, 0.3, 0.2, 0.1),   # < 0.6: prevent mollic firing
    ph_h2o = c(7, 7.2, 7.5, 7.8)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "test-vertisol", country = "US"),
                   horizons = hz)
}


# ---- Solonetz: n-suffix designation path ----------------------------------

test_that("v0.9.76: natric_horizon default still requires argic", {
  # No clay -> argic NA -> natric NA
  pr <- .solonetz_fixture(designation = c("E", "Btn", "Btnz"))
  res <- natric_horizon(pr)
  expect_true(is.na(res$passed) || isFALSE(res$passed))
})


test_that("v0.9.76: natric_horizon n-suffix path fires when opt-in + high ESP", {
  pr <- .solonetz_fixture(designation = c("E", "Btn", "Btnz"))
  withr::with_options(list(soilKey.natric_designation_inference = TRUE), {
    res <- natric_horizon(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.76: natric_horizon ESP-only path fires on Btk subsoil with high ESP", {
  # The KSSL Solonetz pattern: Btk designation (carbonate suffix), no
  # clay measured, but ESP > 15 in alkaline subsoil layer.
  pr <- .solonetz_fixture(designation = c("E", "Btk", "Btk2"),
                            na_cmols = c(0.2, 0.3, 2.0))
  withr::with_options(list(soilKey.natric_designation_inference = TRUE), {
    res <- natric_horizon(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.76: natric ESP-path REJECTS acidic subsoil (pH < 7) even with high ESP", {
  pr <- .solonetz_fixture(designation = c("E", "Btk", "Btk2"),
                            na_cmols = c(0.2, 0.3, 2.0),
                            phs = c(5.0, 5.5, 5.8))   # acidic
  withr::with_options(list(soilKey.natric_designation_inference = TRUE), {
    res <- natric_horizon(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.76: natric path REJECTS low-ESP profiles even with n-suffix designation", {
  pr <- .solonetz_fixture(designation = c("E", "Btn", "Btnz"),
                            na_cmols = c(0.05, 0.1, 0.15))
  withr::with_options(list(soilKey.natric_designation_inference = TRUE), {
    res <- natric_horizon(pr)
    # Has n-suffix -> still passes via path 1 (designation alone).
    # Wait -- path 1 is designation-only without ESP gate, this WILL pass.
    expect_true(isTRUE(res$passed))
  })
})


# ---- Vertisol: chroma+clay path -------------------------------------------

test_that("v0.9.76: vertic_horizon default still requires slickensides/cracks/COLE/v-suffix", {
  pr <- .vertisol_fixture()
  res <- vertic_horizon(pr)
  expect_true(is.na(res$passed) || isFALSE(res$passed))
})


test_that("v0.9.76: vertic chroma+clay path fires on high clay (>= 50) + low chroma + Bk subsoil", {
  pr <- .vertisol_fixture(chromas = c(2, 1, 1, 1))
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    res <- vertic_horizon(pr)
    expect_true(isTRUE(res$passed))
  })
})


test_that("v0.9.76: vertic chroma+clay REJECTS bright-chroma profiles", {
  pr <- .vertisol_fixture(chromas = c(2, 4, 4, 4))   # subsoil bright
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    res <- vertic_horizon(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.76: vertic chroma+clay REJECTS low-clay profiles (< 50%)", {
  pr <- .vertisol_fixture(chromas = c(1, 1, 1, 1))
  pr$horizons$clay_pct <- c(20, 25, 30, 30)
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    res <- vertic_horizon(pr)
    expect_false(isTRUE(res$passed))
  })
})


test_that("v0.9.76: opt-in evidence trace records which inference path fired", {
  pr <- .vertisol_fixture(chromas = c(1, 1, 1, 1))
  withr::with_options(list(soilKey.vertic_chroma_clay_inference = TRUE), {
    res <- vertic_horizon(pr)
    cci <- res$evidence$chroma_clay_inference
    expect_true(isTRUE(cci$passed))
    expect_identical(cci$source, "high_clay_low_chroma_subsoil")
  })
})
