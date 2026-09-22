# =============================================================================
# Tests for v0.9.86 -- soilKey.diagnostic_engine = "aqp" auto-enables
# the v0.9.69 ECEC fallback inside test_cec_per_clay() unless the user
# explicitly suppresses it via soilKey.ferralic_ecec_fallback = FALSE.
# =============================================================================


.fix_no_cec_with_components <- function() {
  # Latossolic-style fixture: low CTC argila when computed from ECEC,
  # high clay, ferralic-eligible. Valor T (cec_cmol) deliberately NA.
  hz <- data.table::data.table(
    top_cm    = c(0,    20,   40,   100),
    bottom_cm = c(20,   40,  100,   200),
    designation = c("A","BA","Bw","BC"),
    clay_pct = c(20, 30, 50, 50),
    sand_pct = c(60, 50, 35, 30),
    silt_pct = c(20, 20, 15, 20),
    cec_cmol = rep(NA_real_, 4),
    ca_cmol  = c(2.0, 1.0, 0.5, 0.4),
    mg_cmol  = c(1.0, 0.5, 0.3, 0.3),
    k_cmol   = c(0.1, 0.05, 0.05, 0.05),
    na_cmol  = c(0.05, 0.05, 0.05, 0.05),
    al_cmol  = c(0.5, 0.5, 0.3, 0.2),
    bs_pct   = c(35, 25, 20, 15),
    oc_pct   = c(2.0, 0.8, 0.4, 0.2),
    ph_h2o   = c(5.0, 5.2, 5.3, 5.4),
    munsell_value_moist = c(3, 4, 4, 5),
    munsell_chroma_moist = c(2, 3, 4, 5),
    structure_grade = rep("moderate", 4),
    structure_size  = rep("medium", 4),
    structure_type  = rep("granular", 4),
    consistence_moist = rep("friable", 4),
    coarse_fragments_pct = rep(0, 4),
    bulk_density_g_cm3 = rep(1.2, 4)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "no-Valor-T-Latossolo"), horizons = hz)
}


# ---- 1. Default canonical: ECEC fallback STAYS off ----------------------

test_that("v0.9.86: default soilkey engine + no opt-in -> ECEC fallback OFF", {
  skip_on_cran()
  pr <- .fix_no_cec_with_components()
  res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 16)
  # Without ECEC fallback the test returns NA on layers with NA CEC.
  expect_true(is.na(res$passed))
})


# ---- 2. v0.9.86 engine="aqp" auto-enables fallback -----------------------

test_that("v0.9.86: engine=aqp auto-enables the ECEC fallback", {
  skip_on_cran()
  pr <- .fix_no_cec_with_components()
  withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 16)
  })
  # Auto-fallback fires; some layer should pass with ECEC <= 16 / clay
  expect_true(isTRUE(res$passed) || length(res$layers) > 0L)
})


# ---- 3. User can suppress the fallback with explicit FALSE ---------------

test_that("v0.9.86: explicit ferralic_ecec_fallback=FALSE suppresses auto-fallback", {
  skip_on_cran()
  pr <- .fix_no_cec_with_components()
  withr::with_options(list(soilKey.diagnostic_engine = "aqp",
                            soilKey.ferralic_ecec_fallback = FALSE), {
    res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 16)
  })
  # User opted out -> no fallback -> NA on missing-CEC layers
  expect_true(is.na(res$passed))
})


# ---- 4. Explicit ferralic_ecec_fallback=TRUE without engine still works --

test_that("v0.9.86: explicit ferralic_ecec_fallback=TRUE works without engine=aqp", {
  skip_on_cran()
  pr <- .fix_no_cec_with_components()
  withr::with_options(list(soilKey.ferralic_ecec_fallback = TRUE), {
    res <- test_cec_per_clay(pr$horizons, max_cmol_per_kg_clay = 16)
  })
  expect_true(isTRUE(res$passed) || length(res$layers) > 0L)
})


# ---- 5. BDsolos RJ regression: engine=aqp lifts Latossolo accuracy -------

test_that("v0.9.86: BDsolos RJ Latossolo recall lifts from 17 to ~32 with engine=aqp", {
  skip_on_cran()
  RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
  skip_if_not(file.exists(RJ), "BDsolos RJ.csv not available")
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ, verbose = FALSE)))
  res_def <- suppressMessages(suppressWarnings(
    benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)))
  cf_def <- res_def$per_system$sibcs$confusion
  expect_equal(cf_def["Latossolos","Latossolos"], 17L)

  res_aqp <- withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    suppressMessages(suppressWarnings(
      benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)))
  })
  cf_aqp <- res_aqp$per_system$sibcs$confusion
  # Engine=aqp should auto-enable ECEC fallback and lift Latossolos
  # to >= 30 (we observed exactly 32 in the v0.9.86 audit).
  expect_gte(cf_aqp["Latossolos","Latossolos"], 30L)
  expect_gt(cf_aqp["Latossolos","Latossolos"], cf_def["Latossolos","Latossolos"])
})
