# =============================================================================
# Tests for v0.9.89 -- soilKey.diagnostic_engine = "aqp" auto-enables
# the v0.9.70 ferralic_texture_morphological_fallback (same tri-state
# precedence as the v0.9.86 ECEC fallback bundling).
# =============================================================================


.fix_no_texture_with_Bw <- function() {
  hz <- data.table::data.table(
    top_cm    = c(0,    25,   60,   100),
    bottom_cm = c(25,   60,  100,   200),
    designation = c("Ap","BA","Bw","Bo"),
    clay_pct = rep(NA_real_, 4),  # texture missing on every layer
    sand_pct = rep(NA_real_, 4),
    silt_pct = rep(NA_real_, 4),
    cec_cmol = c(8, 6, 5, 4),
    bs_pct  = c(35, 30, 25, 20),
    oc_pct  = c(2.0, 0.8, 0.4, 0.2),
    ph_h2o  = c(5.0, 5.2, 5.3, 5.4),
    munsell_value_moist = c(3, 4, 4, 5),
    munsell_chroma_moist = c(2, 3, 4, 5),
    structure_grade = rep("moderate", 4),
    structure_size  = rep("medium", 4),
    structure_type  = rep("granular", 4),
    consistence_moist = rep("friable", 4),
    bulk_density_g_cm3 = rep(1.2, 4),
    coarse_fragments_pct = rep(0, 4)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "Bw-no-texture"), horizons = hz)
}


# ---- 1. Default canonical: morph fallback STAYS off ----------------------

test_that("v0.9.89: default soilkey engine + no opt-in -> texture fallback OFF", {
  skip_on_cran()
  pr <- .fix_no_texture_with_Bw()
  res <- test_ferralic_texture(pr$horizons)
  expect_true(is.na(res$passed))
})


# ---- 2. v0.9.89 engine="aqp" auto-enables fallback -----------------------

test_that("v0.9.89: engine=aqp auto-enables the texture morphological fallback", {
  skip_on_cran()
  pr <- .fix_no_texture_with_Bw()
  withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    res <- test_ferralic_texture(pr$horizons)
  })
  expect_true(isTRUE(res$passed))
  expect_identical(res$details$source, "morphological_fallback")
})


# ---- 3. User can suppress the fallback with explicit FALSE ---------------

test_that("v0.9.89: explicit ferralic_texture_morphological_fallback=FALSE suppresses", {
  skip_on_cran()
  pr <- .fix_no_texture_with_Bw()
  withr::with_options(list(soilKey.diagnostic_engine = "aqp",
                            soilKey.ferralic_texture_morphological_fallback = FALSE), {
    res <- test_ferralic_texture(pr$horizons)
  })
  expect_true(is.na(res$passed))
})


# ---- 4. Explicit TRUE works without engine=aqp --------------------------

test_that("v0.9.89: explicit ferralic_texture_morphological_fallback=TRUE works without engine", {
  skip_on_cran()
  pr <- .fix_no_texture_with_Bw()
  withr::with_options(list(soilKey.ferralic_texture_morphological_fallback = TRUE), {
    res <- test_ferralic_texture(pr$horizons)
  })
  expect_true(isTRUE(res$passed))
  expect_identical(res$details$source, "morphological_fallback")
})


# ---- 5. BDsolos RJ regression: Latossolo recall lifts from 32 to 33 -----

test_that("v0.9.89: BDsolos RJ Latossolo recall reaches 33 with engine=aqp", {
  skip_on_cran()
  RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
  skip_if_not(file.exists(RJ), "BDsolos RJ.csv not available")
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ, verbose = FALSE)))
  res_aqp <- withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    suppressMessages(suppressWarnings(
      benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)))
  })
  cf_aqp <- res_aqp$per_system$sibcs$confusion
  # Engine=aqp now bundles ECEC + texture fallback; Latossolo recall
  # should reach >= 33 (we observed exactly 33 in the v0.9.89 audit).
  expect_gte(cf_aqp["Latossolos","Latossolos"], 33L)
})
