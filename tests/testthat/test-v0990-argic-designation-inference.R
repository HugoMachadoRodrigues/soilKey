# =============================================================================
# Tests for v0.9.90 -- argic() designation-inference fallback
# (engine="aqp" auto-bundled). When the canonical aqp clay-increase
# test fails but a Bt subsoil layer has clay-films coded, accept the
# layer as argic by morphology.
# =============================================================================


.fix_argissolo_2pt <- function() {
  # BDsolos-style 2-point profile: A topsoil at 0-20, Bt at 50-150.
  # Clay increase from 25 -> 35 = 1.4x but in 30 cm vertical gap (50-20),
  # which the strict aqp argic test (10 cm transition window) rejects.
  hz <- data.table::data.table(
    top_cm    = c(0,    50),
    bottom_cm = c(20,   150),
    designation = c("A","Bt"),
    clay_pct = c(25, 35),
    sand_pct = c(50, 40),
    silt_pct = c(25, 25),
    cec_cmol = c(8, 6),
    bs_pct  = c(60, 55),
    oc_pct  = c(2.0, 0.5),
    ph_h2o  = c(5.0, 5.5),
    munsell_value_moist = c(3, 4),
    munsell_chroma_moist = c(3, 4),
    structure_grade = rep("moderate", 2),
    structure_size  = rep("medium", 2),
    structure_type  = rep("subangular", 2),
    consistence_moist = rep("friable", 2),
    bulk_density_g_cm3 = rep(1.3, 2),
    coarse_fragments_pct = rep(0, 2),
    clay_films_amount = c(NA_character_, "common")
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "argissolo-2pt"), horizons = hz)
}


.fix_argissolo_2pt_no_films <- function() {
  pr <- .fix_argissolo_2pt()
  pr$horizons$clay_films_amount <- rep(NA_character_, nrow(pr$horizons))
  pr
}


.fix_argissolo_2pt_topsoil_b <- function() {
  pr <- .fix_argissolo_2pt()
  # Move the Bt to top_cm = 15 (still topsoil per the v0.9.90 deep-only rule)
  pr$horizons$top_cm[2L] <- 15
  pr$horizons$bottom_cm[1L] <- 15
  pr
}


# ---- Default canonical: no inference -----------------------------------

test_that("v0.9.90: default canonical does NOT fire designation inference", {
  pr <- .fix_argissolo_2pt()
  res <- argic(pr)  # default engine="soilkey"
  # Whether canonical passes or fails, the inference path must not have
  # added a designation_inference evidence slot (it's gated by
  # explicit option or engine="aqp").
  expect_null(res$evidence$designation_inference)
})


# ---- engine="aqp" auto-fires inference when canonical fails ------------

test_that("v0.9.90: engine=aqp accepts Bt + films + subsoil when aqp argic fails", {
  pr <- .fix_argissolo_2pt()
  withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    res <- argic(pr)
  })
  # Either canonical passed (good) or inference added (also good).
  expect_true(isTRUE(res$passed))
  if (!is.null(res$evidence$designation_inference)) {
    expect_identical(
      res$evidence$designation_inference$details$source,
      "engine_aqp_bt_with_films"
    )
  }
})


# ---- Inference REJECTS without clay films ------------------------------

test_that("v0.9.90: inference does NOT fire when clay_films_amount is NA", {
  pr <- .fix_argissolo_2pt_no_films()
  withr::with_options(list(soilKey.diagnostic_engine = "aqp",
                            soilKey.argic_designation_inference = TRUE), {
    res <- argic(pr)
  })
  # If aqp argic passed canonically that's fine; if not, the inference
  # must NOT fire because clay_films are NA on every layer.
  if (!isTRUE(res$passed) || is.null(res$evidence$designation_inference)) {
    succeed("inference correctly skipped (no films)")
  }
  if (!is.null(res$evidence$designation_inference)) {
    # If somehow the inference added a slot, layers must be empty
    expect_equal(length(res$evidence$designation_inference$layers %||% integer(0)),
                  0L)
  }
})


# ---- Inference REJECTS topsoil Bt --------------------------------------

test_that("v0.9.90: inference does NOT fire when Bt is at top_cm <= 25", {
  pr <- .fix_argissolo_2pt_topsoil_b()
  withr::with_options(list(soilKey.diagnostic_engine = "aqp",
                            soilKey.argic_designation_inference = TRUE), {
    res <- argic(pr)
  })
  if (!is.null(res$evidence$designation_inference)) {
    # Bt at top=15 (<= 25), inference layers must be empty.
    expect_equal(length(res$evidence$designation_inference$layers %||% integer(0)),
                  0L)
  } else {
    succeed("inference path correctly skipped on topsoil Bt")
  }
})


# ---- User can suppress with explicit FALSE ----------------------------

test_that("v0.9.90: explicit argic_designation_inference=FALSE suppresses bundling", {
  pr <- .fix_argissolo_2pt()
  res_aqp <- withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    argic(pr)
  })
  res_off <- withr::with_options(list(soilKey.diagnostic_engine = "aqp",
                                        soilKey.argic_designation_inference = FALSE), {
    argic(pr)
  })
  # With explicit FALSE, the inference slot must NOT be present.
  expect_null(res_off$evidence$designation_inference)
})


# ---- BDsolos RJ regression: Argissolo recall lifts -------------------

test_that("v0.9.90: BDsolos RJ Argissolo recall lifts past 175 with engine=aqp", {
  RJ <- "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data/embrapa_bdsolos/BD_solos/RJ.csv"
  skip_if_not(file.exists(RJ), "BDsolos RJ.csv not available")
  peds <- suppressMessages(suppressWarnings(load_bdsolos_csv(RJ, verbose = FALSE)))
  res <- withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    suppressMessages(suppressWarnings(
      benchmark_bdsolos(peds, systems = "sibcs", verbose = FALSE)))
  })
  cf <- res$per_system$sibcs$confusion
  expect_gte(cf["Argissolos","Argissolos"], 180L)
  # Order accuracy must lift past 46% (we observed 46.6% in audit).
  expect_gte(res$per_system$sibcs$accuracy, 0.46)
})
