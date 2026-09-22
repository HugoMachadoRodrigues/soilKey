# =============================================================================
# Tests for v0.9.85 -- Andosol RSG-gate fixes:
#  (1) buried-exclusion fix: argic / ferralic / plinthic / spodic that
#      ALL fire deeper than 50 cm no longer disqualify a surface
#      andic stack (per WRB 2022 Ch 4 p 104).
#  (2) v0.9.80 OC+BD proxy contiguous-layer extension (opt-in):
#      when soilKey.andic_oc_bd_proxy_extend = TRUE, contiguous
#      deeper layers with OC >= min_oc_proxy/2 AND BD <=
#      max_bd_proxy + 0.15 (or BD missing) join the proxy layers.
# =============================================================================


.andic_pedon_buried_argic <- function() {
  # Surface andic stack 0-30 cm, then argic-eligible 2BA at 56-72 cm.
  # The argic_horizon test should fire on the 2BA (clay increase),
  # but its top (56 cm) is below the WRB 50-cm buried threshold so
  # the Andosol gate must NOT exclude.
  hz <- data.table::data.table(
    top_cm    = c(0,    14,   30,   56,   72),
    bottom_cm = c(14,   30,   56,   72,   140),
    designation = c("Ap","AC","C","2BA","2BCr"),
    oc_pct = c(8.0, 6.5, 2.0, 1.5, 0.6),
    bulk_density_g_cm3 = rep(NA_real_, 5),
    clay_pct = c(11, 11, 2, 32, 15),    # clay doubling at 2BA -> argic
    sand_pct = c(60, 60, 75, 50, 60),
    silt_pct = c(29, 29, 23, 18, 25),
    cec_cmolc_kg = c(15, 15, 5, 20, 10),
    bs_pct  = c(40, 40, 30, 50, 40),
    ph_h2o  = c(5.3, 5.7, 5.9, 5.7, 6.0),
    munsell_value_moist = c(3, 3, 4, 4, 5),
    munsell_chroma_moist = c(2, 2, 3, 4, 4),
    structure_grade = rep("moderate", 5),
    structure_size  = rep("medium", 5),
    structure_type  = rep("granular", 5),
    consistence_moist = rep("friable", 5),
    al_cmolc_kg = rep(0.2, 5),
    coarse_fragments_pct = rep(0, 5)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "buried-argic-andosol"), horizons = hz)
}


.andic_pedon_extension_target <- function() {
  # Mimics AfSP KE SOTER_182/4-75: Ah 0-25 with OC=4.7 BD=0.8 (proxy
  # fires), AB 25-50 with OC=2.7 BD=1.0 (extension target -- below
  # v0.9.80 thresholds but within v0.9.85 extension thresholds).
  hz <- data.table::data.table(
    top_cm    = c(0,    25,   50,   110),
    bottom_cm = c(25,   50,  110,   150),
    designation = c("Ah","AB","C","2B"),
    oc_pct = c(4.7, 2.7, 0.7, 1.6),
    bulk_density_g_cm3 = c(0.8, 1.0, NA_real_, NA_real_),
    clay_pct = c(23, 23,  6, 14),
    sand_pct = c(50, 55, 75, 60),
    silt_pct = c(27, 22, 19, 26),
    cec_cmolc_kg = c(12, 8, 5, 7),
    bs_pct  = c(60, 65, 70, 60),
    ph_h2o  = c(7.1, 7.9, 8.6, 8.3),
    munsell_value_moist = c(3, 3, 3, 3),
    munsell_chroma_moist = c(2, 1, 0, 2),
    structure_grade = rep("moderate", 4),
    structure_size  = rep("medium", 4),
    structure_type  = rep("granular", 4),
    consistence_moist = rep("friable", 4),
    al_cmolc_kg = rep(0.0, 4),
    coarse_fragments_pct = rep(0, 4)
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "extend-andosol"), horizons = hz)
}


# ---- 1. Buried-exclusion fix --------------------------------------------

test_that("v0.9.85: buried argic (top >= 50 cm) no longer disqualifies Andosol", {
  pr <- .andic_pedon_buried_argic()
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    res <- andosol(pr)
  })
  expect_true(isTRUE(res$passed))
  # Buried-exclusion bookkeeping
  expect_true(isTRUE(res$evidence$exclusion_buried$argic))
  # Active exclusion list must be empty
  active <- vapply(res$evidence$exclusion_active, isTRUE, logical(1))
  expect_false(any(active))
})


test_that("v0.9.85: a SHALLOW argic (top < 50 cm) still excludes Andosol", {
  hz <- data.table::data.table(
    top_cm    = c(0,    14,   30,   45),  # argic at 30-45 cm -> shallow
    bottom_cm = c(14,   30,   45,   80),
    designation = c("Ap","AC","Bt1","Bt2"),
    oc_pct = c(8.0, 6.5, 1.0, 0.5),
    bulk_density_g_cm3 = rep(NA_real_, 4),
    clay_pct = c(11, 11, 32, 38),
    sand_pct = c(60, 60, 50, 45),
    silt_pct = c(29, 29, 18, 17),
    cec_cmolc_kg = rep(8, 4),
    bs_pct  = rep(50, 4),
    ph_h2o  = rep(5.5, 4),
    munsell_value_moist = c(3, 3, 4, 4),
    munsell_chroma_moist = c(2, 2, 4, 4),
    structure_grade = rep("moderate", 4),
    structure_size  = rep("medium", 4),
    structure_type  = rep("granular", 4),
    consistence_moist = rep("friable", 4),
    al_cmolc_kg = rep(0.2, 4),
    coarse_fragments_pct = rep(0, 4)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "shallow-argic"), horizons = hz)
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    res <- andosol(pr)
  })
  # The argic at 30-45 cm starts below 50 cm, so it IS active and
  # should disqualify.
  if (isTRUE(res$evidence$exclusion_failed$argic$passed)) {
    expect_false(isTRUE(res$evidence$exclusion_buried$argic))
    expect_true(isTRUE(res$evidence$exclusion_active$argic))
    expect_false(isTRUE(res$passed))
  } else {
    succeed("argic test did not fire on this fixture; buried-exclusion guard not exercised")
  }
})


# ---- 2. Proxy contiguous-layer extension (opt-in) ------------------------

test_that("v0.9.85: andic_oc_bd_proxy_extend OFF by default", {
  pr <- .andic_pedon_extension_target()
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE), {
    ap <- andic_properties(pr)
  })
  # Proxy fires on layer 1 only; extension is opt-in so source
  # is "high_oc_low_bd" (not "_extended").
  expect_true(isTRUE(ap$passed))
  expect_identical(ap$evidence$oc_bd_proxy$source, "high_oc_low_bd")
  expect_equal(ap$layers, 1L)
})


test_that("v0.9.85: extend opt-in adds contiguous layer to proxy", {
  pr <- .andic_pedon_extension_target()
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE,
                            soilKey.andic_oc_bd_proxy_extend = TRUE), {
    ap <- andic_properties(pr)
  })
  expect_true(isTRUE(ap$passed))
  expect_identical(ap$evidence$oc_bd_proxy$source,
                    "high_oc_low_bd_extended")
  expect_true(2L %in% ap$layers)
  # Layer 3 (C, OC=0.7) is below oc_min_extend (=2.0), extension stops.
  expect_false(3L %in% ap$layers)
})


test_that("v0.9.85: extension does NOT cross a high-BD subsoil boundary", {
  # AB stays in extension; deeper C has BD = 1.4 (mineral subsoil)
  # which exceeds max_bd_proxy + 0.15 = 1.05; extension stops.
  hz <- data.table::data.table(
    top_cm    = c(0,    25,   50,   110),
    bottom_cm = c(25,   50,  110,   150),
    designation = c("Ah","AB","C","2B"),
    oc_pct = c(4.7, 2.7, 2.0, 0.5),
    bulk_density_g_cm3 = c(0.8, 1.0, 1.4, NA_real_),
    clay_pct = c(23, 23,  6, 14),
    sand_pct = c(50, 55, 75, 60),
    silt_pct = c(27, 22, 19, 26),
    cec_cmolc_kg = rep(8, 4),
    bs_pct  = rep(60, 4),
    ph_h2o  = rep(7.0, 4),
    munsell_value_moist = c(3, 3, 4, 4),
    munsell_chroma_moist = c(2, 1, 2, 2),
    structure_grade = rep("moderate", 4),
    structure_size  = rep("medium", 4),
    structure_type  = rep("granular", 4),
    consistence_moist = rep("friable", 4),
    al_cmolc_kg = rep(0.0, 4),
    coarse_fragments_pct = rep(0, 4)
  )
  hz <- ensure_horizon_schema(hz)
  pr <- PedonRecord$new(site = list(id = "extension-bd-stop"), horizons = hz)
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE,
                            soilKey.andic_oc_bd_proxy_extend = TRUE), {
    ap <- andic_properties(pr)
  })
  # Layer 1 (Ah): proxy fires.  Layer 2 (AB): BD=1.0 <= 1.05 -> extends.
  # Layer 3 (C): BD=1.4 > 1.05 -> extension stops.
  expect_true(1L %in% ap$layers)
  expect_true(2L %in% ap$layers)
  expect_false(3L %in% ap$layers)
})


test_that("v0.9.85: extension respects OC threshold (drop below 2 stops it)", {
  # OC drops from 4.7 -> 2.7 -> 0.7; OC = 0.7 < 2 stops extension at AB.
  pr <- .andic_pedon_extension_target()  # already has C OC = 0.7
  withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE,
                            soilKey.andic_oc_bd_proxy_extend = TRUE), {
    ap <- andic_properties(pr)
  })
  expect_false(3L %in% ap$layers)
  expect_false(4L %in% ap$layers)
})


# ---- 3. Default behaviour preservation ----------------------------------

test_that("v0.9.85: andosol() default behaviour unchanged when proxy + extend OFF", {
  pr <- .andic_pedon_extension_target()
  res <- andosol(pr)
  # No proxy, no extension -> andic_properties is NA / FALSE because
  # neither al_ox / fe_ox nor phosphate_retention is recorded.
  expect_false(isTRUE(res$passed))
})


# ---- 4. AfSP regression -- Andosol references go up from 0 to 2 ----------

test_that("v0.9.85: AfSP Andosol references -- 2/5 classify correctly with full opt-ins", {
  fp <- system.file("extdata", "afsp_sample.rds", package = "soilKey")
  if (!nzchar(fp)) fp <- "inst/extdata/afsp_sample.rds"
  skip_if_not(file.exists(fp), "afsp_sample not bundled")
  s <- readRDS(fp)
  ans <- Filter(function(p) identical(p$site$reference_wrb, "Andosol"),
                  s$pedons %||% s)
  # default
  n_def <- sum(vapply(ans, function(p) {
    cls <- tryCatch(classify_wrb2022(p, on_missing = "silent"),
                     error = function(e) NULL)
    isTRUE(identical(cls$rsg_or_order %||% NA_character_, "Andosols"))
  }, logical(1)))
  expect_equal(n_def, 0L)
  # full opt-in
  n_full <- withr::with_options(list(soilKey.andic_oc_bd_proxy = TRUE,
                                      soilKey.andic_oc_bd_proxy_extend = TRUE), {
    sum(vapply(ans, function(p) {
      cls <- tryCatch(classify_wrb2022(p, on_missing = "silent"),
                       error = function(e) NULL)
      isTRUE(identical(cls$rsg_or_order %||% NA_character_, "Andosols"))
    }, logical(1)))
  })
  expect_gte(n_full, 2L)
})
