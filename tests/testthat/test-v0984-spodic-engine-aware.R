# =============================================================================
# Tests for v0.9.84 -- spodic() engine="aqp" relaxation that accepts
# any B* designation under E* with an OC translocation peak when
# Al/Fe oxalate is unmeasured.
# =============================================================================


.spodic_pedon <- function(designations, oc, ph, al_ox = NA_real_, fe_ox = NA_real_) {
  hz <- data.table::data.table(
    top_cm    = c(0,    8,   23,   53,   81),
    bottom_cm = c(8,   23,   53,   81,  102),
    designation = designations,
    munsell_value_moist  = rep(4L, 5),
    munsell_chroma_moist = rep(4L, 5),
    structure_grade  = rep("moderate", 5),
    structure_size   = rep("medium",   5),
    structure_type   = rep("granular", 5),
    consistence_moist = rep("friable", 5),
    clay_pct = rep(8, 5),
    sand_pct = rep(80, 5),
    silt_pct = rep(12, 5),
    cec_cmolc_kg = rep(5, 5),
    bs_pct  = rep(15, 5),
    oc_pct  = oc,
    ph_h2o  = ph,
    bulk_density_g_cm3 = rep(1.2, 5),
    al_cmolc_kg = rep(0.2, 5),
    coarse_fragments_pct = rep(0, 5),
    al_ox_pct = al_ox,
    fe_ox_pct = fe_ox
  )
  hz <- ensure_horizon_schema(hz)
  PedonRecord$new(site = list(id = "spodic-fix"), horizons = hz)
}


# ---- Default engine="soilkey" preserved bit-for-bit ----------------------

test_that("v0.9.84: default engine=soilkey -- v0.9.19 morph requires Bh/Bs", {
  # Generic B1/B2 designations -- v0.9.19 path requires Bh/Bs/Bhs.
  # Default soilkey engine should NOT fire on this.
  pr <- .spodic_pedon(
    designations = c("A","E","B1","B2","C"),
    oc = c(2.0, 1.8, 4.5, 1.2, 0.3),
    ph = c(5.0, 4.5, 4.7, 5.0, 5.5))
  res <- spodic(pr)  # default engine
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.84: default engine=soilkey -- Bh designation NOT picked up under v0.9.19 if albic fails", {
  # The v0.9.19 morphological-inference path requires albic() to
  # pass on the E above the Bh. Our fixture's Munsell (value=4,
  # chroma=4) does not satisfy albic, so even with a Bh
  # designation the v0.9.19 path declines. This is documented v0.2
  # / v0.9.19 behaviour; the v0.9.84 engine="aqp" relaxation is
  # specifically designed to handle profiles where the eluvial E
  # is documented by designation but its colour does not satisfy
  # the strict albic test.
  pr <- .spodic_pedon(
    designations = c("A","E","Bh","B2","C"),
    oc = c(2.0, 1.8, 4.5, 1.2, 0.3),
    ph = c(5.0, 4.5, 4.7, 5.0, 5.5))
  res <- spodic(pr)
  # Default soilkey engine: spodic does NOT fire on this fixture
  # (canonical alfe NA + albic fails -> v0.9.19 morph blocked).
  expect_false(isTRUE(res$passed))
})


# ---- engine="aqp" relaxed path ------------------------------------------

test_that("v0.9.84: engine=aqp accepts B* under E* with OC translocation peak", {
  # KSSL+NASIS pattern: A / E / B1 (no Bh) with OC peak in B1.
  pr <- .spodic_pedon(
    designations = c("A","E","B1","B2","C"),
    oc = c(2.0, 1.8, 4.5, 1.2, 0.3),
    ph = c(5.0, 4.5, 4.7, 5.0, 5.5))
  res <- spodic(pr, engine = "aqp")
  expect_true(isTRUE(res$passed))
  expect_identical(
    res$evidence$alfe_oxalate$details$source,
    "engine_aqp_oc_translocation"
  )
})


test_that("v0.9.84: engine=aqp accepts B* with OC peak when pH is NA but ratio >= 1.5x", {
  # Pedon 604 / 636 / 638 pattern: pH NA at the Bh, but OC ratio
  # OC_in_B / max(OC_above) >= 1.5.
  pr <- .spodic_pedon(
    designations = c("A","E","B1","B2","C"),
    oc = c(2.0, 1.5, 11.8, 2.0, 0.3),
    ph = c(5.5, 5.0, NA_real_, 5.0, 5.5))
  res <- spodic(pr, engine = "aqp")
  expect_true(isTRUE(res$passed))
})


test_that("v0.9.84: engine=aqp REJECTS when pH NA AND OC ratio < 1.5x", {
  # OC ratio = 4.0 / 3.5 = 1.14x -- below 1.5 threshold; pH NA.
  pr <- .spodic_pedon(
    designations = c("A","E","B1","B2","C"),
    oc = c(3.0, 3.5, 4.0, 1.2, 0.3),
    ph = c(5.0, 4.5, NA_real_, 5.0, 5.5))
  res <- spodic(pr, engine = "aqp")
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.84: engine=aqp REJECTS when no E above the B", {
  # No eluvial E -- not a Podzol, just a high-OC B horizon.
  pr <- .spodic_pedon(
    designations = c("A","AB","B1","B2","C"),
    oc = c(2.0, 1.8, 4.5, 1.2, 0.3),
    ph = c(5.0, 4.5, 4.7, 5.0, 5.5))
  res <- spodic(pr, engine = "aqp")
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.84: engine=aqp REJECTS when OC in B is NOT a peak", {
  # OC in B1 (4.5) is GREATER than max above (max(2.0, 5.5) = 5.5)?
  # Actually 4.5 < 5.5, so NO peak. Rejected.
  pr <- .spodic_pedon(
    designations = c("A","E","B1","B2","C"),
    oc = c(2.0, 5.5, 4.5, 1.2, 0.3),
    ph = c(5.0, 4.5, 4.7, 5.0, 5.5))
  res <- spodic(pr, engine = "aqp")
  expect_false(isTRUE(res$passed))
})


test_that("v0.9.84: engine=aqp REJECTS when Al/Fe oxalate IS measured (canonical takes over)", {
  # If oxalate data is present, the canonical alfe_oxalate test
  # decides; the v0.9.84 relax path is gated off measurement.
  pr <- .spodic_pedon(
    designations = c("A","E","B1","B2","C"),
    oc = c(2.0, 1.8, 4.5, 1.2, 0.3),
    ph = c(5.0, 4.5, 4.7, 5.0, 5.5),
    al_ox = c(NA, NA, 0.2, NA, NA),  # measured but below threshold
    fe_ox = c(NA, NA, 0.1, NA, NA))
  res <- spodic(pr, engine = "aqp")
  # alfe path returns FALSE not NA; the v0.9.84 relax is gated on
  # !any(!is.na(al_ox_pct)) so it does NOT fire when Al/Fe is
  # documented.
  expect_false(isTRUE(res$passed))
})


# ---- engine via option ---------------------------------------------------

test_that("v0.9.84: engine read from option soilKey.diagnostic_engine", {
  pr <- .spodic_pedon(
    designations = c("A","E","B1","B2","C"),
    oc = c(2.0, 1.8, 4.5, 1.2, 0.3),
    ph = c(5.0, 4.5, 4.7, 5.0, 5.5))
  # Default option: NULL -> "soilkey"
  expect_false(isTRUE(spodic(pr)$passed))
  # Set option to aqp
  withr::with_options(list(soilKey.diagnostic_engine = "aqp"), {
    expect_true(isTRUE(spodic(pr)$passed))
  })
})


# ---- KSSL+NASIS regression test ------------------------------------------

test_that("v0.9.84: KSSL+NASIS Podzols -- engine=aqp lifts spodic recall by >=3", {
  fp <- system.file("extdata", "kssl_nasis_sample.rds", package = "soilKey")
  if (!nzchar(fp)) fp <- "inst/extdata/kssl_nasis_sample.rds"
  skip_if_not(file.exists(fp), "kssl_nasis_sample not bundled")
  s <- readRDS(fp)
  peds <- s$pedons %||% s
  ref_pod <- vapply(peds, function(p) {
    ref <- p$site$reference_wrb %||% NA_character_
    isTRUE(grepl("Podzol|Spodosol", ref, ignore.case = TRUE))
  }, logical(1))
  n_can <- sum(vapply(peds[ref_pod], function(p) {
    isTRUE(suppressMessages(suppressWarnings(spodic(p)))$passed)
  }, logical(1)))
  n_aqp <- sum(vapply(peds[ref_pod], function(p) {
    isTRUE(suppressMessages(suppressWarnings(spodic(p, engine = "aqp")))$passed)
  }, logical(1)))
  expect_lte(n_can, n_aqp - 3L)
  expect_gte(n_aqp, 4L)
})
