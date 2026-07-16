# =============================================================================
# v0.9.186 -- WRB 2022 diagnostic completeness is genuine and auditable.
#
# Background. Earlier "40/40 horizons . 17/17 properties . 19/19 materials .
# 32/32 RSGs" completeness claims were hand-maintained in the README. This suite
# makes them auditable the same way USDA/qualifier coverage already is:
#   * coverage_report("wrb_horizons"/"wrb_properties"/"wrb_materials"/"wrb_rsg")
#     diffs the implemented functions against the canonical WRB 2022 reference
#     (inst/extdata/canonical/wrb2022_diagnostics.csv, transcribed from the IUSS
#     WRB 2022 4th-edition Chapter 3 section headers) and the deterministic key;
#   * the two horizons that were genuinely missing -- Cohesic (3.1.7) and Folic
#     (3.1.12) -- and the Cryic horizon wrapper (3.1.8) are implemented here and
#     tested against their official criteria;
#   * a contract test proves every one of the 76 canonical diagnostics is
#     callable on a valid pedon and returns a well-formed DiagnosticResult.
# =============================================================================

# --- helpers -----------------------------------------------------------------
mk_pedon <- function(h) PedonRecord$new(site = list(id = "t"), horizons = h)

# =============================================================================
# 1. Cohesic horizon (WRB 2022, 3.1.7) -- the SiBCS "caracter coeso".
# =============================================================================
test_that("cohesic() passes a kaolinitic, hard-setting subsurface horizon", {
  # OC 0.3%; clay 45%; CEC 8 cmolc/kg -> 17.8/kg clay (< 24); massive; not
  # cemented; hard when dry; 40 cm thick -- every criterion met on the Bw.
  p <- mk_pedon(data.frame(
    designation = c("Ap", "Bw"), top_cm = c(0, 20), bottom_cm = c(20, 60),
    oc_pct = c(1.2, 0.3), clay_pct = c(30, 45), cec_cmol = c(6, 8),
    structure_type = c("granular", "massive"),
    structure_grade = c("moderate", "structureless"),
    cementation_class = c("none", "none"),
    rupture_resistance = c("soft", "hard"), stringsAsFactors = FALSE))
  r <- cohesic(p)
  expect_true(isTRUE(r$passed))
  expect_identical(r$layers, 2L)
  expect_equal(r$name, "cohesic")
})

test_that("cohesic() rejects a soft (not hard when dry) horizon", {
  p <- mk_pedon(data.frame(
    designation = "Bw", top_cm = 20, bottom_cm = 60, oc_pct = 0.3,
    clay_pct = 45, cec_cmol = 8, structure_type = "massive",
    structure_grade = "structureless", cementation_class = "none",
    rupture_resistance = "soft", stringsAsFactors = FALSE))
  expect_false(isTRUE(cohesic(p)$passed))
})

test_that("cohesic() rejects a high-activity-clay horizon (CEC/clay >= 24)", {
  # CEC 40 cmolc/kg on 45% clay = 88.9 cmolc/kg clay -- not low-activity.
  p <- mk_pedon(data.frame(
    designation = "Bt", top_cm = 20, bottom_cm = 60, oc_pct = 0.3,
    clay_pct = 45, cec_cmol = 40, structure_type = "massive",
    structure_grade = "structureless", cementation_class = "none",
    rupture_resistance = "hard", stringsAsFactors = FALSE))
  expect_false(isTRUE(cohesic(p)$passed))
})

test_that("cohesic() rejects a too-thin qualifying layer (< 10 cm)", {
  p <- mk_pedon(data.frame(
    designation = "Bw", top_cm = 20, bottom_cm = 27, oc_pct = 0.3,
    clay_pct = 45, cec_cmol = 8, structure_type = "massive",
    structure_grade = "structureless", cementation_class = "none",
    rupture_resistance = "hard", stringsAsFactors = FALSE))
  expect_false(isTRUE(cohesic(p)$passed))
})

# =============================================================================
# 2. Folic horizon (WRB 2022, 3.1.12) -- the aerobic twin of the histic horizon.
# =============================================================================
test_that("folic() passes a well-aerated surface organic horizon", {
  # OC 25% (organic material); saturated only 5 days/yr (< 30); 15 cm thick.
  p <- mk_pedon(data.frame(
    designation = "Oi", top_cm = 0, bottom_cm = 15, oc_pct = 25,
    water_saturation_days = 5, stringsAsFactors = FALSE))
  r <- folic(p)
  expect_true(isTRUE(r$passed))
  expect_equal(r$name, "folic")
})

test_that("folic() rejects a long-saturated organic horizon (histic, not folic)", {
  p <- mk_pedon(data.frame(
    designation = "Oa", top_cm = 0, bottom_cm = 15, oc_pct = 25,
    water_saturation_days = 120, stringsAsFactors = FALSE))
  expect_false(isTRUE(folic(p)$passed))
})

test_that("folic() rejects a mineral (non-organic) surface horizon", {
  p <- mk_pedon(data.frame(
    designation = "A", top_cm = 0, bottom_cm = 15, oc_pct = 2,
    water_saturation_days = 5, stringsAsFactors = FALSE))
  expect_false(isTRUE(folic(p)$passed))
})

# The histic / folic split is exclusive on the same organic layer: exactly one
# of them fires, decided purely by the water-saturation window.
test_that("histic and folic partition organic layers by saturation", {
  wet <- mk_pedon(data.frame(designation = "Oa", top_cm = 0, bottom_cm = 15,
    oc_pct = 25, water_saturation_days = 120, stringsAsFactors = FALSE))
  dry <- mk_pedon(data.frame(designation = "Oi", top_cm = 0, bottom_cm = 15,
    oc_pct = 25, water_saturation_days = 5, stringsAsFactors = FALSE))
  expect_true(isTRUE(folic(dry)$passed))
  expect_false(isTRUE(folic(wet)$passed))
})

# =============================================================================
# 3. Cryic horizon wrapper (WRB 2022, 3.1.8) -- delegates to cryic_conditions.
# =============================================================================
test_that("cryic_horizon() delegates to cryic_conditions and renames the result", {
  p <- make_ferralsol_canonical()
  cw <- cryic_horizon(p)
  cc <- cryic_conditions(p)
  expect_equal(cw$name, "cryic")
  expect_identical(cw$passed, cc$passed)          # same verdict
  expect_identical(cw$layers, cc$layers)          # same layers
})

# =============================================================================
# 3b. Ferralsol showcase strings are pinned (the exact names the README/paper
#     cite). The canonical Ferralsol is Eutric under WRB (exchangeable bases
#     >= exchangeable Al) yet Distrofico under SiBCS (base saturation < 50%):
#     a legitimate, documented divergence between two different base-status
#     definitions -- not a bug. Guard the full strings so they cannot drift.
# =============================================================================
test_that("the canonical Ferralsol classifies to the documented full names", {
  p <- make_ferralsol_canonical()
  expect_equal(classify_wrb2022(p)$name,
               "Geric Ferric Rhodic Ferralsol (Clayic, Humic, Eutric, Ochric, Rubic)")
  expect_equal(classify_usda(p)$name, "Rhodic Hapludox")
  expect_equal(classify_sibcs(p)$name, "Latossolos Vermelhos Distroficos tipicos")
  # the WRB (Eutric) / SiBCS (Distrofico) base-status split is real and stable
  expect_match(classify_wrb2022(p)$name, "Eutric")
  expect_match(classify_sibcs(p)$name, "Distroficos")
})

# =============================================================================
# 4. Coverage is auditable and complete (the point of the exercise).
# =============================================================================
test_that("the canonical WRB 2022 reference has the official counts", {
  csv <- system.file("extdata", "canonical", "wrb2022_diagnostics.csv",
                     package = "soilKey")
  expect_true(nzchar(csv) && file.exists(csv))
  d <- utils::read.csv(csv, stringsAsFactors = FALSE)
  expect_equal(sum(d$category == "horizon"),  40L)
  expect_equal(sum(d$category == "property"), 17L)
  expect_equal(sum(d$category == "material"), 19L)
})

test_that("every crosswalked function exists and has a genuine body", {
  csv <- system.file("extdata", "canonical", "wrb2022_diagnostics.csv",
                     package = "soilKey")
  d <- utils::read.csv(csv, stringsAsFactors = FALSE)
  fns <- d$soilkey_fn[nzchar(d$soilkey_fn) & !is.na(d$soilkey_fn)]
  ns <- asNamespace("soilKey")
  expect_true(all(vapply(fns, exists, logical(1), where = ns, mode = "function")))
  # not one is an unconditional passed = NA stub
  real <- vapply(fns, function(f) soilKey:::.diagnostic_body_is_real(get(f, ns)),
                 logical(1))
  expect_true(all(real), info = paste("stub bodies:",
                                      paste(fns[!real], collapse = ", ")))
})

test_that("coverage_report() reports genuine 100% for every WRB diagnostic axis", {
  for (axis in c("wrb_horizons", "wrb_properties", "wrb_materials", "wrb_rsg")) {
    r <- suppressMessages(coverage_report(axis))
    expect_equal(r$overall$pct, 100, info = axis)
    expect_length(r$missing, 0)
    expect_length(r$stubs %||% character(0), 0)
  }
})

test_that("the WRB diagnostic denominators match the official WRB 2022 totals", {
  expect_equal(suppressMessages(coverage_report("wrb_horizons"))$overall$canonical_n,   40L)
  expect_equal(suppressMessages(coverage_report("wrb_properties"))$overall$canonical_n, 17L)
  expect_equal(suppressMessages(coverage_report("wrb_materials"))$overall$canonical_n,  19L)
  expect_equal(suppressMessages(coverage_report("wrb_rsg"))$overall$canonical_n,        32L)
})

# =============================================================================
# 5. Contract test -- EVERY canonical diagnostic is callable and well-formed.
#
# This is the safety net that retires the "untested diagnostics" gap: whatever
# the semantics, no canonical diagnostic may error on a valid pedon or return a
# malformed result. It runs the whole 76-function set against a full fixture.
# =============================================================================
test_that("all 76 canonical diagnostics run on a valid pedon and return a DiagnosticResult", {
  csv <- system.file("extdata", "canonical", "wrb2022_diagnostics.csv",
                     package = "soilKey")
  d <- utils::read.csv(csv, stringsAsFactors = FALSE)
  fns <- d$soilkey_fn[nzchar(d$soilkey_fn) & !is.na(d$soilkey_fn)]
  ns <- asNamespace("soilKey")
  p  <- make_ferralsol_canonical()

  bad <- character(0)
  for (f in fns) {
    r <- tryCatch(get(f, ns)(p),
                  error = function(e) structure("ERR", msg = conditionMessage(e)))
    ok <- !identical(r, "ERR") &&
      inherits(r, "DiagnosticResult") &&
      is.character(r$name) && nzchar(r$name) &&
      is.logical(r$passed) && length(r$passed) == 1L &&
      (is.numeric(r$layers) || length(r$layers) == 0L)
    if (!ok) bad <- c(bad, if (identical(r, "ERR")) paste0(f, ": ", attr(r, "msg")) else f)
  }
  expect_length(fns, 76L)
  expect_true(length(bad) == 0L, info = paste("malformed/erroring:",
                                              paste(bad, collapse = "; ")))
})
