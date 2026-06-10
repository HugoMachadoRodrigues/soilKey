# Tests for the v0.9.104 USDA Soil Taxonomy family level (5th category).
# The six family-modifier dimensions are pure functions over a PedonRecord;
# we exercise each with a small synthetic pedon, then the end-to-end name and
# the default-unchanged regression.

.uf_pedon <- function(horizons, site = list(id = "uf", lat = -22.5,
                                            lon = -43.7, crs = 4326)) {
  soilKey::PedonRecord$new(site = site, horizons = horizons)
}


test_that("particle_size class keys on clay and rock fragments", {
  fine <- family_particle_size_usda(.uf_pedon(data.frame(
    top_cm = c(0, 30), bottom_cm = c(30, 100),
    clay_pct = c(45, 50), sand_pct = c(30, 25), silt_pct = c(25, 25))))
  expect_equal(fine$value, "fine")

  vfine <- family_particle_size_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, clay_pct = 65, sand_pct = 15, silt_pct = 20)))
  expect_equal(vfine$value, "very-fine")

  skel <- family_particle_size_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, clay_pct = 45, sand_pct = 25, silt_pct = 30,
    coarse_fragments_pct = 50)))
  expect_equal(skel$value, "clayey-skeletal")

  sandy <- family_particle_size_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, clay_pct = 6, sand_pct = 88, silt_pct = 6)))
  expect_equal(sandy$value, "sandy")
})


test_that("cec_activity class keys on the CEC/clay ratio", {
  mk <- function(cec, clay) family_cec_activity_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, cec_cmol = cec, clay_pct = clay)))
  expect_equal(mk(40, 50)$value, "superactive")   # 0.80
  expect_equal(mk(25, 50)$value, "active")        # 0.50
  expect_equal(mk(15, 50)$value, "semiactive")    # 0.30
  expect_equal(mk(5,  50)$value, "subactive")     # 0.10
  # too sandy -> no class
  expect_null(mk(2, 5)$value)
})


test_that("mineralogy: low Kr -> oxidic, high activity -> smectitic", {
  oxidic <- family_mineralogy_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, clay_pct = 60, cec_cmol = 5,
    sio2_sulfuric_pct = 8, al2o3_sulfuric_pct = 20, fe2o3_sulfuric_pct = 18)))
  expect_equal(oxidic$value, "oxidic")            # Kr < 0.75

  smectitic <- family_mineralogy_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, clay_pct = 40, cec_cmol = 32)))
  expect_equal(smectitic$value, "smectitic")      # CEC/clay = 0.80
})


test_that("reaction is calcareous only when carbonates are present", {
  cal <- family_reaction_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, caco3_pct = 12, ph_h2o = 8.0)))
  expect_equal(cal$value, "calcareous")
  none <- family_reaction_usda(.uf_pedon(data.frame(
    top_cm = 25, bottom_cm = 100, caco3_pct = 0, ph_h2o = 5.5)))
  expect_null(none$value)
})


test_that("temperature regime uses the site field, else infers from lat/elev", {
  given <- family_temperature_regime_usda(.uf_pedon(
    data.frame(top_cm = 0, bottom_cm = 50, clay_pct = 30),
    site = list(soil_temperature_regime = "mesic")))
  expect_equal(given$value, "mesic")
  expect_false(isTRUE(given$evidence$inferred))

  # tropical low-elevation site -> inferred iso* warm regime
  inferred <- family_temperature_regime_usda(.uf_pedon(
    data.frame(top_cm = 0, bottom_cm = 50, clay_pct = 30),
    site = list(lat = -5, lon = -60, elevation_m = 100)))
  expect_true(isTRUE(inferred$evidence$inferred))
  expect_match(inferred$value, "^iso")
  expect_true("site$soil_temperature_regime" %in% inferred$missing)
})


test_that("depth class flags shallow soils via a lithic contact", {
  shallow <- family_depth_class_usda(.uf_pedon(data.frame(
    top_cm = c(0, 30), bottom_cm = c(30, 40),
    designation = c("A", "R"))))
  expect_equal(shallow$value, "shallow")
  deep <- family_depth_class_usda(.uf_pedon(data.frame(
    top_cm = c(0, 30), bottom_cm = c(30, 150),
    designation = c("A", "Bw"))))
  expect_null(deep$value)
})


test_that("family_label_usda joins modifiers in canonical order", {
  fam <- list(
    particle_size = FamilyAttribute$new("particle_size", "fine"),
    mineralogy    = FamilyAttribute$new("mineralogy", "kaolinitic"),
    cec_activity  = FamilyAttribute$new("cec_activity", NULL),
    temperature_regime = FamilyAttribute$new("temperature_regime", "isohyperthermic"))
  expect_equal(family_label_usda(fam),
               "fine, kaolinitic, isohyperthermic")
})


test_that("classify_usda(include_family) prepends the family; default unchanged", {
  pr <- make_ferralsol_canonical()
  base <- classify_usda(pr)
  withf <- classify_usda(pr, include_family = TRUE)

  # default output is byte-identical
  expect_equal(base$name, "Rhodic Hapludox")
  # family run keeps the subgroup as the tail and prepends modifiers
  expect_match(withf$name, "Rhodic Hapludox$")
  expect_true(nchar(withf$name) > nchar(base$name))
  expect_true(grepl(",", withf$name))
  # family attributes are exposed in the trace
  expect_true(!is.null(withf$trace$family))
  expect_true(!is.null(withf$trace$family$particle_size))
})


test_that("classify_all forwards include_family to the USDA slot", {
  pr <- make_ferralsol_canonical()
  out <- classify_all(pr, include_family = TRUE)
  expect_match(out$usda$name, "Rhodic Hapludox$")
  expect_true(grepl(",", out$usda$name))
  # SiBCS / WRB unaffected
  expect_false(is.null(out$wrb))
})
