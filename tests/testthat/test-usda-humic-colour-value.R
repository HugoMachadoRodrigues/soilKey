# Tests for dark_color_value_usda -- the KST 13ed "Humic" subgroup colour
# differentia (value moist <= 3 AND value dry <= 5 throughout the upper 18 cm
# of the mineral soil). Re-points the registered Humic Dystrudepts subgroup
# (KFGD) which previously (and incorrectly) reused the mollic/umbric epipedon
# predicate humic_inceptisol_usda. Audited against SoilTaxonomy::ST_criteria_13th
# (Humic Dystrudepts = KFGY, a pure colour-value test).

mk <- function(h) PedonRecord$new(horizons = h)

# ---- dark_color_value_usda unit behaviour ---------------------------------

test_that("dark colour value passes when moist<=3 & dry<=5 throughout 18 cm", {
  p <- mk(data.frame(top_cm = c(0, 25), bottom_cm = c(25, 80),
                     designation = c("A", "Bw"),
                     munsell_value_moist = c(3, 5),
                     munsell_value_dry = c(4, 6)))
  expect_true(dark_color_value_usda(p)$passed)
})

test_that("a moist value of 4 in the window fails (the plinthosol case)", {
  p <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 80),
                     designation = c("A", "Btv"),
                     munsell_value_moist = c(4, 4),
                     munsell_value_dry = c(5, 5)))
  expect_false(dark_color_value_usda(p)$passed)
})

test_that("a dry value of 6 fails even when moist is dark", {
  p <- mk(data.frame(top_cm = c(0, 25), bottom_cm = c(25, 80),
                     designation = c("A", "Bw"),
                     munsell_value_moist = c(3, 4),
                     munsell_value_dry = c(6, 6)))
  expect_false(dark_color_value_usda(p)$passed)
})

test_that("'throughout' fails when a layer inside the window is light", {
  p <- mk(data.frame(top_cm = c(0, 8, 18), bottom_cm = c(8, 18, 80),
                     designation = c("A", "AB", "Bw"),
                     munsell_value_moist = c(2, 4, 5),
                     munsell_value_dry = c(3, 6, 6)))
  expect_false(dark_color_value_usda(p)$passed)
})

test_that("chroma is irrelevant (differs from the umbric/mollic colour test)", {
  # High chroma (6) but dark value -> Humic differentia is value-only.
  p <- mk(data.frame(top_cm = c(0, 25), bottom_cm = c(25, 80),
                     designation = c("A", "Bw"),
                     munsell_value_moist = c(3, 4),
                     munsell_value_dry = c(4, 6),
                     munsell_chroma_moist = c(6, 6)))
  expect_true(dark_color_value_usda(p)$passed)
  expect_false(umbric_epipedon_usda(p)$passed)   # chroma 6 -> not umbric
})

test_that("missing colour and OC -> NA (undetermined, not a hard fail)", {
  p <- mk(data.frame(top_cm = c(0, 25), bottom_cm = c(25, 80),
                     designation = c("A", "Bw")))
  expect_true(is.na(dark_color_value_usda(p)$passed))
})

test_that("depth is measured from the mineral soil surface (O layer skipped)", {
  p <- mk(data.frame(top_cm = c(0, 5, 30), bottom_cm = c(5, 30, 80),
                     designation = c("Oi", "A", "Bw"),
                     munsell_value_moist = c(NA, 3, 4),
                     munsell_value_dry = c(NA, 5, 6)))
  expect_true(dark_color_value_usda(p)$passed)
})

test_that("dry-only Munsell uses the value-moist ~ value-dry - 1 estimate", {
  expect_true(dark_color_value_usda(mk(data.frame(
    top_cm = c(0, 25), bottom_cm = c(25, 80), designation = c("A", "Bw"),
    munsell_value_dry = c(4, 6))))$passed)                 # implies moist ~ 3
  expect_false(dark_color_value_usda(mk(data.frame(
    top_cm = c(0, 25), bottom_cm = c(25, 80), designation = c("A", "Bw"),
    munsell_value_dry = c(5, 6))))$passed)                 # implies moist ~ 4
})

test_that("OC-darkness fallback fires when no Munsell is recorded", {
  p <- mk(data.frame(top_cm = c(0, 25), bottom_cm = c(25, 80),
                     designation = c("A", "Bw"), oc_pct = c(2.0, 0.4)))
  expect_true(dark_color_value_usda(p)$passed)
})

# ---- end-to-end: a colour-only dark Dystrudept now keys to Humic ----------

test_that("dark, high-chroma Dystrudept keys to Humic Dystrudepts", {
  # Dystric cambic, udic, dark colour value (moist 3 / dry 5) but chroma 5,
  # so it is NOT an umbric epipedon. The old humic_inceptisol_usda mapping
  # left this Typic; the colour-value differentia correctly makes it Humic.
  hz <- data.table::data.table(
    top_cm = c(0, 20, 80), bottom_cm = c(20, 80, 150),
    designation = c("A", "Bw", "C"),
    oc_pct = c(1.2, 0.4, 0.2), bs_pct = c(25, 22, 20),
    cec_cmol = c(8, 6, 5), ph_h2o = c(5.2, 5.3, 5.4),
    clay_pct = c(18, 20, 19), silt_pct = c(40, 40, 40), sand_pct = c(42, 40, 41),
    munsell_hue_moist = c("10YR", "10YR", "10YR"),
    munsell_value_moist = c(3, 4, 5), munsell_value_dry = c(5, 6, 6),
    munsell_chroma_moist = c(5, 6, 6),
    structure_grade = c("moderate", "weak", NA),
    structure_size = c("medium", "medium", NA))
  pr <- PedonRecord$new(
    site = list(id = "hd", lat = 45, lon = 10, country = "US",
                parent_material = "residuum", soil_moisture_regime = "udic"),
    horizons = ensure_horizon_schema(hz))
  res <- classify_usda(pr, on_missing = "silent")
  expect_equal(res$rsg_or_order, "Inceptisols")
  expect_equal(res$name, "Humic Dystrudepts")
})

# ---- regression guards -----------------------------------------------------

test_that("canonical plinthosol fixture stays Typic Dystrudepts (value 4)", {
  res <- classify_usda(make_plinthosol_canonical(), on_missing = "silent")
  expect_equal(res$name, "Typic Dystrudepts")
})

test_that("great-group Hum- predicate unchanged: umbrisol -> Typic Humudepts", {
  # humic_inceptisol_usda (mollic/umbric epipedon) still keys the Hum- great
  # groups; only the subgroup modifier was re-pointed.
  res <- classify_usda(make_umbrisol_canonical(), on_missing = "silent")
  expect_equal(res$name, "Typic Humudepts")
})

test_that("Humic Dystrudepts is registered as the residual subgroup (before Typic)", {
  rules <- load_rules("usda")
  kfg <- rules$subgroups$KFG
  names_in_order <- vapply(kfg, function(x) x$name, character(1))
  hi <- match("Humic Dystrudepts", names_in_order)
  ti <- match("Typic Dystrudepts", names_in_order)
  expect_false(is.na(hi))
  expect_equal(ti, length(names_in_order))   # Typic is last (default)
  expect_equal(hi, ti - 1L)                  # Humic immediately precedes it
})
