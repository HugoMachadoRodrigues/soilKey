# Tests for v0.9.122: honest stub-detector (delegation) + measured-decomposition
# fallback in .decomp_class (von Post / rubbed fibre).

# A peat profile (organic material) with a chosen designation + measured fields.
.peat <- function(des = "O", vp = NA_integer_, fb = NA_real_) {
  h <- data.frame(
    top_cm    = c(0, 20, 50),
    bottom_cm = c(20, 50, 90),
    designation = rep(des, 3),
    oc_pct      = c(40, 40, 40),
    von_post_index = rep(as.integer(vp), 3),
    fiber_content_rubbed_pct = rep(as.numeric(fb), 3)
  )
  PedonRecord$new(site = list(country = "BR"), horizons = h)
}

# ---- stub-detector now recognises delegation -------------------------------

test_that("Fibric/Hemic/Sapric are counted as implemented (delegation)", {
  for (q in c("Fibric", "Hemic", "Sapric"))
    expect_true(soilKey:::.qualifier_is_implemented(q))
})

test_that("coverage_report('wrb_qualifiers') no longer flags them as stubs", {
  cov <- coverage_report("wrb_qualifiers")
  expect_false(any(c("Fibric", "Hemic", "Sapric") %in% cov$stubs))
  expect_equal(cov$overall$covered_n, 233L)  # v0.9.145: +etrosalic +3 wrappers
})

test_that("a function that does not exist still returns NA (no over-count)", {
  # Novic is the lone remaining genuine gap (schema-blocked: deposition age),
  # so it has no qual_ function and must read NA, not FALSE/TRUE. (Claric was the
  # earlier example but gained a wrapper in v0.9.145.)
  expect_true(is.na(soilKey:::.qualifier_is_implemented("Novic")))
})

# ---- .decomp_class measured-decomposition fallback -------------------------

test_that("von Post index classifies organic layers lacking an O-subscript", {
  expect_equal(soilKey:::.decomp_class(.peat(des = "O", vp = 2L))$class, "fibric")
  expect_equal(soilKey:::.decomp_class(.peat(des = "O", vp = 5L))$class, "hemic")
  expect_equal(soilKey:::.decomp_class(.peat(des = "O", vp = 9L))$class, "sapric")
})

test_that("rubbed-fibre content is the fallback when von Post is absent", {
  expect_equal(soilKey:::.decomp_class(.peat(des = "O", fb = 50))$class, "fibric")
  expect_equal(soilKey:::.decomp_class(.peat(des = "O", fb = 25))$class, "hemic")
  expect_equal(soilKey:::.decomp_class(.peat(des = "O", fb = 5))$class,  "sapric")
})

test_that("the O-subscript designation keeps priority over measured fields", {
  # Oi (fibric designation) must win even against a sapric von Post.
  expect_equal(soilKey:::.decomp_class(.peat(des = "Oi", vp = 9L))$class, "fibric")
})

test_that("qual_sapric keys end-to-end from a measured von Post", {
  expect_true(qual_sapric(.peat(des = "O", vp = 9L))$passed)
  expect_false(qual_fibric(.peat(des = "O", vp = 9L))$passed)
})

test_that("a profile with no measured decomposition data is unaffected", {
  # No O-subscript, no von Post, no fibre -> unclassified (NA), as before.
  expect_true(is.na(soilKey:::.decomp_class(.peat(des = "O"))$class))
})
