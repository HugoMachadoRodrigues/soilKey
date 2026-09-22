# Tests for v0.9.124: USDA predicate corrections found by the Phase-1 audit
# (each grounded in the verbatim KST 13ed criterion; see the audit report).

mk <- function(h) PedonRecord$new(horizons = h)

# ---- rendoll_qualifying_usda: lithic/paralithic contact WITHIN 50 cm --------

test_that("Rendoll requires the lithic contact within 50 cm (not 100)", {
  shallow <- mk(data.frame(top_cm = c(0, 40), bottom_cm = c(40, 60),
                           designation = c("A", "R"), caco3_pct = c(45, NA)))
  deep <- mk(data.frame(top_cm = c(0, 80), bottom_cm = c(80, 100),
                        designation = c("A", "R"), caco3_pct = c(45, NA)))
  expect_true(rendoll_qualifying_usda(shallow)$passed)
  expect_false(rendoll_qualifying_usda(deep)$passed)   # contact at 80 cm now fails
})

# ---- hydraquent_qualifying_usda: 20-50 cm window + clay>=8% + all-layers -----

test_that("Hydraquent uses the 20-50 cm window with the clay condition", {
  ok <- mk(data.frame(top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 60),
                      water_content_1500kpa = c(50, 85, 90),
                      clay_pct = c(5, 20, 25)))
  expect_true(hydraquent_qualifying_usda(ok)$passed)
})

test_that("Hydraquent fails when 20-50 cm clay is below 8%", {
  low_clay <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                            water_content_1500kpa = c(90, 90),
                            clay_pct = c(5, 5)))
  expect_false(hydraquent_qualifying_usda(low_clay)$passed)
})

test_that("Hydraquent ignores shallow (0-20 cm) wet layers", {
  shallow_only <- mk(data.frame(top_cm = c(0, 60), bottom_cm = c(20, 80),
                                water_content_1500kpa = c(90, 10),
                                clay_pct = c(30, 5)))
  expect_false(hydraquent_qualifying_usda(shallow_only)$passed)
})

# ---- aeric_oxisol_usda: chroma>=3 layer must be BELOW the epipedon -----------

test_that("Aeric does not count high chroma in the A/AB epipedon", {
  epi_only <- mk(data.frame(top_cm = c(0, 15, 25), bottom_cm = c(15, 25, 70),
                            designation = c("A", "AB", "Bw"),
                            munsell_chroma_moist = c(3, 3, 1)))
  expect_false(aeric_oxisol_usda(epi_only)$passed)
})

test_that("Aeric passes on a chroma>=3 horizon below the epipedon", {
  below <- mk(data.frame(top_cm = c(0, 25), bottom_cm = c(25, 70),
                         designation = c("A", "Bw"),
                         munsell_chroma_moist = c(1, 3)))
  expect_true(aeric_oxisol_usda(below)$passed)
})

# ---- duric_subgroup_usda: cemented in >= 90% of the pedon --------------------

test_that("Duric requires cementation in 90% or more of the pedon", {
  mostly <- mk(data.frame(top_cm = c(0, 10), bottom_cm = c(10, 100),
                          cementation_class = c("weakly", "strongly")))
  expect_true(duric_subgroup_usda(mostly)$passed)               # 90/100 cemented
  thin <- mk(data.frame(top_cm = c(0, 5), bottom_cm = c(5, 100),
                        cementation_class = c("strongly", "none")))
  expect_false(duric_subgroup_usda(thin)$passed)                # 5/100 cemented
})
