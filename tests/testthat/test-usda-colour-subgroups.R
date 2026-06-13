# Tests for the USDA colour/contact subgroup predicates added in v0.9.121
# (chromic_subgroup_usda, leptic_vertic_usda) and the reused
# xanthic_subgroup_usda / calcic_subgroup_usda depth windows.

mk <- function(h) PedonRecord$new(horizons = h)

# ---- chromic_subgroup_usda (Vertisol "not dark" value/chroma test) --------

test_that("chromic fails on a uniformly dark (Pellic) upper solum", {
  p <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     munsell_value_moist = c(2, 3),
                     munsell_chroma_moist = c(1, 1)))
  expect_false(chromic_subgroup_usda(p)$passed)
})

test_that("chromic passes when moist value >= 4 within 30 cm", {
  p <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     munsell_value_moist = c(4, 5),
                     munsell_chroma_moist = c(2, 2)))
  expect_true(chromic_subgroup_usda(p)$passed)
})

test_that("chromic chroma clause is gated by use_chroma (Aquerts drop it)", {
  p <- mk(data.frame(top_cm = c(0, 20), bottom_cm = c(20, 60),
                     munsell_value_moist = c(3, 3),
                     munsell_chroma_moist = c(3, 4)))
  expect_true(chromic_subgroup_usda(p, use_chroma = TRUE)$passed)
  expect_false(chromic_subgroup_usda(p, use_chroma = FALSE)$passed)
})

test_that("chromic ignores colours below the 30 cm window", {
  p <- mk(data.frame(top_cm = c(0, 40), bottom_cm = c(40, 80),
                     munsell_value_moist = c(2, 6),
                     munsell_chroma_moist = c(1, 3)))
  expect_false(chromic_subgroup_usda(p)$passed)
})

# ---- leptic_vertic_usda (densic/lithic/paralithic contact within 100 cm) ---

test_that("leptic passes on a lithic (R) contact within 100 cm", {
  p <- mk(data.frame(top_cm = c(0, 80), bottom_cm = c(80, 100),
                     designation = c("Bss", "R")))
  expect_true(leptic_vertic_usda(p)$passed)
})

test_that("leptic passes on a paralithic (Cr) contact within 100 cm", {
  p <- mk(data.frame(top_cm = c(0, 60), bottom_cm = c(60, 100),
                     designation = c("Bss", "Cr")))
  expect_true(leptic_vertic_usda(p)$passed)
})

test_that("leptic does not extrapolate past the 100 cm window", {
  p <- mk(data.frame(top_cm = c(0, 120), bottom_cm = c(120, 150),
                     designation = c("Bss", "R")))
  expect_false(leptic_vertic_usda(p)$passed)
})

test_that("leptic fails when there is no shallow contact", {
  p <- mk(data.frame(top_cm = c(0, 50), bottom_cm = c(50, 120),
                     designation = c("A", "Bss")))
  expect_false(leptic_vertic_usda(p)$passed)
})

# ---- reused predicates: depth windows -------------------------------------

test_that("xanthic matches yellow + light Oxisol subsoil", {
  p <- mk(data.frame(top_cm = c(0, 40), bottom_cm = c(40, 90),
                     munsell_hue_moist = c("10YR", "10YR"),
                     munsell_value_moist = c(6, 7)))
  expect_true(xanthic_subgroup_usda(p)$passed)
})

test_that("calcic honours the per-subgroup depth window", {
  p <- mk(data.frame(top_cm = c(0, 30, 120), bottom_cm = c(30, 120, 160),
                     caco3_pct = c(1, 2, 20)))
  expect_false(calcic_subgroup_usda(p)$passed)                 # default 100 cm
  expect_true(calcic_subgroup_usda(p, max_top_cm = 150)$passed)
})

# ---- end-to-end: the canonical Vertisol now refines to Chromic -------------
# Validated refinement (not a regression): its upper-30 cm colours (chroma 4 /
# value moist 4) meet the KST Chromic criterion, so Chromic Hapluderts is more
# specific than -- and within the same great group as -- the former Typic.

test_that("canonical Vertisol fixture keys to Chromic Hapluderts", {
  res <- classify_usda(make_vertisol_canonical())
  expect_equal(res$rsg_or_order, "Vertisols")
  expect_equal(res$name, "Chromic Hapluderts")
})

test_that("USDA subgroup coverage reflects the +57 colour subgroups", {
  cov <- coverage_report("usda_subgroup")
  # >= rather than == so later fronts that add subgroups don't break this test
  # (the exact current total is pinned in test-usda-intergrade-subgroups.R).
  expect_gte(cov$overall$covered_n, 1978L)
  # every generated colour subgroup is now registered
  gen <- c("Chromic Haplusterts", "Leptic Haplusterts", "Xanthic Hapludox",
           "Calcic Haploxeralfs", "Calcic Haplotorrands")
  expect_true(all(tolower(gen) %in% tolower(setdiff(
    .coverage_registered_usda_subgroups(), ""))))
})
