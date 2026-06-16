# v0.9.141 -- Fix D residue: two verbatim-confirmed WRB 2022 Ch 5 qualifier
# fixes (Mazic rupture clause; Grumic grade + blocky + size). The remaining
# deferred items (Hyposalic / Hyperskeletic non-2022; Raptic / Urbic /
# Evapocrustic proxy-limited) are documented in the report, not changed here.

mkh <- function(df) ensure_horizon_schema(data.table::as.data.table(df))
prh <- function(hz) PedonRecord$new(site = list(id = "t"), horizons = hz)


# ---- Mazic (p.140): massive AND rupture-resistance >= hard -----------------

test_that("v0.9.141: Mazic requires rupture-resistance >= hard when recorded", {
  soft <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                         structure_grade = "massive", rupture_resistance = "soft"))
  expect_false(isTRUE(qual_mazic(prh(soft))$passed))
  # "slightly hard" is NOT >= hard
  sl <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                       structure_grade = "massive", rupture_resistance = "slightly hard"))
  expect_false(isTRUE(qual_mazic(prh(sl))$passed))
  hard <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                         structure_grade = "massive", rupture_resistance = "very hard"))
  expect_true(isTRUE(qual_mazic(prh(hard))$passed))
})

test_that("v0.9.141: Mazic is byte-identical (massive-only) when rupture absent", {
  na <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                       structure_grade = "massive"))
  expect_true(isTRUE(qual_mazic(prh(na))$passed))
})


# ---- Grumic (p.136): STRONG granular/blocky, aggregate <= 1 cm -------------

test_that("v0.9.141: Grumic requires STRONG grade (moderate no longer qualifies)", {
  mo <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                       structure_grade = "moderate", structure_type = "granular",
                       structure_size = "fine"))
  expect_false(isTRUE(qual_grumic(prh(mo))$passed))
  st <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                       structure_grade = "strong", structure_type = "granular",
                       structure_size = "fine"))
  expect_true(isTRUE(qual_grumic(prh(st))$passed))
})

test_that("v0.9.141: Grumic now admits strong blocky self-mulching, size <= 1cm", {
  fine_blocky <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                       structure_grade = "strong", structure_type = "subangular blocky",
                       structure_size = "fine"))
  expect_true(isTRUE(qual_grumic(prh(fine_blocky))$passed))
  # medium blocky (10-20 mm) exceeds 1 cm -> NOT Grumic
  med_blocky <- mkh(data.frame(top_cm = 0, bottom_cm = 20, designation = "A",
                       structure_grade = "strong", structure_type = "subangular blocky",
                       structure_size = "medium"))
  expect_false(isTRUE(qual_grumic(prh(med_blocky))$passed))
})

test_that("v0.9.141: the canonical Vertisol fixture is NOT Grumic (medium blocky)", {
  # surface is strong subangular-blocky MEDIUM (10-20 mm > 1 cm) -> not self-mulching
  expect_false(isTRUE(qual_grumic(make_vertisol_canonical())$passed))
  expect_false(isTRUE(qual_mazic(make_vertisol_canonical())$passed))
})
