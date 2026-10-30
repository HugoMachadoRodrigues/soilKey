# v0.9.317: the frozen suffix that cryic_conditions() and the mollic
# criterion 6a read follows the notation the site declares. FAO and USDA
# write a frozen layer f; the Canadian notation writes it z (its f is the
# Fe and Al of a podzolic B); the New Zealand and Australian notations have
# no frozen suffix (their f is fibric peat and faunal accumulation).
# Made-up profiles.

.frz <- function(designation, system = NULL) {
  n <- length(designation)
  top <- c(0, cumsum(rep(20, n - 1L)))
  site <- list(id = "frz")
  if (!is.null(system)) site$designation_system <- system
  PedonRecord$new(site = site, horizons = ensure_horizon_schema(data.table::data.table(
    designation = designation, top_cm = top, bottom_cm = top + 20)))
}

test_that("an f is frozen in the FAO and USDA notations, and with no notation declared", {
  skip_on_cran()
  expect_true(cryic_conditions(.frz(c("A", "Bw", "Cf")))$passed)
  expect_true(cryic_conditions(.frz(c("Oi", "Bf"), "fao2006"))$passed)
  expect_true(cryic_conditions(.frz(c("Oi", "Bf"), "usda"))$passed)
})

test_that("New Zealand fibric peat and Australian f are not frozen", {
  skip_on_cran()
  expect_false(isTRUE(cryic_conditions(.frz(c("Of", "Om", "Oh"), "nzsc"))$passed))
  expect_false(isTRUE(cryic_conditions(.frz(c("A1", "B2f", "C"), "asc"))$passed))
  # a designation naming permafrost still counts
  expect_true(cryic_conditions(.frz(c("Of", "Om", "C permafrost"), "nzsc"))$passed)
})

test_that("the Canadian notation writes a frozen layer z, and its Bf is no frozen layer", {
  skip_on_cran()
  expect_false(isTRUE(cryic_conditions(.frz(c("LFH", "Ae", "Bhf", "Bf", "BC"), "cssc"))$passed))
  expect_true(cryic_conditions(.frz(c("Ah", "Bmy", "Cz"), "cssc"))$passed)
  expect_true(cryic_conditions(.frz(c("Ah", "Bm", "IICz"), "cssc"))$passed)
  # the Canadian example of a Cryosol is frozen for WRB as well
  expect_true(cryic_conditions(make_cssc3_example("cryosolic"))$passed)
})

test_that("the mollic criterion 6a reads the same frozen suffix", {
  skip_on_cran()
  fz <- function(p) {
    d <- as.character(p$horizons$designation)
    grepl(soilKey:::.frozen_designation_pattern(p$site), sub("^[0-9]*", "", d))
  }
  expect_identical(fz(.frz(c("Of", "Bw"), "nzsc")), c(FALSE, FALSE))
  expect_identical(fz(.frz(c("Of", "Bw"))), c(TRUE, FALSE))
  expect_identical(fz(.frz(c("Bf", "Cz"), "cssc")), c(FALSE, TRUE))
})
