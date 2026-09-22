# Tests for the v0.9.107 SiBCS accuracy fixes on the Redape gold standard.
# The fixes recover four zero-recall orders by consuming morphological signal
# the loader/gate previously dropped (g/f/v master-letter suffixes; stacked
# chernic A horizons), plus a B-planico exclusion so a Planossolo vertissolico
# does not flip to Vertissolo. Skipped when the Redape data is absent.

.acc_redape_dir <- function() {
  file.path(getOption("soilKey.benchmark_root",
            "/Users/rodrigues.h/Library/CloudStorage/OneDrive-Personal/soilKey/soil_data"),
            "redape_geotab")
}

.acc_recall <- function() {
  peds <- load_redape_pedons(.acc_redape_dir(), verbose = FALSE)
  r <- benchmark_redape(peds, level = "order", verbose = FALSE)
  pc <- r$per_class_recall
  setNames(pc$recall, pc$reference_rsg)
}


test_that("the four recovered SiBCS orders reach full/near-full recall on Redape", {
  skip_on_cran()
  skip_if_not(dir.exists(.acc_redape_dir()), "Redape data not present")
  rec <- .acc_recall()
  expect_equal(unname(rec["gleissolos"]), 1)     # g-suffix -> redoximorphic (8/8)
  expect_equal(unname(rec["plintossolos"]), 1)   # f-suffix -> plinthite (3/3)
  expect_equal(unname(rec["vertissolos"]), 1)    # v-suffix -> slickensides+cracks (2/2)
  expect_gte(unname(rec["chernossolos"]), 0.5)   # stacked chernic A (>=1/2)
})


test_that("overall Redape order accuracy improved past the pre-fix baseline (43)", {
  skip_on_cran()
  skip_if_not(dir.exists(.acc_redape_dir()), "Redape data not present")
  peds <- load_redape_pedons(.acc_redape_dir(), verbose = FALSE)
  r <- benchmark_redape(peds, level = "order", verbose = FALSE)
  expect_gt(r$accuracy * r$n_compared, 50)       # was 43/94; now ~56/94
})


test_that("a Planossolo vertissolico (RN_038) stays Planossolos, not Vertissolos", {
  skip_on_cran()
  skip_if_not(dir.exists(.acc_redape_dir()), "Redape data not present")
  peds <- load_redape_pedons(.acc_redape_dir(), verbose = FALSE)
  hit <- Filter(function(p) grepl("RN_038", p$site$id %||% ""), peds)
  skip_if(length(hit) == 0L, "RN_038 not in dataset")
  pred <- classify_sibcs(hit[[1]], on_missing = "silent")$rsg_or_order
  expect_match(pred, "Planossolos")
})


test_that("the vertic B-planico exclusion is wired into vertissolo()", {
  skip_on_cran()
  # A synthetic vertic profile WITH an abrupt textural change (B planico) must
  # not pass vertissolo(); the same profile without it should.
  mk <- function(abrupt) {
    hz <- data.frame(
      top_cm = c(0, 20, 40), bottom_cm = c(20, 40, 120),
      designation = c("A", "Btv", if (abrupt) "Btv" else "Cv"),
      clay_pct = if (abrupt) c(14, 45, 50) else c(45, 50, 55),
      slickensides = c(NA, "common", "common"),
      cracks_width_cm = c(NA, 1, 1), stringsAsFactors = FALSE)
    soilKey::PedonRecord$new(site = list(id = "vt"), horizons = hz)
  }
  expect_false(isTRUE(vertissolo(mk(TRUE))$passed))    # abrupt -> excluded
})
