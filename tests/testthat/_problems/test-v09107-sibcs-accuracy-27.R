# Extracted from test-v09107-sibcs-accuracy.R:27

# prequel ----------------------------------------------------------------------
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

# test -------------------------------------------------------------------------
skip_if_not(dir.exists(.acc_redape_dir()), "Redape data not present")
rec <- .acc_recall()
expect_equal(unname(rec["gleissolos"]), 1)
expect_equal(unname(rec["plintossolos"]), 1)
expect_equal(unname(rec["vertissolos"]), 1)
expect_gte(unname(rec["chernossolos"]), 0.5)
