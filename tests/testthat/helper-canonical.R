# Skips for the two reference datasets soilKey no longer ships (v0.9.204).
#
# * SoilTaxonomy objects (WRB_4th_2022, ST_criteria_13th, ST_features) are read
#   from the installed SoilTaxonomy package, which is GPL-3 and a Suggests.
# * The two KST 13th JSON files come from ncss-tech/SoilKnowledgeBase (GPL-3)
#   and are downloaded on first use, pinned and checked by MD5.
#
# Tests that need them skip cleanly instead of failing when the package is
# absent or the network is unavailable, and never download on CRAN.

skip_if_no_soiltaxonomy <- function() {
  testthat::skip_if_not_installed("SoilTaxonomy", minimum_version = "0.2.8")
}

skip_if_no_kst13 <- function() {
  testthat::skip_on_cran()
  ok <- tryCatch({
    soilKey:::.kst13_path("2022_KST_codes.json")
    soilKey:::.kst13_path("2022_KST_criteria_EN.json")
    TRUE
  }, error = function(e) FALSE)
  if (!ok) testthat::skip("KST 13th files unavailable (no cache and no network)")
}
