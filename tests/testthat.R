library(testthat)
library(soilKey)

# v0.9.403: the full suite runs where NOT_CRAN is "true" (devtools, the
# GitHub checks). On CRAN only the smoke set below runs: one profile
# through each key of the registry (test-v09401-cran-core-keys.R), the
# fixture tests of the WRB 2022, SiBCS, USDA, CSSC and ASC keys, the
# PedonRecord class, the registry and classify_all(). The package code
# has grown with the national systems, and the CRAN check time limit
# leaves no room for the rest.
if (identical(Sys.getenv("NOT_CRAN"), "true")) {
  test_check("soilKey")
} else {
  test_check("soilKey", filter = paste(
    "v09401-cran-core-keys", "key-wrb-end-to-end", "key-sibcs$",
    "key-usda$", "v09267-cssc3-orders", "v09277-asc3-orders",
    "PedonRecord$", "v09263-systems-registry", "v0928-classify-all",
    sep = "|"))
}
