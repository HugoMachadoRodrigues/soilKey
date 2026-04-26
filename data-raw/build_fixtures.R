# ================================================================
# Build canonical fixtures and serialise them to inst/extdata/.
#
# Run from the package root:
#   source("data-raw/build_fixtures.R")
#
# The fixture builders are exported functions in R/utils-fixtures.R, so
# tests and vignettes do not depend on the materialised .rds files.
# Materialised .rds artefacts ship in the binary package as a backup
# entry point and as a cross-check that the builders are reproducible
# (a commit-time diff on these files would flag accidental drift).
# ================================================================

suppressPackageStartupMessages({
  if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(soilKey)
  }
  library(data.table)
})

out_dir <- file.path("inst", "extdata")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

fixtures <- list(
  ferralsol_canonical = make_ferralsol_canonical(),
  luvisol_canonical   = make_luvisol_canonical(),
  chernozem_canonical = make_chernozem_canonical(),
  calcisol_canonical  = make_calcisol_canonical(),
  gypsisol_canonical  = make_gypsisol_canonical(),
  solonchak_canonical = make_solonchak_canonical()
)

for (nm in names(fixtures)) {
  path <- file.path(out_dir, paste0(nm, ".rds"))
  saveRDS(fixtures[[nm]], path, version = 2L)
  message(sprintf("Wrote %s (%d horizons)",
                   path, nrow(fixtures[[nm]]$horizons)))
}

# ---- Sanity checks -----------------------------------------------

stopifnot(
  isTRUE(ferralic(fixtures$ferralsol_canonical)$passed),
  isFALSE(argic   (fixtures$ferralsol_canonical)$passed) ||
     is.na(argic   (fixtures$ferralsol_canonical)$passed),
  isFALSE(mollic  (fixtures$ferralsol_canonical)$passed) ||
     is.na(mollic  (fixtures$ferralsol_canonical)$passed),

  isTRUE(argic    (fixtures$luvisol_canonical)$passed),
  isFALSE(ferralic(fixtures$luvisol_canonical)$passed),
  isFALSE(mollic  (fixtures$luvisol_canonical)$passed),

  isTRUE(mollic   (fixtures$chernozem_canonical)$passed),
  isFALSE(ferralic(fixtures$chernozem_canonical)$passed),
  isFALSE(argic   (fixtures$chernozem_canonical)$passed)
)

message("\nAll fixtures built and diagnostics behave as expected.")
