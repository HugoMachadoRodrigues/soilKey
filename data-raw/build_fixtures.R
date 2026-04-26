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
  ferralsol_canonical  = make_ferralsol_canonical(),
  luvisol_canonical    = make_luvisol_canonical(),
  chernozem_canonical  = make_chernozem_canonical(),
  calcisol_canonical   = make_calcisol_canonical(),
  gypsisol_canonical   = make_gypsisol_canonical(),
  solonchak_canonical  = make_solonchak_canonical(),
  cambisol_canonical   = make_cambisol_canonical(),
  plinthosol_canonical = make_plinthosol_canonical(),
  podzol_canonical     = make_podzol_canonical(),
  gleysol_canonical    = make_gleysol_canonical(),
  vertisol_canonical   = make_vertisol_canonical()
)

for (nm in names(fixtures)) {
  path <- file.path(out_dir, paste0(nm, ".rds"))
  saveRDS(fixtures[[nm]], path, version = 2L)
  message(sprintf("Wrote %s (%d horizons)",
                   path, nrow(fixtures[[nm]]$horizons)))
}

# ---- Sanity checks -----------------------------------------------

.check <- function(fix_name, expected, fixture) {
  passed <- vapply(c("argic","ferralic","mollic","calcic","gypsic","salic",
                       "cambic","plinthic","spodic","gleyic_properties",
                       "vertic_properties"),
                    function(d) isTRUE(get(d, envir = asNamespace("soilKey"))(fixture)$passed),
                    logical(1))
  if (!isTRUE(passed[expected])) {
    stop(sprintf("Fixture %s should pass %s but did not", fix_name, expected))
  }
  invisible(TRUE)
}

.check("ferralsol",  "ferralic",          fixtures$ferralsol_canonical)
.check("luvisol",    "argic",             fixtures$luvisol_canonical)
.check("chernozem",  "mollic",            fixtures$chernozem_canonical)
.check("calcisol",   "calcic",            fixtures$calcisol_canonical)
.check("gypsisol",   "gypsic",            fixtures$gypsisol_canonical)
.check("solonchak",  "salic",             fixtures$solonchak_canonical)
.check("cambisol",   "cambic",            fixtures$cambisol_canonical)
.check("plinthosol", "plinthic",          fixtures$plinthosol_canonical)
.check("podzol",     "spodic",            fixtures$podzol_canonical)
.check("gleysol",    "gleyic_properties", fixtures$gleysol_canonical)
.check("vertisol",   "vertic_properties", fixtures$vertisol_canonical)

message("\nAll fixtures built and diagnostics behave as expected.")
