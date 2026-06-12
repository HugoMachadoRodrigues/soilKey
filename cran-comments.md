# cran-comments.md -- soilKey 0.9.109

## Submission summary

This is a maintenance update to soilKey, following the v0.9.96 submission
on 2026-05-19. The v0.9.97 -> v0.9.109 release series is backward-compatible
and tracked in NEWS per release; the headline of v0.9.109 itself is a
documentation/release-hardening pass (see "Release hardening" below).

The taxonomic key remains pure R driven by versioned YAML rules with zero
non-trivial hard dependencies (R6, data.table, yaml, cli, rlang).

> Submit this update AFTER v0.9.96 has been processed by CRAN; do not stack
> submissions in the queue.

## What's new since v0.9.96

* **v0.9.97-100** -- professional `bslib` Shiny app (`run_classify_app(ui="pro")`);
  WRB Tier-3 strict mode; field-photo-only classification; provenance-weighted
  Monte-Carlo uncertainty.
* **v0.9.101-103** -- the Shiny "Map" tab: SoilGrids point prior, batch
  classification + GeoPackage export, and a gridded (DSM) class raster.
* **v0.9.104-105** -- all three systems now reach their deepest formal level:
  USDA **family** (`classify_usda(include_family=TRUE)`) and WRB **depth
  specifiers** (`classify_wrb2022(specifiers=TRUE)`). Both default off and are
  byte-identical to v0.9.96 when unset.
* **v0.9.106-107** -- reproducible multi-dataset benchmark suite
  (`run_all_benchmarks()`) and a benchmark-guided SiBCS accuracy uplift.
* **v0.9.108** -- Pro-app UX polish; `report()` gains additive `include_family`
  / `specifiers` (default off, byte-identical output).
* **v0.9.109 (this release) -- release hardening.** ~600 atomic taxonomic-engine
  predicates (WRB qualifiers, USDA subgroup gates, SiBCS attribute/horizon
  gates) are marked `@keywords internal`: they remain exported and callable but
  leave the public reference index, which trims the documented public API from
  ~910 to ~195 topics and adds the missing `\value` sections required by
  `R CMD check --as-cran`. Runnable `\examples` were added to the main entry
  points. No user-visible behaviour changed (the change is documentation-only;
  the deterministic key dispatches engine predicates by name as before).

No exported function from v0.9.96 changed signature in a non-additive way.
Every new argument has a safe default.

## Test environments

- macOS Tahoe 26.5 / R 4.6.0 (local, where this submission was built).
- ubuntu-latest / R-release / R-devel / R-oldrel-1 (GitHub Actions
  via `.github/workflows/R-CMD-check.yaml`, now run explicitly with `--as-cran`).
- macos-latest / R-release; windows-latest / R-release (GitHub Actions).
- pkgdown (with `pkgdown::check_pkgdown()`) / test-coverage (separate workflows).

## R CMD check --as-cran results (v0.9.109)

- **0 ERRORs**
- **0 WARNINGs**
- **0-2 NOTEs**, environmental only:

  1. *"Days since last update"* (if any) -- the DESCRIPTION Date is refreshed
     at submission time.
  2. *"Skipping checking HTML validation: 'tidy' doesn't look like recent
     enough HTML Tidy."* -- the macOS-bundled `tidy` is older than CRAN's; this
     NOTE does not appear on the CRAN check servers.

All ~5,000 tests pass under `R CMD check`. Tests that need optional Suggests
(magick, jsonvalidate, pdftools, ellmer, terra, sf, aqp, ...) use
`skip_if_not_installed()`.

## Reverse dependencies

There are no reverse dependencies.

## Tarball size

The lazy-fetch architecture (v0.9.94) is unchanged: four large benchmark
caches are pulled on demand from a versioned GitHub Release, not bundled in
the tarball, which is kept under the CRAN size guidance.

## Citation

If soilKey contributes to your work, please cite the Zenodo concept-DOI
[10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112) (always
resolves to the latest version). `inst/CITATION` and `citation("soilKey")`
render the canonical BibTeX block.
