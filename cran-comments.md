# cran-comments.md -- soilKey 0.9.154

## Resubmission

This is a resubmission. The 0.9.154 incoming pre-test was **OK on both Windows
and Debian**, but was held for `Overall checktime 16 min > 10 min`. This version
is a genuine reduction of the check footprint (not a wholesale disabling of
tests), and addresses the two NOTEs:

* **Manual size (the main lever):** the ~600 internal rule-engine predicates
  were already marked `@keywords internal`; they are now *truly* internal --
  removed from `NAMESPACE` and the reference manual (the rule engine resolves
  them from the namespace; tests reach them via `test_check()`). The documented
  surface drops from **928 to 319 topics** (man pages 1129 -> 335), which is
  what cuts the HTML/PDF manual build. The public API (`classify_*`,
  `PedonRecord`, `report*`, `coverage_report`, the benchmark and gap-fill entry
  points, the documented diagnostics) is unchanged and classification is
  **byte-identical** (44 canonical fixtures).

* **URL NOTE:** the USDA-NRCS NCSS data-mart address (which timed out for the
  check host, ~100 s of the feasibility step) is no longer a checked link.

* **Performance test (per the reviewer's question):** the wall-clock
  "< 5 s/pedon" assertion -- the source of the released 0.9.96 ATLAS WARNING --
  is **removed, not disabled**. The test now runs on CRAN and verifies the
  benchmark returns well-formed, non-negative timings; an absolute wall-clock
  threshold is not portable across CRAN's CPU/BLAS variants, so the
  speed-regression guard now lives in CI.

* Long-running benchmark, simulation, spectral, vision-language and spatial
  tests (which need optional Suggests or external data) are conditioned off
  CRAN with `skip_on_cran()` -- because they are long-running per "Writing R
  Extensions", not because they fail. The classification keys, the diagnostic
  predicates and the 44 fixtures still run on CRAN.

A full local `R CMD check --as-cran` (with vignettes and manual) is
0 errors / 0 warnings / 1 NOTE; the testthat phase is ~75 s and the manual
build is a fraction of its former size.

## Submission summary

This is a maintenance update to soilKey, following the v0.9.96 submission on
2026-05-19 (currently on CRAN). The v0.9.97 -> v0.9.154 series is
backward-compatible and tracked in NEWS per release. Every new feature is
additive and **default off**, so a v0.9.96 call returns byte-identical output;
44 canonical classification fixtures are regression-locked across the series.

The taxonomic key remains pure R driven by versioned YAML rules with zero
non-trivial hard dependencies (R6, data.table, yaml, cli, rlang).

> Submit this update only after v0.9.96 has been fully processed by CRAN; do not
> stack submissions in the queue.

## What's new since v0.9.96 (headline series)

* **v0.9.97-103** -- professional `bslib` Shiny app (`run_classify_app(ui="pro")`),
  WRB Tier-3 strict mode, field-photo-only classification, provenance-weighted
  Monte-Carlo uncertainty, and the "Map" tab (SoilGrids point prior, batch +
  GeoPackage export, gridded DSM raster).
* **v0.9.104-105** -- all three systems reach their deepest formal level: USDA
  **family** (`classify_usda(include_family=TRUE)`) and WRB **depth specifiers**
  (`classify_wrb2022(specifiers=TRUE)`), both default off.
* **v0.9.106-113** -- reproducible multi-dataset benchmark suite
  (`run_all_benchmarks()`); benchmark-guided SiBCS/WRB accuracy work; release
  hardening (engine predicates `@keywords internal`, `\value` sections, runnable
  `\examples`); +829 USDA subgroups via a name-based generator gated against
  KSSL (n=2895).
* **v0.9.114-117** -- bilingual (EN/PT) Pro app and `report()`: dependency-free
  i18n catalog, a11y + responsive layout, exported
  `validate_horizon_geometry()`.
* **v0.9.118-125** -- engineering robustness (`validate_rules()`, graceful
  predicate resolution); honest `coverage_report()` v2; high-frequency WRB
  qualifiers wired; USDA colour/contact + intergrade subgroups; a
  predicate-correctness audit of the USDA and WRB cores against the verbatim
  `ST_criteria_13th` and the WRB 2022 4th-edition text.
* **v0.9.120-144 -- gap-fill of missing horizon attributes (opt-in).** A
  `gapfill=` argument on every `classify_*` entry point fills missing cells from
  one of four sources -- within-pedon depth interpolation, definitional horizon
  derivation, a SoilGrids coordinate prior, or (new in v0.9.144) a **non-circular
  predicted-taxon prior** (`build_taxon_profiles()` +
  `gapfill_by_predicted_taxon()`). All four are default off and tagged with a
  low-authority provenance so they affect the evidence grade. The taxonomic key
  itself is never delegated to a model.
* **v0.9.145-150 -- honest coverage + spectral on-ramp.** WRB qualifier coverage
  corrected to 233/234; a small SiBCS subgroup accuracy fix (Redape 27.1 ->
  32.9%); USDA subgroup coverage 73.8 -> 75.5% via criteria-exact, KSSL-gated
  additions; and a spectral-dataset ingestion path (`read_spectral_library()` /
  `pedons_from_spectral_table()` / `benchmark_spectral_fill()` + a
  `gapfill = "spectra"` method) so a Vis-NIR/MIR + lab-label dataset can drive
  the OSSL prediction engine. The `download_ossl_subset()` endpoint now fails
  gracefully with actionable guidance when the public mirror is unreachable.

No exported function from v0.9.96 changed signature in a non-additive way; every
new argument has a safe default.

## Test environments

- macOS 26.5 / R 4.6.0 (local, where this submission was built).
- ubuntu-latest / R-release / R-devel / R-oldrel-1 (GitHub Actions via
  `.github/workflows/R-CMD-check.yaml`, run with `--as-cran`).
- macos-latest / R-release; windows-latest / R-release (GitHub Actions).
- pkgdown (`pkgdown::check_pkgdown()`) and test-coverage (separate workflows).

## R CMD check --as-cran results (v0.9.154)

A full local build (with vignettes and manual) and `R CMD check --as-cran`:

- **0 ERRORs**
- **0 WARNINGs**
- **1 NOTE**: *"checking HTML version of manual ... Skipping checking HTML
  validation: 'tidy' doesn't look like recent enough HTML Tidy."* This is a
  local-only artefact -- the macOS-bundled `tidy` is older than CRAN's -- and
  does not appear on the CRAN check servers. "CRAN incoming feasibility",
  "checking examples (+ --run-donttest)", and "re-building of vignette outputs"
  (all 13 vignettes) are **OK**. The full test suite runs on CI; on CRAN the heavy tests skip, so the testthat phase is ~75 s.

All tests pass under `R CMD check` (`testthat.R`, ~75 s on CRAN with the
long-running suites skipped; full suite on CI). Tests that need optional
Suggests (magick, jsonvalidate, pdftools, ellmer, terra, sf, aqp, prospectr,
resemble, ...) use `skip_if_not_installed()`.

## Reverse dependencies

There are no reverse dependencies.

## Tarball size

The lazy-fetch architecture (v0.9.94) is unchanged: large benchmark caches are
pulled on demand from a versioned GitHub Release, not bundled in the tarball,
which is kept within the CRAN size guidance.

## Citation

If soilKey contributes to your work, please cite the Zenodo concept-DOI
[10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112) (always
resolves to the latest version). `inst/CITATION` and `citation("soilKey")`
render the canonical BibTeX block.
