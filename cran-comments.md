# cran-comments.md -- soilKey 0.9.157

## Resubmission

Two small, self-contained correctness fixes have landed on top of the 0.9.155
check-time work (which is unchanged and carried forward, below):

* **0.9.156 -- Munsell-from-spectra colorimetry.** Reported by Glenn Davis,
  author of the CRAN packages 'munsellinterpol' and 'spacesXYZ' on which this
  optional feature relies. The conversion was missing the mandatory
  D65 -> Illuminant-C chromatic adaptation (the Munsell renotation is anchored to
  Illuminant C), and `roundHVC()` was called without its required `books=`
  argument. Both are fixed; only the opt-in `predict_munsell_from_spectra()` is
  affected, and a constant-reflectance spectrum now correctly returns a neutral.
* **0.9.157 -- one USDA subgroup predicate.** `Humic Dystrudepts` now uses the
  colour-value differentia (matching its sibling `Humic Eutrudepts`); the 44
  canonical fixtures are byte-identical.

Neither affects check time. The 0.9.155 reductions remain the substance of this
resubmission. The 0.9.154 pre-test was **OK on Debian and OK on Windows** but the
Windows overall check time was 12 min (target 10 min); 0.9.155 trims it:

* **A data-server URL was timing out.** `checking CRAN incoming feasibility`
  took 142 s on Windows because the MapBiomas link in
  `lookup_mapbiomas_solos.Rd` timed out (60 s, server unreachable from the
  check host). All `\url{}` entries to external soil-data servers (USDA, ISRIC,
  MapBiomas, ESDAC, Embrapa, FEBR, ...) -- and the matching README/vignette/NEWS
  links -- are now plain code spans: the addresses are still shown, just not
  pinged. Feasibility drops to ~30 s.

* **Suggests-backed integration tests ran only on the Windows host.** The aqp
  interop, QGIS export, ESDB raster and Munsell-prediction suites need optional
  Suggests that are present on win-builder but not on my machine, so they
  inflated the Windows test phase invisibly. They are now `skip_on_cran()` and
  run in full on CI.

The 0.9.154 reductions are retained:

* The ~600 internal rule-engine predicates, already marked `@keywords internal`,
  are no longer exported and no longer documented (928 -> 319 topics; man pages
  1129 -> 335), which cut the HTML/PDF manual build. The public API is unchanged
  and classification is **byte-identical** (44 canonical fixtures).
* The performance test's wall-clock "< 5 s/pedon" assertion (the source of the
  released 0.9.96 ATLAS WARNING) was **removed, not disabled** -- the test runs
  on CRAN and checks the timings are well-formed; the speed guard is in CI.
* Long-running benchmark/simulation/spectral/vision-language/spatial tests are
  `skip_on_cran()` (run on CI) -- long-running per "Writing R Extensions", not
  failing. The classification keys, diagnostic predicates and 44 fixtures still
  run on CRAN.

A full local `R CMD check --as-cran` (with vignettes and manual) is
0 errors / 0 warnings / 1 NOTE; feasibility ~30 s, testthat ~60 s, and the
manual build is a fraction of its former size.

## Submission summary

This is a maintenance update to soilKey, following the v0.9.96 submission on
2026-05-19 (currently on CRAN). The v0.9.97 -> v0.9.157 series is
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
* **v0.9.156-157 -- correctness fixes.** A Munsell-from-spectra illuminant
  adaptation (D65 -> Illuminant C) and a `roundHVC(books=)` fix, both reported by
  the `munsellinterpol` author Glenn Davis; and one USDA Inceptisol "Humic"
  subgroup re-pointed to its colour-value predicate. Both are correctness-only;
  the 44 canonical fixtures are byte-identical.

No exported function from v0.9.96 changed signature in a non-additive way; every
new argument has a safe default.

## Test environments

- macOS 26.5 / R 4.6.0 (local, where this submission was built).
- ubuntu-latest / R-release / R-devel / R-oldrel-1 (GitHub Actions via
  `.github/workflows/R-CMD-check.yaml`, run with `--as-cran`).
- macos-latest / R-release; windows-latest / R-release (GitHub Actions).
- pkgdown (`pkgdown::check_pkgdown()`) and test-coverage (separate workflows).

## R CMD check --as-cran results (v0.9.157)

Multi-platform `R CMD check --as-cran` via GitHub Actions -- ubuntu
R-release / R-devel / R-oldrel-1, macOS R-release, and Windows R-release -- is
**Status: OK** on every platform (0 errors / 0 warnings / 0 notes; the CI build
uses `--no-manual`). "CRAN incoming feasibility", "checking examples
(+ --run-donttest)", and "re-building of vignette outputs" are **OK**; on CRAN
the heavy tests skip, so the testthat phase is ~60 s.

A full local build that additionally renders the HTML/PDF manual adds a single
**local-only NOTE**: *"checking HTML version of manual ... Skipping checking HTML
validation: 'tidy' doesn't look like recent enough HTML Tidy."* -- the
macOS-bundled `tidy` is older than CRAN's, so this does not appear on the CRAN
check servers (giving the expected 1 NOTE there).

All tests pass under `R CMD check` (`testthat.R`, ~60 s on CRAN with the
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
