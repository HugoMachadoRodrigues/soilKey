# cran-comments.md -- soilKey 0.9.213

## Submission summary

This is an update to soilKey 0.9.184 (on CRAN since 2026-07-08). Its main
purpose is licensing hygiene: **the package no longer ships any third-party
data.** It also carries classification-correctness fixes found since.

* **No third-party observations or GPL-3 material in the tarball.**
  - A file of 40 real WoSIS profiles (`inst/extdata/wosis_sa_sample.rds`) is
    removed (0.9.199). WoSIS data are now queried live from ISRIC's public API
    by the optional Shiny app, per request, and never stored.
  - The SoilTaxonomy datasets `WRB_4th_2022`, `ST_criteria_13th` and
    `ST_features`, previously vendored under `inst/extdata/canonical/`, are
    GPL-3 while soilKey is MIT; they are now read from the installed
    SoilTaxonomy package (Suggests, >= 0.2.8). The copies were `identical()`,
    so no result changes (0.9.204).
  - Two KST 13th-edition JSON files (GPL-3, from SoilKnowledgeBase) are no
    longer bundled. `kst13_codes()` and `coverage_report()` download them on
    first use from a pinned commit, check their MD5 and cache them under
    `tools::R_user_dir("soilKey", "cache")`. The classification keys never
    read them, so classifying works offline. The one example that needs the
    download is in `\donttest{}` behind `try()`, and the tests that need it
    are `skip_on_cran()`.
  - Benchmark reports computed from third-party databases are removed.
    `inst/DATA-PROVENANCE.md` now accounts for every data file shipped: each
    is synthetic, hand-constructed, or the classification criteria
    themselves.
  - `Authors@R` now declares the copyright holder (`cph`), matching LICENSE.
* **Classification fixes** (each in NEWS with the source text it follows):
  - CEC-per-clay limits that WRB 2022 and SiBCS write as "less than" (< 16,
    < 24, < 17) accepted a value equal to the limit; they are now strict,
    while USDA's oxic horizon keeps its "16 or less" (0.9.212).
  - A profile with no horizons is refused with a classed error instead of
    being classified with evidence grade A (0.9.208); a profile with horizons
    but no soil property now has no evidence grade (NA) and a warning, not A
    (0.9.213).
  - munsellinterpol 3.6-0 (CRAN, September 2026) warns on a negative Munsell
    Chroma; soilKey's continuous-notation path now passes the raw value so
    that warning can reach the user (0.9.203). Output is unchanged.
* **Optional Shiny app** (`run_classify_app()`): many fixes for its hosted
  deployment. Its slow handlers can run in background R processes through
  `mirai`; `mirai`, `promises` and `later` are new in Suggests and are used only
  by the app, after `requireNamespace()`.

No exported function changed signature in a non-additive way. `ferralic()`
gains one argument with a default that keeps the WRB text.

## Test environments

* macOS 26 (arm64), R 4.6.1: `R CMD check --as-cran` on the built tarball.
* GitHub Actions, `R CMD check` with `--as-cran`: ubuntu-latest R-devel,
  R-release and R-oldrel-1; macos-latest and windows-latest R-release.

## R CMD check --as-cran results

On the built tarball (R 4.6.1, macOS), with the vignettes, the PDF and HTML
manuals and `--run-donttest` examples: **0 errors | 0 warnings | 1 note**.
The note is local only:

    * checking HTML version of manual ... NOTE
      Skipping checking HTML validation: 'tidy' doesn't look like recent
      enough HTML Tidy.

macOS ships an old `tidy`; the CRAN servers have a current one. CRAN incoming
feasibility (with remote URL checks) is OK. The tests take 71 s. The tarball
is 5.5 MB (0.9.184: 6.6 MB); the installed size, 11.0 MB, is reported as INFO.

## Notes for the CRAN team

* The CRAN check page for 0.9.184 shows a WARN on r-release-windows-x86_64 at
  "checking for unstated dependencies in 'tests'": "unable to access index for
  repository" for a Bioconductor mirror. It is a network failure of that
  check run, not a problem in the package; every other flavour is OK.
* Long-running benchmark, simulation, spectral, vision-language and network
  tests are `skip_on_cran()` (they run in full on CI); the classification
  keys, diagnostics and canonical fixtures run on CRAN. Tests that need
  optional packages use `skip_if_not_installed()`.

## Reverse dependencies

There are no reverse dependencies.

## Citation

Zenodo concept-DOI [10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112)
(always resolves to the latest version); `citation("soilKey")` renders the
BibTeX entry.
