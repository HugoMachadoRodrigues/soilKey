# cran-comments.md -- soilKey 0.9.259

## Resubmission

This is a resubmission. The previous submission, 0.9.258 (2026-10-14), did
not pass the incoming checks automatically:

    Flavor: r-devel-windows-x86_64
    Check: Overall checktime, Result: NOTE
      Overall checktime 14 min > 10 min

mainly from the tests (461 s on Windows). As asked, the less important tests
now run only off CRAN: every test that took more than 0.03 s on CRAN's terms
(`NOT_CRAN` unset) on a fast machine starts with `skip_on_cran()`, 457 tests
in 137 files. The core of the three classification keys stays on CRAN: the
canonical profiles are classified to their WRB Reference Soil Group, SiBCS
order and USDA order. On this machine the tests now take 30 s on CRAN's
terms (98 s before); by the ratio of the 0.9.258 pre-test, about 150 s on
Windows. There are no code changes.

The three links to Embrapa's AgroAPI (`www.agroapi.cnptia.embrapa.br`), whose
server did not answer during the check of this version, are now plain text.

Before that, 0.9.213 (2026-10-08) was archived after one NOTE of its
pre-test, an invalid file URI in NEWS.md; it was removed in 0.9.258.

## Submission summary

This is an update to soilKey 0.9.184 (on CRAN since 2026-07-08). Everything
said for 0.9.213 still holds:

* **No third-party data in the tarball.** No real soil observations and no
  GPL-3 material are shipped. The SoilTaxonomy datasets are read from the
  installed SoilTaxonomy package (Suggests); two KST 13th-edition JSON files
  are downloaded on first use from a pinned commit, checked by MD5 and cached
  under `tools::R_user_dir("soilKey", "cache")`, and the classification keys
  never read them. `inst/DATA-PROVENANCE.md` accounts for every data file.
* `Authors@R` declares the copyright holder (`cph`), matching LICENSE.

Since 0.9.213 the three classification keys were checked against the texts
they implement, each change in NEWS with the passage it follows:

* **WRB 2022**: names built by the Chapter 4 lists and the Chapter 2.2 rules,
  no qualifier that WRB 2022 does not have, and the diagnostics checked
  against Chapter 3 (0.9.216 to 0.9.220). One export is added, `nitisol()`.
* **SiBCS 2018**: the diagnostic horizons and attributes, and the steps of
  the key, read item by item from the book (0.9.225 to 0.9.257).
* Loaders of the Brazilian soil databases and the optional Shiny app were
  corrected along the way.

No exported function was removed and none changed its signature in a
non-additive way. DESCRIPTION is unchanged apart from Version and Date.

## Test environments

* macOS 27 (arm64), R 4.6.1: `R CMD check --as-cran` on the built tarball.
* GitHub Actions, `R CMD check` with `--as-cran`: ubuntu-latest R-devel,
  R-release and R-oldrel-1; macos-latest and windows-latest R-release.
* The pre-tests of 0.9.213 and 0.9.258 on R-devel (Debian and Windows) gave
  only the two issues fixed here.

## R CMD check --as-cran results

On the built tarball of 0.9.259 (R 4.6.1, macOS), with
`_R_CHECK_CRAN_INCOMING_REMOTE_=true`, the vignettes and the PDF and HTML
manuals: **0 errors | 0 warnings | 1 note**.

* `checking CRAN incoming feasibility ... OK`: no invalid URL or file URI.
* The one note is local only:

      * checking HTML version of manual ... NOTE
        Skipping checking HTML validation: 'tidy' doesn't look like recent
        enough HTML Tidy.

  macOS ships an old `tidy`; the CRAN servers have a current one, and the
  pre-test of 0.9.213 did not give this note.

The tests take 30 s on this machine. The tarball is 6.0 MB; the installed size, 11.5 MB,
is reported as INFO.

## Notes for the CRAN team

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
