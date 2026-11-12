# cran-comments.md -- soilKey 0.9.403

## Submission summary

This is an update to soilKey 0.9.259 (on CRAN since 2026-10-14).

The package adds keys for twelve national soil classification systems and
for the FAO-Unesco legends of 1974 and 1988, beside the WRB 2022, SiBCS and
USDA keys it had (see NEWS.md, 0.9.263 to 0.9.403). Every key is read from
its published text; no data of these systems is shipped.

### Check time

The R code grew with these keys, so the step "checking R code for possible
problems" takes longer. To stay within the time limit:

* On CRAN (`NOT_CRAN` unset) `tests/testthat.R` runs a smoke set: one
  profile through each of the seventeen keys, the fixture tests of the WRB
  2022, SiBCS, USDA, CSSC and ASC keys, the `PedonRecord` class, the
  registry of systems and `classify_all()`. The full suite runs off CRAN.
* The vignettes run their code only off CRAN. The vignettes in the tarball
  were built with every chunk run.

The win-builder checks of the tarball of 0.9.403 (2026-11-12), Status OK
on both:

* R-devel: 479 s (R code 272 s, examples 39 s, tests 12 s, vignettes 12 s).
* R-release: 527 s (R code 258 s, examples 45 s, tests 12 s, vignettes
  11 s, HTML manual 80 s).

As for earlier versions:

* **No third-party data in the tarball.** No real soil observations and no
  GPL-3 material are shipped. The SoilTaxonomy datasets are read from the
  installed SoilTaxonomy package (Suggests); two KST 13th-edition JSON files
  are downloaded on first use from a pinned commit, checked by MD5 and cached
  under `tools::R_user_dir("soilKey", "cache")`, and the classification keys
  never read them. `inst/DATA-PROVENANCE.md` accounts for every data file.
* `Authors@R` declares the copyright holder (`cph`), matching LICENSE.

No exported function was removed and none changed its signature in a
non-additive way; 208 functions are added, mostly the `classify_*()` and
`make_*_example()` functions of the new keys. In DESCRIPTION, Imports gains
`parallel` (a base package); otherwise only Version and Date change.

## Test environments

* win-builder, R-devel and R-release, on the tarball of 0.9.403.
* macOS 27 (arm64), R 4.6.1: `R CMD check --as-cran` on the built tarball.
* GitHub Actions, `R CMD check` with the full test suite: ubuntu-latest
  R-devel, R-release and R-oldrel-1; macos-latest and windows-latest
  R-release.

## R CMD check --as-cran results

win-builder R-devel and R-release: Status OK (no notes).

On macOS: **0 errors | 0 warnings | 1 note**, local only:

    * checking HTML version of manual ... NOTE
      Skipping checking HTML validation: 'tidy' doesn't look like recent
      enough HTML Tidy.

macOS ships an old `tidy`; win-builder did not give this note. The tarball
is 9.0 MB; the installed size, 24 MB, is reported as INFO (R code 10 MB).

## Notes for the CRAN team

* Long-running benchmark, simulation, spectral, vision-language and network
  tests run only off CRAN. Tests that need optional packages use
  `skip_if_not_installed()`.

## Reverse dependencies

There are no reverse dependencies.

## Citation

Zenodo concept-DOI [10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112)
(always resolves to the latest version); `citation("soilKey")` renders the
BibTeX entry.
