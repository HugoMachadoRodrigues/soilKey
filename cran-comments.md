# cran-comments.md -- soilKey 0.9.184

## Submission summary

This is a maintenance update to soilKey 0.9.157 (accepted 2026-06-30, currently on
CRAN). It lands sooner than a routine cycle for one reason, and I have batched the
rest of the accumulated work into the same submission rather than submitting more
than once:

* **Trigger -- munsellinterpol 3.4-0 (CRAN, 2026-07-03).** soilKey's optional
  Munsell-from-spectra feature now adopts that package's new, documented
  `XYZtoMunsell(XYZ, white=)` conversion, which performs the mandatory
  D65 -> Illuminant-C chromatic adaptation internally -- the correctness issue its
  author (Glenn Davis) originally reported for v0.9.156. The previous
  `XYZ -> CIELAB -> LabToMunsell()` route is retained as a numerically identical
  fallback, so **no new hard dependency and no version bump to Suggests**; output
  is byte-identical. v0.9.184 additionally fixes a neutral-hue edge case (hue is
  undefined at Chroma 0, so a constant-reflectance spectrum now reports the
  neutral "N" in the continuous notation too, matching the rounded path).

The series is backward-compatible and tracked in NEWS per release. Every new
feature is additive and **default off**, so a v0.9.157 call returns the same
result; the canonical classification fixtures are regression-locked in the test
suite (which passes under `R CMD check`, below). The taxonomic key remains pure R
driven by versioned YAML rules, with no non-trivial hard dependencies (R6,
data.table, yaml, cli, rlang); every colorimetry and model-assisted path is
optional and gated behind `Suggests`.

## What's new since v0.9.157 (headline)

* **Colorimetry.** v0.9.158 derives the D65 white point from the same bundled CIE
  table the spectra are integrated against, so a constant reflectance maps to an
  exact neutral (Chroma 0); v0.9.183 adopts `munsellinterpol::XYZtoMunsell()` and
  adds a test that a perfect reflecting diffuser gives Munsell Value 10; v0.9.184
  reports hue "N" for neutrals in the continuous notation.
* **Professional Shiny app (`run_classify_app(ui = "pro")`), all opt-in UI.**
  Live Vis-NIR preprocessing (absorbance + Savitzky-Golay smoothing/derivatives);
  a unified Map tab with continuous class overlays, a ranked-neighbour table and
  buffer query; per-point uncertainty drill-in for a group of sites; a right-side
  assistant drawer (an offline, grounded scripted assistant with no key, or a free
  open Groq model if `GROQ_API_KEY` is set -- it never classifies, it only
  explains the deterministic result); a restored profile-photo tab; and bilingual
  (EN/PT) polish. None of this changes the classification API.

No exported function from v0.9.157 changed signature in a non-additive way; every
new argument has a safe default.

## Test environments

- macOS 26.5 / R 4.6.1 (local, where this submission was built and checked with
  `R CMD check --as-cran`, vignettes + manual).
- ubuntu-latest / R-release / R-devel / R-oldrel-1 (GitHub Actions via
  `.github/workflows/R-CMD-check.yaml`, run with `--as-cran`).
- macos-latest / R-release; windows-latest / R-release (GitHub Actions).

## R CMD check --as-cran results

A local `R CMD check --as-cran` (R 4.6.1, macOS 26.5), building the vignettes and
the HTML/PDF manual, is **0 errors | 0 warnings | 1 NOTE**. The single NOTE is
local-only:

    * checking HTML version of manual ... NOTE
      Skipping checking HTML validation: 'tidy' doesn't look like recent enough
      HTML Tidy.

The macOS-bundled `tidy` is older than CRAN's, so this NOTE does not appear on the
CRAN check servers, nor in the GitHub Actions `--as-cran` runs (Status: OK,
0/0/0, which use `--no-manual`). The installed size is reported as INFO (14.6 MB,
dominated by the versioned YAML `rules/` -- 3.9 MB -- that drive the classification
key), not as a NOTE.

Long-running benchmark, simulation, spectral, vision-language and spatial tests
are `skip_on_cran()` (they run in full on CI), per "Writing R Extensions"; the
classification keys, diagnostic predicates and the canonical fixtures run on CRAN
(testthat ~60 s). Tests that need optional Suggests (magick, jsonvalidate,
pdftools, ellmer, terra, sf, aqp, prospectr, resemble, munsellinterpol, ...) use
`skip_if_not_installed()`.

## Reverse dependencies

There are no reverse dependencies.

## Tarball size

The tarball is ~6.6 MB. The lazy-fetch architecture (v0.9.94) is unchanged: large
benchmark caches are pulled on demand from a versioned GitHub Release, not bundled,
keeping the tarball within CRAN size guidance.

## Citation

If soilKey contributes to your work, please cite the Zenodo concept-DOI
[10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112) (always resolves
to the latest version). `inst/CITATION` and `citation("soilKey")` render the
canonical BibTeX block.
