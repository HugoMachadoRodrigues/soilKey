# cran-comments.md -- soilKey 0.9.100

## Submission summary

This is the next maintenance update to soilKey, following the v0.9.96
submission on 2026-05-19. The v0.9.97 -> v0.9.100 release series ships
the four roadmap features that v0.9.96's README listed as pending --
all backward-compatible and tracked in NEWS per release.

The taxonomic key remains pure R driven by versioned YAML rules with
zero non-trivial dependencies (R6, data.table, yaml, cli, rlang).

> Submit this update AFTER v0.9.96 has been processed by CRAN; do not
> stack submissions in the queue.

## What's new since v0.9.96

* **v0.9.97 -- Professional Shiny app** (`run_classify_app(ui="pro")`).
  Eight-tab `bslib` interface exposing the full pipeline. The legacy
  single-page app remains available via `ui="classic"`. New Suggests:
  bslib, shinyWidgets, plotly, htmltools -- all conditionally required
  with a clear install message.
* **v0.9.98 -- WRB Tier-3 RSG-gate strict mode**. `classify_wrb2022(strict=TRUE)`
  tightens seven Tier-2 RSG gates toward the WRB 2022 Ch 4 intent.
  Default `strict=FALSE` is byte-identical to v0.9.96; all 31 canonical
  fixtures classify the same under both modes.
* **v0.9.99 -- Field-photo-only classification**. `classify_from_photos()`,
  `apply_soilgrids_depth_prior()`, `compute_per_attribute_evidence_grade()`.
  Evidence grade E (user-assumed) split out from D.
* **v0.9.100 -- Provenance-weighted uncertainty MC**.
  `classify_with_uncertainty()` returns the posterior over classes,
  perturbation magnitudes scaled by provenance grade. Existing
  `classification_robustness()` gains an opt-in `provenance_aware`
  argument; `FALSE` (default) is byte-identical to v0.9.42.

No exported function from v0.9.96 changed signature in a non-additive
way. Every new argument has a safe default.

## Test environments

- macOS Tahoe 26.5 / R 4.6.0 (local, where this submission was built).
- ubuntu-latest / R-release / R-devel / R-oldrel-1 (GitHub Actions
  via `.github/workflows/R-CMD-check.yaml`).
- macos-latest / R-release (GitHub Actions).
- windows-latest / R-release (GitHub Actions).
- pkgdown / test-coverage (separate workflows on every PR).

All CI matrix runs green on every commit merged to `main` since
v0.9.65; the CI run for this release tag is linked from the GitHub
Actions tab.

## R CMD check --as-cran results (v0.9.100, 2026-05-23)

- **0 ERRORs**
- **0 WARNINGs**
- **2 NOTEs** (both environmental, not package defects):

  1. *"Days since last update: 4"* -- the DESCRIPTION Date is 4 days
     before the local build date; expected since this update follows
     the v0.9.96 submission closely. The Date will be refreshed at
     submission time.
  2. *"Skipping checking HTML validation: 'tidy' doesn't look like
     recent enough HTML Tidy."* -- macOS' bundled `tidy` is older than
     the one CRAN uses; this NOTE does not appear on the CRAN check
     servers.

All ~4,710 tests pass (~7 min under `R CMD check`), including the
new test files `test-v0997-shiny-pro-app.R`, `test-v0998-tier3-strict.R`,
`test-v0999-photo-only.R` and `test-v0100-uncertainty.R`. Tests that
need optional Suggests (magick, jsonvalidate, pdftools, ellmer) use
`skip_if_not_installed()`.

## Reverse dependencies

There are no reverse dependencies.

## Tarball size

Source tarball is ~6.3 MB (up from ~5.9 MB at v0.9.96), the small
increment is the new Shiny app (`inst/shiny/classify_app_pro/`),
three new R source files, four new vignettes, and the new test
files + extra man pages. The lazy-fetch architecture (v0.9.94) is
unchanged: four large benchmark caches are still pulled on demand
from a versioned GitHub Release, not bundled in the tarball.

## Suggests-only dependencies

New in v0.9.97: bslib, shinyWidgets, plotly, htmltools. All are
already on CRAN, all are used only inside `run_classify_app(ui="pro")`,
and `run_classify_app()` raises a copy-pasteable
`install.packages(...)` error if any are missing. The classic app
(`ui="classic"`) still needs only shiny + DT.

## Why these features matter scientifically

* **Photo-only classification** answers a field-realistic question
  (what is this soil given only a phone photo and a GPS fix?) and is
  honest about uncertainty -- the evidence grade is never A on a
  photo-only profile.
* **Provenance-weighted uncertainty** propagates the per-cell
  measurement quality into a posterior over classes, not just a
  robustness percentage. The leave-one-attribute-out sensitivity
  ranking is a direct, defensible answer to "what should I measure
  next?".
* **WRB Tier-3 strict mode** lets a user opt into the canonical Ch 4
  thresholds when the v0.9.96 regionally-tuned defaults are too
  permissive for their pedons.

## Citation

If soilKey contributes to your work, please cite the Zenodo
concept-DOI [10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112)
(always resolves to the latest version). `inst/CITATION` and
`citation("soilKey")` render the canonical BibTeX block.
