# cran-comments.md -- soilKey 0.9.92

## Submission summary

This is the first CRAN submission of soilKey, an R package providing
deterministic classification keys for the World Reference Base for
Soil Resources 2022 (4th edition), the Brazilian System of Soil
Classification (SiBCS, 5th edition), and USDA Soil Taxonomy (13th
edition).

The taxonomic key is implemented in pure R driven by versioned YAML
rules and has zero non-trivial dependencies (R6, data.table, yaml,
cli, rlang). Optional integrations (multimodal extraction via
ellmer; spatial priors via terra/sf; OSSL spectroscopy via
prospectr/resemble; aqp-canonical NRCS diagnostics via aqp +
SoilTaxonomy + mpspline2) are pulled in via Suggests.

## Test environments

- macOS 26.4.1 / R 4.6.0 (local).
- ubuntu-latest / R-release / R-devel / R-oldrel-1 (GitHub Actions
  via `.github/workflows/R-CMD-check.yaml`).
- macos-latest / R-release (GitHub Actions).
- windows-latest / R-release (GitHub Actions).
- pkgdown / test-coverage (separate workflows on every PR).

All seven matrix runs return clean on every PR merged to `main`
since v0.9.65 (≈30 PRs at time of submission). The CI run for the
release tag is linked from the GitHub Actions tab.

## R CMD check --as-cran results (v0.9.92, 2026-05-09)

- **0 ERRORs**
- **0 WARNINGs**
- **1 NOTE** -- "New submission" + maintainer line. Expected for a
  first submission.

The previous 4 sub-issues flagged by `--as-cran` in the v0.9.91
audit (2 invalid URLs, 1 invalid AfSP DOI, 5 raw `https://doi.org/`
URLs that should use `\doi{}`) are all resolved in v0.9.92.

## Reverse dependencies

There are no reverse dependencies (this is the first submission).

## Tarball size

Source tarball is ~10 MB. The package ships seven offline-usable
benchmark caches under `inst/extdata/`:

- 26 canonical-fixture pedons (one per WRB Reference Soil Group)
- AfSP Africa sample (n = 120, 1.2 MB)
- WoSIS stratified sample (n = 130, 1.3 MB)
- KSSL/NCSS sample (n = 99, 1.0 MB)
- KSSL+NASIS morphological sample (n = 99, 1.1 MB)

Each cache is the smallest reproducible slice of the upstream
dataset (ISRIC AfSP / WoSIS, NCSS Lab Data Mart) that exercises a
distinct WRB Reference Soil Group. Together they enable every
example, every vignette, every test, and every benchmark to run
offline and CRAN-friendly.

We considered moving the caches to a Suggests-pulled data-only
package but the caches are integral to `\examples{}` and the test
suite; splitting would create a circular dependency.

## Why this matters scientifically

soilKey closes three documented gaps in pedological tooling:

1. **No public, maintained implementation of the WRB 2022 key.** The
   IUSS Working Group's 4th-edition preface acknowledges that
   internal classification algorithms exist but have not been
   released. The CRAN package `SoilTaxonomy` (Beaudette et al.)
   provides USDA lookup tables, not the key.

2. **No public software for the Brazilian SiBCS** (Embrapa, 2018)
   anywhere -- not in R, not in Python, not in QGIS. soilKey is the
   first.

3. **No tool that integrates multimodal extraction, spectral
   prediction, and a deterministic key in a single provenance-aware
   pipeline.** soilKey enforces this separation as an architectural
   invariant: the taxonomic key is never delegated to a language
   model; vision-language models are restricted to schema-validated
   extraction.

## Validation

The package ships eleven benchmark drivers under `inst/benchmarks/`,
of which the canonical and offline ones run in <30 s without a
network. Empirical accuracy on five external datasets (post-v0.9.91):

| Dataset           | n   | Default | Best opt-in (`engine="aqp"`) |
|-------------------|----:|--------:|-----------------------------:|
| SiBCS BDsolos RJ  | 722 | 40.3%   | **46.6%**  (Argissolo +7.9pp, Latossolo +13.2pp) |
| SiBCS Redape Order|  94 | 45.7%   | **58.5%**  (also Subordem 39.4%, GG 35.2%, Subgrupo 25.0%) |
| WRB KSSL+NASIS    |  99 | 21.2%   | 24.2%                         |
| WRB AfSP          | 120 | 21.7%   | **30.8%**                      |
| WRB LUCAS Stage 3 |  30 |  0.0%   | **60.0%**  (Cambisols 18/18 = 100% recall via SoilGrids subsoil fill) |

`engine="aqp"` activates the v0.9.65+ NCSS-canonical diagnostics
and auto-bundles the v0.9.69 ECEC fallback (v0.9.86), the v0.9.70
texture-morphological fallback (v0.9.89), and the v0.9.90 argic
designation-inference fallback. Default canonical behaviour is
bit-for-bit preserved -- every opt-in is reversible via explicit
`options(soilKey.<rule> = FALSE)`.

## Citation

If soilKey contributes to your work, please cite the Zenodo
concept-DOI [10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112)
(always resolves to the latest version). `inst/CITATION` and
`citation("soilKey")` render the canonical BibTeX block.
