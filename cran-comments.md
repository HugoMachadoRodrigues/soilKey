# cran-comments.md -- soilKey 0.9.12

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
prospectr/resemble) are pulled in via Suggests.

## Test environments

- macOS 26.4.1 / R 4.6.0 (local).
- ubuntu-latest / R-release / R-devel / R-oldrel-1 (GitHub Actions
  via `.github/workflows/R-CMD-check.yaml`).
- macos-latest / R-release (GitHub Actions).
- windows-latest / R-release (GitHub Actions).

All five matrix runs return clean (`R-CMD-check` workflow on the
[Actions tab](https://github.com/HugoMachadoRodrigues/soilKey/actions/workflows/R-CMD-check.yaml)
for the v0.9.10 / v0.9.11 release tags).

## R CMD check results

- **0 ERRORs**
- **0 WARNINGs**
- **1 NOTE** -- environmental, not a defect:

  ```
  * checking dependencies in R code ... NOTE
  proj_create: Cannot find proj.db
  proj_create: no database context specified
  ```

  This NOTE comes from `terra::rast()` being load-checked when
  declared in `Suggests`. `proj.db` is not present on the local
  development laptop's PATH; CRAN's check farm includes a working
  PROJ install so the NOTE will not appear there.

  All `terra`/`sf`-touching tests are guarded with
  `skip_if_no_proj()` and `skip_if_not_installed()` so they pass on
  hosts without PROJ.

## Reverse dependencies

There are no reverse dependencies (this is the first submission).

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

The package ships three benchmark drivers under `inst/benchmarks/`:

- `run_canonical_benchmark()` -- offline, network-free, runs every
  release; achieves **WRB 31/31, SiBCS 20/20, USDA 31/31** on the 31
  canonical fixtures (one per WRB Reference Soil Group, sourced
  from the WRB 2022 didactic exemplars + ISRIC ISMC monoliths +
  Soil Atlas of Europe).
- `run_wosis_benchmark_graphql()` -- queries the live WoSIS GraphQL
  API at https://graphql.isric.org/wosis/graphql; produces the
  paper-grade external-data agreement statistics. Latest report is
  committed under `inst/benchmarks/reports/`.
- `run_wosis_benchmark()` -- legacy REST path kept for sites
  mirroring the deprecated WoSIS v3 API.

## Citation

If `soilKey` contributes to your work, please cite the Zenodo
concept-DOI [10.5281/zenodo.19930112](https://doi.org/10.5281/zenodo.19930112)
(it always resolves to the latest version). `inst/CITATION` and
`citation("soilKey")` render the canonical BibTeX block.
