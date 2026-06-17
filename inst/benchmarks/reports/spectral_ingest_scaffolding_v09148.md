# Spectral-dataset ingestion scaffolding (v0.9.148)

The roadmap gap audit (v0.9.145) concluded that the soilKey accuracy ceiling is
missing reference data, and named the **one genuine remaining accuracy lever**: a
real Vis-NIR / MIR + lab-label dataset (a Brazilian spectral library) exercising
the package's OSSL prediction + Munsell colorimetry + spectral-neighbour engine —
which is real and installed, but had no in-package data and no ingestion path.

This release builds that path so the lever is ready the moment data arrives.

## What was already there (engine, unchanged)

- `predict_ossl_mbl` / `predict_ossl_plsr_local` / `predict_ossl_pretrained`,
  `preprocess_spectra`, `fill_from_spectra(ossl_library=)`,
  `classify_by_spectral_neighbours`, `predict_munsell_from_spectra`.
- `PedonRecord$spectra = list(vnir = <matrix rows=horizons × cols=wavelengths>)`.
- `ossl_library_template()` and the canonical `list(Xr, Yr, metadata)` shape;
  the synthetic `ossl_demo_sa` for tests/examples.

## What was missing (the glue, added here) — `R/spectra-ingest.R`

1. **`read_spectral_library(reflectance, metadata, …)`** → `list(Xr, Yr,
   metadata)`. Accepts wide or long reflectance, fraction or percent
   (`normalize = "auto"`), any instrument grid (`resample_to=`). Maps column
   headers to the canonical attributes + taxonomic labels via a built-in alias
   table **including Portuguese** (argila/silte/areia/carbono/ctc/ph), overridable.
2. **`pedons_from_spectral_table(…)`** → `PedonRecord`s with `$spectra$vnir`
   attached + reference labels in `$site`, grouped by `profile_col`.
3. **`benchmark_spectral_fill(…, system, folds)`** → non-circular k-fold ON/OFF:
   calibrate the library on the train profiles, classify each held-out profile
   OFF (spectra-only) vs ON (after `fill_from_spectra`), score vs the reference.
   Returns `accuracy_off` / `accuracy_on` / `delta` — the data-blocked number.
4. **Gap-fill method `"spectra"`** wired into `.classify_apply_gapfill`:
   `gapfill = list(method = "spectra", ossl_library = <lib>, fill_method = "mbl")`
   on any `classify_*`. (`method` is the dispatcher key, so the model choice is
   passed as `fill_method`.)

Input contract: `inst/templates/spectral_library_format.md`.

## Verification

- End-to-end synthetic smoke test passes: PT headers map correctly
  (argila→clay_pct, silte→silt_pct, areia→sand_pct, ctc→cec_cmol, carbono→oc_pct,
  ph→ph_h2o), percent→fraction, long+wide+resample paths, pedon binding, the
  `"spectra"` dispatch (caller never mutated — deep copy), and the k-fold
  benchmark all run and return the right structures.
- On the bundled **synthetic** `ossl_demo_sa` the benchmark delta is meaningless
  (random spectra → random labels); the harness — not a number — is what is
  proven here. The real ON/OFF lift will be measured the moment a labelled
  spectra-bearing BR dataset is loaded.
- +8 unit tests (`test-v09148`). Default classification path byte-identical
  (44 canonical fixtures unchanged; `gapfill` unset → no spectral code runs).
  `R CMD check --as-cran` = 1 NOTE (CRAN-incoming). 3 new exports added to the
  pkgdown reference.

## How to use it when the data lands

```r
lib <- read_spectral_library("refl.csv", read.csv("meta.csv"), id_col = "id")
res <- benchmark_spectral_fill("refl.csv", read.csv("meta.csv"),
                               system = "sibcs", folds = 5, method = "mbl")
res$accuracy_off; res$accuracy_on; res$delta
classify_sibcs(pedon, gapfill = list(method = "spectra", ossl_library = lib))
```
