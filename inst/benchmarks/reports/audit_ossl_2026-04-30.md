# OSSL spectra audit — 2026-04-30 (soilKey v0.9.8)

## Summary

`R/spectra-ossl.R` and `R/spectra-predict.R` ship **plumbing for the OSSL
pipeline** but **no bundled OSSL data** and **no first-party fetch
helper**. The default code path is the deterministic *synthetic*
predictor — useful for testing, vignettes and documentation, but not a
soil-physical prediction.

This audit was triggered by the question raised in the v0.9.8 review:
*"`predict_ossl_*()` puxa OSSL de verdade ou só roda sobre fixture
sintética?"* — the answer is the latter, by default.

## What is real

| Component                            | State                                                                           |
| :----------------------------------- | :------------------------------------------------------------------------------ |
| `pi_to_confidence()`                 | Real, soil-physically meaningful, fully tested.                                 |
| `preprocess_spectra()`               | Real (SNV, Savitzky–Golay 1st derivative); delegates to `prospectr` when present. |
| `fill_from_spectra()` plumbing       | Real — wires preprocess → predict → `pedon$add_measurement(source = "predicted_spectra")`. |
| Provenance tag downgrades grade A→B  | Real and tested via `evidence_grade` machinery.                                 |
| `predict_ossl_mbl()` real branch     | Real — delegates to `resemble::mbl()` when both `resemble` is installed AND a populated `ossl_library = list(Xr, Yr)` is supplied. |
| `predict_ossl_pretrained()` real branch | Real — calls `predict(ossl_models[[prop]], X)` when a list of pre-trained models is supplied. |

## What is synthetic / placeholder

| Component                            | State                                                                           |
| :----------------------------------- | :------------------------------------------------------------------------------ |
| Bundled OSSL training data           | **None.** The package does not ship any OSSL spectra or reference values.       |
| Fetch helper                         | **None.** No `download_ossl()` function exists. Users must construct `ossl_library` themselves from the OSSL S3 bucket / soilspectroscopy.org artefacts. |
| Pre-trained model bundle             | **None.** No `ossl_models` factory ships with the package.                       |
| Default `predict_ossl_*()` path      | **Synthetic.** Returns a deterministic-by-spectrum draw within the OSSL property ranges. The seed is `digest(X)`-equivalent, so two runs on identical spectra yield identical results — but this is not a prediction. |
| Region tag (`south_america` etc.)    | **Tag only.** When the synthetic path is taken, region tightens / loosens the prediction interval but does not influence the predicted value. The real path uses region only via `ossl_library` construction (caller's responsibility). |

## What this means for the README claim

The README headline says:

> *"OSSL spectroscopy and explicit per-attribute provenance"*

That claim is honest when read carefully — the spectroscopy-to-pedon
pipeline IS wired and the provenance tagging IS real. **But the
out-of-the-box default does not predict from the OSSL library.** A
pedologist running `fill_from_spectra(pedon)` today gets
`evidence_grade = "B"` (because `predicted_spectra` was tagged) but the
underlying numbers come from the synthetic fallback unless they
explicitly construct an OSSL library and a `resemble`/pretrained
artefact.

## Path to "real" by default

For v0.9.9 / v1.0, three concrete steps would close the gap:

1. **Ship a curated OSSL subset.** The OSSL public S3 mirror
   (`https://storage.googleapis.com/soilspec4gg-public/ossl_*` and the
   `ossl-import` package) lets us cherry-pick ~500 South-America
   profiles into a ~10 MB `data/ossl_subset_sa.rda`. Users would call
   `data(ossl_subset_sa)` and pass it as `ossl_library`.

2. **Add `download_ossl_subset()`.** A guarded helper that fetches a
   region-filtered subset on demand (with caching under
   `tools::R_user_dir("soilKey", "cache")`). Fail loudly when offline
   so the user knows to fall back to synthetic.

3. **Make the synthetic fallback announce itself.** Wrap the
   `attr(preds, "backend") == "synthetic"` branch with
   `cli::cli_alert_warning()` describing what just happened and how to
   get the real path. This is implemented in v0.9.9 (this commit).

## Tests

`tests/testthat/test-spectra-ossl.R` validates:

- `pi_to_confidence()` saturation behaviour (real numerical contract).
- `make_synthetic_pedon_with_spectra()` shape contract.
- `fill_from_spectra()` rejects non-PedonRecord inputs and
  shape-mismatched spectra.
- The end-to-end pipeline writes the expected number of provenance
  rows tagged `predicted_spectra`.

None of these tests exercise the real-data path because no real-data
artefact ships with the package. They are correctness tests on the
plumbing, not on the prediction quality. **A paper-grade run requires
an OSSL artefact and is currently a manual setup step**, documented in
`vignettes/05-spatial-spectra-pipeline.Rmd`.
