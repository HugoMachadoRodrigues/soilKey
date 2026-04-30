# Deterministic synthetic prediction (fallback)

Generates predictions from a stable seed derived from the input spectra.
Each (horizon, property) draw is a shifted, lightly noised centre within
the property's plausibility range. Prediction intervals scale inversely
with the row's spectral information content (here: 1 -
clipped_variance). This is \*not\* a soil-physical model – it exists so
that the v0.4 plumbing can be tested end-to-end without OSSL installed.

## Usage

``` r
.predict_synthetic(X, properties, region, k, method_label)
```
