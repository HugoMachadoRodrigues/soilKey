# Plausibility ranges for OSSL-backed soil property predictions

Used by
[`predict_ossl_mbl`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md),
[`predict_ossl_plsr_local`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md)
and
[`predict_ossl_pretrained`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md)
to clip implausible values and to seed the synthetic-prediction
fallback. Ranges follow the Open Soil Spectral Library (OSSL) global
summary statistics.

## Usage

``` r
.ossl_property_ranges()
```

## Value

Named list of `c(min, max)` numeric pairs.
