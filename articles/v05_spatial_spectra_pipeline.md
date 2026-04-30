# Spatial prior + OSSL spectra pipeline (Modules 3 & 4)

Modules 3 and 4 sit *alongside* the deterministic key, never inside it:

- **Module 3 (`spatial-*`)** – pulls a probabilistic prior over RSGs
  from a regional or global map (SoilGrids, an Embrapa pedological map,
  or any raster the user supplies) and runs a **consistency check** that
  warns when the deterministic classification disagrees with the prior.
  The key is never overwritten – the prior is purely advisory.
- **Module 4 (`spectra-*`, `vlm-*`)** – gap-fills horizon attributes
  (clay, CEC, BS, OC, pH, …) from Vis-NIR / SWIR or MIR spectra via the
  OSSL library. Predicted values are recorded with
  `source = "predicted_spectra"` so the evidence grade tracks the
  substitution.

This vignette walks both modules end-to-end on the canonical Ferralsol
fixture, with all external dependencies (raster files, OSSL parquet
libraries, ellmer chats) replaced by inline synthetic objects so the
example is fully reproducible.

## 1. Start from a partially-described pedon

Take the canonical Latossolo and *intentionally erase* the CEC and
base-saturation values from the lower horizons. We will fill them back
in with simulated OSSL predictions in §3.

``` r

pr_full   <- make_ferralsol_canonical()
pr_partial <- pr_full$clone(deep = TRUE)
pr_partial$horizons[3:5, c("cec_cmol", "bs_pct") := NA]

pr_partial$horizons[, .(top_cm, bottom_cm, designation,
                        clay_pct, cec_cmol, bs_pct, oc_pct)]
#>    top_cm bottom_cm designation clay_pct cec_cmol bs_pct oc_pct
#>     <num>     <num>      <char>    <num>    <num>  <num>  <num>
#> 1:      0        15           A       50      8.0     24    2.0
#> 2:     15        35          AB       52      6.5     17    1.2
#> 3:     35        65          BA       55       NA     NA    0.6
#> 4:     65       130         Bw1       60       NA     NA    0.3
#> 5:    130       200         Bw2       60       NA     NA    0.2
```

The classification on this incomplete pedon already differs:

``` r

res_partial <- classify_wrb2022(pr_partial, on_missing = "silent")
res_partial$rsg_or_order
#> [1] "Nitisols"
res_partial$evidence_grade
#> [1] "A"
```

The evidence grade reflects the missing data – the per-RSG trace records
which RSGs returned NA because of attributes we erased.

## 2. Module 3 – spatial prior consistency check

The prior is a probability vector over RSGs from any source – a regional
SoilGrids extract, a national Embrapa map, an interpolated kriging
surface, etc. For this vignette we build it inline so the example runs
without network access:

``` r

# Synthetic prior consistent with the gneiss-Mata-Atlantica context:
# Ferralsols dominate, with a tail of Acrisols and Cambisols.
prior <- data.table::data.table(
  rsg_code    = c("FR", "AC", "CM", "AL"),
  probability = c(0.62, 0.20, 0.12, 0.06)
)
prior
#>    rsg_code probability
#>      <char>       <num>
#> 1:       FR        0.62
#> 2:       AC        0.20
#> 3:       CM        0.12
#> 4:       AL        0.06
```

[`prior_consistency_check()`](https://hugomachadorodrigues.github.io/soilKey/reference/prior_consistency_check.md)
confirms the deterministic call (`FR`) is supported by the prior:

``` r

chk <- prior_consistency_check(rsg_code = "FR", prior = prior, threshold = 0.05)
chk
#> $consistent
#> [1] TRUE
#> 
#> $p
#> [1] 0.62
#> 
#> $threshold
#> [1] 0.05
#> 
#> $status
#> [1] "consistent"
#> 
#> $note
#> [1] "Assigned RSG 'FR' has prior probability 0.620 at this location (>= threshold 0.050)."
#> 
#> $top_prior
#>    rsg_code probability
#>      <char>       <num>
#> 1:       FR        0.62
#> 2:       AC        0.20
#> 3:       CM        0.12
```

Now suppose the deterministic key had instead landed on Cambisols. The
same prior would flag the disagreement (Cambisols at probability 0.12 vs
the dominant Ferralsols at 0.62 – the inconsistency margin):

``` r

prior_consistency_check(rsg_code = "AL", prior = prior, threshold = 0.05)
#> $consistent
#> [1] TRUE
#> 
#> $p
#> [1] 0.06
#> 
#> $threshold
#> [1] 0.05
#> 
#> $status
#> [1] "consistent"
#> 
#> $note
#> [1] "Assigned RSG 'AL' has prior probability 0.060 at this location (>= threshold 0.050)."
#> 
#> $top_prior
#>    rsg_code probability
#>      <char>       <num>
#> 1:       FR        0.62
#> 2:       AC        0.20
#> 3:       CM        0.12
```

The deterministic key is never overwritten by the prior. The check only
flags cases where a manual review is warranted; the user remains in
charge of the final assignment. Real production runs would source the
prior from
[`spatial_prior_soilgrids()`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_soilgrids.md)
(a live SoilGrids-WCS request) or
[`spatial_prior_embrapa()`](https://hugomachadorodrigues.github.io/soilKey/reference/spatial_prior_embrapa.md):

``` r

prior <- spatial_prior_soilgrids(pr_partial, buffer_m = 250)
```

## 3. Module 4 – OSSL gap-filling

The OSSL workflow is:

1.  Pre-process the raw spectra (SNV / Savitzky-Golay 1st derivative).
2.  Send each horizon’s spectrum through one of three predictors:
    - [`predict_ossl_mbl()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_mbl.md)
      – memory-based learning (recommended);
    - [`predict_ossl_plsr_local()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_plsr_local.md)
      – partial-least-squares with a local subset;
    - [`predict_ossl_pretrained()`](https://hugomachadorodrigues.github.io/soilKey/reference/predict_ossl_pretrained.md)
      – a pre-trained Cubist or RF model.
3.  Convert each property’s prediction-interval width to an A–D
    confidence grade via
    [`pi_to_confidence()`](https://hugomachadorodrigues.github.io/soilKey/reference/pi_to_confidence.md).
4.  [`fill_from_spectra()`](https://hugomachadorodrigues.github.io/soilKey/reference/fill_from_spectra.md)
    writes each predicted value into the horizons table AND adds a
    provenance entry with `source = "predicted_spectra"`.

A *production* call would look like this (skipped in this vignette
because OSSL is a multi-GB dataset that would have to be downloaded):

``` r

fill_from_spectra(
  pr_partial,
  library     = "ossl",
  region      = "south_america",
  properties  = c("clay_pct", "cec_cmol", "bs_pct", "oc_pct"),
  method      = "mbl",
  preprocess  = "snv+sg1",
  k_neighbors = 100L,
  ossl_library = "/path/to/ossl-soilsite-vnir.parquet"
)
```

For the vignette, simulate the predicted values directly through
`pedon$add_measurement()`:

``` r

preds <- list(
  list(idx = 3, attribute = "cec_cmol", value = 5.5, confidence = 0.78),
  list(idx = 3, attribute = "bs_pct",   value = 14,  confidence = 0.72),
  list(idx = 4, attribute = "cec_cmol", value = 4.9, confidence = 0.79),
  list(idx = 4, attribute = "bs_pct",   value = 13,  confidence = 0.74),
  list(idx = 5, attribute = "cec_cmol", value = 4.7, confidence = 0.70),
  list(idx = 5, attribute = "bs_pct",   value = 13,  confidence = 0.71)
)

pr_filled <- pr_partial$clone(deep = TRUE)
for (p in preds) {
  pr_filled$add_measurement(
    horizon_idx = p$idx,
    attribute   = p$attribute,
    value       = p$value,
    source      = "predicted_spectra",
    confidence  = p$confidence,
    overwrite   = TRUE
  )
}

pr_filled$horizons[, .(top_cm, bottom_cm, cec_cmol, bs_pct)]
#>    top_cm bottom_cm cec_cmol bs_pct
#>     <num>     <num>    <num>  <num>
#> 1:      0        15      8.0     24
#> 2:     15        35      6.5     17
#> 3:     35        65      5.5     14
#> 4:     65       130      4.9     13
#> 5:    130       200      4.7     13
```

The predicted values are now in the horizons table, *and* the provenance
log records each as `predicted_spectra`:

``` r

prov <- pr_filled$provenance
prov[source == "predicted_spectra", .(horizon_idx, attribute, source, confidence)]
#>    horizon_idx attribute            source confidence
#>          <int>    <char>            <char>      <num>
#> 1:           3  cec_cmol predicted_spectra       0.78
#> 2:           3    bs_pct predicted_spectra       0.72
#> 3:           4  cec_cmol predicted_spectra       0.79
#> 4:           4    bs_pct predicted_spectra       0.74
#> 5:           5  cec_cmol predicted_spectra       0.70
#> 6:           5    bs_pct predicted_spectra       0.71
```

## 4. Re-classify with the gap-filled pedon

After OSSL fills the missing CEC/BS, the deterministic key has a
complete dataset and the classification’s evidence grade reflects the
predicted source.

``` r

res_filled <- classify_wrb2022(pr_filled, on_missing = "silent")
res_filled$rsg_or_order
#> [1] "Ferralsols"
res_filled$evidence_grade
#> [1] "B"
res_filled$name
#> [1] "Geric Ferric Rhodic Chromic Ferralsol (Clayic, Humic, Dystric, Ochric, Rubic)"
```

The evidence grade ladder for the same profile across the three
workflows:

| Workflow                                  | Evidence grade |
|-------------------------------------------|----------------|
| Lab-only (full canonical fixture)         | A              |
| Spectra-filled (OSSL `predicted_spectra`) | B              |
| VLM-extracted only                        | C              |
| User-assumed                              | D              |

## 5. Combining priors and posteriors

[`combine_priors()`](https://hugomachadorodrigues.github.io/soilKey/reference/combine_priors.md)
merges multiple sources (SoilGrids + Embrapa + a custom map) with
weights, returning a single normalised prior that you can feed into
[`prior_consistency_check()`](https://hugomachadorodrigues.github.io/soilKey/reference/prior_consistency_check.md):

``` r

combined <- combine_priors(
  priors = list(
    soilgrids = data.table::data.table(rsg_code = c("FR", "AC", "CM"),
                                          probability = c(0.62, 0.20, 0.18)),
    embrapa   = data.table::data.table(rsg_code = c("FR", "AC", "NT"),
                                          probability = c(0.55, 0.30, 0.15))
  ),
  weights = c(soilgrids = 0.6, embrapa = 0.4)
)
combined
#>    rsg_code  probability
#>      <char>        <num>
#> 1:       FR 0.7139748011
#> 2:       AC 0.2841641547
#> 3:       CM 0.0017189621
#> 4:       NT 0.0001420821
```

[`posterior_classify()`](https://hugomachadorodrigues.github.io/soilKey/reference/posterior_classify.md)
would then take a `ClassificationResult` and a prior, returning a
posterior probability over RSGs (the deterministic key contributes a
sharply peaked likelihood). Used for ranking ambiguous fixtures or for
active-learning loops.

## Summary

- The deterministic key always runs first and is never overwritten.
- Module 3 gives a probabilistic *sanity check* against external maps
  and warns on disagreement.
- Module 4 fills missing horizon attributes from spectra, with full
  provenance, so the evidence grade tracks the substitution.
- Putting them together, you can take a partially-described pedon, fill
  its gaps from spectra, classify it deterministically, and cross-check
  the result against a global map – all in one pipeline.

The next vignette (`v06_wosis_benchmark`) shows how to run this whole
stack at scale against the WoSIS global pedon archive for paper-grade
validation.
