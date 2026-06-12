# Reproducible bootstrap 95% CIs for the headline metrics

Expands the pooled confusion matrix back to its per-item (reference,
predicted) pairs, resamples them with replacement `B` times, and returns
percentile 95% CIs for accuracy / balanced accuracy / macro-F1 / kappa.
Seeded (default 42) and RNG-state-preserving, exactly like
`.benchmark_reproducible_sample`, so the CI is reproducible and never
perturbs the caller's randomness. Degenerate inputs (`< 2` items or
`< 2` reference classes) return `c(NA, NA)` per metric.

## Usage

``` r
.benchmark_bootstrap_metrics(cm, B = 1000L, seed = 42L)
```
