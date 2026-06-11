# Reproducible random row sample (seed 42), restoring the global RNG state so the benchmark never perturbs the caller's randomness.

Reproducible random row sample (seed 42), restoring the global RNG state
so the benchmark never perturbs the caller's randomness.

## Usage

``` r
.benchmark_reproducible_sample(n, size)
```
