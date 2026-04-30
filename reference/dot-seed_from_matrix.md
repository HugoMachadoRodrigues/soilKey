# Hash-derived seed from a numeric matrix

Produces a deterministic 32-bit integer seed from the contents of a
numeric matrix so that synthetic predictions are reproducible per input
spectrum without relying on global RNG state.

## Usage

``` r
.seed_from_matrix(X)
```
