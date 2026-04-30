# Standard Normal Variate transform

Per-row centring and scaling: `(x - rowMeans) / rowSds`. Uses
[`prospectr::standardNormalVariate`](https://rdrr.io/pkg/prospectr/man/standardNormalVariate.html)
when available, otherwise a native vectorised implementation. Returns a
matrix of the same shape as the input.

## Usage

``` r
.snv(X)
```
