# Standard Normal Variate transform

Per-row centring and scaling: `(x - rowMeans) / rowSds`. Uses
[`prospectr::standardNormalVariate`](https://l-ramirez-lopez.github.io/prospectr/reference/standardNormalVariate.html)
when available, otherwise a native vectorised implementation. Returns a
matrix of the same shape as the input.

## Usage

``` r
.snv(X)
```
