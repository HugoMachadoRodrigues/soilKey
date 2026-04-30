# Savitzky-Golay 1st derivative

Delegates to
[`prospectr::savitzkyGolay`](https://rdrr.io/pkg/prospectr/man/savitzkyGolay.html)
when available (`m = 1`, polynomial `p`, window `w`). The native
fallback uses the closed-form 5-point coefficients
`(-2, -1, 0, 1, 2) / 10`, which is the SG solution for `w = 5`, `p = 2`,
`m = 1`, and trims two columns from each edge. For `w != 5` the native
path falls back to a generic SG coefficient computation via least
squares.

## Usage

``` r
.sg1(X, w = 5L, p = 2L)
```
