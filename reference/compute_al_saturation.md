# Compute aluminium saturation (%) from exchangeable bases and Al

Returns `al_cmol / (ca + mg + k + na + al) * 100`, or NA if any input is
missing or the sum (ECEC) is non-positive.

## Usage

``` r
compute_al_saturation(ca, mg, k, na, al)
```
