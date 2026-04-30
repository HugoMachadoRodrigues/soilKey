# CEC per kg clay (cmol_c)

`cec_cmol * 100 / clay_pct`. Returns `NA` when either input is missing
or `clay_pct <= 0`.

## Usage

``` r
cec_per_clay(cec_cmol, clay_pct)
```
