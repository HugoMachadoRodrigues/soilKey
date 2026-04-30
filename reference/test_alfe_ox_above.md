# Test for the andic+vitric Al_ox + 1/2 Fe_ox sum

Reuses `compute_alfe_ox()` (declared inline below to keep the file
self-contained); pass thresholds for andic (\>=2.0) or vitric (\>=0.4).

## Usage

``` r
test_alfe_ox_above(h, min_pct, candidate_layers = NULL)
```
