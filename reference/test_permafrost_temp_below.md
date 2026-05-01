# Test mean annual permafrost-zone temperature at or below threshold

WRB 2022 alternative criterion for cryic conditions: a horizon within
the upper `max_depth_cm` reporting `permafrost_temp_C` at or below
`max_temp_C` (default 0 C). Used as an explicit OR-alternative to the
designation-pattern path.

## Usage

``` r
test_permafrost_temp_below(h, max_temp_C = 0, max_top_cm = 100)
```
