# WRB 2006 RSG code -\> 2022 RSG name

AfSP ships WRB 2006 RSG codes (2-letter, e.g.\\ LV, AC, AR). The
2-letter codes are stable across WRB editions (2006 -\> 2022); only a
handful of qualifier names changed. This helper maps the codes to the
WRB 2022 RSG names that `classify_wrb2022` emits.

## Usage

``` r
wrb06_code_to_rsg(code)
```

## Arguments

- code:

  Character vector of WRB 2006 codes.

## Value

Character vector of singular WRB 2022 RSG names; `NA` for unrecognised
codes.
