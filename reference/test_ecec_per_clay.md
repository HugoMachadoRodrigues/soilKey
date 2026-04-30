# Test effective CEC (sum of bases + Al) per kg clay \<= threshold

Default threshold is 12 cmol_c/kg clay (WRB 2022 ferralic horizon). If
`ecec_cmol` is missing, computes ECEC from
`ca_cmol + mg_cmol + k_cmol + na_cmol + al_cmol` when those are
available.

## Usage

``` r
test_ecec_per_clay(h, max_cmol_per_kg_clay = 12, candidate_layers = NULL)
```

## Arguments

- h:

  Numeric threshold or option (see Details).

- max_cmol_per_kg_clay:

  Numeric threshold or option (see Details).

- candidate_layers:

  Numeric threshold or option (see Details).
